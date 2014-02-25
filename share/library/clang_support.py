"""
This is the clang plugin for GPS. Its aim is to integrate the libclang
functionality into GPS
TODO: Complete plugin doc
"""

from clang import cindex as ci
import GPS
from modules import Module
from text_utils import forward_until
import completion
from completion import CompletionResolver, CompletionProposal

style_warning = None
style_error = None


def initialize_styles():
    global style_warning
    global style_error
    style_warning = GPS.Style("clang-live-warnings")
    style_warning.set_background(
        GPS.Preference("Warnings-Src-Highlight-Color").get()
    )
    style_error = GPS.Style("clang-live-errors")
    style_error.set_background(
        GPS.Preference("Errors-Src-Highlight-Color").get()
    )


def to_completion_point(ed_loc):
    return forward_until(
        ed_loc.forward_char(-1),
        lambda c: not (c.isalpha() or c.isdigit() or c == "_"),
        backwards=True
    ).forward_char()

####################
# Main clang class #
####################


class Clang(object):

    def can_find_builtin_headers(self, args=[]):
        flags = 0
        current_file = ("test.c", '#include "stddef.h"')

        try:
            tu = self.index.parse("test.c", args, [current_file], flags)
        except ci.TranslationUnitLoadError:
            return False

        return len(tu.diagnostics) == 0

    def __init__(self, project):

        self.project = project
        self.index = ci.Index.create()

        if not self.can_find_builtin_headers():
            raise Exception("Must implement smart headers resolution !")

        self.translation_units = {}
        self.translation_units_changed = {}
        self.messages = []

    def invalidate_index(self):
        self.index = ci.Index.create()

    def get_compiler_options(self, gps_file):
        """
        This function will return the compiler options for a specific file in
        the current project.  TODO: gpr projects enable the user to define
        custom switches on a per file basis, that are not taken care of for the
        moment.
        """
        return self.project.get_attribute_as_list(
            package="compiler", attribute="switches",
            index=gps_file.language().upper()
        )

    def __get_translation_unit(self, args, file_content,
                               file_name, update=False):

        file_tuple = (file_name, file_content)
        tu = self.translation_units.get(file_name)

        if tu:
            if update:
                tu.reparse([file_tuple])
                self.translation_units_changed[file_name] = True
            return tu

        flags = ci.TranslationUnit.PARSE_PRECOMPILED_PREAMBLE | \
            ci.TranslationUnit.PARSE_DETAILED_PROCESSING_RECORD

        try:
            tu = self.index.parse(file_name, args, [file_tuple], flags)
        except ci.TranslationUnitLoadError:
            return None

        self.translation_units[file_name] = tu

        # Reparse to initialize the PCH cache even for auto completion
        # This should be done by index.parse(), however it is not.
        # So we need to reparse ourselves.

        tu.reparse([file_tuple])
        self.translation_units_changed[file_name] = True

        return tu

    def get_translation_unit(self, ed_buffer, update=False):
        f = ed_buffer.file()
        return self.__get_translation_unit(
            self.get_compiler_options(f),
            ed_buffer.get_chars(), f.name(), update
        )

    def update_translation_unit(self, ed_buffer):
        f = ed_buffer.file()
        _ = self.__get_translation_unit(
            self.get_compiler_options(f),
            ed_buffer.get_chars(), f.name(), True
        )

    def refresh_buffer(self, ed_buffer, update=False):
        f = ed_buffer.file()
        if f.language() in ("c", "c++"):
            if update:
                self.update_translation_unit(ed_buffer)

            self.add_diagnostics(ed_buffer)
            self.translation_units_changed[f.name()] = False

    def add_diagnostics(self, ed_buffer):
        f = ed_buffer.file()
        tu = self.get_translation_unit(ed_buffer)

        if self.translation_units_changed[f.name()]:

            for m in self.messages:
                m.remove()
            self.messages = []

            global diags
            diags = tu.diagnostics
            for d in tu.diagnostics:
                m = GPS.Message(
                    "Clang live diagnostics", f,
                    d.location.line,
                    d.location.column, d.spelling, 2
                )

                if d.severity < 3:
                    m.set_action("", "gps-build-warning", d.spelling)
                    m.set_style(style_warning)
                else:
                    m.set_action("", "gps-build-error", d.spelling)
                    m.set_style(style_error)

                self.messages.append(m)

    def get_completions_at(self, ed_loc):
        """
        This method will return completions at a given point in the buffer.
        """

        ed = ed_loc.buffer()
        ed_loc = to_completion_point(ed_loc)

        file_tuple = (ed.file().name(), ed.get_chars())
        tu = self.get_translation_unit(ed, True)

        if not tu:
            return None

        cr = tu.codeComplete(
            file_tuple[0], ed_loc.line(), ed_loc.column(),
            [file_tuple], 0
        )

        return cr


#######################
# Global clang module #
#######################

class ClangCompletionResolver(CompletionResolver):
    """
        A completion resolver based on clang results
    """

    def __init__(self):
        self.prefix = None
        pass

    def get_completion_prefix(self, loc):
        loc_begin = to_completion_point(loc)
        self.prefix = loc.buffer().get_chars(loc_begin, loc)[:-1]
        return self.prefix

    def get_completions(self, loc):

        cr = Clang_Module.clang_instance.get_completions_at(loc)
        if not cr:
            return

        current_result = 0
        while current_result < cr.results.numResults:
            s = cr.results[current_result].string

            len = s.num_chunks

            # Useful debug trace
            # print "found " + " ".join([s[n].spelling for n in range(0, len)])

            if len >= 2:
                label = s[1].spelling
                if label.startswith(self.prefix):
                    yield CompletionProposal(
                        label,
                        label,
                        " ".join([s[n].spelling for n in range(0, len)]),
                        language_category=completion.Cat_Unknown)

            current_result += 1


class Clang_Module(Module):

    clang_instance = None

    def setup(self):
        initialize_styles()

    def init_clang_instance(self):
        Clang_Module.clang_instance = Clang(GPS.Project.root())

    def gps_started(self):
        self.init_clang_instance()
        Clang_Module.clang_instance.refresh_buffer(GPS.EditorBuffer.get())
        GPS.Completion.register(ClangCompletionResolver())

    def project_changed(self):
        self.init_clang_instance()

    def project_view_changed(self):
        Clang_Module.clang_instance.invalidate_index()

    def buffer_edited(self, ed_buffer):
        Clang_Module.clang_instance.refresh_buffer(
            GPS.EditorBuffer.get(ed_buffer), True
        )

    def file_edited(self, f):
        if f.language() in ("c", "c++"):
            self.clang_instance.refresh_buffer(GPS.EditorBuffer.get(f))

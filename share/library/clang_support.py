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
import os
from time import time

style_warning = None
style_error = None

kind_name_to_category = {
    ci.CursorKind.ENUM_CONSTANT_DECL: completion.CAT_LITERAL,
    ci.CursorKind.FIELD_DECL: completion.CAT_FIELD,
    ci.CursorKind.FUNCTION_DECL: completion.CAT_FUNCTION,
    ci.CursorKind.NOT_IMPLEMENTED: completion.CAT_UNKNOWN,
    ci.CursorKind.PARM_DECL: completion.CAT_PARAMETER,
    ci.CursorKind.TYPEDEF_DECL: completion.CAT_TYPE,
    ci.CursorKind.VAR_DECL: completion.CAT_VARIABLE,
    }
# translates cursor names to completion language categories


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

####################
# Main clang class #
####################


class Clang(object):

    def get_builtin_headers_path(self, library_path=None):
        # TODO: We will want to get the headers of the compiler the user is
        # using for that particular project

        if not library_path:
            # TODO: Very extremely hackish way of getting the lib path !!! Find
            # a better way
            lib = ci.Config().get_cindex_library()
            try:
                lib.foo
            except Exception, e:
                library_path = str(e).split(":")[0]

        knownPaths = [
            library_path + "/../lib/clang",  # default value
            library_path + "/../clang",      # gentoo
            library_path + "/clang",         # opensuse
            library_path + "/",              # Google
            "/usr/lib64/clang",              # x86_64 (openSUSE, Fedora)
            "/usr/lib/clang"
        ]

        for path in knownPaths:
            try:
                files = os.listdir(path)
                if len(files) >= 1:
                    files = sorted(files)
                    subDir = files[-1]
                else:
                    subDir = '.'
                path = path + "/" + subDir + "/include/"
                arg = "-I" + path
                if self.can_find_builtin_headers([arg]):
                    return path
            except:
                pass

        return None

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
        self.global_opts = []

        if not self.can_find_builtin_headers():
            hpath = self.get_builtin_headers_path()
            if not hpath:
                raise Exception("Couldn't find clang headers")
            else:
                self.global_opts = ["-I" + hpath]

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
        return self.global_opts + gps_file.project().get_attribute_as_list(
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
            t = time()
            tu = self.index.parse(file_name, args, [file_tuple], flags)
            print time() - t
        except ci.TranslationUnitLoadError:
            print "FAIL"
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
            _ = self.get_translation_unit(ed_buffer, update)

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
        ed_loc = completion.to_completion_point(ed_loc)

        file_tuple = (ed.file().name(), ed.get_chars())
        tu = self.get_translation_unit(ed, True)

        if not tu:
            return None

        cr = tu.codeComplete(
            file_tuple[0], ed_loc.line(), ed_loc.column(),
            [file_tuple], 0
        )

        return cr


#############################
# Clang completion resolver #
#############################


class ClangCompletionResolver(CompletionResolver):
    """
        A completion resolver based on clang results
    """

    def __init__(self):
        self.prefix = None
        pass

    def get_completion_prefix(self, loc):
        loc_begin = completion.to_completion_point(loc)
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

                    kind = cr.results[current_result].kind
                    if kind in kind_name_to_category:
                        language_category = kind_name_to_category[kind]
                    else:
                        language_category = completion.CAT_UNKNOWN

                    yield CompletionProposal(
                        label,
                        label,
                        " ".join([s[n].spelling for n in range(0, len)]),
                        language_category=language_category)

            current_result += 1


#######################
# Global clang module #
#######################


class Clang_Module(Module):

    clang_instance = None

    def setup(self):
        initialize_styles()

    def init_clang_instance(self):
        Clang_Module.clang_instance = Clang(GPS.Project.root())

    def gps_started(self):
        self.init_clang_instance()
        GPS.Completion.register(ClangCompletionResolver())
        ed = GPS.EditorBuffer.get()
        if ed.file().language() in ("c", "c++"):
            Clang_Module.clang_instance.refresh_buffer(ed)

    def project_changed(self):
        self.init_clang_instance()

    def project_view_changed(self):
        Clang_Module.clang_instance.invalidate_index()

    def buffer_edited(self, ed_buffer):
        Clang_Module.clang_instance.refresh_buffer(
            GPS.EditorBuffer.get(ed_buffer), True
        )

    def file_edited(self, f):
        """
        This hook is called when a new file editor is being opened
        """
        if f.language() in ("c", "c++"):
            self.clang_instance.refresh_buffer(GPS.EditorBuffer.get(f))

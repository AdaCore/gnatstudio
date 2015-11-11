"""
This is the python part of the clang integration in GPS. Python is used at the
moment for the live-diagnostics part of the clang plug-in. It doesn't parse
translation units directly, but instead uses the bridge that is the
GPS.Libclang API.
"""

import GPS
from modules import Module
import colorschemes


####################
# Main clang class #
####################


class Clang(object):

    def __init__(self):
        self.messages = []

    def get_translation_unit(self, ed_buffer, update=False):
        return GPS.Libclang.get_translation_unit(ed_buffer.file())

    def refresh_buffer(self, ed_buffer, update=False):
        f = ed_buffer.file()
        if f.language() in ("c", "c++"):
            self.add_diagnostics(ed_buffer)

    def add_diagnostics(self, ed_buffer):
        f = ed_buffer.file()
        tu = self.get_translation_unit(ed_buffer)

        # If none was got, don't do anything
        if not tu:
            return

        for m in self.messages:
            m.remove()
        self.messages = []

        for d in tu.diagnostics:
            m = GPS.Message(
                category="Clang live diagnostics",
                file=f,
                line=d.location.line,
                column=d.location.column,
                text=d.spelling,
                flags=0,
                allow_auto_jump_to_first=False
            )

            if d.severity < 3:
                m.set_action("", "gps-emblem-build-warning", d.spelling)
                m.set_style(colorschemes.STYLE_WARNING, 1)
            else:
                m.set_action("", "gps-emblem-build-error", d.spelling)
                m.set_style(colorschemes.STYLE_ERROR, 1)

            self.messages.append(m)


#######################
# Global clang module #
#######################


class Clang_Module(Module):

    clang_instance = None

    def setup(self):
        Clang_Module.clang_instance = Clang()

        # Get the current editor but don't open a new editor if none is open
        ed = GPS.EditorBuffer.get(open=False)

        if ed and ed.file().language() in ("c", "c++"):
            Clang_Module.clang_instance.refresh_buffer(ed)

    def buffer_edited(self, f):
        Clang_Module.clang_instance.refresh_buffer(GPS.EditorBuffer.get(f))

    def file_edited(self, f):
        """
        This hook is called when a new file editor is being opened
        """
        if f.language() in ("c", "c++"):
            self.clang_instance.refresh_buffer(GPS.EditorBuffer.get(f))


# We want to remove the del methods on TranslationUnit and Index, because in
# GPS, GPS has ownership of those
from clang import cindex
cindex.TranslationUnit.__del__ = lambda self: None
cindex.Index.__del__ = lambda self: None

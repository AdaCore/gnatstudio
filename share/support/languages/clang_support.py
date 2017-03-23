"""
This is the python part of the clang integration in GPS. Python is used at the
moment for the live-diagnostics part of the clang plugin. It doesn't parse
translation units directly, but instead uses the bridge that is the
GPS.Libclang API.
"""

import GPS
from modules import Module
import colorschemes

EDITOR_LOCATIONS, EDITOR, DISABLED = (
    "editor_and_locations", "editor_only", "disabled"
)

show_diags_pref = GPS.Preference("Editor/C & C++:Clang/Show diagnostics")
show_diags_pref.create(
    "Show clang live diagnostics", "enum",
    "How to show clang live diagnostics in GPS, for C/C++.",
    0, EDITOR_LOCATIONS, EDITOR, DISABLED
)

####################
# Main clang class #
####################


class Clang(object):

    def __init__(self):
        self.messages = []

    def get_translation_unit(self, ed_buffer, update=False):
        return GPS.Libclang.get_translation_unit(ed_buffer.file())

    def refresh_buffer(self, ed_buffer, update=False):
        """Refresh the information displayed in the buffer"""
        f = ed_buffer.file()

        # Do not display diagnostics if the semantic tree is not ready.
        # Rather, count on this being called in reaction to the
        # 'semantic_tree_updated' hook.
        if f.language() in ("c", "c++") and GPS.SemanticTree(f).is_ready():
            self.add_diagnostics(ed_buffer)

    def add_diagnostics(self, ed_buffer):
        """Add diagnostic information to the side of the buffer

           This will request a Translation_Unit, and therefore will be
           blocking if the Translation_Unit is not ready.
        """
        f = ed_buffer.file()

        tu = self.get_translation_unit(ed_buffer)

        # If none was got, don't do anything
        if not tu:
            return

        for m in self.messages:
            m.remove()
        self.messages = []

        for d in tu.diagnostics:
            # Skip diagnostics that are not in the current file. If diagnostic
            # has no file, it might be a config error (bad switch for example)
            # so we want to show it
            # ??? Did we mean 'name()' (or path) below, instead of just 'name'
            if not d.location.file or d.location.file.name != f.path:
                continue
            m = GPS.Message(
                category="Clang live diagnostics",
                file=f,
                line=d.location.line,
                column=d.location.column,
                text=d.spelling,
                show_in_locations=(show_diags_pref.get() == EDITOR_LOCATIONS),
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

    def is_on(self):
        return show_diags_pref.get() != DISABLED

    def refresh_current_editor(self):
        # Get the current editor but don't open a new editor if none is open
        ed = GPS.EditorBuffer.get(open=False)
        if ed and self.is_on():
            Clang_Module.clang_instance.refresh_buffer(ed)

    def setup(self):
        Clang_Module.show_diags_pref_val = show_diags_pref.get()
        Clang_Module.clang_instance = Clang()
        self.refresh_current_editor()

    def semantic_tree_updated(self, f):
        if self.is_on():
            # The file might have been opened in a QGen browser for instance
            buffer = GPS.EditorBuffer.get(f, open=False)
            if buffer:
                Clang_Module.clang_instance.refresh_buffer(buffer)

    def preferences_changed(self, *args):
        if show_diags_pref.get() != self.show_diags_pref_val:
            self.show_diags_pref_val = show_diags_pref.get()
            for m in self.clang_instance.messages:
                m.remove()
            self.refresh_current_editor()


# We want to remove the del methods on TranslationUnit and Index, because in
# GPS, GPS has ownership of those
from clang import cindex
cindex.TranslationUnit.__del__ = lambda self: None
cindex.Index.__del__ = lambda self: None

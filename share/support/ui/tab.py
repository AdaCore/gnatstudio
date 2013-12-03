"""
This plug-in provides a function to reformat the current line
or selection.
It tries to do a number of things, which can be configured
separately to suit your work style.

This function is available in GPS through the action
"Format Selection", which is bound to the <tab> key by default.
"""



import GPS
from gps_utils import interactive, in_editor
import pygps
import aliases
import align
from gi.repository import Gtk

if not GPS.Logger("PREVENT_ALIGN_ON_TAB").active:
    tabs_align_selection = GPS.Preference("Editor/tabs_align_selection")
    tabs_align_selection.create(
        "Align selection on tab", "boolean",
        "Whether <tab> should also align arrows, use clauses and assignments (:=)"
        " when multiple lines are selected.",
        True)

@interactive(name='Format selection',
             category='Editor',
             filter="Source editor",
             key='Tab')
def smart_tab():
    """
    This action is the default binding for the tab key, and will
    apply different behaviors depending on the current context.

    When expanding aliases, <tab> will move to the next field.
    Otherwise, when multiple lines are selected it will try to align
    special sequences like ":", "=>", "use" and ":=".
    Finally, it will automatically indent the selected lines.
    """

    editor = GPS.EditorBuffer.get()

    # When expanding aliases, <tab> should move to the next field

    if aliases.is_in_alias_expansion(editor):
        aliases.toggle_next_field(editor)
        return

    # If multiple lines are selected, perform various alignments

    if not GPS.Logger("PREVENT_ALIGN_ON_TAB").active:
        if tabs_align_selection.get():
            start = editor.selection_start()
            end = editor.selection_end()

            if abs(start.line() - end.line()) >= 1:
                align.align_colons()
                align.align_arrows()
                align.align_use_clauses()
                align.align_assignments()

    # Otherwise, reformat the current selection

    action = GPS.Action("Autoindent selection")
    if not action.execute_if_possible():
        editor.insert(editor.current_view().cursor(), "\t")


@interactive(name='smart escape',
             category='Editor',
             filter="Source editor",
             key='Escape')
def smart_escape():
    """
    This action is the default binding for the Escape key, and will
    interrupt the current alias expansion (if any).
    """
    editor = GPS.EditorBuffer.get()
    if aliases.is_in_alias_expansion(editor):
        aliases.exit_alias_expand(editor)

    editor.remove_all_multi_cursors()


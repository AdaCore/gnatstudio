"""Jump to the body of an Ada separate entity

This scripts adds a new contextual menu, shown when you click on
an Ada entity that is declared as "separate". If you select that
menu, an editor will be opened to show the implementation directly.

GPS's standard "Go to body" contextual menu would take you to the
editor that has the "is separate" statement instead, so this mode
makes it slightly faster to navigate if you use Ada separates a
lot
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils


def __filter(context):
    try:
        return context.entity().body() != context.entity().body(2)
    except:
        return False


@gps_utils.interactive(
    name='jump to separate body',
    filter=__filter,
    contextual="Goto separate body of %e",
    contextual_ref='goto body')
def on_goto_separate():
    """
    Jump to the actual implementation for an Ada entity that is declared as
    'separate'. Otherwise, the standard 'go to body' operation will first jump
    to the location of the 'is separate', and then to the actual
    implementation.
    """
    context = GPS.current_context()
    loc = context.entity().body(2)
    buffer = GPS.EditorBuffer.get(loc.file())
    buffer.current_view().goto(buffer.at(loc.line(), loc.column()))

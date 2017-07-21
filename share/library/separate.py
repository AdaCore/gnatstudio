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

    package Pkg is
       procedure Foo;    --  "Goto spec"
    end Pkg;

    package body Pkg is
       procedure Foo is separate;  --  "Goto separate body of"
    end Pkg;

    separate(Pkg)
    procedure Foo is     --  "Goto body"
    begin
       null;
    end Foo;
    """
    context = GPS.current_context()

    # "Goto body" seems to jump to "body(2)" in the case of a separate entity
    loc = context.entity().body(2)
    buffer = GPS.EditorBuffer.get(loc.file())
    view = buffer.current_view()
    GPS.MDI.get_by_child(view).raise_window()
    view.goto(buffer.at(loc.line(), loc.column()))

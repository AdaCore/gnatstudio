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
import os.path


def __filter(context):
    try:
        body_2 = context.entity().body(2)
        return (context.entity().body() != body_2 or
                context.entity().body(1) != body_2)
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
    entity = GPS.current_context().entity()

    # There are two possibilities: the xref engine could place the separate
    # body either in body(1) or in body(2).
    loc_1 = entity.body(1)
    loc_2 = entity.body(2)

    # To figure out which one is the most likely separate, go to the longest
    # file name. This won't work with unconventional naming schemes.
    if len(os.path.basename(loc_1.file().name())
           ) > len(os.path.basename(loc_2.file().name())):
        loc = loc_1
    else:
        loc = loc_2

    buffer = GPS.EditorBuffer.get(loc.file())
    view = buffer.current_view()
    GPS.MDI.get_by_child(view).raise_window()
    view.goto(buffer.at(loc.line(), loc.column()))

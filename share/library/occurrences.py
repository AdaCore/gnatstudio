"""Highlights all instances of current word in current editor

This script provides a GPS action which, when executed, highlights all
occurrences of the current word in the current editor.
This is a textual search, and ignores cross-references information. As
a result, if you select "Tmp" and have two variables with that names,
occurrences of the two variables will be highlighted.

This is similar to an Eclipse feature called "Dynamically marking occurrences
in file"

You can bind any shortcut you want to the action defined in this package.
This is done through the /Edit/Key Shortcuts menu. The two actions are
called /Editor/Mark Occurrences and /Editor/Remove Marked Occurrences. A new
menu is also provided in /Navigate/Mark Occurrences In File.

The resulting highlights can be removed either through the "Remove Marked
Occurrences" action, or simply by deleting the corresponding category in the
Locations window.
"""

############################################################################
# no user customization below this line
############################################################################

import GPS
from gps_utils import interactive
from text_utils import get_selection_or_word

GPS.Preference("Plugins/occurrences/color").create(
    "Highlight Color", "color",
    """color used to highlight matching occurrences.
you must restart gps to take changes into account""",
    "lightblue")


def on_gps_started(hook_name):
    GPS.Editor.register_highlighting(
            "dynamic occurrences",
            GPS.Preference("Plugins/occurrences/color").get(),
            True)


@interactive("Editor", filter="Source editor", name="mark occurrences",
             menu="/Navigate/Mark Occurrences In File")
def mark_selected():
    """Mark all the occurrences of the selected element in the current editor.
       Does nothing if multiple lines are selected"""
    (buffer, start, end) = get_selection_or_word()
    selection = buffer.get_chars(start, end)

    if selection != "":
        for m in buffer.file().search(selection, regexp=False):
            GPS.Locations.add("Local occurrences",
                              m.file(), m.line(), m.column(),
                              selection,
                              highlight="dynamic occurrences",
                              length=len(selection))


@interactive("Editor", filter="Source editor",
             name="remove marked occurrences")
def unmark_selected():
    GPS.Locations.remove_category("Local occurrences")

GPS.Hook("gps_started").add(on_gps_started)

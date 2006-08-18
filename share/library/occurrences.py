"""Highlights all instances of current word in current editor

This script provides a GPS action which, when executed, hilights all
occurrences of the current word in the current editor.
This is a textual search, and ignores cross-references information. As
a result, if you select "Tmp" and have two variables with that names,
occurrences of the two variables will be highlighted.

This is similar to an Eclipse feature called "Dynamically marking occurrences
in file"

You can bind any shortcut you want to the action defined in this package.
This is done through the /Edit/Key Shortcuts menu.

The resulting highlights can be removed either through the "Remove Marked
Occurrences" action, or simply by deleting the corresponding category in the
Locations window.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

menu_name = "/Navigate/Mark Occurrences in File"
mark_action_name = "Mark occurrences"
remove_action_name = "Remove marked occurrences"

highlight_color = "lightblue"


############################################################################
## No user customization below this line
############################################################################

import GPS

def on_gps_started (hook_name):
  GPS.parse_xml ("""
  <action name='""" + mark_action_name + """' category="Editor">
     <filter id="Source editor" />
     <description>Mark all the occurrences of the selected element in the current editor</description>
     <shell lang="python" >occurrences.mark_selected()</shell>
  </action>

  <action name='""" + remove_action_name + """' category="Editor">
     <filter id="Source editor" />
     <description>Remove all highlightings done through Mark Occurrences</description>
     <shell lang="python" >occurrences.unmark_selected()</shell>
  </action>

  <menu action='""" + mark_action_name + """'>
     <title>""" + menu_name + """</title>
  </menu>""")

  GPS.Editor.register_highlighting \
    ("dynamic occurrences", highlight_color, True)

def mark_selected ():
   buffer = GPS.EditorBuffer.get()
   selection = buffer.get_chars (buffer.selection_start(), buffer.selection_end() - 1)
   context=GPS.current_context()

   if selection=="" and context.__class__ == GPS.EntityContext:
      selection=context.entity().name()

   if selection != "":
      for m in buffer.file().search (selection):
         GPS.Locations.add ("Local occurrences", m.file(), m.line(), m.column(),
              selection, highlight="dynamic occurrences", length=len(selection))

def unmark_selected ():
   GPS.Locations.remove_category ("Local occurrences")


GPS.Hook ("gps_started").add (on_gps_started)

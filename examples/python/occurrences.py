# This file provides an action that highlights all instances of the
# selected element in the current editor.
# Eclipse calls this "Dynamically marking occurrences in file"
#
# It adds an action to which keys can be bound through Edit->Key Shortcuts
# menu, as well as a new menu Navigate->Mark Occurrences In File
# The resulting highlighting can be either removed through the Remove Marked
# Occurrences action, or simply by deleting the category through the locations
# window

import GPS

GPS.parse_xml ("""
  <action name="Mark occurrences" category="Editor">
     <filter id="Source editor" />
     <description>Mark all the occurrences of the selected element in the current editor</description>
     <shell lang="python" >occurrences.mark_selected()</shell>
  </action>

  <action name="Remove marked occurrences" category="Editor">
     <filter id="Source editor" />
     <description>Remove all highlightings done through Mark Occurrences</description>
     <shell lang="python" >occurrences.unmark_selected()</shell>
  </action>

  <Submenu>
     <Title>Navigate</Title>
     <menu action="Mark occurrences">
        <Title>Mark Occurrences in File</Title>
     </menu>
  </Submenu>""")


GPS.Editor.register_highlighting ("dynamic occurrences", "lightblue", True)

def mark_selected ():
   selection=GPS.Editor.get_chars (GPS.current_context().file().name())
   context=GPS.current_context()

   if selection=="" and context.__class__ == GPS.EntityContext:
      selection=context.entity().name()
   if selection != "":
      for m in context.file().search (selection):
         GPS.Locations.add ("Local occurrences", m.file(), m.line(), m.column(),
              selection, highlight="dynamic occurrences", length=len(selection))

def unmark_selected ():
   GPS.Locations.remove_category ("Local occurrences")


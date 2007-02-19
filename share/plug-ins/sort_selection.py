"""Provides sort functions in the editors

This file provides two sort functions, which can be used to sort lines
in a source file.
To use: first select the lines that you wish to sort, and then select
one of the two menus:
  - /Edit/Sort Ascending
  - /Edit/Sort Descending
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Scripts)

menu_name1 = "/Edit/Sort Descending"
menu_name2 = "/Edit/Sort Ascending"


############################################################################
## No user customization below this line
############################################################################

import GPS
import string

def on_gps_started (hook_name):
  GPS.parse_xml ("""
  <action name="sort selected lines ascending" output="none" category="Editor">
     <filter id="Source editor" />
     <description>Sorts current selection</description>
     <shell lang="python" >sort_selection.sort_selection(0)</shell>
  </action>

  <menu action="sort selected lines ascending" after="Refill">
     <title>""" + menu_name2 + """</title>
  </menu>

   <action name="sort selected lines descending" output="none" category="Editor">
      <filter id="Source editor" />
      <description>Sorts current selection</description>
      <shell lang="python" >sort_selection.sort_selection(1)</shell>
   </action>

  <menu action="sort selected lines descending" after="Refill">
     <title>""" + menu_name1 + """</title>
  </menu>
""")

def sort_selection (revert):
   """Sorts the current selection, either in ascending order if revert is 0
      or in descending order otherwise"""
   context = GPS.current_context ();
   ed      = GPS.EditorBuffer.get (context.file())
   start   = ed.selection_start()
   to      = ed.selection_end()

   # If the end is at the first column we really want to sort the lines
   # before the current one.

   if to.column() == 1:
	to = to.forward_char (-1)

   selection = ed.get_chars (start, to)

   if selection == "" or context.__class__ == GPS.EntityContext:
      return;
      
   lines = string.split (selection,"\n");
   # strip off extraneous trailing "" line
   lines = lines[:-1];
   lines.sort ();
   if revert:
      lines.reverse ();
   ed.start_undo_group()
   ed.delete (start, to)
   ed.insert (start, "\n".join (lines) + "\n")
   ed.finish_undo_group()

GPS.Hook ("gps_started").add (on_gps_started)

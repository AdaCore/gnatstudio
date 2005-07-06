### Support file for the GNAT Programm System
##
## This file provides two sort functions, which can be used to sort lines
## in a source file.
## It is used by selecting the lines to sort, and then selecting one the
## menus in Edit/Sort Ascending or Edit/Sort Descending


import GPS
import string

GPS.parse_xml ("""
  <action name="sort selected lines ascending" output="none">
     <filter id="Source editor" />
     <description>Sorts current selection</description>
     <shell lang="python" >sort_selection.sort_selection(0)</shell>
  </action>

  <menu action="sort selected lines ascending">
     <title>/Edit/Sort Ascending</title>
  </menu>

   <action name="sort selected lines descending" output="none">
      <filter id="Source editor" />
      <description>Sorts current selection</description>
      <shell lang="python" >sort_selection.sort_selection(1)</shell>
   </action>

  <menu action="sort selected lines descending">
     <title>/Edit/Sort Descending</title>
  </menu>
""")

def sort_selection (revert):
   """Sorts the current selection, either in ascending order if revert is 0
      or in descending order otherwise"""
   context = GPS.current_context ();
   ed      = GPS.EditorBuffer.get (context.file())
   start   = ed.selection_start()
   to      = ed.selection_end()
   selection = ed.get_chars (start, to)

   if selection == "" and context.__class__ == GPS.EntityContext:
      return;
      
   if selection != "":
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


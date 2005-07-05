import GPS
import string

GPS.parse_xml ("""
  <action name="sort selected lines ascending" output="none">
     <filter id="Source editor" />
     <description>Sorts current selection</description>
     <shell lang="python" >sort_selection.sort_selected_ascending()</shell>
  </action>

  <key action="sort selected lines ascending">F12</key>
  <menu action="sort selected lines ascending">
     <title>/Edit/Sort Ascending</title>
  </menu>

   <action name="sort selected lines descending" output="none">
      <filter id="Source editor" />
      <description>Sorts current selection</description>
      <shell lang="python" >sort_selection.sort_selected_descending()</shell>
   </action>

  <key action="sort selected lines descending">shift-F12</key>
  <menu action="sort selected lines descending">
     <title>/Edit/Sort Descending</title>
  </menu>
""")


def sort_selected_ascending ():
   """Sorts the selected text alphabetically in ascending order"""
   context = GPS.current_context ();
   selection = GPS.Editor.get_chars (context.file().name());   
   if selection == "" and context.__class__ == GPS.EntityContext:
      return;
      
   if selection != "":
      lines = string.split (selection,"\n");
      # strip off extraneous trailing "" line
      lines = lines[:-1];
      lines.sort ();
      GPS.Editor.cut ();
      for line in lines: 
         GPS.Editor.insert_text (line + "\n");


def sort_selected_descending ():
   """Sorts the selected text alphabetically in descending order"""
   context = GPS.current_context ();
   selection = GPS.Editor.get_chars (context.file().name());   
   if selection == "" and context.__class__ == GPS.EntityContext:
      return;
      
   if selection != "":
      lines = string.split (selection,"\n");
      # strip off extraneous trailing "" line
      lines = lines[:-1];
      lines.sort ();
      # the following call is the only difference between these functions
      lines.reverse ();
      GPS.Editor.cut ();
      for line in lines: 
         GPS.Editor.insert_text (line + "\n");


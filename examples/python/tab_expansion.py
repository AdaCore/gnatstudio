# This module defines functions that insert spaces to implement tab stops
# based on language-specific indentation preferences.
#
# For example, using Ada, say the user sets the Ada indentation preference
# to 3: the logical tab stops will then be at columns 4, 7, 10, 13, and so
# on.  Pressing the tab key at any point on a line will emit the required
# number of blanks so that the cursor will be placed at the next stop on
# that line.  Any text to the right of the cursor will be shifted further
# to the right by the number of blanks inserted.
#
# Ada, C, and C++ are supported.  Each language uses the Tab key without
# conflict because the corresponding actions have an associated language
# filter.  The action is only applied if the language for the current buffer
# matches the action's specified language.

import GPS

#
# Ada 
#

def goto_Ada_tab_stop ():
   """Insert spaces to move to the next logical tab stop, where tab stops
   are based strictly on the user's indentation preference for Ada"""
   current_file = GPS.current_context().file().name();
   current_column = GPS.Editor.cursor_get_column (current_file);
   indent = GPS.Preference ("Ada-Indent-Level").get();
   next_tab_stop = (((current_column + indent - 1) / indent) * indent) + 1;
   offset = next_tab_stop - current_column;
   GPS.Editor.insert_text (' ' * offset);


GPS.parse_xml ("""
   <action name="goto next Ada tab stop" output="none" category="Ada">
      <description>Inserts text to next tab stop defined by user's Ada indentation preference</description>
      <filter module="Source_Editor" language="Ada"/>
      <shell lang="python">tab_expansion.goto_Ada_tab_stop()</shell>
   </action>
   <key action="goto next Ada tab stop">Tab</key>
""")


#
# C and C++
#

def goto_C_tab_stop ():
   """Insert spaces to move to the next logical tab stop, where tab stops
   are based strictly on the user's indentation preference for C/C++"""
   current_file = GPS.current_context().file().name();
   current_column = GPS.Editor.cursor_get_column (current_file);
   indent = GPS.Preference ("C-Indent-Level").get();
   next_tab_stop = (((current_column + indent - 1) / indent) * indent) + 1;
   offset = next_tab_stop - current_column;
   GPS.Editor.insert_text (' ' * offset);


GPS.parse_xml ("""
   <action name="goto next C tab stop" output="none">
      <description>Inserts text to next tab stop defined by user's C/C++ indentation preference</description>
      <filter module="Source_Editor" language="C"/>
      <shell lang="python">tab_expansion.goto_C_tab_stop()</shell>
   </action>
   <key action="goto next C tab stop">Tab</key>
""")


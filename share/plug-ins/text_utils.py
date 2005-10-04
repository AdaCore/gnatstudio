## This module implements high level actions related to text editing

import GPS
import string
import navigation_utils

##  Variable scope

mark_line = 0
mark_col  = 0
mark_set  = False

## Register the actions
GPS.parse_xml ("""
   <action name="subprogram box" output="none">
      <filter_and>
         <filter id="Source editor" />
         <filter language="ada" />
      </filter_and>
      <shell lang="python">text_utils.add_subprogram_box()</shell>
   </action>

   <action name="next line" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.next_line(1)</shell>
   </action>

   <action name="previous line" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.next_line(-1)</shell>
   </action>

   <!-- There is currently no way to know what is the visible part of
        a buffer so scroll down goes only 15 line down -->
   <action name="scroll down" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.next_line(15)</shell>
   </action>

   <!-- Idem for scroll up -->
   <action name="scroll up" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.next_line(-15)</shell>
   </action>

   <action name="kill line" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.kill_line()</shell>
   </action>

   <action name="open line" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.open_line()</shell>
   </action>

   <action name="kill forward" output="none">
      <filter id="Source editor" />
      <shell>current_context</shell>
      <shell>FileContext.file %1</shell>
      <shell>File.name %1</shell>
      <shell>Editor.cursor_get_line %1</shell>
      <shell>Editor.cursor_get_column %2</shell>
      <shell>Editor.replace_text %3 %2 %1 "" 0 1</shell>
   </action>

   <action name="transpose chars" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.transpose_chars()</shell>
   </action>

   <action name="save current editor"  output="none">
      <filter id="Source editor" />
      <shell>current_context</shell>
      <shell>FileContext.file %1</shell>
      <shell>File.name %1</shell>
      <shell>Editor.save_buffer %1</shell>
   </action>

   <action name="goto beginning of line" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.goto_beginning_of_line()</shell>
   </action>

   <action name="goto end of line" output="none">
      <filter id="Source editor" />
      <shell lang="python" >text_utils.goto_end_of_line()</shell>
   </action>

   <action name="goto beginning of buffer" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.beginning_of_buffer()</shell>
   </action>

   <action name="goto end of buffer" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.end_of_buffer()</shell>
   </action>

   <action name="set mark" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.set_mark_command()</shell>
   </action>
   <key action="set-mark-command">control-space</key>

   <action name="kill region" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.kill_region()</shell>
   </action>

   <action name="kill ring save"  output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.kill_ring_save()</shell>
   </action>

   <action name="center cursor on screen"  output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.center_cursor()</shell>
   </action>
""")

def add_subprogram_box():
   """ Insert in the current editor a box just before the current subprogram
       starts """
   match = navigation_utils.__find_subprogram_decl()
   if match[0] != None:
      prefix = ' ' * len (match[0].group (1))
      box = prefix + ('-' * (6 + len (match[0].group (3)))) + "\n"
      GPS.Editor.replace_text (GPS.current_context().file().name(), match[1], 1,
                    box + prefix + "-- " + match[0].group (3) + " --\n" + box + "\n",
                    0, 0)

def next_line(nb_line):
   """  Move cursor vertically nb_line down . """
##        Use this function since binding a key to ""Move to next line""
##        does not work after a search.
   file = GPS.current_context().file().name()
   line_to_go = GPS.current_context().location().line() + nb_line
   last_line = GPS.Editor.get_last_line(file)
   col  = GPS.current_context().location().column()
   result_line = GPS.current_context().location().line()
   if line_to_go > last_line:
      end_of_line (file, last_line)
   elif line_to_go < 1:
      GPS.Editor.cursor_set_position (file, 1, 1)
   else:
      str = GPS.Editor.get_chars (file, line_to_go, 1, 0)
      if col > len (str):
         end_of_line (file, line_to_go)
      else:
         GPS.Editor.cursor_set_position (file, line_to_go, col)

def kill_line():
   """ Kills the end-of-line, or the whole line if it is empty or contains
       only white spaces. This is a better emulation of Emacs's behavior
       than the one provided by default by gtk+, which doesn't handle
       whitespaces correctly  """
   # Will not work in new files, since there is no file name set. We would
   # need to access directly the current editor instead of current context.
   file = GPS.current_context().file().name()
   line = GPS.current_context().location().line()
   col  = GPS.current_context().location().column()
   # get the characters following the current location
   str = GPS.Editor.get_chars (file, line, col, 0)
   strip_str = string.rstrip (str)

   # case when there is no non-blank caracters after the
   # cursor and an end-of-line => kill all non-blank
   # caracters and the end-of-line
   if strip_str == "" and str [len (str) - 1] == '\n':
      GPS.Editor.select_text (line, line, col, col + len (str))
   # there are non-blank caracters => kill them until the end-of-line if one
   elif str [len (str) - 1] == '\n':
      GPS.Editor.select_text (line, line, col, col + len (str) - 1)
   else:
      GPS.Editor.select_text (line, line, col, col + len (str))
   GPS.Editor.cut()

def beginning_of_buffer():
   """  Move the cursor to the beginning of the buffer. """
   file = GPS.current_context().file().name()
   GPS.Editor.edit (file, 1, 1)

def end_of_buffer():
   """  Move the cursor to the end of the buffer. """
   file = GPS.current_context().file().name()
   line = GPS.Editor.get_last_line(file)
   str  = GPS.Editor.get_chars (file, line, 1)
   if str == "":
      GPS.Editor.edit (file, line, 1)
   else:
      GPS.Editor.edit (file, line, 1 + len (str))

def goto_beginning_of_line():
   """  Goto the beginning of line. """
   try:
      file = GPS.current_context().file().name()
      line = GPS.current_context().location().line()
      GPS.Editor.cursor_set_position (file, line, 1)
   except:
      pass

def end_of_line(file, line):
   """   Goto to the end of the line in file. """
   str  = GPS.Editor.get_chars (file, line, 1, 0)
   # we must test this special case if we want to test
   # if there is an end of line character in str
   if str == "":
      GPS.Editor.cursor_set_position (file, line, 1)
   # test if there is an end of line character
   elif str [len (str) - 1] == '\n':
      GPS.Editor.cursor_set_position (file, line, len (str))
   else:
      GPS.Editor.cursor_set_position (file, line, len (str) + 1)

def goto_end_of_line():
   """   Goto the end of line. """
   try:
      file = GPS.current_context().file().name()
      line = GPS.current_context().location().line()
      end_of_line (file, line);
   except:
      pass

def transpose_chars():
   """Interchange characters around cursor, moving forward one character. """
   # This procedure gives the same behaviour as the emacs one except on
   # end of lines

   file = GPS.current_context().file().name()
   line = GPS.current_context().location().line()
   col  = GPS.current_context().location().column()
   str  = GPS.Editor.get_chars (file, line, col, 1, 1)
   # do nothing when the cursor is on the first character of the file
   if len (str) == 2:
      GPS.Editor.replace_text (file, line, col, str [1] + str[0], 1, 1)

def open_line():
   """ Insert a newline and leave cursor before it."""
   file = GPS.current_context().file().name()
   line = GPS.current_context().location().line()
   col  = GPS.current_context().location().column()
   GPS.Editor.insert_text ("\n")
   GPS.Editor.cursor_set_position (file, line, col)

## Try to reproduce the way emacs use the cut & paste
## using a global mark

def set_mark_command():
    """  Set mark where cursor is. """
    global mark_line
    global mark_col
    global mark_set
    file = GPS.current_context().file().name()
    mark_line = GPS.current_context().location().line()
    mark_col  = GPS.current_context().location().column()
    mark_set  = True

def kill_region():
    """ Kill betwenn cursor and mark. """

    global mark_line
    global mark_col
    global mark_set

    if mark_set:
        file = GPS.current_context().file().name()
        line = GPS.current_context().location().line()
        col  = GPS.current_context().location().column()

        GPS.Editor.select_text (line, mark_line, col, mark_col)
        GPS.Editor.cut()

def kill_ring_save():
    """ Save the region as if killed, but don't kill it. """

    global mark_line
    global mark_col
    global mark_set

    if mark_set:
        file = GPS.current_context().file().name()
        line = GPS.current_context().location().line()
        col  = GPS.current_context().location().column()

        GPS.Editor.select_text (line, mark_line, col, mark_col)
        GPS.Editor.copy()
        GPS.Editor.cursor_set_position (file, line, col)

def center_cursor():
   """ Insert a newline and leave cursor before it."""
   file = GPS.current_context().file().name()
   line = GPS.current_context().location().line()
   GPS.Editor.edit (file, line)

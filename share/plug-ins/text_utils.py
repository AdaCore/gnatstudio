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

   <action name="set mark command" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.set_mark_command()</shell>
   </action>

   <action name="kill region" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.kill_region()</shell>
   </action>

   <action name="kill ring save"  output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.kill_ring_save()</shell>
   </action>

   <action name="delete horizontal space" output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.delete_horizontal_space()</shell>
   </action>

   <action name="center cursor on screen"  output="none">
      <filter id="Source editor" />
      <shell lang="python">text_utils.center_cursor()</shell>
   </action>
""")

## The blocks for which we want to display boxes
subprogram_box_blocks={}
for b in ["CAT_PROCEDURE", "CAT_FUNCTION", "CAT_ENTRY",
          "CAT_PROTECTED", "CAT_TASK", "CAT_PACKAGE"]:
  subprogram_box_blocks[b]=1

def add_subprogram_box():
   """ Insert in the current editor a box just before the current subprogram
       starts """

   buffer  = GPS.EditorBuffer.get ()
   loc     = buffer.current_view().cursor()
   initial = loc.create_mark()
   min     = buffer.beginning_of_buffer()

   while (not subprogram_box_blocks.has_key (loc.block_type())) and (loc > min):
      loc = loc.block_start() - 1

   if loc > min:
      name = loc.block_name()
      loc = loc.block_start().beginning_of_line();
      dashes = '-' * (len (name) + 6)
      box = dashes + "\n" + "-- " + name + " --\n" + dashes + "\n\n" 

      # Start an undo group so that the whole process can be undone with a
      # single click
      buffer.start_undo_group()
      buffer.insert (loc, box)
      buffer.indent (loc, loc.forward_line (3))
      buffer.current_view().goto (initial.location())
      buffer.finish_undo_group()
   
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

########################################
## Implementation of kill_line
## The goal is to have an implementation as close to Emacs as possible.
## This means that the killed line must be added to the clipboard, and
## that multiple successive calls at the same location should merge
## their clipboard entries
########################################

last_kill_line = None
moved_by_kill_line = False

def kill_line():
   """ Kills the end-of-line, or the whole line if it is empty or contains
       only white spaces. This is a better emulation of Emacs's behavior
       than the one provided by default by gtk+, which doesn't handle
       whitespaces correctly.
       When called several times from the same line, entries are appended in
       the clipboard"""
   global last_kill_line
   global moved_by_kill_line
   buffer   = GPS.EditorBuffer.get()
   start    = buffer.current_view().cursor()
   append   = last_kill_line and last_kill_line == start
   last_kill_line     = start
   moved_by_kill_line = True

   # In case the current location points to a line terminator we just cut it
   if start.get_char() == "\n":
      buffer.cut (start, start, append)
   else:
      end       = start.end_of_line ()
      str       = buffer.get_chars (start, end)
      strip_str = str.rstrip ()
      if len (str) > 0 and str [len (str) - 1] == '\n' and strip_str != "":
         end = end.forward_char (-1)
      buffer.cut (start, end, append)

def on_location_changed (hook, file, line, column):
   ## This hook is called asynchronously: it will be called after kill_line
   ## has finished executing, in which case we do not want to reset the
   ## variable, so that multiple calls at the same location append to the
   ## clipboard
   global last_kill_line
   global moved_by_kill_line
   if not moved_by_kill_line:
      last_kill_line = None
   moved_by_kill_line = False
GPS.Hook ("location_changed").add (on_location_changed)

################################################
## Moving the cursor
################################################

def beginning_of_buffer():
   """Move the cursor to the beginning of the buffer"""
   buffer = GPS.EditorBuffer.get()
   buffer.current_view().goto (buffer.beginning_of_buffer())

def end_of_buffer():
   """Move the cursor to the end of the buffer"""
   buffer = GPS.EditorBuffer.get()
   buffer.current_view().goto (buffer.end_of_buffer())

def goto_beginning_of_line():
   """Goto the beginning of line"""
   view = GPS.EditorBuffer.get().current_view()
   view.goto (view.cursor().beginning_of_line())

def end_of_line(file, line):
   """Goto to the end of the line in file"""
   buffer = GPS.EditorBuffer.get (GPS.File (file))
   loc  = GPS.EditorLocation (buffer, line, 1)
   buffer.current_view().goto (loc.end_of_line() - 1)

def goto_end_of_line():
   """Goto the end of line"""
   view = GPS.EditorBuffer.get().current_view()
   view.goto (view.cursor().end_of_line())

def is_space (char):
   return char == ' ' or char == '\t'

def delete_horizontal_space(backward=1, forward=1):
   """Delete all spaces and tabs around the cursor in the current editor.
The two parameters can be used to control in what directions white spaces are
searched for"""
   buffer = GPS.EditorBuffer.get()
   start = buffer.current_view().cursor()
   end = start
   if forward:
      max = end.end_of_line()
      while is_space (end.get_char()) and end < max:
        end = end.forward_char(1)
      if end != max:
        end = end.forward_char(-1)
   if backward:
      max = start.beginning_of_line()
      while is_space (start.get_char()) and start > max:
        start = start.forward_char (-1)
      if start != max:
        start = start.forward_char (1)
   buffer.delete (start, end)

def transpose_chars():
   """Transpose characters around cursor, moving forward one character. """
   buffer = GPS.EditorBuffer.get()
   cursor = buffer.current_view().cursor()
   if cursor > buffer.beginning_of_buffer():
      c = cursor.get_char()
      buffer.start_undo_group()
      buffer.delete (cursor, cursor)
      buffer.insert (cursor - 1, c)
      buffer.current_view().goto (cursor + 1)
      buffer.finish_undo_group()

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

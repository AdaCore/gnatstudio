"""This file provides support for rectangle-mode in an editor.

   In particular, it is possible to select a rectangular area,
   cut it, and paste it elsewhere.

   The highlighting of the selection itself is done on the whole
   lines, not on the rectangle itself, but the rectangle is the
   part between the column of the start of the selection, and the
   column of the end of the selection. For instance:

       ABCD
       EFGH
       IJKL

   If the selection goes from B to K, the rectangle will be:
        BC
        FG
        JK
   However, the highlighting will include:
        BCD
       EFGH
       IJK

   Cutting the rectangle will result in:
       AD
       EH
       IL

   If you move the cursor before "A", and paste the rectangle:
       BCAD
       FGEH
       JKIL
"""


############################################################################
## No user customization below this line
############################################################################

from GPS import *
import traceback

def rectangle_delete (menu):
    """Delete the selected rectangle"""

    buffer = EditorBuffer.get ()
    apply_on_rectangle (rectangle_cut_func, buffer.selection_start(),
                        buffer.selection_end() - 1, False, False)

def rectangle_cut (menu):
    """Cut the selected rectangle into the clipboard"""

    buffer = EditorBuffer.get ()
    apply_on_rectangle (rectangle_cut_func, buffer.selection_start(),
                        buffer.selection_end() - 1, True, False)

def rectangle_copy (menu):
    """Copy the selected rectangle into the clipboard"""

    buffer = EditorBuffer.get ()
    start  = buffer.selection_start().create_mark()
    end    = buffer.selection_end().create_mark()
    apply_on_rectangle (rectangle_cut_func, buffer.selection_start(),
                        buffer.selection_end() - 1, True, True)
    buffer.select (start.location(), end.location())

def rectangle_paste (menu):
    """Paste the last entry in the clipboard as a rectangle in the current editor"""

    try:
      buffer = EditorBuffer.get ()
      start = buffer.current_view ().cursor ()
      selection = Clipboard.contents () [Clipboard.current()]

      buffer.start_undo_group ()
      for line in selection.splitlines():
         buffer.insert (start, line)
         start = EditorLocation (buffer, start.line() + 1, start.column())
      buffer.finish_undo_group ()

    except:
      Logger ("RECTANGLE").log ("Unexpected exception: " + traceback.format_exc())

def rectangle_open (menu):
    """Insert blank spaces to fill the selected rectangle.
       This pushes its text to the right"""

    buffer = EditorBuffer.get ()
    start  = buffer.selection_start ()
    end    = buffer.selection_end ()
    rectangle_insert (menu, " " * (end.column() - start.column()))

def rectangle_insert (menu, text=None):
    """Insert TEXT at the beginning of each line of the selected rectangle.
       If TEXT is unspecified, an interactive dialog is open"""
    buffer = EditorBuffer.get ()
    if not text:
       text = MDI.input_dialog ("Text to insert before each line:", "")
       if not text: return
       text = text [0]
    apply_on_rectangle (rectangle_insert_func, buffer.selection_start(),
                        buffer.selection_end() - 1, text)

def rectangle_clear (menu):
    """Replaces the contents of the rectangle with spaces"""

    buffer = EditorBuffer.get()
    start  = buffer.selection_start ()
    end    = buffer.selection_end ()
    apply_on_rectangle (rectangle_replace_func, start, end - 1,
                        " " * abs (end.column() - start.column()))

def rectangle_string (menu, text=None):
    """Replaces the contents of the rectangle with TEXT on each line.
       If TEXT is narrower or wider than the rectangle, the text is shifted
       right or left as appropriate.
       If TEXT is unspecified, an interactive dialog is open."""

    buffer = EditorBuffer.get()
    start  = buffer.selection_start ()
    end    = buffer.selection_end ()
    if not text:
       text = MDI.input_dialog ("Text to replace each line with:", "")
       if not text: return
       text = text [0]
    apply_on_rectangle (rectangle_replace_func, start, end - 1, text)

def on_gps_started (hook_name):
    """Create the menus associated with this module"""

    Menu.create ("/Edit/Rectangle",
                 ref        = "Redo",
                 add_before = False)
    Menu.create ("/Edit/Rectangle/Cut",    on_activate = rectangle_cut)
    Menu.create ("/Edit/Rectangle/Copy",   on_activate = rectangle_copy)
    Menu.create ("/Edit/Rectangle/Paste",  on_activate = rectangle_paste)
    Menu.create ("/Edit/Rectangle/-")
    Menu.create ("/Edit/Rectangle/Delete", on_activate = rectangle_delete)
    Menu.create ("/Edit/Rectangle/Clear",  on_activate = rectangle_clear)
    Menu.create ("/Edit/Rectangle/Open",   on_activate = rectangle_open)
    Menu.create ("/Edit/Rectangle/Replace with Text", on_activate = rectangle_string)
    Menu.create ("/Edit/Rectangle/Insert Text", on_activate = rectangle_insert)

##############################################################################
## No public function below this
##############################################################################

def rectangle_cut_func (rect_start, rect_end, start, end, in_clipboard, copy):
    """Removes or Copies the range START .. END, and copy it in the clipboard if
       IN_CLIPBOARD is true"""

    if in_clipboard:
       if copy:
          start.buffer().copy (start, end, append=(start != rect_start))
       else:
          start.buffer().cut (start, end, append=(start != rect_start))
       if end != rect_end:
          Clipboard.copy (text="\n", append=True)
    else:
       start.buffer().delete (start, end)

def rectangle_replace_func (rect_start, rect_end, start, end, text):
    """Replaces the range START .. END with TEXT"""
    start.buffer().delete (start, end)
    start.buffer().insert (start, text)

def rectangle_insert_func (rect_start, rect_end, start, end, text):
    """Fills the range START .. END with spaces and moves the text rightward"""
    start.buffer().insert (start, text)

def apply_on_rectangle (func, start, end, *args):
   """Applies FUNC for each line segment in the rectangle starting at
      the EditorLocation position START and ending at END.
      START and END must of course point to the same buffer.
      FUNC is called wth four parameters of type EditorLocation for the
      total range of the rectangle and the range that FUNC should apply on.
      Any additional parameter specified in ARGS is passed to FUNC."""

   try:
     if start.buffer() == end.buffer():
        if start > end:
           current = start
           start   = end
           end     = current
        else:
           current = start

        start.buffer().start_undo_group()
        while current < end:
           endcolumn = EditorLocation (current.buffer(), current.line(), end.column())
           func (start, end, current, endcolumn, *args)
           if current.line() == start.buffer().lines_count():
              current = end
           else:
              current = EditorLocation (start.buffer(), current.line() + 1, start.column())
        start.buffer().finish_undo_group()
   except:
      Logger ("RECTANGLE").log ("Unexpected exception: " + traceback.format_exc())


Hook ("gps_started").add (on_gps_started)

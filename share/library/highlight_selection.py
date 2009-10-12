"""
When a text is selected, all other occurrences in the same file are
also highlighted
"""

from GPS import *
from gps_utils.highlighter import *

min_length = 2
# Minimal length of the selection before we start highlighting.
# For efficiency reasons, it isn't recommended to highlight when there is
# a single character selected.

class Editor_Highlighter (Text_Highlighter):
   def __init__ (self, name, text, buffer,
                context_lines=0,
                fg_color="grey", bg_color="",
                weight="",   # or "bold", "normal", "light"
                style="",    # or "normal", "oblique", "italic"
                editable=True): # or None

      self.buffer = buffer

      Text_Highlighter.__init__ (
         self, name=name, text=text, context_lines=context_lines,
         fg_color=fg_color, bg_color=bg_color,
         weight=weight, style=style, editable=editable)

   def must_highlight (self, buffer):
      return self.buffer == buffer

def on_location_changed (hook, file, line, column):
   buffer = EditorBuffer.get (file)
   if not buffer:
      return

   start = buffer.selection_start ()
   end   = buffer.selection_end ()

   # Highlight all other occurrences of the selection if needed. This
   # automatically removes any previous highlighting done for the selection.
   # For efficiency, don't do anything if there are less than two characters
   # selected

   if start != end and abs (start.offset() - end.offset()) >= min_length:
      buffer.selection_highlighter = Editor_Highlighter (
         name="selection_occurrences",
         text   = buffer.get_chars (start, end - 1),
         buffer = buffer,
         bg_color = Preference ("Plugins/highlight_selection/bgcolor").get (),
         fg_color = Preference ("Plugins/highlight_selection/fgcolor").get (),
         weight = "normal")

   else:
      try:
         # Remove highlighting for the previous selection
         buffer.selection_highlighter.stop ()
         buffer.selection_highlighter = None
      except:
         pass

Preference ("Plugins/highlight_selection/bgcolor").create (
   "Background color", "color",
   "Background color used to highlight other occurrences of the current"
   + " selection", "#CFCFFA")
Preference ("Plugins/highlight_selection/fgcolor").create (
   "Foreground color", "color",
   "Foreground color used to highlight other occurrences of the current"
   + " selection", "black")

Hook ("location_changed").add (on_location_changed)

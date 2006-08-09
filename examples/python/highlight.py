# This script provides a new toolbar button to highlight Ada specific
# entities, like types and packages.
# This is just a quick example, and will not do the highlighting on the
# fly. Instead, you'll need to press the button regularly to recompute
# the highlighting

from GPS import *

parse_xml ("""<action name="Highlight Ada Types">
    <filter id="Source editor" />
    <shell lang="python">highlight.highlight_ada_types()</shell>
 </action>
 <button action="Highlight Ada Types">
    <title>Highlight Ada Types</title>
 </button>""")


overlay = None

def highlight_ada_types():
   global overlay
   buffer = EditorBuffer.get ()
   file = buffer.file()

   # Remove old highlights
   if overlay:
      loc = buffer.beginning_of_buffer ()
      is_on = loc.has_overlay (overlay)
      end = buffer.end_of_buffer ()
      while loc < end:
        loc2 = loc.forward_overlay (overlay)
        if is_on:
           buffer.remove_overlay (overlay, loc, loc2)
        is_on = not is_on
        loc = loc2

   # Create the overlay if it doesn't exist
   if not overlay:
      overlay = buffer.create_overlay ("Ada types")
      overlay.set_property ("foreground", "magenta") 

   # Highlight again
   for fileloc in file.search \
    ("(type|package)(\s+body)?\s+(\S+)\s+is", regexp=True, scope="code"):
      loc = EditorLocation (buffer, fileloc.line(), fileloc.column())

      if loc.get_char() == 't': loc = loc + 5
      else:                     loc = loc + 7

      loc2 = loc.forward_word()
      while loc2.get_char() == '_' or loc2.get_char() == '.':
         loc2 = (loc2 + 1).forward_word()
      buffer.apply_overlay (overlay, loc, loc2)
   

"""This script saves the cursor location when an editor is closed, and
   restore it when the editor is reopened later on"""


############################################################################
## No user customization below this line
############################################################################

from GPS import *

def on_file_closed (hook, file):
   buffer = EditorBuffer.get (file)
   line   = buffer.current_view().cursor().line()
   column = buffer.current_view().cursor().column()
   file.set_property ("lastloc_line", `line`, persistent=True)
   file.set_property ("lastloc_column", `column`, persistent=True)
   Logger ("FileLoc").log \
     ("Last location for " + file.name() + " is " + `line` + " " + `column`)

def on_file_edited (hook, file):
   try:
      buffer = EditorBuffer.get (file)
      cursor = buffer.cursor()

      # Do not change the line if the editor was already scrolled for
      # any reason
      if cursor.line() == 1 and cursor.column() == 1:
         line   = file.get_property ("lastloc_line")
         column = file.get_property ("lastloc_column")
         Logger ("FileLoc").log ("Restoring last location " + line + " " + column)
         buffer.current_view().goto \
            (EditorLocation (buffer, int (line), int (column)))
   except:
      pass

Hook ("file_closed").add (on_file_closed)
Hook ("file_edited").add (on_file_edited)


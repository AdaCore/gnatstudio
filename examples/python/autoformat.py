"""This plug-in will automatically reformat a source file each time it is
saved on disk.
"""


import GPS

# Actions to be performed each time a file is saved
def on_file_saved (hook, file):
   buf = GPS.EditorBuffer.get ()

   # Save the cursor location
   view = buf.current_view()
   cursor = view.cursor().create_mark()

   # Select the whole buffer
   buf.select (buf.beginning_of_buffer(), buf.end_of_buffer())

   # Reformat the buffer
   GPS.execute_action ("Format Selection")

   # Restore the cursor location
   view.goto (cursor.location())
   view.center (view.cursor())

# Register the callback on the "before_file_saved" hook
GPS.Hook ("before_file_saved").add (on_file_saved)

""" Enables a selection of Eclipse-like shortcuts in GPS.


The "delete line" (Ctrl-D) action was contributed by Robert ter Vehn.

"""

import GPS

#
# Local subprograms
#
def delete_line():
  """ Remove the lines that include the current selection. If there is
      no selection, remove the line at the cursor position.
   """

  buffer = GPS.EditorBuffer.get()
  cpos = buffer.current_view().cursor()

  append = GPS.last_command() == "delete line"
  if append:
    GPS.set_last_command ("delete line")

  start = buffer.selection_start().beginning_of_line()
  end   = buffer.selection_end().end_of_line()

  buffer.delete (start, end)

#
# Bindings
#
GPS.parse_xml ("""
   <action name="delete line" output="none" category="Editor">
      <description>Delete the lines that include the selection, or the current line.</description>
      <shell lang="python">eclipse.delete_line()</shell>
   </action>
   <key action="delete line">control-d</key>""")

""" Provides read-only protection for areas surrounded by markers;
--  begin read only
--  end read only
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os
from GPS import *
from os import *

Preference ("Plugins/read_only_lines/color").create (
  "Highlight color", "color",
   """Background color for read-only areas""",
   "#e0e0e0")

def on_file_edited (hook,file):
    read_only_overlay = None
    buffer = GPS.EditorBuffer.get (file)
    loc = buffer.beginning_of_buffer ()

    # Iterate over read-only areas
    while loc :
        found = loc.search ("--  begin read only", dialog_on_failure=False)

        if found:
            from_line,last = found
            found = last.search ("--  end read only", dialog_on_failure=False)

            if found:
                to_line,loc = found
            else:
                loc = None

        else:
            loc = None

        # if area found
        if loc:
            from_line = from_line.beginning_of_line ()
            to_line = to_line.end_of_line ()

            # if overlay hasn't exist yet, create one
            if read_only_overlay == None:
                read_only_overlay = buffer.create_overlay ()
                color = Preference ("Plugins/read_only_lines/color").get ()
                read_only_overlay.set_property ("background", color)
                read_only_overlay.set_property ("editable", False)

            buffer.apply_overlay (read_only_overlay, from_line, to_line)
    # No more read-only areas

GPS.Hook ("file_edited").add (on_file_edited)

"""This plug-in provides Cut, Copy and Paste buttons in the toolbar
"""


#############################################################################
## No user customization below this line
#############################################################################

import GPS

def on_gps_started (hook_name):
    t = GPS.Toolbar()
    cut   = GPS.ToolButton ("gtk-cut",   "Cut",   lambda x : GPS.execute_action ("/Edit/Cut"))
    copy  = GPS.ToolButton ("gtk-copy",  "Copy",  lambda x : GPS.execute_action ("/Edit/Copy"))
    paste = GPS.ToolButton ("gtk-paste", "Paste", lambda x : GPS.execute_action ("/Edit/Paste"))

    t.insert (cut,   3, "Cut to Clipboard")
    t.insert (copy,  4, "Copy to Clipboard")
    t.insert (paste, 5, "Paste from Clipboard")

GPS.Hook ("gps_started").add (on_gps_started)

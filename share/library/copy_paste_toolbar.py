"""This plug-in provides Cut, Copy and Paste buttons in the toolbar
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS


def on_gps_started(hook_name):
    t = GPS.Toolbar()
    cut = GPS.ToolButton(
        "gps-cut-symbolic",   "Cut",
        lambda x: GPS.execute_action("Cut to Clipboard"))
    copy = GPS.ToolButton(
        "gps-copy-symbolic",  "Copy",
        lambda x: GPS.execute_action("Copy to Clipboard"))
    paste = GPS.ToolButton(
        "gps-paste-symbolic", "Paste",
        lambda x: GPS.execute_action("Paste From Clipboard"))

    t.insert(cut,   3, "Cut to Clipboard")
    t.insert(copy,  4, "Copy to Clipboard")
    t.insert(paste, 5, "Paste from Clipboard")

GPS.Hook("gps_started").add(on_gps_started)

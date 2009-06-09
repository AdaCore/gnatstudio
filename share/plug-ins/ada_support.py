"""GNAT support for GPS

This file provides support for switches for Ada and GNAT in the project editor.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, gps_utils.gnat_rules

def on_switch_editor (hook_name):
   gps_utils.gnat_rules.EnsureInitialized();

GPS.Hook ("project_editor").add (on_switch_editor)

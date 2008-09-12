"""Provides an action to reset the Win32 registry settings for GPS.

This script specifically resets all the file associations that are normally
created when installing GPS. It can be the case that installing another
application breaks the initial associations.

This is only to be used on Windows plateforms.
"""

import GPS

def on_gps_started (hook_name):
   "Initializes this module."

   GPS.parse_xml ("""<action name='Reset Registry' category='Editor'>
     <description>Reset the Win32 registry setting for GPS.</description>
      <shell lang="python" output="none">reset_gps_registry.reset_registry()</shell>
   </action>

   <menu action='Reset Registry' before="About">
      <title>/Help/Reset Windows Registry</title>
   </menu>""")

def write_gnat_reg (ext, val):
    key = CreateKey (HKEY_CLASSES_ROOT, ext)
    SetValue (key, None, REG_SZ, val)

def reset_registry():
    write_gnat_reg (".ads", "GNAT.ads_file")
    write_gnat_reg (".adb", "GNAT.adb_file")
    write_gnat_reg (".ada", "GNAT.ada_file")
    write_gnat_reg (".ali", "GNAT.ali_file")
    write_gnat_reg (".gpr", "GNAT.project_file")
    write_gnat_reg (".o", "GNAT.object_file")
    GPS.Console ("Messages").write ("Windows registry reset for GPS.\n")

try:
  from _winreg import *
  GPS.Hook ("gps_started").add (on_gps_started)
except:
  pass

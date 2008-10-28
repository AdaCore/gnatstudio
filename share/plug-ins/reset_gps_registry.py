"""Provides an action to reset the Win32 registry settings for GPS.

This script specifically resets all the file associations that are normally
created when installing GPS. It can be the case that installing another
application breaks the initial associations.

This is only to be used on Windows plateforms.
"""

import GPS
from gps_utils import *

try:
  from _winreg import *
  def write_gnat_reg (ext, val):
      key = CreateKey (HKEY_CLASSES_ROOT, ext)
      SetValue (key, None, REG_SZ, val)

  @interactive ("Editor", name="Reset Registry",
                menu="/Help/Reset Windows Registry", before="About")
  def reset_registry():
      """Reset the Win32 registry setting for GPS"""
      write_gnat_reg (".ads", "GNAT.ads_file")
      write_gnat_reg (".adb", "GNAT.adb_file")
      write_gnat_reg (".ada", "GNAT.ada_file")
      write_gnat_reg (".ali", "GNAT.ali_file")
      write_gnat_reg (".gpr", "GNAT.project_file")
      write_gnat_reg (".o", "GNAT.object_file")
      GPS.Console ("Messages").write ("Windows registry reset for GPS.\n")

except:
  pass

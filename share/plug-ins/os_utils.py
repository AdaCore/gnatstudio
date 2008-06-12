"""Utility library used by other plug-ins"""


import os, os.path, string

def locate_exec_on_path (prog):
  """Utility function to locate an executable on path."""

  if os.name == 'nt':
    extension = ".exe"
  else:
    extension = ""

  alldirs = string.split (os.getenv('PATH'), os.pathsep)
  for file in [os.path.join(dir,prog) for dir in alldirs]:
    if os.path.isfile(file+extension):
      return file
  return ""

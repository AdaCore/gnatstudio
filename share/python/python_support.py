## This file provides extra support for editing python scripts through GPS

import GPS, sys, os.path

def reload_file (file):
  """Reload the currently edited file in python"""
  module=os.path.splitext (os.path.basename (file))[0]

  # If the module was already loaded, reload, else load if for the first time
  try:
    reload (sys.modules[module])
  except KeyError:
    import module


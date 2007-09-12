"""Automatically insert text when a new file is created

This script shows how you can implement what other editors call
skeletons. Any time you create a new file from within GPS, the
text set in the variable skeleton will be inserted in the file.

The text inserted can be customized through the /Tools/Plug-ins
menu.

Another possible implementation is to use text aliases (/Edit/Aliases),
which do not insert anything in the file but can be quickly expanded to
advanced contents containing for instance the current date.
"""

###########################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Plug-ins)

skeleton="""------------------------------------------------------------------
-- Demo for a skeleton, see skeleton.py in the GPS installation --
------------------------------------------------------------------
"""
### The text to insert in newly created files


############################################################################
## No user customization below this line
############################################################################

## Possible enhancements: if we knew the type/language of the file, we could
## insert one skeleton or another

import GPS, os.path

def add_skeleton (hook_name, file):
   # Only for new files:
   if not os.path.exists (file.name()):
      GPS.Editor.replace_text (file.name(), 1, 1, skeleton)

GPS.Hook ("file_edited").add (add_skeleton)

## This file provides a basic implementation for skeletons.
## Whenever a new file is created, it inserts a skeleton at the beginning
## of the file.

## Possible enhancements: if we knew the type/language of the file, we could
## insert one skeleton or another

import GPS, os.path

skeleton="""------------------------------------------------------------------
-- Demo for a skeleton, see skeleton.py in the GPS installation --
------------------------------------------------------------------
"""

def add_skeleton (hook_name, file):
   # Only for new files:
   if not os.path.exists (file.name()):
      GPS.Editor.replace_text (file.name(), 1, 1, skeleton)

GPS.Hook ("file_edited").add ("skeleton.add_skeleton")

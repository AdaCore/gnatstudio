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


############################################################################
## No user customization below this line
############################################################################

## Possible enhancements: if we knew the type/language of the file, we could
## insert one skeleton or another

import GPS, os.path


template_pref = "Plugins/skeleton/skeleton"

GPS.Preference(template_pref).create(
  "Template", "multiline",
  """Text to insert in newly created files""",
  """------------------------------------------------------------------
-- Demo for a skeleton, see skeleton.py in the GPS installation --
------------------------------------------------------------------
""")


def add_skeleton(hook_name, file):
    if not os.path.exists(file.name()):
        # Only for new files
        ed = GPS.EditorBuffer.get(file)
        loc = GPS.EditorLocation(ed, 1, 1)
        ed.insert(loc, GPS.Preference(template_pref).get())

GPS.Hook ("file_edited").add (add_skeleton)

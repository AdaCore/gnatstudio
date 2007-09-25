"""Highlighting all dispatching calls in the current editor

This package will highlight with a special background color all
dispatching calls found in the current editor. In particular, at such
locations, the cross-references might not lead accurate result (for
instance "go to body"), since the exact subprogram that is called is
not known until run time.

For the Ada language, this plug-in requires a version of GNAT more recent
than 2007-09-20, since the xref information was not created before then.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Plug-ins)

highlight_on_open = True
# Whether highlighting should be done as soon as a file is opened. This can
# slow down the opening of a file, though, so should be deactivated if opening
# a file appears slow.

highlight_color="cyan"
# The background color to use for the dispatching calls in the editors


#############################################################################
## No user customization below this line
#############################################################################

from GPS import *

def highlight_dispatching_calls (buffer):
  if not buffer:
     return

  over = buffer.create_overlay ("dispatchcalls")
  over.set_property ("background", highlight_color)
  buffer.remove_overlay (over)

  buffer.dispatch_calls_highlighted = True

  entities = buffer.file().entities (local = False)
  for e in entities:
     refs = e.references (show_kind = True, in_file = buffer.file())
     for r in refs:
        if refs[r] == "dispatching call":
           try:
              loc = EditorLocation (buffer, r.line(), r.column())
              buffer.apply_overlay (over, loc, loc + len (e.name()) - 1)
           except:
              # The xref location might no longer be valid, just ignore it
              pass


def on_highlight_dispatching_calls (menu):
  highlight_dispatching_calls (EditorBuffer.get())

def on_file_edited (hook, file):
  highlight_dispatching_calls (EditorBuffer.get (file))  

def on_compilation_finished (hook, category):
  # Re-highlight all editors
  for l in EditorBuffer.list():
     highlight_dispatching_calls (l)
  
def on_gps_start (hook):
  if highlight_on_open:
     Hook ("file_edited").add (on_file_edited)
     Hook ("file_changed_on_disk").add (on_file_edited)
     Hook ("compilation_finished").add (on_compilation_finished)
     on_compilation_finished (hook, "")


Hook ("gps_started").add (on_gps_start)
Menu.create ("/Navigate/Highlight Dispatching Calls",
             on_highlight_dispatching_calls,
             ref = "Find All References",
             add_before = False)


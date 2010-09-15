"""Highlighting all dispatching calls in the current editor

This package will highlight with a special background color all
dispatching calls found in the current editor. In particular, at such
locations, the cross-references might not lead accurate result (for
instance "go to body"), since the exact subprogram that is called is
not known until run time.
"""


#############################################################################
## No user customization below this line
#############################################################################

import GPS
from GPS import *

Preference ("Plugins/dispatching/onopen").create (
  "Highlight dispatching calls", "boolean",
   """If enabled, highlighting of dispatching calls is done as soon as a file is opened. This can slow down GPS, though, so should be deactivated if opening a file seems slow""", True)

Preference ("Plugins/dispatching/color").create (
  "Highlight color", "color",
   """Background color to use for dispatching calls""",
   "#FFF3C2")

Preference ("Plugins/dispatching/context").create (
  "Search context", "integer",
  """When the cross-reference information is not up-to-date, GPS will search a few lines around the original location for matching entities. This preference indicates how many lines it will search -- the bigger the slower of course, and potentially less precise too""", 5, 0, 50)

try:
   ## If we have PyGTK installed, we'll do the highlighting of the next
   ## matches in the background, which makes the interface more responsive
   import gobject
   has_pygtk = 1
except:
   has_pygtk = 0

context = 0    # Mirror of the preference
insert_overlays_id = 0
to_highlight=[]
current_entities=[]
current_entity=None

def highlight_entity_references (buffer, entity):
  """Highlight all dispatching calls to entity in buffer"""

  global context

  refs = entity.references (kind_in = "dispatching call",
                            in_file = buffer.file())
  n = entity.name().lower()

  if refs:
    for r in refs:
      for c in range (0, context + 1):
        try:
           # Search after original xref line
           cloc = EditorLocation (buffer, r.line() + c, r.column())
           endloc = cloc + len (n) - 1
           if buffer.get_chars (cloc, endloc).lower() == n:
              buffer.apply_overlay (buffer.dispatch_overlay, cloc, endloc)
              break

           # Search before original xref line
           if c != 0:
              cloc = EditorLocation (buffer, r.line() - c, r.column())
              endloc = cloc + len (n) - 1
              if buffer.get_chars (cloc, endloc).lower () == n:
                 buffer.apply_overlay (buffer.dispatch_overlay, cloc, endloc)
                 break

        except:
          # The xref location might no longer be valid, just ignore it
          pass

def highlight_file_idle ():
  """Process the next entity or file to highlight"""
  global to_highlight
  global current_entities
  global current_entity
  global insert_overlays_id

  if to_highlight == []:
     insert_overlays_id = 0
     current_entities=[]
     return False

  buffer = to_highlight[0]

  if current_entities == []:
     try:
        current_entities = buffer.file().entities (local = False)
        current_entity   = current_entities.__iter__()
     except GPS.Exception:
        ## The buffer might have been destroyed. Give up
        return True

  try:
     e = current_entity.next()
     highlight_entity_references (buffer, e)
     return True
  except StopIteration:
     to_highlight.pop (0)
     current_entities=[]
     return True
  except GPS.Exception:
     ## The buffer might have been destroyed. Give up
     to_highlight.pop (0)
     current_entities=[]
     return True


def highlight_dispatching_calls (buffer):
  global insert_overlays_id
  global to_highlight

  if not buffer:
     return

  try:
     buffer.remove_overlay (buffer.dispatch_overlay)
  except:
     buffer.dispatch_overlay = buffer.create_overlay ("dispatchcalls")

  buffer.dispatch_overlay.set_property ("background", Preference ("Plugins/dispatching/color").get())

  if has_pygtk:
     if not buffer in to_highlight:
        to_highlight.append (buffer)
     if insert_overlays_id == 0:
        insert_overlays_id = gobject.idle_add (highlight_file_idle)

  else:
     entities = buffer.file().entities (local = False)
     for e in entities:
        highlight_entity_references (buffer, e)


def on_highlight_dispatching_calls (menu):
  highlight_dispatching_calls (EditorBuffer.get())

def on_file_edited (hook, file):
  highlight_dispatching_calls (EditorBuffer.get (file))

def on_compilation_finished (hook, category, target_name="", mode_name="", status=""):
  # Re-highlight all editors
  for l in EditorBuffer.list():
     highlight_dispatching_calls (l)

hooks_set = 0

def preferences_changed (hook):
  global hooks_set
  global context
  context = Preference ("Plugins/dispatching/context").get ()

  if Preference ("Plugins/dispatching/onopen").get():
     if not hooks_set:
        Hook ("file_edited").add (on_file_edited)
        Hook ("file_changed_on_disk").add (on_file_edited)
        Hook ("compilation_finished").add (on_compilation_finished)
        hooks_set = 1

     # Always redo the highlighting to take into account changes in colors
     on_compilation_finished (hook, "")

  else:
     if hooks_set:
        Hook ("file_edited").remove (on_file_edited)
        Hook ("file_changed_on_disk").remove (on_file_edited)
        Hook ("compilation_finished").remove (on_compilation_finished)
        hooks_set = 0
     for l in EditorBuffer.list():
       try:
          l.remove_overlay (l.dispatch_overlay)
       except:
          pass

Hook ("preferences_changed").add (preferences_changed)
Menu.create ("/Navigate/Highlight Dispatching Calls",
             on_highlight_dispatching_calls,
             ref = "Find All References",
             add_before = False)

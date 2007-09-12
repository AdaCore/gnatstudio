"""Jump to the body of an Ada separate entity

This scripts adds a new contextual menu, shown when you click on
an Ada entity that is declared as "separate". If you select that
menu, an editor will be open to show the implementation directly.

GPS's standard "Go to body" contextual menu would take you to the
editor that has the "is separate" statement instead, so this mode
makes it slightly faster to navigate if you use Ada separates a
lot
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Plug-ins)


############################################################################
## No user customization below this line
############################################################################

import GPS

def on_goto_separate (context):
   loc = context.entity().body (2) 
   buffer = GPS.EditorBuffer.get (loc.file())
   buffer.current_view().goto \
      (GPS.EditorLocation (buffer, loc.line(), loc.column()))

def separate_filter (context):
   if isinstance (context, GPS.EntityContext) and context.entity():
      try:
        context.entity().body (2)
        return True
      except:
        return False
   else:
      return False

def separate_label (context):
   return "Goto separate body of " + context.entity().name()

def on_gps_started (hook_name):
   GPS.Contextual ("Jump to separate body").create \
     (on_activate=on_goto_separate,
      filter=separate_filter,
      label=separate_label,
      ref="Go to body") 

GPS.Hook ("gps_started").add (on_gps_started)

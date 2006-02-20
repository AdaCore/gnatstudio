## This example shows how to add a contextual menu to jump straight to
## the implementation of a separate Ada subprogram. This is implemented
## through a custom contextual menu

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

GPS.Contextual ("Jump to separate body").create \
  (on_activate=on_goto_separate,
   filter=separate_filter,
   label=separate_label,
   ref="Go to body") 

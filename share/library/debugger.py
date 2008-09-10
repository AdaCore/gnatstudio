"""Various extensions to the debugger contextual menu.
   This is mostly intended as a template for your own custom commands.
   It currently adds a single entry in the Debug contextual menu so that
   you can display the entity under the cursor as a decimal. In particular,
   this is useful when you click on an enumeration literal or a variable of
   an enumeration type if you want to see the actual value instead of the
   literal value."""


from GPS import *

def print_as_dec_label (context):
   return "Debug/Print <b> " + context.entity().name() + "</b> as decimal"

def print_as_dec_run (context):
   Debugger.get().send ("print/d " + context.entity().name(),
                        show_in_console=True)

def in_debugger (context):
   try:
     return Debugger.get() != None and context.entity() != None 
   except:
     return False

Contextual ("debug print as decimal").create (
   label       = print_as_dec_label,
   on_activate = print_as_dec_run,
   filter      = in_debugger)
   



"""Various extensions to the debugger contextual menu.
   This is mostly intended as a template for your own custom commands.

   It adds a single entry in the Debug contextual menu so that
   you can display the entity under the cursor as a decimal. In particular,
   this is useful when you click on an enumeration literal or a variable of
   an enumeration type if you want to see the actual value instead of the
   literal value.

   It also adds a /Debug/Data/Graph Display Local Variables menu, which
   displays all local variables in the data window, one box per variable.
   This is different from the /Debug/Data/Display Local Variables menu
   which does not pretty-print the variables.
"""


from GPS import *
import text_utils

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

def display_local_vars (menu):
   buffer = EditorBuffer.get()
   subp   = text_utils.goto_subprogram_start (buffer.current_view().cursor())
   if subp:
      entity = Entity (subp.block_name(), buffer.file(), subp.line())
   vars = text_utils.get_local_vars (entity)
   debug = Debugger.get()
   for v in vars:
      debug.send ("graph display " + v.name(), False)

Contextual ("debug print as decimal").create (
   label       = print_as_dec_label,
   on_activate = print_as_dec_run,
   filter      = in_debugger)

Menu.create (
   path = "/Debug/Data/Graph display local variables",
   on_activate = display_local_vars)

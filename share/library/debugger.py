"""Various extensions to the visual debugger support in GPS.
   This is mostly intended as a template for your own custom commands.

   This plug-in adds:

   - an entry in the Debug contextual menu so that you can display the entity
   with its full expanded name (e.g. Foo.Bar.Var). In particular, this is
   useful when you have lots of partial name qualification in your code, such
   as Bar.Var to refer to Foo.Bar.Var, where the underlying debugger cannot
   resolve the value by itself.

   - an entry in the Debug contextual menu so that you can display the entity
   under the cursor as a decimal. In particular, this is useful when you click
   on an enumeration literal or a variable of an enumeration type if you want
   to see the actual value instead of the literal value.

   - /Debug/Data/Graph Display Local Variables menu, which
   displays all local variables in the data window, one box per variable.
   This is different from the /Debug/Data/Display Local Variables menu
   which does not pretty-print the variables.

   - a way to ignore exceptions raised at specific source locations,
   while still stopping on all other exceptions. The source locations are
   set through the contextual menu "Ignore exception breakpoints" (and removed
   likewise), and are preserved between GPS sessions if the preference is set
   appropriately.

   - a GPS action (to which key bindings can be set) called
   "Continue till line" (in the General category of the key shortcut manager).
   This allows you through a simple key shortcut to automatically continue the
   debugger till the current line.
"""


###########################################################################
## No user customization below this line
###########################################################################

from GPS import *
from gps_utils import *
import text_utils, re, os.path

Preference ("Plugins/debugger/save_autocont_br").create (
   "Preserve auto-cont breakpoints", "boolean",
   """If set, the source locations where the debugger should not stop on an exception are preserved across GPS sessions. If unset, you'll have to reset them the next time you start the debugger, but on the other hand this might work better when the source code has changed""", True)

def in_debugger (context):
   try:
      return Debugger.get() != None
   except:
      return False

def print_in_console (debug, txt):
   Console ("Debugger Console").write (txt)

###################################
# Display all local vars in graph #
###################################

def display_local_vars (menu):
   buffer = EditorBuffer.get()
   subp   = text_utils.goto_subprogram_start (buffer.current_view().cursor())
   if subp:
      entity = Entity (subp.block_name(), buffer.file(), subp.line())
   vars = text_utils.get_local_vars (entity)
   debug = Debugger.get()
   for v in vars:
      debug.send ("graph display " + v.name(), False)

Menu.create (
   path = "/Debug/Data/Graph display local variables",
   on_activate = display_local_vars)

#####################
# Display full name #
#####################

def display_full_name_label (context):
   return "Debug/Display <b>" + context.entity().full_name() + "</b>"

def display_full_name_run (context):
   Debugger.get().send ("graph display " + context.entity().full_name(),
                        show_in_console=True)

def in_debugger_on_entity (context):
   try:
     return in_debugger (context) and context.entity() != None
   except:
     return False

Contextual ("debug display full name").create (
   label       = display_full_name_label,
   on_activate = display_full_name_run,
   filter      = in_debugger_on_entity)

#######################
# Printing as decimal #
#######################

def print_as_dec_label (context):
   return "Debug/Print <b>" + context.entity().name() + "</b> as decimal"

def print_as_dec_run (context):
   Debugger.get().send ("print/d " + context.entity().name(),
                        show_in_console=True)

Contextual ("debug print as decimal").create (
   label       = print_as_dec_label,
   on_activate = print_as_dec_run,
   filter      = in_debugger_on_entity)

###################################
# Continuing till a specific line #
###################################

@interactive (name="continue till line", category="Debugger",
              filter="Debugger active", key="control-b",
              menu="/Debug/Continue to current line", after="Continue")
def continue_till_line ():
  """Continue executing the debuggee until it reaches the current editor line. If this line is never reached, the debugger will not stop"""
  context = current_context()
  try:
     debug = Debugger.get()
     debug.send ("tbreak " + context.file().name() + ":" + `context.location().line()`)
     debug.send ("cont")
  except:
     pass  # No debugger active

#########################
# Breakpoint exceptions #
#########################

def debug_add_br_exception_label (context):
   f = os.path.basename (context.file().name())\
       + ":" + `context.location().line()`
   if f in autocont_br:
     return "Debug/Do not ignore exception breakpoints on line <b>" \
        + `context.location().line ()` + "</b>"
   else:
     return "Debug/Ignore exception breakpoints on line <b>" \
        + `context.location().line ()` + "</b>"

autocont_br = set ()

def on_debugger_stopped (h, debug):
   frame = debug.send ("frame", False)

   # Process terminated ?
   if frame.find ("No stack.") != -1:
      return

   # The only case where we don't stop at frame 0 is for exceptions. So
   # if we are on frame 0, we are in an exception
   if re.search ("^#0\s", frame): return

   m = re.search ("at (.+):(\d+)", frame)
   file = os.path.basename (m.group(1)) + ":" + m.group(2)
   if file in autocont_br:
      debug.non_blocking_send ("cont")

def add_breakpoint_exception (context):
   global autocont_br
   # Only consider base names of files, since the debugger does not always
   # show the full name in the "frame" command
   f = os.path.basename (context.file().name())\
       + ":" + `context.location().line()`
   if f in autocont_br:
      autocont_br.remove (f)
   else:
      autocont_br.add (f)
   if Preference ("Plugins/debugger/save_autocont_br").get():
      Project.root().set_property ("autocont_br", "--".join (autocont_br), True)

def on_project_view_changed (h):
   global autocont_br
   try:
     autocont_br = set (Project.root().get_property ("autocont_br").split ("--"))
     Console().write ("The debugger will not stop when an exception is raised at " + "\n".join (autocont_br))
   except:
     autocont_br = set ()

Contextual ("debug add breakpoint exception").create (
   label       = debug_add_br_exception_label,
   on_activate = add_breakpoint_exception,
   filter      = in_debugger)

Hook ("debugger_process_stopped").add (on_debugger_stopped)
Hook ("project_view_changed").add (on_project_view_changed)


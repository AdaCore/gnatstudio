## This module implements some functions to provide services available through
## the Emacs ada-mode.
## It is mostly intended as an example on how to write such functions, rather
## than as an extensive GPS customization
##
## These functions are bound by default to the same key binding used in Emacs.
## As usual, the user can change these key bindings through the
## Edit->Key Shortcuts menu.

## Current limitations for these functions:
##   __find_subprogram_decl uses an ad-hoc heuristic, but better services
##     could be provided by GPS itself
##   add_subprogram_box will only work for Ada subprograms 

import GPS
import re, string


GPS.parse_xml ("""
   <action name="find other file" output="none">
     <filter id="Source editor" />
     <shell lang="python">emacs.goto_other_file()</shell>
   </action>
   <key action="find other file">control-c o</key>

   <action name="subprogram box" output="none">
      <filter_and>
         <filter id="Source editor" />
         <filter language="ada" />
      </filter_and>
      <shell lang="python">emacs.add_subprogram_box()</shell>
   </action>
   <key action="subprogram box">control-c n</key>
                                                                                
   <action name="kill-line" output="none">
      <filter id="Source editor" />
      <shell lang="python">emacs.kill_line()</shell>
   </action>
   <key action="kill-line">control-k</key>

   <action name="Clone-and-split-horizontally" output="none">
      <shell>MDI.clone_window</shell>
      <shell>MDI.split_horizontally</shell>
   </action>
   <key action="Clone-and-split-horizontally">control-x 3</key>

   <action name="Clone-and-split-vertically" output="none">
      <shell>MDI.clone_window</shell>
      <shell>MDI.split_vertically</shell>
   </action>
   <key action="Clone-and-split-vertically">control-x 2</key>
""")

subprograms_re=re.compile ("^([ \t]*)(procedure|function) ([a-zA-Z0-9_]+)", re.IGNORECASE)

def __find_subprogram_decl():
   """ Return the subprogram declaration closest to the cursor. This returns
       a (MatchObject, line) tuple for the regexp subprograms_re """
   f = GPS.current_context().file().name ()
   line = GPS.current_context().location().line()
   while line > 0 :
      match = re.search (subprograms_re, GPS.Editor.get_chars (f, line, 1))
      if match != None:
         return (match, line)
      line = line - 1
   return (None, 0)

def add_subprogram_box():
   """ Insert in the current editor a box just before the current subprogram
       starts """
   match = __find_subprogram_decl()
   if match[0] != None:
      prefix = ' ' * len (match[0].group (1))
      box = prefix + ('-' * (6 + len (match[0].group (3)))) + "\n"
      GPS.Editor.replace_text (GPS.current_context().file().name(), match[1], 1,
                    box + prefix + "-- " + match[0].group (3) + " --\n" + box + "\n",
                    0, 0)
  
def goto_other_file():
   """ Switch to the other file, if possible on the matching subprogram """
   match = __find_subprogram_decl()
   current_file = GPS.current_context().file()
   if match[0] != None:
      name = match[0].group (3)
      line = match[1]
      try:
         entity = GPS.Entity (name, current_file, line)

         if entity.decl_file() == current_file:
            body = entity.body()
            if body.file() != current_file:
	       GPS.Editor.edit (body.file().name(), line=body.line(), column=body.column())
            else:
               GPS.Editor.edit (current_file.other_file().name())
         else:
            GPS.Editor.edit (entity.decl_file().name(), line=entity.decl_line(),
                  column=entity.decl_column())
      except:
         print "Not found " + name + ":" + current_file.name() + ":" + `line`
         GPS.Editor.edit (current_file.other_file().name())
   else:
      GPS.Editor.edit (current_file.other_file().name())

def kill_line():
   """ Kills the end-of-line, or the whole line if it is empty or contains
       only white spaces. This is a better emulation of Emacs's behavior
       than the one provided by default by gtk+, which doesn't handle
       whitespaces correctly  """

   # Will not work in new files, since there is no file name set. We would
   # need to access directly the current editor instead of current context.
   file = GPS.current_context().file().name()
   line = GPS.current_context().location().line()
   col  = GPS.current_context().location().column()
   str = GPS.Editor.get_chars (file, line, col, 0)
   if string.rstrip (str) == "":
	GPS.Editor.replace_text (file, line, col, "", 0, len (str))
   else:
        GPS.Editor.replace_text (file, line, col, "", 0, len (str) - 1)
   
   


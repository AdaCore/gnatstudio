## This module implements high level actions related to source navigation

import GPS
import string, re


## Register the GPS actions

GPS.parse_xml ("""
   <action name="goto declaration or body" output="none" category="Editor">
     <filter id="Source editor" />
     <shell lang="python">navigation_utils.goto_declaration_body()</shell>
   </action>

   <action name="goto other file" output="none" category="Editor">
     <filter id="Source editor" />
     <shell lang="python">navigation_utils.goto_other_file()</shell>
   </action>
""")


##  ??? At the moment, this is ada-specific, and ad-hoc. We should use
##  the GPS engine to get that sort of functionality with any language.

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

def goto_declaration_body():
   """ Go to the declaration of the reference, or to the body if we are
       already on the declaration """
   current_file = GPS.current_context().file()
   current_line = GPS.current_context().location().line()

   try:
         entity = GPS.current_context().entity();

         decl = entity.declaration().file()
         decl_line = entity.declaration().line()

         if current_file == decl and current_line == decl_line:
            body = entity.body().file()
            body_line = entity.body().line()

            GPS.Editor.edit (body.name(),
                             line=body_line,
                             column=entity.body().column())
         else:
            GPS.Editor.edit (decl.name(),
                             line=decl_line,
                             column=entity.declaration().column())
   except:
         print "Not found " + name + ":" + current_file.name() + ":" + `line`
         GPS.Editor.edit (current_file.other_file().name())

def goto_other_file():
   """ Switch to the other file, if possible on the matching subprogram """
   match = __find_subprogram_decl()
   current_file = GPS.current_context().file()

   if match[0] != None:
      name = match[0].group (3)
      line = match[1]
      try:
         entity = GPS.Entity (name, current_file, line)

         if entity.declaration ().file() == current_file:
            body = entity.body()

            if body.file() != current_file:
	       GPS.Editor.edit (body.file().name(),
                                line=body.line(),
                                column=body.column())

            else:
               GPS.Editor.edit (current_file.other_file().name())

         else:
            GPS.Editor.edit (entity.declaration ().file().name(),
                             line=entity.declaration ().line(),
                             column=entity.declaration ().column())
      except:
         print "Not found " + name + ":" + current_file.name() + ":" + `line`
         GPS.Editor.edit (current_file.other_file().name())
   else:
      GPS.Editor.edit (current_file.other_file().name())


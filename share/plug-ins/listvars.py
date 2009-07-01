"""List all variables referenced in the selected subprogram

This script adds a contextual menu when you click on a subprogram.
The submenu will list the first reference to all variables (local or global)
used in the selected subprogram.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS
import gobject

def list_vars (subprogram, global_only=False):
   """List all variables referenced by the subprogram.
      subprogram is an instance of GPS.Entity"""

   try:
      locFile = subprogram.body().file()
      locFrom = subprogram.body().line()
      locTo   = subprogram.end_of_scope().line()
   except:
      return

   category = "variables referenced in " + subprogram.full_name()
   if global_only:
      category = "non local " + category

   GPS.Locations.remove_category (category)
   added = False
   highlight = ""

   # to enable colors:
   # highlight = "light green"
   # Editor.register_highlighting ("var refs", "light green")

   for e in locFile.entities(local=False):
      if e.category() == "object":
         found = False
         for r in e.references (include_implicit=True, in_file=locFile):
            if not found \
              and r.file() == locFile \
              and r.line() >= locFrom and r.line() <= locTo:
               decl = e.declaration()

               if decl.file() != locFile \
                 or decl.line() < locFrom \
                 or decl.line() > locTo:
                  GPS.Locations.add (category=category,
                                     file=r.file(),
                                     line=r.line(),
                                     column=r.column(),
                                     message=e.full_name() + " (decl: "
                                     + `e.declaration()` + ")",
                                     highlight=highlight,
                                     length=0)
                  added = True
               elif not global_only:
                  GPS.Locations.add (category=category,
                                     file=r.file(),
                                     line=r.line(),
                                     column=r.column(),
                                     message=e.name(),
                                     highlight=highlight,
                                     length=0)
                  added = True
               found = True

   if added:
      GPS.MDI.get ("Locations").raise_window()

def on_filter (context):
   return (isinstance (context, GPS.EntityContext) and
     context.entity() and
     (context.entity().category() == "subprogram"))

def on_label (context):
   entity = context.entity()
   if entity:
     return "References/Variables used in <b>" + gobject.markup_escape_text (entity.name()) + "</b>"
   else:
     return ""

def on_global_label (context):
   entity = context.entity()
   if entity:
     return "References/Non local variables used in <b>" + gobject.markup_escape_text (entity.name()) + "</b>"
   else:
     return ""

def on_gps_started (hook_name):
   GPS.Contextual ("Variables referenced").create (
      on_activate=lambda context: list_vars (context.entity(), False),
      filter=on_filter,
      label=on_label)
   GPS.Contextual ("Non local variables referenced").create (
      on_activate=lambda context: list_vars (context.entity(), True),
      filter=on_filter,
      label=on_global_label)

GPS.Hook ("gps_started").add (on_gps_started)

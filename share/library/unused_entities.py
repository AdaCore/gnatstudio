"""This plug-in provides menus to list unusued entities

This menu adds some submenus to /Navigate that will show the list of entities
that are declared in the whole application, a specific project or a specific
file, and that are not used anywhere. In some cases, the compiler can warn
about unused entities local to a subprogram (or even to a package body in
Ada), but cannot do so for entities declared in specs.

The global entities that are not used anywhere will be listed in the locations
window, so that you can click on them to jump to their declaration.

For the time being, subprograms that are primitive operations of a tagged
type (or method of a class in languages other than Ada) are not listed, since
they might actually be called through dynamic dispatching and should not be
removed.

Executing this script blocks the whole GPS interface, so it is normal that
GPS becomes unresponsive. Depending of the size of your project, this can
take a while to execute.
Note that you can save the contents of the Locations window, after execution,
through the GPS.Locations.dump() method in the python console.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

xmlada_projects=["xmlada_sax", "xmlada_dom", "xmlada_schema", "xmlada_unicode",
                 "xmlada_input", "xmlada_shared", "xmlada"]
aws_projects=["aws_config", "aws_libz", "aws_shared", "aws_ssl_support",
              "aws_components", "aws_xmlada", "aws"]


ignore_projects = [] + xmlada_projects + aws_projects
# Names of the projects for which we never want to look for unused entities.
# This should in general include those projects from third-party libraries.
# You can still search for unusued entities if you select that project
# specifically. The two constants xmlada_projects and aws_projects can be
# used to initialize those lists, since it is expected that any project
# depending on those does not use all their entities.
# Names should be lower-cased


#############################################################################
## No user customization below this line
#############################################################################

from GPS import *

def EntityIterator (where):
  """Return all entities from WHERE"""
  if not where:
     for p in Project.root().dependencies (recursive=True):
       if p.name().lower() not in ignore_projects:
         Console ().write ("Searching unused entities in project " + p.name() + "\n")
         for s in p.sources ():
          for e in s.entities():
            yield e
  elif isinstance (where, Project):
    for s in where.sources():
       for e in s.entities():
          yield e
  elif isinstance (where, File):
    for e in where.entities():
      yield e


def GlobalIterator (where):
   """Return all global entities from WHERE"""
   for e in EntityIterator (where):
     if e.attributes()["global"]:
        yield e


def is_unused (entity):
   refs = entity.references \
      (include_implicit=True, synchronous=True, show_kind=True)
   for loc,kind in refs.iteritems():
     if kind != 'declaration' \
       and kind != 'body' \
       and kind != 'label':
        # Logger ("UNUSED").log (`entity` + " not unused because of " + `loc`)
        return False

   # If we have a primitive operation, do not report it for now, since it
   # might actually be called through dispatching. We do not know yet how
   # to test that

   if entity.primitive_of():
      return False

   #Logger ("UNUSED").log (`entity` + " is unused: " + `refs`)
   return True


def UnusedIterator (where, globals_only):
   """Return all unused entities from WHERE, and only global entities if
      GLOBALS_ONLY is true"""
   if globals_only:
      iter = GlobalIterator
   else:
      iter = EntityIterator

   for e in iter(where):
     if is_unused (e):
        yield e

def show_unused_entities (where, globals_only):
   """List all unused global entities from WHERE in the locations window"""
   Editor.register_highlighting ("Unused_Entities", "blue")
   Locations.remove_category ("Unused entity")
   MDI.get ("Messages").raise_window()

   set_busy()
   for e in UnusedIterator (where, globals_only=globals_only):
      Locations.add (category  = "Unused entity",
                     file      = e.declaration().file(),
                     line      = e.declaration().line(),
                     column    = e.declaration().column(),
                     message   = "unused entity " + e.name(),
                     highlight = "Unused_Entities",
                     length    = len (e.name()))
   unset_busy()
   Console().write ("Done searching for unused entities")

def show_unused_entities_in_file (menu):
    show_unused_entities (EditorBuffer.get().file(), True)
def show_unused_entities_in_project (menu):
    show_unused_entities (EditorBuffer.get().file().project(), True)
def show_unused_entities_in_projects (menu):
    show_unused_entities (None, True)


Menu.create ("/Navigate/List unused entities/From file",
             on_activate=show_unused_entities_in_file,
             ref="Goto Body",
             add_before=False)
Menu.create ("/Navigate/List unused entities/From project",
             on_activate=show_unused_entities_in_project,
             ref="Goto Body",
             add_before=False)
Menu.create ("/Navigate/List unused entities/From all projects",
             on_activate=show_unused_entities_in_projects,
             ref="Goto Body",
             add_before=False)

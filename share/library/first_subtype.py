"""
This plugin adds a new contextual menu entry which points to the first
subtype of a type. For instance, if you have the following Ada code:

    type T is new Integer;
    type T1 is T;
    type T2 is T;

and you click on T2, that contextual menu would jump to the declaration
of T.
"""



from GPS import *

def is_predefined (entity):
   f = entity.declaration ().file ().name ()
   return f.find ("predefined") != -1

def get_first_subtype (entity):
   try:
     while True:
        parent = entity.type ()
        if is_predefined (parent):
           return entity
        entity = parent
     return None
   except:
     return None

def goto_first_subtype (context):
   if context.first_subtype:
      decl    = context.first_subtype.declaration ()
      buffer  = EditorBuffer.get (decl.file())
      buffer.current_view ().goto (
         EditorLocation (buffer, decl.line (), decl.column ()))

def has_first_subtype (context):
   if isinstance (context, EntityContext):
      context.first_subtype = get_first_subtype (context.entity ())
      return context.first_subtype != None
   return False

def label (context):
   return "Goto first subtype of <b>" + context.first_subtype.name() + "</b>"

Contextual ("Goto first subtype").create (
   on_activate=goto_first_subtype,
   label=label,
   filter=has_first_subtype,
   ref="Goto body of entity",
   add_before=False)

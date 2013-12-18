"""
This plugin adds a new contextual menu entry which points to the first
subtype of a type. For instance, if you have the following Ada code:

    type T is new Integer;
    type T1 is new T;

and you click on T1, that contextual menu would jump to the declaration
of T.
"""

import GPS


def get_first_subtype(entity):
    try:
        while True:
            parents = entity.parent_types()
            if not parents:
                return None

            for p in parents:
                if p.is_predefined():
                    return entity

            entity = parents[0]

        return None
    except Exception as e:
        return None


def goto_first_subtype(context):
    if context.first_subtype:
        decl = context.first_subtype.declaration()
        buffer = GPS.EditorBuffer.get(decl.file())
        buffer.current_view().goto(
            buffer.at(decl.line(), decl.column()))


def has_first_subtype(context):
    if isinstance(context, GPS.EntityContext):
        context.first_subtype = get_first_subtype(context.entity())
        return context.first_subtype != None
    return False


def label(context):
    return 'Goto first subtype of <b>' + context.entity().name() + '</b>'


GPS.Contextual('Goto first subtype').create(
    on_activate=goto_first_subtype,
    label=label, filter=has_first_subtype, ref='Goto body of entity',
    add_before=False)

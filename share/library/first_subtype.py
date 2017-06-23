"""
This plugin adds a new contextual menu entry which points to the first
subtype of a type. For instance, if you have the following Ada code:

    type T is new Integer;
    type T1 is new T;

and you click on T1, that contextual menu would jump to the declaration
of T.
"""

import GPS
import gps_utils


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
    except Exception:
        return None


def has_first_subtype(context):
    try:
        context.first_subtype = get_first_subtype(context.entity())
        return context.first_subtype is not None
    except:
        return False


@gps_utils.interactive(
    name='goto first subtype',
    contextual='Goto first subtype of %e',
    filter=has_first_subtype,
    contextual_ref='goto other file')
def __goto_first_subtype():
    context = GPS.current_context()
    decl = context.first_subtype.declaration()
    buffer = GPS.EditorBuffer.get(decl.file())
    buffer.current_view().goto(
        buffer.at(decl.line(), decl.column()))

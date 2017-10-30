"""List all variables referenced in the selected subprogram

This script adds a contextual menu when you click on a subprogram.
The submenu will list the first reference to all variables (local or global)
used in the selected subprogram.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
from gi.repository import GLib
import gps_utils


def list_vars(subprogram, global_only=False):
    """List all variables referenced by the subprogram.
       subprogram is an instance of GPS.Entity"""

    try:
        locFile = subprogram.body().file()
        locFrom = subprogram.body().line()
        locTo = subprogram.end_of_scope().line()
    except:
        return

    category = "variables referenced in " + subprogram.full_name()
    if global_only:
        category = "non local " + category

    GPS.Locations.remove_category(category)
    added = False
    highlight = ""

    # to enable colors:
    # highlight = "light green"
    # Editor.register_highlighting ("var refs", "light green")

    for e in locFile.entities(local=False):
        if not e.is_type():
            found = False
            refs = e.references(
                include_implicit=True,
                in_file=locFile,
                show_kind=True)
            for loc, kind in refs.iteritems():
                if not found \
                   and loc.file() == locFile \
                   and loc.line() >= locFrom \
                   and loc.line() <= locTo:

                    decl = e.declaration()

                    if decl.file() != locFile \
                       or decl.line() < locFrom \
                       or decl.line() > locTo:
                        GPS.Locations.add(
                            category=category,
                            file=loc.file(),
                            line=loc.line(),
                            column=loc.column(),
                            message="%s (decl: %s) %s" % (
                                e.full_name(), e.declaration(), kind),
                            highlight=highlight,
                            length=0)
                        added = True
                    elif not global_only:
                        GPS.Locations.add(
                            category=category,
                            file=loc.file(),
                            line=loc.line(),
                            column=loc.column(),
                            message="%s %s" % (e.name(), kind),
                            highlight=highlight,
                            length=0)
                        added = True
                    found = True

    if added:
        GPS.MDI.get("Locations").raise_window()


def on_filter(context):
    return (context.entity_name() is not None and
            context.entity() and context.entity().is_subprogram())


def on_label(context):
    entity = context.entity()
    if entity:
        return "References/Variables used in <b>" + \
            GLib.markup_escape_text(entity.name()) + "</b>"
    else:
        return ""


def on_global_label(context):
    entity = context.entity()
    if entity:
        return "References/Non local variables used in <b>" + \
            GLib.markup_escape_text(entity.name()) + "</b>"
    else:
        return ""


@gps_utils.interactive(
    name='Variables referenced',
    contextual=on_label,
    filter=on_filter,
    static_path="References/Variables used in")
def __list_local_vars():
    """List all variables referenced by the subprogram."""
    list_vars(GPS.current_context().entity(), True)


@gps_utils.interactive(
    name='Non local Variables referenced',
    contextual=on_global_label,
    filter=on_filter,
    static_path="References/Non local variables used in")
def __list_global_vars():
    """List all non local variables referenced by the subprogram."""
    list_vars(GPS.current_context().entity(), False)

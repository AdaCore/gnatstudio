"""This module implements high level actions related to source navigation

This script defines a number of functions and GPS actions that you can
reuse in your own scripts.
In particular, it provides the following GPS actions, to which you can
bind key shortcuts through the menu /Edit/Key shortcuts:
  - "goto declaration or body"
  - "goto other file"
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import re
from gps_utils import interactive

# ??? At the moment, this is ada-specific, and ad-hoc. We should use
# the GPS engine to get that sort of functionality with any language.

subprograms_re = re.compile(
    "^([ \t]*)(procedure|function) ([a-zA-Z0-9_]+)", re.IGNORECASE)


def __find_subprogram_decl():
    """ Return the subprogram declaration closest to the cursor. This returns
        a (MatchObject, line) tuple for the regexp subprograms_re """
    f = GPS.current_context().file().path
    line = GPS.current_context().location().line()
    while line > 0:
        match = re.search(subprograms_re, GPS.Editor.get_chars(f, line, 1))
        if match is not None:
            return (match, line)
        line = line - 1
    return (None, 0)


@interactive("Editor", "Source editor", name="goto declaration or body")
def goto_declaration_body():
    """
    Jump to the declaration of the current entity. If the cursor
    is already on the declaration, jump to the body/implementation
    of the entity instead.
    """
    current_file = GPS.current_context().file()
    current_line = GPS.current_context().location().line()

    try:
        entity = GPS.current_context().entity()

        decl = entity.declaration().file()
        decl_line = entity.declaration().line()

        GPS.Editor.mark_current_location()
        if current_file == decl and current_line == decl_line:
            body = entity.body().file()
            body_line = entity.body().line()

            GPS.Editor.edit(body.name(),
                            line=body_line,
                            column=entity.body().column())
        else:
            GPS.Editor.edit(decl.name(),
                            line=decl_line,
                            column=entity.declaration().column())
        GPS.Editor.mark_current_location()
    except Exception:
        print "Not found %s:%s" % (current_file.path, current_line)
        GPS.Editor.edit(current_file.other_file().path)

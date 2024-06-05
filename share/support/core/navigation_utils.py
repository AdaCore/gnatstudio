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
import libadalang
import re
from gs_utils import interactive

# ??? At the moment, this is ada-specific, and ad-hoc. We should use
# the GPS engine to get that sort of functionality with any language.

subprograms_re = re.compile(
    "^([ \t]*)(procedure|function) ([a-zA-Z0-9_]+)", re.IGNORECASE
)


def __find_subprogram_decl():
    """Return the subprogram declaration closest to the cursor. This returns
    a (MatchObject, line) tuple for the regexp subprograms_re"""
    f = GPS.current_context().file().path
    line = GPS.current_context().location().line()
    while line > 0:
        match = re.search(subprograms_re, GPS.Editor.get_chars(f, line, 1))
        if match is not None:
            return (match, line)
        line = line - 1
    return (None, 0)


@interactive("Editor", "Source editor", name="cycle in block")
def cycle_in_entity():
    context = GPS.current_context()
    current_file = context.file()
    if not current_file or not current_file.language().lower() == "ada":
        return
    buf = GPS.EditorBuffer.get(current_file, open=False)
    if not buf:
        return
    location = context.location()
    line = location.line()
    column = location.column()

    unit = buf.get_analysis_unit()
    node = unit.root.lookup(libadalang.Sloc(line, column))
    begin_line = None

    # Search for the enclosing block
    if node:
        for parent in node.parents():
            if isinstance(
                parent,
                (
                    libadalang.BaseSubpBody,
                    libadalang.TaskBody,
                    libadalang.EntryBody,
                    libadalang.DeclBlock,
                ),
            ):
                start_line = parent.sloc_range.start.line
                # We only need the start_column => everything should be
                # aligned
                dest_col = parent.sloc_range.start.column
                end_line = parent.sloc_range.end.line
                # Search for the declarative part (begin block)
                for child in parent.children:
                    if isinstance(child, libadalang.DeclarativePart):
                        begin_line = child.sloc_range.end.line
                        break
                break
    else:
        return

    try:
        if begin_line and line < begin_line:
            dest_line = begin_line
        elif line < end_line:
            dest_line = end_line
        else:
            dest_line = start_line
        buf.current_view().goto(buf.at(int(dest_line), int(dest_col)))
    except NameError:
        # An exception can be raised if no parent was found (end_line, dest_col
        # can be not set)
        return

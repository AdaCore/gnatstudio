"""This plugin provides an action to find the declarative part in the current
 Ada context

It will move the cursor to the end of the current declarative part. This is
useful to find the begin keyword in a procedure with multiple nested
procedures.

A convenience menu is added to execute the action:
   /Find/Goto Begin
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
import libadalang
from gps_utils import interactive


def context_has_file(context):
    """A filter to only allow this action if a file is selected"""
    return context.file() is not None


@interactive(name="go to current begin",
             filter=context_has_file,
             menu="/Find/Goto Begin")
def goto_begin():
    """Find the begin keyword of the current block"""
    # Get the buffer
    buf = GPS.EditorBuffer.get()
    # Get the position of the cursor
    line = GPS.Editor.cursor_get_line(str(buf.file()))
    column = GPS.Editor.cursor_get_column(str(buf.file()))
    # Get the libadalang tree
    sloc = libadalang.Sloc(line, column)
    analysis = buf.get_analysis_unit()

    if analysis.root:
        node = analysis.root.lookup(sloc)
        while node and not isinstance(node, libadalang.BodyNode):
            node = node.parent
        if node:
            # At this point we found the current Body, we need to go through
            # its children to find its begin keyword.
            for child in node.children:
                if isinstance(child, libadalang.DeclarativePart):
                    loc = buf.at(int(child.sloc_range.end.line),
                                 int(node.sloc_range.end.column))
                    buf.current_view().goto(loc)
                    break
    return

"""
This plug-in improves the behavior of Left and Right arrow keys
when navigating in a tree, for instance the project explorer.

It brings a behavior similar to the Windows default, that is:
  - Right expands the current node, and then selects its first child
    The Down key does not expand before it moves the cursor down.

  - Left collapse the current node if it is expanded otherwise it moves
    the cursor to the parent node.
"""

import GPS
from gps_utils import interactive
from gi.repository import Gtk


logger = GPS.Logger("treemove")


def current_tree():
    """Return the current tree widget and path that has the focus or None"""
    for widget in Gtk.Window.list_toplevels():
        if widget.is_active():
            result = widget.get_focus()
            if isinstance(result, Gtk.TreeView):
                return result, result.get_cursor()[0]
            return None
    return None


@interactive(category="General",
             filter=lambda x: current_tree() is not None,
             key="Left")
def move_cursor_up_or_collapse():
    current = current_tree()
    if current is None:
        return
    tree, path = current
    if tree and path:
        if tree.row_expanded(path):
            # The current cursor is on an expanded row, so collapse
            # the row
            logger.log("collapse row %s" % path)
            tree.collapse_row(path)
        else:
            # The current cursor is not on an expanded row so try to
            # to move up to the parent node.
            path.up()
            if path:
                # Check that path is valid as path.up on the root
                # node might return an invalid path ?
                tree.set_cursor(path, tree.get_column(0))


@interactive(category="General",
             filter=lambda x: current_tree() is not None,
             key="Right")
def expand_and_move_cursor_down():
    current = current_tree()
    if current is None:
        return
    tree, path = current
    if tree and path:
        logger.log("expand row %s" % path)
        if tree.row_expanded(path):
            path.down()
            tree.set_cursor(path, tree.get_column(0))
        else:
            tree.expand_row(path, False)

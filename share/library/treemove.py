"""
This plug-in improves the behavior of Left and Right arrow keys
when navigating in a tree, for instance the project explorer.

It brings a behavior similar to the Windows default, that is:
  - Right expands the current node, and then selects its first child
    The Down key does not expand before it moves the cursor down.

  - Left moves to the parent node, and then collapses it.
"""

import GPS
import pygps
from gps_utils import interactive
from gi.repository import Gtk


def current_widget():
    for w in Gtk.Window.list_toplevels():
        if w.is_active():
            return w.get_focus()
    return None


def in_gtktree(ctxt):
    w = current_widget()
    return w and isinstance(w, Gtk.TreeView)


def current_row(tree):
    path, column = tree.get_cursor()
    if path:
        return path
    model, iter = tree.get_selection().get_selected()
    if model:
        return model.get_path(iter)


@interactive(category="General", filter=in_gtktree, key="Left")
def move_cursor_up_and_collapse():
    w = current_widget()    # a GtkTreeView
    path = current_row(w)
    if path:
        path.up()
        w.set_cursor(path, w.get_column(0))
        w.collapse_row(path)


@interactive(category="General", filter=in_gtktree, key="Right")
def expand_and_move_cursor_down():
    w = current_widget()    # a GtkTreeView
    path = current_row(w)
    if path:
        p = path.copy()
        path.down()
        w.set_cursor(path, w.get_column(0))
        w.expand_row(p, False)

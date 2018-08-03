"""A tree view to explore Ada/SPARK code using libadalang.
"""

import GPS
import os
from modules import Module
from gi.repository import Gtk, Gdk, GLib
from gps_utils import make_interactive

COL_LABEL = 0
COL_FOREGROUND = 1
COL_START_LINE = 2
COL_START_COLUMN = 3
COL_END_LINE = 4
COL_END_COLUMN = 5


class LAL_View_Widget():
    """The widget for the Libadalang view"""

    def __init__(self):
        self.box = Gtk.VBox()

        # A label to push diagnostics messages
        self.message_label = Gtk.Label()

        # The model: see COL_* constants above
        self.store = Gtk.TreeStore(str, Gdk.RGBA, int, int, int, int)

        # Initialize the tree view
        self.view = Gtk.TreeView(self.store)
        self.node_col = Gtk.TreeViewColumn("Node")
        cell = Gtk.CellRendererText()
        self.node_col.pack_start(cell, True)
        self.node_col.add_attribute(cell, "markup", COL_LABEL)
        self.node_col.add_attribute(cell, "foreground-rgba", COL_FOREGROUND)
        self.view.append_column(self.node_col)
        self.view.connect("button_press_event", self._on_view_button_press)

        # Pack things together
        self.box.pack_start(self.message_label, False, False, 3)
        scroll = Gtk.ScrolledWindow()
        scroll.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scroll.add(self.view)
        self.box.pack_start(scroll, True, True, 3)

        # This is the current location
        self.file = None
        self.line = 1
        self.column = 1

        # The list of iters that are currently highlighted
        self.highlighted_iters = []

        # The colors to highlight the tree with
        self.default_fg = Gdk.RGBA()
        self.highlight_fg = Gdk.RGBA()

        # Initialize the colors
        self.preferences_changed()

        # Initialize the contents
        self.refresh()

    def _on_view_button_press(self, _, event):
        """React to a button_press on the view.
        """

        if event.get_click_count() == (True, 2):
            # On a double click, select the node in the editor
            buf = GPS.EditorBuffer.get(open=False)
            if not buf:
                return False

            _, paths = self.view.get_selection().get_selected_rows()
            if not paths:
                return False

            it = self.store.get_iter(paths[0])
            row = self.store[it]
            begin_loc = buf.at(row[COL_START_LINE], row[COL_START_COLUMN])

            # Scroll to the location
            buf.current_view().goto(begin_loc)

            # Select the current node
            buf.select(begin_loc,
                       buf.at(row[COL_END_LINE], row[COL_END_COLUMN]))
            return False

    def preferences_changed(self):
        """Apply the contents of the preferences"""
        prev = (self.default_fg, self.highlight_fg)

        default = GPS.Preference("Src-Editor-Reference-Style").get()
        self.default_fg.parse(default.split('@')[1])
        highlight = GPS.Preference("Src-Editor-Keywords-Variant").get()
        self.highlight_fg.parse(highlight.split('@')[1])

        if prev != (self.default_fg, self.highlight_fg):
            self.show_current_location(self.line, self.column)

    def _add_node(self, parent, node):
        """Add a node as child of parent. parent can be None"""
        if not node:
            return
        it = self.store.append(parent)

        start_line = node.sloc_range.start.line
        end_line = node.sloc_range.end.line
        text = "<b>{}</b>{}".format(
            # Uncomment this for a representation useful for debug:
            # GLib.markup_escape_text(repr(node)),
            node.kind_name,
            " {}".format(GLib.markup_escape_text(node.text))
            if start_line == end_line else "")

        self.store[it] = [
            text,
            self.default_fg,
            start_line,
            node.sloc_range.start.column,
            end_line,
            node.sloc_range.end.column,
        ]

        for n in node.children:
            self._add_node(it, n)

    def _traverse_and_highlight(self, it, line, column):
        """Traverse the subtree starting at iter it, and highlight the
           nodes that encompass the location at line/column.

           Return the deepest iter found that matches.
        """
        lowest_found = None
        child = self.store.iter_children(it)
        while child:
            row = self.store[child]

            if row[COL_START_LINE] > line:
                # We are past the point where the nodes will match the
                # location: we can stop traversing.
                return lowest_found

            if ((row[COL_START_LINE], row[COL_START_COLUMN]) <=
                (line, column) <=
                    (row[COL_END_LINE], row[COL_END_COLUMN])):

                # This node encompasses the location: highlight it...
                lowest_found = child
                self.highlighted_iters.append(child)
                row[COL_FOREGROUND] = self.highlight_fg

                # ... and look below it for more
                below = self._traverse_and_highlight(child, line, column)
                if below:
                    lowest_found = below

            child = self.store.iter_next(child)

        return lowest_found

    def show_current_location(self, line, column):
        """Highlight the given location in the tree and scroll to it"""
        self.line = line
        self.column = column

        # Clear all previous highlighting
        for j in self.highlighted_iters:
            self.store[j][COL_FOREGROUND] = self.default_fg
        self.highlighted_iters = []

        lowest_found = self._traverse_and_highlight(None, line, column)

        # If we have finished iterating, scroll to the lowest found
        if lowest_found:
            self.view.scroll_to_cell(
                self.store.get_path(lowest_found),
                self.node_col, True, 0.5, 0.5)

    def refresh(self):
        """Refresh the contents of the view"""
        buf = GPS.EditorBuffer.get(open=False)

        if not buf:
            return

        self.store.clear()
        self.highlighted_iters = []
        self.view.set_model(None)

        self.file = buf.file()
        if not self.file.language().lower() == "ada":
            return

        self.store.clear()
        unit = buf.get_analysis_unit()

        if unit.diagnostics:
            self.message_label.set_text(
                "\n".join([str(d) for d in unit.diagnostics]))
            return
        else:
            self.message_label.set_text("{} loaded ok".format(
                os.path.basename(buf.file().name())))

        self._add_node(None, unit.root)

        cursor = buf.current_view().cursor()
        self.view.set_model(self.store)
        self.view.expand_all()
        self.show_current_location(cursor.line(), cursor.column())


class LAL_View(Module):
    """ A GPS module, providing the libadalang view """

    view_title = "Libadalang"
    mdi_position = GPS.MDI.POSITION_RIGHT
    mdi_group = GPS.MDI.GROUP_DEBUGGER_STACK

    def __init__(self):
        self.widget = None

    def setup(self):
        # Create an "open Libadalang" action
        make_interactive(
            self.get_view,
            category="Views",
            name="open Libadalang")

    def preferences_changed(self, name='', pref=None):
        if self.widget:
            self.widget.preferences_changed()

    def location_changed(self, file, line, column):
        if self.widget:
            if file != self.widget.file:
                self.widget.refresh()
            self.widget.show_current_location(line, column)

    def buffer_edited(self, file):
        if self.widget:
            self.widget.refresh()

    def on_view_destroy(self):
        self.widget = None

    def create_view(self):
        self.widget = LAL_View_Widget()
        return self.widget.box

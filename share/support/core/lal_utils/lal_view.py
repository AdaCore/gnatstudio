"""A tree view to explore Ada/SPARK code using libadalang.
"""

import GPS
import os
import libadalang
from modules import Module
from gi.repository import Gtk, Gdk, GLib, Pango
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

        self.compact_mode = True
        # The view has a compact mode (the default) and a full tree mode.
        # In full tree mode, the whole tree is displayed and refreshed when
        # the buffer is modified. In compact mode, The tree is refreshed
        # everytime the cursor location changes, and only shows the current
        # tree path.

        # A label to push diagnostics messages and token info
        self.message_label = Gtk.Label()
        self.message_label.set_halign(Gtk.Align.START)
        self.message_label.set_ellipsize(Pango.EllipsizeMode.END)

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

        full_mode_toggle = Gtk.CheckButton("full tree (slow)")
        full_mode_toggle.connect("toggled", self._full_mode_toggled)

        # Pack things together
        label_box = Gtk.HBox()
        label_box.pack_start(self.message_label, True, True, 3)
        label_box.pack_start(full_mode_toggle, False, False, 3)
        self.box.pack_start(label_box, False, False, 3)
        scroll = Gtk.ScrolledWindow()
        scroll.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scroll.add(self.view)
        self.box.pack_start(scroll, True, True, 3)

        # The contextual menu
        self.menu = Gtk.Menu()
        item = Gtk.MenuItem()
        item.set_label("view in Python Console")
        item.connect("activate", self._on_view_in_python_console)
        self.menu.append(item)
        self.menu.show_all()

        # This is the current location
        self.file = None
        self.line = 1
        self.column = 1
        self.unit = None  # The current successfully loaded AU, if any
        self.token = None  # The current token, if any

        # The list of iters that are currently highlighted
        self.highlighted_iters = []

        # The colors to highlight the tree with
        self.default_fg = Gdk.RGBA()
        self.highlight_fg = Gdk.RGBA()

        # Initialize the colors
        self.preferences_changed()

        # Initialize the contents
        self.refresh()

    def _full_mode_toggled(self, b):
        """React to the toggle of the full mode button"""
        self.compact_mode = not b.get_active()
        self.refresh()
        b = GPS.EditorBuffer.get(open=False)
        if b:
            cursor = b.current_view().cursor()
            self.show_current_location(cursor.line(), cursor.column())

    def _selected_row(self):
        """Return the selected row in self, if any"""
        _, paths = self.view.get_selection().get_selected_rows()
        if not paths:
            return None

        it = self.store.get_iter(paths[0])
        return self.store[it]

    def _on_view_in_python_console(self, _):
        """Contextual menu 'view in Python console'"""
        row = self._selected_row()
        if not row:
            return False

        GPS.execute_action("open Python")
        GPS.Console("Python").add_input(
            "node = lal_utils.node('{}', {}, {})".format(
                row[COL_LABEL].split(" ")[0][3:-4],
                row[COL_START_LINE],
                row[COL_START_COLUMN]))

    def _on_view_button_press(self, _, event):
        """React to a button_press on the view.
        """
        if event.button == 3:
            # On this button, raise the contextual menu
            self.menu.popup(None, None, None, None, 3, 0)
            return False

        if event.get_click_count() == (True, 2):
            # On a double click, select the node in the editor
            buf = GPS.EditorBuffer.get(open=False)
            if not buf:
                return False

            row = self._selected_row()
            if not row:
                return False
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

        start_line = node.sloc_range.start.line
        start_column = node.sloc_range.start.column
        end_line = node.sloc_range.end.line
        end_column = node.sloc_range.end.column

        if not self.compact_mode or (
            (start_line, start_column) <=
                (self.line, self.column) <=
                (end_line, end_column)):

            it = self.store.append(parent)
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
                start_column,
                end_line,
                end_column,
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

        if not self.unit:
            return

        self.line = line
        self.column = column

        if self.compact_mode:
            self.store.clear()
            self._add_node(None, self.unit.root)
            self.view.expand_all()
        else:
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

        # Display the current token in the label
        self.token = self.unit.lookup_token(libadalang.Sloc(line, column))
        if self.token:
            self.message_label.set_markup(
                "Token: <b>{}</b> {}".format(self.token.kind,
                                             self.token.text.strip()
                                             if self.token.text else ''))
        else:
            self.message_label.set_text("")

    def refresh(self):
        """Refresh the contents of the view"""
        buf = GPS.EditorBuffer.get(open=False)

        if not buf:
            return

        self.view.set_model(None)
        self.store.clear()
        self.highlighted_iters = []

        self.file = buf.file()
        if not self.file.language().lower() == "ada":
            return

        self.store.clear()
        unit = buf.get_analysis_unit()

        if unit.diagnostics:
            self.message_label.set_text(
                "\n".join([str(d) for d in unit.diagnostics]))
            self.unit = None
            return
        else:
            self.unit = unit
            self.message_label.set_text("{} loaded ok".format(
                os.path.basename(buf.file().name())))

        if self.compact_mode:
            # In compact mode, the view is regenerated when we change
            # locations
            pass
        else:
            # In full mode, display the whole tree now
            self._add_node(None, unit.root)

        self.view.set_model(self.store)
        self.view.expand_all()


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
        GPS.Hook("location_changed").add_debounce(
            self.location_changed_debounced)

    def preferences_changed(self, name='', pref=None):
        if self.widget:
            self.widget.preferences_changed()

    def location_changed_debounced(self, _, file, line, column):
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

"""This module gives access to all tree and tables in GPS"""


try:
    from gi.repository import Gtk, GObject, Gdk
    import os
    import sys
    import pygps
    import GPS

    def find_in_tree(tree, column, key, iter=None):
        """Return the path for the row in tree that has "key" as the
           content for the given column. Search starts at iter, or
           by default the top of the tree.
           Column is an integer (starting at 0) that indicates the
           column.
           Returns None if no such row exists."""
        if not iter:
            iter = tree.get_model()

        for row in iter:
            if row[column] == key:
                return row.path
            iter2 = row.iterchildren()
            if iter2:
                path = find_in_tree(tree, column, key, iter2)
                if path:
                    return path

        return None

    def select_in_tree(tree, column, key):
        """Select a row in a tree view. The row is such that the
           contents of the given column is key.
           :return: the path of the selected row
        """

        path = find_in_tree(tree, column, key)
        if path:
            # Expand so that path is visible, but not path itself
            if path.get_depth() >= 2:
                p = path.copy()
                p.up()
                tree.expand_to_path(p)
            tree.get_selection().select_path(path)
            pygps.process_all_events()
            return path
        return None

    def click_in_tree(view, path=None, column=0, button=1,
                      events=pygps.single_click_events, process_events=True,
                      control=False, alt=False, shift=False,
                      modifier=None, through_gps=True):
        """Simulate a click in the TreeView on the given path and column.
           This event is sent asynchronously, and you should check its
           result in an idle callback, or call process_all_events() immediately
           after the call to click_in_tree.
           If path is none, the event is sent to the first selected row.

           modifier is a Gdk.ModifierType, overriding control, alt or shift.

           If you are using the third button to display a contextual menu, see
           also activate_contextual()

           To send a double-click, emit an event with type=Gdk._2BUTTON_PRESS
        """
        if not view.get_realized():
            GPS.Logger("TESTSUITE").log("click_in_tree: view is not realized")
            return

        if (os.name == 'nt'
                and button == 3 and events == pygps.single_click_events):
            # ??? work around
            # On Windows sending a BUTTON_PRESS followed by a
            # BUTTON_RELEASE event when opening a contextual menu does
            # not work. The BUTTON_RELEASE close the contextual menu.
            # For now we remove this event.
            events = events[:1]

        if not path:
            path = view.get_selection().get_selected_rows()[1][0]
        rect = view.get_cell_area(path, view.get_column(column))

        x = float(rect.x + rect.width / 2)
        y = float(rect.y + rect.height / 2)

        if modifier is not None:
            state = modifier
        else:
            state = Gdk.ModifierType(0)
            if control:
                if sys.platform == 'darwin':
                    # on Mac, we need to also pass the Command key
                    state |= Gdk.ModifierType.MOD2_MASK
                else:
                    state |= Gdk.ModifierType.CONTROL_MASK

            if shift:
                state |= Gdk.ModifierType.SHIFT_MASK
            if alt:
                state |= Gdk.ModifierType.MOD1_MASK

        # TreeView doesn't handle single click well without
        # getting "enter-notify-event" first
        if through_gps:
            GPS.send_crossing_event(
                type=Gdk.EventType.ENTER_NOTIFY,
                window=view.get_bin_window(),
                x=int(x),
                y=int(y),
                state=0)
        else:
            event = Gdk.EventCrossing()
            event.type = Gdk.EventType.ENTER_NOTIFY
            event.window = view.get_bin_window()
            event.device = pygps.default_event_device()
            event.x = x
            event.y = y
            event.state = 0
            event.put()

        for t in events:
            if through_gps:
                GPS.send_button_event(
                    button=button,
                    x=int(x),
                    y=int(y),
                    window=view.get_bin_window(),
                    state=state,
                    type=t)
            else:
                # event = Gdk.Event.new(t)
                event = Gdk.EventButton()
                event.type = t
                event.window = view.get_bin_window()
                event.device = pygps.default_event_device()
                event.button = button
                event.x = x
                event.y = y
                event.state = state
                event.put()

        if process_events:
            pygps.process_all_events()

except ImportError:
    pass

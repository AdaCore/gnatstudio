"""This module gives access to all tree and tables in GPS"""



try:
   import gtk, gobject, os
   import pygps

   def find_in_tree (tree, column, key, iter=None):
      """Return the path for the row in tree that has "key" as the
         content for the given column. Search starts at iter, or
         by default the top of the tree.
         Column is an integer (starting at 0) that indicates the
         column.
         Returns None if no such row exists."""
      if not iter:
         was_tree = True
         iter = tree.get_model()
      else:
         was_tree = False

      for row in iter:
         if row[column] == key:
            return row.path
         iter2 = row.iterchildren()
         if iter2:
            path = find_in_tree (tree, column, key, iter2)
            if path: return path

      return None

   def select_in_tree (tree, column, key):
      """Select a row in a tree view. The row is such that the
         contents of the given column is key"""

      path = find_in_tree (tree, column, key)
      if path:
         # Expand so that path is visible, but not path itself
         if len (path) >= 2:
            tree.expand_to_path (path[:-1])
         tree.get_selection().select_path (path)
         pygps.process_all_events()

   def click_in_tree (view, path=None, column=0, button=1, \
                      events=pygps.single_click_events, process_events=True, \
                      control=False, alt=False, shift=False):
      """Simulate a click in the TreeView on the given path and column.
         This event is sent asynchronously, and you should check its
         result in an idle callback, or call process_all_events() immediately
         after the call to click_in_tree.
         If path is none, the event is sent to the first selected row.

         If you are using the third button to display a contextual menu, see
         also activate_contextual()

         To send a double-click, emit an event with type=gtk.gdk._2BUTTON_PRESS
      """

      if os.name=='nt' and button==3 and events==pygps.single_click_events:
         # ??? work around
         # On Windows sending a BUTTON_PRESS followed by a
         # BUTTON_RELEASE event when opening a contextual menu does
         # not work. The BUTTON_RELEASE close the contextual menu.
         # For now we remove this event.
         events = events[:1]

      if not path:
         path = view.get_selection().get_selected_rows()[1][0]
      model = view.get_model()
      rect = view.get_cell_area (path, view.get_column (column))

      for t in events:
         event = gtk.gdk.Event (t)
         event.window = view.get_bin_window()
         event.button = button
         event.x = float (rect.x + rect.width / 2)
         event.y = float (rect.y + rect.height / 2)

         event.state = 0
         if control: event.state = event.state or gtk.gdk.CONTROL_MASK
         if shift:   event.state = event.state or gtk.gdk.SHIFT_MASK
         if alt:     event.state = event.state or gtk.gdk.MOD1_MASK

         event.put()

      if process_events:
         pygps.process_all_events ()


except ImportError:
  pass


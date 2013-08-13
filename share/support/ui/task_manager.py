"""
This plugin implements the "Task Manager" view.
"""




import GPS
from modules import Module
from gi.repository import Gtk, GLib
from gps_utils import make_interactive

COL_LABEL = 0
COL_PROGRESS = 1
COL_PROGRESS_TEXT = 2
COL_CANCEL_PIXBUF = 3
COL_PLAYPAUSE_PIXBUF = 4
COL_TASK_ID = 5

class Task_Manager(Module):
    view_title = "Task Manager"

    def __init__(self):
        self.store = None

    def setup(self):
        make_interactive(
            self.get_view,
            category="Views",
            name="open Task Manager",
            menu="/Tools/Views/Tasks", before="Windows")

    def create_view(self):
        box = Gtk.VBox()
        scroll = Gtk.ScrolledWindow()
        self.store = Gtk.ListStore(str, int, str, str, str, str)
        self.view = Gtk.TreeView(self.store)
        self.view.set_headers_visible(False)

        scroll.add(self.view)
        box.pack_start(scroll, True, True, 0)

        # Initialize the tree view

        self.close_col = Gtk.TreeViewColumn("Close")
        cell = Gtk.CellRendererPixbuf()
        self.close_col.pack_end(cell, False)
        self.close_col.add_attribute(cell, "stock_id", COL_CANCEL_PIXBUF)
        self.view.append_column(self.close_col)

        self.view.append_column(
            Gtk.TreeViewColumn("Task", Gtk.CellRendererText(), text=COL_LABEL))

        col = Gtk.TreeViewColumn("Progress", Gtk.CellRendererProgress(),
            value=COL_PROGRESS, text=COL_PROGRESS_TEXT)
        col.set_expand(True)
        self.view.append_column(col)

        self.playpause_col = Gtk.TreeViewColumn("Play Pause")
        cell = Gtk.CellRendererPixbuf()
        self.playpause_col.pack_end(cell, False)
        self.playpause_col.add_attribute(
            cell, "stock_id", COL_PLAYPAUSE_PIXBUF)
        self.view.append_column(self.playpause_col)

        # Connect to a click on the tree view
        self.view.connect("button_press_event", self.__on_click)

        return box

    def task_started(self, task):
        """
        Add one task to the tree view.
        :param task: a GPS.Task.
        """
        if self.store and task.visible:
            iter = self.__iter_from_task(task)
            if not iter:
                iter = self.store.append()
            self.__update_row(iter, task)

    task_changed = task_started

    def task_terminated(self, task):
        if self.store:
            iter = self.__iter_from_task(task)
            if iter:
                self.store.remove(iter)

    def __task_from_row(self, path):
        """ Return the GPS.Task corresponding to the row at path.
            Verify before that the task does exist.
        """
        task_id = self.store[path][COL_TASK_ID]
        for task in GPS.Task.list():
            if task_id == str(id(task)):
                return task
        return None

    def __on_click(self, view, event):
        """ Called on a button press on the view """
        if event.button == 1:
            results = self.view.get_path_at_pos(event.x, event.y)
            if results:
                path, col, x, y = results
                if col == self.close_col:
                    task = self.__task_from_row(path)
                    if task:
                        task.interrupt()
                elif col == self.playpause_col:
                    task = self.__task_from_row(path)
                    if task:
                        if task.status() == "RUNNING":
                            task.pause()
                        else:
                            task.resume()

    def __update_row(self, iter, task):
        """ Refresh the data in iter """
        progress = task.progress()
        progress_percent = 0

        status = task.status()
        status_icon = "gtk-media-pause"
        if status == "PAUSED":
            status_icon = "gtk-media_play"

        if progress[1] > 0:
            progress_percent = (progress[0] * 100) / progress[1]

        self.store[iter] = [
            task.name(),
            progress_percent,
            "%s / %s" % (progress[0], progress[1]),
            "gtk-close",
            status_icon,
            str(id(task))]

    def __iter_from_task(self, task):
        """
        return the GtkTreeIter from a task.
        """
        iter = self.store.get_iter_first()
        s = str(id(task))
        while iter:
            task_id = self.store.get_value(iter, COL_TASK_ID)
            if s == task_id:
                return iter
            iter = self.store.iter_next(iter)
        return None

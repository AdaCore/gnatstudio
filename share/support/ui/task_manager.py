"""
This plugin implements the "Task Manager" view.
"""


import GPS
from modules import Module
from gi.repository import Gtk, GLib
from gps_utils import make_interactive

COL_PROGRESS = 0
COL_PROGRESS_TEXT = 1
COL_CANCEL_PIXBUF = 2
COL_PLAYPAUSE_PIXBUF = 3
COL_TASK_ID = 4


class Task_Manager_Widget():

    """ A widget containing a task manager """

    def __init__(self):
        self.store = None
        self.box = Gtk.VBox()
        scroll = Gtk.ScrolledWindow()
        self.store = Gtk.ListStore(int, str, str, str, str)
        self.view = Gtk.TreeView(self.store)
        self.view.set_headers_visible(False)

        scroll.add(self.view)
        self.box.pack_start(scroll, True, True, 0)

        # Initialize the tree view

        self.close_col = Gtk.TreeViewColumn("Close")
        cell = Gtk.CellRendererPixbuf()
        self.close_col.pack_end(cell, False)
        self.close_col.add_attribute(cell, "icon_name", COL_CANCEL_PIXBUF)
        self.view.append_column(self.close_col)

        col = Gtk.TreeViewColumn("Progress", Gtk.CellRendererProgress(),
                                 value=COL_PROGRESS, text=COL_PROGRESS_TEXT)
        col.set_expand(True)
        self.view.append_column(col)

        self.playpause_col = Gtk.TreeViewColumn("Play Pause")
        cell = Gtk.CellRendererPixbuf()
        self.playpause_col.pack_end(cell, False)
        self.playpause_col.add_attribute(
            cell, "icon_name", COL_PLAYPAUSE_PIXBUF)
        self.view.append_column(self.playpause_col)

        # Connect to a click on the tree view
        self.view.connect("button_press_event", self.__on_click)

        self.__on_task_changed_hook = GPS.Hook(
            "task_changed").add(self.__task_changed)
        self.__on_task_terminated_hook = GPS.Hook(
            "task_terminated").add(self.__task_terminated)

        self.box.connect("destroy", self.__destroy)

        # Initial fill: we need to do this, since the widget will not get
        # notifications for tasks that have started before it is created

        for t in GPS.Task.list():
            self.__task_changed(t)

    def __destroy(self):
        GPS.Hook("task_changed").remove(self.__on_task_changed_hook)
        GPS.Hook("task_terminated").remove(self.__on_task_terminated_hook)

    def __task_terminated(self, task):
        iter = self.__iter_from_task(task)
        if iter:
            self.store.remove(iter)

    def __task_changed(self, task):
        """
        Add one task to the tree view.
        :param task: a GPS.Task.
        """
        if task.visible:
            iter = self.__iter_from_task(task)
            if not iter:
                iter = self.store.append()
            self.__update_row(iter, task)

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
        status_icon = "gps-pause-symbolic"
        if status == "PAUSED":
            status_icon = "gps-run-symbolic"

        if progress[1] > 0:
            progress_percent = (progress[0] * 100) / progress[1]

        self.store[iter] = [
            progress_percent,  # COL_PROGRESS
            # COL_PROGRESS_TEXT
            "%s %s / %s" % (task.name(), progress[0], progress[1]),
            "gps-close-symbolic",  # COL_CANCEL_PIXBUF
            status_icon,           # COL_PLAYPAUSE_PIXBUF
            str(id(task))]         # COL_TASK_ID

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


class Task_Manager(Module):
    """ A GPS module, providing a view that wraps around a task manager """

    view_title = "Task Manager"

    def __init__(self):
        self.widget = None
        GPS.Hook("before_exit_action_hook").add(self.on_exit)

    def on_exit(self, hook):
        """ Intercept the exit hook, and present a dialog if some tasks
            are present that should block the exit.
        """
        blocking_tasks = [t for t in GPS.Task.list() if t.block_exit()]
        if not blocking_tasks:
            return True

        d = Gtk.Dialog(
            "Tasks are running",
            flags=Gtk.DialogFlags.MODAL or Gtk.DialogFlags.DESTROY_WITH_PARENT,
            parent=GPS.MDI.current().pywidget().get_toplevel())

        l = Gtk.Label(
            "The following tasks are running, do you want to quit GPS?\n"
            "Warning: Quitting will kill all running tasks")
        l.set_alignment(0.0, 0.0)
        d.get_content_area().pack_start(l, False, False, 10)

        t = Task_Manager_Widget()
        d.get_content_area().pack_start(t.box, True, True, 3)

        quit_button = d.add_button("gtk-quit", Gtk.ResponseType.YES)
        quit_button.grab_default()
        cancel_button = d.add_button("gtk-cancel", Gtk.ResponseType.CANCEL)

        d.set_default_size(400, 300)
        d.show_all()
        response = d.run()

        d.get_content_area().remove(t.box)
        d.destroy()

        if response == Gtk.ResponseType.YES:
            return True

        return False

    def setup(self):
        make_interactive(
            self.get_view,
            category="Views",
            name="open Task Manager")

    def create_view(self):
        self.widget = Task_Manager_Widget()
        return self.widget.box

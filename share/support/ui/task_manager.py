"""
This plugin implements the "Task Manager" view.
"""




import GPS
from gi.repository import Gtk, GLib
from gps_utils import interactive

COL_LABEL = 0
COL_PROGRESS = 1
COL_PROGRESS_TEXT = 2
COL_CANCEL_PIXBUF = 3
COL_PLAYPAUSE_PIXBUF = 4
COL_TASK_ID = 5

REFRESH_EVERY = 250 # milliseconds

class Task_Manager ():

    def __init__(self):
        self.box = Gtk.VBox()
        self.scroll = Gtk.ScrolledWindow()
        self.store = Gtk.ListStore(str, int, str, str, str, str)
        self.view = Gtk.TreeView(self.store)
        self.view.set_headers_visible(False)

        self.scroll.add(self.view)
        self.box.pack_start(self.scroll, True, True, 0)

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

        # schedule automatic refresh
        self.__source_id = GPS.Timeout(REFRESH_EVERY, self.__refresh)

        # Connect to a click on the tree view
        self.view.connect("button_press_event", self.__on_click)

        # Remove the refresh on destroy
        self.view.connect("destroy", self.__on_destroy)

    def __task_from_row(self, path):
        """ Return the GPS.Task corresponding to the row at path.
            Verify before that the task does exist.
        """

        try:
            task_str = self.store[path][COL_TASK_ID]
            for task in GPS.Task.list():
                if str(task) == task_str:
                    return task

            return None
        except:
            return None

    def __on_click(self, view, event):
        """ Called on a button press on the view """
        if event.button != 1:
            return

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

    def __on_destroy(self, object):
        """ Called when the tree view is being destroyed. """
        self.__source_id.remove()

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
            str(task)]

    def __add_one_task(self, task):
        """ Add one task to the tree view. task is a GPS.Task. """
        iter = self.store.append()
        self.__update_row(iter, task)

    def __refresh(self, timeout):
        """ Refresh the view """

        tasks = {} # keys: task id; values: tasks
        tasks_updated = []

        for t in GPS.Task.list():
            tasks[str(t)] = t

        # browse all rows currently displayed

        iter = self.store.get_iter_first()

        while iter:
            task_id = self.store.get_value(iter, COL_TASK_ID)

            if not task_id in tasks:
                # if the iter is not in the list of current tasks,
                # remove it and start from the top.
                if task_id in tasks_updated:
                    iter = self.store.iter_next(iter)
                else:
                    self.store.remove(iter)
                    iter = self.store.get_iter_first()
            else:
                # iter is representing a task: update the data for this task
                self.__update_row(iter, tasks[task_id])
                tasks_updated.append(task_id)
                tasks.pop(task_id)
                iter = self.store.iter_next(iter)

        # We have browsed all rows. Now, all items in tasks are those that
        # are not yet in the tree: add them now

        for t in tasks:
            self.__add_one_task(tasks[t])


@interactive("Views",
    name="open Task Manager", menu="/Tools/Views/Tasks", before="Windows")
def launch_task_manager():
    """Open the Task Manager view"""
    child = GPS.MDI.get("Task Manager")
    if child:
        # Task manager already open? raise it
        child.raise_window()
    else:
        # Create the task manager and raise it
        t = Task_Manager()
        GPS.MDI.add(t.box,
            position=GPS.MDI.POSITION_BOTTOM,
            group=GPS.MDI.GROUP_CONSOLES,
            title="Task Manager", short="Task Manager")
        child = GPS.MDI.get("Task Manager").raise_window()

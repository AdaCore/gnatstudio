"""
This plugin implements the "Tasks" view.
"""


import GPS
from modules import Module
from gi.repository import Gtk, Gdk, GLib, Pango
from gps_utils import make_interactive
import pygps

COL_PROGRESS = 0
COL_PROGRESS_TEXT = 1
COL_CANCEL_PIXBUF = 2
COL_PLAYPAUSE_PIXBUF = 3
COL_TASK_ID = 4

icon_size_action = 0


class HUD_Widget():

    """ A widget representing the GPS HUD """

    def __init__(self):
        global icon_size_action

        if not icon_size_action:
            icon_size_action = Gtk.IconSize.register("action", 10, 10)

        self.hbox = Gtk.HBox()
        self.hbox.get_style_context().add_class("gps-task-manager")
        self.label = Gtk.Label()
        self.label.set_alignment(0.0, 1.0)
        self.progress_label = Gtk.Label()
        self.progress_label.set_alignment(1.0, 1.0)
        self.progress_bar = Gtk.ProgressBar()

        self.button = Gtk.Button()
        self.button.set_relief(Gtk.ReliefStyle.NONE)
        self.button.connect("clicked", self.__on_button_clicked)
        arrow = Gtk.Arrow(Gtk.ArrowType.DOWN, Gtk.ShadowType.NONE)
        self.button.add(arrow)

        vbox = Gtk.VBox()
        self.hbox.pack_start(vbox, True, True, 0)
        label_box = Gtk.HBox()
        label_box.pack_start(self.label, True, True, 2)
        label_box.pack_end(self.progress_label, True, True, 2)
        vbox.pack_start(label_box, True, True, 0)
        vbox.pack_start(self.progress_bar, True, False, 0)
        self.hbox.pack_start(self.button, False, True, 0)
        self.refresh_timeout = None

        self.hbox.connect("destroy", self.__destroy)

        # The window that shows the mini tasks view
        self.window = Gtk.Window()
        self.window.set_decorated(False)
        self.window.connect("focus_out_event", self.__on_window_focus_out)
        self.window.connect("key-press-event", self.__on_window_key_press)

        # ??? Setting of the font could be done via CSS rather than via
        # a preference
        self.font_string = ""
        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        self.__on_preferences_changed(None)

        self.start_monitoring()

    def __destroy(self, widget):
        """ Callback on destroy """
        if self.refresh_timeout is not None:
            GLib.source_remove(self.refresh_timeout)
            self.refresh_timeout = None
        GPS.Hook("preferences_changed").remove(self.__on_preferences_changed)
        self.window.destroy()

    def __hide_auxiliary_window(self):
        """ Hide the window that is showing the mini tasks view """
        self.window.hide()
        if self.window.get_child():
            self.window.get_child().destroy()

    def __on_window_key_press(self, window, event):
        """ Callback for key_press on the auxiliary window """
        if event.get_keyval()[1] == Gdk.KEY_Escape:
            self.__hide_auxiliary_window()

    def __on_window_focus_out(self, window, item):
        """ Callback for focus out on the auxiliary window """
        self.__hide_auxiliary_window()

    def __on_button_clicked(self, button):
        """ Callback for a click on the local button """
        # show a mini tasks view

        parent_window = self.button.get_parent_window()
        parent_x, parent_y = parent_window.get_root_coords(0, 0)
        button_alloc = self.button.get_allocation()
        width, height = 400, 200

        self.window.set_default_size(width, height)
        self.window.set_attached_to(self.button)

        m = Tasks_View_Widget()
        self.window.add(m.box)

        self.window.move(
            parent_x + button_alloc.x + button_alloc.width + 1 - width,
            parent_y + button_alloc.y + button_alloc.height + 1
        )

        self.window.show_all()
        self.window.present()

    def refresh(self):
        """ Refresh the contents of the HUD """
        tasks = filter(lambda x: x.visible, GPS.Task.list())
        if len(tasks) == 0:
            # No visible tasks
            self.label.set_text("")
            self.progress_label.set_text("")
            self.progress_bar.hide()
            self.button.hide()
            self.__hide_auxiliary_window()

            # Stop monitoring if there are no tasks left
            self.refresh_timeout = None
            return False

        else:
            self.progress_bar.show_all()
            self.button.show_all()

            if len(tasks) == 1:
                # Only one visible task: set the label and button
                self.label.set_text(tasks[0].name())
                cur, tot = tasks[0].progress()
                self.progress_bar.set_fraction(float(cur)/(max(1, tot)))
                self.progress_label.set_text("{}/{}".format(cur, tot))
            else:
                self.label.set_text("{} tasks".format(len(tasks)))
                fraction = 0.0
                for t in tasks:
                    cur, tot = t.progress()
                    fraction += float(cur)/(max(1, tot))
                self.progress_bar.set_fraction(fraction/len(tasks))
                self.progress_label.set_text("")

        return True

    def start_monitoring(self):
        """ Start the background loop which refreshes the HUD """
        if not self.refresh_timeout:
            self.refresh_timeout = GLib.timeout_add(300, self.refresh)

    def __on_preferences_changed(self, pref):
        font_string_pref = GPS.Preference("General-Small-Font")

        if pref and pref != font_string_pref:
            return

        font_string = font_string_pref.get()

        if font_string != self.font_string:
            self.font_string = font_string
            font = Pango.font_description_from_string(font_string)
            self.label.override_font(font)
            self.progress_label.override_font(font)


class Tasks_View_Widget():

    """ A widget containing a task view """

    def __init__(self, hide_nonblocking=False):
        """
        :param bool hide_nonblocking: if True, tasks that do not block
           the exit dialog are not displayed
        """

        self.box = Gtk.VBox()
        self.hide_nonblocking = hide_nonblocking
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

        self.timeout = None
        self.on_empty = None

        self.box.connect("destroy", self.__destroy)

        # Initial fill: we need to do this, since the widget will not get
        # notifications for tasks that have started before it is created

        self.start_monitoring()
        self.refresh()

    def __destroy(self, widget):
        if self.timeout:
            GLib.source_remove(self.timeout)
            self.timeout = None

    def set_on_empty(self, cb):
        """
        Calls `cb` when there are no task pending any more
        """
        self.on_empty = cb

    def __task_terminated(self, task):
        iter = self.__iter_from_task(task)
        if iter:
            self.store.remove(iter)

    def __show_task(self, task):
        """
        Whether the given task should be displayed
        """
        return task.visible and \
            (not self.hide_nonblocking or task.block_exit())

    def __task_changed(self, task):
        """
        Add one task to the tree view.
        :param task: a GPS.Task.
        """
        iter = self.__iter_from_task(task)
        if not iter:
            iter = self.store.append()
        self.__update_row(iter, task)

    def refresh(self):
        """ Refresh the view """
        # First refresh the status of all tasks

        task_ids = set()

        for t in GPS.Task.list():
            if self.__show_task(t):
                self.__task_changed(t)
                task_ids.add(str(id(t)))

        # And then remove tasks that are shown that are no longer running

        iter = self.store.get_iter_first()

        while iter:
            task_id = self.store.get_value(iter, COL_TASK_ID)
            if task_id not in task_ids:
                self.store.remove(iter)
                iter = self.store.get_iter_first()
            else:
                iter = self.store.iter_next(iter)

        # Stop monitoring if there are no tasks left
        if len(task_ids) == 0:
            self.timeout = None
            if self.on_empty:
                self.on_empty()
            return False

        return True

    def start_monitoring(self):
        """ Start the background loop which refreshes the HUD """
        if not self.timeout:
            self.timeout = GLib.timeout_add(300, self.refresh)

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


class Tasks_View(Module):
    """ A GPS module, providing a view that wraps around a task manager """

    view_title = "Tasks"

    def __init__(self):
        self.widget = None  # The tasks view, if any
        self.HUD = None  # The toolbar HUD widget, if any

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

        label = Gtk.Label(
                 "The following tasks are running, do you want to quit GPS?\n"
                 "Warning: Quitting will kill all running tasks")
        label.set_alignment(0.0, 0.0)
        d.get_content_area().pack_start(label, False, False, 10)

        t = Tasks_View_Widget(hide_nonblocking=True)
        d.get_content_area().pack_start(t.box, True, True, 3)

        # If the list of tasks becomes empty, assume the user has clicked Quit
        def on_empty():
            d.response(Gtk.ResponseType.YES)
        t.set_on_empty(on_empty)

        quit_button = d.add_button("gtk-quit", Gtk.ResponseType.YES)
        quit_button.grab_default()
        d.add_button("gtk-cancel", Gtk.ResponseType.CANCEL)

        d.set_default_size(400, 300)
        d.show_all()

        # We can't call d.run(), which results in storage_error on OSX at
        # least. So instead we prevent closing GPS, and we will do so when
        # we have a YES from the user.
        def on_response(dialog, response):
            dialog.get_content_area().remove(t.box)
            dialog.destroy()
            if response == Gtk.ResponseType.YES:
                GPS.exit(force=True)   # force exit
        d.connect('response', on_response)

        return False   # prevent exit (and hide the "exit" task from dialog")

    def setup(self):
        # Add the Tasks view
        make_interactive(
            self.get_view,
            category="Views",
            name="open Tasks")

        # Create a HUD widget and add it to the toolbar
        self.HUD = HUD_Widget()
        self.HUD.hbox.show_all()
        pygps.get_widget_by_name("toolbar-box").pack_end(
            self.HUD.hbox, False, False, 3
        )

    def task_started(self):
        if self.HUD is not None:
            self.HUD.start_monitoring()
        if self.widget is not None:
            self.widget.start_monitoring()

    def on_view_destroy(self):
        self.widget = None

    def create_view(self):
        self.widget = Tasks_View_Widget()
        return self.widget.box

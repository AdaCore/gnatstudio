#!/usr/bin/python
# -*- coding: utf-8 -*-
import GPS
import inspect
import os
import imp
import sys
from pygps import *
import pygps.tree
from pygps.tree import *
import pygps.notebook
from pygps.notebook import *
from pygps.project import *
import traceback
import platform
import workflows
from workflows.promises import Promise, timeout, known_tasks


system_is_cygwin = ('uname' in os.__dict__ and
                    os.uname()[0].find('CYGWIN') != -1)
system_is_windows = system_is_cygwin or platform.system().find("Windows") != -1
system_is_osx = 'uname' in os.__dict__ and os.uname()[0].find('Darwin') != -1

# This should likely be True in general, but on some setups generating events
# results in a storage_error.
can_generate_events = True   # not system_is_osx

nightly_testsuite = os.getenv('GPS_TEST_CONTEXT') == 'nightly'


def compare_to_file(editor, filepath):
    """
    Compare the content of editor the file at filepath
    @type editor: GPS.EditorBuffer
    @type filepath: str
    """
    with open(filepath) as f:
        gps_assert(editor.get_chars().strip(), f.read().strip())


def requires_not_windows(reason=""):
    if system_is_windows:
        gps_not_run('disabled on Windows %s' % reason)


def test_pygtk():
    """Test whether pygtk support was built in GPS. This is needed if
       you want to use pywidget(), but not if you only want to use gtk
       functions like idle"""

    if 'pywidget' not in GPS.GUI.__dict__:
        gps_not_run('PyGTK support not compiled in')
    from gi.repository import Gtk, GObject, GLib


def sort_by_value(hash):
    items = sorted([(v, k) for (k, v) in hash.items()])
    items = [(k, v) for (v, k) in items]
    return items


def sort_by_key(hash):
    items = sorted([(k, v) for (k, v) in hash.items()])
    return items


def remove_extension(str):
    last_dot = str.rfind('.')
    last_window_sep = str.rfind('\\\\')
    last_unix_sep = str.rfind('/')

    if last_dot > last_window_sep and last_dot > last_unix_sep:
        return str[0:last_dot]
    else:
        return str


def get_editor_from_title(title):
    for b in GPS.EditorBuffer.list():
        if b.current_view().title() == title:
            return b
    return None


def before_exit(hook):
    """ Print error messages to stderr and exit GPS """

    global before_exit_has_run
    if before_exit_has_run == 0:
        before_exit_has_run = 1
        # We can force now, since any handling for not force has already been
        # handled
        GPS.exit(force=1, status=exit_status)
    return True


before_exit_has_run = 0
GPS.Hook('before_exit_action_hook').add(before_exit)


def gps_not_run(msg=''):
    """Set the exit status to NOT_RUN"""

    global exit_status
    exit_status = NOT_RUN
    GPS.exit(force=1, status=NOT_RUN)


def gps_fatal_error(msg):
    """Unconditional error"""

    global exit_status
    exit_status = FAILURE
    GPS.Logger('TESTSUITE').log(msg)
    GPS.exit(force=1)
    raise Exception("Fatal Error: %s" % msg)


def log_debug(var):
    """Display a variable. Convenient for debugging test scripts"""

    GPS.Logger('testsuite').log("%s" % (var, ))


def simple_error(message):
    global exit_status
    exit_status = FAILURE
    GPS.Logger('TESTSUITE').log(message)

    if not nightly_testsuite:
        GPS.MDI.dialog(message)

Known_Commands = ["load C/C++ xref info", "load entity db", "load C/C++ xref",
                  "Semantic tree update", "load constructs",
                  "Recompute Xref info"]


def safe_exit(expected_commands=[], delay=0, force=1):
    """Close the background tasks which are known to be running, and attempt
       to exit.
       expected commands contains a list of commands which are known to be
         running and which can be safely interrupted.
       If force is true, ignore any unsaved files.
       Wait at least delay milliseconds before closing.
    """

    global Known_Commands

    # This is the list of commands that are expected when running tests, and
    # which can be interrupted safely.
    expected_commands = expected_commands + Known_Commands
    commands_found = 0
    unexpected_commands = []

    # Look through all running commands, and interrupt them.
    # Emit an error for every command which is not expected to run.
    for L in GPS.Task.list():
        if L.block_exit():
            name = L.name()
            L.interrupt()
            commands_found = commands_found + 1

            if name not in expected_commands:
                unexpected_commands = unexpected_commands + [name]

    if unexpected_commands != []:
        # If we have encountered unexpected commands, emit an error.
        simple_error('Commands still running at end of test: ' +
                     str(unexpected_commands))

    # exit GPS after a timeout, so that the Tasks view has time to remove
    # the interrupted commands from the list.
    GPS.Timeout(max(delay, 100) + 300 * commands_found,
                lambda timeout: GPS.exit(force, status=exit_status))


@workflows.run_as_workflow
def wait_for_mdi_child(name, step=500, n=10):
    """
    Wait for the MDI child designated by :param str name: to be added
    to the MDI, waiting for the time specified in :param int step: n
    times.
    """

    k = 0
    while GPS.MDI.get(name) is None and k < n:
        yield timeout(step)
        k += 1


@workflows.run_as_workflow
def wait_until_not_busy(debugger, t=100):
    """
    Wait until the given GPS.Debugger is not busy
    """

    while debugger.is_busy():
        yield timeout(t)


def wait_for_entities(cb, *args, **kwargs):
    """Execute cb when all entities have finished loading.
       This function is not blocking"""

    def on_timeout(timeout):
        if GPS.Command.list() == []:
            timeout.remove()
            cb(*args, **kwargs)

    GPS.Timeout(200, on_timeout)


def wait_for_tasks(cb, *args, **kwargs):
    """Execute cb when all tasks have completed."""

    def internal_on_idle():
        cb(*args, **kwargs)

    def internal_wait_until_no_tasks(timeout):
        if GPS.Task.list() == []:
            timeout.remove()

            # Tasks can update locations view, so wait until locations view
            # has completed its operations also.

            process_all_events()
            GLib.idle_add(internal_on_idle)

    GPS.Timeout(400, internal_wait_until_no_tasks)


def wait_for_idle(cb, *args, **kwargs):

    def internal_on_idle():
        cb(*args, **kwargs)

    process_all_events()
    windows = Gtk.Window.list_toplevels()
    GLib.idle_add(internal_on_idle)


def record_time(t):
    """ Record the time t in the time.out file.
        t should be a float representing the number of seconds we want to
        record.
    """

    f = open('time.out', 'w')
    f.write(str(t))
    f.close()


def recompute_xref():
    """ Force an Xref recomputation immediately. """

    import cross_references
    cross_references.r.recompute_xref()

############
# Editors #
############


def get_all_tags(buffer, name=''):
    """return a string listing all highlighting tags used in buffer. Each
      line starts with name, then the name of the tag and the starting line
      and column, then the ending line and column.
    """

    if name:
        name = name + ' '
    result = ''
    loc = buffer.beginning_of_buffer()
    while loc < buffer.end_of_buffer():
        over = loc.get_overlays()
        if over != []:
            loc2 = loc.forward_overlay(over[0]) - 1
            result = result + name + over[0].name() \
                + ' %s:%s %s:%s\n' % (loc.line(), loc.column(),
                                      loc2.line(), loc2.column())
            loc = loc2 + 1
        else:
            loc = loc.forward_overlay()
    return result


def open_and_raise(filename, line, col):
    """Open an editor, if needed, raise it, and move the cursor to the
       specified (line, column).
    """

    buffer = GPS.EditorBuffer.get(GPS.File(filename))
    GPS.MDI.get_by_child(buffer.current_view()).raise_window()
    buffer.current_view().goto(buffer.at(line, col))


def get_completion():
    """
    Return the content, as a list of strings, of the completion window. Waits
    until it stops computing

    :rtype: Promise[Iterator[str]]
    """
    p = Promise()

    def timeout_handler(t):
        try:
            pop_tree = get_widget_by_name("completion-view")
            comps = [row[0] for row in dump_tree_model(pop_tree.get_model())]
            if comps[-1] != 'Computing...':
                t.remove()
                p.resolve(comps)
        except Exception as e:
            pass

    GPS.Timeout(100, timeout_handler)

    return p


def send_keys(*input_seq):
    """
    Workflow

    Given an input sequence composed of strings and character codes, send them
    to the application, waiting a small amount of time between each keystroke,
    to simulate human keyboard input. Returns nothing
    """
    for chunk in input_seq:
        if isinstance(chunk, int):
            send_key_event(chunk)
            yield timeout(10)
        elif isinstance(chunk, str):
            for c in chunk:
                if c == "\n":
                    send_key_event(GDK_RETURN)
                else:
                    send_key_event(ord(c))
                yield timeout(10)


######################################
# The following functions are only available if PyGTK is available
######################################

try:
    from gi.repository import Gtk, GObject, GLib

    def enqueue(fun, timeout=200):
        """ Register fun to be executed once, after timeout milliseconds.
         This function is useful for programming tests that require GPS to
         process events in a sequence."""

        GLib.idle_add(fun)

    def get_current_focus():
        """Return the widget that has the current keyboard focus"""

        grab = Gtk.grab_get_current()
        if grab:
            return grab

        for win in Gtk.Window.list_toplevels():
            if win.get_property('has-toplevel-focus'):
                return win.get_focus()
        return None

    # #####################
    # # Shortcuts editor ##
    # #####################

    def select_action_in_shortcuts_editor(action, key):
        """Select the line corresponding to action in the key shortcuts editor.
         Check that the keybinding is the one we are expecting"""

        editor = get_widget_by_name('Key shortcuts')
        gps_not_null(editor, 'Key shortcuts editor not open')
        toggle_local_config(editor, 'Show categories', False)
        process_all_events()
        tree = get_widget_by_name('Key shortcuts tree', [editor])

        for m in tree.get_model():
            if m[0].lower() == action.lower():
                current = m[1].decode('utf-8')
                gps_assert(current, key, 'Shortcut for ' + action +
                           ' is "%s", expecting "%s"' % (current, key))
                tree.get_selection().select_path(m.path)
                return editor

        gps_assert(False, True, action + ' not found in key shortcuts editor')
        return editor

    ###############################
    # Startup scripts and themes ##
    ###############################

    def load_xml_startup_script(name):
        """Load an XML startup script. Name must include the .xml extension"""

        for dir in ("%sshare/gps/support/core/" % GPS.get_system_dir(),
                    "%sshare/gps/support/ui/" % GPS.get_system_dir(),
                    "%sshare/gps/library/" % GPS.get_system_dir(),
                    "%sshare/gps/plug-ins/" % GPS.get_system_dir()):

            try:
                f = file("%s%s" % (dir, name)).read()
                break
            except:
                f = None

        GPS.parse_xml(f)
        process_all_events()

    def load_python_startup_script(name):
        """Load a python startup script, and initializes it immediately so
         that its menus are visible"""

        try:
            return sys.modules[name]
        except KeyError:
            pass

        (fp, pathname, description) = imp.find_module(name)
        try:
            module = imp.load_module(name, fp, pathname, description)
            # Special to GPS: if the module has a on_gps_started function,
            # execute it
            module.on_gps_started('gps_started')
        except AttributeError:
            pass
        finally:

            if fp:
                fp.close()

        return module

    class PyConsole(GPS.Console):

        def write(self, text):
            GPS.Console.write(self, text)
            GPS.Logger('UNEXPECTED_EXCEPTION').log(text)

    # Redirect the standard error from the Messages window to an instance of
    # the PyConsole class based on the Messages window. Each python error
    # will therefore be displayed both in the Messages window and in the traces
    # (under the UNEXPECTED_EXCEPTION debug handle).
    # Disabled on Windows for now so that we can concentrate on the other
    # issues ???

    if os.name != 'nt':
        sys.stderr = PyConsole('Messages')

    ##############
    # Notebooks ##
    ##############

    def switch_notebook_page(notebook, label):
        result = pygps.notebook.switch_notebook_page(notebook, label)
        if result == -1:
            gps_fatal_error("Notebook doesn't contain " + label + ' page')
        return result

    ######################
    # Open From Project ##
    ######################

    def open_from_project(on_open, *args, **kwargs):
        """Focus in the global search box to search files from project.
           Then call on_open and pass it the search field:

               on_open (completionList, entry, tree, *args, **kwargs)
        """
        GPS.execute_action("Global Search in context: file names")

        def on_timeout(timeout):
            timeout.remove()

            field = get_widget_by_name("global-search")
            gps_not_null(field, "Global search field not found")
            field = get_widgets_by_type(Gtk.Entry, field)[0]
            gps_not_null(field, "Global search contains no GtkEntry")

            popup = get_widget_by_name("completion-list")
            gps_not_null(popup, "Global search's completion list not found")

            tree = get_widgets_by_type(Gtk.TreeView, popup)[0]

            on_open(*(popup, field, tree) + args, **kwargs)

        GPS.Timeout(200, on_timeout)

    ############
    # Dialogs ##
    ############

    def open_key_shortcuts(on_open, *args, **kwargs):
        """Open the keyshortcuts editor, and call
          on_open (dialog, *args, **kwargs)"""

        open_menu('/Edit/Key Shortcuts...', on_open, [], args, kwargs)

    def open_file_switches(on_open, *args, **kwargs):
        """Open the file-specific switches editor, and call
          on_open (mdichild, tree, *args, **kwargs)"""

        def on_timeout(timeout):
            timeout.remove()
            mdi = GPS.MDI.get('Project Switches')
            tree = get_widgets_by_type(Gtk.TreeView,
                                       mdi.get_child().pywidget())[0]
            on_open(*(mdi, tree) + args, **kwargs)

        GPS.Timeout(1000, on_timeout)
        GPS.Menu.get(
            '/Project/Edit File Switches...').action.execute_if_possible()

    def open_breakpoint_editor(on_open, *args, **kwargs):
        """Open the breakpoint editor dialog and call
          on_open (MDIWindow, *args, **kwargs)"""

        def __internal():
            m = GPS.MDI.get('Breakpoints')
            if not m:
                return True  # Wait again
            on_open(*(m, ) + args, **kwargs)
            return False

        GLib.timeout_add(200, __internal)
        GPS.Menu.get('/Debug/Data/Breakpoints').action.execute_if_possible()

    ############
    # Wizards ##
    ############

    def wizard_current_page(wizard):
        """Return the widget currently visible in the wizard"""

        contents = get_widget_by_name('wizard contents', wizard)
        for w in contents.get_children():
            if w.get_mapped():
                return w
        return None

    ############################
    # TextView and TextBuffer ##
    ############################

    def iter_from_location(loc):
        """Creates a Gtk.TextIter from an EditorLocation"""

        view = text_view_from_location(loc)
        b = view.get_buffer()

        mark_name = "iter_from_loc_temp_mark"

        _ = loc.create_mark(mark_name)
        mark = b.get_mark(mark_name)

        return b.get_iter_at_mark(mark)

    def compare_editor_contextual(loc, expected, indexes=None, msg='',
                                  when_done=None):
        """Check the contextual menu in an editor at a specific location.
         indexes could be set to range(0,2) to only check part of the
         menu. when_done is done when the contextual menu has been computed
         (since this is done asynchronously)"""

        def on_contextual(windows):
            menu = dump_contextual(windows)
            if indexes:
                menu = [menu[r] for r in indexes]
            gps_assert(expected, menu, msg)
            close_contextual(windows)
            if when_done:
                when_done()

        def wait_for_editor():
            windows = Gtk.Window.list_toplevels()
            GLib.idle_add(on_contextual, windows)
            click_in_text(loc, button=3)

        GLib.idle_add(wait_for_editor)  # Make sure editor is displayed

    ###########
    # Canvas ##
    ###########

    def click_in_canvas(canvas, xoffset=0, yoffset=0, button=1,
                        events=single_click_events):

        origin = canvas.get_window().get_origin()
        click_in_widget(
            canvas.get_window(), x=origin[0] + xoffset, y=origin[1] +
            yoffset, button=button, events=events)

    #####################
    # Dialogs          ##
    #####################
    # Dialogs are open asynchronously, so if you want to detect whether a
    # dialog has been opened, you must use code similar to:
    #  def on_gps_started (h):
    #     before_dialog (on_dialog, args)
    #     ... action that opens the dialog
    #
    #  def on_dialog (dialog, args):
    #     ...

    def get_new_toplevels(old_toplevels):
        """
        Compare the current list of toplevel windows with the one stored
        in old_toplevels, and returns list of new windows.
        This can be used to get a handle on a window that was just opened by
        an action:
            old = Gtk.Window.list_toplevels()
            ...
            dialog = get_new_toplevels(old)
        """
        return [w for w in Gtk.Window.list_toplevels()
                if w not in old_toplevels and w.get_mapped()]

    def before_dialog(callback, args=[], kwargs=dict()):
        """Return the current context, needed to compute later on what dialogs
        were opened in between.
        Callback's first argument is the first window opened

        Use wait_for_dialog() instead
        """

        def on_dialog(windows):
            new = [w for w in Gtk.Window.list_toplevels() if w not in
                   windows and w.get_mapped()]
            if new:
                params = [new[0]]
            else:
                params = [None]

            callback(*params + args, **kwargs)

        windows = Gtk.Window.list_toplevels()
        GLib.idle_add(on_dialog, windows)

    def wait_for_dialog(func):
        """
        Execute func() and wait until a new dialog appears on screen.
        Returns that dialog.

            dialog = yield wait_for_dialog(button.click)
        """
        windows = Gtk.Window.list_toplevels()
        func()
        while True:
            yield wait_idle()
            new = [w for w in Gtk.Window.list_toplevels() if w not in
                   windows and w.get_mapped()]
            if new:
                yield new[0]
                break

    #####################
    # Contextual menus ##
    #####################

    def get_contextual(old_windows, is_fatal=True):
        """Return the contextual menu that was displayed. old_windows is the
         list of windows before you opened the contextual menu"""

        c = [w for w in Gtk.Window.list_toplevels() if w not in
             old_windows and w.get_mapped()]
        if not c:
            if is_fatal:
                gps_fatal_error('No contextual menu created')
            return None
        return c[0]

    def activate_contextual(old_windows, label, accel_prefix="<gps>/"):
        """Activate a contextual menu. Old_Windows should be the list of
         toplevel windows that existed before the contextual menu was
         displayed:
              windows = Gtk.Window.list_toplevels ()
              ...
              activate_contextual (windows, "FOO")
         This is a low-level function, consider using select_editor_contextual
         when dealing with editors.
        """
        contextual = get_contextual(old_windows)
        contextual = MenuTree(contextual, accel_prefix=accel_prefix)

        goal = '%s%s' % (accel_prefix, label)
        for (menu, menu_label, accel, level) in contextual:
            if menu_label == goal:
                menu.activate()
                return True
        gps_fatal_error("Couldn't find contextual menu %s" % label)
        return False

    def dump_contextual(old_windows):
        """Dump the contextual menu (see dump_menu). old_windows is the
         list of toplevel windows that existed before the contextual menu
         is displayed

         :param old_windows: a list of Gtk.Window, which is used
            to find a new window and use it as the contextual menu
         """

        try:
            contextual = get_contextual(old_windows, is_fatal=False)
            return dump_menu('', topwidget=contextual)
        except:
            return None

    def close_contextual(old_windows):
        """Close the contextual menu opened since old_windows was computed"""

        try:
            contextual = get_contextual(old_windows, is_fatal=False)
            contextual.destroy()
        except:
            pass

    def select_widget_contextual(widget, menuName, onselected, *args,
                                 **kwargs):
        """Display the contexual menu on any widget"""

        process_all_events()
        windows = Gtk.Window.list_toplevels()
        click_in_widget(widget.get_window(), button=3)

        def internal_onselected(windows):
            process_all_events()
            onselected(*args, **kwargs)

        GLib.idle_add(internal_onselected, windows)
        activate_contextual(windows, menuName)

    def select_editor_contextual(menuName, onselected=None, *args, **kwargs):
        """Select the selection of a contextual menu in the current editor.
           When the menu item has been selected, the menu is closed and
           onselected is called with the extra arguments passed to this
           function.
        """

        process_all_events()
        windows = Gtk.Window.list_toplevels()
        click_in_text(GPS.EditorBuffer.get().current_view().cursor(), button=3)

        def internal_onselected(windows):
            close_contextual(windows)
            process_all_events()
            if onselected:
                onselected(*args, **kwargs)

        GLib.idle_add(internal_onselected, windows)
        activate_contextual(windows, menuName)

    def toggle_local_config(view, text, value=None):
        """
        Open the local config menu for the view, and selects the menu with
        the "text" label (either set it active, inactive, or just toggle,
        depending on value)
        """

        def onidle(windows):
            menu = get_contextual(windows)
            for m in WidgetTree(menu):
                if isinstance(m, Gtk.Menu):
                    for w in MenuTree(m):
                        if w[1] == '<gps>/%s' % text:
                            if value is None:
                                w[0].emit("toggled")
                            elif value:
                                w[0].set_active(True)
                            else:
                                w[0].set_active(False)
                            process_all_events()
                            return
            GPS.Logger('TESTSUITE').log('Local config not found "%s"' % text)

        windows = Gtk.Window.list_toplevels()
        p = view
        while p.get_parent() and p.__class__.__name__ != 'AdaMDIChild':
            p = p.get_parent()

        b = get_widget_by_name('local-config', [p])
        button = b.get_child()
        assert isinstance(button, Gtk.Button)
        # ??? Sending an event doesn't seem to work because there is a grab
        # pending. The error might be because we generate our events with a
        # 0 timestamp, which might be "older" than the grab timestamp.
        click_in_widget(button.get_window(), button=1,
                        events=[Gdk.EventType.BUTTON_PRESS])
        GLib.idle_add(onidle, windows)

    def select_locations_contextual(menuName, onselected, *args, **kwargs):
        """Select the selection of a contextual menu in the locations window.
           When the menu item has been selected, the menu is closed and
           onselected is called with the extra arguments passed to this
           function
        """

        def internal_onidle(windows):
            tree = pygps.get_widgets_by_type(
                Gtk.TreeView,
                GPS.MDI.get('Locations').pywidget())[0]
            model = tree.get_model()

            if tree.get_selection().get_mode() == Gtk.SelectionMode.MULTIPLE:
                m, selected = tree.get_selection().get_selected_rows()
                path = selected[0]
            else:
                path = model.get_path(tree.get_selection().get_selected()[1])

            process_all_events()
            click_in_tree(tree, path, button=3)

            def internal_onselected(windows):
                close_contextual(windows)
                process_all_events()
                onselected(*args, **kwargs)

            GLib.idle_add(internal_onselected, windows)
            activate_contextual(windows, menuName)

        process_all_events()
        windows = Gtk.Window.list_toplevels()
        GLib.idle_add(internal_onidle, windows)

    def select_coverage_contextual(menuName, onselected, *args, **kwargs):
        """Select the selection of a contextual menu in the Code Coverage
           window.
           When the menu item has been selected, the menu is closed and
           onselected is called with the extra arguments passed to this
           function
        """

        def internal_onidle(windows):
            tree = get_widget_by_name('Coverage')
            model = tree.get_model()
            path = model.get_path(tree.get_selection().get_selected()[1])

            process_all_events()
            click_in_tree(tree, path, button=3)

            def internal_onselected(windows):
                close_contextual(windows)
                process_all_events()
                onselected(*args, **kwargs)

            GLib.idle_add(internal_onselected, windows)
            activate_contextual(windows, menuName)

        process_all_events()
        windows = Gtk.Window.list_toplevels()
        GLib.idle_add(internal_onidle, windows)

    def dump_locations_tree():
        mdi = GPS.MDI.get('Locations')
        if mdi is None:
            simple_error('Locations window is not opened')
            safe_exit()
        else:
            tree = pygps.get_widgets_by_type(Gtk.TreeView, mdi.pywidget())[0]
            return Tree(tree).dump_model(column=7)

    def load_messages_from_file(name, onload, *args, **kwargs):
        """Loads contents of the Messages window and parse it to fill Locations
           view.
        """

        def internal_onloaded():
            onload(*args, **kwargs)

        def internal_onfileopendialog(dialog):
            entry = pygps.get_widgets_by_type(Gtk.Entry, dialog)[0]
            entry.set_text(name)
            get_stock_button(dialog, Gtk.STOCK_OK).clicked()
            wait_for_tasks(internal_onloaded)

        before_dialog(internal_onfileopendialog)
        GPS.execute_action('Messages load from file')

    class Test_Queue:
        """A list of tests to perform. One test is executed when the previous
           has finished (after setting an explicit flag). The goal is that
           tests that need idle_callbacks can still be performed sequentially.

           Example of use:

            q = Test_Queue ()

            def my_test (p1, p2):
                ...
                q.test_finished ()

            q.add (my_test, param1, param2)
            q.add (my_test, param3, param4)

           The queue will automatically start executing in the "gps_started"
           hook callback, unless you pass auto_execute to False in the
           constructor. This means you do not have to connect to that hook
           yourself
        """

        def __init__(self, auto_exec=True):
            """If auto_exec is True, execute the loop automatically when the
            gps_started callback is called. Otherwise no automatic execution,
            you'll need to call execute() explicitly"""

            self.list = []
            if auto_exec:
                GPS.Hook('gps_started').add(self.execute)

        def add(self, callback, *args, **kwargs):
            """Add a new test to be executed when the previous ones have
               finished.
            """

            self.list.append((callback, args, kwargs))

        def test_finished(self):
            """Should be called by tests after they have finished executing"""

            # We'll start the next test in an idle, so that the current one is
            # properly terminated, and we do not execute in its context

            GLib.idle_add(self._do_test)

        def _do_test(self):
            """Execute the next test in the list"""

            process_all_events()

            if self.list:
                (callback, args, kwargs) = self.list.pop(0)
                callback(*args, **kwargs)
            else:
                safe_exit(force=1)

        def execute(self, *args):
            """Execute all tests on the list, and then exit GPS.
            This function returns when the first test has finished executing
            or is waiting in an idle loop"""

            # We accept any number of args because this can either be called
            # explicitely by the user, or as part of the gps_started hook
            self._do_test()

except:
    # No graphical mode
    def enqueue(fun, timeout=200):
        """ Register fun to be executed once, after timeout milliseconds.
        This function is useful for programming tests that require GPS to
        process events in a sequence."""

        def on_timeout(timeout):
            timeout.remove()
            fun()

        GPS.Timeout(timeout, on_timeout)


################################
# Below are just examples for now

# Open a menu from PyGtk (instead of using a GPS action):
#     GPS.Menu.get ("/File/New").pywidget().activate()

# Getting the second column of the first grand-child of the root of a
# TreeModel
#     print model["0:0:0"][1]
#  or print model[(0,0,0)][1]

# Last line of a treeModel:
#     model[-1].path

# Print the second column for all top-level nodes of a TreeModel
#     for row in model:
#       print row[1]

# Same as above, get result as list
#     values = [row[1] for row in model]

# Delete a row from a tree model
#     del model[0]

# Getting the tree view widget:
#     mdi_widget=GPS.MDI.get("Project").pywidget().get_child() \
#        .get_children()[1]
#     scrolled  = mdi_widget.get_children()[0].get_children()[0]
#     tree      = scrolled.get_child()
#     model     = tree.get_model()
# This can also be done by getting the widget by its name:
#     box = get_widget_by_name ("Project")


# Make visible for tests that only to "from testsuite import *"
from driver import *
from dialogs import *
from asserts import *
from tree import *
from menu import *
from editor import *
from vcs import *

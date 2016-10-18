#!/usr/bin/python
# -*- coding: utf-8 -*-

# This module contains various classes to interface with GPS's dialogs

import GPS
from gi.repository import Gtk
from asserts import gps_assert, gps_not_null
from tree import Tree
from workflows.promises import timeout, wait_idle, Promise, modal_dialog, \
    idle_modal_dialog, wait_tasks
from pygps import get_stock_button, get_widget_by_name, WidgetTree, \
    get_button_from_label, get_widgets_by_type, select_combo, \
    get_window_by_prefix, get_window_by_title
import pygps.tree
from pygps.tree import select_in_tree, click_in_tree
import gps_utils
import re


##########
# Dialog #
##########

class Dialog(object):
    """
    An abstract class, used by other classes that open dialogs.
    It is meant to be used from a run_test_driver test.
    """

    def _open_and_yield(self, action_name, timeout_ms=300, retries=6):
        """
        Compatible with run_test_driver, to be used in a yield statement
            editor = Project_Properties_Editor()
            yield editor.open_and_yield()
            ...
        :param action_name: the name of the action that will open window
        """

        windows = Gtk.Window.list_toplevels()
        yield modal_dialog(lambda: GPS.execute_action(action_name), timeout_ms)

        self.dialogs = []
        attempt = 0
        while True:
            self.dialogs = [
                w for w in Gtk.Window.list_toplevels()
                if w not in windows and w.get_mapped()]

            if self.dialogs:
                # Wait little bit more to allow dialog to be dispayed and
                # focus to be moved.
                yield timeout(timeout_ms)
                break
            attempt += 1
            if attempt > retries:
                break
            yield timeout(timeout_ms)

    def ok(self):
        """
        Press the OK button. Use as::
            yield self.ok()
        """
        get_stock_button(self.dialogs, Gtk.STOCK_OK).clicked()
        yield wait_idle()

    def cancel(self):
        """
        Press the cancel button. Use as::
            yield self.cancel()
        """
        get_stock_button(self.dialogs, Gtk.STOCK_CANCEL).clicked()
        yield wait_idle()


######################
# Project properties #
######################

class Project_Properties_Editor(Dialog):
    """
    A class that interfaces with the project properties editor in GPS.
    This is really meant for interfacing with the GUI itself. The
    :class:`GPS.Project` class provides more convenient methods for
    modifying a project directly.

    A lot of functions use a path attribute, which describes how to
    access the page, for instance 'Sources/Naming/Ada'.
    """

    COLUMN_WIDGET = 1
    COLUMN_PATH = 3

    def __init__(self):
        """
        Opens the project properties editor, and calls::
            self.on_open()
        """
        self.treeview = None

    def open_and_yield(self, wait_scan=True):
        """
        Compatible with run_test_driver, to be used in a yield statement
            editor = Project_Properties_Editor()
            yield editor.open_and_yield()
            ...

        if :param wait_scan: is True, this will wait for GPRconfig to complete
        its scanning before returning.
        """
        yield self._open_and_yield('open project properties')

        # Wait for the GPRconfig scan to complete before editing
        # and/or saving the Project Properties editor
        if wait_scan:
            yield wait_tasks()

        self.treeview = get_widget_by_name(
            'Project Properties Tree', self.dialogs)

    def select(self, path):
        """
        Show the page corresponding to a specific path

        :param path: see above for a description of path.
        :return: a GtkTreePath for the page
        """
        return pygps.tree.select_in_tree(
            self.treeview, Project_Properties_Editor.COLUMN_PATH, path)

    def get_page(self, path):
        """
        Return the widget that is displayed when the user selects a
        specific page.
        :param path: see above for a description of path.
        :return: a widget
        """
        rowpath = self.select(path)
        gps_not_null(rowpath, 'Page "%s" not found' % path)
        model = self.treeview.get_model()
        iter = model.get_iter(rowpath)
        return model[iter][Project_Properties_Editor.COLUMN_WIDGET]

    GNAT_NAMING_SCHEME = 0
    APEX_NAMING_SCHEME = 1

    def set_ada_naming_scheme(self, scheme=GNAT_NAMING_SCHEME):
        page = self.get_page('Sources/Naming/Ada')
        ent = [w for w in WidgetTree(page)
               if isinstance(w, Gtk.ComboBox) and
               w.get_active_text() == "GNAT default"]
        gps_not_null(ent, "Entry field for GNAT default not found")
        ent[0].set_active(scheme)

    def toggle_language(self, lang):
        if not isinstance(lang, list):
            lang = [lang]

        page = self.get_page('Sources/Languages')
        tree = pygps.get_widgets_by_type(Gtk.TreeView, page)[0]
        found = 0
        for m in tree.get_model():
            if m[1] in lang:
                pygps.tree.click_in_tree(tree, m.path)
                found += 1
        gps_assert(found, len(lang), "Some languages not found %s" % lang)


################
# Project view #
################

class Project_View(Dialog, Tree):
    """
    Interface to the project view
    """

    def __init__(self):
        Tree.__init__(self, get_widget_by_name('Project Explorer Tree'))

    def compare_contents(self, expected, msg='', column=1):
        Tree.compare_contents(self, expected, msg=msg, column=column)

    def open_and_yield(self):
        yield self._open_and_yield("/Tools/Views/Project")
        self.dialog = get_widget_by_name('Project Explorer Tree')


#############
# Bookmarks #
#############

class Bookmarks(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('open Bookmarks')
        self.treeview = get_widget_by_name('Bookmark TreeView')


#################
# Key Shortcuts #
#################

class KeyShortcuts(Dialog):

    def open_and_yield(self):
        preferences_dialog = Preferences()
        yield preferences_dialog.open_and_yield()
        preferences_dialog.select_page("Key Shortcuts")

        self.editor = get_widget_by_name('Key shortcuts')

        if self.editor:
            self.modify_button = get_button_from_label('Modify', self.editor)
            self.remove_button = get_button_from_label('Remove', self.editor)
            self.close_button = get_button_from_label('Close')

    def yield_modify(self):
        yield idle_modal_dialog(self.modify_button.clicked)

    def yield_remove(self):
        self.remove_button.clicked()
        yield wait_idle()

    def yield_close(self):
        self.close_button.clicked()
        yield wait_idle()

    def select_action(self, action):
        from GPS import process_all_events

        tree = get_widget_by_name('Key shortcuts tree', [self.editor])
        GPS.Preference("shortcuts-categories").set(False)
        # ??? Used to manipulate config menu, but this seems to fail now
        # toggle_local_config(self.editor, 'Show categories', False)

        for m in tree.get_model():
            if m[0].lower() == action.lower():
                tree.get_selection().select_path(m.path)
                return

        gps_assert(False, True, action + ' not found in key shortcuts editor')


###############
# Preferences #
###############

class Preferences(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('/Edit/Preferences...')
        self.dialog = get_window_by_prefix('GPS - Preferences -')
        self.tree = get_widgets_by_type(Gtk.TreeView,
                                        self.dialog)[0]

    def select_page(self, page_name):
        select_in_tree(self.tree, 0, page_name)

    def get_selected_page_name(self):
        model, iter = self.tree.get_selection().get_selected()
        return model.get_value(iter, column=0)


########################
# Build targets editor #
########################

class BuildTargetsEditor(Dialog):
    """
    An interface for the Build Targets editor dialog.
    It opens up the dialog, and then allows to select a given page
    and to get pointers on widgets.
    """

    @staticmethod
    def get_switch_label(switch, gtk_type):
        if (gtk_type == Gtk.ToggleButton or
                gtk_type == Gtk.Button or
                gtk_type == Gtk.RadioButton):
            return switch.get_label()
        else:
            labels = get_widgets_by_type(Gtk.Label, switch.get_parent())
            if labels:
                return labels[0].get_label()
            else:
                return ""

    def open_and_yield(self):
        """
        Open the build targets editor dialog, and returns a handle to it.

        This is compatible with run_test_driver:
            editor = GPS.BuildTargetsEditor()
            yield editor.open_and_yield()
        """

        yield self._open_and_yield('/Build/Settings/Targets')
        self.dialog = get_window_by_prefix('Target Configuration')
        self.tree = get_widgets_by_type(Gtk.TreeView,
                                        self.dialog)[0]

    def select_page(self, page_name):
        """
        Select the given page in the Build Targets editor tree view.

        Here is a simple example showing how to select a page in
        the editor:

            # open the editor first
            editor = GPS.BuildTargetsEditor()
            yield editor.open_and_yield()

            # select the wanted page
            editor.select_page("Build All")
        """

        select_in_tree(self.tree, 1, page_name)

    def get_switch(self, label, gtk_type):
        """
        Return the switch widget identified by ``label`` and
        belonging to the given ``gtk_type``.

        Here is a simple example showing how to get the widget
        associated to a given switch:
            # open the editor first
            editor = GPS.BuildTargetsEditor()
            yield editor.open_and_yield()

            # select the page containing the switch
            editor.select_page("Build All")

            # get the widget associated to the 'Compile only'
            # switch
            switch_widget = editor.get_switch("Compile only", Gtk.ToggleButton)
        """

        switches = get_widgets_by_type(gtk_type, self.dialog)

        result = [switch for switch in switches
                  if BuildTargetsEditor.get_switch_label(
                      switch, gtk_type) == label and
                  switch.get_mapped()]

        if result:
            return result[0]
        else:
            return None


##########
# Search #
##########

class Search(Dialog):
    """
    An interface for the Search dialog. It opens up the dialog, and then
    gets pointers to the various widgets into fields of Search.
    """

    Context = gps_utils.enum(
        CURRENT_FILE="Current File",
        CURRENT_SELECTION="Current Selection",
        OPEN_FILES="Open Files",
        FILES="Files...",
        FROM_FROM_RUNTIME="Files From Runtime",
        FILES_FROM_PROJECT="Files From Projects")

    def open_and_yield(self):
        """
        Open the search dialog, and returns a handle to it.
        This is compatible with run_test_driver:
            editor = GPS.Search()
            yield editor.open_and_yield()
        """

        yield self._open_and_yield("Search")
        self.dialog = get_window_by_prefix('GPS - Search -')

        if self.dialog:
            combos = get_widgets_by_type(Gtk.ComboBox, self.dialog)

            self.find = get_button_from_label("Find", self.dialog)
            self.find_all = get_button_from_label("Find All", self.dialog)
            self.next = self.find   # This is in fact the same button

            self.replace = get_button_from_label("Replace", self.dialog)
            self.replace_all = get_button_from_label(
                "Replace All",
                self.dialog)
            self.close = get_button_from_label("Close", self.dialog)
            self.replace_and_find = get_button_from_label(
                "Replace & Find", self.dialog)
            self.scope = get_widget_by_name(
                "search scope combo", self.dialog)
            self.pattern = combos[0].get_child()
            self.replace_text = combos[1].get_child()
            self.look_in = combos[3] if len(combos) >= 4 else combos[2]
            self.previous = get_button_from_label("Previous", self.dialog)

            toggle_buttons = get_widgets_by_type(Gtk.ToggleButton, self.dialog)
            self.regexp = toggle_buttons[0]
            self.case = toggle_buttons[1]
            self.whole_word = toggle_buttons[2]

    def current_scope(self):
        """
        Return the name of the current context
        """
        return self.scope.get_model()[self.scope.get_active()][0]

    def set_scope(self, name):
        """
        Select a specific scope.
        :param: either a string (the name to select) or one of the constants
           defined in Search.Context
        """
        if isinstance(name, str):
            select_combo(self.scope, name)
        else:
            self.look_in.set_active_text(name)

    def yield_find(self):
        # Could open a modal dialog to warn that we reached the end
        yield idle_modal_dialog(self.find.clicked)

    def yield_find_all(self):
        yield idle_modal_dialog(self.find_all.clicked)

    def yield_replace(self):
        yield idle_modal_dialog(self.replace.clicked)

    def yield_replace_all(self):
        yield idle_modal_dialog(self.replace_all.clicked)

    def yield_replace_and_find(self):
        yield idle_modal_dialog(self.replace_and_find.clicked)

    def yield_close(self):
        self.close.clicked()
        yield wait_idle()


##########
# Breakpoints view
##########

class _Breakpoints_Editor(Dialog):

    def __init__(self, action_name):
        self.action_name = action_name

    def open_and_yield(self):
        yield self._open_and_yield(self.action_name)
        fields = get_widgets_by_type(Gtk.Entry, self.dialogs)
        self.filename = fields[0]
        self.line = fields[1]


class Breakpoints_View():

    def __init__(self):
        GPS.execute_action("open breakpoints editor")
        self.view = GPS.MDI.get("Breakpoints")
        self.list = get_widgets_by_type(Gtk.TreeView, self.view.pywidget())
        gps_not_null(self.list, 'List of breakpoints not found')
        self.list = self.list[0]

    def select(self, num):
        """
        Select the num-th breakpoint in the list
            b = Breakpoints_View()
            yield b.select(1)
        """
        self.list.get_selection().select_path("%d" % num)
        yield wait_idle()

    def edit(self):
        """
        Show the properties of the selected breakpoint.
        The Breakpoints view must have the focus.
        b = Breakpoints_View()
        ed = b.edit()
        yield ed.open_and_yield()
        """
        return _Breakpoints_Editor('debug edit breakpoint')

    def create(self):
        """
        Open the dialog to create a new breakpoint
        """
        return _Breakpoints_Editor('debug create breakpoint')


###################
# Debug_Run_Dialog
###################

class Debug_Run_Dialog(Dialog):

    def open_and_yield(self):
        """
        Compatible with run_test_driver, to be used in a yield statement
            dialog = Debug_Run_Dialog()
            yield dialog.open_and_yield()
        """
        yield self._open_and_yield("/Debug/Run...")
        yield wait_idle()

    def set_use_exec_dir(self, value):
        """
        Check or uncheck "Use exec dir"

        :param value: True or False
        """
        check = [w for w in get_widgets_by_type(Gtk.CheckButton, self.dialogs)
                 if w.get_label().startswith("Use exec dir")][0]
        check.set_active(value)


##########################
# Gtk_File_Chooser_Dialog
##########################

class Gtk_File_Chooser_Dialog(Dialog):

    def open_and_yield(self, action):
        """
        Compatible with run_test_driver, to be used in a yield statement
            dialog = Gtk_File_Chooser_Dialog()
            yield dialog.open_and_yield("open file")
            ...
        :param action: the name of the action that will open window
        """
        yield self._open_and_yield(action)
        self.dialog = self.dialogs[0]

    def select_file(self, name):
        """
        Select given file in dialog
        """
        self.dialog.select_uri(
            self.dialog.get_current_folder_uri() + "/" + name)

    def cancel(self):
        """
        Press the Cancel button. Use as::
            yield dialog.cancel()
        """
        get_button_from_label("Cancel", self.dialog).clicked()
        yield timeout(300)

    def ok(self):
        """
        Press the OK button. Use as::
            yield dialog.ok()
        """
        get_button_from_label("OK", self.dialog).clicked()
        yield timeout(300)


###########
# Variables view
###########

class Variables_View(Dialog):

    def open_and_yield(self):
        """
        Compatible with run_test_driver, to be used in a yield statement
            view = Variables_View()
            yield view.open_and_yield()
            ...
        """
        yield self._open_and_yield("open debugger variables window")
        GPS.execute_action("open debugger variables window")
        self.view = GPS.MDI.get("Debugger Variables")
        gps_not_null(self.view, "Variables view not found")
        self.tree = get_widgets_by_type(Gtk.TreeView, self.view.pywidget())
        self.tree = self.tree[0]

    @staticmethod
    def display(expression):
        d = GPS.Debugger.get()
        d.send("tree display %s" % expression)

    def expand(self, path):
        self.tree.expand_row(Gtk.TreePath(path), False)

    def dump(self):
        r = re.compile("0x[0-9a-f]+")
        m = self.tree.get_model()

        def internal(iter):
            result = []
            while iter is not None:
                v = str(m[iter][0])
                v = r.sub("0xfff", v)
                result.append(v)

                if m.iter_has_child(iter):
                    result.append(internal(m.iter_children(iter)))

                iter = m.iter_next(iter)
            return result

        return internal(m.get_iter_first())


##############
# Custom Build
##############

class Custom_Build_Dialog(Dialog):
    def open_and_yield(self):
        yield self._open_and_yield("/Build/Project/Custom Build...")
        self.dialog = get_window_by_title("Custom Build...")

    def get_command_line_entry(self):
        return get_widgets_by_type(Gtk.Entry, self.dialog)[0]

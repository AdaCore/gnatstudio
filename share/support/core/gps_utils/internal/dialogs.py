#!/usr/bin/python
# -*- coding: utf-8 -*-

# This module contains various classes to interface with GPS's dialogs

import GPS
from gi.repository import Gtk
from asserts import gps_assert, gps_not_null
from tree import Tree
from workflows.promises import timeout, wait_idle, modal_dialog, \
    idle_modal_dialog, wait_tasks
from pygps import get_stock_button, get_widget_by_name, WidgetTree, \
    get_button_from_label, get_widgets_by_type, select_combo, \
    get_window_by_prefix, get_window_by_title
import pygps.tree
from pygps.tree import select_in_tree
import gps_utils
from gps_utils.internal.tree import dump_tree_model
from gps_utils.internal.editor import click_in_widget
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

        scenario_selector = get_widget_by_name(
            'Project Properties Scenario Selector', self.dialogs)
        self.scenario_selector_tree = get_widgets_by_type(
            Gtk.TreeView, scenario_selector)[0]

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

    def get_scenarios(self):
        """
        Return the selected scenarios in the Project Properties editor, in the
        following format:

        [var_name_1, [value_1, value_2], var_name_2, [value_3, ...], ...]
        """
        result = []

        for m in self.scenario_selector_tree.get_model():
            if m[0]:
                result.append(m[1])
                children = m.iterchildren()
                children_list = []
                for child in children:
                    if child[0]:
                        children_list.append(child[1])
                if len(children_list) > 0:
                    result.append(children_list)

        return result

    def select_scenario(self, var, values=None):
        """
        Select a scenario for which the changes made in the Project Properties
        should be applied.

        ``var`` is used to identify the scenario variable and ``value`` the
        possible value for which we want to apply the changes.

        When no ``values`` are specified, all the possible values for the
        scenario variable are selected.
        """
        for m in self.scenario_selector_tree.get_model():
            if m[1] == var:
                # Unselect all the variable's values first
                pygps.tree.click_in_tree(
                    self.scenario_selector_tree,
                    m.path)
                if m[0]:
                    pygps.tree.click_in_tree(
                        self.scenario_selector_tree,
                        m.path)

                if values:
                    children = m.iterchildren()
                    for child in children:
                        if child[1] in values:
                            pygps.tree.click_in_tree(
                                self.scenario_selector_tree,
                                child.path)
                else:
                    pygps.tree.click_in_tree(
                        self.scenario_selector_tree,
                        m.path)
                    return

    def get_switch(self, label, gtk_type):
        """
        Return the switch widget identified by ``label`` and
        belonging to the given ``gtk_type``.

        Here is a simple example showing how to get the widget
        associated to a given switch:
            # open the editor first
            editor = GPS.Project_Properties()
            yield editor.open_and_yield()

            # select the page containing the switch
            editor.select_page("Build/Switches/Builder")

            # get the widget associated to the 'Compile only'
            # switch
            switch_widget = editor.get_switch("Compile only", Gtk.ToggleButton)
        """
        model, iter = self.treeview.get_selection().get_selected()
        page_widget = model[iter][Project_Properties_Editor.COLUMN_WIDGET]
        switches = get_widgets_by_type(gtk_type, page_widget)

        result = [switch for switch in switches
                  if BuildTargetsEditor.get_switch_label(
                      switch, gtk_type) == label and
                  switch.get_mapped()]

        if result:
            return result[0]
        else:
            return None

    def save(self):
        """
        Press the Save button, saving the modifications that have been made in
        the project file and closing the dialog.

        Use as::
            yield dialog.save()
        """

        save_button = get_widget_by_name("project properties edit source")
        save_button.clicked()
        yield timeout(300)


###############################
# Project Templates Assistant #
###############################


class ProjectTemplatesAssistant(Dialog):
    """
    Interface to the project templates assistant.
    """

    __assistant = None

    def open_and_yield(self):
        """
        Compatible with run_test_driver, to be used in a yield statement
            assistant = ProjectTemplatesAssistant()
            yield assistant.open_and_yield()
            ...
        :param action: the name of the action that will open window
        """

        yield self._open_and_yield('create project from template')
        self.__assistant = get_widget_by_name("Project Templates Assistant")

    def select_template(self, template):
        """
        Select the given template in the assistant's left tree view and
        click on the 'Next' button.

        :param template: The label of the template to select
        """

        tree = get_widgets_by_type(Gtk.TreeView, self.__assistant)[0]
        path = pygps.tree.find_in_tree(tree, 0, template)
        pygps.tree.click_in_tree(tree, path)

        get_button_from_label("Next", self.__assistant).clicked()

    def get_current_page_widget(self):
        """
        Return the currently displayed page in the assistant.
        """

        return self.__assistant.get_nth_page(
            self.__assistant.get_current_page())

    def cancel(self):
        """
        Cancel the assistant.
        """

        get_button_from_label("Cancel", self.__assistant).clicked()
        yield timeout(300)

    def apply(self):
        """
        Click on the 'Apply' button of the assistant to deploy the currently
        selected template.
        """

        get_button_from_label("Apply", self.__assistant).clicked()
        yield timeout(300)


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
        yield self._open_and_yield("open project")
        self.dialog = get_widget_by_name('Project Explorer Tree')

    def get_selected_name(self):
        """
        Return the displayed name of the Project view's selected node, or None
        if there is no selection.
        """

        _, select_iter = self.dialog.get_selection().get_selected()
        if not select_iter:
            return None

        selected_name = self.dialog.get_model().get_value(select_iter, 1)

        return selected_name


#############
# Bookmarks #
#############

class Bookmarks(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('open Bookmarks')
        self.treeview = get_widget_by_name('Bookmark TreeView')


##################
# AnalysisReport #
##################

class AnalysisReport(Dialog):

    FilterKind = gps_utils.enum(
        TOOL=0,
        SEVERITY=1,
        RULE=2)

    MessagesReportColumn = gps_utils.enum(
        ICON_NAME=0,
        ENTITY_ID=1,
        ENTITY_NAME=2,
        TOTAL=3)

    MetricsReportColumn = gps_utils.enum(
        NAME=0,
        VALUE=1)

    def open_and_yield(self, force=False):
        self.report_mdi = GPS.MDI.get("Analysis Report")

        if (self.report_mdi is None) or force:
            yield self._open_and_yield('gnathub display analysis')
            self.report_mdi = GPS.MDI.get("Analysis Report")

        self.report = self.report_mdi.pywidget()
        self.filters = GPS.MDI.get("Filters").pywidget()
        self.tools = get_widget_by_name(
            'gnathub tools editor', self.filters)
        self.severities = get_widget_by_name(
            'gnathub severities editor', self.filters)
        self.rules = get_widget_by_name(
            'gnathub rules editor', self.filters)
        self.messages_report = get_widget_by_name(
            'messages-report', self.report)
        self.metrics_report = get_widget_by_name(
            'metrics-report', self.report)

    def yield_close(self):
        self.report_mdi.close()
        yield wait_idle()

    def __get_filters_tree(self, filter_kind):
        if filter_kind == AnalysisReport.FilterKind.TOOL:
            tree = get_widgets_by_type(Gtk.TreeView, self.tools)[0]
        elif filter_kind == AnalysisReport.FilterKind.SEVERITY:
            tree = get_widgets_by_type(Gtk.TreeView, self.severities)[0]
        else:
            tree = get_widgets_by_type(Gtk.TreeView, self.rules)[0]

        return tree

    def yield_toggle_filter(self, name, filter_kind):
        """
        Toggle the filter identified by the given name and kind (eg:
        AnalysisReport.FilterKind.TOOL to toggle a tool filter).

        :param name: The name of the filter to toggle.
        :param kind: The filter's kind
        """

        tree = self.__get_filters_tree(filter_kind)
        model = tree.get_model()

        for row in model:
            if row[0] == name:
                pygps.tree.click_in_tree(tree, row.path, column=0)
                return

        yield wait_tasks()

    def get_filters(self, filter_kind):
        """
        Return the list of dictionaries representing the filters of the
        given kind.

        The returned dictionaries have the following form:

          {"name": string, "number": string, "state": bool}

        :param kind: The filter's kind
        """
        model = self.__get_filters_tree(filter_kind).get_model()
        filters = []

        for row in model:
            filters.append(
                {"name": row[0], "number": row[1], "state": row[2]})

        return filters

    def yield_select_in_messages(self, entity):
        """
        Select the given entity in the message's report

        :param entity: The entity's name
        """
        self.messages_report.get_selection().unselect_all()
        yield wait_idle()

        select_in_tree(
            self.messages_report,
            AnalysisReport.MessagesReportColumn.ENTITY_NAME,
            entity)
        yield wait_idle()

    def dump_filters(self, filter_kind):
        """
        Dump the filters editor corresponding to the given kind.

        e.g: >>> report.dump_filters(AnalysisReport.FilterKind.TOOL)
             >>> [['codepeer', '5', True], ['gnatcheck', '1', True]]
        """
        model = self.__get_filters_tree(filter_kind).get_model()

        return dump_tree_model(model, -1)

    def dump_messages_report(self, column):
        """
        Dump the messages report of the given column.

        :param column: an AnalysisReport.MessagesReportColumn
        """
        model = self.messages_report.get_model()

        return dump_tree_model(model, column)

    def dump_metrics_report(self, column):
        """
        Dump the metrics report of the given column.

        :param column: an AnalysisReport.MetricsReportColumn
        """
        model = self.metrics_report.get_model()
        return dump_tree_model(model, column)

    def get_messages_report_total(self):
        """
        Return the total number of messages displayed in the messages
        report's first row.

        :return: An integer
        """
        total_row = self.messages_report.get_model()[0]
        return total_row[AnalysisReport.MessagesReportColumn.TOTAL]


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
            self.modify_button = get_button_from_label('Add', self.editor)
            self.remove_button = get_button_from_label('Remove', self.editor)
            self.close_button = get_button_from_label('Close')
            self.key_theme_combo = get_widgets_by_type(
                Gtk.ComboBoxText, self.editor)[0]

    def yield_modify(self):
        yield idle_modal_dialog(self.modify_button.clicked)

    def yield_remove(self):
        self.remove_button.clicked()
        yield wait_idle()

    def yield_close(self):
        self.close_button.clicked()
        yield wait_idle()

    def get_key_theme(self):
        return self.key_theme_combo.get_active_text()

    def select_action(self, action):
        tree = get_widget_by_name('Key shortcuts tree', [self.editor])
        GPS.Preference("shortcuts-categories").set(False)
        # ??? Used to manipulate config menu, but this seems to fail now
        # toggle_local_config(self.editor, 'Show categories', False)

        for m in tree.get_model():
            if m[0].lower() == action.lower():
                tree.get_selection().select_path(m.path)
                return

        gps_assert(False, True, action + ' not found in key shortcuts editor')


#########
# Learn #
#########

class Learn(Dialog):
    """
    Interface to the Learn view
    """

    def open_and_yield(self):
        GPS.execute_action("open Learn")
        self.dialog = GPS.MDI.get('Learn').pywidget()
        self.paned_view = get_widgets_by_type(Gtk.Paned, self.dialog)[0]
        self.doc_label = get_widgets_by_type(
            Gtk.Label, self.paned_view.get_child2())[0]

    def yield_click_on_item(self, label_text, button=1,
                            events=pygps.single_click_events):
        """
        Clicks on the Learn view's item identified with the given
        ``label_text``.
        """
        children = get_widgets_by_type(Gtk.FlowBoxChild, self.dialog)

        for child in children:
            label = get_widgets_by_type(Gtk.Label, child)[0]
            if label.get_label() == label_text:
                if isinstance(child.get_parent(), Gtk.FlowBox):
                    click_in_widget(child.get_window(), -1, -1, events=events)
                    yield wait_idle()
                    return

    def get_current_doc(self):
        """
        Return the documentation currently displayed in the Learn view.
        """
        return self.doc_label.get_label()


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


###########
# Outline
###########

class Outline_View(Dialog):

    def open_and_yield(self):
        """
        Compatible with run_test_driver, to be used in a yield statement
            view = Outline_View()
            yield view.open_and_yield()
        """
        yield self._open_and_yield('open Outline')
        self.tree = get_widget_by_name("Outline View Tree")

    def model(self):
        """
        Return the tree model of what is currently displayed in the Outline
        """
        return dump_tree_model(self.tree.get_model(), 1)

    def set_options(self,
                    show_profiles=None,
                    show_param_names=None,
                    show_with=None,
                    group_by_category=None,
                    alphabetical=None):
        """
        Overrides some of the configuration parameters
        """
        if show_param_names is not None:
            GPS.Preference('outline-no-param-names').set(not show_param_names)
        if show_profiles is not None:
            GPS.Preference('outline-show-profile').set(show_profiles)
        if show_with is not None:
            GPS.Preference('outline-show-with').set(show_with)
        if group_by_category is not None:
            GPS.Preference('outline-group-by-category').set(group_by_category)
        if alphabetical is not None:
            GPS.Preference('outline-alphabetical').set(alphabetical)


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
        preferences_dialog = Preferences()
        yield preferences_dialog.open_and_yield()
        preferences_dialog.select_page("Build Targets")

        self.editor = get_widget_by_name("Build Targets Editor")
        self.notebook = pygps.get_widgets_by_type(Gtk.Notebook, self.editor)[0]
        self.close_button = get_button_from_label(
            "Close", preferences_dialog.dialog)
        self.apply_button = get_button_from_label(
            "Apply", preferences_dialog.dialog)
        self.tree = get_widgets_by_type(Gtk.TreeView,
                                        self.editor)[0]

    def yield_close(self):
        """
        Close the Build Targets editor.
        """
        self.close_button.clicked()
        yield wait_idle()

    def yield_apply(self):
        """
        Apply the changes made in the Build Targets editor.
        """
        self.apply_button.clicked()
        yield wait_idle()

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

    def get_current_page_widget(self):
        """
        Return the currently displayed page in the Build Targets Editor.
        """

        return self.notebook.get_nth_page(self.notebook.get_current_page())

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

        switches = get_widgets_by_type(
            gtk_type, self.get_current_page_widget())

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
        FILES_FROM_RUNTIME="Files From Runtime",
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

            # Disable confirmation dialog for 'Replace all' button
            GPS.Preference("Ask-Confirmation-For-Replace-All").set(False)

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
        self.view = GPS.MDI.get("Variables")
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
                name = str(m[iter][0])
                val = str(m[iter][1])
                var_type = str(m[iter][2])
                val = r.sub("0xfff", val)

                if name == 'None':
                    result.append(name)
                else:
                    result.append(name + ' = ' + var_type + ' ' + val)

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


###################
# Editor_Properties
###################

class Editor_Properties_Dialog(Dialog):

    def open_and_yield(self, file):
        yield self._open_and_yield("edit file properties")
        self.dialog = get_window_by_title("Properties for %s" % file)

    def get_combobox_by_label(self, text):
        label = [label for label in get_widgets_by_type(Gtk.Label, self.dialog)
                 if label.get_label() == text][0]

        return label.get_parent().get_children()[1]

    def get_language_entry(self):
        return self.get_combobox_by_label("Language: ")

    def get_character_set_entry(self):
        return self.get_combobox_by_label("Character set: ")


###############
# Refactoring #
###############

class Refactoring_Rename(Dialog):
    """The simple "rename" refactoring dialog.

       To use, do something like this:

          # Get a buffer
          b = GPS.EditorBuffer.get()

          # Position the cursor
          b.current_view().goto(b.at(7, 12))

          # Launch the dialog
          d = Refactoring_Rename()
          yield d.open_and_yield()

          # Rename something and exit
          d.set_new_name("XYZ")
          yield d.ok()
    """

    def __init__(self):
        pass

    def open_and_yield(self):
        yield self._open_and_yield("rename entity", timeout_ms=200)

    def set_new_name(self, text):
        pygps.get_widget_by_name("new_name").set_text(text)


###########
# Save As #
###########

class Save_As(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('save as')
        labels = get_widgets_by_type(Gtk.Label, self.dialogs[0])
        label = [x for x in labels if x.get_label() == '_Name:'][0]
        self.entry = label.get_parent().get_children()[0]

    def set_new_name(self, text):
        self.entry.set_text(text)


############
# GNATtest #
############

class GNATtest(Dialog):
    """
    Interface to the gnattest in generation mode.
    """

    def open_and_yield(self):
        yield self._open_and_yield('run gnattest on root')

    def ok(self):
        get_button_from_label("Execute", self.dialogs).clicked()
        yield wait_idle()

    def get_checkbox(self, name):
        return [w for w in get_widgets_by_type(Gtk.CheckButton, self.dialogs)
                if w.get_label().startswith(name)][0]


class GNATtest_Run(Dialog):
    """
    Interface to the gnattest in execution mode.
    """

    def open_and_yield(self):
        yield self._open_and_yield('Run a test drivers list Number 1')

    def ok(self):
        get_button_from_label("Execute", self.dialogs).clicked()
        yield wait_idle()

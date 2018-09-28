# This package contains the documentation for the GPS module
# and all its classes and methods.
# There is no code in this package, since the code is written
# in Ada. If you need to access the documentation from GPS
# itself (in the python console for instance), you will need
# to do the following:
#
#     >>> import GPS_doc
#     >>> print GPS_doc.class.method.__doc__
#
# Instead of
#
#     >>> print GPS.class.method.__doc__

"""
This package groups all the classes and functions exported by the GNAT
Programming System.

These functions are made available through various programming languages
(Python and the GPS shell at the moment). The documentation in this package is
mostly oriented towards Python, but can also be used as a reference for the
GPS shell.

Function description
--------------------

For all functions, the list of parameters is specified. The first parameter
is often called "self", and refers to the instance of the class to which
the method applies. In Python, the parameter is generally put before the
method's name, as in::

    self.method(arg1, arg2)

Although it could also be called as in::

    method(self, arg1, arg2)

For all other parameters, their name and type are specified. An additional
default value is given when the parameter is optional. If no default value is
specified, the parameter is mandatory and should always be specified. The name
of the parameter is relevant if you chose to use Python's named parameters
feature, as in::

    self.method(arg1="value1", arg2="value2")

which makes the call slightly more readable. The method above would be defined
with three parameters in this documentation (resp. "self", "arg1" and "arg2").

Some examples are also provides for several functions, to help clarify the use
of the function.

User data in instances
----------------------

A very useful feature of Python is that all class instances can be associated
with any number of user data fields. For example, if you create an instance
of the class :class:`GPS.EditorBuffer`, you can associate two fields "field1"
and "field2" to it (the names and number are purely for demonstration
purposes, and you can use your own), as in::

    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    ed.field1 = "value1"
    ed.field2 = 2

GPS takes great care for most classes to always return the same Python
instance for a given GUI object. For example, if you were to get another
instance of :class:`GPS.EditorBuffer` for the same file as above, you would
receive the same Python instance and thus the two fields are available to
you, as in::

    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    # ed.field1 is still "value1"

This is a very convenient way to store your own data associated with the
various objects exported by GPS. These data cease to exist when the GPS
object itself is destroyed (for instance when the editor is closed in the
example above).

Hooks
-----

In many cases, you need to connect to specific hooks exported by GPS to be
aware of events happening in GPS (such as the loading of a file or closing
a file).  These hooks and their use are described in the GPS manual (see
also the :class:`GPS.Hook` class).

Here is a small example, where the function :func:`on_gps_started` is called
when the GPS window is fully visible to the user::

    import GPS
    def on_gps_started(hook):
        pass

    GPS.Hook("gps_started").add(on_gps_started)

The list of parameters for the hooks is described for each hook below. The
first parameter is always the name of the hook, so that the same function can
be used for multiple hooks if necessary.

There are two categories of hooks: the standard hooks and the action
hooks. The former return nothing, the latter return a boolean indicating
whether your callback was able to perform the requested action. They are
used to override some of GPS's internal behavior.

"""

import exceptions
from constructs import INDENTATION_SIMPLE


class __enum_proxy(object):
    def __init__(self, name, **enums):
        for k, v in enums.iteritems():
            setattr(self, k, "%s.%s" % (name, k))


def enum(name, **enums):
    """Replaces an enumeration so that the values are not displayed as
       ints in the doc, but as a string representing the name.

       This function is used whenever a value from the enum is accessed
       (for instance for the default value of parameters).
       However, for the class definition themselves, they use the enum()
       defined in gps_utils, because of the order in which sphinx loads
       things.
    """
    return __enum_proxy("GPS.Browsers.%s" % name, **enums)


###########################################################
# GUI
###########################################################

class GUI(object):

    """
    This is an abstract class (ie no instances of it can be created from your
    code, which represents a graphical element of the GPS interface.

    .. seealso:: :func:`GPS.GUI.__init__`
    """

    def __init__(self):
        """
        Prevents the creation of instances of :class:`GPS.GUI`. Such
        instances are created automatically by GPS as a result of calling
        other functions.

        .. seealso:: :func:`GPS.Toolbar.append`
        .. seealso:: :func:`GPS.Toolbar.entry`
        .. seealso:: :func:`GPS.Menu.get`
        """
        pass  # implemented in Ada

    def destroy(self):
        """
        Destroy the graphical element. It disappears from the interface,
        and cannot necessarily be recreated later on.
        """
        pass  # implemented in Ada

    def hide(self):
        """
        Temporarily hide the graphical element. It can be shown again through a
        call to :func:`GPS.GUI.show`.

        .. seealso:: :func:`GPS.GUI.show`
        """
        pass  # implemented in Ada

    def is_sensitive(self):
        """
        Return False if the widget is currently greyed out and not clickable
        by users.

        :return: A boolean

        .. seealso:: :func:`GPS.GUI.set_sensitive`
        """
        pass  # implemented in Ada

    def pywidget(self):
        """
        This function is only available if GPS was compiled with support for
        pygobject and the latter was found at run time. It returns a widget
        that can be manipulated through the usual PyGtk functions. PyGObject
        is a binding to the gtk+ toolkit, and allows you to create your own
        windows easily, or manipulate the entire GPS GUI from Python.

        :return: An instance of PyWidget

        .. seealso:: :func:`GPS.MDI.add`

        .. code-block:: python

           # The following example makes the project view inactive. One could
           # easily change the contents of the project view as well.
           widget = GPS.MDI.get("Project View")
           widget.pywidget().set_sensitive False)

        """
        pass  # implemented in Ada

    def set_sensitive(self, sensitive=True):
        """
        Indicate whether the associated graphical element should respond to
        user interaction or not. If the element is not sensitive, the user
        will not be able to click on it.

        :param boolean sensitive: A boolean

        .. seealso:: :func:`GPS.GUI.is_sensitive()`
        """
        pass  # implemented in Ada

    def show(self):
        """
        Show again the graphical element that was hidden by :func:`hide`.

        .. seealso:: :func:`GPS.GUI.hide`
        """
        pass  # implemented in Ada


###########################################################
# Filter
###########################################################

class Filter(object):
    """
    This class gives access to various aspects of the filters that
    are used by GPS to compute whether an action (and thus a menu,
    contextual menu or toolbar button) can be activated by the user
    at the current time.
    """

    @staticmethod
    def list():
        """
        Return the list of all registered named filters.
        Instead of duplicating their implementation, it is better to
        reuse existing filters when possible, since their result is
        cached by GPS. Since lots of filters might be evaluated when
        computing the contextual menu, it will be faster when using
        named filters in such a case.

        The returned named can be used in :func:`GPS.Action.create`
        for instance.

        :return: a list of strings (the name of the filters)
        """


###########################################################
# Action
###########################################################

class Action(object):
    """
    This class gives access to the interactive commands in GPS. These are the
    commands to which the user can bind a key shortcut or for which we can
    create a menu. Another way to manipulate those commands is through the XML
    tag <action>, but it might be more convenient to use Python since you
    do not have to qualify the function name.
    """

    def __init__(self, name):
        """
        Creates a new instance of :class:`Action`. This is bound with either
        an existing action or with an action that will be created through
        :func:`GPS.Action.create`. The name of the action can either be a
        simple name, or a path name to reference a menu, such as /Edit/Copy.

        :param string name: A string
        """
        pass  # implemented in Ada

    def exists(self):
        """
        Returns a Boolean indicating if an action has already been created
        for this name.
        """
        pass  # implemented in Ada

    def contextual(self, path, ref='', add_before=True, group=0,
                   static_path=''):
        """
        Create a new contextual menu associated with the action.

        .. seealso: :func:`GPS.Action.destroy_ui` to remove this menu

        :param path: A string or a function(GPS.Context):string, which
           describes the path for the contextual menu.
        :param string ref: A string
        :param int group: the group of items in the contextual menu. These
           groups are ordered numerically, so that all items in group 0
           appear before items in group 1, and so on.
        :param boolean add_before: A boolean
        :param static_path A string which describes the path for the contextual
           menu when path is a function.
        """
        pass  # implemented in Ada

    def create(self, on_activate, filter='', category='General',
               description='', icon='', for_learning=False):
        """
        Export the function :func:`on_activate` and make it interactive so
        that users can bind keys and menus to it. The function should not
        require any argument, since it will be called with none.

        The package :file:`gps_utils.py` provides a somewhat more convenient
        Python interface to make functions interactive (see
        gps_utils.interactive).

        :param on_activate: A subprogram
        :type on_activate: () -> None

        :type filter: string|(Context) -> boolean
        :param filter: A string or subprogram
           Either the name of a predefined filter (a string), or a
           subprogram that receives the context as a parameter, and should
           return True if the command can be executed within that
           context. This is used to disable menu items when they are not
           available.
           See :func:`GPS.Filter.list` to retrieve the list of all defined
           named filters.

        :param str category: Category of the command in the Key Shortcuts
           editor.

        :param str description: Description of the command that appears in
           the dialog or in tooltips. If you are using Python, a convenient
           value is on_activate.__doc__, which avoids duplicating the comment.

        :param str icon: Name of the icon to use for this action (in toolbars,
           dialogs, ...). This is the name of an icon file in the GPS icons
           directory.

        :param bool for_learning: Set it to True if you want to display this
        action in the Learn view.

        """

    def destroy_ui(self):
        """
        Remove all elements associated with this action (menus, toolbar
        buttons, contextual menus,...). The action itself is not destroyed
        """

    def disable(self, disabled=True):
        """
        Prevent the execution of the action, whether through menus,
        contextual menus, key shortcuts,...

        :param bool disabled: whether to disable or enable
        """
        pass  # implemented in Ada

    def can_execute(self):
        """
        Return True if the action can be executed in the current context.

        :rtype: boolean
        """

    def execute_if_possible(self):
        """
        Execute the action if its filter matches the current context. If it
        could be executed, True is returned, otherwise False is returned.

        :rtype: boolean
        """

    def key(self, key, exclusive=True):
        """
        Associate a default key binding with the action. This is ignored if
        the user defined his own key binding. You can experiment with
        possible values for keys by using the /Edit/Key Shortcuts dialog.

        :param string key: A string
        :param bool exclusive: if True, the shortcut will no longer be
           associated with any action is was previously bound to. If False,
           the shortcut will be associated with multiple action. The first
           one for which the filter matches is executed when the user
           presses those keys.
        """
        pass  # implemented in Ada

    def get_keys(self):
        """
        Return the key shortcuts associated to the given action.
        The returned string should only be used for displaying purposes.

        :rtype: a string
        """
        pass  # implemented in Ada

    def menu(self, path, ref='', add_before=True):
        """
        Create a new menu associated with the command. This function is
        somewhat a duplicate of :func:`GPS.Menu.create`, but with one major
        difference: the callback for the action is a python function that takes
        no argument, whereas the callback for :func:`GPS.Menu` receives one
        argument.

        .. seealso: :func:`GPS.Action.destroy_ui` to remove this menu

        :param string path: A string
            If path ends with a '-', a separator line is created, instead of a
            menu item with text.
        :param string ref: A string
        :param boolean add_before: A boolean
        :return: The instance of GPS.Menu that was created
        :rtype: Menu
        """
        pass  # implemented in Ada

    def button(self, toolbar='main', section='', group='', label='', icon='',
               hide=False):
        """
        Add a new button in some toolbars.
        When this button is clicked, it executes the action from self.

        .. seealso: :func:`GPS.Action.destroy_ui` to remove this button

        :param string toolbar: identifies which toolbar the action should be
           added to. The default is to add to the main toolbar for the main
           GPS window and all floating windows. Other possible names are the
           names of the various views, as listed in the /Tools/Views menu.

        :param string section: identifies which part of the toolbar the button
           should be added to. By default, the button is added at the end of
           the toolbar. However, some toolbars define various sections (see
           the menus.xml file for valid section names).

        :param string group: when a group is specified, the new button
           contains a popup menu. A long click on the menu displays a popup
           with all actions in that group. A short click executes the last
           action from this group.

        :param string label: use this as a label for the button, when the user
           choses to display labels in toolbars. The default is to use the
           action's name.

        :param string icon: override the default icon registered for the
           action.

        :param bool hide: if the action is disabled or not applicable to the
           current context, the button will be hidden instead of simply be
           disabled.

        .. code-block:: python

           # The following adds a 'Copy to clipboard' button in the Messages
           # window's local toolbar:
           GPS.Action("Copy to Clipboard").button(
               toolbar='Messages', label='Copy')
        """

###########################################################
# Analysis
###########################################################


class Analysis(object):
    """
    Used to interface external analysis tools and display their messages
    in a global analysis report.

    See :class:`GPS.AnalysisTool` for more  information.
    """

    @staticmethod
    def display_report(tool=None):
        """
        Display the GPS analysis report.
        Make sure to add your messages before calling this function in order
        to display them.

        When the optional :class:`GPS.AnalysisTool` parameter is specified,
        only this tool will be selected in the Filters view.

        :param :class:`GPS.AnalysisTool` tool: The tool that will be
        exclusively selected.
        """
        pass  # implemented in Ada


###########################################################
# AnalysisTool
###########################################################


class AnalysisTool(object):
    """
    This class is used to interface external analysis tools with GPS.
    A :class:`GPS.AnalysisTool` should define some rules that will be later
    associated to the messages retrieved from the analysis tool's output.
    The messages added via this class will then be displated in the GPS
    Analysis Report.

    See :func:`GPS.Analysis.display_report` to display the messages added
    for this analysis tool.
    """

    def __init__(self, name):
        """
        Creates a :class:`GPS.AnalysisTool` object, associating it to a name.

        :param string name: The name of the analysis tool.
        """
        pass  # implemented in Ada

    def add_rule(self, name, id):
        """
        Adds a rule for the :class:`GPS.AnalysisTool`.
        A rule is defined by a name and a unique ID. Rules will then be used
        to filter the analysys tool's messages in the GPS Analysis Report.

        :param string name: The rule's name.
        :param string id: The rule's id.
        """
        pass  # implemented in Ada

    def add_message(self, msg, rule_id):
        """
        Adds the given message to the list of messages that will be displayed
        in the GPS Analysis Report. The message will be associated to this tool
        and to the rule identified by ``rule_id``.

        :param :class:`GPS.Message` msg: The message.
        :param string rule_id: The id of the rule associated to the message.
        """
        pass  # implemented in Ada


###########################################################
# Context
###########################################################

class Context(object):

    """
    Represents a context in GPS. Depending on the currently selected window, an
    instance of one of the derived classes will be used.
    """

    module_name = None
    """
    The name (a string) of the GPS module which created the context.
    """

    @staticmethod
    def contextual_menu():
        """
        Returns a list containing the contextual menu labels of the currently
        focused window. The output has the form "depth - label".
        """
        pass  # implemented in Ada

    def file(self):
        """
        Return the name of the file in the context.
        This method used to be set only for a GPS.Context.

        :rtype: :class:`GPS.File`

        .. seealso:: :class:`GPS.Context.files`

        """

    def set_file(self, file):
        """
        Set the file stored in the context.
        :param GPS.File file:
        """

    def directory(self):
        """
        Return the current directory of the context.

        :rtype: string
        """
        pass  # implemented in Ada

    def files(self):
        """
        Return the list of selected files in the context

        :rtype: list[:class:`GPS.File`]
        """
        pass  # implemented in Ada

    def location(self):
        """
        Return the file location stored in the context.

        :rtype: :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def project(self):
        """
        Return the project in the context or the root project if none was
        specified in the context. Return an error if no project can be found
        from the context.

        :rtype: :class:`GPS.Project`
        """
        pass  # implemented in Ada

    def message(self):
        """
        Returns the current message that was clicked on

        :returntype: :class:`GPS.Message`
        """

    def end_line(self):
        """
        Return the last selected line in the context.

        :rtype: integer
        """
        pass  # implemented in Ada

    def start_line(self):
        """
        Return the first selected line in the context.

        :rtype: integer
        """
        pass  # implemented in Ada

    def entity(self, approximate_search_fallback=True):
        """
        Returns the entity stored in the context.
        This might be expensive to compute, so it is often recommend to check
        whether `GPS.Context.entity_name` returns None, first.

        :param approximate_search_fallback: If True, when the line and column
           are not exact, this parameter will trigger approximate search in the
           database (eg. see if there are similar entities in the surrounding
           lines)
        :return: An instance of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def entity_name(self):
        """
        Return the name of the entity that the context points to.
        This is None when the context does not contain entity information.

        :rtype: str
        """


###########################################################
# Bookmark
###########################################################

class Bookmark(object):
    """
    This class provides access to GPS's bookmarks. These are special types
    of markers that are saved across sessions, and can be used to save a
    context within GPS. They are generally associated with a specific location
    in an editor, but can also be used to locate special boxes in a graphical
    browser, for example.
    """

    note = ""
    """
    The bookmark's note value - A string.
    """

    def __init__(self):
        """
        This function prevents the creation of a bookmark instance directly.
        You must use :func:`GPS.Bookmark.get` instead, which always returns
        the same instance for a given bookmark, thus allowing you to save your
        own custom data with the bookmark

        .. seealso:: :func:`GPS.Bookmark.get`
        """
        pass  # implemented in Ada

    @staticmethod
    def create(name):
        """
        This function creates a new bookmark at the current location in
        GPS. If the current window is an editor, it creates a bookmark that
        will save the exact line and column, so the user can go back to them
        easily. Name is the string that appears in the bookmarks window, and
        that can be used later to query the same instance using
        :func:`GPS.Bookmark.get`. This function emits the hook
        bookmark_added.

        :param string name: The name of the bookmark
        :rtype: :class:`GPS.Bookmark`

        .. seealso:: :func:`GPS.Bookmark.get`

        .. code-block:: python

           GPS.MDI.get("file.adb").raise_window()
           bm = GPS.Bookmark.create("name")
        """
        pass  # implemented in Ada

    def delete(self):
        """
        Delete an existing bookmark. This emits the hook
        :file:`bookmark_removed`.

        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Retrieves a bookmark by its name. If no such bookmark exists, an
        exception is raised. The same instance of :class:GPS.Bookmark is
        always returned for a given bookmark, so you can store your own user
        data within the instance. Note however that this custom data will not
        be automatically preserved across GPS sessions, so you may want to
        save all your data when GPS exits

        :param string name: The name of the bookmark
        :rtype: :class:`GPS.Bookmark`

        .. seealso:: :func:`GPS.Bookmark.create`

        .. code-block:: python

           GPS.Bookmark.get("name").my_own_field = "GPS"
           print GPS.Bookmark.get("name").my_own_field   # prints "GPS"

        """
        pass  # implemented in Ada

    def goto(self):
        """
        Changes the current context in GPS so it matches the one saved in the
        bookmark. In particular, if the bookmark is inside an editor, this
        editor is raised, and the cursor moved to the correct line and
        column. You cannot query directly the line and column from the
        bookmark, since these might not exist, for instance when the editor
        points inside a browser.
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Return the list of all existing bookmarks.

        :rtype: [:class`GPS.Bookmark`]

        .. code-block:: python

           # The following command returns a list with the name of all
           # existing purposes
           names = [bm.name() for bm in GPS.Bookmark.list()]

        """
        pass  # implemented in Ada

    def name(self):
        """
        Return the current name of the bookmark. It might not be the same one
        that was used to create or get the bookmark, since the user might have
        used the bookmarks view to rename it.

        :rtype: string
        """
        pass  # implemented in Ada

    def rename(self, name):
        """
        Rename an existing bookmark. This updates the bookmarks view
        automatically, and emits the hooks :file:`bookmark_removed` and
        :file:`bookmark_added`.

        :param string name: The new name of the bookmark
        """
        pass  # implemented in Ada


###########################################################
# BuildTarget
###########################################################

class BuildTarget(object):

    """
    This class provides an interface to the GPS build targets. Build targets
    can be configured through XML or through the Target Configuration dialog.
    """

    def __init__(self, name):
        """
        Initializes a new instance of the class :class:`BuildTarget`. ``name``
        must correspond to an existing target.

        :param string name: Name of the target associated with this instance

        .. code-block:: python

           compile_file_target=GPS.BuildTarget("Compile File")
           compile_file_target.execute()
        """
        pass  # implemented in Ada

    def clone(self, new_name, new_category):
        """
        Clone the target to a new target. All the properties of the new target
        are copied from the target.  Any graphical element corresponding to
        this new target is created.

        :param string new_name: The name of the new target
        :param string new_category: The category in which to place the new
          target
        """
        pass  # implemented in Ada

    def execute(self, main_name='', file='', force=False, extra_args='',
                build_mode='', synchronous=True, directory='', quiet=False,
                on_exit=None):
        """
        Launch the build target.

        :param string main_name: The base name of the main source to build, if
            this target acts on a main file.

        :param file: The file to build if this targets acts on a file.
        :type file: :class:`GPS.File`

        :param bool force:
           If True, this means that the target should be launched directly,
           even if its parameters indicate that it should be launched through
           an intermediary dialog.

        :param string|list[string] extra_args:
           any extra parameters to pass to the command line. When a single
           string is passed, it is split into multiple arguments.

        :param string build_mode:  Indicates build mode to be used for build.

        :param bool synchronous:
           if False, build target is launched asynchronously.
           ``compilation_finished hook`` will be called when build target
           execution is completed.

        :param string directory: A String

        :param bool quiet: A Boolean

        :param on_exit: A subprogram which will be called when the build target
           finishes executing. This subprogram takes as parameter an integer,
           representing the exit code of the command. For instance:

           GPS.BuildTarget("Custom...").execute(
               synchronous=True,
               on_exit=lambda status: GPS.MDI.dialog("status is %s" % status))
        """
        pass  # implemented in Ada

    def get_command_line(self):
        """
        Returns a string list containing the current arguments of this
        BuildTarget.

        Note that these arguments are not expanded.
        """
        pass  # implemented in Ada

    def remove(self):
        """
        Remove target from the list of known targets.
        Any graphical element corresponding to this target is also removed.
        """
        pass  # implemented in Ada

    def hide(self):
        """
        Hide target from menus and toolbar.
        """
        pass  # implemented in Ada

    def show(self):
        """
        Show target in menus and toolbar where it was before hiding.
        """
        pass  # implemented in Ada


###########################################################
# Clipboard
###########################################################

class Clipboard(object):

    """
    This class provides an interface to the GPS clipboard. This clipboard
    contains the previous selections that were copied or cut from a text
    editor. Several older selections are also saved so that they can be pasted
    later on.
    """

    @staticmethod
    def contents():
        """
        This function returns the contents of the clipboard. Each item in the
        list corresponds to a past selection, the one at position 0 being the
        most recent. If you want to paste text in a buffer, you should paste
        the text at position :func:`GPS.Clipboard.current` rather than the
        first in the list.

        :rtype: [string]
        """
        pass  # implemented in Ada

    @staticmethod
    def copy(text, append=False):
        """
        Copies a given static text into the clipboard. It is better in general
        to use :func:`GPS.EditorBuffer.copy`, but it might happen that you need
        to append text that do not exist in the buffer.

        :param string text: The content you want to put int the clipboard.
        :param boolean append: Wether you want to append to the current
           clipboard content or not.

        .. seealso:: :func:`GPS.EditorBuffer.copy`
        """
        pass  # implemented in Ada

    @staticmethod
    def current():
        """
        This function returns the index, in :func:`GPS.Clipboard.contents`, of
        the text that was last pasted by the user. If you were to select the
        menu /Edit/Paste, that would be the text pasted by GPS. If you select
        /Edit/Paste Previous, current will be incremented by 1, and the next
        selection in the clipboard is pasted.

        :rtype: integer
        """
        pass  # implemented in Ada

    @staticmethod
    def merge(index1, index2):
        """
        This function merges two levels of the clipboard, so that the one at
        index ``index1`` now contains the concatenation of both. The one at
        ``index2`` is removed.

        :param integer index1: A null or positive integer
        :param integer index2: A null or positive integer
        """
        pass  # implemented in Ada


###########################################################
# CodeAnalysis
###########################################################

class CodeAnalysis(object):

    """
    This class is a toolset that allows handling :class:`CodeAnalysis`
    instances.
    """

    def __del__(self):
        """
        Called when a :class:`CodeAnalysis` instance is deleted by Python.
        """
        pass  # implemented in Ada

    def __init__(self):
        """
        Raises an exception to prevent users from creating new instances.
        """
        pass  # implemented in Ada

    def add_all_gcov_project_info(self):
        """
        Adds coverage information for every source files referenced in the
        current project loaded in GPS and every imported projects.

        .. seealso::

           :func:`GPS.CodeAnalysis.add_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_file_info`
        """
        pass  # implemented in Ada

    def add_gcov_file_info(self, src, cov):
        """
        Adds coverage information provided by parsing a :file:`.gcov`
        file. The data is read from the cov parameter that should have been
        created from the specified src file.

        :param src: The corresponding source file
        :type src: :class:`GPS.File`
        :param cov: The corresponding coverage file
        :type cov: :class:`GPS.File`

        .. seealso::

           :func:`GPS.CodeAnalysis.add_all_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_project_info`

        .. code-block:: python

           a = GPS.CodeAnalysis.get("Coverage Report")
           a.add_gcov_file_info(src=GPS.File("source_file.adb"),
                                cov=GPS.File("source_file.adb.gcov"))
        """
        pass  # implemented in Ada

    def add_gcov_project_info(self, prj):
        """
        Adds coverage information of every source files referenced in
        the GNAT project file (:file:`.gpr`) for ``prj``.

        :param prj: The corresponding project file
        :type prj: A :class:`GPS.File` instance

        .. seealso::

           :func:`GPS.CodeAnalysis.add_all_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_file_info`
        """
        pass  # implemented in Ada

    def clear(self):
        """
        Removes all code analysis information from memory.
        """
        pass  # implemented in Ada

    def dump_to_file(self, xml):
        """
        Create an XML-formated file containing a representation of the given
        code analysis.

        :param xml: The output xml file
        :type xml: :class:`GPS.File`

        .. seealso:: :func:`GPS.CodeAnalysis.load_from_file`

        .. code-block:: python

           a = GPS.CodeAnalysis.get ("Coverage")
           a.add_all_gcov_project_info ()
           a.dump_to_file (xml=GPS.File ("new_file.xml"))
        """
        pass  # implemented in Ada

    @staticmethod
    def expand_line_cov_info(file, line):
        """
        Expand the coverage information at line of file

        :param file: The file
        :type file: :class:`GPS.File`

        :param line: The line number
        :type line: A positive integer
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Creates an empty code analysis data structure. Data can be put in this
        structure by using one of the primitive operations.

        :param string name: The name of the code analysis data structure to
           get or create
        :return: An instance of :class:`GPS.CodeAnalysis` associated to a code
           analysis data structure in GPS.
        :rtype: :class:`GPS.CodeAnalysis`

        .. code-block:: python

           a = GPS.CodeAnalysis.get("Coverage")
           a.add_all_gcov_project_info()
           a.show_coverage_information()
        """
        pass  # implemented in Ada

    def hide_coverage_information(self):
        """
        Removes from the :guilabel:`Locations` view any listed coverage
        locations, and remove from the source editors their annotation column
        if any.

        .. seealso:: :func:`GPS.CodeAnalysis.show_coverage_information`
        """
        pass  # implemented in Ada

    def load_from_file(self, xml):
        """
        Replace the current coverage information in memory with the given
        XML-formated file one.

        :param xml: The source xml file
        :type xml: :class:`GPS.File`

        .. seealso:: :func:`GPS.CodeAnalysis.dump_to_file`

        .. code-block:: python

           a = GPS.CodeAnalysis.get ("Coverage")
           a.add_all_gcov_project_info ()
           a.dump_to_file (xml=GPS.File ("new_file.xml"))
           a.clear ()
           a.load_from_file (xml=GPS.File ("new_file.xml"))
        """
        pass  # implemented in Ada

    def show_analysis_report(self):
        """
        Displays the data stored in the :class:`CodeAnalysis` instance into a
        new MDI window. This window contains a tree view that can be
        interactively manipulated to analyze the results of the code analysis.
        """
        pass  # implemented in Ada

    def show_coverage_information(self):
        """
        Lists in the :guilabel:`Locations` view the lines that are not
        covered in the files loaded in the :class:`CodeAnalysis`
        instance. The lines are also highlighted in the corresponding source
        file editors, and an annotation column is added to the source
        editors.

        .. seealso:: :func:`GPS.CodeAnalysis.hide_coverage_information`
        """
        pass  # implemented in Ada


###########################################################
# Codefix
###########################################################

class Codefix(object):

    """
    This class gives access to GPS's features for automatically fixing
    compilation errors.

    .. seealso::

       :func:`GPS.CodefixError`

       :func:`GPS.Codefix.__init__()`
    """

    def __init__(self, category):
        """
        Returns the instance of codefix associated with the given category.

        :param string category: The corresponding category
        """
        pass  # implemented in Ada

    def error_at(self, file, line, column, message=''):
        """
        Returns a specific error at a given location. If message is null, then
        the first matching error will be taken. None is returned if no such
        fixable error exists.

        :param file: The file where the error is
        :type file: :class:`GPS.File`
        :param integer line: The line where the error is
        :param integer column: The column where the error is
        :param string message: The message of the error
        :rtype: :class:`GPS.CodefixError`
        """
        pass  # implemented in Ada

    def errors(self):
        """
        Lists the fixable errors in the session.

        :rtype: list[:class:`GPS.CodefixError`]
        """
        pass  # implemented in Ada

    @staticmethod
    def parse(category, output, regexp='', file_index=-1, line_index=-1,
              column_index=-1, style_index=-1, warning_index=-1):
        """
        Parses the output of a tool and suggests auto-fix possibilities
        whenever possible. This adds small icons in the location window, so
        that the user can click on it to fix compilation errors. You should
        call :func:`Locations.parse` with the same output prior to calling
        this command.

        The regular expression specifies how locations are recognized. By
        default, it matches `file:line:column`. The various indexes indicate
        the index of the opening parenthesis that contains the relevant
        information in the regular expression. Set it to 0 if that information
        is not available.

        Access the various suggested fixes through the methods of the
        :class:`Codefix` class.

        :param string category: A string
        :param string output: A string
        :param string regexp: A string
        :param integer file_index: An integer
        :param integer line_index: An integer
        :param integer column_index: An integer
        :param integer style_index: An integer
        :param integer warning_index: An integer

        .. seealso:: :func:`GPS.Editor.register_highlighting`
        """
        pass  # implemented in Ada

    @staticmethod
    def sessions():
        """
        Lists all the existing :class:`Codefix` sessions. The returned values
        can all be used to create a new instance of :class:`Codefix` through
        its constructor.

        :rtype: [string]

        .. code-block:: python

           # After a compilation failure:
           >>> GPS.Codefix.sessions()
           => ['Builder results']
        """
        pass  # implemented in Ada


###########################################################
# CodefixError
###########################################################

class CodefixError(object):

    """
    This class represents a fixable error in the compilation output.

    .. seealso::

       :func:`GPS.Codefix`

       :func:`GPS.CodefixError.__init__()`
    """

    def __init__(self, codefix, file, message=''):
        """
        Describes a new fixable error. If the message is not specified, the
        first error at that location is returned.

        :param codefix: The owning codefix instance
        :type codefix: :class:`GPS.Codefix`

        :param file: The location of the error
        :type file: :class:`GPS.FileLocation`

        :param string message: The message of the error
        """
        pass  # implemented in Ada

    def fix(self, choice=0):
        """
        Fixes the error, using one of the possible fixes. The index given in
        parameter is the index in the list returned by
        :func:`possible_fixes`. By default, the first choice is
        taken. Choices start at index 0.

        :param integer choice: Index of the fix to apply, see output of
           :func:`GPS.CodefixError.possible_fixes`

        .. code-block:: python

           for err in GPS.Codefix ("Builder results").errors():
               print err.fix()

           # will automatically fix all fixable errors in the last compilation
           # output
        """
        pass  # implemented in Ada

    def location(self):
        """
        Returns the location of the error.

        :rtype: :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def message(self):
        """
        Returns the error message, as issue by the tool.

        :rtype: string
        """
        pass  # implemented in Ada

    def possible_fixes(self):
        """
        Lists the possible fixes for the specific error.

        :rtype: [string]

        .. code-block:: python

           for err in GPS.Codefix ("Builder results").errors():
               print err.possible_fixes()
        """
        pass  # implemented in Ada


###########################################################
# Command
###########################################################

class Command(object):

    """
    Interface to GPS command. This class is abstract, and can be subclassed.
    """

    def __del__(self):
        """
        Destructor of a GPS command. This should not be called manually by the
        user.
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Returns the list of commands of the name given in the parameter,
        scheduled or running in the tasks view

        :param string name: A string
        :rtype: list[:class:`GPS.Command`]
        """
        pass  # implemented in Ada

    def get_result(self):
        """
        Returns the result of the command, if any. Must be overriden by
        children.
        """
        pass  # implemented in Ada

    def interrupt(self):
        """
        Interrupts the current command.
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of commands scheduled or running in the tasks view.

        :rtype: [:class:`GPS.Command`]
        """
        pass  # implemented in Ada

    def name(self):
        """Return The name of the command"""
        pass  # implemented in Ada

    def progress(self):
        """
        Returns a list representing the current progress of the command. If
        current = total, the command has completed.

        :return: A list [current, total]
        :rtype: [int]
        """
        pass  # implemented in Ada


###########################################################
# CommandWindow
###########################################################

class CommandWindow(GUI):

    """
    This class gives access to a command-line window that pops up on the
    screen. This window is short-lived (in fact there can be only one such
    window at any given time) and any key press is redirected to that
    window. It can be used to interactively query a parameter
    for an action, for example.

    Among other things, it is used in the implementation of the interactive
    search facility, where each key pressed should be added to the search
    pattern instead of to the editor.

    .. code-block:: python

       class Isearch(CommandWindow):
           def __init__(self):
              CommandWindow.__init__(
                self, prompt="Pattern",
                on_key=self.on_key,
                on_changed=self.on_changed)

           def on_key(self, input, key, cursor_pos):
              if key == "control-w":
                 .... # Copy current word from editor into the window
                 self.write(input[:cursor_pos + 1] +
                            "FOO" + input[cursor_pos + 1:])
                 return True  ## No further processing needed
              return False

           def on_changed(self, input, cursor_pos):
              ## Search for next occurrence of input in buffer
              ....
    """

    def __init__(self, prompt='', global_window=False, on_changed=None,
                 on_activate=None, on_cancel=None, on_key=None,
                 close_on_activate=True):
        """
        Initializes an instance of a command window. An exception is raised
        if such a window is already active in GPS. Otherwise, the new window
        is popped up on the screen. Its location depends on the
        ``global_window`` parameter.

        :param string prompt: the short string displayed just before the
          command line itself. Its goal is to indicate to the user what he
          is entering.

        :param bool global_window: If true, the command window is displayed
          at the bottom of the GPS window and occupies its whole width. If
          false, it is displayed at the bottom of the currently selected
          window.

        :param on_changed: A subprogram, is called when the user has entered
          new characters in the command line. This function is given two
          parameters: the current input string, and the last cursor position
          in this string. See the example above on how to get the part of the
          input before and after the cursor.
        :type on_changed: (string, int) -> None

        :param on_activate: A subprogram, is called when the user pressed
          enter.
          The command window has already been closed at that point if
          close_on_activate is True and the focus given back to the initial
          MDI window that had it. This callback is passed a single parameter,
          the final input string.
        :type on_activate: (string) -> None

        :param on_cancel: A subprogram, is called when the user pressed a key
          that closed the dialog, for example :kbd:`Esc`. It is passed a single
          parameter, the final input string. This callback is also called when
          you explicitly destroy the window yourself by calling
          :func:`self.destroy`.
        :type on_cancel: (string) -> None

        :param on_key: Is called when the user has pressed a new key on his
          keyboard but before the corresponding character has been added to
          the command line. This can be used to filter out some characters or
          provide special behavior for some key combination (see the example
          above). It is passed three parameters, the current input string, the
          key that was pressed, and the current cursor position.
        :type on_key: (string, int) -> None

        :param bool close_on_activate: A boolean, determines wether the
          command window has to be closed on pressing enter.
        """
        pass  # implemented in Ada

    def read(self):
        """
        Returns the current contents of the command window.

        :rtype: string
        """
        pass  # implemented in Ada

    def set_background(self, color=''):
        """
        Changes the background color of the command window.  This can be used
        to make the command window more obvious or to highlight errors by
        changing the color. If the color parameter is not specified, the
        color reverts to its default.

        :param string color: The new background color
        """
        pass  # implemented in Ada

    def set_prompt(self, prompt):
        """
        Changes the prompt displayed before the text field.

        :param string prompt: The new prompt to display
        """
        pass  # implemented in Ada

    def write(self, text, cursor=-1):
        """
        This function replaces the current content of the command line. As a
        result, you should make sure to preserve the character you want, as
        in the :func:`on_key` callback in the example above. Calling this
        function also results in several calls to the :func:`on_changed`
        callback, one of them with an empty string (since gtk first deletes
        the contents and then writes the new contents).

        The cursor parameter can be used to specify where the cursor should
        be left after the insertion. -1 indicates the end of the string.

        :param string text: A string
        :param integer cursor: An integer
        """
        pass  # implemented in Ada


###########################################################
# Console
###########################################################

class Console(GUI):

    """
    This class is used to create and interact with the interactive consoles in
    GPS. It can be used to redirect the output of scripts to various consoles
    in GPS, or to get input from the user has needed.

    .. seealso::

       :class:`GPS.Process`

       :func:`GPS.Console.__init__`

    .. code-block:: python

        # The following example shows how to redirect the output of a script to
        # a new console in GPS:

        console = GPS.Console("My_Script")
        console.write("Hello world")  # Explicit redirection

        # The usual Python's standard output can also be redirected to this
        # console:

        sys.stdout = GPS.Console("My_Script")
        print "Hello world, too"  # Implicit redirection
        sys.stdout = GPS.Console("Python")  # Back to python's console
        sys.stdout = GPS.Console() # Or back to GPS's console

    .. code-block:: python

        # The following example shows an integration between the GPS.Console
        # and GPS.Process classes, so that a window containing a shell can be
        # added to GPS.

        # Note that this class is in fact available directly through "from
        # gps_utils.console_process import Console_Process" if you need it in
        # your own scripts.

        import GPS
        class Console_Process(GPS.Console, GPS.Process):
            def on_output(self, matched, unmatched):
              self.write(unmatched + matched)

            def on_exit(self, status, unmatched_output):
              try:
                  self.destroy()
              except:
                  pass  # Might already have been destroyed

            def on_input(self, input):
              self.send(input)

            def on_destroy(self):
              self.kill()  # Will call on_exit

            def __init__(self, command):
              GPS.Console.__init__(
                 command[0],
                 on_input=Console_Process.on_input,
                 on_destroy=Console_Process.on_destroy,
                 force=True)
              GPS.Process.__init__(
                 self, command, ".+",
                 on_exit=Console_Process.on_exit,
                 on_match=Console_Process.on_output)

        bash = Console_Process(["/bin/sh", "-i"])
    """

    def __init__(self, name, force=False, on_input=None, on_destroy=None,
                 accept_input=True, on_resize=None, on_interrupt=None,
                 on_completion=None, on_key='', manage_prompt=True,
                 ansi=False, toolbar_name='', give_focus_on_create=True):
        """
        Creates a new instance of :class:`GPS.Console`. GPS tries to reuse
        any existing console with the same name. If none exists yet, or the
        parameter force is set to True, GPS creates a new console.

        You cannot create the Python and Shell consoles through this call. If
        you try, an exception is raised. Instead, use
        :func:`GPS.execute_action` ("/Tools/Consoles/Python"), and then get a
        handle on the console through :class:`GPS.Console`. This is because
        these two consoles are tightly associated with each of the scripting
        languages.

        If GPS reuses an existing console, :func:`on_input` overrides the
        callback that was already set on the console, while
        :func:`on_destroy` is called in addition to the one that was
        already set on the console.

        If this is not the desired behavior, you can also call
        :func:`destroy` on the console and call the constructor again.

        - The function :func:`on_input` is called whenever the user has
          entered a new command in the console and pressed <enter> to execute
          it. It is called with the following parameters:

          - $1: The instance of :class:`GPS.Console`
          - $2: The command to execute

          See the function :func:`GPS.Console.set_prompt_regexp` for proper
          handling of input in the console.

        - The subprogram :func:`on_destroy` is called whenever the user
          closes the console. It is called with a single parameter:

          - $1: The instance of :class:`GPS.Console`

        - The subprogram :func:`on_completion` is called whenever the user
          presses :kbd:`Tab` in the console. It is called with a single
          parameter:

          - $1: The instance of :class:`GPS.Console`

          The default implementation inserts a tab character, but you can to
          add additional user input through :func:`GPS.Console.add_input` for
          example.

        - The subprogram :func:`on_resize` is called whenever the console is
          resized by the user. It is passed three parameters:

          - $1: the instance of :class:`GPS.Console`
          - $2: the number of visible rows in the console,
          - $3: the number of visible columns.

          This is mostly useful when a process is running in the
          console, in which case you can use
          :func:`GPS.Process.set_size` to let the process know the
          size. Note that the size passed to this callback is
          conservative: since all characters might not have the same
          size, GPS tries to compute the maximal number of visible
          characters and pass this to the callback, but the exact
          number of characters might depend on the font.

        - The subprogram :func:`on_interrupt` is called when the user
          presses :kbd:`Ctrl-c` in the console. It receives a single
          parameter, the instance of :class:`GPS.Console`. By default a
          :kbd:`Ctrl-c` is handled by GPS itself by killing the last
          process that was started.

          As described above, GPS provides a high-level handling of
          consoles, where it manages histories, completion, command
          line editing and execution on its own through the callbacks
          described above. This is usually a good thing and provides
          advanced functionalities to some programs that lack
          them. However, there are cases where this gets in the
          way. For example, if you want to run a Unix shell or a
          program that manipulates the console by moving the cursor
          around on its own, the high-level handling of GPS gets in
          the way. In such a case, the following parameters can be
          used: on_key, manage_prompt and ansi.

        - ``ansi`` should be set to true if GPS should emulate an ANSI
          terminal. These are terminals that understand certain escape
          sequences that applications sent to move the cursor to
          specific positions on screen or to change the color and
          attributes of text.

        - ``manage_prompt`` should be set to False to disable GPS's
          handling of prompts. In general, this is incompatible with
          using the :func:`on_input` callback, since GPS no longer
          distinguishes what was typed by the user and what was
          written by the external application. This also means that
          the application is free to write anywhere on the
          screen. This should in general be set to True if you expect
          your application to send ANSI sequences.

        - :func:`on_key` is a function called every time the user
          presses a key in the console. This is much lower-level than
          the other callbacks above, but if you are driving external
          applications you might have a need to send the keys as they
          happen, and not wait for a newline. :func:`on_key` receives
          four parameters:

          - $1: the instance of :class:`GPS.Console`

          - $2: "keycode": this is the internal keycode for the key
            that the user pressed. All keys can be represented this
            way, but this will occasionaly be left to 0 when the user
            input was simulated and no real key was pressed.

          - $3: "key": this is the unicode character that the user
            entered. This will be 0 when the character is not
            printable (for example return, tab, and key up). In
            Python, you can manipulate it with code like
            :command:`unichr(key).encode("utf8")` to get a string
            representation that can be sent to an external process

          - $4: "modifier": these are the state of the control, shift,
            mod1 and lock keys. This is a bitmask, where shift is 1,
            lock is 2, control is 4 and mod1 is 8.

        - :``toolbar_name`` is used to register a toolbar for the
          console.
          The given name can be used later to register toolbar items
          (e.g: using the `GPS.Action.button` function).

        - :``give_focus_on_create`` is only used if a new console is
          being created. It should be set to True if the newly created
          console should receive the focus. If it's set to False,
          the console will not receive the focus: its tab label
          will be highlighted instead.

        :param name: A string
        :param force: A boolean
        :param on_input: A subprogram, see the description below
        :param on_destroy: A subprogram
        :param accept_input: A boolean
        :param on_resize: A subprogram
        :param on_interrupt: A subprogram
        :param on_completion: A subprogram
        :param on_key: A subprogram
        :param manage_prompt: A boolean
        :param ansi: A boolean
        :param toolbar_name: A string
        :param give_focus_on_create: A boolean
        """
        pass  # implemented in Ada

    def accept_input(self):
        """
        Returns True if the console accepts input, False otherwise.

        :return: A boolean
        """
        pass  # implemented in Ada

    def add_input(self, text):
        """
        Adds extra text to the console as if the user had typed it. As
        opposed to text inserted using :func:`GPS.Console.write`, this text
        remains editable by the user.

        :param text: A string
        """
        pass  # implemented in Ada

    def clear(self):
        """
        Clears the current contents of the console.
        """
        pass  # implemented in Ada

    def clear_input(self):
        """
        Removes any user input that the user has started typing (i.e., since
        the last output inserted through :func:`GPS.Console.write`).
        """
        pass  # implemented in Ada

    def copy_clipboard(self):
        """
        Copies the selection to the clipboard.
        """
        pass  # implemented in Ada

    def create_link(self, regexp, on_click, foreground="blue", background="",
                    underline=True):
        """
        Registers a regular expression that should be highlighted in this
        console to provide hyperlinks, which are searched for when calling
        :func:`GPS.Console.write_with_links`. The part of the text that
        matches any of the link registered in the console through
        :func:`GPS.Console.create_link` is highlighted in blue and
        underlined, just like a hyperlink in a web browser. If the user
        clicks on that text, :func:`on_click` is called with one parameter,
        the text that was clicked on. This can, for example, be used to jump
        to an editor or open a web browser.

        If the regular expression does not contain any parenthesis, the text
        that matches the whole regexp is highlighted as a link. Otherwise,
        only the part of the text that matches the first parenthesis group is
        highlighted (so you can test for the presence of text before or after
        the actual hyper link).

        Parameters foreground and background specify colors to visualize
        matched text, while underline turns underscore on.

        :param regexp: A string
        :param on_click: A subprogram
        :param foreground: A string
        :param background: A string
        :param underline: A boolean

        .. seealso:: :func:`GPS.Console.write_with_links`
        """
        pass  # implemented in Ada

    def delete_links(self):
        """
        Drops each regular expression registered with :func:`create_link`.

        """
        pass  # implemented in Ada

    def enable_input(self, enable):
        """
        Makes the console accept or reject input according to the value of
        "enable".

        :param enable: A boolean
        """
        pass  # implemented in Ada

    def flush(self):
        """
        Does nothing, needed for compatibility with Python's file class.
        """
        pass  # implemented in Ada

    def get_text(self):
        """
        Returns the content of the console.

        :return: A string
        """
        pass  # implemented in Ada

    def insert_link(self, text, on_click):
        """
        Inserts the given text in the console as an hyperlink, using the
        default hyperlink style.
        If the user clicks on that text, :func:`on_click` is called with one
        parameter, the text that was clicked on. This can, for example, be used
        to jump to an editor or open a web browser.

        :param text: A string
        :param on_click: A subprogram
        """
        pass  # implemented in Ada

    def isatty(self):
        """
        Returns True if the console behaves like a terminal. Mostly needed for
        compatibility with Python's file class.

        :return: A boolean
        """
        pass  # implemented in Ada

    def read(self):
        """
        Reads the available input in the console. Currently, this behaves
        exactly like :func:`readline`.

        :return: A String
        """
        pass  # implemented in Ada

    def readline(self):
        """
        Asks the user to enter a new line in the console, and returns that
        line. GPS is blocked until enter has been pressed in the console.

        :return: A String
        """
        pass  # implemented in Ada

    def select_all(self):
        """
        Selects the complete contents of the console.
        """
        pass  # implemented in Ada

    def write(self, text, mode='"text"'):
        """
        Outputs some text on the console. This text is read-only. If the user
        started typing some text, that text is temporarily removed, the next
        text is inserted (read-only), and the user text is put back.

        The optional parameter mode specifies the kind of the output text:
        "text" for ordinary messages (this is default), "log" for log messages,
        and "error" for error messages.

        :param text: A utf8 string
        :param mode: A string, one of "text", "log", "error"

        .. seealso:: :func:`GPS.Console.write_with_links`

        .. code-block:: python

            Console().write(
                u"\N{LATIN CAPITAL LETTER E WITH ACUTE}".encode("utf-8")
            )
        """
        pass  # implemented in Ada

    def write_with_links(self, text):
        """
        Outputs some text on the console, highlighting the parts of it that
        match the regular expression registered by
        :func:`GPS.Console.create_link`.

        :param text: A utf8 string

        .. code-block:: python

           import re

           console = GPS.Console("myconsole")
           console.create_link("(([\w-]+):(\d+))", open_editor)
           console.write_with_link("a file.adb:12 location in a file")

           def open_editor(text):
              matched = re.match("([\w+-]+):(\d+)", text)
              buffer = GPS.EditorBuffer.get(GPS.File (matched.group(1)))
              buffer.current_view().goto(
                 buffer.at(int(matched.group(2)), 1))

        """
        pass  # implemented in Ada


###########################################################
# Contextual
###########################################################

class Contextual(object):

    """
    This class is a general interface to the contextual menus in GPS. It gives
    you control over which menus should be displayed when the user right clicks
    in parts of GPS.

    .. seealso:: :func:`GPS.Contextual.__init__`
    """

    name = ''
    """The name of the contextual menu (see __init__)"""

    def __init__(self, name):
        """
        Initializes a new instance of :class:`GPS.Contextual`. The name is
        what was given to the contextual menu when it was created and is a
        static string independent of the actual label used when the menu is
        displayed (and which is dynamic, depending on the context). You can
        get the list of valid names by checking the list of names returned by
        :func:`GPS.Contextual.list`.

        :param name: A string

        .. seealso:: :func:`GPS.Contextual.list`

        .. code-block:: python

           # You could for example decide to always hide the "Goto
           # declaration" contextual menu with the following call:

           GPS.Contextual ('Goto declaration of entity').hide()

           # After this, the menu will never be displayed again.
        """
        pass  # implemented in Ada

    def create_dynamic(self, factory, on_activate, label='', filter=None,
                       ref='', add_before=True, group='0'):
        """
        Creates a new dynamic contextual menu.

        This is a submenu of a contextual menu, where the entries are
        generated by the ``factory`` parameter. This parameter should return
        a list of strings, which will be converted to menus by GPS. These
        strings can contain '/' characters to indicate submenus.

        ``filter`` is a subprogram that takes the :class:`GPS.Context` as a
        parameter and returns a boolean indicating whether the submenu
        should be displayed.

        ``label`` can be used to specify the label to use for the menu
        entry. It can include directory-like syntax to indicate
        submenus. This label can include standard macro substitution (see the
        GPS documentation), for instance %e for the current entity name.

        ``on_activate`` is called whenever any of the entry of the menu is
        selected, and is passed three parameters, the context in which the
        contextual menu was displayed, the string representing the selected
        entry and the index of the selected entry within the array returned
        by factory (index starts at 0).

        The parameters ``ref`` and ``add_before`` can be used to control the
        location of the entry within the contextual menu. ref is the name of
        another contextual menu entry, and add_before indicates whether the
        new entry is put before or after that second entry.

        :param factory: A subprogram
        :param on_activate: A subprogram
        :param label: A string
        :param filter: A subprogram
        :param ref: A string
        :param add_before: A boolean
        :param group: A integer

        .. code-block:: python

           ## This example shows how to create a contextual menu
           ## through global functions

           def build_contextual(context):
              return ["Choice1", "Choice2"]

           def on_activate(context, choice, choice_index):
              GPS.Console("Messages").write("You selected " + choice)

           def filter(contextl):
              return context.entity_name() is not None

           GPS.Contextual("My_Dynamic_Menu").create_dynamic(
              on_activate=on_activate, factory=build_contextual, filter=filter)

        .. code-block:: python

           ## This example is similar to the one above, but shows how
           ## to create the menu through a python class.
           ## Note how self can be passed to the callbacks thanks to the
           ## call to self.create_dynamic.

           class Dynamic(GPS.Contextual):
              def __init__(self):
                 GPS.Contextual.__init__(self, "My Dynamic Menu")
                 self.create_dynamic(on_activate=self.on_activate,
                                     label="References/My menu",
                                     filter=self.filter,
                                     factory=self.factory)

               def filter(self, context):
                  return context.entity_name() is not None

               def on_activate(self, context, choice, choice_index):
                  GPS.Console("Messages").write("You selected " + choice)

               def factory(self, context):
                  return ["Choice1", "Choice2"]
        """
        pass  # implemented in Ada

    def hide(self):
        """
        Makes sure the contextual menu never appears when the user right
        clicks anywhere in GPS. This is the standard way to disable
        contextual menus.

        .. seealso:: :func:`GPS.Contextual.show`
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of all registered contextual menus. This is a list
        of strings which are valid names that can be passed to the
        constructor of :class:`GPS.Contextual`. These names were created when
        the contextual menu was registered in GPS.

        :return: A list of strings

        .. seealso:: :func:`GPS.Contextual.__init__`
        """
        pass  # implemented in Ada

    def set_sensitive(self, Sensitivity):
        """
        Controls whether the contextual menu is grayed-out: False if it
        should be grayed-out, True otherwise.

        :param Sensitivity: Boolean value
        """
        pass  # implemented in Ada

    def show(self):
        """
        Makes sure the contextual menu is shown when appropriate. The entry
        might still be invisible if you right clicked on a context where it
        does not apply, but it will be checked.

        .. seealso:: :func:`GPS.Contextual.hide`
        """
        pass  # implemented in Ada


###########################################################
# Debugger
###########################################################

class Debugger(object):

    """
    Interface to debugger related commands. This class allows you to start a
    debugger and send commands to it.  By connection to the various debugger_*
    hooks, you can also monitor the state of the debugger.

    By connecting to the "debugger_command_action_hook", you can also create
    your own debugger commands, that the user can then type in the debugger
    console. This is a nice way to implement debugger macros.

    .. seealso:: :func:`GPS.Debugger.__init__`

    .. code-block:: python

       import GPS

       def debugger_stopped(hook, debugger):
          GPS.Console("Messages").write(
            "hook=" + hook + " on debugger="
            + `debugger.get_num()` + "\\n")

       def start():
          d = GPS.Debugger.spawn(GPS.File("../obj/parse"))
          d.send("begin")
          d.send("next")
          d.send("next")
          d.send("graph display A")

       GPS.Hook("debugger_process_stopped").add(debugger_stopped)
    """

    current_file = None
    """
    A :class:`GPS.File` which indicates the current file for the debugger.
    This is the place where the debugger stopped, or when the user selected
    a new frame, the file corresponding to that frame.
    """

    current_line = 0
    """
    The current line. See description of `GPS.Debugger.current_file`.
    """

    remote_target = None
    """
    A string set to the currently used debugger's remote target.
    This remote target is either retrieved from the IDE'Protocol_Host
    project atttribute or from a manually sent
    'target [remote_protocol] [remote_target]' command.
    """

    remote_protocol = None
    """
    A string set to the debugger's currently used remote protocol. This remote
    target is either retrieved from the IDE'Communication_Protocol project
    atttribute or from a manually sent
    'target [remote_protocol] [remote_target]' command.
    """

    breakpoints = []
    """
    A read-only property that returns the list of breakpoints currently set
    in the debugger. This information is updated automatically by GPS whenever
    a command that might modify this list of breakpoints is executed.
    The elements in this list are instances of :class:`GPS.DebuggerBreakpoint`
    """

    def __init__(self):
        """
        It is an error to create a :class:`Debugger` instance
        directly. Instead, use :func:`GPS.Debugger.get` or
        :func:`GPS.Debugger.spawn`.

        .. seealso::

           :func:`GPS.Debugger.get`

           :func:`GPS.Debugger.spawn`
        """
        pass  # implemented in Ada

    def close(self):
        """
        Closes the given debugger. This also closes all associated windows
        (such as the call stack and console).
        """
        pass  # implemented in Ada

    def command(self):
        """
        Returns the command being executed in the debugger. This is often
        only available when called from the "debugger_state_changed" hook,
        where it might also indicate the command that just finished.

        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get(id=None):
        """
        Gives access to an already running debugger, and returns an instance
        of :class:`GPS.Debugger` attached to it. The parameter can be null,
        in which case the current debugger is returned, it can be an integer,
        in which case the corresponding debugger is returned (starting at 1),
        or it can be a file, in which case this function returns the debugger
        currently debugging that file.

        :param id: Either an integer or an instance of :class:`GPS.File`
        :return: An instance of :class:`GPS.Debugger`

        """
        pass  # implemented in Ada

    def get_executable(self):
        """
        Returns the name of the executable currently debugged in the debugger.

        :return: An instance of :class:`GPS.File`

        .. seealso:: :func:`GPS.Debugger.get_num`
        """
        pass  # implemented in Ada

    def get_num(self):
        """
        Returns the index of the debugger. This can be used later to retrieve
        the debugger from :func:`GPS.Debugger.get` or to get access to other
        windows associated with that debugger.

        :return: An integer

        .. seealso:: :func:`GPS.Debugger.get_file`
        """
        pass  # implemented in Ada

    def is_break_command(self):
        """
        Returns true if the command returned by :func:`GPS.Debugger.command`
        is likely to modify the list of breakpoints after it finishes
        executing.

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_busy(self):
        """
        Returns true if the debugger is currently executing a command. In this
        case, it is an error to send a new command to it.

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_context_command(self):
        """
        Returns true if the command returned by :func:`GPS.Debugger.command`
        is likely to modify the current context (e.g., current task or
        thread) after it finishes executing.

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_exec_command(self):
        """
        Returns true if the command returned by :func:`GPS.Debugger.command`
        is likely to modify the stack trace in the debugger (e.g., "next" or
        "cont").

        :return: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of currently running debuggers.

        :return: A list of :class:`GPS.Debugger` instances
        """
        pass  # implemented in Ada

    def non_blocking_send(self, cmd, output=True):
        """
        Works like send, but is not blocking, and does not return the result.

        :param cmd: A string
        :param output: A boolean

        .. seealso:: :func:`GPS.Debugger.send`
        """
        pass  # implemented in Ada

    def send(self, cmd, output=True, show_in_console=False):
        """
        Executes ``cmd`` in the debugger. GPS is blocked while ``cmd`` is
        executing on the debugger. If output is true, the command is displayed
        in the console.

        If ``show_in_console`` is True, the output of the command is displayed
        in the debugger console, but is not returned by this function. If
        ``show_in_console`` is False, the result is not displayed in the
        console, but is returned by this function.

        There exists a number of functions that execute specific commands and
        parse the output appropriately. It is better to use this functions
        directly, since they will change the actual command emitted depending
        on which debugger is running, whether it is currently in a C or Ada
        frame,...

        :param cmd: A string
        :param output: A boolean
        :param show_in_console: A boolean
        :return: A string

        .. seealso:: :func:`GPS.Debugger.non_blocking_send`
        .. seealso:: :func:`GPS.Debugger.value_of`
        .. seealso:: :func:`GPS.Debugger.set_variable`
        .. seealso:: :func:`GPS.Debugger.break_at_location`
        .. seealso:: :func:`GPS.Debugger.unbreak_at_location`
        """

    def value_of(self, expression):
        """
        Compute the value of expression in the current context.

        Equivalent gdb command is "print".

        :param str expression: the expression to evaluate.
        :return: a string, or "" if the expression could not be evaluated in
           the current context.
        """

    def set_variable(self, variable, value):
        """
        Set the value of a specific variable in the current context.

        Equivalent gdb command is "set variable".

        :param str variable: the name of the variable to set.
        :param str value: the value to set it to, as a string
        """

    def break_at_location(self, file, line):
        """
        Set a breakpoint at a specific location.
        If no debugger is currently running, this commands ensures that a
        breakpoint will be set when a debugger is actually started.

        Equivalent gdb command is "break".

        :param GPS.File file: the file to break into
        :param int line: the line to break at
        """

    def unbreak_at_location(self, file, line):
        """
        Remove any breakpoint set at a specific location.

        Equivalent gdb command is "clear".
        If no debugger is currently running, this commands ensures that no
        breakpoint will be set at that location when a debugger is actually
        started.

        :param GPS.File file: the file to break into
        :param int line: the line to break at
        """

    def get_console(self):
        """
        Returns the :class:`GPS.Console` instance associated with the
        the given debugger's console.

        :return: An instance of :class:`GPS.Console`
        """
        pass  # implemented in Ada

    @staticmethod
    def spawn(executable,
              args='',
              remote_target='',
              remote_protocol='',
              load_executable=False):
        """
        Starts a new debugger. It will debug ``executable``. When the program
        is executed, the extra arguments args are passed.

        If ``remote_target`` and ``remote_protocol`` are specified and
        non-empty, the debugger will try to initialize a remote debugging
        session with these parameters. When not specified, the
        ``IDE'Program_Host`` and ``IDE'Communication_Protocol`` are used
        if present in the .gpr project file.

        When ``load_executable`` is True, GPS will try to load ``executable``
        on the specified remote target, if any.

        :param executable: An instance of GPS.File
        :param args: A string
        :param remote_target: A string
        :param remote_protocol: A string
        :param load_executable: A boolean
        :return: An instance of :class:`GPS.Debugger`
        """
        pass  # implemented in Ada

    def frames(self):
        """
        Returns list of dictionaries:
          "level" - integer
          "addr" - string
          "func" - string
          "file" - GPS.FileLocation
          "args" - another dictionary with func parameters
            represented as string

        :return: A list of frames
        """
        pass  # implemented in Ada

    def current_frame(self):
        """
        Returns the number of current frame.

        :return: integer, the number of frame
        """
        pass  # implemented in Ada

    def frame_up(self):
        """
        Select next frame.
        """
        pass  # implemented in Ada

    def frame_down(self):
        """
        Select previous frame.
        """
        pass  # implemented in Ada

    def select_frame(self, num):
        """
        Select frame by number.

        :param num: The number of frame where 0 is the first frame
        """
        pass  # implemented in Ada


###########################################################
# DebuggerBreakpoint
###########################################################

class DebuggerBreakpoint(object):
    """
    Instances of this class represents one breakpoint set in the debugger.
    """

    num = 0
    """The identifier for this breakpoint"""

    type = ""
    """Either 'breakpoint' or 'watchpoint'"""

    enabled = True
    """Whether this breakpoint is enabled"""

    watched = ""
    """
    If the breakpoint is a watchpoint, i.e. monitors changes to a
    variable, this property gives the name of the variable.
    """

    file = None
    """
    An instance of GPS.File, where the debugger will stop.
    """

    line = 0
    """
    The line on which the debugger will stop
    """


###########################################################
# Docgen
###########################################################

class Docgen(object):

    """
    Interface for handling customized documentation generation. This class is
    used in conjunction with GPS.DocgenTagHandler. You cannot directly create
    this class.  Instead, use the ones furnished by
    :func:`GPS.DocgenTagHandler` callbacks.

    .. seealso:: :func:`GPS.DocgenTagHandler`
    """

    def generate_index_file(self, name, filename, content):
        """
        Creates a new Index file. The file ``filename`` will be titled
        ``name`` and will contain the general decoration along with
        `content``.  All other generated documentation files will have a link
        to it, for convenience.

        :param name: The name of the new index file.
        :param filename: The created file name.
        :param content: The content of the created file.
        """
        pass  # implemented in Ada

    def get_current_file(self):
        """
        Retrieves the current analysed source file. You should call this
        method only from a :func:`GPS.DocgenTagHandler.on_match` callback.

        :return: A :class:`GPS.File` instance
        """
        pass  # implemented in Ada

    def get_doc_dir(self):
        """
        Retrieves the directory that will contain the documentation. You
        should call this method only from a
        :func:`GPS.DocgenTagHandler.on_match` callback.

        :return: A :class:`GPS.File` instance

        """
        pass  # implemented in Ada

    @staticmethod
    def register_css(filename):
        """
        Registers a new CSS file to use when generating the documentation. This
        allows to overriding a default style or adding new ones for custom
        tag handling.

        :param filename: A file name
        """
        pass  # implemented in Ada

    @staticmethod
    def register_main_index(filename):
        """
        Registers the file to be used as main page
        (e.g. :file:`index.html`). By default, the first page generated in
        the Table of Contents is used.

        :param filename: A file name
        """
        pass  # implemented in Ada

    @staticmethod
    def register_tag_handler(handler):
        """
        Registers a new tag handler. This handler will be used each time a new
        documentation is generated and the corresponding tag is found.

        :param handler: The handler to register

        .. code-block:: python

           # register a default handler for tag <description>
           # that is, -- <description>sth</description>
           # will be translated as <div class="description">sth</div>
           GPS.Docgen.register_tag_handler(GPS.DocgenTagHandler("description"))
        """
        pass  # implemented in Ada

###########################################################
# DocgenTagHandler
###########################################################


class DocgenTagHandler(object):

    """
    This class is used to handle user-defined documentation tags. This allows
    custom handling of comments such as ::

    -- <summary>This fn does something</summary>

    .. seealso:: :func:`GPS.Docgen`

    .. code-block:: python

      import GPS

      class ScreenshotTagHandler(GPS.DocgenTagHandler):
         "Handling for <screenshot>screen.jpg</screenshot>"

         def __init__(self):
            GPS.DocgenTagHandler.__init__(
                self, "screenshot",
                on_match=self.on_match, on_start=self.on_start,
                on_exit=self.on_exit
            )

         def on_start(self, docgen):
            self.list = {}

         def on_match(self, docgen, attrs, value, entity_name, entity_href):
            # In this examples, images are in the directory
            # _project_root_/doc/imgs/

            dir = docgen.get_current_file().project().file().directory()
            dir = dir + "doc/imgs/"
            img = '<img src="%s%s" alt="%s"/>"' % (dir, value, value)
            self.list[entity_name] = [entity_href, img]
            return "<h3>Screenshot</h3><p>%s</p>" % (img)

         def on_exit(self, docgen):
            content=""

            for pict in sorted(self.list.keys()):
               content += "<div class='subprograms'>"
               content += "  <div class='class'>"
               content += "    <h3>%s</h3>" % (pict)
               content += "    <div class='comment'>"
               content += "      <a href="%s">%s</a>" % (self.list[pict][0],
                                                      self.list[pict][1])
               content += "    </div>"
               content += "  </div>"
               content += "</div>"

            if content != "":
               docgen.generate_index_file("Screenshots",
                                          "screenshots.html", content)

      def on_gps_start(hook):
         GPS.Docgen.register_css(GPS.get_system_dir() +
                                 "share/mycustomfiles/custom.css")
         GPS.Docgen.register_tag_handler(ScreenshotTagHandler())

      GPS.Hook("gps_started").add(on_gps_start)
    """

    def __init__(self, tag, on_start=None, on_match=None, on_exit=None):
        """
        Create a new :func:`GPS.DocgenTagHandler` instance handling the tag
        "tag". You need to register it afterwards using
        :func:`GPS.Docgen.register_tag_handler`.

        ``on_match`` is a callback that is called each time a tag
        corresponding to the GPS.DocgenTagHandler is analysed. It takes the
        following parameters:

        - $1 = the instance of :class:`GPS.Docgen`
        - $2 = the eventual attributes of the tag
        - $3 = the value of the tag
        - $4 = the entity name linked to the analysed tag
        - $5 = the href to the entity documentation location

        ``on_start`` is a callback that is called each time a documentation
        generation starts. It takes the following parameters:

        - $1 = the instance of :class:`GPS.Docgen`

        ``on_exit`` is a callback that is called each time a documentation
        generation finishes. It takes the following parameters:

        - $1 = the instance of :class:`GPS.Docgen`

        Using the default values of the callbacks (e.g. None), the
        :func:`GPS.DocgenTagHandler` handler will translate comments of the
        form "-- <tag>value</tag>" by "<div class="tag">value</div>".

        :param tag: The tag that is handled
        :param on_start: A subprogram
        :param on_match: A subprogram
        :param on_exit: A subprogram
        """
        pass  # implemented in Ada


###########################################################
# Editor
###########################################################

class Editor(object):

    """
    Deprecated interface to all editor-related commands.
    """

    @staticmethod
    def add_blank_lines(file, start_line, number_of_lines, category=''):
        """
        OBSOLESCENT.

        Adds number_of_lines non-editable lines to the buffer editing file,
        starting at line start_line. If category is specified, use it for
        highlighting. Create a mark at beginning of block and return it.

        :param file: A string
        :param start_line: An integer
        :param number_of_lines: An integer
        :param category: A string
        :return: an instance of :class:`GPS.EditorMark`
        """
        pass  # implemented in Ada

    @staticmethod
    def add_case_exception(name):
        """
        OBSOLESCENT.

        Adds name into the case exception dictionary.

        :param name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def block_fold(file, line=None):
        """
        OBSOLESCENT.

        Folds the block around line. If line is not specified, fold all blocks
        in the file.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_end(file, line):
        """
        OBSOLESCENT.

        Returns ending line number for block enclosing line.

        :param file: A string
        :param line: An integer
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_level(file, line):
        """
        OBSOLESCENT.

        Returns nested level for block enclosing line.

        :param file: A string
        :param line: An integer
        :return: An integer

        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_name(file, line):
        """
        OBSOLESCENT.

        Returns name for block enclosing line

        :param file: A string
        :param line: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_start(file, line):
        """
        OBSOLESCENT.

        Returns ending line number for block enclosing line.

        :param file: A string
        :param line: An integer
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_type(file, line):
        """
        OBSOLESCENT.

        Returns type for block enclosing line.

        :param file: A string
        :param line: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def block_unfold(file, line=None):
        """
        OBSOLESCENT.

        Unfolds the block around line. If line is not specified, unfold all
        blocks in the file.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def close(file):
        """
        OBSOLESCENT.

        Closes all file editors for file.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def copy():
        """
        OBSOLESCENT.

        Copies the selection in the current editor.
        """
        pass  # implemented in Ada

    @staticmethod
    def create_mark(filename, line=1, column=1, length=0):
        """
        Creates a mark for file_name, at position given by line and
        column. Length corresponds to the text length to highlight after the
        mark. The identifier of the mark is returned. Use the command
        goto_mark to jump to this mark.

        :param filename: A string
        :param line: An integer
        :param column: An integer
        :param length: An integer
        :return: An instance of :class:`GPS.EditorMark`

        .. seealso::

             :func:`GPS.Editor.goto_mark`

             :func:`GPS.Editor.delete_mark`
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_center(file):
        """
        OBSOLESCENT.

        Scrolls the view to center cursor.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_get_column(file):
        """
        OBSOLESCENT.

        Returns current cursor column number.

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_get_line(file):
        """
        OBSOLESCENT.

        Returns current cursor line number.

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_set_position(file, line, column=1):
        """
        OBSOLESCENT.

        Sets cursor to position line/column in buffer file.

        :param file: A string
        :param line: An integer
        :param column: An integer

        """
        pass  # implemented in Ada

    @staticmethod
    def cut():
        """
        OBSOLESCENT.

        Cuts the selection in the current editor.
        """
        pass  # implemented in Ada

    @staticmethod
    def edit(filename, line=1, column=1, length=0, force=False, position=5):
        """
        OBSOLESCENT.

        Opens a file editor for file_name. Length is the number of characters
        to select after the cursor. If line and column are set to 0, then the
        location of the cursor is not changed if the file is already opened
        in an editor. If force is set to true, a reload is forced in case the
        file is already open. Position indicates the MDI position to open the
        child in (5 for default, 1 for bottom).

        The filename can be a network file name, with the following general
        format::

            protocol://username@host:port/full/path

        where protocol is one of the recognized protocols (http, ftp,.. see the
        GPS documentation), and the username and port are optional.

        :param filename: A string
        :param line: An integer
        :param column: An integer
        :param length: An integer
        :param force: A boolean
        :param position: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def get_buffer(file):
        """
        OBSOLESCENT.

        Returns the text contained in the current buffer for file.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_chars(filename, line=0, column=1, before=-1, after=-1):
        """
        OBSOLESCENT.

        Gets the characters around a certain position. Returns string between
        "before" characters before the mark and "after" characters after the
        position. If "before" or "after" is omitted, the bounds will be at the
        beginning and/or the end of the line.

        If the line and column are not specified, then the current selection is
        returned, or the empty string if there is no selection.

        :param filename: A string
        :param line: An integer
        :param column: An integer
        :param before: An integer
        :param after: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_last_line(file):
        """
        OBSOLESCENT.

        Returns the number of the last line in file.

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def goto_mark(mark):
        """
        OBSOLESCENT.

        Jumps to the location of the mark corresponding to identifier.

        :param mark: A instance of :class:`GPS.EditorMark`

        .. seealso:: :func:`GPS.Editor.create_mark`
        """
        pass  # implemented in Ada

    @staticmethod
    def highlight(file, category, line=0):
        """
        OBSOLESCENT

        Marks a line as belonging to a highlighting category. If line is not
        specified, mark all lines in file.

        :param file: A string
        :param category: A string
        :param line: An integer

        .. seealso:: :func:`GPS.Editor.unhighlight`
        """
        pass  # implemented in Ada

    @staticmethod
    def highlight_range(file, category, line=0, start_column=0,
                        end_column=-1):
        """
        OBSOLESCENT>

        Highlights a portion of a line in a file with the given category.

        :param file: A string
        :param category: A string
        :param line: An integer
        :param start_column: An integer
        :param end_column: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def indent(current_line_only=False):
        """
        OBSOLESCENT.

        Indents the selection (or the current line if requested) in current
        editor. Does nothing if the current GPS window is not an editor.

        :param current_line_only: A boolean

        """
        pass  # implemented in Ada

    @staticmethod
    def indent_buffer():
        """
        OBSOLESCENT.

        Indents the current editor. Does nothing if the current GPS window is
        not an editor.
        """
        pass  # implemented in Ada

    @staticmethod
    def insert_text(text):
        """
        OBSOLESCENT.

        Inserts a text in the current editor at the cursor position.

        :param text: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def mark_current_location():
        """
        OBSOLESCENT.

        Pushes the location in the current editor in the history of
        locations.  This should be called before jumping to a new location on
        a user's request, so that he can easily choose to go back to the
        previous location.
        """
        pass  # implemented in Ada

    @staticmethod
    def paste():
        """
        OBSOLESCENT.

        Pastes the selection in the current editor.
        """
        pass  # implemented in Ada

    @staticmethod
    def print_line_info(file, line):
        """
        OBSOLESCENT.

        Prints the contents of the items attached to the side of a line. This
        is used mainly for debugging and testing purposes.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def redo(file):
        """
        OBSOLESCENT.

        Redoes the last undone editing command for file.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def refill():
        """
        OBSOLESCENT.

        Refills selected (or current) editor lines. Does nothing if the
        current GPS window is not an editor.
        """
        pass  # implemented in Ada

    @staticmethod
    def register_highlighting(category, color, speedbar=False):
        """
        OBSOLESCENT.

        Creates a new highlighting category with the given color. The format
        for color is "#RRGGBB". If speedbar is true, then a mark will be
        inserted in the speedbar to the left of the editor to give a fast
        overview to the user of where the highlighted lines are.

        :param category: A string
        :param color: A string
        :param speedbar: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_blank_lines(mark, number=0):
        """
        OBSOLESCENT

        Removes blank lines located at mark. If number is specified, remove
        only the number first lines.

        :param mark: an instance of :class:`GPS.EditorMark`
        :param number: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_case_exception(name):
        """
        OBSOLESCENT.

        Removes name from the case exception dictionary.

        :param name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def replace_text(file, line, column, text, before=-1, after=-1):
        """
        OBSOLESCENT.

        Replaces the characters around a certain position. "before"
        characters before (line, column), and up to "after" characters after
        are removed, and the new text is inserted instead. If "before" or
        "after" is omitted, the bounds will be at the beginning and/or the
        end of the line.

        :param file: A string
        :param line: An integer
        :param column: An integer
        :param text: A string
        :param before: An integer
        :param after: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def save(interactive=True, all=True):
        """
        OBSOLESCENT.

        Saves current or all files. If interactive is true, then prompt before
        each save. If all is true, then all files are saved.

        :param interactive: A boolean
        :param all: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def save_buffer(file, to_file=None):
        """
        OBSOLESCENT.

        Saves the text contained in the current buffer for file. If to_file is
        specified, the file will be saved as to_file, and the buffer status
        will not be modified.

        :param file: A string
        :param to_file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def select_all():
        """
        OBSOLESCENT.

        Selects the whole editor contents.
        """
        pass  # implemented in Ada

    @staticmethod
    def select_text(first_line, last_line, start_column=1, end_column=0):
        """
        OBSOLESCENT.

        Selects a block in the current editor.

        :param first_line: An integer
        :param last_line: An integer
        :param start_column: An integer
        :param end_column: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def set_background_color(file, color):
        """
        OBSOLESCENT.

        Sets the background color for the editors for file.

        :param file: A string
        :param color: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_synchronized_scrolling(file1, file2, file3=''):
        """
        OBSOLESCENT.

        Synchronizes the scrolling between multiple editors.

        :param file1: A string
        :param file2: A string
        :param file3: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_title(file, title, filename):
        """
        OBSOLESCENT.

        Changes the title of the buffer containing the given file.

        :param file: A string
        :param title: A string
        :param filename: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_writable(file, writable):
        """
        OBSOLESCENT.

        Changes the Writable status for the editors for file.

        :param file: A string
        :param writable: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def subprogram_name(file, line):
        """
        OBSOLESCENT.

        Returns the name of the subprogram enclosing line.

        :param file: A string
        :param line: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def undo(file):
        """
        OBSOLESCENT.

        Undoes the last editing command for file.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def unhighlight(file, category, line=0):
        """
        OBSOLESCENT.

        Unmarks the line for the specified category. If line is not specified,
        unmark all lines in file.

        :param file: A string
        :param category: A string
        :param line: An integer

        .. seealso:: :func:`GPS.Editor.highlight`
        """
        pass  # implemented in Ada

    @staticmethod
    def unhighlight_range(file, category, line=0, start_column=0,
                          end_column=-1):
        """
        OBSOLESCENT.

        Removes highlights for a portion of a line in a file.

        :param file: A string
        :param category: A string
        :param line: An integer
        :param start_column: An integer
        :param end_column: An integer
        """
        pass  # implemented in Ada


###########################################################
# EditorBuffer
###########################################################

class EditorBuffer(object):

    """
    This class represents the physical contents of a file. It is always
    associated with at least one view (a :class:`GPS.EditorView instance`),
    which makes it visible to the user. The contents of the file can be
    manipulated through this class.
    """

    extend_existing_selection = False
    """
    When set to True, this flag puts the editor in a special mode where all
    cursor moves will create and extend the selection.
    This is used to emulate the behavior of some editors, like Emacs, or
    vi's "v" mode".

    The default behavior is that cursor moves will cancel any existing
    selection, unless they are associated with the :kbd:`shift` key. In
    this case, a new selection is created if none exists, and the selection
    is extended to include the new cursor location.
    """

    def __init__(self):
        """
        Prevents the direct creation of instances of
        :class:`EditorBuffer`. Use :func:`GPS.EditorBuffer.get` instead
        """
        pass  # implemented in Ada

    def add_special_line(self, start_line, text, category='', name=''):
        """
        Adds one non-editable line to the buffer, starting at line
        ``start_line`` and containing the string ``text``. If ``category`` is
        specified, use it for highlighting. Creates a mark at beginning of
        block and return it. If ``name`` is specified, the returned mark has
        this name.

        :param int start_line: An integer
        :param string text: A string
        :param string category: A string
           Reference one of the categories that were registered via
           :func:`GPS.Editor.register_highlighting`. This can also be the
           name of a style defined via :class:`GPS.Style`
        :param string name: A string
        :rtype: :class:`EditorMark`

        .. seealso:: :func:`GPS.EditorBuffer.get_mark`
        """
        pass  # implemented in Ada

    def apply_overlay(self, overlay, frm='begining of buffer',
                      to='end of buffer'):
        """
        Applies the overlay to the given range of text. This immediately
        changes the rendering of the text based on the properties of the
        overlay.

        :param EditorOverlay overlay: An instance of :class:`GPS
            .EditorOverlay`
        :param EditorLocation frm: An instance of :class:`GPS.EditorLocation`
        :param EditorLocation to: An instance of :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.EditorBuffer.remove_overlay`
        """
        pass  # implemented in Ada

    def at(self, line, column):
        """
        Returns a new location at the given line and column in the buffer.

        :rtype: :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def beginning_of_buffer(self):
        """
        Returns a location pointing to the first character in the buffer.

        :rtype: :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def blocks_fold(self):
        """
        Folds all the blocks in all the views of the buffer. Block folding is
        a language-dependent feature, where one can hide part of the source
        code temporarily by keeping only the first line of the block (for
        instance the first line of a subprogram body, the rest is hidden). A
        small icon is displayed to the left of the first line so it can be
        unfolded later.

        .. seealso::

            :func:`GPS.EditorBuffer.blocks_unfold`

            :func:`GPS.EditorLocation.block_fold`
        """
        pass  # implemented in Ada

    def blocks_unfold(self):
        """
        Unfolds all the blocks that were previously folded in the buffer, ie
        make the whole source code visible. This is a language dependent
        feature.

        .. seealso::

            :func:`GPS.EditorBuffer.blocks_fold`

            :func:`GPS.EditorLocation.block_unfold`
        """
        pass  # implemented in Ada

    def characters_count(self):
        """
        Returns the total number of characters in the buffer.

        :rtype: integer
        """
        pass  # implemented in Ada

    def click_on_side_icon(self, line, column, icon_name):
        """
        Simulate a click on the editor's side icon identified with the
        given ``icon_name`` and present at the given ``line`` and in the given
        side information ``column``.
        The default side information ``column`` (i.e: the one that displays
        block folding of codefix icons) starts at 1.

        :param integer line: the line where the clickable icon is displayed
        :param integer column: the side information column where the
          clickable icon is displayed
        :param string icon_name: the name of the clickable icon
        """
        pass  # implemented in Ada

    def close(self, force=False):
        """
        Closes the editor and all its views. If the buffer has been modified
        and not saved, a dialog is open asking the user whether to save. If
        force is True, do not save and do not ask the user. All changes are
        lost.

        :param bool force: A boolean
        """
        pass  # implemented in Ada

    def copy(self, frm='beginning of buffer', to='end of buffer',
             append=False):
        """
        Copies the given range of text into the clipboard, so that it can be
        further pasted into other applications or other parts of GPS. If
        append is True, the text is appended to the last clipboard entry
        instead of generating a new one.

        :param EditorLocation frm : An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`
        :param bool append: A boolean

        .. seealso:: :func:`GPS.Clipboard.copy`
        """
        pass  # implemented in Ada

    def create_overlay(self, name=''):
        """
        Creates a new overlay. Properties can be set on this overlay, which
        can then be applied to one or more ranges of text to changes its
        visual rqendering or to associate user data with it. If name is
        specified, this function will return an existing overlay with the
        same name in this buffer if any can be found. If the name is not
        specified, a new overlay is created. Changing the properties of an
        existing overlay results in an immediate graphical update of the
        views associated with the buffer.

        A number of predefined overlays exit. Among these are the ones used
        for syntax highlighting by GPS itself, which are "keyword",
        "comment", "string", "character". You can use these to navigate from
        one comment section to the next for example.

        :param string name: A string
        :rtype: :class:`EditorOverlay`
        """
        pass  # implemented in Ada

    def current_view(self):
        """
        Returns the last view used for this buffer, ie the last view that had
        the focus and through which the user might have edited the buffer's
        contents.

        :rtype: :class:`EditorView`
        """
        pass  # implemented in Ada

    def cut(self, frm='beginning of buffer', to='end of buffer',
            append=False):
        """
        Copies the given range of text into the clipboard so that it can be
        further pasted into other applications or other parts of GPS. The
        text is removed from the edited buffer. If append is True, the text
        is appended to the last clipboard entry instead of generating a new
        one.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`
        :param bool append: A boolean
        """
        pass  # implemented in Ada

    def delete(self, frm='beginning of buffer', to='end of buffer'):
        """
        Deletes the given range of text from the buffer.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def end_of_buffer(self):
        """
        Returns a location pointing to the last character in the buffer.

        :rtype: :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def expand_alias(alias):
        """
        Expands given alias in the editor buffer at the point where the
        cursor is.
        """

    def file(self):
        """
        Returns the name of the file edited in this buffer.

        :rtype: :class:`File`
        """
        pass  # implemented in Ada

    @staticmethod
    def get(file='current editor', force=False, open=True,
            only_if_focused=False):
        """
        If ``file`` is already opened in an editor, get a handle on its
        buffer. This instance is then shared with all other buffers
        referencing the same file. As a result, you can, for example,
        associate your own data with the buffer, and retrieve it at any time
        until the buffer is closed. If the file is not opened yet, it is
        loaded in a new editor, and a new view is opened at the same time
        (and thus the editor becomes visible to the user).  If the file is
        not specified, the current editor is returned, which is the last one
        that had the keyboard focus.

        If the file is not currently open, the behavior depends on ``open``:;
        if true, a new editor is created for that file, otherwise None is
        returned.

        When a new file is open, it has received the focus. But if the editor
        already existed, it is not raised explicitly, and you need to do it
        yourself through a call to :func:`GPS.MDIWindow.raise_window` (see
        the example below).

        If ``force`` is true, a reload is forced in case the file is already
        open.

        If ``only_if_focused`` is true, None is returned if the
        corresponding editor does not have the focus.

        :param File file: An instance of :class:`GPS.File`
        :param bool force: A boolean
        :param bool open: A boolean
        :param bool only_if_focused: A boolean
        :rtype: :class:`EditorBuffer`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File ("a.adb"))
           GPS.MDI.get_by_child(ed.current_view()).raise_window()
           ed.data = "whatever"

           # ... Whatever, including modifying ed

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           ed.data   # => "whatever"
        """
        pass  # implemented in Ada

    def get_analysis_unit(self):
        """
        Returns the corresponding libadalang AnalysisUnit.

        :rtype: libadalang.AnalysisUnit
        """
        pass  # implemented in Ada

    def get_chars(self, frm='beginning of buffer', to='end of buffer'):
        """
        Returns the contents of the buffer between the two locations given in
        parameter. Modifying the returned value has no effect on the buffer.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`
        :rtype: string
        """
        pass  # implemented in Ada

    def get_mark(self, name):
        """
        Checks whether there is a mark with that name in the buffer, and return
        it. An exception is raised if there is no such mark.

        :param string name: A string
        :rtype: :class:`GPS.EditorMark`

        .. seealso:: :func:`GPS.EditorLocation.create_mark`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = ed.at(4, 5)
           mark = loc.create_mark("name")
           mark.data = "whatever"

           # .. anything else

           mark = ed.get_mark("name")
           # mark.data is still "whatever"
        """
        pass  # implemented in Ada

    @staticmethod
    def get_new():
        """
        Opens a new editor on a blank file. This file has no name, and you will
        have to provide one when you save it.

        :rtype: :class:`EditorBuffer`
        """
        pass  # implemented in Ada

    def indent(self, frm='beginning of buffer', to='end of buffer'):
        """
        Recomputes the indentation of the given range of text. This feature
        is language-dependent.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def _insert_at_location(self, location, text):
        """
        Inserts some text in the buffer.

        :param EditorLocation location: An instance of :class:`EditorLocation`
        :param string text: A string
        """
        pass  # implemented in Ada

    def is_modified(self):
        """
        Tests whether the buffer has been modified since it was last opened
        or saved.

        :rtype: bool
        """
        pass  # implemented in Ada

    def is_read_only(self):
        """
        Whether the buffer is editable or not.

        :rtype: bool

        .. seealso:: :func:`GPS.EditorBuffer.set_read_only`
        """
        pass  # implemented in Ada

    def lines_count(self):
        """
        Returns the total number of lines in the buffer.

        :rtype: int
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of all editors that are currently open in GPS.

        :return: A list of instances of :class:`GPS.EditorBuffer`
        :rtype: [EditorBuffer]

        .. code-block:: python

           # It is possible to close all editors at once using a command like

           for ed in GPS.EditorBuffer.list():
               ed.close()
        """
        pass  # implemented in Ada

    def paste(self, location):
        """
        Pastes the contents of the clipboard at the given location in the
        buffer.

        :param EditorLocation location: An instance of :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def redo(self):
        """
        Redoes the last undone command in the editor.
        """
        pass  # implemented in Ada

    def refill(self, frm='beginning of buffer', to='end of buffer'):
        """
        Refills the given range of text, i.e., cuts long lines if necessary
        so that they fit in the limit specified in the GPS preferences.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`

        """
        pass  # implemented in Ada

    def remove_overlay(self, overlay, frm='begining of buffer',
                       to='end of buffer'):
        """
        Removes all instances of the overlay in the given range of text. It is
        not an error if the overlay is not applied to any of the character in
        the range, it just has no effect in that case.

        :param EditorOverlay overlay: An instance of :class:`EditorOverlay`
        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`

        .. seealso:: :func:`GPS.EditorBuffer.apply_overlay`

        """
        pass  # implemented in Ada

    def remove_special_lines(self, mark, lines):
        """
        Removes specified number of special lines at the specified mark. It
        does not delete the mark.

        :param EditorMark mark: An instance of :class:`EditorMark`
        :param int lines: An integer
        """
        pass  # implemented in Ada

    def save(self, interactive=True, file='Same file as edited by the buffer'):
        """
        Saves the buffer to the given file. If ``interactive`` is true, a
        dialog is open to ask for confirmation from the user first, which
        gives him a chance to cancel the saving. ``interactive`` is ignored if
        ``file`` is specified.

        :param bool interactive: A boolean
        :param File file: An instance of :class:`File`
        """
        pass  # implemented in Ada

    def select(self, frm='beginning of buffer', to='end of buffer'):
        """
        Selects an area in the buffer. The boundaries are included in the
        selection. The order of the boundaries is irrelevant, but the cursor
        is be left on ``to``.

        :param EditorLocation frm: An instance of :class:`EditorLocation`
        :param EditorLocation to: An instance of :class:`EditorLocation`

        """
        pass  # implemented in Ada

    def selection_end(self):
        """
        Returns the character after the end of the selection. This is always
        located after the start of the selection, no matter what the order of
        parameters given to :func:`GPS.EditorBuffer.select` is. If the
        selection is empty, :func:`EditorBuffer.selection_start` and
        :func:`EditorBuffer.selection_end` will be equal.

        :rtype: :class:`EditorLocation`

        .. code-block:: python

           # To get the contents of the current selection, one would use:

           buffer = GPS.EditorBuffer.get()
           selection = buffer.get_chars(
               buffer.selection_start(), buffer.selection_end() - 1)

        """
        pass  # implemented in Ada

    def selection_start(self):
        """
        Returns the start of the selection. This is always located before the
        end of the selection, no matter what the order of parameters passed
        to :func:`GPS.EditorBuffer.select` is.

        :rtype: :class:`EditorLocation`

        """
        pass  # implemented in Ada

    def set_lang(self, lang):
        """
        Set the highlighting programming language. When you open an existing
        file, GPS automatically computes the best highlighting language based
        on file extensions and naming schemes defined in your project, or on
        the language that was set manually via the Properties contextual menu.

        This function can be used to override this, or set it for newly
        created files (:func:`GPS.EditorBuffer.get_new`)

        .. seealso:: :func:`GPS.EditorBuffer.get_lang`

        """

    def get_lang(self):
        """
        Return the name of the programming language used for this editor,
        in particular for syntax highlighting and auto indentation.

        :return: a :class:`GPS.LanguageInfo` instance.

        ..  seealso:: :func:`GPS.EditorBuffer.set_lang`

        """
        pass  # implemented in Ada

    def set_read_only(self, read_only=True):
        """
        Indicates whether the user should be able to edit the buffer
        interactively (through any view).

        :param bool read_only: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.is_read_only`

        """
        pass  # implemented in Ada

    def undo(self):
        """
        Undoes the last command in the editor.
        """
        pass  # implemented in Ada

    def unselect(self):
        """
        Cancels the current selection in the buffer.
        """
        pass  # implemented in Ada

    def views(self):
        """
        Returns the list of all views currently editing the buffer. There is
        always at least one such view. When the last view is destroyed, the
        buffer itself is destroyed.

        :return: A list of :class:`EditorView` instances
        :rtype: list[:class:`EditorView`]

        """
        pass  # implemented in Ada

    def has_slave_cursors(self):
        """
        Returns true if there are any alive slave cursors in the buffer
        currently.

        :rtype: bool
        """

    def add_cursor(self, location):
        """
        Adds a new slave cursor at the given location.

        :rtype: The resulting :class:`Cursor` instance
        """

    def delete_cursor(self, location):
        """
        Deletes a slave cursor at the given location.
        """
        pass  # implemented in Ada

    def main_cursor(self):
        """
        Returns the main cursor. Generally you should not use this method
        except if you have a really good reason to perform actions only on the
        main cursor. Instead, you should iterate on the result of
        :meth:`EditorBuffer.cursors`.

        :return: A :class:`Cursor` instance
        :rtype: Cursor
        """

    def cursors(self):
        """
        Returns a list of :class:`Cursor` instances. This method returns a
        generator that automatically handles the calls to
        :func:`set_manual_sync` for each cursor and the call to
        :func:`update_cursors_selection` at the end.

        :rtype: list[:class:`Cursor`]

        .. code-block:: python

            # To perform action on every cursors of the current editor
            # This will move every cursor forward 1 char

            ed = GPS.EditorBuffer.get()
            for cursor in ed.cursors():
                cursor.move(c.mark().location().forward_char())

        """

    def get_cursors(self):
        """
        Returns a list of :class:`Cursor` instances.  Note that if you
        intend to perform actions with tgnores __hem (in particular
        deletions/insertions), you should call set_manual_sync,
        on the cursor's instance.  Also, if you move any selection
        mark, you should call update_cursors_selection afterwards.

        There is a higher level method, :meth:`EditorBuffer.cursors` that
        returns a generator that will handle this manual work for you.

        :rtype: list[:class:`Cursor`]
        """

    def remove_all_slave_cursors(self):
        """
        Removes all active slave cursors from the buffer.
        """

    def set_cursors_auto_sync(self):
        """
        Sets the buffer in auto sync mode regarding multi cursors.  This
        means that any insertion/deletion will be propagated in a 'naive' way
        on all multi cursors. Cursor movements will not be propagated.
        """

    def update_cursors_selection(self):
        """
        Updates the overlay used to show the multi cursor's current
        selection.  This must be called after any operation on multi cursor
        selection marks
        """


###########################################################
# EditorHighlighter
###########################################################

class EditorHighlighter(object):

    """
    This class can be used to transform source editor text into hyperlinks when
    the Control key is pressed.  Two actions can then be associated with this
    hyperlink: clicking with the left mouse button on the hyperlink triggers
    the primary action, and clicking with the middle mouse button on the
    hyperlink triggers the alternate action.
    """

    def __init__(self, pattern, action, index=0, secondary_action=None):
        """
        Register a highlighter. The action is a Python function that takes a
        string as a parameter: the string being passed is the section of text
        which is highlighted.

        :param pattern: A regular expression representing the patterns on which
           we want to create hyperlinks.
        :param action: The primary action for this hyperlink
        :param index: This indicate the number of the
           parenthesized group in pattern that needs to be highlighted.
        :param secondary_action: The alternate action for this hyperlink

        .. code-block:: python

           # Define an action
           def view_html(url):
              GPS.HTML.browse (url)

           def wget_url(url):
              def on_exit_cb(self, code, output):
                 GPS.Editor.edit (GPS.dump (output))
                 p=GPS.Process("wget %s -O -" % url, on_exit=on_exit_cb)

           # Register a highlighter to launch a browser on any URL
           #  left-clicking on an URL will open the default browser to
           #  this URL middle-clicking will call "wget" to get the
           #  source of this URL and open the output in a new editor

           h=GPS.EditorHighlighter ("http(s)?://[^\s:,]*", view_html,
                                    0, wget_url)

           # Remove the highlighter
           h.remove()

    """
    pass  # implemented in Ada

    def remove(self):
        """
        Unregister the highlighter. This cannot be called while the
        hyper-mode is active.
        """
        pass  # implemented in Ada


###########################################################
# EditorLocation
###########################################################

class EditorLocation(object):

    """
    This class represents a location in a specific editor buffer. This location
    is not updated when the buffer changes, but will keep pointing to the same
    line/column even if new lines are added in the buffer. This location is no
    longer valid when the buffer itself is destroyed, and the use of any of
    these subprograms will raise an exception.

    .. seealso:: :func:`GPS.EditorMark`
    """

    def __add__(self, count):
        """
        Returns a new location located ``count`` characters after
        self. If ``count`` is negative, this moves backward in the
        buffer. It is more conveniently used through the standard +
        operator in Python.

        :param count: An integer
        :return: An instance of :class:`GPS.EditorLocation`

        .. seealso::

            :func:`GPS.EditorLocation.__sub__`

            :func:`GPS.EditorLocation.forward_char`

        .. code-block:: python

            ed   = GPS.EditorBuffer.get(GPS.File("a.adb"))
            loc  = ed.at(line=4, column=5)
            loc2 = loc + 3
        """
        pass  # implemented in Ada

    def __cmp__(self, location):
        """
        Internal subprogram used to implement the comparison of two
        locations. It returns -1, 0, or 1 depending on whether the first
        location is before, equal or after the second one. This is more
        conveniently used through the usual <, == and > operators in most
        languages.

        :param location: An instance of :class:`GPS.EditorLocation`
        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, buffer, line, column):
        """
        Initializes a new instance. Creating two instances at the same
        location will not return the same instance of
        :class:`GPS.EditorLocation`, and therefore any user data you have
        stored in the location will not be available in the second instance.

        :param buffer: The instance of GPS.EditorBuffer
        :param line: An integer
        :param column: An integer

        .. code-block:: python

           ed  = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = ed.at(line=4, column=5)
           loc.data = "MY OWN DATA"
           loc2 = ed.at(line=4, column=5)
           # loc2.data is not defined at this point

        """
        pass  # implemented in Ada

    def __sub__(self, count):
        """
        Returns a new location located ``count`` characters before ``self``,
        if ``count`` is an integer. If ``count`` is negative, moves forward
        instead. If ``count`` is another location, it returns the number of
        characters between the two locations. This function is more
        conveniently used through the standard "-" operator in Python.

        :param count: An integer or another instance of
           :class:`GPS.EditorLocation`
        :return: A new instance of :class:`GPS.EditorLocation`

        .. seealso::

            :func:`GPS.EditorLocation.__add__`

            :func:`GPS.EditorLocation.forward_char`
        """
        pass  # implemented in Ada

    def backward_overlay(self, overlay=None):
        """
        Same as :func:`GPS.EditorLocation.forward_overlay`, but moves
        backward instead. If there are no more changes, the location is left
        at the beginning of the buffer.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def beginning_of_line(self):
        """
        Returns a location at the beginning of the line on which ``self`` is.

        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_end(self):
        """
        Returns the location of the end of the current block.

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_end_line(self):
        """
        Returns the last line of the block surrounding the location. The
        definition of a block depends on the specific language of the source
        file.

        :return: An integer
        """
        pass  # implemented in Ada

    def block_fold(self):
        """
        Folds the block containing the location, i.e., makes it invisible on
        the screen, except for its first line. Clicking on the icon next to
        this first line unfolds the block ands make it visible to the user.

        .. seealso:: :func:`GPS.EditorLocation.block_unfold`
        """
        pass  # implemented in Ada

    def block_level(self):
        """
        Returns the nesting level of the block surrounding the location. The
        definition of a block depends on the specific programming language.

        :return: An integer
        """
        pass  # implemented in Ada

    def block_name(self):
        """
        Returns the name of the bock surrounding the location. The definition
        of a block depends on the specific language of the source file.

        :return: A string
        """
        pass  # implemented in Ada

    def block_start(self):
        """
        Returns the location of the beginning of the current block.

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_start_line(self):
        """
        Returns the first line of the block surrounding the location. The
        definition of a block depends on the programming language.

        :return: An integer
        """
        pass  # implemented in Ada

    def block_type(self):
        """
        Returns the type of the block surrounding the location. This type
        indicates whether the block is, e.g., subprogram, an if statement, etc.

        :return: A string
        """
        pass  # implemented in Ada

    def block_unfold(self):
        """
        Unfolds the block containing the location, i.e., makes visible any
        information that was hidden as a result of running
        :func:`GPS.EditorLocation.block_fold`.

        .. seealso:: :func:`GPS.EditorLocation.block_fold`
        """
        pass  # implemented in Ada

    def buffer(self):
        """
        Returns the buffer in which the location is found.

        :return: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def column(self):
        """
        Returns the column of the location.

        :return: An integer
        """
        pass  # implemented in Ada

    def create_mark(self, name='', left_gravity=True):
        """
        Creates a mark at that location in the buffer. The mark will stay
        permanently at that location, and follows it if the buffer is
        modified. In fact, even if the buffer is closed and then reopened,
        the mark will keep track of the location, but of course not if the
        file is edited outside of GPS.

        :param str name: The name of the mark. If specified, this creates a
           named mark, which can later be retrieved through a call to
           :func:`GPS.EditorBuffer.get_mark`. If a mark with the same name
           already exists, it is moved to the new location and then returned.
        :param bool left_gravity: decides whether the mark is moved towards
           the left or the right when text that contains the mark is deleted,
           or some text is inserted at that location.
        :return: An instance of :class:`GPS.EditorMark`

        .. seealso:: :func:`GPS.EditorBuffer.get_mark`

        .. code-block:: python

           buffer = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = buffer.at(3, 4)
           mark = loc.create_mark()
           buffer.insert(loc, "text")
           loc = mark.location()
           # loc.column() is now 8
        """
        pass  # implemented in Ada

    def end_of_line(self):
        """
        Returns a location located at the end of the line on which self is.

        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def ends_word(self):
        """
        Returns true if self is currently at the end of a word. The definition
        of a word depends on the language used.

        :return: A boolean
        """
        pass  # implemented in Ada

    def forward_char(self, count):
        """
        Returns a new location located ``count`` characters after self. If
        ``count`` is negative, the location is moved backward instead.

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def forward_line(self, count):
        """
        Returns a new location located ``count`` lines after self. The
        location is moved back to the beginning of the line. If self is on
        the last line, the beginning of the last line is returned.

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def forward_overlay(self, overlay=''):
        """
        Moves to the next change in the list of overlays applying to the
        character. If ``overlay`` is specified, go to the next change for
        this specific overlay (i.e., the next beginning or end of range where
        it applies). If there are no more changes, the location is left at
        the end of the buffer.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: An instance of :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.EditorLocation.backward_overlay`
        """
        pass  # implemented in Ada

    def forward_word(self, count):
        """
        Returns a new location located ``count`` words after self. If
        ``count`` is negative, the location is moved backward instead. The
        definition of a word depends on the language.

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def get_char(self):
        """
        Returns the character at that location in the buffer. An exception is
        raised when trying to read past the end of the buffer. The character
        might be encoded in several bytes since it is a UTF8 string.

        :return: A UTF8 string

        .. code-block:: python

           char = buffer.beginning_of_buffer().get_char()
           GPS.Console().write (char)  ## Prints the character
           # To manipulate in python, convert the string to a unicode string:
           unicode = char.decode("utf-8")
        """
        pass  # implemented in Ada

    def get_overlays(self):
        """
        Returns the list of all overlays that apply at this specific
        location. The color and font of the text is composed through the
        contents of these overlays.

        :return: A list of :class:`GPS.EditorOverlay` instances

        """
        pass  # implemented in Ada

    def has_overlay(self, overlay):
        """
        Returns True if the given overlay applies to the character at that
        location.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: A boolean
        """
        pass  # implemented in Ada

    def inside_word(self):
        """
        Returns true if self is currently inside a word. The definition of a
        word depends on the language.

        :return: A boolean
        """
        pass  # implemented in Ada

    def line(self):
        """
        Returns the line number of the location.

        :return: An integer
        """
        pass  # implemented in Ada

    def offset(self):
        """
        Returns the offset of the location in the buffer, i.e., the number of
        characters from the beginning of the buffer to the location.

        :return: An integer
        """
        pass  # implemented in Ada

    def search(self, pattern, backward=False, case_sensitive=False,
               regexp=False, whole_word=False, scope='Whole',
               dialog_on_failure=True):
        """
        Searches for the next occurrence of ``pattern`` in the editor,
        starting at the given location. If there is such a match, this
        function returns the two locations for the beginning of the match and
        the end of the match. Typically, these would be used to highlight the
        match in the editor.

        When no match is found, this function returns null. Additionally, if
        ``dialog_on_failure`` is true, a dialog is displayed to the user
        asking whether the search should restart at the beginning of the
        buffer.

        :param pattern: A string
        :param backward: A boolean
        :param case_sensitive: A boolean
        :param regexp: A boolean
        :param whole_word: A boolean
        :param scope: A string
        :param dialog_on_failure: A boolean
        :return: A list of two :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.File.search`
        """
        pass  # implemented in Ada

    def starts_word(self):
        """
        Returns true if self is currently at the start of a word. The
        definition of a word depends on the language.

        :return: A boolean
        """
        pass  # implemented in Ada

    def subprogram_name(self):
        """
        Returns the name of the subprogram containing the location.

        :return: A string
        """
        pass  # implemented in Ada


###########################################################
# EditorMark
###########################################################

class EditorMark(object):

    """
    This class represents a specific location in an open editor. As opposed
    to the :class:`GPS.EditorLocation` class, the exact location is updated
    whenever the buffer is modified. For example, if you add a line before
    the mark, then the mark is moved one line forward as well, so that it
    still points to the same character in the buffer.

    The mark remains valid even if you close the buffer; or if you reopen it
    and modify it. It will always point to the same location in the file, while
    you have kept the Python object.

    :func:`GPS.EditorLocation.create_mark` allows you to create named marks
    which you can then retrieve through
    :func:`GPS.EditorBuffer.get_mark`. Such named marks are only valid while
    the editor exists. As soon as you close the editor, you can no longer use
    get_mark to retrieve it (but the mark is still valid if you have kept a
    python object referencing it).

    .. seealso:: :func:`GPS.EditorLocation`

    """

    file = None
    """
    Read only property that gives the location of the mark.
    :type: :class:`GPS.File`
    """

    line = 0
    """
    Read only property that gives the location of the mark.
    :type: int
    """

    column = 0
    """
    Read only property that gives the location of the mark.
    :type: int
    """

    def __del__(self):
        """
        This subprogram is automatically called whenever self is unreferenced
        by Python, and destroys the physical mark in the buffer if the mark
        is unnamed, since there is no way to access it anyway afterward.
        """
        pass  # implemented in Ada

    def __init__(self):
        """
        Always raises an exception, thus preventing the direct creation of a
        mark. Instead, you should use :func:`GPS.EditorLocation.create_mark`
        to create such a mark.
        """
        pass  # implemented in Ada

    def delete(self):
        """
        Deletes the physical mark from the buffer. All instances referencing
        the same mark will no longer be valid. If you have not given a name to
        the mark in the call to :func:`GPS.EditorLocation.create_mark`, it
        will automatically be destroyed when the last instance referencing it
        goes out of scope. Therefore, calling :func:`delete` is not mandatory
        in the case of unnamed marks, although it is still recommended.
        """
        pass  # implemented in Ada

    def is_present(self):
        """
        Returns True if mark's location is still present in the buffer.
        """
        pass  # implemented in Ada

    def location(self):
        """
        Returns the current location of the mark. This location will vary
        depending on the changes that take place in the buffer.
        Calling this function will open the corresponding source editor.

        :return: An instance of :class:`GPS.EditorLocation`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = ed.at(3, 5)
           mark = loc.create_mark()
           # ...
           loc = mark.location()
        """
        pass  # implemented in Ada

    def move(self, location):
        """
        Moves the mark to a new location in the buffer. This is slightly less
        expensive than destroying the mark and creating a new one through
        :func:`GPS.EditorLocation.create_mark`, although the result is the
        same.

        :param location: An instance of :class:`GPS.EditorLocation`

        """
        pass  # implemented in Ada


###########################################################
# EditorOverlay
###########################################################

class EditorOverlay(object):

    """
    This class represents properties that can be applied to one or more ranges
    of text. This can be used to change the display properties of the text
    (colors, fonts,...) or store any user-specific attributes that can be
    retrieved later. GPS itself uses overlays to do syntax highlighting. If two
    or more overlays are applied to the same range of text, the final colors
    and fonts of the text depends on the priorities of these overlays and the
    order in which they were applied to the buffer.

    This class is fairly low-level; we recommend using the class
    :py:class:`gps_utils.highlighter.OverlayStyle` instead. That class
    provides similar support for specifying attributes, but makes it easier
    to highlight sections of an editor with that style, or to remove the
    highlighting.

    In fact, if your goal is to highlight parts of editors, it might be
    simpler to use :py:class:`gps_utils.highilghter.Background_Highlighter`
    or one of the classes derived from it. These classes provide convenient
    support for highlighting editors in the background, i.e. without
    interfering with the user or slowing things down.
    """

    def __init__(self):
        """
        This subprogram is used to prevent the direct creation of
        overlays. Overlays need to be created through
        :func:`GPS.EditorBuffer.create_overlay`.

        .. seealso:: :func:`GPS.EditorBuffer.create_overlay`
        """
        pass  # implemented in Ada

    def get_property(self, name):
        """
        Retrieves one of the predefined properties of the overlay. This list
        of these properties is described for
        :func:`GPS.EditorOverlay.set_property`.

        :param name: A string
        :return: A string or a boolean, depending on the property
        """
        pass  # implemented in Ada

    def name(self):
        """
        Returns the name associated with this overlay, as given to
        :func:`GPS.EditorBuffer.create_overlay`.

        :return: A string

        .. seealso:: :func:`GPS.EditorBuffer.create_overlay`
        """
        pass  # implemented in Ada

    def set_property(self, name, value):
        """
        Changes some of the predefined properties of the overlay. These are
        mostly used to change the visual rendering of the text. The following
        attribute names are currently recognized:

        - *foreground* (value is a string with the color name)

          Changes the foreground color of the text.

        - *background* (value is a string with the color name)

          Changes the background color of the text.

        - *paragraph-background* (value is a string with the color name)

          Changes the background color of entire lines.  Contrary to the
          "background" property, this highlights the entire line, including the
          space after the end of the text, regardless of which characters are
          actually covered by the overlay.

        - *font* (value is a string with the font name)

          Changes the text font.

        - *weight* (value is a string, one of "light", "normal" and "bold")

        - *style* (value is a string, one of "normal", "oblique" and "italic")

        - *editable* (value is a boolean)

          Indicates whether this range of text is editable.

        - *variant* (one of 0 ("normal") or 1 ("small_caps"))

        - *stretch* (from 0 ("ultra-condensed") to 8 ("ultra-expanded"))

        - *underline* (one of 0 ("none"), 1 ("single"), 2 ("double"), 3
           ("low"))

        - *size-points* (an integer)

          Font size in points.

        - *rise* (an integer)

          Offset of text above the baseline (below the baseline if rise is
          negative), in Pango units.

        - *pixels-above-lines* (an integer)

          Pixels of blank space above paragraphs.

        - *pixels-below-lines* (an integer)

          Pixels of blank space below paragraphs.

        - *pixels-inside-wrap* (an integer)

          Pixels of blank space between wrapped lines in a paragraph.

        - *invisible* (a boolean)

          Whether this text is hidden.

        - *strikethrough* (a boolean)

          Whether to strike through the text.

        - *background-full-height* (a boolean)

          Whether the background color fills the entire line height or only
          the height of the tagged characters.

          The set of predefined attributes is fixed. However, overlays are
          especially useful to store your own user data in the usual Python
          manner, which you can retrieve later. This can be used to mark
          specially specific ranges of text which you want to be able to find
          easily later on, even if the buffer has been modified since then
          (see :func:`GPS.EditorLocation.forward_overlay`).

        :param name: A string
        :param value: A string or a boolean, depending on the property
        """
        pass  # implemented in Ada


###########################################################
# EditorView
###########################################################

class EditorView(GUI):

    """
    One view of an editor, i.e., the visible part through which users can
    modify text files. A given :class:`GPS.EditorBuffer` can be associated
    with multiple views. Closing the last view associated with a buffer will
    also close the buffer.

    .. code-block:: python

       # To get a handle on the current editor, use the following code:
       view = GPS.EditorBuffer.get().current_view()
    """

    def __init__(self, buffer):
        """
        Called implicitly whenever you create a new view. It creates a new
        view for the given buffer, and is automatically inserted into the GPS
        MDI.

        :param EditorBuffer buffer: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def buffer(self):
        """
        Returns the buffer to which the view is attached. Editing the text of
        the file should be done through this instance.

        :rtype: :class:`EditorBuffer`
        """
        pass  # implemented in Ada

    def center(self, location='location of cursor'):
        """
        Scrolls the view so that the location is centered.

        :param EditorLocation location: An instance of :class:`GPS
            .EditorLocation`
        """
        pass  # implemented in Ada

    def cursor(self):
        """
        Returns the current location of the cursor in this view.

        :rtype: :class:`EditorLocation`
        """
        pass  # implemented in Ada

    def goto(self, location, extend_selection=False):
        """
        Moves the cursor to the given location. Each view of a particular
        buffer has its own cursor position, which is where characters typed
        by the user will be inserted. If extend_selection is True, extend the
        selection from the current bound to the new location.

        :param EditorLocation location: An instance of :class:`GPS
            .EditorLocation`
        :param bool extend_selection: A Boolean
        """
        pass  # implemented in Ada

    def is_read_only(self):
        """
        Whether the view is editable or not. This property is shared by all
        views of the same buffer.

        :rtype: bool

        .. seealso:: :func:`GPS.EditorBuffer.is_read_only`
        """
        pass  # implemented in Ada

    def set_read_only(self, read_only=True):
        """
        Indicates whether the user should be able to edit interactively through
        this view. Setting a view Writable/Read Only will also modify the
        status of the other views of the same buffer.

        :param bool read_only: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.get_read_only`
        """
        pass  # implemented in Ada

    def title(self, short=False):
        """
        Returns the view's title; the short title is returned if ``short`` is
        set to True.

        :param bool short: A boolean
        """
        pass  # implemented in Ada


###########################################################
# Entity
###########################################################

class Entity(object):

    """
    Represents an entity from the source, based on the location of its
    declaration.

    .. seealso:: :func:`GPS.Entity.__init__`
    """

    def __cmp__(self, file):
        """
        Compares two instances of :class:`GPS.Entity` and returns -1, 0 or 1
        depending on their relative sort order.

        :param file: An instance of :class:`GPS.Entity`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Returns a hash value suitable for storing self in a dictionary.

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name, file=None, line=-1, column=-1,
                 approximate_search_fallback=True):
        """
        Initializes a new instance of the :class:`Entity` class from any
        reference to the entity. The ``file`` parameter should only be
        omitted for a predefined entity of the language. This will only work
        for languages for which a cross-reference engine has been defined

        :param name: A string, the name of the entity
        :param file: An instance of :class:`GPS.File` in which the entity
           is referenced
        :param line: An integer, the line at which the entity is referenced
        :param column: An integer, the column at which the entity is referenced
        :param approximate_search_fallback: If True, when the line and column
           are not exact, this parameter will trigger approximate search in the
           database (eg. see if there are similar entities in the surrounding
           lines)

        >>> GPS.Entity("foo", GPS.File("a.adb"),
                       10, 23).declaration().file().name()
        => will return the full path name of the file in which the entity
           "foo", referenced in a.adb at line 10, column 23, is defined.
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def attributes(self):
        """
        Returns various boolean attributes of the entity: is the entity global,
        static, etc.

        :return: A htable with the following keys:
            - 'global': whether the entity is a global entity
            - 'static': whether the entity is a local static variable (C/C++)
            - 'in': for an in parameter for an Ada subprogram
            - 'out': for an out parameter for an Ada subprogram
            - 'inout': for an in-out parameter for an Ada subprogram
            - 'access': for an access parameter for an Ada subprogram
        """
        pass  # implemented in Ada

    def body(self, nth='1'):
        """
        Returns the location at which the implementation of the entity is
        found. For Ada subprograms and packages, this corresponds to the body
        of the entity. For Ada private types, this is the location of the full
        declaration for the type. For entities which do not have a notion of
        body, this returns the location of the declaration for the entity.
        Some entities have several bodies. This is for instance the case of a
        separate subprogram in Ada, where the first body just indicates the
        subprogram is separate, and the second body provides the actual
        implementation. The nth parameter gives access to the other bodies. An
        exception is raised when there are not at least nth bodies.

        :param nth: An integer
        :return: An instance of :class:`GPS.FileLocation`

        .. code-block:: python

           entity = GPS.Entity("bar", GPS.File("a.adb"), 10, 23)
           body = entity.body()
           print "The subprogram bar's implementation is found at " \
               + body.file.name() + ':' + body.line() + ':' + body.column()

        """
        pass  # implemented in Ada

    def called_by(self, dispatching_calls=False):
        """
        Displays the list of entities that call the entity. The returned
        value is a dictionary whose keys are instances of :class:`Entity`
        calling this entity, and whose value is a list of
        :class:`FileLocation` instances where the entity is referenced.
        If dispatching_calls is true, then calls to self that might occur
        through dispatching are also listed.

        :param dispatching_calls: A boolean
        :return: A dictionary, see below
        """
        pass  # implemented in Ada

    def called_by_browser(self):
        """
        Opens the call graph browser to show what entities call self.
        """
        pass  # implemented in Ada

    def calls(self, dispatching_calls=False):
        """
        Displays the list of entities called by the entity. The returned
        value is a dictionary whose keys are instances of :class:`Entity`
        called by this entity, and whose value is a list of
        :class:`FileLocation` instances where the entity is referenced.  If
        ``dispatching_calls`` is true, calls done through dispatching will
        result in multiple entities being listed (i.e., all the possible
        subprograms that are called at that location).

        :param dispatching_calls: A boolean
        :return: A dictionary, see below

        .. seealso:: :func:`GPS.Entity.is_called_by()`
        """
        pass  # implemented in Ada

    def category(self):
        """
        Returns the category of a given entity. Possible values include:
        label, literal, object, subprogram, package, namespace, type, and
        unknown.  The exact list of strings is not hard-coded in GPS and
        depends on the programming language of the corresponding source.

        See instead :func:`is_access`, :func:`is_array`,
        :func:`is_subprogram`, etc.

        :return: A string
        """
        pass  # implemented in Ada

    def child_types(self):
        """
        Return the list of entities that extend self (in the object-oriented
        sense)

        :return: a list of :class:`GPS.Entity`
        """

    def get_called_entities(self):
        """
        Return the list of entities referenced within the scope of
        self.

        :return: a list of :class:`GPS.Entity`
        """

    def has_body(self, nth='1'):
        """
        Whether the entity has a body.

        :return: A boolean

        .. seealso:: :func:`GPS.Entity.body`
        """

    def instance_of(self):
        """
        If self is an instantiation of some other generic entity, this
        returns that entity. For instance, if the Ada code contains::

             procedure Foo is new Generic_Proc (Integer);

        and `e` is an instance of :class:`GPS.Entity` for Foo, then
        `e.instance_of()` returns an entity for Generic_Proc.

        :return: an instance of :class:`GPS.Entity` or None
        """

    def is_subprogram(self):
        """
        Whether the entity is a subprogram, procedure or function.

        :return: A boolean
        """
        pass

    def is_generic(self):
        """
        Whether the entity is a generic.

        :return: A boolean
        """
        pass

    def is_global(self):
        """
        Whether self is a global entity.

        :return: A boolean
        """
        pass

    def is_access(self):
        """
        Whether self is a pointer or access (variable or type)

        :return: A boolean
        """
        pass

    def is_array(self):
        """
        Whether self is an array type or variable.

        :return: A boolean
        """
        pass

    def is_type(self):
        """
        Whether self is a type declaration (as opposed to a variable).

        :return: A boolean
        """
        pass

    def is_container(self):
        """
        Whether self contains other entities (such as a package or a record).

        :return: A boolean
        """
        pass

    def is_predefined(self):
        """
        Whether self is a predefined entity, i.e. an entity for which there
        is no explicit declaration (like an 'int' in C or an 'Integer' in Ada).

        :return: A boolean
        """
        pass  # implemented in Ada

    def declaration(self):
        """
        Returns the location of the declaration for the entity. The file's name
        is is "<predefined>" for predefined entities.

        :return: An instance of :class:`GPS.FileLocation` where the entity is
           declared

        .. code-block:: python

           entity=GPS.Entity("integer")
           if entity.declaration().file().name() == "<predefined>":
              print "This is a predefined entity"

        """
        pass  # implemented in Ada

    def derived_types(self):
        """
        Returns a list of all the entities that are derived from self. For
        object-oriented languages, this includes types that extend self. In
        Ada, this also includes subtypes of self.

        :return: A list of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def discriminants(self):
        """
        Returns the list of discriminants for entity. This is a list of
        entities, empty if the type has no discriminant or if this notion
        does not apply to the language.

        :return: A list of instances of :class:`GPS.Entity`

        """
        pass  # implemented in Ada

    def documentation(self, extended=False):
        """
        Returns the documentation for the entity. This is the comment block
        found just before or just after the declaration of the entity (if any
        such block exists). This is also the documentation string displayed
        in the tooltips when you leave the mouse cursor over an entity for a
        while. If ``extended`` is true, the returned documentation includes
        formatting and the full entity description.

        :param extended: A boolean
        :return: A string
        """
        pass  # implemented in Ada

    def end_of_scope(self):
        """
        Returns the location at which the end of the entity is found.

        :return: An instance of :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def fields(self):
        """
        Returns the list of fields for the entity. This is a list of
        entities. This applies to Ada record and tagged types, or C structs
        for instance.

        In older versions of GPS, this used to return the literals for
        enumeration types, but these should now be queried through
        :func:`self.literals` instead.

        :return: A list of instances of :class:`GPS.Entity`

        """
        pass  # implemented in Ada

    def literals(self):
        """
        Returns the list of literals for an enumeration type.

        :return: A list of instances of :class:`GPS.Entity`
        """
        pass

    def find_all_refs(self, include_implicit=False):
        """
        Displays in the :guilabel:`Locations` view all the references to the
        entity. If ``include_implicit`` is true, implicit uses of the entity
        are also referenced, for example when the entity appears as an
        implicit parameter to a generic instantiation in Ada.

        :param include_implicit: A boolean

        .. seealso:: :func:`GPS.Entity.references()`
        """
        pass  # implemented in Ada

    def full_name(self):
        """
        Returns the full name of the entity that it to say the name of the
        entity prefixed with its callers and parent packages names. The
        casing of the name has been normalized to lower-cases for
        case-insensitive languages.

        :return: A string, the full name of the entity
        """
        pass  # implemented in Ada

    def methods(self, include_inherited=False):
        """
        Returns the list of primitive operations (aka methods) for self. This
        list is not sorted.

        :param include_inherited: A boolean
        :return: A list of instances of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def name(self):
        """
        Returns the name of the entity. The casing of the name has been
        normalized to lower-cases for case-insensitive languages.

        :return: A string, the name of the entity
        """
        pass  # implemented in Ada

    def name_parameters(self, location):
        """
        Refactors the code at the location, to add named parameters. This
        only work if the language has support for such parameters, namely Ada
        for now.

        :param location: An instance of :class:`GPS.FileLocation`

        .. code-block:: python

           GPS.Entity("foo", GPS.File("decl.ads")).rename_parameters(
               GPS.FileLocation(GPS.File("file.adb"), 23, 34))
        """
        pass  # implemented in Ada

    def overrides(self):
        """
        Returns the entity that self overrides.

        :rtype: :class:`GPS.Entity`
        """
        pass

    def parameters(self):
        """
        Returns the list of parameters for entity. This is a list of
        entities. This applies to subprograms.

        :return: A list of instances of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def parent_types(self):
        """
        Returns the list of parent types when self is a type. For example,
        if we have the following Ada code::

            type T is new Integer;
            type T1 is new T;

        then the list of parent types for T1 is [T].

        :return: A list of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def pointed_type(self):
        """
        Returns the type pointed to by entity. If self is not a pointer (or
        an Ada access type), None is returned. This function also applies to
        variables, and returns the same information as their type would

        :return: An instance of :class:`GPS.Entity`

        .. code-block:: python

           ## Given the following Ada code:
           ##    type Int is new Integer;
           ##    type Ptr is access Int;
           ##    P : Ptr;
           ## the following requests would apply:

           f = GPS.File("file.adb")
           GPS.Entity("P", f).type()           # Ptr
           GPS.Entity("P", f).pointed_type()   # Int
           GPS.Entity("Ptr", f).pointed_type() # Int
        """
        pass  # implemented in Ada

    def primitive_of(self):
        """
        Returns the list of type for which self is a primitive operation (or a
        method, in other languages than Ada).

        :return: A list of instances of :class:`GPS.Entity` or []

        """
        pass  # implemented in Ada

    def references(self, include_implicit=False, synchronous=True,
                   show_kind=False, in_file=None, kind_in=''):
        """
        Lists all references to the entity in the project sources. If
        ``include_implicit`` is true, implicit uses of the entity are also
        referenced, for example when the entity appears as an implicit
        parameter to a generic instantiation in Ada.

        If ``synchronous`` is True, the result is returned directly,
        otherwise a command is returned and its result is accessible with
        :func:`get_result`. The result, in that case, is either a list of
        locations (if show_kind is False) or a htable indexed by location,
        and whose value is a string indicating the kind of the reference
        (such as declaration, body, label, or end-of-spec).  ``in_file`` can
        be used to limit the search to references in a particular file, which
        is.  ``kind_in`` is a list of comma-separated list of reference kinds
        (as would be returned when show_kind is True). Only such references
        are returned, as opposed to all references.

        :param include_implicit: A boolean
        :param synchronous: A boolean
        :param show_kind: A boolean
        :param in_file: An instance of :class:`GPS.File`
        :param kind_in: A string
        :return: A list of :class:`GPS.FileLocation`, :class:`htable`,
           or :class:`GPS.Command`

        .. seealso:: :func:`GPS.Entity.find_all_refs()`

        .. code-block:: python

           for r in GPS.Entity("GPS", GPS.File("gps.adb")).references():
               print "One reference in " + r.file().name()
        """
        pass  # implemented in Ada

    def rename(self, name, include_overriding=True, make_writable=False,
               auto_save=False):
        """
        Renames the entity everywhere in the application. The source files
        should have been compiled first, since this operation relies on the
        cross-reference information which have been generated by the
        compiler. If ``include_overriding`` is true, subprograms that
        override or are overridden by self are also renamed. Likewise, if
        self is a parameter to a subprogram then parameters with the same
        name in overriding or overridden subprograms are also renamed.

        If some renaming should be performed in a read-only file, the
        behavior depends on `make_writable`: if true, the file is made
        writable and the renaming is performed; if false, no renaming is
        performed in that file, and a dialog is displayed asking whether you
        want to do the other renamings.

        The files will be saved automatically if `auto_save` is true,
        otherwise they are left edited but unsaved.

        :param name: A string
        :param include_overriding: A boolean
        :param make_writable: A boolean
        :param auto_save: A boolean
        """
        pass  # implemented in Ada

    def requires_body(self):
        """
        Whether the entity should be completed with a body.

        :return: A boolean

        .. seealso:: :func:`GPS.Entity.body`
        """

    def return_type(self):
        """
        Return the return type for entity. This applies to subprograms.

        :return: An instance of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def show(self):
        """
        Displays in the type browser the informations known about the entity,
        such as the list of fields for records, list of primitive subprograms
        or methods, and list of parameters.
        """
        pass  # implemented in Ada

    def type(self):
        """
        Returns the type of the entity. For a variable, this its type.  This
        function used to return the parent types when self is itself a type,
        but this usage is deprecated and you should be using
        :func:`self.parent_types` instead.

        :return: An instance of :class:`GPS.Entity`
        """
        pass  # implemented in Ada


###########################################################
# Exception
###########################################################

class Exception(exceptions.Exception):

    """
    One of the exceptions that can be raised by GPS. It is a general error
    message, and its semantic depends on what subprogram raised the exception.
    """
    pass  # implemented in Ada


###########################################################
# File
###########################################################

class File(object):

    """
    Represents a source file of your application.

    .. seealso:: :func:`GPS.File.__init__`
    """

    executable_path = None
    """
    Return a :class:`File` instance of the executable associated with this
    file.

    The result may be meaningless if the given :class:`File` is not supposed
    to produce an executable.
    """

    path = ""
    """
    The absolute path name for the current instance of :class:`GPS.File`,
    including directories from the root of the filesystem.
    """

    def __cmp__(self, file):
        """
        Compares two instances of :func:`GPS.File` and returns -1, 0 or 1
        depending on their relative sort order.

        :param file: An instance of :class:`GPS.File`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Returns a hash value suitable for storing self in a dictionary.

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name, local=False):
        """
        Initializes a new instance of the class :class:`File`. This does not
        need to be called explicitly, since GPS calls it automatically when
        you create such an instance. If name is a base file name (no
        directory is specified), GPS attempts to search for this file in the
        list of source directories of the project. If a directory is
        specified, or the base file name was not found in the source
        directories, then the file name is considered as relative to the
        current directory. If ``local`` is "true", the specified file name is
        to be considered as local to the current directory.

        :param name: Name of the file associated with this instance
        :param local: A boolean

        .. seealso:: :func:`GPS.File.name`

        .. code-block:: python

           file=GPS.File("/tmp/work")
           print file.name()
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def check_semantic(self):
        """
        Checks the semantic for current file. This call returns only after
        the check is completed.

        .. seealso::

             :func:`GPS.File.shadow_check_semantic`

             :func:`GPS.File.check_syntax`

             :func:`GPS.File.shadow_check_syntax`
        """
        pass  # implemented in Ada

    def check_syntax(self):
        """
        Checks the syntax for current file. This call returns only after the
        check is completed.

        .. seealso::

             :func:`GPS.File.shadow_check_syntax`

             :func:`GPS.File.shadow_check_semantic`

             :func:`GPS.File.check_semantic`
        """
        pass  # implemented in Ada

    def compile(self, extra_args=''):
        """
        Compiles the current file. This call returns only after the
        compilation is completed. Additional arguments can be added to the
        command line.

        :param extra_args: A string

        .. seealso:: :func:`GPS.File.make`

        .. code-block:: python

           GPS.File("a.adb").compile()
        """
        pass  # implemented in Ada

    def directory(self):
        """
        Returns the directory in which the file is found.

        :return: A string

        .. code-block:: python

           ## Sorting files by TN is easily done with a loop like
           dirs={}
           for s in GPS.Project.root().sources():
             if dirs.has_key (s.directory()):
                dirs[s.directory()].append (s)
             else:
                dirs[s.directory()] = [s]
        """
        pass  # implemented in Ada

    def entities(self, local=True):
        """
        Returns the list of entities that are either referenced (``local`` is
        false) or declared (``local`` is true) in self.

        :param local: A boolean
        :return: A list of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def generate_doc(self):
        """
        Generates the documentation fo the file and displays it in the default
        browsers.

        .. seealso: :func:`GPS.Project.generate_doc`
        """

    def get_property(self, name):
        """
        Returns the value of the property associated with the file. This
        property might have been set in a previous GPS session if it is
        persistent. An exception is raised if no such property already exists
        for the file.

        :param name: A string
        :return: A string

        .. seealso:: :func:`GPS.File.set_property`
        """
        pass  # implemented in Ada

    def imported_by(self, include_implicit=False, include_system=True):
        """
        Returns the list of files that depends on file_name. This command
        might take some time to execute since GPS needs to parse the
        cross-reference information for multiple source files. If
        ``include_implicit`` is true, implicit dependencies are also
        returned. If ``include_system`` is true, dependent system files from
        the compiler runtime are also returned.

        :param include_implicit: A boolean.
           This is now ignored, and only explicit dependencies corresponding
           to actual 'with' or '#include' lines will be returned.
        :param include_system: A boolean
        :return: A list of files

        .. seealso:: :func:`GPS.File.imports`
        """
        pass  # implemented in Ada

    def imports(self, include_implicit=False, include_system=True):
        """
        Returns the list of files that self depends on. If
        ``include_implicit`` is true, implicit dependencies are also
        returned. If ``include_system`` is true, then system files from the
        compiler runtime are also considered.

        :param include_implicit: A boolean
        :param include_system: A boolean
        :return: A list of files

        .. seealso:: :func:`GPS.File.imported_by`
        """
        pass  # implemented in Ada

    def language(self):
        """
        Returns the name of the language this file is written in. This is
        based on the file extension and the naming scheme defined in the
        project files or the XML files. The empty string is returned when the
        language is unknown.

        :return: A string
        """
        pass  # implemented in Ada

    def make(self, extra_args=''):
        """
        Compiles and links the file and all its dependencies. This call
        returns only after the compilation is completed. Additional arguments
        can be added to the command line.

        :param extra_args: A string

        .. seealso:: :func:`GPS.File.compile`
        """
        pass  # implemented in Ada

    def name(self, remote_server='GPS_Server'):
        """
        Returns the name of the file associated with self. This is an absolute
        file name, including directories from the root of the filesystem.

        If ``remote_server`` is set, the function returns the equivalent path
        on the specified server. GPS_Server (default) is always the local
        machine. This argument is currently ignored.

        This function returns the same value as the `self.path` property,
        and the latter might lead to more readable code.

        :param remote_server: A string. Possible values are "GPS_Server"
           (or empty string), "Build_Server", "Debug_Server",
           "Execution_Server" and "Tools_Server".
        :return: A string, the name of the file
        """
        pass  # implemented in Ada

    def other_file(self):
        """
        Returns the name of the other file semantically associated with this
        one. In Ada this is the spec or body of the same package depending on
        the type of this file. In C, this will generally be the :file:`.c` or
        :file:`.h` file with the same base name.

        :return: An instance of :class:`GPS.File`

        .. code-block:: python

           GPS.File("tokens.ads").other_file().name()
           => will print "/full/path/to/tokens.adb" in the context of the
           => project file used for the GPS tutorial.
        """
        pass  # implemented in Ada

    def project(self, default_to_root=True):
        """
        Returns the project to which file belongs. If file is not one of the
        souces of the project, the returned value depends on
        ``default_to_root``: if false, None is returned. Otherwise, the root
        project is returned.

        :param default_to_root: A boolean
        :return: An instance of :class:`GPS.Project`

        .. code-block:: python

           GPS.File("tokens.ads").project().name()
           => will print "/full/path/to/sdc.gpr" in the context of the project
           => file used for the GPS tutorial
        """
        pass  # implemented in Ada

    def references(self, kind="", sortby=0):
        """
        Returns all references (to any entity) within the file. The acceptable
        values for kind can currently be retrieved directly from the
        cross-references database by using a slightly convoluted approach::

               sqlite3 obj/gnatinspect.db
               > select display from reference_kinds;

        :param string kind: this can be used to filter the references, and is
           more efficient than traversing the list afterward. For instance,
           you can get access to the list of dispatching calls by passing
           "dispatching call" for kind. The list of kinds is defined in the
           cross-reference database, and new values can be added at any time.
           See above on how to retrieve the list of possible values.

        :param integer sortby: how the returned list should be sorted.
           0 indicates that they are sorted in the order in which they
           appear in the file; 1 indicates that they are sorted first by
           entity, and then in file order.

        :return: A list of tuples (:class:`GPS.Entity`,
           :class:`GPS.FileLocation`)
        """

    def remove_property(self, name):
        """
        Removes a property associated with a file.

        :param name: A string

        .. seealso:: :func:`GPS.File.set_property`
        """
        pass  # implemented in Ada

    def search(self, pattern, case_sensitive=False,
               regexp=False, scope='whole'):
        """
        Returns the list of matches for pattern in the file. Default values are
        False for case_sensitive and regexp. Scope is a string, and should be
        any of 'whole', 'comments', 'strings', 'code'. The latter will match
        only for text outside of comments.

        :param pattern: A string
        :param case_sensitive: A boolean
        :param regexp: A boolean
        :param scope: One of ("whole", "comments", "strings", "code")
        :return: A list of :class:`GPS.FileLocation` instances

        .. seealso::

           :func:`GPS.EditorLocation.search`

           :func:`GPS.File.search_next`
        """
        pass  # implemented in Ada

    def search_next(self, pattern, case_sensitive=False, regexp=False):
        """
        Returns the next match for pattern in the file. Default values are
        False for case_sensitive and regexp. Scope is a string, and should be
        any of 'whole', 'comments', 'strings', 'code'. The latter will match
        only for text outside of comments.

        :param pattern: A string
        :param case_sensitive: A boolean
        :param regexp: A boolean
        :return: An instance of :class:`GPS.FileLocation`

        .. seealso:: :func:`GPS.File.search_next`
        """
        pass  # implemented in Ada

    def set_property(self, name, value, persistent=False):
        """
        Associates a string property with the file. This property is
        retrievable during the whole GPS session, or across GPS sessions if
        persistent is set to True.

        This is different than setting instance properties through Python's
        standard mechanism in that there is no guarantee that the same
        instance of :class:`GPS.File` will be created for each physical file
        on the disk, and therefore you would not be able to associate a
        property with the physical file itself.

        :param name: A string
        :param value: A string
        :param persistent: A boolean

        .. seealso::

           :func:`GPS.File.get_property`

           :func:`GPS.Project.set_property`
        """
        pass  # implemented in Ada

    def shadow_check_semantic(self):
        """
        Checks the semantic for current file. The current file is not saved,
        but a temporary extension project is created and deleted when
        the compilation ends. This call launches a background process and
        immediately returns.

        .. seealso::

           :func:`GPS.File.check_semantic`

           :func:`GPS.File.check_syntax`

           :func:`GPS.File.shadow_check_syntax`
        """
        pass  # implemented in Ada

    def shadow_check_syntax(self):
        """
        Checks the syntax for current file. The current file is not saved, but
        a temporary extension project is created and deleted when the
        compilation ends. This call launches a background process and
        immediately returns.

        .. seealso::

           :func:`GPS.File.check_syntax`

           :func:`GPS.File.check_semantic`

           :func:`GPS.File.shadow_check_semantic`

        """
        pass  # implemented in Ada

    def unit(self):
        """
        Return the unit name for this file.
        For Ada source files, this is the unit name (i.e. the name of the
        package or the library-level subprogram). For other languages, this
        function always returns the empty string.

        :return: a string
        """

    def used_by(self):
        """
        Displays in the dependency browser the list of files that depends on
        file_name. This command might take some time to execute since GPS
        needs.  to parse the cross-reference information for multiple source
        files

        .. seealso:: :func:`GPS.File.uses`
        """
        pass  # implemented in Ada

    def uses(self):
        """
        Displays in the dependency browser the list of files that file_name
        depends on.

        .. seealso:: :func:`GPS.File.used_by`
        """
        pass  # implemented in Ada


###########################################################
# FileLocation
###########################################################

class FileLocation(object):

    """
    Represents a location in a file.

    .. seealso:: :func:`GPS.FileLocation.__init__`
    """

    def __cmp__(self, file):
        """
        Compares two instances of :class:`GPS.FileLocation` and returns -1, 0
        or 1 depending on their relative sort order.

        :param file: An instance of :class:`GPS.FileLocation`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Returns a hash value suitable for storing self in a dictionary.

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, filename, line, column):
        """
        Initializes a new instance of :class:`GPS.FileLocation`.

        :param filename: An instance of :class:`GPS.File`
        :param line: An integer
        :param column: An integer

        .. code-block:: python

           location = GPS.FileLocation(GPS.File("a.adb"), 1, 2)
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def column(self):
        """
        Returns the column of the location.

        :return: An integer, the column of the location

        .. seealso::

           :func:`GPS.FileLocation.file`

           :func:`GPS.FileLocation.line`
        """
        pass  # implemented in Ada

    def file(self):
        """
        Returns the file of the location.

        :return: An instance of :class:`GPS.File`, the file of the location

        .. seealso::

           :func:`GPS.FileLocation.line`

           :func:`GPS.FileLocation.column`
        """
        pass  # implemented in Ada

    def line(self):
        """
        Returns the line number of the location.

        :return: An integer

        .. seealso::

           :func:`GPS.FileLocation.file`

           :func:`GPS.FileLocation.column`
        """
        pass  # implemented in Ada


###########################################################
# FileTemplate
###########################################################

class FileTemplate(object):

    """
    This class allows the user to create file templates from
    registered aliases.
    """

    @staticmethod
    def register(alias_name, label, unit_param, language,
                 is_impl, impl_alias_name=None, post_action=None):
        """
        Register a new file template and create a 'New/create ``label``
        contextual menu allowing users to create a new file from it for a given
        directory.

        A file template is associated with the registered alias retrieved from
        ``alias_name``: when clicking on the file template's contextual menu,
        a dialog asks the user to enter the alias parameters values and the
        expanded text of the alias is then used to prefill the new file.

        The base name of the newly created file is deduced from the
        ``unit_param`` alias parameter value and the naming sheme deduced from
        the given ``language``. Finally, the extension is computed from the
        ``is_impl`` boolean parameter, which indicates if the file is an
        implementation file or a specification file. The file is then placed
        in the directory from which the contextual menu was spawned.

        The optional ``impl_alias_name`` is used when when the file template
        should be used for a specification file (i.e: when ``is_impl`` is
        False): when specified, the user will have the choice to also create
        the corresponding implementation file from the given alias (e.g: create
        also the Ada body file when creating a package specification file).
        The parameters of both aliases should match in that case.

        The optional ``post_action`` parameter allows you to specify a function
        that will be called after the creation of a file from this template.
        This function will receive the newly created file and its associated
        project as parameters and should return True if it succeeds, False
        otherwise.

        example:

        # post_action callback
        def __add_to_main_units(project, file):
            # Ask the user if he wants to add the newly created main unit to
            # the project's main units.

            unit = file.unit()
            dialog_msg = ("Do you want to add '%s' to the main units of "
                  "project '%s'?" % (unit, project.name()))

            if GPS.MDI.yes_no_dialog(dialog_msg):
                project.add_main_unit(unit)
                project.save()
                project.recompute()

            return True

        # Register the 'Main Unit' FileTemplate
        GPS.FileTemplate.register(
            alias_name="main_unit",
            label="Ada Main Unit",
            unit_param="name",
            language="ada",
            is_impl=True,
            post_action=__add_to_main_units)

        :param str alias_name: the name of the alias to use
        :param str label: label used for displaying purposes
        :param str unit_param: the alias parameter to use for naming
        :param str language: the file template's language
        :param bool is_impl: whether it's an implementation file or not
        :param string impl_alias_name: The optional implementation alias name
        :param post_action:  A subprogram called after the creation of a file
        from this template.
        :type post_action: (:class:`GPS.File`, :class:`GPS.Project`) -> bool
        """
        pass  # implemented in Ada


###########################################################
# HTML
###########################################################

class HTML(object):

    """
    This class gives access to the help system of GPS as well as the
    integrated browser.
    """

    @staticmethod
    def add_doc_directory(directory):
        """
        Adds a new directory to the :file:`GPS_DOC_PATH` environment
        variable. This directory is searched for documentation files. If this
        directory contains a :file:`gps_index.xml` file, it is parsed to find
        the list of documentation files to add to the :guilabel:`Help`
        menu. See the GPS documentation for more information on the format of
        the :file:`gps_index.xml` files

        :param directory: Directory containing the documentation
        """
        pass  # implemented in Ada

    @staticmethod
    def browse(URL, anchor='', navigation=True):
        """
        Opens the GPS HTML viewer, and loads the given URL. If anchor matches
        a <a> tag in this file, GPS jumps to it. If URL is not an absolute file
        name, it is searched in the path set by the environment variable
        :file:`GPS_DOC_PATH`.

        If ``navigation`` is True, the URL is saved in the navigation list, so
         users can move back and forward from and to this location later on.

        The URL can be a network file name, with the following general format::

           protocol://username@host:port/full/path

        where protocol is one of the recognized protocols (http, ftp,.. see the
        GPS documentation), and the username and port are optional.

        :param URL: Name of the file to browse
        :param anchor: Location in the file where to jump to
        :param navigation: A boolean

        .. seealso:: :func:`GPS.HTML.add_doc_directory`

        .. code-block:: python

           GPS.HTML.browse("gps.html")
           => will open the GPS documentation in the internal browser

           GPS.HTML.browse("http://host.com/my/document")
           => will download documentation from the web
        """
        pass  # implemented in Ada


###########################################################
# Help
###########################################################

class Help(object):

    """
    This class gives access to the external documentation for shell
    commands. This external documentation is stored in the file
    :file:`shell_commands.xml`, part of the GPS installation, and is what you
    are currently seeing.

    You almost never need to use this class yourself, since it is used
    implicitly by Python when you call the help(object) command at the GPS
    prompt.

    The help browser understands the standard http urls, with links to specific
    parts of the document. For instance::

        "http://remote.com/my_document"
        or  "#link"

    As a special case, it also supports links starting with '%'. These are
    shell commands to execute within GPS, instead of a standard html file. For
    instance::

        <a href="%shell:Editor.edit g-os_lib.ads">GNAT.OS_Lib%lt;/a%gt;

    The first word after '%' is the language of the shell command, the rest of
    the text is the command to execute

    .. seealso:: :func:`GPS.Help.__init__`
    """

    def __init__(self):
        """
        Initializes the instance of the :class:`Help` class. This parses the
        XML file that contains the description of all the commands. With
        python, the memory occupied by this XML tree will be automatically
        freed. However, with the GPS shell you need to explicitly call
        :func:`GPS.Help.reset`.

        .. seealso:: :func:`GPS.Help.reset`
        """
        pass  # implemented in Ada

    def file(self):
        """
        Returns the name of the file that contains the description of the
        shell commands. You should not have to access it yourself, since you
        can do so using :func:`GPS.Help().getdoc` instead.

        :return: A string

        .. seealso:: :func:`GPS.Help.getdoc`
        """
        pass  # implemented in Ada

    def getdoc(self, name, html=False):
        """
        Searches in the XML file :file`shell_commands.xml` for the
        documentation for this specific command or entity. If no documentation
        is found, an error is raised. If ``html`` is true, the documentation is
        formated in HTML

        :param name: The fully qualified name of the command
        :param html: A boolean
        :return: A string, containing the help for the command

        .. code-block:: python

                print GPS.Help().getdoc("GPS.Help.getdoc")

        .. code-block:: python

                Help
                Help.getdoc %1 "GPS.Help.getdoc"
                Help.reset %2

        """
        pass  # implemented in Ada

    def reset(self):
        """
        Frees the memory occupied by this instance. This frees the XML tree
        that is kept in memory. As a result, you can no longer call
        :func:`GPS.Help.getdoc`.
        """
        pass  # implemented in Ada


###########################################################
# Hook
###########################################################

class Hook(object):

    """
    General interface to hooks. Hooks are commands executed when some specific
    events occur in GPS, and allow you to customize some of the aspects of GPS.

    All standard hooks are documented in the :class:`GPS.Predefined_Hooks`
    class.

    """

    def __init__(self, name):
        """
        Creates a new hook instance, referring to one of the already defined
        hooks.

        :param name: A string, the name of the hook
        """
        pass  # implemented in Ada

    def add(self, function_name, last=True):
        """
        Connects a new function to a specific hook. Any time this hook is run
        through :func:`run_hook`, this function is called with the same
        parameters passed to :func:`run_hook`. If ``last`` is True, this
        function is called after all functions currently added to this
        hook. If false, it is called before.

        :param function_name: A subprogram, see the "Subprogram Parameters"
           section in the GPS documentation
        :param last: A boolean

        .. seealso:: :func:`GPS.Hook.remove`

        .. code-block:: python

           def filed_edited(hook_name, file):
              print "File edited hook=" + hook_name + " file=" + file.name()
              GPS.Hook("file_edited").add(file_edited)

              """
        pass  # implemented in Ada

    def add_debounce(self, function_name, last=True):
        """
        The same as above but for calling callback asynchronously. Only for
        hooks which has :asynchronouse parameter.
              """
        pass  # implemented in Ada

    def describe_functions(self):
        """
        Lists all the functions executed when the hook is executed. The
        returned list might contain <<internal> strings, which indicate that
        an Ada function is connected to this hook.

        :return: A list of strings

        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Lists all defined hooks. See also :func:`run_hook`,
        :func:`register_hook` and :func:`add_hook`.

        :return: A list of strings

        .. seealso:: :func:`GPS.Hook.list_types`
        """
        pass  # implemented in Ada

    @staticmethod
    def list_types():
        """
        Lists all defined hook types.

        :return: A list of strings

        .. seealso:: :func:`GPS.Hook.register`
        """
        pass  # implemented in Ada

    @staticmethod
    def register(name, type=''):
        """
        Definess a new hook. This hook can take any number of parameters: the
        default is none. The type and number of parameters is called the type
        of the hook and is described by the optional second parameter. The
        value of this parameter should be either the empty string for a hook
        that does not take any parameter. Or it could be one of the predefined
        types exported by GPS itself (see :func:`list_hook_types`). Finally,
        it could be the word ""generic"" if this is a new type of hook purely
        defined for this scripting language

        :param name: A string, the name of the hook to create
        :param type: A string, the type of the hook.
           See :func:`GPS.Hook.list_types`
        """
        pass  # implemented in Ada

    def remove(self, function_name):
        """
        Removes ``function_name`` from the list of functions executed when the
        hook is run. This is the reverse of :func:`GPS.Hook.add`.

        :param function_name: A subprogram, see the "Subprogram Parameters"
           section in the GPS documentation

        .. seealso:: :func:`GPS.Hook.add`
        """
        pass  # implemented in Ada

    def run(self, *args):
        """
        Runs the hook. Calls all the functions that attached to that hook, and
        returns the return value of the last callback (this depends on the
        type of the hook, most often this is always None). When the callbacks
        for this hook are expected to return a boolean, this command stops as
        soon as one the callbacks returns True.

        :param args: Any number of parameters to pass to the hook.

        .. seealso::

           :func:`GPS.Hook.run_until_success`

           :func:`GPS.Hook.run_until_failure`
        """
        pass  # implemented in Ada

    def run_until_failure(self, *args):
        """
        Applies to hooks returning a boolean. Executes all functions attached
        to this hook until one returns False, in which case no further
        function is called. Returns the returned value of the last executed
        function.

        :param args: Any number of parameters to pass to the hook.
        :return: A boolean

        .. seealso::

           :func:`GPS.Hook.run_until_success`

           :func:`GPS.Hook.run`
        """
        pass  # implemented in Ada

    def run_until_success(self, *args):
        """
        Applies to hooks returning a boolean. Executes all functions attached
        to this hook until one returns True, in which case no further function
        is called. This returns the returned value of the last executed
        function. This is mostly the same as :func:`GPS.Hook.run`, but makes
        the halt condition more explicit.

        :param args: Any number of parameters to pass to the hook.
        :return: A boolean

        .. seealso::

           :func:`GPS.Hook.run_until_failure`

           :func:`GPS.Hook.run`
        """
        pass  # implemented in Ada


###########################################################
# Invalid_Argument
###########################################################

class Invalid_Argument(Exception):

    """
    An exception raised by GPS. Raised when calling a subprogram from the GPS
    module with an invalid argument type (passing an integer when a string is
    expected, for example).
    """
    pass  # implemented in Ada


###########################################################
# Locations
###########################################################

class Locations(object):
    """
    General interface to the :guilabel:`Locations` view.
    """

    @staticmethod
    def add(category, file, line, column, message,
            highlight='', length='0', look_for_secondary=False):
        """
        Adds a new entry to the :guilabel:`Locations` view. Nodes are created
        as needed for ``category`` or ``file``. If ``highlight`` is specified
        as a non-empty string, the enter line is highlighted in the file with
        a color determined by that highlight category (see
        :func:`register_highlighting` for more information). ``length`` is
        the length of the highlighting; the default of 0 indicates the whole
        line should be highlighted

        :param category: A string
        :param file: An instance of :class:`GPS.File`
        :param line: An integer
        :param column: An integer
        :param message: A string
        :param highlight: A string, the name of the highlight category
        :param length: An integer
        :param look_for_secondary: A boolean

        .. code-block:: python

           GPS.Editor.register_highlighting("My_Category", "blue")
           GPS.Locations.add(category="Name in location window",
                             file=GPS.File("foo.c"),
                             line=320,
                             column=2,
                             message="message",
                             highlight="My_Category")
        """
        pass  # implemented in Ada

    @staticmethod
    def dump(file):
        """
        Dumps the contents of the :guilabel:`Locations` view to the specified
        file, in XML format.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def list_categories():
        """
        Returns the list of all categories currently displayed in the
        :guilabel:`Locations` view. These are the top-level nodes used to
        group information generally related to one command, such as the
        result of a compilation.

        :return: A list of strings

        .. seealso:: :func:`GPS.Locations.remove_category`
        """
        pass  # implemented in Ada

    @staticmethod
    def list_locations(category, file):
        """
        Returns the list of all file locations currently listed in the given
        category and file.

        :param category: A string
        :param file: A string
        :return: A list of EditorLocation

        .. seealso:: :func:`GPS.Locations.remove_category`
        """
        pass  # implemented in Ada

    @staticmethod
    def parse(output, category, regexp='', file_index=-1,
              line_index=-1, column_index=-1, msg_index=-1,
              style_index=-1, warning_index=-1,
              highlight_category='Builder results',
              style_category='Style errors',
              warning_category='Builder warnings'):
        """
        Parses the contents of the string, which is supposedly the output of
        some tool, and adds the errors and warnings to the
        :guilabel:`Locations` view. A new category is created in the
        locations window if it does not exist. Preexisting contents for that
        category are not removed, see :func:`locations_remove_category`.

        The regular expression specifies how locations are recognized. By
        default, it matches `file:line:column`. The various indexes indicate
        the index of the opening parenthesis that contains the relevant
        information in the regular expression. Set it to 0 if that
        information is not available. ``style_index`` and ``warning_index``,
        if they match, force the error message in a specific category.

        ``highlight_category``, ``style_category`` and ``warning_category``
        reference the colors to use in the editor to highlight the messages
        when the regexp has matched. If they are set to the empty string, no
        highlighting is done in the editor. The default values match those by
        GPS itself to highlight the error messages. Create these categories
        with :func:`GPS.Editor.register_highlighting`.

        :param output: A string
        :param category: A string
        :param regexp: A string
        :param file_index: An integer
        :param line_index: An integer
        :param column_index: An integer
        :param msg_index: An integer
        :param style_index: An integer
        :param warning_index: An integer
        :param highlight_category: A string
        :param style_category: A string
        :param warning_category: A string

        .. seealso:: :func:`GPS.Editor.register_highlighting`
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_category(category):
        """
        Removes a category from the :guilabel:`Locations` view. This removes
        all associated files.

        :param category: A string

        .. seealso:: :func:`GPS.Locations.list_categories`
        """
        pass  # implemented in Ada

    @staticmethod
    def set_sort_order_hint(category):
        """
        Sets desired sorting order for file nodes of the category. Actual sort
        order can be overrided by user.

        :param category: A string ("Chronological" or "Alphabetical")

        """
        pass  # implemented in Ada


###########################################################
# Logger
###########################################################

class Logger(object):

    """
    This class provides an interface to the GPS logging mechanism. This can
    be used when debugging scripts, or even be left in production scripts for
    post-mortem analysis for instance. All output through this class is done
    in the GPS log file, :file:`$HOME/.gps/log`.

    GPS comes with some predefined logging streams, which can be used to
    configure the format of the log file, such as whether colors should be
    used or whether timestamps should be logged with each message.
    """

    def __init__(self, name):
        """
        Creates a new logging stream. Each stream is associated with a name,
        which is displayed before each line in the GPS log file, and is used
        to distinguish between various parts of GPS. Calling this constructor
        with the same name multiple times creates a new class instance.

        :param name: A string

        .. code-block:: python

           log = GPS.Logger("my_script")
           log.log("A message")
        """
        pass  # implemented in Ada

    def check(self, condition, error_message, success_message=''):
        """
        If ``condition`` is False, ``error_message`` is logged in the log
        file. If True, ``success_message`` is logged if present.

        :param condition: A boolean
        :param error_message: A string
        :param success_message: A string

        .. code-block:: python

           log=GPS.Logger("my_script")
           log.check(1 == 2, "Invalid operation")

        """
    count = None

    active = True
    """Whether this logging stream is active"""

    def log(self, message):
        """
        Logs a message in the GPS log file.

        :param message: A string
        """
        pass  # implemented in Ada

    def set_active(self, active):
        """
        Activates or deactivates a logging stream. The default for a sttream
        depends on the file :file:`$HOME/.gps/traces.cfg`, and will generally
        be active. When a stream is inactive, no message is sent to the log
        file.

        Use self.active to test whether a log stream is active.

        :param active: A boolean
        """
        pass  # implemented in Ada


###########################################################
# MDI
###########################################################

class MDI(object):

    """
    Represents GPS's Multiple Document Interface. This gives access to general
    graphical commands for GPS, as well as control over the current layout of
    the windows within GPS

    .. seealso:: :class:`GPS.MDIWindow`

    If the pygobject package is installed, GPS will export a few more functions
    to Python so that it is easier to interact with GPS itself. In particular,
    the :func:`GPS.MDI.add` function allows you to put a widget created by
    pygobject under control of GPS's MDI, so users can interact with it as with
    all other GPS windows.

    .. code-block:: python

       import GPS

       ## The following line is the usual way to make pygobject visible
       from gi.repository import Gtk, GLib, Gdk, GObject

       def on_clicked(*args):
          GPS.Console().write("button was pressed\\n")

       def create():
          button=Gtk.Button('press')
          button.connect('clicked', on_clicked)
          GPS.MDI.add(button, "From testgtk", "testgtk")
          win = GPS.MDI.get('testgtk')
          win.split()

       create()
    """

    GROUP_CONSOLES = 0
    GROUP_DEBUGGER_DATA = 0
    GROUP_DEBUGGER_STACK = 0
    GROUP_DEFAULT = 0
    GROUP_GRAPHS = 0
    GROUP_VCS_ACTIVITIES = 0
    GROUP_VCS_EXPLORER = 0
    GROUP_VIEW = 0
    # constants to be used in GPS.MDI.add()

    POSITION_AUTOMATIC = 0
    POSITION_BOTTOM = 0
    POSITION_TOP = 0
    POSITION_LEFT = 0
    POSITION_RIGHT = 0
    # constants to be used in GPS.MDI.add()

    @staticmethod
    def current_perspective():
        """
        The name of the current perspective.

        :return: str

        .. seealso: :func:`GPS.MDI.load_perspective`
        """

    @staticmethod
    def add(widget, title="", short="", group=0,
            position=0, save_desktop=None):
        """
        This function is only available if pygobject could be loaded in the
        python shell. You must install this library first, see the
        documentation for GPS.MDI itself.

        This function adds a widget inside the MDI of GPS. The resulting
        window can be manipulated by the user like any other standard GPS
        window. For example, it can be split, floated, or resized. ``title``
        is the string used in the title bar of the window, ``short`` is the
        string used in the notebook tabs. You can immediately retrieve a
        handle to the created window by calling GPS.MDI.get (short).

        This function has no effect if the widget is already in the MDI.
        In particular, the save_desktop parameter will not be taken into
        account in such a case.

        :param widget: A widget, created by pygobject, or an instance of
            GPS.GUI or one of the derived classes.
        :param title: A string
        :param short: A string
        :param group: An integer, see the constants MDI.GROUP_*
            This indicates to which logical group the widget belongs (the
            default group should be reserved for editors). You can create
            new groups as you see fit.
        :param position: An integer, see the constants MDI.POSITION_*.
            It is used when no other widget of the same group exists, to
            specify the initial location of the newly created notebook.
            When other widgets of the same group exist, the widget is put
            on top of them.
        :param save_desktop: A function that should be called when GPS saves
            the desktop into XML. This function receives the
            :class:`GPS.MDIWindow` as a parameter and should return a tuple
            of two elements (name, data) where name is a unique identifier
            for this window, and data is a string containing additional data
            to be saved (and later restored).  One suggestion is to encode
            any Python data through JSON and send the resulting string as
            data.  An easier alternative is to use the :file:`modules.py`
            support script in GPS, which handles this parameter automatically
            on your behalf.
        :return: The instance of :class:`GPS.MDIWindow` that was created

        .. code-block:: python

           from gi.repository import Gtk
           b = Gtk.Button("Press Me")
           GPS.MDI.add(b)

        .. seealso::

           :func:`GPS.MDI.get`

           :func:`GPS.GUI.pywidget`

           :func:`GPS.MDI`
        """
        pass  # implemented in Ada

    @staticmethod
    def children():
        """
        Returns all the windows currently in the MDI.

        :return: A list of :class:`GPS.MDIWindow`
        """
        pass  # implemented in Ada

    @staticmethod
    def current():
        """
        Returns the window that currently has the focus, or None if
        there is none.

        :return: An instance of :class:`GPS.MDIWindow` or None

        """
        pass  # implemented in Ada

    @staticmethod
    def dialog(msg):
        """
        Displays a modal dialog to report information to a user. This blocks
        the interpreter until the dialog is closed.

        :param msg: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def exit(force=False):
        """
        Exits GPS. If there are unsaved changes, a dialog is first displayed
        to ask whether these should be saved. If the user cancels the
        operation through the dialog, GPS will not exit. If ``force`` is
        true, no dialog is opened and nothing is saved.

        :param force: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def file_selector(file_filter='empty'):
        """
        Displays a modal file selector. The user selected file is returned,
        or a file with an empty name if :guilabel:`Cancel` is pressed.

        A file filter can be defined (such as "\*.ads") to show only a
        category of files.

        :param file_filter: A string
        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    @staticmethod
    def directory_selector(base_dir=""):
        """
        Displays a modal directory selector, allowing the user to create a new
        directoy if needed.
        The user selected directory is returned, or a directory with an empty
        name if :guilabel:`Cancel` is pressed.

        A base directory can be specified in order to start the dialog from it.
        When not specified, the base directory is set to the current one by
        default.

        :param base_dir: A string
        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Returns the window whose name is ``name``. If there is no such
        window, None is returned.

        :param name: A string
        :return: An instance of :class:`GPS.MDIWindow`
        """
        pass  # implemented in Ada

    @staticmethod
    def get_by_child(child):
        """
        Returns the window that contains ``child`` or raises an error if
        there is none.

        :param child: An instance of :class:`GPS.GUI`
        :return: An instance of :class:`GPS.MDIWindow`

        """
        pass  # implemented in Ada

    @staticmethod
    def hide():
        """
        Hides the graphical interface of GPS.
        """
        pass  # implemented in Ada

    @staticmethod
    def input_dialog(msg, *args):
        """
        Displays a modal dialog and requests some input from the user. The
        message is displayed at the top and one input field is displayed for
        each remaining argument. The arguments can take the form
        ""label=value"", in which case ""value"" is used as default for this
        entry. If argument is prepend with 'multiline:' prefix field is
        edited as multi-line text. The return value is the value that the
        user has input for each of these parameters.

        An empty list is returned if the user presses :guilabel:`Cancel`.

        :param msg: A string
        :param args: Any number of strings
        :return: A list of strings

        .. code-block:: python

           a, b = GPS.MDI.input_dialog("Please enter values", "a", "b")
           print a, b

        """
        pass  # implemented in Ada

    @staticmethod
    def combo_selection_dialog(title, message, choices, combo_label=None):
        """
        Displays a modal dialog with the given ``title``, the given
        ``message`` displayed at the top, and a combobox displaying the
        possible ``choices``.

        Dy default, the first value in ``choices`` is selected in the combobox.

        The optional ``combo_label`` parameter can be used to display a label
        on the left-side of the combobox.

        This function returns the choice that is selected when the user presses
        the :guilabel:`Ok` button.

        :param title: a string
        :param message: a string
        :param choiches: a string list
        :param combo_label: a string
        """
        pass  # implemented in Ada

    @staticmethod
    def save_all(force=False):
        """
        Saves all currently unsaved windows. This includes open editors, the
        project, and any other window that has registered some save
        callbacks.

        If ``force`` is false, a confirmation dialog is displayed so the user
        can select which windows to save.

        :param force: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def show():
        """
        Shows the graphical interface of GPS.
        """
        pass  # implemented in Ada

    @staticmethod
    def yes_no_dialog(msg):
        """
        Displays a modal dialog to ask a question to the user. This blocks
        the interpreter until the dialog is closed. The dialog has two
        buttons :guilabel:`Yes` and :guilabel:`No`, and the selected button
        is returned to the caller.

        :param msg: A string
        :return: A boolean

        .. code-block:: python

           if GPS.MDI.yes_no_dialog("Do you want to print?"):
               print "You pressed yes"

        """

    @staticmethod
    def information_popup(self, text='', icon=''):
        """
        Display a temporary information popup on the screen. This popup
        automatically disappears after a short while, so should only be
        used to indicate success or failure for an action, for instance.

        :param str text: The text to display.
        :param str icon: The name of an icon to display beside the text.
        """

    @staticmethod
    def load_perspective(name):
        """
        Change the current perspective to the one designated by ``name``.
        This function does nothing if ``name`` does not refer to any known
        perspective.

        :param name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def present_main_window():
        """
        Present the GPS main window. This is useful when you want to
        programatically give the focus to the GPS main window.
        """

        pass  # implemented in Ada


###########################################################
# MDIWindow
###########################################################

class MDIWindow(GUI):

    """
    This class represents one of the windows currently displayed in GPS. This
    includes both the windows currently visible to the user, and the ones that
    are temporarily hidden, for instance because they are displayed below
    another window.  Windows acts as containers for other widgets.
    """

    def __init__(self):
        """
        Prevents the creation of instances of :class:`GPS.MDIWindow`. This is
        done by calling the various subprograms in the :class:`GPS.MDI`
        class.
        """
        pass  # implemented in Ada

    def close(self, force=False):
        """
        Close the window. If force is False, give the window an opportunity
        to prevent its deletion (for instance through a save confirmation
        dialog).

        :param force: A boolean
        """
        pass  # implemented in Ada

    def float(self, float=True):
        """
        Floats the window, i.e., creates a new toplevel window to display
        it. It is then under control of the user's operating system or window
        manager. If ``float`` is False, the window is reintegrated within the
        GPS MDI instead.

        :param float: A boolean
        """
        pass  # implemented in Ada

    def get_child(self):
        """
        Returns the child contained in the window. The returned value might
        be an instance of a subclass of :class:`GPS.GUI`, if that window was
        created from a shell command.

        :return: An instance of :class:`GPS.GUI`

        .. code-block:: python

            # Accessing the GPS.Console instance used for python can be done
            # with:
            GPS.MDI.get("Python").get_child()
        """
        pass  # implemented in Ada

    def is_floating(self):
        """
        Returns True if the window is currently floating (i.e., in its own
        toplevel window) or False if the window is integrated into the main
        GPS window.

        :return: A boolean
        """
        pass  # implemented in Ada

    def name(self, short=False):
        """
        Returns the name of the window. If ``short`` is False, the long name
        is returned, the one that appears in the title bar. If True, the
        short name is returned, the one that appears in notebook tabs.

        :param short: A boolean
        :return: A string
        """
        pass  # implemented in Ada

    def next(self, visible_only=True):
        """
        Returns the next window in the MDI, or the current window if there is
        no other window. If ``visible_only`` is True, only the windows
        currently visible to the user can be returned. This always returns
        floating windows.

        :param visible_only: A boolean
        :return: An instance of :class:`GPS.MDIWindow`
        """
        pass  # implemented in Ada

    def raise_window(self):
        """
        Raises the window so that it becomes visible to the user. The window
        also gains the focus.
        """
        pass  # implemented in Ada

    def rename(self, name, short=''):
        """
        Changes the title used for a window.  ``name`` is the long title, as
        it appears in the title bar, and ``short``, if specified, is the name
        that appears in  notebook tabs.

        Using this function may be dangereous in some contexts, since GPS
        keeps track of editors through their name.

        :param name: A string
        :param short: A string
        """
        pass  # implemented in Ada

    def split(self, vertically=True, reuse=False, new_view=False):
        """
        Splits the window in two parts, either horizontally (side by side),
        or vertically (one below the other).

        :param bool vertically:
        :param bool reuse: whether to reuse an existing space to the side of
           current window, rather than splitting the current window.
           This should be used to avoid ending up with too small windows.
        :param bool new_view: whether to create a new view when the current
           window is an editor.

        .. seealso:: :func:`GPS.MDIWindow.single`
        """
        pass  # implemented in Ada


###########################################################
# Menu
###########################################################

class Menu(object):

    """
    This class is a general interface to the menu system in GPS. It gives you
    control over such things as which menus should be active and what should
    be executed when the menu is selected by the user.

    .. seealso:: :func:`GPS.Menu.__init__`

    """

    action = None
    """The GPS.Action executed by this menu"""

    def __init__(self):
        """
        Prevents the creation of a menu instance. Such instances can only be
        created internally by GPS as a result of calling :func:`GPS.Menu.get`
        or :func:`GPS.Menu.create`. This is so you always get the same
        instance of :class:`GPS.Menu` when refering to a given menu in GPS
        and so you can store your own specific data with the menu.
        """
        pass  # implemented in Ada

    @staticmethod
    def get(path):
        """
        Returns the menu found at the given path. ``path`` is similar to
        file paths, starting with the main GPS menu ('/'), down to each
        submenus. For example, '/VCS/Directory/Update Directory' refers to
        the submenu 'Update Directory' of the submenu 'Directory' of the menu
        'VCS'. Path is case-sensitive.

        :param path: A string
        :return: The instance of :class:`GPS.Menu`

        .. code-block:: python

           # The following example will prevent the user from using the VCS
           # menu and all its entries:

           GPS.Menu.get('/VCS').set_sensitive (False)
        """
        pass  # implemented in Ada


###########################################################
# MemoryUsageProvider
###########################################################

class MemoryUsageProvider(object):

    """
    General interface used to populate the GPS Memory Usage View.

    In practice, this class is derived in the code to provide memory usage
    providers that are specific to ones or more external tools (e.g: a memory
    usage provider that fetches data generated from the ld linker).
    """

    @staticmethod
    def _register(name, construct):
        """
        Register a new memory usage provider.
        This function is not meant to be called directly. Instead, check the
        memory_usage_provider/core.py plugin.

        :param str name: the name of the memory usage provider
        :param construct: a function called when initializing a new provider
        """
        pass  # implemented in Ada


###########################################################
# MemoryUsageProviderVisitor
###########################################################

class MemoryUsageProviderVisitor(object):

    """
    This class is used to notify GPS of events that occur during a memory usage
    provider task (e.g: when a memory usage provider has finished to fetch all
    the memory usage data needed by the Memory Usage View).
    """

    def on_memory_usage_data_fetched(self, regions, sections, modules):
        """
        Report when a :class:`GPS.MemoryUsageProvider` finished to fetch all
        the memory usage data of the last built executable (i.e: memory regions
        and memory sections and modules).

        This method is called in
        `GPS.MemoryUsageProvider.async_fetch_memory_usage_data`.

        Note that the given :class:`GPS.MemoryUsageProviderVisitor` instance
        is freed after calling this method.

        :param regions: a list of (name, origin, length) tuples describing
            memory regions.

        :param sections: a list of (name, origin, length, region_name) tuples
            describing memory sections.

        :param modules: a list of (obj_file, lib_file, origin, size,

            region_name, section_name) tuples describing modules, which are
            file based split of ressources consumed: obj_file and lib_file
            repectively correspond to the full paths of the object file and, if
            any, of the library file for which this artifact was compiled.
        """
        pass  # implemented in Ada


###########################################################
# Message
###########################################################

class Message(object):

    """
    This class is used to manipulate GPS messages: build errors, editor
    annotations, etc.
    """

    Flags = enum('Message.Flags', INVISIBLE=0, IN_SIDEBAR=1,
                 IN_LOCATIONS=2, IN_SIDEBAR_AND_LOCATIONS=3)

    @staticmethod
    def __del__():
        """
        Called when the message instance is destroyed.
        """
        pass  # implemented in Ada

    def __init__(self, category, file, line, column, text,
                 show_on_editor_side=True,
                 show_in_locations=True,
                 allow_auto_jump_to_first=True):
        """
        Adds a Message in GPS.

        :param category: A String indicating the message category
        :param file: A File indicating the file
        :param line: An integer indicating the line
        :param column: An integer indicating the column
        :param text: A pango markup String containg the message text

        :param bool show_on_editor_side: Whether to show the message in the
            editor's gutter

        :param bool show_in_locations: Whether to show the message in the
            locations view

        :param bool allow_auto_jump_to_first: If True, then adding a message
            that is the first for its category will auto jump the editor to it,
            if the corresponding preference is activated

        .. code-block:: python

           # Create a message

           m=GPS.Message("default", GPS.File("gps-main.adb"),
                 1841, 20, "test message")

           # Remove the message
           m.remove()
        """
        pass  # implemented in Ada

    def create_nested_message(self, file, line, column, text):
        """
        Add nested message.

        :param file: A File indicating the file
        :param line: An integer indicating the line
        :param column: An integer indicating the column
        :param text: A string containg the message text
        """

        pass  # implemented in Ada

    def execute_action(self):
        """
        If the message has an associated action, executes it.
        """
        pass  # implemented in Ada

    def get_category(self):
        """
        Returns the message's category.
        """
        pass  # implemented in Ada

    def get_column(self):
        """
        Returns the message's column.
        """
        pass  # implemented in Ada

    def get_file(self):
        """
        Returns the message's file.
        """
        pass  # implemented in Ada

    def get_flags(self):
        """
        Returns an integer representing the location of the message: should
        it be displayed in locations view and source editor's
        sidebar. Message is displayed in source editor's sidebar when zero
        bit is set, and is displayed in locations view when first bit is set,
        so here is possible values:

        * GPS.Message.Flags.INVISIBLE:
          message is invisible

        * GPS.Message.Flags.IN_SIDEBAR:
          message is visible in source editor's sidebar only

        * GPS.Message.Flags.IN_LOCATIONS:
          message is visible in locations view only

        * GPS.Message.Flags.IN_SIDEBAR_AND_LOCATIONS:
          message is visible in source editor and locations view

        Note, this set of flags can be extended in the future, so they should
        be viewed as bits that are "or"ed together.
        """
        pass  # implemented in Ada

    def get_line(self):
        """
        Returns the message's line.
        """
        pass  # implemented in Ada

    def get_mark(self):
        """
        Returns an :class:`EditorMark` which was created with the message and
        keeps track of the location when the file is edited.
        """
        pass  # implemented in Ada

    def get_text(self):
        """
        Returns the message's text.
        """
        pass  # implemented in Ada

    @staticmethod
    def list(file=None, category=None):
        """
        Returns a list of all messages currently stored in GPS.

        :param file: a :class:`GPS File`.
            Specifying this parameter restricts the output to messages
            to this file only.

        :param category: a String.
            Specifying this parameter restricts the output to messages
            of this category only

        :return: a list of :class:`GMS.Message`
        """
        pass  # implemented in Ada

    def remove(self):
        """
        Removes the message from GPS.
        """
        pass  # implemented in Ada

    def set_action(self, action, image, tooltip=None):
        """
        Adds an action item to the message. This adds an icon to the message;
        Clicking on the icon executes ``action``.

        :param action: A String corresponding to a registered GPS action
        :param image: A String name of the icon to display.
           See :ref:`Adding_stock_icons` for more information on icons
        :param tooltip: A string containing the tooltip to
           display when the mouse is on the icon
        """
        pass  # implemented in Ada

    @staticmethod
    def set_sort_order_hint(category, hint):
        """
        Sets default sorting method for files in the :guilabel:`Locations`
        view.

        :param category: Name of messages category
        :param hint: Default sorting method ("chronological" or
           "alphabetical")
        """
        pass  # implemented in Ada

    def set_style(self, style, len):
        """
        Sets the style of the message. ``len`` is the length in number of
        characters to highlight. If 0, highlight the whole line. If omitted,
        the length of the message highlighting is not modified.

        :param style: An integer
        :param len: An integer
        """
        pass  # implemented in Ada

    def set_subprogram(self, subprogram, image, tooltip=None):
        """
        Adds an action item to the message. This adds an icon to the message.
        Clicking on this icon calls ``subprogram``, with the message passed
        as its parameter.

        :param subprogram: A subprogram in the scripting language.
            This subprogram takes on parameter, which is a message
        :param image: A String name of the icon to display.
           See :ref:`Adding_stock_icons` for more information on icons
        :param tooltip: A string which contains the tooltip to
           display when the mouse is on the icon

        .. code-block:: python

           # This adds a "close" button to all the messages
           [msg.set_subprogram(lambda m : m.remove(), "gtk-close", "")
                               for msg in GPS.Message.list()]
        """
        pass  # implemented in Ada

    def cancel_subprogram(self):
        """
        Remove the action item associated to this message.
        """
        pass  # implemented in Ada


###########################################################
# Missing_Arguments
###########################################################

class Missing_Arguments(Exception):

    """
    An exception raised by GPS. Raised when calling a subprogram from the GPS
    module with missing arguments.
    """
    pass  # implemented in Ada


###########################################################
# Cursor
###########################################################

class Cursor(object):

    """
    Interface to a cursor in :class:`GPS.EditorBuffer`. Only gives access to
    the insertion mark and to the selection mark of the cursor.
    """

    def move(self, loc, extend_selection=False):
        """
        Moves the cursor to the given location.

        :param loc: A :class:`GPS.EditorLocation` that you want to move the
            cursor to
        :param extend_selection: A boolean. If True, the selection mark will
            not move so the selection is extended. If False, both marks move
            simultaneously
        """

    def set_manual_sync(self):
        """
        Sets the buffer in manual sync mode regarding this cursor. This set
        sync to be manual and all insertions/deletions are considered as
        originating from this cursor instance. If you do not do this, an
        action on the buffer (like an insertion) is repercuted on every alive
        cursor instance.

        NOTE: Do not call this manually. Instead, iterate on the results of
        :meth:`EditorBuffer.cursors` so this method is called for you
        automatically.
        """

    def sel_mark(self):
        """
        Returns the cursor's selection mark.

        NOTE: If you can interact with your cursor via :meth:`Cursor.move`
        rather than via manually moving marks, you should prefer that method.

        :return: The :class:`GPS.EditorMark` instance corresponding to the
           cursor's selection mark
        """
        pass

    def mark(self):
        """
        Returns the cursor's main mark.

        NOTE: If you can interact with your cursor via :meth:`Cursor.move`
        rather than via manually moving marks, you should prefer that method.

        :return: The :class:`GPS.EditorMark` instance corresponding to the
           cursor's insert mark
        """
        pass


###########################################################
# Preference
###########################################################
class Preference(object):

    """
    Interface to the GPS preferences, as set in the :menuselection:`Edit -->
    Preferences...` dialog. New preferences are created through XML
    customization files (or calls to :func:`GPS.parse_xml`, see the GPS
    documentation).

    .. seealso:: :func:`GPS.Preference.__init__`

    .. code-block:: python

       GPS.parse_xml('''
          <preference name="custom-adb-file-color"
              label="Background color for .adb files"
              page="Editor:Fonts &amp; Colors"
              default="yellow"
              type="color" />''')
       print "color is " + GPS.Preference("custom-adb-file-color").get()
    """

    def __init__(self, name):
        """
        Initializes an instance of the :class:`GPS.Preference` class,
        associating it with the preference ``name``, which is the one that is
        found in the :file:`$HOME/.gps/preferences` file. When you are
        creating a new preference, this name can include '/' characters,
        which results in subpages created in the :guilabel:`Preferences`
        dialog. The name after the last '/' should only include letters and
        '-' characters. You can also specify a group before the last '/', by
        appending a ':' delimitor followed by the name of the preference's
        group. If the name starts with '/' and contains no other
        '/', the preference is not visible in the :guilabel:`Preferences`
        dialog, though it can be manipulated as usual and is loaded
        automatically by GPS on startup.

        :param name: A string
        """
        pass  # implemented in Ada

    def create_style(self, label, doc='', default_fg="",
                     default_bg="transparent",
                     default_font_style="default"):
        """
        Creates a new text style preference, which enables the user to choose
        between different text style characteristics, namely foreground
        color, background color, and wether the text is bold, italic, both,
        or neither.

        :param string label: The label of the preference
        :param string doc: The documentation of the preference
        :param string default_fg: The default foreground color for this
          preference, as a CSS-like color.
        :param string default_bg: The default background color for this
          preference, as a CSS-like color
        :param string default_font_style: The style, one of "default",
          "normal", "bold", "italic" or "bold_italic"
        """

    def create(self, label, type, doc='', default='', *args):
        """
        Creates a new preference and makes it visible in the preferences
        dialog. In the dialog, the preference appears in the page given by
        the name used when creating the instance of
        :class:`GPS.Preference`. ``label`` qualifies the preference and
        ``doc`` appears as a tooltip to explain the preference to users.
        ``type`` describes the type of preference and therefore how it should
        be edited by users.

        The parameters to this function cannot be named (since it uses a
        variable number of parameters, see the documentation below).

        The additional parameters depend on the type of preference you are
        creating:

        - For "integer", the default value is 0, and the two additional
          parameters are the minimum and maximum possible values. These are
          integers.

        - For a "boolean", the default is True.

        - For a "string", the default is the empty string.

        - A "multiline" behaves the same as a string except it is edited on
          multiple lines in the :guilabel:`Preferences` dialog.

        - For a "color", the default is "black".

        - For a "font", the default is "sans 9".

        - For an "enum", any number of additional parameters can be
          specified. They are all the possible values of the preference. The
          default is the index in the list of possible values, starting at 0.

        :param label: A string
        :param type: A string, one of "integer", "boolean", "string",
           "color", "font", "enum", "multiline"
        :param doc: A string
        :param default: Depends on the type
        :param args: Additional parameters depending on the type

        :returns: The preference itself
        """
        pass  # implemented in Ada

    def create_with_priority(self, label, type, priority, doc='',
                             default='', *args):
        """
        Same as :func:`GPS.Preferences.create` but with an additional parameter
        allowing to specify the priority of the preference.
        By default, preferences have -1 for priority. Higher priority
        preferences are placed at the top of the page.

        :param priority: An integer
        """
        pass  # implemented in Ada

    def get(self):
        """
        Gets value of the given preference. The exact returned type depends
        on the type of the preference. Note that boolean values are returned
        as integers, for compatibility with older versions of Python.

        :return: A string or an integer

        .. code-block:: python

           if GPS.Preference("MDI-All-Floating"):
              print "We are in all-floating mode"
        """
        pass  # implemented in Ada

    def set(self, value, save=True):
        """
        Sets ``value`` for the given preference. The type of the parameter
        depends on the type of the preference.

        :param value: A string, boolean or integer
        :param save: no longer used, kept for backward compatibility only.
        """
        pass  # implemented in Ada


###########################################################
# PreferencesPage
###########################################################

class PreferencesPage:
    """
    Interface to the GPS preferences pages, as set in the
    :menuselection:`Edit --> Preferences...` dialog.

    This interface can be used to create custom preferences pages.
    """
    @staticmethod
    def create(self, name, get_widget, priority=-1, is_integrated=False):
        """
        Create a new preferences page and makes it visible in the
        :guilabel:`Preferences` dialog, adding an entry with the
        given ```name``.

        Each time the page is selected, the PyGtk widget returned by
        the ``get_widget``function is displayed. Note that this widget
        is destroyed when closing the preferences dialog: thus, ``get_widget``
        can't return the same widget twice and should create a new one
        each time it is called instead.

        The ``priority`` is used to order the preferences pages in
        the :guilabel:`Preferences` dialog tree view, using the
        following policy:

           - Pages with higher priorities are listed at the top of the
             tree view.

           - If two pages have the same priority, the alphabetical order
             determines which page will appear first.

        when ``is_integrated`` is True, the preferences editor dialog will
        not create an entry for this page in its left tree view. This is
        generally needed for pages that are integrated in another visible
        preferences pages or for pages displayed in the GPS preferences
        assistant.

        :param name: A string
        :param get_widget: function returning a PyGtk widget
        :param priority: integer defining the page's priority
        :param is_integrated: A boolean
        """
        pass   # implemented in Ada

###########################################################
# Process
###########################################################


class Process(Command):
    """
    Interface to :program:`expect`-related commands. This class can be used
    to spawn new processes and communicate with them later. It is similar to
    what GPS uses to communicate with :program:`gdb`. This is a subclass of
    :class:`GPS.Command`.

    .. seealso::

       :func:`GPS.Process.__init__`

       :func:`GPS.Command`

    .. code-block:: python

       # The following example launches a gdb process, lets it print its
       # welcome message, and kills it as soon as a prompt is seen in the
       # output.  In addition, it displays debugging messages in a new GPS
       # window.  As you might note, some instance-specific data is stored in
       # the instance of the process, and can be retrieve in each callback.

       import GPS, sys

       def my_print(msg):
          sys.stdout.set_console("My gdb")
          print(msg)
          sys.stdout.set_console()

       def on_match(self, matched, unmatched):
          my_print "on_match (" + self.id + ")=" + matched
          self.kill()

       def on_exit(self, status, remaining_output):
          my_print "on_exit (" + self.id + ")"

       def run():
          proc = GPS.Process("gdb", "^\\(gdb\\)", on_match=on_match,
                             on_exit=on_exit)
          proc.id = "first session"

       run()

    .. code-block:: python

       # A similar example can be implemented by using a new class. This is
       # slightly cleaner, since it does not pollute the global namespace.

       class My_Gdb(GPS.Process):
          def matched(self, matched, unmatched):
             my_print("matched " + self.id)
             self.kill()

          def exited(self, status, output):
             my_print("exited " + self.id)

          def __init__(self):
              self.id = "from class"
              GPS.Process.__init__(self, "gdb",
                  "^\\(gdb\\)",
                  on_match=My_Gdb.matched,
                  on_exit=My_Gdb.exited)

       My_Gdb()
    """

    def __init__(self, command, regexp='', on_match=None, on_exit=None,
                 task_manager=True, progress_regexp='', progress_current=1,
                 progress_total=1, before_kill=None, remote_server='',
                 show_command=False, single_line_regexp=False,
                 case_sensitive_regexp=True, strip_cr=True, active=False,
                 directory="", block_exit=True):
        """
        Spawns ``command``, which can include triple-quoted strings, similar
        to Python, which are always preserved as one argument.

        The external process might not start immediately. Instead, it will
        start whenever GPS starts processing events again (once your script
        gives the hand back to GPS), or when you call :func:`expect()`
        or :func:`get_result()` below.

        If ``regexp`` is not-empty and ``on_match_action`` is specified,
        launch ``on_match_action`` when ``regexp`` is found in the process
        output. If ``on_exit_action`` is specified, execute it when the
        process terminates. Return the ID of the spawned process.

        ``regexp`` is always compiled with the multi_line option, so "^" and
        "$" also match at the beginning and end of each line, not just the
        whole output. You can optionally compile it with the single_line
        option whereby "."  also matches the newline character. Likewise you
        can set the regexp to be case insensitive by setting
        case_sensitive_regexp to False.

        ``on_match`` is a subprogram called with the parameters:

        - $1 = the instance of :class:`GPS.Process`
        - $2 = the string which matched the regexp
        - $3 = the string since the last match

        ``before_kill`` is a subprogram called just before the process is
        about to be killed. It is called when the user is interrupting the
        process through the tasks view, or when GPS exits. It is not called
        when the process terminates normally. When it is called, the process
        is still valid and can be send commands. Its parameters are:

        - $1 = the instance of :class:`GPS.Process`
        - $2 = the entire output of the process

        ``on_exit`` is a subprogram called when the process has exited. You
        can no longer send input to it at this stage. Its parameters are:

        - $1 = the instance of :class:`GPS.Process`
        - $2 = the exit status
        - $3 = the output of the process since the last call to
          :func:`on_match`

          If ``task_manager`` is True, the process will be visible in the GPS
          tasks view and can be interrupted or paused by users. Otherwise,
          it is running in the background and never visible to the user.  If
          ``progress_regexp`` is specified, the output of the process will be
          scanned for this regexp. The part that matches will not be returned
          to ``on_match``. Instead, they will be used to guess the current
          progress of the command. Two groups of parenthesis are parsed, the
          one at ``progress_current``, and the one at ``progress_total``. The
          number returned for each of these groups indicate the current
          progress of the command and the total that must be reached for this
          command to complete. For example, if your process outputs lines like
          "done 2 out of 5", you should create a regular expression that
          matches the 2 and the 5 to guess the current progress. As a result,
          a progress bar is displayed in the tasks view of GPS, and will
          allow users to monitor commands.

          An exception is raised if the process could not be spawned.

          :param command: A string or list of strings.
             The list of strings is preferred, since it provides a better
             handling of arguments with spaces (like filenames). When you
             are using a string, you need to quote such arguments.
          :param regexp: A string
          :param on_match: A subprogram, see the section
             "Subprogram parameters" in the GPS documentation
          :param on_exit: A subprogram
          :param task_manager: A boolean
          :param progress_regexp: A string
          :param progress_current: An integer
          :param progress_total: An integer
          :param before_kill: A subprogram
          :param str remote_server: Possible values are "GPS_Server",
             the empty string (equivalent to "GPS_Server"), "Build_Server",
             "Debug_Server", "Execution_Server" and "Tools_Server".
             This represents the server used to spawn the process. By
             default, the GPS_Server is used, which is always the
             local machine. See the section "Using GPS for Remote Development"
             in the GPS documentation for more information on this field.
          :param bool show_command: if True, the command line used to spawn
             the new process is displayed in the :guilabel:`Messages` console.
          :param bool single_line_regexp:
          :param bool case_sensitive_regexp:
          :param bool strip_cr: If true, the output of the process will have
             all its ASCII.CR removed before the string is passed to GPS and
             your script. This, in general, provides better portability to
             Windows systems, but might not be suitable for applications for
             which CR is relevant (for example, those that drive an ANSI
             terminal).
          :param bool active: Whether GPS should actively monitor the state
             of the process. This will require more CPU (and might make the
             GUI less reactive while the process runs), but ensures that
             events like on_exit will be called earlier.
          :param str directory: The directory in which the external process
             should be started.
          :param bool block_exit: If true, then GPS will display a dialog
             when the user wants to exit, asking whether to kill this
             process.

          .. seealso:: :class:`GPS.Process`
        """
        pass  # implemented in Ada

    def expect(self, regexp, timeout=-1):
        """
        Blocks execution of the script until either ``regexp`` has been seen
        in the output of the command or ``timeout`` has expired. If
        ``timeout`` is negative, wait forever until we see ``regexp`` or the
        process completes execution.

        While in such a call, the usual ``on_match`` callback is called as
        usual, so you might need to add an explicit test in your on_match
        callback not to do anything in this case.

        This command returns the output of the process since the start of the
        call and up to the end of the text that matched regexp. Note that it
        also includes the output sent to the on_match callback while it is
        running. It does not, however, include output already returned by a
        previous call to this function (nor does it guarantee that two
        successive calls return the full output of the process, since some
        output might have been matched by on_match between the two calls, and
        would not be returned by the second call).

        If a timeout occurred or the process terminated, an exception is
        raised.

        :param regexp: A string
        :param timeout: An integer, in milliseconds
        :return: A string

        .. code-block:: python

           proc = GPS.Process("/bin/sh")
           print("Output till prompt=" + proc.expect (">"))
           proc.send("ls")
        """
        pass  # implemented in Ada

    def get_result(self):
        """
        Waits untill the process terminates and returns its output. This is
        the output since the last call to this function, so if you call it
        after performing some calls to :func:`expect`, the returned string
        does not contain the output already returned by :func:`expect`.

        :return: A string
        """
        pass  # implemented in Ada

    def interrupt(self):
        """
        Interrupts a process controlled by GPS.
        """
        pass  # implemented in Ada

    def kill(self):
        """
        Terminates a process controlled by GPS.
        """
        pass  # implemented in Ada

    def send(self, command, add_lf=True):
        """
        Sends a line of text to the process. If you need to close the input
        stream to an external process, it often works to send the character
        ASCII 4, for example through the Python command chr(4).

        :param command: A string
        :param add_lf: A boolean
        """
        pass  # implemented in Ada

    def set_size(self, rows, columns):
        """
        Tells the process about the size of its terminal. ``rows`` and
        ``columns`` should (but need not) be the number of visible rows and
        columns of the terminal in which the process is running.

        :param rows: An integer
        :param columns: An integer
        """
        pass  # implemented in Ada

    def wait(self):
        """
        Blocks the execution of the script until the process has finished
        executing. The exit callback registered when the process was started
        will be called before returning from this function.

        This function returns the exit status of the command.

        :return: An integer

        """
        pass  # implemented in Ada


###########################################################
# Project
###########################################################

class Project(object):

    """
    Represents a project file. Also see the GPS documentation on how to
    create new project attributes.

    .. seealso:: :func:`GPS.Project.__init__`

    Related hooks:

    - project_view_changed

      Called whenever the project is recomputed, such as when one of its
      attributes was changed by the user or the environment variables are
      changed.

      This is a good time to test the list of languages
      (:func:`GPS.Project.languages`) that the project supports and do
      language-specific customizations

   - project_changed

      Called when a new project was loaded. The hook above is called after
      this one.
    """

    target = None
    """
    Returns the Target project attribute value or an empty string if not
    defined.

    If the given project extends from another project, the attribute is also
    looked up in the extended project.
    """

    def __cmp__(self, file):
        """
        Compares two instances of :class:`GPS.Project` and returns -1, 0 or 1
        depending on their relative sort order.

        :param file: An instance of :class:`GPS.Project`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Returns a hash value suitable for storing self in a dictionary.

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name):
        """
        Initializes an instance of :class:`GPS.Project`. The project must be
        currently loaded in GPS.

        :param name: The project name

        .. seealso:: :func:`GPS.Project.name`
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Returns a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python.

        :return: A string
        """
        pass  # implemented in Ada

    def add_attribute_values(self, attribute, package, index, value):
        """
         Adds some values to an attribute. You can add as many values you
         need at the end of the param list.  If the package is not specified,
         the attribute at the toplevel of the project is queried.  The index
         only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value
            of this attribute
        :param value: A string, the name of the first value to add

        .. seealso::

           :func:`GPS.Project.set_attribute_as_string`

           :func:`GPS.Project.remove_attribute_values`

           :func:`GPS.Project.clear_attribute_values`

        For example:

        .. code-block:: python

           GPS.Project.root().add_attribute_values(
               "Default_Switches", "Compiler", "ada", "-gnatwa", "-gnatwe");

        """
        pass  # implemented in Ada

    def add_dependency(self, path):
        """
        Adds a new dependency from self to the project file pointed to by
        ``path``. This is the equivalent of putting a with clause in self,
        and means that the source files in self can depend on source files
        from the imported project.

        :param path: The path to another project to depend on

        .. seealso:: :func:`GPS.Project.remove_dependency`
        """
        pass  # implemented in Ada

    def add_main_unit(self, *args):
        """
        Adds some main units to the current project for the current
        scenario. The project is not saved automatically.

        :param args: Any number of arguments, at least one

        """
        pass  # implemented in Ada

    @staticmethod
    def add_predefined_paths(sources='', objects=''):
        """
        Adds some predefined directories to the source path or the objects
        path. These are searched when GPS needs to open a file by its base
        name, in particular from the :menuselection:`Find --> Find File in
        Project` dialog.  The new paths are added in front, so they have
        priorities over previously defined paths.

        :param sources: A list of directories separated by the appropriate
            separator (':' or ';' depending on the system
        :param objects: As above

        .. code-block:: python

           GPS.Project.add_predefined_paths(os.pathsep.join(sys.path))
        """
        pass  # implemented in Ada

    def add_source_dir(self, directory):
        """
        Adds a new source directory to the project. The new directory is
        added in front of the source path. You should call
        :func:`GPS.Project.recompute` after calling this method to recompute
        the list of source files. The directory is added for the current
        value of the scenario variables only. Note that if the current source
        directory for the project is not specified explicitly in the .gpr
        file), it is overriden by the new directory you are adding. If the
        directory is already part of the source directories for the project,
        it is not added a second time.

        :param directory: A string

        .. seealso::

           :func:`GPS.Project.source_dirs`

           :func:`GPS.Project.remove_source_dir`
        """
        pass  # implemented in Ada

    def ancestor_deps(self):
        """
        Returns the list of projects that might contain sources that depend
        on the project's sources. When doing extensive searches it is not
        worth checking other projects. Project itself is included in the
        list.

        This is also the list of projects that import self.

        :return: A list of instances of GPS.Project

        .. code-block:: python

           for p in GPS.Project("kernel").ancestor_deps():
               print p.name()

           # will print the name of all the projects that import kernel.gpr
        """
        pass  # implemented in Ada

    def artifacts_dir(self):
        """
        Return the directory that contains the artifacts generated by
        this project.

        :return: A string
        """
        pass  # implemented in Ada

    def clear_attribute_values(self, attribute, package, index):
        """
         Clears the values list of an attribute.

         If the package is not specified, the attribute at the toplevel of the
         project is queried.

         The index only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value
           of this attribute
        """
        pass  # implemented in Ada

    def dependencies(self, recursive=False):
        """
        Returns the list of projects on which self depends (either directly
        if ``recursive`` is False, or including indirect dependencies if
        True).

        :param recursive: A boolean
        :return: A list of :class:`GPS.Project` instances

        """
        pass  # implemented in Ada

    def file(self):
        """
        Returns the project file.

        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def generate_doc(self, recursive=False):
        """
        Generates the documentation for the projet (and its subprojects
        if ``recursive`` is True) and displays it in the default browser.

        .. seealso: :func:`GPS.File.generate_doc`
        """

    def get_attribute_as_list(self, attribute, package='', index=''):
        """
        Fetches the value of the attribute in the project.

        If ``package`` is not specified, the attribute at the toplevel of the
        project is queried.

        ``index`` only needs to be specified if it applies to that attribute.

        If the attribute value is stored as a simple string, a list with a
        single element is returned. This function always returns the value of
        the attribute in the currently selected scenario.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value
            of this attribute
        :return: A list of strings

        .. seealso::

           :func:`GPS.Project.scenario_variables`

           :func:`GPS.Project.get_attribute_as_string`

           :func:`GPS.Project.get_tool_switches_as_list`

        .. code-block:: python

           # If the project file contains the following text:
           #
           #    project Default is
           #      for Exec_Dir use "exec/";
           #      package Compiler is
           #         for Switches ("file.adb") use ("-c", "-g");
           #      end Compiler;
           #    end Default;

           # Then the following commands;

           a = GPS.Project("default").get_attribute_as_list("exec_dir")
           => a = ("exec/")

           b = GPS.Project("default").get_attribute_as_list(
               "switches", package="compiler", index="file.adb")
           => b = ("-c", "-g")
        """
        pass  # implemented in Ada

    def get_attribute_as_string(self, attribute, package='', index=''):
        """
        Fetches the value of the attribute in the project.

        If ``package`` is not specified, the attribute at the toplevel of the
        project is queried.

        ``index`` only needs to be specified if it applies to that attribute.

        If the attribute value is stored as a list, the result string is a
        concatenation of all the elements of the list. This function always
        returns the value of the attribute in the currently selected
        scenario.

        When the attribute is not explicitely overridden in the project, the
        default value is returned. This default value is the one described in
        an XML file (see the GPS documentation for more information). This
        default value is not necessarily valid, and could for instance be a
        string starting with a parenthesis, as explained in the GPS
        documentation.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value
            of this attribute
        :return: A string, the value of this attribute

        .. seealso::

           :func:`GPS.Project.scenario_variables`

           :func:`GPS.Project.get_attribute_as_list`

           :func:`GPS.Project.get_tool_switches_as_string`

        .. code-block:: python

           # If the project file contains the following text:
           #    project Default is
           #      for Exec_Dir use "exec/";
           #      package Compiler is
           #         for Switches ("file.adb") use ("-c", "-g");
           #      end Compiler;
           #    end Default;

           a = GPS.Project("default").get_attribute_as_string("exec_dir")
           => a = "exec/"

           b = GPS.Project("default").get_attribute_as_string(
               "switches", package="compiler", index="file.adb")
           => b = "-c -g"
        """
        pass  # implemented in Ada

    def get_executable_name(self, main):
        """
        Returns the name of the executable, either read from the project or
        computed from `main`.

        :param GPS.File main: the main source file.
        :return: A string

        """
        pass  # implemented in Ada

    def get_property(self, name):
        """
        Returns the value of the property associated with the project. This
        property might have been set in a previous GPS session if it is
        persistent. An exception is raised if no such property exists for the
        project.

        :param name: A string
        :return: A string

        .. seealso:: :func:`GPS.Project.set_property`
        """
        pass  # implemented in Ada

    def get_tool_switches_as_list(self, tool):
        """
        Like :func:`get_attribute_as_list`, but specialized for the switches
        of ``tool``. Tools are defined through XML customization files, see
        the GPS documentation for more information.

        :param tool: The name of the tool whose switches you want to get
        :return: A list of strings

        .. seealso::

           :func:`GPS.Project.get_attribute_as_list`

           :func:`GPS.Project.get_tool_switches_as_string`

        .. code-block:: python

           # If GPS has loaded a customization file that contains the
           # following tags:
           #
           #    <?xml version="1.0" ?>
           #    <toolexample>
           #       <tool name="Find">
           #          <switches>
           #             <check label="Follow links" switch="-follow" />
           #          </switches>
           #       </tool>
           #    </toolexample>

           # The user will as a result be able to edit the switches for Find
           # in the standard Project Properties editor.

           # Then the Python command

           GPS.Project("default").get_tool_switches_as_list("Find")

           # will return the list of switches that were set by the user in the
           # Project Properties editor.

        """
        pass  # implemented in Ada

    def get_tool_switches_as_string(self, tool):
        """
        Like :func:`GPS.Project.get_attribute_as_string`, but specialized for
        a ``tool``.

        :param tool: The name of the tool whose switches you want to get
        :return: A string

        .. seealso:: :func:`GPS.Project.get_tool_switches_as_list`

        """
        pass  # implemented in Ada

    def is_harness_project(self):
        """
        Returns True if the project is a harness project generated by
        gnattest tool.

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_modified(self, recursive=False):
        """
        Returns True if the project has been modified but not saved yet. If
        ``recursive`` is true, the return value takes into account all
        projects imported by self.

        :param recursive: A boolean
        :return: A boolean
        """
        pass  # implemented in Ada

    def languages(self, recursive=False):
        """
        Returns the list of languages used for the sources of the project
        (and its subprojects if ``recursive`` is True). This can be used to
        detect whether some specific action in a module should be activated
        or not. Language names are always lowercase.

        :param recursive: A boolean
        :return: A list of strings

        .. code-block:: python

           # The following example adds a new menu only if the current project
           # supports C. This is refreshed every time the project is changed
           # by the user.

           import GPS
           c_menu=None

           def project_recomputed(hook_name):
             global c_menu
             try:
                ## Check whether python is supported
                GPS.Project.root().languages(recursive=True).index("c")
                if c_menu == None:
                   c_menu = GPS.Menu.create("/C support")
             except:
                if c_menu:
                   c_menu.destroy()
                   c_menu = None

           GPS.Hook("project_view_changed").add(project_recomputed)
        """
        pass  # implemented in Ada

    @staticmethod
    def load(filename, force=False, keep_desktop=False):
        """
        Loads a new project, which replaces the current root project, and
        returns a handle to it. All imported projects are also loaded at the
        same time. If the project is not found, a default project is loaded.

        If ``force`` is True, the user will not be asked whether to save the
        current project, whether it was modified or not.

        If ``keep_desktop`` is False, load the saved desktop configuration,
        otherwise keep the current one.

        :param filename: A string, the full path to a project file
        :param force: A boolean
        :param keep_desktop: A boolean
        :return: An instance of :class:`GPS.Project`
        """
        pass  # implemented in Ada

    def save(self):
        """
        Save the project. Return True if the project was saved without
        problems, False otherwise.

        :return: A bool indicating the saving status
        """

    def name(self):
        """
        Returns the name of the project. This does not include directory
        information; use :func:`self.file().name` if you want to access that
        information.

        :return: A string, the name of the project
        """
        pass  # implemented in Ada

    def object_dirs(self, recursive=False):
        """
        Returns the list of object directories for this project. If
        ``recursive`` is True, the source directories of imported projects is
        also returned. There might be duplicate directories in the returned
        list.

        :param recursive: A boolean
        :return: A list of strings
        """
        pass  # implemented in Ada

    def exec_dir(self):
        """
        Return the directory that contains the executables generated for the
        main programs of this project. This is either Exec_Dir or Object_Dir.

        :return: A string
        """
        pass  # implemented in Ada

    def original_project(self):
        """
        For given harness project returns the name of the original user
        project, which harness was generated from. Returns No_Project if this
        is not a harness project.

        :return: An instance of :class:`GPS.Project`
        """
        pass  # implemented in Ada

    def properties_editor(self):
        """
        Launches a graphical properties editor for the project.
        """
        pass  # implemented in Ada

    @staticmethod
    def recompute():
        """
        Recomputes the contents of a project, including the list of source
        files that are automatically loaded from the source directories. The
        project file is not reloaded from the disk and this should only be
        used if you have created new source files outside of GPS.

        .. code-block:: python

            GPS.Project.recompute()
        """
        pass  # implemented in Ada

    def remove_attribute_values(self, attribute, package, index, value):
        """
         Removes specific values from an attribute. You can specify as many
         values you need at the end of the param list.

         If ``package`` is not specified, the attribute at the toplevel of the
         project is queried.

         ``index`` only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value
            of this attribute
        :param value: A string, the name of the first value to remove

        .. seealso::

           :func:`GPS.Project.set_attribute_as_string`

           :func:`GPS.Project.add_attribute_values`

           :func:`GPS.Project.clear_attribute_values`

        For example:

        .. code-block:: python

           GPS.Project.root().remove_attribute_values(
               "Default_Switches", "Compiler", "ada", "-gnatwa", "-gnatwe");
        """
        pass  # implemented in Ada

    def remove_dependency(self, imported):
        """
        Removes a dependency between two projects. You must call
        :func:`GPS.Project.recompute` once you are done doing all the
        modifications on the projects.

        :param imported: An instance of GPS.Project

        .. seealso:: :func:`GPS.Project.add_dependency`
        """
        pass  # implemented in Ada

    def remove_property(self, name):
        """
        Removes a property associated with a project.

        :param name: A string

        .. seealso:: :func:`GPS.Project.set_property`
        """
        pass  # implemented in Ada

    def remove_source_dir(self, directory):
        """
        Removes a source directory from the project. You should call
        :func:`GPS.Project.recompute` after calling this method to recompute
        the list of source files. The directory is added only for the current
        value of the scenario variables.

        :param directory: A string

        .. seealso:: :func:`GPS.Project.add_source_dir`
        """
        pass  # implemented in Ada

    def rename(self, name, path='<current path>'):
        """
        Renames and moves a project file (the project is only put in the new
        directory when it is saved, but is not removed from its original
        directory). You must call :func:`GPS.Project.recompute` at some point
        after changing the name.

        :param name: A string
        :param path: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def root():
        """
        Returns the root project currently loaded in GPS.

        :return: An instance of GPS.Project

        .. code-block:: python

           print "Current project is " + GPS.Project.root().name()
        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables():
        """
        Returns the list of scenario variables for the current project
        hierarchy and their current values. These variables are visible at
        the top of the :guilabel:`Project` view in the GPS window. The
        initial value for these variables is set from the environment
        variables' value when GPS is started. However, changing the value of
        the environment variable later does not change the value of the
        scenario variable.

        :return: hash table associating variable names and values

        .. seealso:: :func:`GPS.Project.set_scenario_variable`

        For example:

        .. code-block:: python

           GPS.Project.scenario_variables()["foo"]
           => returns the current value for the variable foo

        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables_cmd_line(prefix=''):
        """
        Returns a concatenation of VARIABLE=VALUE, each preceded by
        ``prefix``. This string is generally used when calling external
        tools, for example, :program:`make` or GNAT.

        :param prefix: String to print before each variable in the output
        :return: a string

        .. code-block:: python

           # The following GPS action can be defined in an XML file, and will
           # launch the make command with the appropriate setup for the
           # environment
           # variables:
           # <action name="launch make"> \\
           #  <shell lang="python">GPS.scenario_variables_cmd_line()</shell> \\
           #  <external>make %1</external> \\
           # </action>
        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables_values():
        """
        Returns a hash table where keys are the various scenario variables
        defined in the current project and values the different values that
        this variable can accept.

        :return: A hash table of strings
        """
        pass  # implemented in Ada

    def search(self, pattern, case_sensitive=False, regexp=False,
               scope='whole', recursive=True):
        """
        Returns the list of matches for pattern in all the files belonging to
        the project (and its imported projects if recursive is true,
        which is the default). ``scope`` is a string, and should be any of
        'whole', 'comments', 'strings', 'code'. The latter will match only
        for text outside of comments.

        :param pattern: A string
        :param case_sensitive: A boolean
        :param regexp: A boolean
        :param scope: One of ("whole", "comments", "strings", "code")
        :param recursive: A boolean
        :return: A list of GPS.FileLocation instances
        """
        pass  # implemented in Ada

    def set_attribute_as_string(self, attribute, package, index, value):
        """
        Sets the value of an attribute. The attribute has to be stored as a
        single value.  If ``package`` is not specified, the attribute at the
        toplevel of the project is queried.  ``index`` only needs to be
        specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific
            value of this attribute
        :param value: A string, the name of the value to set

        .. seealso::

           :func:`GPS.Project.add_attribute_values()`

           :func:`GPS.Project.remove_attribute_values()`

           :func:`GPS.Project.clear_attribute_values()`
        """
        pass  # implemented in Ada

    def set_property(self, name, value, persistent=False):
        """
        Associates a string property with the project. This property is
        retrievable during the whole GPS session, or across GPS sessions if
        persistent is set to True.

        This is different than setting instance properties through Python's
        standard mechanism in that there is no guarantee that the same
        instance of :class:`GPS.Project` is created for each physical project
        on the disk and therefore you would not be able to associate a
        property with the physical project itself.

        :param name: A string
        :param value: A string
        :param persistent: A boolean

        .. seealso::

           :func:`GPS.Project.get_property`

           :func:`GPS.Project.remove_property`

           :func:`GPS.File.set_property`
        """
        pass  # implemented in Ada

    @staticmethod
    def set_scenario_variable(name, value):
        """
        Changes the value of a scenario variable. You need to call
        :func:`GPS.Project.recompute` to activate this change (so that
        multiple changes to the project can be grouped).

        If name does not correspond to an actual scenario variable in
        your project (i.e. the name of the variable in an "external(...)"
        typed expression), the corresponding environment variable is
        still changed. This might impact the reloading of the project,
        for instance when "external(...)" is used to construct the name
        of a directory, as in::

            for Object_Dir use external("BASE") & "/obj";

        :param name: A string
        :param value: A string

        .. seealso:: :func:`GPS.Project.scenario_variables`
        """
        pass  # implemented in Ada

    def source_dirs(self, recursive=False):
        """
        Returns the list of source directories for this project. If
        ``recursive`` is True, the source directories of imported projects is
        also returned. There might be duplicate directories in the returned
        list.

        :param recursive: A boolean
        :return: A list of strings

        .. seealso:: :func:`GPS.Project.add_source_dir`
        """
        pass  # implemented in Ada

    def sources(self, recursive=False):
        """
        Returns the list of source files for this project. If ``recursive``
        is true, all sources from imported projects are also
        returned. Otherwise, only the direct sources are returned. The
        basenames of the returned files are always unique: not two files with
        the same basenames are returned, and the one returned is the first
        one see while traversing the project hierarchy.

        :param recursive: A boolean
        :return: A list of instances of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def external_sources(self):
        """
        Return the list of all sources visible to the builder, but that are
        not part of a project. This includes sources found in one of the
        predefined directories for the builder, or sources found in the
        directories references in the ADA_SOURCE_PATH environment variable.

        :return: A list of instances of :class:`GPS.File`
        """
        pass  # implemented in Ada


###########################################################
# ProjectTemplate
###########################################################

class ProjectTemplate(object):

    """
    This class is used to manipulate GPS Project Templates.
    """

    @staticmethod
    def add_templates_dir(noname):
        """
        Adds a directory to the path in which GPS looks for templates.  GPS
        will look for project templates in immediate subdirectories of this
        directory.

        :param noname: A :class:`GPS.File` pointing to a directory.

        """
        pass  # implemented in Ada


###########################################################
# ReferencesCommand
###########################################################

class ReferencesCommand(Command):

    """
    This is the type of the commands returned by the references extractor.

    .. seealso::

       :func:`GPS.Command`

       :func:`GPS.Entity.references`
    """

    def get_result(self):
        """
        Returns the references that have been found so far by the command.

        :return: A list of strings

        .. seealso:: :func:`GPS.Entity.references`
        """
        pass  # implemented in Ada


###########################################################
# Revision
###########################################################

class Revision(object):

    """
    General interface to the revision browser.
    """

    @staticmethod
    def add_link(file, revision_1, revision_2):
        """
        Creates a link between ``revision_1`` and ``revision_2`` for
        ``file``.

        :param file: A string
        :param revision_1: A string
        :param revision_2: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def add_log(file, revision, author, date, log):
        """
        Adds a new log entry into the revision browser.

        :param file: A string
        :param revision: A string
        :param author: A string
        :param date: A string
        :param log: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def add_revision(file, revision, symbolic_name):
        """
        Registers a new symbolic name (tag or branches) corresponding to the
        ``revision`` of ``file``.

        :param file: A string
        :param revision: A string
        :param symbolic_name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def clear_view(file):
        """
        Clears the revision view of ``file``.

        :param file: A string
        """
        pass  # implemented in Ada


###########################################################
# Search
###########################################################

class Search(object):

    """
    This class provides an interface to the search facilities used for the
    GPS omni-search. In particular, this allows you to search file names,
    sources, and actions, etc.

    This class provides facilities exported directly by Ada, so you can
    for example look for file names by writting::

        s = GPS.Search.lookup(GPS.Search.FILE_NAMES)
        s.set_pattern("search", flags=GPS.Search.FUZZY)
        while True:
            (has_next, result) = s.get()
            if result:
                print result.short
            if not has_next:
                break

    However, one of the mandatory GPS plugins augments this base class with
    high-level constructs such as iterators and now you can write code as::

        for result in GPS.Search.search(
           GPS.Search.FILE_NAMES, "search", GPS.Search.FUZZY):
             print result.short

    Iterations are meant to be done in the background, so they are split into
    small units.

    It is possible to create your own search providers (which would be fully
    included in the omni-search of GPS) by subclassing this class, as in::

          class MySearchResult(GPS.Search_Result):
              def __init__(self, str):
                  self.short = str
                  self.long = "Long description: %s" % str

              def show(self):
                  print "Showing a search result: '%s'" % self.short

          class MySearchProvider(GPS.Search):
              def __init__(self):
                  # Override default so that we can build instances of our
                  # class
                  pass

              def set_pattern(self, pattern, flags):
                  self.pattern = pattern
                  self.flags = flags
                  self.current = 0

              def get(self):
                  if self.current == 3:
                      return (False, None)   # no more matches
                  self.current += 1
                  return (
                      True,  # might have more matches
                      MySearchResult(
                          "<b>match</b> %d for '%s' (flags=%d)"
                          % (self.current, self.pattern, self.flags)
                      )
                  )

          GPS.Search.register("MySearch", MySearchProvider())
    """

    FUZZY = 1
    SUBSTRINGS = 2
    REGEXP = 4
    """
    The various types of search, similar to what GPS provides in its
    omni-search.
    """

    CASE_SENSITIVE = 8
    WHOLE_WORD = 16
    """
    Flags to configure the search, that can be combined with the above.
    """

    FILE_NAMES = "File names"
    ACTIONS = "Actions"
    BUILDS = "Build"
    OPENED = "Opened"
    ENTITIES = "Entities"
    SOURCES = "Sources"
    BOOKMARKS = "Bookmarks"
    """
    The various contexts in which a search can occur.
    """

    def __init__(self):
        """
        Always raises an exception; use :func:`GPS.Search.lookup` to retrieve
        an instance.
        """

    def set_pattern(self, pattern, flags=0):
        """
        Sets the search pattern.

        :param pattern: a string
        :param flags: an integer, the combination of values such as
           GPS.Search.FUZZY, GPS.Search.REGEXP, GPS.Search.SUBSTRINGS,
           GPS.Search.CASE_SENSITIVE, GPS.Search.WHOLE_WORD
        """

    def get(self):
        """
        Returns the next occurrence of the pattern.

        :return: a tuple containing two elements; the first element is a
           boolean that indicates whether there might be further results;
           the second element is either None or an instance of
           GPS.Search_Result. It might be set even if the first element is
           False. On the other hand, it might be None even if there might
           be further results, since the search itself is split into small
           units. For instance, when searching in sources, each source file
           will be parsed independently. If a file does not contain a match,
           next() will return a tuple that contains True (there might be
           matches in other files) and None (there were no match found in the
           current file)
        """

    @staticmethod
    def lookup(self, name):
        """
        Looks up one of the existing search factories.

        :param name: a string, one of, e.g., GPS.Search.FILE_NAMES,
            GPS.Search.SOURCES
        """

    @staticmethod
    def register(name, factory, rank=-1):
        """
        Registers a new custom search.  This will be available to users via
        the omni-search in GPS, or via the :class:`GPS.Search` class.

        :param name: a string
        :param factory: an instance of :class:`GPS.Search` that will be reused
           every time the user starts a new search.
        :param rank: the search order for the provider. If negative, the new
           provider is added last. Other providers might be registered later,
           though, so the rank could change. User preferences will also
           override that rank.
        """

    @staticmethod
    def search(context, pattern, flags=SUBSTRINGS):
        """
        A high-level wrapper around lookup and set_pattern to make Python
        code more readable (see general documentation for
        :class:`GPS.Search`).

        :param context: a string, for example GPS.Search.SOURCES
        :param pattern: a string
        :param flags: an integer, see :func:`GPS.Search.set_pattern`
        """

    def next(self):
        """
        Results the next non-null result. This might take longer than
        :func:`get`, since it keeps looking until it actually finds a new
        result.  It raises StopIteration when there are no more results.
        """

    def __iter__(self):
        """
        Makes :class:`GPS.Search` compatible with python iterators.
        """

###########################################################
# Search
###########################################################


class Search_Result(object):

    """
    A class that represents the results found by :class:`GPS.Search`.
    """

    short = ""
    """The short description of the result"""

    long = ""
    """A long version of the description. For instance, when looking
    in sources it might contain the full line that matches"""

    def show(self):
        """
        Executes the action associated with the result. This action depends
        on where you were searching. For example, search in file names would
        as a result open the corresponding file; searching in bookmarks would
        jump to the corresponding location; search in actions would execute
        the corresponding action.
        """


###########################################################
# SemanticTree
###########################################################

class SemanticTree(object):
    """
    This class represents the semantic information known to GPS for
    a given file.
    """

    def __init__(self, file):
        """
        Creates a SemanticTree.

        :param file: A :class:`File`.
        """

    def is_ready(self):
        """
        Return True if and only if the semantic tree for this file is available

        :return: A boolean.
        """

    def update(self):
        """Ask for an immediate recomputation of the sematic tree.

        This should be used by custom implementations of semantic trees,
        to force GPS to ask for the new contents of the tree.
        """


###########################################################
# Style
###########################################################

class Style(object):

    """
    This class is used to manipulate GPS Styles, which are used, for example,
    to represent graphical attributes given to Messages.

    This class is fairly low-level, and we recommend using the class
    :py:func:`gps_utils.highlighter.OverlayStyle` instead. That class
    provides similar support for specifying attributes, but makes it easier
    to highlight sections of an editor with that style, or to remove the
    highlighting.

    """

    def __init__(self, name, create):
        """
        Creates a style.

        :param name: A String indicating the name of the style
        :param create: A :class:`File` indicating the file

        .. code-block:: python

           # Create a new style
           s=GPS.Style("my new style")

           # Set the background color to yellow
           s.set_background("#ffff00")

           # Apply the style to all the messages
           [m.set_style(s) for m in GPS.Message.list()]

        """
        pass  # implemented in Ada

    def get_background(self):
        """
        :return: a string, background of the style
        """
        pass  # implemented in Ada

    def get_foreground(self):
        """
        :return: a string, foreground of the style
        """
        pass  # implemented in Ada

    def get_in_speedbar(self):
        """
        Returns a Boolean indicating whether this style is shown in
        the speedbar.

        :return: a boolean
        """
        pass  # implemented in Ada

    def get_name():
        """
        :return: a string, the name of the style.
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns a list of all styles currently registered in GPS.

        :return: a list of :class:`GPS.Style`
        """
        pass  # implemented in Ada

    def set_background(self, noname):
        """
        Sets the background of style to the given color.

        :param noname: A string representing a color, for instance "blue" or
           "#0000ff"
        """
        pass  # implemented in Ada

    def set_foreground(self, noname):
        """
        Sets the foreground of style to the given color.

        :param noname: A string representing a color, for instance "blue" or
           "#0000ff"
        """
        pass  # implemented in Ada

    def set_in_speedbar(self, noname):
        """
        Whether this style should appear in the speedbar.

        :param noname: A Boolean
        """
        pass  # implemented in Ada


###########################################################
# SwitchesChooser
###########################################################

class SwitchesChooser(GUI):

    """
    This class represents a GTK widget that can be used to edit a tool's
    command line.
    """

    def __init__(self, name, xml):
        """
        Creates a new :class:`SwitchesChooser` widget from the tool's name
        and switch description in XML format.

        :param name: A string
        :param xml: A string
        """
        pass  # implemented in Ada

    def get_cmd_line(self):
        """
        Returns the tool's command line parameter.

        :return: A string
        """
        pass  # implemented in Ada

    def set_cmd_line(self, cmd_line):
        """
        Modifies the widget's aspect to reflect the command line.

        :param cmd_line: A string
        """
        pass  # implemented in Ada


###########################################################
# Task
###########################################################

class Task(object):

    """
    This class provides an interface to the background tasks being handled by
    GPS, such as the build commands, the query of cross references,
    etc. These are the same tasks that are visible through the GPS
    :guilabel:`Tasks` view.
    """

    visible = False

    EXECUTE_AGAIN = "execute_again"
    SUCCESS = "success"
    FAILURE = "failure"

    """
    Whether the task has a visible progress bar in GPS's toolbar or the
    Tasks view.
    """

    def __init__(self, name, execute, active=False, block_exit=False):
        """
        Create a task.

        :param name: A string identifying the task.
        :param execute: a function which takes the task as parameter and
            returns one of the constants:
                GPS.Task.EXECUTE_AGAIN if execute should be reexecuted by GPS
                GPS.Task.SUCCESS if the task has terminated successfully
                GPS.Task.FAILURE if the task has terminated unsuccessfully
        :param active: A boolean. By default the 'execute' functions are
            executed in the background approximately every 100ms - setting
            this to True makes GPS run the 'execute' function much more
            aggressively, every time the GUI is idle. Use this with caution,
            as this might impact the responsiveness of the user interface.
        :param block_exit: A boolean. Set this to True if a confirmation
            popup should appear when the task is running and GPS has been
            asked to quit.
        """
        pass  # implemented in Ada

    def interrupt(self):
        """
        Interrupts the task.
        """
        pass  # implemented in Ada

    def block_exit(self):
        """
        Returns True if and only if this task should block the exit of GPS.

        :return: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        :return: a list of :class:`GPS.Task`, all running tasks
        """
        pass  # implemented in Ada

    def name(self):
        """
        Returns the name of the task.

        :return: A string
        """
        pass  # implemented in Ada

    def pause(self):
        """
        Pauses the task.
        """
        pass  # implemented in Ada

    def resume(self):
        """
        Resumes the paused task.
        """
        pass  # implemented in Ada

    def progress(self):
        """
        Returns the current progress of the task.

        :return: A list containing the current step and the total steps
        """
        pass  # implemented in Ada

    def status(self):
        """
        Returns the status of the task.

        :return: A string
        """
        pass  # implemented in Ada

    def set_progress(self, current, total):
        """
        Sets the progress indication for this task.

        :param current: an integer, the current progress.
        :param total: an integer, the total progress.
        """
        pass  # implememnted in Ada


###########################################################
# Timeout
###########################################################

class Timeout(object):

    """
    This class gives access to actions that must be executed regularly at
    specific intervals.

    .. seealso:: :func:`GPS.Timeout.__init__`

    .. code-block:: python

       ## Execute callback three times and remove it
       import GPS;

       def callback(timeout):
          if not hasattr(timeout, "occur"):
              return True

          timeout.occur += 1
          print "A timeout occur=" + `timeout.occur`
          if timeout.occur == 3:
             timeout.remove()

       t = GPS.Timeout(500, callback)
       t.occur = 0
    """

    def __init__(self, timeout, action):
        """
        A timeout object executes a specific action repeatedly, at a
        specified interval, as long as it is registered.  The action takes a
        single argument, the instance of :class:`GPS.Timeout` that called it.

        :param timeout: The timeout in milliseconds at which
           to execute the action
        :param action: A subprogram parameter to execute periodically
        """
        pass  # implemented in Ada

    def remove(self):
        """
        Unregisters a timeout.
        """
        pass  # implemented in Ada


###########################################################
# Unexpected_Exception
###########################################################

class Unexpected_Exception(Exception):

    """
    An exception raised by GPS. It indicates an internal error in GPS, raised
    by the Ada code itself. This exception is unexpected and indicates a bug
    in GPS itself, not in the Python script, although it might be possible to
    modify the latter to work around the issue.
    """
    pass  # implemented in Ada


###########################################################
# Valgrind
###########################################################

class Valgrind(object):
    """
    This class helps testing GPS. To use it run GPS under valgrind and call
    corresponding methods to turn on/off callgrin for intresting part of
    GPS execution. See more info in valgrind documentation.
    """

    @staticmethod
    def callgrind_dump_stats():
        """
        Force generation of a profile dump at specified position in code,
        for the current thread only. Written counters will be reset to zero.
        """

    @staticmethod
    def callgrind_start_instrumentation():
        """
        Start full Callgrind instrumentation if not already enabled.
        """

    @staticmethod
    def callgrind_stop_instrumentation():
        """
        Stop full Callgrind instrumentation if not already disabled.
        """

    @staticmethod
    def callgrind_toggle_collect():
        """
        Toggle the collection state. This allows to ignore events with
        regard to profile counters.
        """

    @staticmethod
    def callgrind_zero_stats():
        """
        Reset the profile counters for the current thread to zero.
        """


###########################################################
# VCS2
###########################################################

class VCS2(object):
    """
    An interface to a version control engine.

    One project is always associated with at most one version control,
    which is used for all of its sources.

    However, in a given tree of projects, there can be multiple such
    engines, if different repositories are used for the sources (for
    instance a third-party repository is imported), possibly for different
    implementations of version control (one for git, one for subversion...)

    As a result, for a given source file you first need to find the
    relevant engine, via a call to `GPS.VCS2.get`.

    For efficiency, GPS caches the status of files locally, and only refreshes
    at specific points. To get the status of a file as currently cached, use
    `GPS.VCS2.get_file_status`. This will always return a valid status, even if
    the cache has never been initialized by querying the actual VCS on the
    disk. To do this, call one of `GPS.VCS2.ensure_status_for_*` methods. These
    methods will eventually run the `vcs_file_status_update` hook to let you
    know that the status has changed. This is all implemented asynchronously
    though, since such a query might take time.

    This class provides the user view for VCS engines.

    In practice, it is further derived in the code, to provide support for
    various VCS engines like git, CVS, subversion, clearcase,... The hierarchy
    is::

                   GPS.VCS2
                      |
                   vcs2.VCS   (abstract)
                      |
                +-----+-----+
                |     |     |
              Git   CVS    Subversion
    """

    Actions = enum('VCS2.Actions', DOUBLE_CLICK=0, TOOLTIP=1, ADD=2,
                   REMOVE=3, RENAME=4)
    Status = enum('VCS2.Status', UNMODIFIED=2**0, MODIFIED=2**1)

    class Branch:
        def __init__(self, name, active, annotation, id):
            pass

    class Commit:
        def __init__(self, id, author, date, subject, parents, names=None,
                     flags=0):
            pass

    @staticmethod
    def _register(name, construct, default_status, discover_repo):
        """
        Register support for a new Version Control System.
        This function is not meant to be called directly. Instead, check the
        vcs/__init__.py plugin.

        :param str name: the name of the system, as it should be set in
           the project properties IDE.VCS_Kind attribute.
        :param construct: a function that takes a GPS.File (the working
           directory location) and returns an instance of GPS.VCS.
        :param int default_status: the default status for files not in the
           cache yet.
        :param discover_repo: a function that takes a :class:`GPS.File`, and
           returns a string. This function tries to guess the repository for
           the given file.
        """

    @staticmethod
    def active_vcs(self):
        """
        Return the currently active VCS. When the project uses a single VCS,
        it will always be the same instance. But when the project tree has
        multiple VCS, or the same VCS but multiple working directories, this
        will return the instance selected by the user in the local toolbar
        of the VCS views.

        :rtype: `GPS.VCS2`

        .. seealso: `GPS.VCS2.vcs_in_use`
        """

    @staticmethod
    def vcs_in_use(self):
        """
        Return the list of all VCS in use for the loaded project and its
        imported projects.

        :rtype: [GPS.VCS2]
        """

    @staticmethod
    def get(project):
        """
        Return the VCS to use for the files in a given project.
        Each project can have its own VCS, if for instance it is imported
        from another repository.

        :param GPS.Project project:
        :rtype: `GPS.VCS2`
        """

    @staticmethod
    def supported_systems():
        """
        Shows the list of supported VCS systems.

        :return: List of strings
        """
        pass  # implemented in Ada

    @property
    def name(self):
        """
        Return the name of the VCS (as could be set in the project's
        IDE.VCS_Kind attribute). This is always lower-cased.

        :type: str
        """

    def ensure_status_for_files(self, files):
        """
        Make sure that all files has a known status in self's cache.
        This is computed asynhronously.

        :param List[GPS.File] files:
        """

    def ensure_status_for_project(self, project):
        """
        Make sure that all source files of the project have a known status
        in self's cache.
        This is computed asynhronously.

        :param GPS.Project project:
        """

    def ensure_status_for_all_source_files(self):
        """
        Ensure that all source files in any of the loaded project have a
        known status in self's cache. This doesn't ensure that the status
        for files that are under version control but not part of the project
        sources is also computed, although in most cases the VCS engine
        will indeed compute them.

        This is computed asynhronously.
        """

    def set_run_in_background(self, background):
        """
        Should be called to let GPS know when background commands are
        executing. This is used to queue commands instead of running
        several of them in parallel.
        Do not call this function directly. Instead, use the python
        function vcs2.core.run_in_background which provides a higher-level
        API for this purpose.
        """

    def get_file_status(self, file):
        """
        Return the file status, as seen in self'cache.

        :param GPS.File file:
        :returntype: a tuple (GPS.VCS2.Status, str, str)
           where the two strings are the version and the repository version
        """

    def invalidate_status_cache(self):
        """
        Mark all entries in self's cache as no longer valid. This will force
        a refresh next time one of the `ensure_status_*` method is called.
        """

    def _set_file_status(self, files, status, version, repo_version):
        """
        Modifies self's cache.
        This function is meant to be called only by the implementation of
        specific VCS engines.

        :param List(GPS.File) files:
        :param GPS.VCS2.Status status:
        :param str version:
        :param str repo_version:
        """

    def _override_status_display(self, status, label, icon_name):
        """
        Override the label and icon to use for a given status.
        This function is meant to be called only by the implementation of
        specific VCS engines.

        :param GPS.VCS2.Status status:
        :param str label:
        :param str icon_name:
        """


###########################################################
# VCS2_Task_Visitor
###########################################################

class VCS2_Task_Visitor(object):
    """
    A class used in `GPS.VCS2.async_fetch_history`.
    This is only used when writing your own support for VCS engines.
    """

    def success(self, msg=''):
        """
        This should be called whenever an action succeed.
        It is used to perform various cleanups on the Ada side (for instance
        recomputing the status of files).

        :param str msg: If specified, a temporary popup is displayed to the
           user showing the message. The popup automatically disappears after
           a short while.
        """

    def history_lines(self, list):
        """
        Report when a new line for the VCS history was seen. Used from
        `GPS.VCS2.async_fetch_history`.

        :param List(GPS.VCS2.Commit) list: a list of lines from the history.
           This doesn't have to be the whole log, though, although it is
           more efficient to send bigger chunks.
        """

    def set_details(self, id, header, message):
        """
        Used to provide details on one specific commit, from the
        `GPS.VCS2.async_fetch_commit_details` method.

        :param str id: the commit for which we are reporting details
        :param str header: a multi-string piece of information to
            display in the History view. This should show the commit
            id, the date and author of the commit,...
        :param str message: a multi-string description of the commit
            message, and possibly a diff of what has changed.
        """

    def diff_computed(self, diff):
        """
        Used to report a diff, from `GPS.VCS2.async_diff`.

        :param str diff: the diff, using standard diff format.
        """

    def file_computed(self, contents):
        """
        Used to provide the contents of a file at a specific version.

        :param str contents: the contents of the file.
        """

    def annotations(self, file, first_line, ids, annotations):
        """
        Report annotations to add to the side of the editors. Such
        annotations should provide author, last modification date,
        commit id,... for each line.

        :param GPS.File file: the file for which we add annotations
        :param int first_line: the first line number for which we
           return information.
        :param List(str) ids: the commit ids, for each line.
        :param List(str) annotations: the annotations. The first
           entry is for `first_line`, then the next line, and so on.
        """

    def branches(self, category, iconname, can_rename, branches):
        """
        Report a list of branches available for the current VCS.

        :param str category: the name of the category, as displayed
           in the Branches view. You can call this method several times
           for the same category if need be.
           If the category is 'BRANCHES', it will be expanded in the GUI to
           show all the branches within.
        :param str iconname: icon to use for this category.
        :param bool can_rename: true if the branches can be renamed.
        :param List branches: a list of branches (see `GPS.VCS2.Branch`).
         """

    def tooltip(self, text):
        """
        Report additonal text to display in tooltips.
        In particular, this is called in the Branches view, as a result of
        calling the VCS engine's `async_action_on_branch` method.

        :param str text: additional text for the tooltip
        """


###########################################################
# Vdiff
###########################################################

class Vdiff(object):

    """
    This class provides access to the graphical comparison between two or
    three files or two versions of the same file within GPS. A visual diff is
    a group of two or three editors with synchronized scrolling. Differences
    are rendered using blank lines and color highlighting.
    """

    @staticmethod
    def __init__():
        """
        This function prevents the creation of a visual diff instance
        directly. You must use :func:`GPS.Vdiff.create` or
        :func:`GPS.Vdiff.get` instead.

        .. seealso::

           :func:`GPS.Vdiff.create`

           :func:`GPS.Vdiff.get`
        """
        pass  # implemented in Ada

    def close_editors(self):
        """
        Closes all editors involved in a visual diff.
        """
        pass  # implemented in Ada

    @staticmethod
    def create(file1, file2, file3=''):
        """
        If none of the files given as parameter is already used in a visual
        diff, creates a new visual diff and returns it. Otherwise, None is
        returned.

        :param file1: An instance of :class:`GPS.File`
        :param file2: An instance of :class:`GPS.File`
        :param file3: An instance of :class:`GPS.File`
        :return: An instance of :class:`GPS.Vdiff`
        """
        pass  # implemented in Ada

    def files(self):
        """
        Returns the list of files used in a visual diff.

        :return: A list of GPS.File

        """
        pass  # implemented in Ada

    @staticmethod
    def get(file1, file2='', file3=''):
        """
        Returns an instance of an already exisiting visual diff. If an
        instance already exists for this visual diff, it is returned. All
        files passed as parameters must be part of the visual diff but not
        all files of the visual diff must be passed for the visual diff to be
        returned. For example if only one file is passed, the visual diff
        that contains it, if any, is returned even if it is a two or three
        file visual diff.

        :param file1: An instance of :class:`GPS.File`
        :param file2: An instance of :class:`GPS.File`
        :param file3: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of visual diffs currently opened in GPS.

        :return: A list :class:`GPS.Vdiff`

        .. code-block:: python

           # Here is an example that demonstrates how to use GPS.Vdiff.list to
           # close all the visual diff.

           # First two visual diff are created
           vdiff1 = GPS.Vdiff.create(GPS.File("a.adb"), GPS.File("b.adb"))
           vdiff2 = GPS.Vdiff.create(GPS.File("a.adb"), GPS.File("b.adb"))

           # Then we get the list of all current visual diff
           vdiff_list = GPS.Vdiff.list()

           # And we iterate on that list in order to close all editors used in
           # each visual diff from the list.

           for vdiff in vdiff_list:
              files = vdiff.files()

              # But before each visual diff is actually closed, we just inform
              #  the user of the files that will be closed.

              for file in files:
                 print "Beware! " + file.name () + "will be closed."

              # Finally, we close the visual diff

              vdiff.close_editors()
        """
        pass  # implemented in Ada

    def recompute(self):
        """
        Recomputes a visual diff. The content of each editor used in the
        visual diff is saved. The files are recompared and the display is
        redone (blank lines and color highlighting).
        """
        pass  # implemented in Ada


###########################################################
# XMLViewer
###########################################################

class XMLViewer(object):

    """
    This class represents Tree-based views for XML files.

    """

    def __init__(self, name, columns=3, parser=None, on_click=None,
                 on_select=None, sorted=False):
        """
        Creates a new XMLViewer, named ``name``.

        ``columns`` is the number of columns that the table representation
        should have. The first column is always the one used for sorting the
        table.

        ``parser`` is a subprogram called for each XML node that is
        parsed. It takes three arguments: the name of the XML node being
        visited, its attributes (in the form "attr='foo' attr="bar""), and
        the text value of that node. This subprogram should return a list of
        strings, one per visible column create for the table. Each element
        will be put in the corresponding column.

        If ``parser`` is not specified, the default is to display in the
        first column the tag name, in the second column the list of
        attributes, and in the third column when it exists the textual
        contents of the node.

        ``on_click`` is an optional subprogram called every time the user
        double-clicks on a line, and is passed the same arguments as
        ``parser``. It has no return value.

        ``on_select`` has the same profile as ``on_click``, but is called
        when the user has selected a new line, not double-clicked on it.

        If ``sorted`` is True, the resulting graphical list is sorted on the
        first column.

        :param name: A string
        :param columns: An integer
        :param parser: A subprogram
        :param on_click: A subprogram
        :param on_select: A subprogram
        :param sorted: A boolean

        .. code-block:: python


            # Display a very simply tree. If you click on the file name,
            # the file will be edited.
            import re

            xml = '''<project name='foo'>
                 <file>source.adb</file>
              </project>'''

            view = GPS.XMLViewer("Dummy", 1, parser, on_click)
            view.parse_string(xml)

            def parser(node_name, attrs, value):
               attr = dict()
               for a in re.findall('''(\\w+)=['"](.*?)['"]\B''', attrs):
                  attr[a[0]] = a[1]

               if node_name == "project":
                  return [attr["name"]]

               elif node_name == "file":
                  return [value]

            def on_click(node_name, attrs, value):
               if node_name == "file":
                  GPS.EditorBuffer.get(GPS.File(value))
        """
        pass  # implemented in Ada

    @staticmethod
    def get_existing(name):
        """
        Returns a :class:`XMLViewer` instance if ``name`` corresponds to
        an existing XMLViewer window. If no :class:`XMLViewer`window has been
        found for the given ``name``, returns None instead.

        :param string name: A string

        """
        pass  # implemented in Ada

    @staticmethod
    def create_metric(name):
        """
        Creates a new :class:`XMLViewer` for an XML file generated by
        gnatmetric.  ``name`` is the name for the window.

        :param string name: A string

        """
        pass  # implemented in Ada

    def parse(self, filename):
        """
        Replaces the contents of self by that of the XML file.

        :param filename: An XML file
        """
        pass  # implemented in Ada

    def parse_string(self, str):
        """
        Replaces the contents of self by that of the XML string ``str``.

        :param str: A string
        """
        pass  # implemented in Ada

###########################################################
# Alias
###########################################################


class Alias(object):

    """
    This class represents a GPS Alias, a code template to be expanded in an
    editor. This class allows you to manipulate them programmatically.
    """

    @staticmethod
    def get(name):
        """
        Gets the alias instance corresponding to ``name``.
        """

###########################################################
# Completion
###########################################################


class Completion(object):

    """
    This class is used to handle editor completion.
    See the documentation in the :file:`completion.py` plugin.
    """

    @staticmethod
    def register(resolver, language):
        """
        Registers a resolver, which inherits from :class:`CompletionResolver`.
        language is a string indicating which language this resolver supports.
        """

###########################################################
# OutputParserWrapper
###########################################################


class OutputParserWrapper(object):

    """
    This class is used to handle user-defined tool output parsers.  Parsers
    are organized in chains. Output of one parser is passed as input to next
    one. Chains of parser could be attached to a build target.  This class is
    for internal use only. Instead users should inherit custom parser from
    :class:`OutputParser` defined in :file:`tool_output.py`, but their
    methods match.

    .. code-block:: python

       # Here is an example of custom parser:
       #
       import GPS, tool_output

       class PopupParser(tool_output.OutputParser):
         def on_stdout(self,text,command):
           GPS.MDI.dialog (text)
           if self.child != None:
             self.child.on_stdout (text,command)

    You can attach custom parser to a build target by specifying it in an XML
    file.

    .. code-block:: python

       <target model="myTarget" category="_Run" name="My Target">
          <output-parsers>[default] popupparser</output-parsers>
       </target>

    Where [default] abbreviates names of all parsers predefined in GPS.
    """

    def __init__(self, child=None):
        """
        Creates a new parser and initialize its child reference, if provided.
        """
        pass  # implemented in Ada

    def on_stdout(self, text, command):
        """
        Called each time a portion of output text is ready to parse. Takes
        the portion of ``text`` as a parameter and passes filtered portion to
        its child.
        """
        pass  # implemented in Ada

    def on_stderr(self, text, command):
        """
        Like :func:`on_stdout`, but for the error stream.
        """
        pass  # implemented in Ada

    def on_exit(self, status, command):
        """
        Called when all output is parsed to flush any buffered data at end of
        the stream.
        """
        pass  # implemented in Ada


class Icon(object):

    def __init__(self, id, label, path, alt_menu=None, alt_small_toolbar=None,
                 alt_large_toolbar=None, alt_local_toolbar=None,
                 alt_button=None, alt_dnd=None, alt_dialog=None):
        """
        This will create a new icon with specified parameters, and add it to
        GPS stock icons. The returned instance is a placeholder that has no
        methods at the moment.

        :param string id: The id of the icon
        :param string label: The label of the icon
        :param string path: The path of the default icon file
        :param string alt_menu: The path for the alternate menu icon file,
            typically in a 16x16 format.
        :param string alt_small_toolbar: The path for the alternate small
            toolbar icon file, typically in a 18x18 format.
        :param string alt_large_toolbar: The path for the alternate large
            toolbar icon file, typically in a 24x24 format.
        :param string alt_local_toolbar: The path for the alternate local
            toolbar icon file, typically in a 12x12 format.
        :param string alt_button: The path for the alternate button icon
            file, typically in a 20x20 format.
        :param string alt_dnd: The path for the alternate drag&drop
            operation icon file, typically in a 32x32 format.
        :param string alt_dialog: The path for the main image in a dialog,
            typically 48x48 pixels.
        """
        pass


class History(object):
    """
    This class gives access to GPS internal settings. These settings are
    used in addition to the preferences, and are used to keep information
    such as the list of files recently opened, or the state of various
    check boxes in the interface so that GPS can display them again in the
    same state when it is restarted.
    """

    def __init__(self):
        """
        No instances of this class can be created.
        """

    @staticmethod
    def add(key, value):
        """
        Update the value of one of the settings. The new value is added to
        the list (for instance for recently opened files), and the oldest
        previous value might be removed, depending on the maximum number
        of elements that GPS wants to preserve for that key.
        """


class Construct(object):
    """
    One node of the semantic tree when parsing a file for a given programming
    language.
    Instances of such classes are only created by GPS internally

    .. seealso: GPS.Language.clicked_on_construct
    """

    def __init__(self):
        """Instances are only created by GPS itself"""

    name = ''
    """The name of the construct"""

    file = ''
    """The GPS.File in which the construct occurs"""

    start = (0, 0, 0)
    """The source location for the beginning of this construct,
       (line, column offset)"""

    id = ''
    """Unique id for the entity"""


class ConstructsList(object):
    """
    This class is closely associated with the :class:`GPS.Language` class,
    and is used by plug-ins to describe the semantic organization in a source
    file.

    This can be used in particular to populate the Outline view for custom
    languages (see the :file:`python_support.py` plugin in the GPS sources).
    """

    def __init__(self):
        """Instances are only created by GPS itself"""

    def add_construct(self, category, is_declaration, visiblity, name,
                      profile, sloc_start, sloc_end, sloc_entity, id=""):
        """
        Register a new semantic construct from the file.

        :param int category: the name of the category. It should be one of the
           CAT_* constants in the :file:`constructs.py` module. If your
           language has different constructs, you should map them to one of the
           existing categories.

        :param bool is_declaration: whether this is the declaration for the
           entity, or a reference to it.

        :param int visibility: whether the entity is public, protected or
           private. It should be one of the constants in the
           :file:`constructs.py` module.

        :param str name: the name of the entity

        :param str profile: a description of its profile (the list of
           parameters for a subprogram, for instance).

        :param (int,int,int) sloc_start: the position at which this constructs
           starts. This is a tuple (line, column, offset), where offset is the
           number of bytes since the start of the file.
        :param (int,int,int) sloc_end: the position at which this constructs
           ends. This is a tuple (line, column, offset).
        :param (int,int,int) sloc_entity: the position at which the entity name
           starts. This is a tuple (line, column, offset).
        :param str id: a unique identifier for this identity. You can retrieve
           it in calls to :func:`GPS.Language.clicked_on_construct`, and this
           is used to identify overloading identifiers in the Outline view
           when it is refreshed.
        """


class Language(object):
    """
        A few methods can be overridden when you create your own child class
        of :class:`GPS.Language`, to provide support for the Outline view.
        They are not defined by default, and thus the documentation is given
        below:

        def parse_constructs(self, constructs_list, gps_file, content_string):
            '''
            Abstract method that has to be implemented by the subclasses.

            Given an empty list of constructs, a file instance and a string
            containing the contents of the file, this needs to populate the
            list of language constructs. In turn this will give support for a
            number of features in GPS including:

            - Outline support
            - Block highlighting/folding support
            - Entity search support

            ..  seealso: :class:`GPS.ConstructsList`
            ..  seealso: :func:`GPS.Language.should_refresh_constructs`

            :param GPS.ConstructList constructs_list: The list of constructs to
                populate.
            :param GPS.File gps_file: the name of the file to parse.
            :param str content_string: The content of the file

            '''

        def should_refresh_constructs(self, file):
            '''
            Whether GPS should call parse_constructs to refresh the list.
            This is called when the file has not changed on the disk, but GPS
            thinks there might be a need to refresh because various hooks have
            been run.
            By default, this returns False, so that parse_constructs is only
            called when the file changes on the disk.

            :param GPS.File file: the file to test
            :return: a bool

            '''

        def clicked_on_construct(self, construct):
            '''
            Called when the user wants to jump to a specific construct.
            The default is to open an editor for the file/line/column.

            :param GPS.Construct construct: the construct as build in
               :func:`GPS.Language.parse_constructs`.

            '''

        def get_last_selected_construct_id(self, file):
            '''
            Called when the Outline view needs to reselect in its tree view
            the construct that was selected just before leaving the view
            associated with the given file.

            This function should return the string ID of the last selected
            construct for the given file.

            :param GPS.File file: the file that is associated with the Outline
            :return: a string
            '''
    """

    @staticmethod
    def register(instance, name, body_suffix, spec_suffix="", obj_suffix="",
                 indentation_kind=INDENTATION_SIMPLE):
        """
        Register an instance of language in GPS.

        :param Language instance: The instance you want to register
        :param string name: The name of the language
        :param body_suffix: The file suffix for the language - ".c" for the C
            language for example
        :param spec_suffix: The file suffix for specification files for the
            language, if it applies - ".h" for the C language.
        :param obj_suffix: The suffix for object files produced for the
            language, if it applies - ".o" for the C language.
        :param int indentation_kind: One of the INDENTATION_NONE,
            INDENTATION_SIMPLE or INDENTATION_EXTENDED constants defined in
            the constructs module, defining the way the language will be
            indented.
        """
        pass

    @staticmethod
    def get(name):
        """
        Return a description of the language, from its name.
        For instance::

             GPS.Language.get('ada').keywords

        or::

             GPS.EditorBuffer.get().get_lang().keywords

        :return: a :class:`GPS.LanguageInfo`
        """


class LanguageInfo(object):
    """
    This class gives access to various information known about the
    programing languages supported by GPS.
    """

    name = ''
    """
    Return the name of the language
    """

    keywords = ''
    """
    Return a regular expression that can be used to test whether a
    string is a keyword for the language. The regexp is anchored with
    '^' and ends with '\\\\b' (word separator).
    """


class OutlineView(object):
    """
    This class gives access to suprograms used to control the GPS Outline
    view (e.g: selecting a specific construct in the Ouline view).
    """

    @staticmethod
    def select_construct(id):
        """
        Select the construct idetified with the given id in the Outline
        view.

        An exception is raised when the id does not match a construct
        in the Outline view.
        """

        pass  # implemented in Ada


###########################################################
# Globals
###########################################################


def __run_hook__():
    """
    Internal function used for the support of hooks.
    """
    pass  # implemented in Ada


def add_location_command(command):
    """
    Adds a command to the navigation buttons in the toolbar. When the user
    presses the :guilabel:`Back` button, this command is executed and puts GPS
    in a previous state. This is, for example, used while navigating in the
    HTML browsers to handle their :guilabel:`Back` button.

    :param command: A string

    """
    pass  # implemented in Ada


def base_name(filename):
    """
    Returns the base name for the given full path name.

    :param filename: A string

    """
    pass  # implemented in Ada


def cd(dir):
    """
    Changes the current directory to ``dir``.

    :param dir: A string

    """
    pass  # implemented in Ada


def clear_cache():
    """
    Frees the internal cache used for return values. This function needs to
    be called explicitly.  If not, previously returned value are never
    freed. After calling this function, you can no longer use %1, %2,... to
    refer to previously returned values.
    """
    pass  # implemented in Ada


def compute_xref():
    """
    Updates the cross-reference information stored in GPS. This needs to be
    called after major changes to the sources only, since GPS itself is able
    to work with partially up-to-date information

    """
    pass  # implemented in Ada


def compute_xref_bg():
    """
    Updates cross-reference information stored in GPS in the background.

    .. seealso:: :func:`GPS.compute_xref`

    """
    pass  # implemented in Ada


def contextual_context():
    """
    Returns the context at the time the contextual menu was open.

    This function only returns a valid context while the menu is open or
    while an action executed from that menu is being executed. You can store
    your own data in the returned instance so that, for example, you can
    precompute some internal data in the filters for the contextual actions
    (see <filter> in the XML files) and reuse that precomputed data when the
    menu is executed.  See also the documentation for the
    "contextual_menu_open" hook.

    :return: An instance of :class:`GPS.Context`

    .. seealso:: :func:`GPS.current_context`

    .. code-block:: python

       # Here is an example that shows how to precompute some data when we
       # decide whether a menu entry should be displayed in a contextual menu,
       # and reuse that data when the action executed through the menu is
       # reused.

       import GPS

       def on_contextual_open(name):
          context = GPS.contextual_context()
          context.private = 10
          GPS.Console().write("creating data " + `context.private` + '\\n')

       def on_contextual_close(name):
          context = GPS.contextual_context()
          GPS.Console().write("destroying data " + `context.private` + '\\n')

       def my_own_filter():
          context = GPS.contextual_context()
          context.private += 1
          GPS.Console().write("context.private=" + `context.private` + '\\n')
          return 1

       def my_own_action():
          context = GPS.contextual_context()
          GPS.Console().write("my_own_action " + `context.private` + '\\n')

       GPS.parse_xml('''
          <action name="myaction%gt;"
             <filter shell_lang="python"
                     shell_cmd="contextual.my_own_filter()" />
             <shell lang="python">contextual.my_own_action()</shell>
          </action>

          <contextual action="myaction">
             <Title>Foo1</Title>
          </contextual>
          <contextual action="myaction">
             <Title>Foo2</Title>
          </contextual>
        ''')

       GPS.Hook("contextual_menu_open").add(on_contextual_open)
       GPS.Hook("contextual_menu_close").add(on_contextual_close)


    .. code-block:: python

       # The following example does almost the same thing as the above, but
       # without relying on the hooks to initialize the value. We set the
       # value in the context the first time we need it, instead of every
       # time the menu is opened.

       import GPS

       def my_own_filter2():
          try:
             context = GPS.contextual_context()
             context.private2 += 1

          except AttributeError:
             context.private2 = 1

          GPS.Console().write("context.private2=" + `context.private2` + '\\n')
          return 1

       def my_own_action2():
          context = GPS.contextual_context()
          GPS.Console().write(
             "my_own_action, private2=" + `context.private2` + '\\n')

       GPS.parse_xml('''
          <action name="myaction2">
             <filter shell_lang="python"
                     shell_cmd="contextual.my_own_filter2()" />
             <shell lang="python">contextual.my_own_action2()</shell>
          </action>
          <contextual action="myaction2">
             <Title>Bar1</Title>
          </contextual>
          <contextual action="myaction2">
             <Title>Bar2</Title>
          </contextual>
       ''')
    """
    pass  # implemented in Ada


def current_context(refresh=False):
    """
    Returns the current context in GPS. This is the currently selected file,
    line, column, project, etc. depending on what window is currently
    active. From one call of this function to the next, a different instance
    is returned, so you should not store your own data in the
    instance, since you will not be able to recover it later on

    :param boolean refresh: If false, the last compute context is returned.
       The context is set by the views whenever their selection change. You
       can however set this parameter to true to force a recomputation of the
       context. This is only useful when your script has executed a number of
       commands and needs to ensure that the context is properly refresh
       synchronously.

    :return: An instance of :class:`GPS.Context`

    .. seealso::

       :func:`GPS.Editor.get_line`

       :func:`GPS.MDI.current:` Access the current window

       :func:`GPS.contextual_context`
    """
    pass  # implemented in Ada


def debug__usage(size):
    """
    Dumps on stdout the largest (by size) memory allocators in GPS. This is
    meant as a debug function for GPS developers.

    :param size: An integer

    """
    pass  # implemented in Ada


def delete(name):
    """
    Deletes the file or directory ``name`` from the file system.

    :param name: A string
    """
    pass  # implemented in Ada


def dir(pattern=''):
    """
    Lists files matching ``pattern`` (all files by default).

    :param pattern: A string
    :return: A list of strings
    """
    pass  # implemented in Ada


def dir_name(filename):
    """
    Returns the directory name for the given full path name.

    :param filename: A string
    """
    pass  # implemented in Ada


def dump(string, add_lf=False):
    """
    Dumps ``string`` to a temporary file. Return the name of the file. If
    ``add_lf`` is True, appends a line feed at end of the name.

    :param string: A string
    :param add_lf: A boolean
    :return: A string, the name of the output file
    """
    pass  # implemented in Ada


def dump_file(text, filename):
    """
    Writes text to the file specified by ``filename``. This is mostly
    intended for poor shells like the GPS shell which do not have better
    solutions. In Python, you should use its own mechanisms.

    :param text: A string
    :param filename: A string
    """
    pass  # implemented in Ada


def echo(*args):
    """
    Displays a line of text. This command is specific to the GPS shell.

    :param args: Any number of parameters
    """
    pass  # implemented in Ada


def echo_error(*args):
    """
    Displays a line of text. This command is specific to the GPS shell. It is
    designed to be used to output error messages. This command raises the
    shell window.

    :param args: Any number of parameters
    """
    pass  # implemented in Ada


def exec_in_console(noname):
    """
    This function is specific to Python. It executes the string given in
    argument in the context of the GPS Python console. If you use the
    standard Python :func:`exec` function instead, it only modifies the
    current context, which generally has no impact on the GPS console itself.

    :param noname: A string

    .. code-block:: python

       # Import a new module transparently in the console, so that users can
       # immediately use it
       GPS.exec_in_console("import time")
    """
    pass  # implemented in Ada


def execute_action(action, *args):
    """
    Executes one of the actions defined in GPS. Such actions are either
    predefined by GPS or defined by the users through customization files.
    See the GPS documentation for more information on how to create new
    actions.  GPS waits until the command completes to return control to the
    caller, whether you execute a shell command or an external process.

    The action's name can start with a '/', and be a full menu path. As a
    result, the menu itself will be executed, just as if the user had pressed
    it.

    The extra arguments must be strings, and are passed to the action, which
    can use them through $1, $2, etc.

    The list of existing actions can be found using the
    :menuselection:`Edit --> Preferences...` menu and opening the General->
    Key Shortcuts section.

    The action is not executed if the current context is not appropriate for it

    :param action: Name of the action to execute
    :param args: Any number of string parameters

    .. seealso:: :func:`GPS.execute_asynchronous_action`

    .. code-block:: python

       GPS.execute_action(action="Split vertically")
       # will split the current window vertically
    """
    pass  # implemented in Ada


def execute_asynchronous_action(action, *args):
    """
    Like :func:`GPS.execute_action`, but commands that execute external
    applications or menus are executed asynchronously: this function
    immediately returns even though external application may not have
    completed its execution.

    :param action: Name of the action to execute
    :param args: Any number of string parameters

    .. seealso:: :func:`GPS.execute_action`
    """
    pass  # implemented in Ada


def exit(force=False, status='0'):
    """
    Exits GPS, asking for confirmation if any file is currently modified and
    unsaved. If ``force`` is True, no check is done.

    ``status`` is the exit status to return to the calling shell. 0 means
    success on most systems.

    :param force: A boolean
    :param status: An integer

    """
    pass  # implemented in Ada


def extract_method(file, line_start, line_end, method_name='New_Method'):
    """
    Extracts the code from ``line_start`` to ``line_end`` in ``file`` into a
    new subprogram with the given name. All needed local variables are
    declared properly in this new subprogram and parameters are created for
    it, if needed.

    :param file: A string
    :param line_start: An integer
    :param line_end: An integer
    :param method_name: A string
    """
    pass  # implemented in Ada


def freeze_prefs():
    """
    Prevents the signal "preferences_changed" from being emitted.  Call
    :func:`thaw_prefs` to unfreeze.

    Freezing/thawing this signal is useful when you are about to modify a
    large number of preferences in one batch.

    .. seealso::

      :func:`GPS.thaw_prefs`
    """
    pass  # implemented in Ada


def getenv(key):
    """
    Gets the value of the given environment variable.

    :param key: A string
    :return: a string
    """
    pass


def setenv(key, value):
    """
    Sets the value of the given environment variable to the given value.

    :param key: A string
    :param value: A string
    """
    pass


def thaw_prefs():
    """
    Re-enables calling the "preferences_changed" hook.

    .. seealso::

      :func:`GPS.freeze_prefs`
    """
    pass  # implemented in Ada


def xref_db():
    """
    Returns the location of the xref database. This is an :program:`sqlite`
    database created by GPS when it parses the :file:`.ali` files generated
    by the compiler.

    Its location depends mainly on the optional IDE'Artifacts_Dir
    attribute, which defaults to the project's object directory if not
    specified.

    The location can also depend on the optional IDE'Xref_Database
    attribute which specifies a complete path to the cross-references
    database file.

    :return: a string
    """


def get_build_mode():
    """
    Returns the name of the current build mode. Returns an empty string if no
    mode is registered.
    """
    pass  # implemented in Ada


def get_target():
    """
    Returns the target currently set in the project or the GPS interface.

    :return: a string
    """
    pass  # implemented in Ada


def get_runtime():
    """
    Returns the runtime currently set in the project or the GPS interface.

    :return: a string
    """
    pass  # implemented in Ada


def get_build_output(target_name, shadow, background, as_string):
    """
    Returns the result of the last compilation command.

    :param target_name: (optional) a string
    :param shadow: (optional) a Boolean, indicating whether we want the output
       of shadow builds
    :param background: (optional) a Boolean, indicating whether we want the
        output of background builds
    :param as_string: (optional) a Boolean, indicating whether the output
       should be returned as a single string. By default the output is
       returned as a list in script languages that support it
    :return: A string or list, the output of the latest build for the
        corresponding target

    .. seealso::

       :func:`GPS.File.make`

       :func:`GPS.File.compile`
    """
    pass  # implemented in Ada


def get_home_dir():
    """
    Returns the directory that contains the user-specific files. This
    string always ends with a directory separator.

    :return: The user's GPS directory

    .. seealso:: :func:`GPS.get_system_dir`

    .. code-block:: python

       log = GPS.get_home_dir() + "log"
       # will compute the name of the log file generated by GPS

    """
    pass  # implemented in Ada


def get_system_dir():
    """
    Returns the installation directory for GPS.  This string always ends with
    a directory separator.

    :return: The install directory for GPS

    .. seealso:: :func:`GPS.get_home_dir`

    .. code-block:: python

       html = GPS.get_system_dir() + "share/doc/gps/html/gps.html"
       # will compute the location of GPS's documentation
    """
    pass  # implemented in Ada


def get_tmp_dir():
    """
    Returns the directory where gps creates temporary files.  This string
    always ends with a directory separator.

    :return: The install directory for GPS
    """
    pass  # implemented in Ada


def help(command=''):
    """
    Returns the description of the command given in parameter or the list of
    all commands exported by GPS. :func:`GPS.help` is specific to the GPS
    shell.

    :param command: A string
    :return: A string
    """
    pass  # implemented in Ada


def insmod(shared_lib, module):
    """
    Dynamically registers a new module, reading its code from ``shared_lib``.

    The library must define the following two symbols:

    - _init: This is called by GPS to initialize the library itself

    - __register_module: This is called to do the actual module registration,
      and should call the :func:`Register_Module` function in the GPS source
      code.

    This is work in progress, and not fully supported on all systems.

    :param shared_lib: Library containing the code of the module
    :param module: Name of the module

    .. seealso:: :func:`GPS.lsmod`
    """
    pass  # implemented in Ada


def is_server_local(server):
    """
    Indicates where the ``server`` is the local machine.

    :param server: The server. Possible values are "Build_Server",
       "Debug_Server", "Execution_Server" and "Tools_Server"
    :return: A boolean
    """
    pass  # implemented in Ada


def last_command():
    """
    Returns the name of the last action executed by GPS. This name is not
    ultra-precise: it is accurate only when the action is executed through a
    key binding. Otherwise, an empty string is returned. However, the intent
    is for a command to be able to check whether it is called multiple times
    consecutively. For this reason, this function returns the command set by
    :func:`GPS.set_last_command`, if any.

    :return: A string

    .. seealso:: :func:`GPS.set_last_command`

    .. code-block:: python

       def kill_line():
          '''Emulates Emacs behavior: when called multiple times, the cut line
             must be appended to the previously cut one.'''

          # The name of the command below is unknown to GPS. This is just a
          # string we use in this implementation to detect multiple
          # consecutive calls to this function. Note that this works whether
          # the function is called from the same key binding or not and from
          # the same GPS action or not

          append = GPS.last_command() == "my-kill-line":
          GPS.set_last_command("my-kill-line")
    """
    pass  # implemented in Ada


def load(filename):
    """
    Loads and executes a script file. This command is specific to the GPS
    shell.

    :param filename: A string
    """
    pass  # implemented in Ada


def lookup_actions():
    """
    Returns the list of all known GPS actions, not including menu names. All
    actions are lower-cased, but the order of the list is not significant.

    :return: A list of strings

    .. seealso:: :func:`GPS.lookup_actions_from_key`
    """
    pass  # implemented in Ada


def lookup_actions_from_key(key):
    """
    Given a key binding, for example "control-x control-b", returns the list
    of actions that could be executed. Not all actions would be executed,
    however, since only the ones for which the filter matches are
    executed. The names of the actions are always in lower case.

    :param key: A string
    :return: A list of strings

    .. seealso:: :func:`GPS.lookup_actions`
    """
    pass  # implemented in Ada


def ls(pattern=''):
    """
    Lists the files matching ``pattern`` (all files by default).

    :param pattern: A string
    :return: A list of strings
    """
    pass  # implemented in Ada


def lsmod():
    """
    Returns the list of modules currently registered in GPS. Each facility in
    GPS is provided in a separate module so that users can choose whether to
    activate specific modules or not. Some modules can also be dynamically
    loaded.

    :return: List of strings

    .. seealso:: :func:`GPS.insmod`
    """
    pass  # implemented in Ada


def macro_load(file):
    """
    Loads ``file``, containing a set of recorded events.

    :param file: A string
    """
    pass  # implemented in Ada


def macro_play(speed='1.0'):
    """
    Plays the current set of events.

    :param speed: A string
    """
    pass  # implemented in Ada


def macro_record():
    """
    Starts recording a set of events.
    """
    pass  # implemented in Ada


def parse_xml(xml):
    """
    Loads an XML customization string. This string should contain one or more
    toplevel tags similar to what is normally found in custom files, such as
    <key>, <alias>, <action>.

    Optionally you can also pass the full contents of an XML file, starting
    with the <?xml?> header.

    :param xml: The XML string to parse

    .. code-block:: python

       GPS.parse_xml(
          '''<action name="A"><shell>my_action</shell></action>
             <menu action="A"><title>/Edit/A</title></menu>''')
       Adds a new menu in GPS, which executes the command my_action
    """
    pass  # implemented in Ada


def process_all_events():
    """
    Process all the graphical events that have been queue by the system:
    these events typically involve demands to refresh part of the screen,
    handle key or mouse events, ...
    This is mostly useful when writing automatic tests. In plugins, the
    recommand approach is instead to create actions via
    :func:`gps_utils.interactive`, and run them in the background with
    :func:`GPS.execute_action`.
    Another possible approach is to use python generators with the yield
    keyword.
    """


def pwd():
    """
    Prints name of the current (working) directory.

    :return: A string

    This function has the same return value as the standard Python function
    :func:`os.getcwd`. The current directory can also be changed through a
    call to os.chdir("dir").
    """
    pass  # implemented in Ada


def repeat_next(count):
    """
    Executes the next action ``count`` times.

    :param count: An integer
    """
    pass  # implemented in Ada


def save_persistent_properties():
    """
    Forces an immediate save of the persistent properties that GPS maintains
    for files and projects (for example the text encoding, the programming
    language, and the debugger breakpoints).

    This is done automatically by GPS on exit, so you normally do not have to
    call this subprogram.
    """
    pass  # implemented in Ada


def send_key_event(
        keyval, window=None, primary=False, alt=False,
        shift=False, control=False, hardware_keycode=0):
    """
    synthesize and queue an event to simulate a key press. This event
    will be processed later by gtk+ (unless you call
    :func:`gps.process_all_events`). as much as possible, this function
    should be avoided and you should use :func:`gps.execute_action`
    instead.

    :param GUI window: the window to which the event should be sent. This
       defaults to the window that currently has the focus.
    :param hardware_keycode: the hardware keycode associated to keyval
    """


def send_button_event(window=None, type=None, button=1, x=1, y=1, state=0):
    """
    synthesize and queue an event to simulate a mouse action. This event
    will be processed later by gtk+ (unless you call
    :func:`gps.process_all_events`). as much as possible, this function
    should be avoided and you should use :func:`gps.execute_action`
    instead.

    :param int type: the type of event. This defaults to a button press.
    :param GUI window: the window to which the event should be sent. This
       defaults to the window that currently has the focus.
    :param int state: the state of the modified keys (control, shift,...)
    """


def send_crossing_event(window=None, type=None, x=1, y=1, state=0):
    """
    synthesize and queue an event to simulate a mouse movement. This event
    will be processed later by gtk+ (unless you call
    :func:`gps.process_all_events`). as much as possible, this function
    should be avoided and you should use :func:`gps.execute_action`
    instead.

    :param int type: the type of event. This defaults to an Enter notify
       event.
    :param GUI window: the window to which the event should be sent. This
       defaults to the window that currently has the focus.
    :param int state: the state of the modified keys (control, shift,...)
    """


def set_build_mode(mode=''):
    """
    Sets the current build mode. If ``mode`` is not a registered mode,
    does nothing.

    :param mode: Name of the mode to set
    """
    pass  # implemented in Ada


def set_last_command(command):
    """
    Overrides the name of the last command executed by GPS. This new name is
    the one returned by :func:`GPS.last_command` until the user performs a
    different action. Thus, multiple consecutive calls of the same action
    always return the value of the ``command`` parameter. See the example in
    :func:`GPS.last_command`.

    :param command: A string

    .. seealso:: :func:`GPS.last_command`
    """
    pass  # implemented in Ada


def supported_languages():
    """
    Returns the list of languages for which GPS has special handling. Any
    file can be opened in GPS, but some extensions are recognized specially
    by GPS to provide syntax highlighting, cross-references, or other special
    handling. See the GPS documentation on how to add support for new
    languages in GPS.

    The returned list is sorted alphabetically and the name of the language
    has been normalized (starts with an upper case character and is lowercase
    for the rest except after an underscore character).

    :return: List of strings

    .. code-block:: python

       GPS.supported_languages()[0]
       => return the name of the first supported language
    """
    pass  # implemented in Ada


def reset_xref_db():
    """
    Empties the internal xref database for GPS. This is rarely useful,
    unless you want to force GPS to reload everything.
    """
    pass


def version():
    """
    Returns the GPS version as a string.

    :return: A string
    """
    pass  # implemented in Ada


def visual_diff(file1, file2, file3=''):
    """
    Opens a Visual Diff between ``file1``, ``file2`` and (optionally)
    ``file3``.

    :param file1: A string
    :param file2: A string
    :param file3: A string
    """
    pass  # implemented in Ada

# import extensions

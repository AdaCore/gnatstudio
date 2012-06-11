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

These functions are made available through various programming languages (Python
and the GPS shell at the moment). The documentation in this package is mostly
oriented towards Python, but it can also be used as a reference for the GPS
shell

Function description
--------------------

For all functions, the list of parameters is given. The first parameter will
often be called "self", and refers to the instance of the class to which the
method applies. In Python, the parameter is generally put before the method's
name, as in::

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

A very useful feature of python is that all class instances can be associated
with any number of user data fields. For example, if you create an instance of
the class GPS.EditorBuffer, you can associate two fields "field1" and "field2"
to it (the names and number are purely for demonstration purposes, and you can
use your own), as in::

    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    ed.field1 = "value1"
    ed.field2 = 2

GPS takes great care for most classes of always returning the same python
instance for a given GUI object. For instance, if you were to get another
instance of GPS.EditorBuffer for the same file as above, you would in fact
receive the same Python instance, and thus the two fields are available to you,
as in::

    ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
    # ed.field1 is still "value1"

This is a very convenient way to store your own data associated with the various
objects exported by GPS. These data will cease to exist when the GPS object
itself is destroyed (for instance when the editor is closed in the example
above).

Hooks
-----

In a lot of cases, you will need to connect to specific hooks exported by GPS to
be aware of events happening in GPS (loading of a file, closing a file,...).
These hooks and their use are described in the GPS manual (see also the
:class:`GPS.Hook` class).

Here is a small example, where the function on_gps_started is called
when the GPS window is fully visible to the user::

    import GPS
    def on_gps_started(hook):
        pass

    GPS.Hook("gps_started").add(on_gps_started)

The list of parameters for the hooks is described for each hook below. The first
parameter is always the name of the hook, so that the same function can be used
for multiple hooks if necessary.

There are two categories of hooks: the standard hooks and the action hooks. The
former return nothing, the latter return a boolean indicating whether your
callback was able to perform the requested action. They are used to override
some of GPS's internal behavior.

"""

import exceptions


###########################################################
# GUI
###########################################################

class GUI(object):
    """
    This is an abstract class (ie no instances of it can be created from your
    code, which represents a graphical element of the GPS interface

    .. seealso:: :func:`GPS.GUI.__init__`
    """

    def __init__(self):
        """
        Prevents the creation of instances of GPS.GUI. Such instances are
        created automatically by GPS as a result of calling other functions

        .. seealso:: :func:`GPS.Toolbar.append`
        .. seealso:: :func:`GPS.Toolbar.entry`
        .. seealso:: :func:`GPS.Menu.get`
        """
        pass  # implemented in Ada

    def destroy(self):
        """
        Destroy the graphical element. It will disappear from the interface,
        and cannot necessarily be recreated later on
        """
        pass  # implemented in Ada

    def hide(self):
        """
        Temporarily hide the graphical element. It can be shown again through a
        call to GPS.GUI.show()

        .. seealso:: :func:`GPS.GUI.show`
        """
        pass  # implemented in Ada

    def is_sensitive(self):
        """
        Return False if the widget is currently greyed out, and is not clickable by users

        :return: A boolean

        .. seealso:: :func:`GPS.GUI.set_sensitive`
        """
        pass  # implemented in Ada

    def pywidget(self):
        """
        This function is only available if GPS was compiled with support for
        pygtk, and the latter was found at run time. It returns a widget that
        can be manipulated through the usual PyGtk functions. PyGtk is a
        binding to the gtk+ toolkit, and allows you to create your own windows
        easily, or manipulate the entire GPS GUI from python

        :return: An instance of PyWidget

        .. seealso:: :func:`GPS.MDI.add`

        .. code-block:: python

           # The following example makes the project view inactive. One could easily
           # change the contents of the project view as well
           widget = GPS.MDI.get("Project View")
           widget.pywidget().set_sensitive False)

        """
        pass  # implemented in Ada

    def set_sensitive(self, sensitive=True):
        """
        Indicate whether the associated graphical element should respond to
        user interaction or not. If the element is not sensitive, then the user
        will not be able to click on it

        :param sensitive: A boolean

        .. seealso:: :func:`GPS.GUI.is_sensitive()`
        """
        pass  # implemented in Ada

    def show(self):
        """
        Show again the graphical element that was hidden by hide()

        .. seealso:: :func:`GPS.GUI.hide`
        """
        pass  # implemented in Ada


###########################################################
# Action
###########################################################

class Action(GUI):
    """
    This class gives access to the interactive commands in GPS. These are the
    commands to which the user can bind a key shortcut, or for which we can
    create a menu. Another way to manipulate those commands is through the XML
    tag <action>, but it might be more convenient to use python since you do
    not have to qualify the function name as a result
    """

    def __init__(self, name):
        """
        Creates a new instance of Action. This is bound with either an existing
        action, or with an action that will be created through
        GPS.Action.create(). The name of the action can either be a simple
        name, or a path name to reference a menu, as in /Edit/Copy for
        instance.

        :param name: A string
        """
        pass  # implemented in Ada

    def contextual(self, path, ref='', add_before=True):
        """
        Create a new contextual menu associated with the command. This function
        is somewhat a duplicate of GPS.Contextual.create, but with one major
        difference: the callback for the action is a python function that takes
        no argument, whereas the callback for GPS.Contextual receives one
        argument.

        :param path: A string
        :param ref: A string
        :param add_before: A boolean
        """
        pass  # implemented in Ada

    def create(self, on_activate, filter='', category='General', description=''):
        """
        Export the function on_activate and make it interactive so that users
        can bind keys and menus to it. The function should not require any
        argument, since it will be called with none.

        ``filter`` is either the name of a predefined filter (a string), or a
        subprogram that receives the context as a parameter, and should return
        True if the command can be executed within that context. This is used
        to disable menu items when they are not available.

        ``category`` is the category of the command in the /Edit/Key Shortcuts
        dialog.

        ``description`` is the description of the command that appears in that
        dialog. If you are using python, a convenient value is
        on_activate.__doc__, which avoids duplicating the comment.

        The package gps_utils.py provides a somewhat more convenient python
        interface to make function interactives (see gps_utils.interactive).

        :param on_activate: A subprogram
        :param filter: A string or subprogram
        :param category: A string
        :param description: A string
        """
        pass  # implemented in Ada

    def key(self, key):
        """
        Associate a default key binding with the action. This will be ignored
        if the user has defined his own key binding. Possible values for key
        can be experimented with by using the /Edit/Key Shortcuts dialog

        :param key: A string
        """
        pass  # implemented in Ada

    def menu(self, path, ref='', add_before=True):
        """
        Create a new menu associated with the command. This function is
        somewhat a duplicate of :func:`GPS.Menu.create`, but with one major
        difference: the callback for the action is a python function that takes
        no argument, whereas the callback for :func:`GPS.Menu` receives one
        argument.

        :param path: A string
        :param ref: A string
        :param add_before: A boolean
        """
        pass  # implemented in Ada


###########################################################
# Activities
###########################################################

class Activities(object):
    """
    General interface to version control activities systems
    """

    def __init__(self, name):
        """
        Creates a new activity and returns its instance

        :param name: Activity's name to be given to this instance

        .. code-block:: python

            a=GPS.Activities("Fix loading order")
            print a.id()
        """
        pass  # implemented in Ada

    def add_file(self, file):
        """
        Adds the file into the activity

        :param file: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def commit(self):
        """
        Commit the activity
        """
        pass  # implemented in Ada

    def diff(self):
        """
        Diff all activity's files
        """
        pass  # implemented in Ada

    def files(self):
        """
        Returns the activity's files list

        :return: A list of files
        """
        pass  # implemented in Ada

    @staticmethod
    def from_file(file):
        """
        Returns the activity containing the given file

        :param file: An instance of :class:`GPS.File`
        :return: An instance of :class:`GPS.Activities`
        """
        pass  # implemented in Ada

    @staticmethod
    def get(id):
        """
        Returns the activity given its id

        :param id: The unique activity's id
        :return: An instance of :func:`GPS.Activities`

        .. seealso:: :func:`GPS.Activities.list`

        """
        pass  # implemented in Ada

    def group_commit(self):
        """
        Returns true if the activity will be commit atomically

        :return: A boolean
        """
        pass  # implemented in Ada

    def has_log(self):
        """
        Returns true if the activity has a log present

        :return: A boolean
        """
        pass  # implemented in Ada

    def id(self):
        """
        Returns the activity's unique id

        :return: A string
        """
        pass  # implemented in Ada

    def is_closed(self):
        """
        Returns true if the activity is closed

        :return: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Returns the list of all activities's id

        :return: A list of all activities's id defined
        """
        pass  # implemented in Ada

    def log(self):
        """
        Returns the activity's log content

        :return: A string
        """
        pass  # implemented in Ada

    def log_file(self):
        """
        Returns the activity's log file

        :return: A file
        """
        pass  # implemented in Ada

    def name(self):
        """
        Returns the activity's name
        :return: A string
        """
        pass  # implemented in Ada

    def query_status(self):
        """
        Query status of all activity's files
        """
        pass  # implemented in Ada

    def remove_file(self, file):
        """
        Removes the file into the activity

        :param file: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def set_closed(self, status):
        """
        Set the activity's closed status

        :param status: A boolean
        """
        pass  # implemented in Ada

    def toggle_group_commit(self):
        """
        Change the activity's group commit status
        """
        pass  # implemented in Ada

    def update(self):
        """
        Update all activity's files
        """
        pass  # implemented in Ada

    def vcs(self):
        """
        Returns the activity's VCS name

        :return: A string
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
        returns a list containing the contextual menu labels of the currently
        focused window. The output have the form "depth - label"
        """
        pass  # implemented in Ada


###########################################################
# FileContext
###########################################################

class FileContext(Context):
    """
    Represents a context that contains file information

    .. seealso:: :func:`GPS.FileContext.__init__`
    """

    def __init__(self):
        """
        Dummy function, whose goal is to prevent user-creation of a
        GPS.FileContext instance. Such instances can only be created internally
        by GPS
        """
        pass  # implemented in Ada

    def directory(self):
        """
        Return the current directory in the context

        :return: A string
        """
        pass  # implemented in Ada

    def file(self):
        """
        Return the name of the file in the context

        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def files(self):
        """
        Return the list of selected files in the context

        :return: A list of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def location(self):
        """
        Return the file location stored in the context

        :return: An instance of :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def project(self):
        """
        Return the project in the context, or the root project if none was
        specified in the context. Return an error if no project can be
        determined from the context

        :return: An instance of :class:`GPS.Project`
        """
        pass  # implemented in Ada


###########################################################
# AreaContext
###########################################################

class AreaContext(FileContext):
    """
    Represents a context that contains file information and a range of lines
    currently selected

    .. seealso:: :func:`GPS.AreaContext.__init__`

    """

    def __init__(self):
        """
        Dummy function, whose goal is to prevent user-creation of a
        GPS.AreaContext instance. Such instances can only be created internally
        by GPS
        """
        pass  # implemented in Ada

    def end_line(self):
        """
        Return the last selected line in the context

        :return: An integer
        """
        pass  # implemented in Ada

    def start_line(self):
        """
        Return the first selected line in the context

        :return: An integer
        """
        pass  # implemented in Ada


###########################################################
# Bookmark
###########################################################

class Bookmark(object):
    """
    This class provides access to the bookmarks of GPS. These are special types
    of markers that are saved across sessions, and can be used to save a
    context within GPS. They are generally associated with a specific location
    in an editor, but can also be used to location special boxes in a graphical
    browser for instance.
    """

    def __init__(self):
        """
        This function prevents the creation of a bookmark instance
        directly. You must use :func:`GPS.Bookmark.get` instead, which will always
        return the same instance for a given bookmark, thus allowing you to
        save your own custom data with the bookmark

        .. seealso:: :func:`GPS.Bookmark.get`
        """
        pass  # implemented in Ada

    @staticmethod
    def create(name):
        """
        This function creates a new bookmark at the current location in GPS. If
        the current window is an editor, it creates a bookmark that will save
        the exact line and column, so that the user can go back to them
        easily. Name is the string that appears in the bookmarks window, and
        that can be used later to query the same instance using
        :func:`GPS.Bookmark.get`. This function emits the hook bookmark_added.

        :param name: A string
        :return: An instance of GPS.Bookmark

        .. seealso:: :func:`GPS.Bookmark.get`

        .. code-block:: python

           GPS.MDI.get("file.adb").raise_window()
           bm = GPS.Bookmark.create("name")
        """
        pass  # implemented in Ada

    def delete(self):
        """
        Delete an existing bookmark. This emits the hook bookmark_removed
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        This function retrieves a bookmark by its name. If no such bookmark
        exists, an exception is raised. The same instance of GPS.Bookmark is
        always return for a given bookmark, so that you can store your own user
        data within the instance. Note however that this custom data will not
        be automatically preserved across GPS sessions, so you might want to
        save all your data when GPS exits

        :param name: A string
        :return: An instance of GPS.Bookmark

        .. seealso:: :func:`GPS.Bookmark.create`

        .. code-block:: python

           GPS.Bookmark.get("name").my_own_field = "GPS"
           print GPS.Bookmark.get("name").my_own_field   # prints "GPS"

        """
        pass  # implemented in Ada

    def goto(self):
        """
        Change the current context in GPS so that it matches the one saved in
        the bookmark. In particular, if the bookmark is inside an editor, this
        editor is raised, and the cursor moved to the correct line and
        column. You cannot query directly the line and column from the
        bookmark, since these might not exist, for instance when the editor
        points inside a browser.
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Return the list of all existing bookmarks

        :return: A list of GPS.Bookmark instances

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
        used the bookmarks view to rename it

        :return: A string
        """
        pass  # implemented in Ada

    def rename(self, name):
        """
        Rename an existing bookmark. This updates the bookmarks view
        automatically, and emits the hooks bookmark_removed and bookmark_added

        :param name: A string
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
        Initializes a new instance of the class BuildTarget. Name must correspond to an existing target.

        :param name: Name of the target associated with this instance

        .. code-block:: python

           compile_file_target=GPS.BuildTarget("Compile File")
           compile_file_target.execute()
        """
        pass  # implemented in Ada

    def clone(self, new_name, new_category):
        """
        Clone the target to a new target. All the properties of the new target
        are copied from the target.  Any graphical element corresponding to this
        new target is created.

        :param new_name: The name of the new target
        :param new_category: The category in which to place the new target
        """
        pass  # implemented in Ada

    def execute(self, main_name='', file='', force=False, extra_args='',
                build_mode='', synchronous=True, directory='', quiet=False):
        """
        Launch the build target:

- main_name indicates the base name of the main source to build, if this target
  acts on a main file.

- ``file`` indicates the file to build if this targets acts on a file.
- if ``force`` is True, this means that the target should be launched directly,
  even if its parameters indicate that it should be launched through an
  intermediary dialog.
- ``extra_args`` contains any extra parameters to pass to the command line.
- ``build_mode`` indicates build mode to be used for build.
- if ``synchronous`` is False, build target is launched
  asynchronously. ``compilation_finished hook`` will be called when build target
  execution is completed.

        :param main_name: A String
        :param file: A GPS.File
        :param force: A Boolean
        :param extra_args: A String
        :param build_mode: A String
        :param synchronous: A Boolean
        :param directory: A String
        :param quiet: A Boolean
        """
        pass  # implemented in Ada

    def remove(self):
        """
        Remove target from the list of known targets.
        Any graphical element corresponding to this target is also removed.
        """
        pass  # implemented in Ada


###########################################################
# Button
###########################################################

class Button(GUI):
    """
    This class represents a button that can be pressed to trigger various
    actions

    .. seealso:: :func:`GPS.Button.__init__()`
    """

    def __init__(self, id, label, on_click):
        """
        Initializes a new button. When the button is pressed by the user,
        ``on_click`` is called with the a single parameter, ``self``.

        :param id: A string, a unique identifier for the button
        :param label: A string, the text that appears on the button
        :param on_click: A subprogram, see the GPS documentation

        .. code-block:: python

           def on_click (button):
               print "Button pressed"
           button = GPS.Button ("my_id", label="Press me", on_click=on_click)
           GPS.Toolbar().append (button)
        """
        pass  # implemented in Ada

    def set_text(self, label):
        """
        Change the text that appears on the button

        :param label: A string
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
    later on
    """

    @staticmethod
    def contents():
        """
        This function returns the contents of the clipboard. Each item in the
        list corresponds to a past selection, the one at position 0 being the
        most recent. If you want to paste text in a buffer, you should paste
        the text at position :func:`GPS.Clipboard.current`` rather than the
        first in the list

        :return: A list of strings
        """
        pass  # implemented in Ada

    @staticmethod
    def copy(text, append=False):
        """
        Copies a given static text into the clipboard. It is better in general
        to use :func:`GPS.EditorBuffer.copy`, but it might happen that you need
        to append text that doesn't exist in the buffer.

        :param text: A string
        :param append: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.copy`
        """
        pass  # implemented in Ada

    @staticmethod
    def current():
        """
        This function returns the index, in :func:`GPS.Clipboard.contents`, of the
        text that was last pasted by the user. If you were to select the menu
        /Edit/Paste, that would be the text pasted by GPS. If you select
        /Edit/Paste Previous, current will be incremented by 1, and the next
        selection in the clipboard will be pasted

        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def merge(index1, index2):
        """
        This function merges two levels of the clipboard, so that the one at
        index ``index1`` now contains the concatenation of both. The one at
        ``index2`` is removed.

        :param index1: A null or positive integer
        :param index2: A null or positive integer
        """
        pass  # implemented in Ada


###########################################################
# CodeAnalysis
###########################################################

class CodeAnalysis(object):
    """
    This class is a toolset that allows to handle CodeAnalysis instances.
    """

    def __del__(self):
        """
        Called when a CodeAnalysis instance is deleted by python.
        """
        pass  # implemented in Ada

    def __init__(self):
        """
        Raises an exception to prevent users from creating new instances.
        """
        pass  # implemented in Ada

    def add_all_gcov_project_info(self):
        """
        Adds coverage information of every source files referenced in the
        current project loaded in GPS, and every imported projects.

        .. seealso::

           :func:`GPS.CodeAnalysis.add_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_file_info`
        """
        pass  # implemented in Ada

    def add_gcov_file_info(self, src, cov):
        """
        Adds coverage information provided by a .gcov file parsing. The data is
        read from the cov parameter, that should have been created from the
        specified src file.

        :param src: A GPS.File instance
        :param cov: A GPS.File instance

        .. seealso::

           :func:`GPS.CodeAnalysis.add_all_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_project_info`

        .. code-block:: python

           a = GPS.CodeAnalysis.get ("Coverage Report")
           a.add_gcov_file_info (src=GPS.File ("source_file.adb"), cov=GPS.File ("source_file.adb.gcov"))
        """
        pass  # implemented in Ada

    def add_gcov_project_info(self, prj):
        """
        Adds coverage information of every source files referenced in the given
        'prj' gnat project file (.gpr).

        :param prj: A GPS.File instance

        .. seealso::

           :func:`GPS.CodeAnalysis.add_all_gcov_project_info`

           :func:`GPS.CodeAnalysis.add_gcov_file_info`
        """
        pass  # implemented in Ada

    def clear(self):
        """Removes all code analysis information from memory."""
        pass  # implemented in Ada

    def dump_to_file(self, xml):
        """
        Create an xml-formated file that contains a representation of the given
        code analysis.

        :param xml: A GPS.File instance

        .. seealso:: :func:`GPS.CodeAnalysis.load_from_file`

        .. code-block:: python

           a = GPS.CodeAnalysis.get ("Coverage")
           a.add_all_gcov_project_info ()
           a.dump_to_file (xml=GPS.File ("new_file.xml"))
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Creates an empty code analysis data structure. Data can be put in this
        structure by using one of the primitive operations.

        :param name: The name of the code analysis data structure to get or
            create
        :return: An instance of :class:`GPS.CodeAnalysis` associated to a code
           analysis data structure in GPS.

        .. code-block:: python

           a = GPS.CodeAnalysis.get ("Coverage")
           a.add_all_gcov_project_info ()
           a.show_coverage_information ()
        """
        pass  # implemented in Ada

    def hide_coverage_information(self):
        """
        Removes from the Locations view any listed coverage locations, and
        remove from the source editors their annotation column if any.

        .. seealso:: :func:`GPS.CodeAnalysis.show_coverage_information`
        """
        pass  # implemented in Ada

    def load_from_file(self, xml):
        """
        Replace the current coverage information in memory with the given
        xml-formated file one.

        :param xml: A GPS.File instance

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
        Displays the data stored in the CodeAnalysis instance into a new MDI
        window. This window contains a tree view that can be interactively
        manipulated to analyze the results of the code analysis (Coverage,
        ...).
        """
        pass  # implemented in Ada

    def show_coverage_information(self):
        """
        Lists in the Locations view the lines that are not covered in the files
        loaded in the CodeAnalysis instance. The lines are also highlighted in
        the corresponding source file editors, and an annotation column is
        added to the source editors.

        .. seealso:: :func:`GPS.CodeAnalysis.hide_coverage_information`
        """
        pass  # implemented in Ada


###########################################################
# Codefix
###########################################################

class Codefix(object):
    """
    This class gives access to GPS's features for automatically fixing compilation errors

    .. seealso::

       :func:`GPS.CodefixError`

       :func:`GPS.Codefix.__init__()`
    """

    def __init__(self, category):
        """
        Return the instance of codefix associated with the given category

        :param category: A string
        """
        pass  # implemented in Ada

    def error_at(self, file, line, column, message=''):
        """
        Return a specific error at a given location. If message is null, then
        the first matching error will be taken. None is returned if no such
        fixable error exists.

        :param file: The file where the error is
        :param line: The line where the error is
        :param column: The column where the error is
        :param message: The message of the error
        :return: An instance of :class:`GPS.CodefixError`
        """
        pass  # implemented in Ada

    def errors(self):
        """
        List the fixable errors in that session

        :return: A list of instances of :class:`GPS.CodefixError`
        """
        pass  # implemented in Ada

    @staticmethod
    def parse(category, output, regexp='', file_index=-1, line_index=-1,
              column_index=-1, style_index=-1, warning_index=-1):
        """
        Parse the output of a tool, and suggests auto-fix possibilities
        whenever possible. This adds small icons in the location window, so
        that the user can click on it to fix compilation errors. You should
        call Locations.parse with the same output prior to calling this
        command.

        The regular expression specifies how locations are recognized. By
        default, it matches file:line:column. The various indexes indicate the
        index of the opening parenthesis that contains the relevant information
        in the regular expression. Set it to 0 if that information is not
        available.

        Access the various suggested fixes through the methods of the Codefix
        class

        :param category: A string
        :param output: A string
        :param regexp: A string
        :param file_index: An integer
        :param line_index: An integer
        :param column_index: An integer
        :param style_index: An integer
        :param warning_index: An integer

        .. seealso:: :func:`GPS.Editor.register_highlighting`
        """
        pass  # implemented in Ada

    @staticmethod
    def sessions():
        """
        List all the existing Codefix sessions. The returned values can all be
        used to create a new instance of Codefix through its constructor.

        :return: A list of strings

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
    This class represents a fixable error in the compilation output

    .. seealso::

       :func:`GPS.Codefix`

       :func:`GPS.CodefixError.__init__()`
    """

    def __init__(self, codefix, file, message=''):
        """
        Describe a new fixable error. If the message is not specified, the
        first error at that location is returned

        :param codefix: An instance of GPS.Codefix
        :param file: An instance of GPS.FileLocation
        :param message: A string
        """
        pass  # implemented in Ada

    def fix(self, choice='0'):
        """
        Fix the error, using one of the possible fixes. The index given in
        parameter is the index in the list returned by "possible_fixes. By
        default, the first choice is taken. Choices start at index 0.

        :param choice: The index of the fix to apply, see output of GPS.CodefixError.possible_fixes()

        .. code-block:: python

           for err in GPS.Codefix ("Builder results").errors():
               print err.fix()

           # will automatically fix all fixable errors in the last compilation
           # output
        """
        pass  # implemented in Ada

    def location(self):
        """
        Return the location of the error

        :return: An instance of :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def message(self):
        """
        Return the error message, as issues by the tool

        :return: A string
        """
        pass  # implemented in Ada

    def possible_fixes(self):
        """
        List the possible fixes for the specific error

        :return: A list of strings

        .. code-block:: python

           for err in GPS.Codefix ("Builder results").errors():
               print err.possible_fixes()
        """
        pass  # implemented in Ada


###########################################################
# Combo
###########################################################

class Combo(GUI):
    """
    This class represents a combo box, ie a text entry widget with a number of
    predefined possible values. The user can interactively select one of
    multiple values through this widget

    .. seealso::

       :class:`GPS.Toolbar`

       :func:`GPS.Combo.__init__`
    """

    def __init__(self, id, label='', on_changed=None):
        """
Create a new combo. The combo will graphically be preceded by some text
if label was specified. ``on_changed`` will be called every time the
user selects a new value for the combo box. Its parameters are the
following:

- $1 = The instance of GPS.Combo (self)
- $2 = The newly selected text (a string)

:param id: A string, the name of the combo to create
:param label: A string, the label to add next to the entry
:param on_changed: A subprogram, see the GPS documentaion on Subprogram parameters

        .. seealso::

             :func:`GPS.Toolbar.append()`

             :func:`GPS.Toolbar.ge()`
        """
        pass  # implemented in Ada

    def add(self, choice, on_selected=None):
        """
Add a choice to specified entry, ``on_selected`` will be executed
whenever this choice is selected. It is called with the following
parameters:

- $1 = The instance of GPS.Combo (self)
- $2 = The newly selected text (a string)

:param choice: A string
:param on_selected: A subprogram, see the GPS documentation on Subprogram parameters
        """
        pass  # implemented in Ada

    def clear(self):
        """Remove all choices from specified entry"""
        pass  # implemented in Ada

    def get_text(self):
        """
        Return the current selection in specified entry

        :return: A string
        """
        pass  # implemented in Ada

    def remove(self, choice):
        """
        Remove a choice from specified entry

        :param choice: A string

        .. seealso:: :func:`GPS.Combo.clear()`
        """
        pass  # implemented in Ada

    def set_text(self, choice):
        """
        Set the current selection in specified entry

        :param choice: A string
        """
        pass  # implemented in Ada


###########################################################
# Command
###########################################################

class Command(object):
    """
    Interface to GPS command. This class is abstract, and shall be subclassed.
    """

    def __del__(self):
        """
        Destructor of a GPS command. This should not be called manually by the user.
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Return the list of commands of the name given in parameter, scheduled
        or running in the task manager

        :param name: A string
        :return: a list of :class:`GPS.Command`
        """
        pass  # implemented in Ada

    def get_result(self):
        """
        Return the result of the command, if any. Must be overriden by children
        """
        pass  # implemented in Ada

    def interrupt(self):
        """Interrupt the current command"""
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Return the list of commands scheduled or running in the task manager

        :return: a list of :class:`GPS.Command`
        """
        pass  # implemented in Ada

    def name(self):
        """Return The name of the command"""
        pass  # implemented in Ada

    def progress(self):
        """
        Return a list representing the current progress of the command. If
        current = total, then the command is finished.

        :return: a list [current, total]
        """
        pass  # implemented in Ada


###########################################################
# CommandWindow
###########################################################

class CommandWindow(GUI):
    """
    This class gives access to a command-line window that pops up on the
    screen. This window is short-lived (in fact there can be only one such
    window at any given time), and any key press is redirected to that
    window. As a result, it should be used to interactively query a parameter
    for an action, for instance.

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
                 self.write(input[:cursor_pos + 1] + "FOO" + input[cursor_pos + 1:])
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
This function initializes an instance of a command window. An exception is
raised if such a window is already active in GPS. Otherwise, the new window is
popped up on the screen. Its location depends on the ``global_window``
parameter: if true, the command window is displayed at the bottom of the GPS
window and occupies its whole width. If false, it is displayed at the bottom of
the currently selected window.

The prompt is the short string displayed just before the command line
itself. Its goal is to indicate to the user what he is entering.

The last four parameters are callbacks:

- ``on_changed`` is called when the user has entered one or more new characters
  in the command line. This function is given two parameters: the current input
  string, and the last cursor position in this string. See the example above on
  how to get the part of the input before and after the cursor.

- ``on_activate`` is called when the user has pressed enter. The command window
  has already been closed at that point if close_on_activate is True, and the
  focus given back to the initial MDI window that had it. This callback is
  given a single parameter, the final input string

- ``on_cancel`` is called when the user has pressed a key that closed the
  dialog, for instance Esc. It is given a single parameter, the final input
  string. This callback is also called when you explicitly destroy the window
  yourself by calling self.destroy().

- ``on_key`` is called when the user has pressed a new key on his keyboard, but
  before the corresponding character has been added to the command line. This
  can be used to filter out some characters, or provide special behavior for
  some key combination (see the example above). It is given three parameters,
  the current input string, the key that was pressed, and the current cursor
  position.

        :param prompt: A string
        :param global_window: A boolean
        :param on_changed: A subprogram
        :param on_activate: A subprogram
        :param on_cancel: A subprogram
        :param on_key: A subprogram
        :param close_on_activate: A boolean
        """
        pass  # implemented in Ada

    def read(self):
        """
        This function returns the current contents of the command window

        :return: A string
        """
        pass  # implemented in Ada

    def set_background(self, color=''):
        """
        Change the background color of the command window. In most cases, this
        can be used to make the command window more obvious, or to point out
        errors by changing the color. If the color parameter is not specified,
        the color reverts to its default

        :param color: A string
        """
        pass  # implemented in Ada

    def set_prompt(self, prompt):
        """
        Changes the prompt that is displayed before the text field

        :param prompt: A string
        """
        pass  # implemented in Ada

    def write(self, text, cursor=-1):
        """
This function replaces the current content of the command line. As a result,
you should make sure to preserve the character you want, as in the on_key
callback in the example above. Calling this function will also result in
several calls to the on_changed callback, one of them with an empty string
(since gtk first deletes the contents and then writes the new contents.

The cursor parameter can be used to specify where the cursor should be left
after the insertion. -1 indicates the end of the string.

        :param text: A string
        :param cursor: An integer
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

        # The usual python's standard output can also be redirected to this
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

            def __init__(self, process, args=""):
              GPS.Console.__init__(
                 self, process,
                 on_input=Console_Process.on_input,
                 on_destroy=Console_Process.on_destroy,
                 force=True)
              GPS.Process.__init__(
                 self, process + ' ' + args, ".+",
                 on_exit=Console_Process.on_exit,
                 on_match=Console_Process.on_output)

        bash = Console_Process("/bin/sh", "-i")
    """

    def __init__(self, name, force=False, on_input=None, on_destroy=None,
                 accept_input=True, on_resize=None, on_interrupt=None,
                 on_completion=None, on_key='', manage_prompt=True, ansi=False):
        """
Create a new instance of GPS.Console. GPS will try to reuse any existing
console with the same name. If none exists yet, or the parameter force is set
to True, then GPS will create a new console.

You cannot create the Python and Shell consoles through this call. If you do,
an exception is raised. Instead, use GPS.execute_action
("/Tools/Consoles/Python"), and then get a handle on the console through
GPS.Console. This is because these two consoles are tightly associated with
each of the scripting languages.

If GPS reuses an existing console, on_input overrides the callback that was
already set on the console, whereas on_destroy will be called in addition to
the one that was already set on the console.

If this is not the desired behavior, you can also call destroy() on the
console, and call the constructor again.

- The subprogram ``on_input`` is called whenever the user has entered a new
  command in the console and pressed <enter> to execute it. It is called with
  the following parameters:

   - $1: The instance of the GPS.Console
   - $2: The command to execute

  See the subprogram GPS.Console.set_prompt_regexp for proper handling of input
  in the console.

- The subprogram ``on_destroy`` is called whenever the user closes the
  console. It is called with a single parameter:

   - $1: The instance of the GPS.Console

- The subprogram ``on_completion`` is called whenever the user presses tab in
  the console. It is called with a single parameter:

   - $1: The instance of the GPS.Console

  The default implementation is to insert a tab character, but you could choose
  to add some user input through GPS.Console.add_input for instance.

- The subprogram ``on_resize`` is called whenever the console is resized by the
  user. It is passed three parameters:

    - $1 is the instance of GPS.Console
    - $2 is the number of visible rows in the console,
    - and $3 is the number of visible columns.

  This is mostly useful when a process is running in the console, in which case
  you can use GPS.Process.set_size to let the process know about the size. Note
  that the size passed to this callback is conservative: since all characters
  might not have the same size, GPS tries to compute the maximal number of
  visible characters and pass this to the callback, but the exact number of
  characters might depend on the font.

- The subprogram ``on_interrupt`` is called when the user presses control-c in
  the console. It receives a single parameter, which is the instance of
  GPS.Console. By default a control-c is handled by GPS itself and will kill
  the last process that was started.

As described above, GPS provides a high-level handling of consoles, where it
manages histories, completion, command line editing and execution on its own
through the callbacks described above. This is in general a good thing and
provides advanced functionalities to some programs that lack them. However,
there are cases where this gets in the way. For instance, if you want to run a
Unix shell or a program that manipulates the console by moving the cursor
around on its own, the high-level handling of GPS gets in the way. In such a
case, the following parameters can be used: on_key, manage_prompt and ansi.

- ``ansi`` should be set to true if GPS should emulate an ANSI terminal. These
  are terminals that understand certain escape sequences that applications sent
  to move the cursor to specific positions on screen or to change the color and
  attributes of text.

- ``manage_prompt`` should be set to False to disable GPS's handling of
  prompt. In general, this is incompatible with using the on_input callback,
  since GPS no longer distinguishes what was typed by the user and what was
  written by the external application. This also means that the application is
  free to write anywhere on the screen. This should in general be set to True
  if you expect your application to send ANSI sequences.

- ``on_key`` is a subprogram that is called every time the user presses a key
  in the console. This is much lower-level than the other callbacks above, but
  if you are driving external applications you might have a need to send the
  keys as they happen, and not wait for a newline. on_key receives four
  parameters:

    - $1: the instance of GPS.Console
    - $2: "keycode": this is the internal keycode for the key that the user
          pressed. All keys can be represented this way, but this will
          occasionaly be left to 0 when the user input was simulated and no
          real key was pressed.
    - $3: "key": this is the unicode character that the user entered. This will
          be 0 when the character is not printable (for instance return, tab,
          key up,...). In python, you can manipulate it with code like
          ``unichr(key).encode("utf8")`` to get a string representation that
          can be sent to an external process
    - $4: "modifier": these are the state of the control, shift, mod1 and lock
          keys. This is a bitmask, where shift is 1, lock is 2, control is 4
          and mod1 is 8.

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
        """
        pass  # implemented in Ada

    def accept_input(self):
        """
        Return True if the console accepts input, False otherwise

        :return: A boolean
        """
        pass  # implemented in Ada

    def add_input(self, text):
        """
        Add some extra text to the console as if the user had typed it. As
        opposed to text inserted with GPS.Console.write, this text remains
        editable by the user

        :param text: A string
        """
        pass  # implemented in Ada

    def clear(self):
        """
        Clear the current contents of the console
        """
        pass  # implemented in Ada

    def clear_input(self):
        """
        Removes any user input that the user has started typing (ie since the
        last output inserted through GPS.Console.write
        """
        pass  # implemented in Ada

    def copy_clipboard(self):
        """
        Copy the selection to the clipboard
        """
        pass  # implemented in Ada

    def create_link(self, regexp, on_click):
        """
        Register a regular expression that should be highlight in this console
        to provide hyper links. These links are searched for when calling
        GPS.Console.write_with_links. The part of the text that matches any of
        the link registered in the console through GPS.Console.create_link gets
        highlighted in blue and underlined, just like an hyper link in a web
        browser. If the user clicks on that text, on_click gets called with one
        parameter, the text that was clicked on. This can for instance be used
        to jump to an editor, open a web browser,...

        If the regular expression does not contain any parenthesis, the text
        that matches the whole regexp is highlighted as a link. Otherwise, only
        the part of the text that matches the first parenthesis group is
        highlighted (so that you can test for the presence of text before or
        after the actual hyper link).

        :param regexp: A string
        :param on_click: A subprogram

        .. seealso:: :func:`GPS.Console.write_with_links`
        """
        pass  # implemented in Ada

    def enable_input(self, enable):
        """
        Make the console accept / reject input according to the value of
        "enable"

        :param enable: A boolean
        """
        pass  # implemented in Ada

    def flush(self):
        """
        Do nothing, needed for compatibility with Python's file class
        """
        pass  # implemented in Ada

    def get_text(self):
        """
        Return the content of the console

        :return: A string
        """
        pass  # implemented in Ada

    def isatty(self):
        """
        Return True if the console behaves like a terminal. Mostly needed for
        compatibility with Python's file class

        :return: A boolean
        """
        pass  # implemented in Ada

    def read(self):
        """
        Read the available input in the console. Currently, this behaves
        exactly like readline()

        :return: A String
        """
        pass  # implemented in Ada

    def readline(self):
        """
        Ask the user to enter a new line in the console, and returns that
        line. GPS is blocked until enter has been pressed in the console

        :return: A String
        """
        pass  # implemented in Ada

    def select_all(self):
        """Select the complete contents of the console"""
        pass  # implemented in Ada

    def write(self, text, mode='"text"'):
        """
        Output some text on the console. This text is read-only. If the user
        had started typing some text, that text is temporarily remove, the next
        text is inserted (read-only), and the user text is put back afterward.

        The optional parameter mode specifies the kind of the output text:
        "text" for ordinary messages (this is default), "log" for log messages,
        and "error" for error messages.

        :param text: A utf8 string
        :param mode: A string, one of "text", "log", "error"

        .. seealso:: :func:`GPS.Console.write_with_links`

        .. code-block:: python

            Console().write(u"\N{LATIN CAPITAL LETTER E WITH ACUTE}".encode("utf-8"))
        """
        pass  # implemented in Ada

    def write_with_links(self, text):
        """
        Output some text on the console, highlight the parts of it that matches
        the regular expression registered by GPS.Console.create_link.

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
                 GPS.EditorLocation(buffer, int(matched.group(2)), 1))

        """
        pass  # implemented in Ada


###########################################################
# Contextual
###########################################################

class Contextual(object):
    """
    This class is a general interface to the contextual menus in GPS. It gives
    you control over which menus should be displayed when the user right clicks
    in parts of GPS

    .. seealso:: :func:`GPS.Contextual.__init__`
    """

    def __init__(self, name):
        """
        Initializes a new instance of GPS.Contextual. The name is the name that
        was given to the contextual menu when it was created, and is a static
        string independent of the actual label used when the menu is displayed
        (and which is dynamic, depending on the context). You can get the list
        of valid names by checking the list of names returned by
        GPS.Contextual.list

        :param name: A string

        .. seealso:: :func:`GPS.Contextual.list()`

        .. code-block:: python

           # You could for instance decide to always hide the "Goto
           # declaration" contextual menu with the following call:

           GPS.Contextual ('Goto declaration of entity').hide()

           # After this, the menu will never be displayed again.
        """
        pass  # implemented in Ada

    def create(self, on_activate, label=None, ref='', add_before=True,
               filter=None, group='0'):
        """
Create a new contextual menu entry.  Whenever this menu entry is selected by
the user, GPS will execute on_activate, passing one parameter which is the
context for which the menu is displayed (this is generally the same as
GPS.current_contextual()).

If ``on_activate`` is None, a separator will be created.

The ``filter`` parameter can be used to filter when the entry should be
displayed in the menu. It is a subprogram that receives one parameter, an
instance of GPS.Context, and returns a boolean. If it returns True, the entry
will be displayed, otherwise it is hidden.

The ``label`` parameter can be used to control the text displayed in the
contextual menu.  By default, it is the same as the contextual name (used in
the constructor to GPS.Contextual.__init__).  If specified, it must be a
subprogram that takes an instance of GPS.Context in parameter, and returns a
string, which will be displayed in the menu.

The parameters ``group``, ``ref`` and ``add_before`` can be used to control the
location of the entry within the contextual menu.  group allows you to create
groups of contextual menus that will be put together.  Items of the same group
appear before all items with a greater group number.  ``ref`` is the name of
another contextual menu entry, and add_before indicates whether the new entry
is put before or after that second entry.

:param on_activate: A subprogram with one parameter context
:param label: A subprogram
:param ref: A string
:param add_before: A boolean
:param filter: A subprogram
:param group: An integer

.. code-block:: python

   ## This example demonstrates how to create a contextual
   ## menu with global functions

   def on_contextual(context):
      GPS.Console("Messages").write("You selected the custom entry")

   def on_filter(context):
      return isinstance(context, GPS.EntityContext)

   def on_label(context):
      global count
      count += 1
      return "Custom " + count

   GPS.Contextual("Custom").create(
      on_activate=on_contextual, filter=on_filter, label=on_label)

.. code-block:: python

   ## This example is similar to the one above, but uses a python
   ## class to encapsulate date.
   ## Note how the extra parameter self can be passed to the callbacks
   ## thanks to the call to self.create

   class My_Context(GPS.Contextual):
      def on_contextual(self, context):
          GPS.Console("Messages").write(
              "You selected the custom entry " + self.data)

      def on_filter(self, context):
          return isinstance(context, GPS.EntityContext)

      def on_label(self, context):
          return self.data

      def __init__(self):
          GPS.Contextual.__init__(self, "Custom")
          self.data = "Menu Name"
          self.create(on_activate=self.on_contextual,
                      filter=self.on_filter,
                      label=self.label)
        """
        pass  # implemented in Ada

    def create_dynamic(self, factory, on_activate, label='', filter=None,
                       ref='', add_before=True, group='0'):
        """
Create a new dynamic contextual menu.

This is a submenu of a contextual menu, where the entries are generated by the
factory parameter. This parameter should return a list of strings, which will
be converted to menus by GPS. These strings can contain '/' characters to
indicate submenus.

``filter`` is a subprogram that takes the GPS.Context as a parameter, and
returns a boolean indicating whether the submenu should be displayed.

``label`` can be used to specify the label to use for the menu entry. It can
include directory-like syntax to indicate submenus. This label can include
standard macro substitution (see the GPS documentation), for instance %e for
the current entity name.

``on_activate`` is called whenever any of the entry of the menu is selected,
and is passed three parameters, the context in which the contextual menu was
displayed, the string representing the selected entry and the index of the
selected entry within the array returned by factory (index starts at 0).

The parameters ``ref`` and ``add_before`` can be used to control the location
of the entry within the contextual menu. ref is the name of another contextual
menu entry, and add_before indicates whether the new entry is put before or
after that second entry.

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
      return isinstance(context, GPS.EntityContext)

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
         return isinstance(context, GPS.EntityContext)

      def on_activate(self, context, choice):
         GPS.Console("Messages").write("You selected " + choice)

      def factory(self, context):
         return ["Choice1", "Choice2"]
        """
        pass  # implemented in Ada

    def hide(self):
        """
        Make sure the contextual menu will never appear when the user right
        clicks anywhere in GPS. This is the standard way to disable contextual
        menus

        .. seealso:: :func:`GPS.Contextual.show`
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        Return the list of all registered contextual menus. This is a list of
        strings which are valid names that can be passed to the constructor of
        GPS.Contextual. These names were created when the contextual menu was
        registered in GPS.

        :return: a list of strings

        .. seealso:: :func:`GPS.Contextual.__init__()`
        """
        pass  # implemented in Ada

    def set_sensitive(self, Sensitivity):
        """
        Control whether the contextual menu is grayed-out: False if it should
        be grayed-out, True otherwise.

        :param Sensitivity: Boolean value
        """
        pass  # implemented in Ada

    def show(self):
        """
        Make sure the contextual menu will be shown when appropriate. The entry
        might still be invisible if you right clicked on a context where it
        doesn't apply, but it will be checked

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

    While developping such debugger interfaces, it might be useful to modify
    the file $HOME/.gps/traces.cfg, and add a line "GVD.Out=yes" in it. This
    will copy all input/output with the debuggers into the GPS log file.

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

    def __init__(self):
        """
        It is an error to create a Debugger instance directly. Instead, use
        GPS.Debugger.get() or GPS.Debugger.spawn()

        .. seealso::

           :func:`GPS.Debugger.get`

           :func:`GPS.Debugger.spawn`
        """
        pass  # implemented in Ada

    def close(self):
        """
        Closes the given debugger. This also closes all associated windows
        (call stack, console,...)
        """
        pass  # implemented in Ada

    def command(self):
        """
        Return the command that is being executed in the debugger. This is
        often only available when called from the debugger_state_changed hook,
        where it might also indicate the command that just finished

        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get(id=None):
        """
        This command gives access to an already running debugger, and will
        return an instance of GPS.Debugger attached to it. The parameter can be
        null, in which case the current debugger is returned; it can be an
        integer, in which case the corresponding debugger is returned (starting
        at 1); or it can be a file, in which case this function returns the
        debugger currently debugging that file.

        :param id: Either an integer or an instance of :class:`GPS.File`
        :return: An instance of :class:`GPS.Debugger`

        """
        pass  # implemented in Ada

    def get_executable(self):
        """
        Returns the name of the executable currently debugged in that debugger

        :return: An instance of :class:`GPS.File`

        .. seealso:: :func:`GPS.Debugger.get_num`
        """
        pass  # implemented in Ada

    def get_num(self):
        """
        Returns the index of the debugger. This can be used later on to
        retrieve the debugger from GPS.Debugger.get(), or to get access to
        other windows associated with that debugger

        :return: An integer

        .. seealso:: :func:`GPS.Debugger.get_file`
        """
        pass  # implemented in Ada

    def is_break_command(self):
        """
        Return true if the command returned by GPS.Debugger.command is likely
        to modify the list of breakpoints after it has finished executing

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_busy(self):
        """
        Returns true if the debugger is currently executing a command. In this
        case, it is an error to send a new command to it

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_context_command(self):
        """
        Return true if the command returned by GPS.Debugger.command is likely
        to modify the current context (current task, thread,...) after it has
        finished executing

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_exec_command(self):
        """
        Return true if the command returned by GPS.Debugger.command is likely
        to modify the stack trace in the debugger ("next", "cont", ...)

        :return: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        This command returns the list of currently running debuggers

        :return: A list of :class:`GPS.Debugger` instances
        """
        pass  # implemented in Ada

    def non_blocking_send(self, cmd, output=True):
        """
        This command works like send, but is not blocking, and does not return
        the result.

        :param cmd: A string
        :param output: A boolean

        .. seealso:: :func:`GPS.Debugger.send`
        """
        pass  # implemented in Ada

    def send(self, cmd, output=True, show_in_console=False):
        """
        This command executes cmd in the debugger. GPS is blocked while cmd is
        executing on the debugger. If output is true, the command is displayed
        in the console.

        If ``show_in_console`` is True, the output of the command is displayed in
        the debugger console, but is not returned by this function. If
        ``show_in_console`` is False, the result is not displayed in the console,
        but is returned by this function

        :param cmd: A string
        :param output: A boolean
        :param show_in_console: A boolean
        :return: A string

        .. seealso:: :func:`GPS.Debugger.non_blocking_send`
        """
        pass  # implemented in Ada

    @staticmethod
    def spawn(executable, args=''):
        """
        This command starts anew debugger. It will debug file. When file is
        executed, the extra arguments args are passed

        :param executable: An instance of GPS.File
        :param args: A string
        :return: An instance of :class:`GPS.Debugger`
        """
        pass  # implemented in Ada


###########################################################
# Docgen
###########################################################

class Docgen(object):
    """
    Interface for handling customized documentation generation. This class is
    used in conjunction with GPS.DocgenTagHandler. You cannot create directly
    this class, but use the ones furnished in GPS.DocgenTagHandler callbacks.

    .. seealso:: :func:`GPS.DocgenTagHandler`
    """

    def generate_index_file(self, name, filename, content):
        """
        Create a new Index file. The file 'filename' will be titled 'name', and
        will contain the general decoration along with 'content'.

        All other generated documentation file will have a link to it for
        convenience.

        :param name: The name of the new index file.
        :param filename: The created file name.
        :param content: The content of the created file.
        """
        pass  # implemented in Ada

    def get_current_file(self):
        """
        Retrieves the current analysed source file. You should call this method
        only from a GPS.DocgenTagHandler.on_match() callback.

        :return: A :class:`GPS.File` instance
        """
        pass  # implemented in Ada

    def get_doc_dir(self):
        """
        Retrieves the directory that will contain the documentation. You should
        call this method only from a GPS.DocgenTagHandler.on_match() callback.

        :return: A :class:`GPS.File` instance

        """
        pass  # implemented in Ada

    @staticmethod
    def register_css(filename):
        """
        Registers a new CSS file to use when generating the documentation. This
        allows either to override a default style, or add new ones for custom
        tags handling

        :param filename: A file name
        """
        pass  # implemented in Ada

    @staticmethod
    def register_main_index(filename):
        """
        Registers the file to be used as main page (e.g. index.html). By
        default, the first page generated in the Table of Contents is used.

        :param filename: A file name
        """
        pass  # implemented in Ada

    @staticmethod
    def register_tag_handler(handler):
        """
        Registers a new tag handler. This handler will be used each time a new
        documentation is generated and the corresponding tag is found

        :param handler: The handler to register

        .. code-block:: python

           # register a default handler for tag <description>
           # that is, -- <description>sth</description>
           # will be translated as <div class="description">sth</div>
           GPS.Docgen.register_tag_handler(GPS.DocgenTagHandler ("description"))
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
               on_match=self.on_match, on_start=self.on_start, on_exit=self.on_exit)

          def on_start(self, docgen):
             self.list = {}

          def on_match(self, docgen, attrs, value, entity_name, entity_href):
             # In this examples, images are in the directory _project_root_/doc/imgs/

             dir = docgen.get_current_file().project().file().directory()+"doc/imgs/"
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
                content += "      <a href="%s">%s</a>" % (self.list[pict][0], self.list[pict][1])
                content += "    </div>"
                content += "  </div>"
                content += "</div>"

             if content != "":
                docgen.generate_index_file("Screenshots", "screenshots.html", content)

       def on_gps_start(hook):
          GPS.Docgen.register_css(GPS.get_system_dir() + "share/mycustomfiles/custom.css")
          GPS.Docgen.register_tag_handler(ScreenshotTagHandler())

       GPS.Hook("gps_started").add(on_gps_start)
    """

    def __init__(self, tag, on_start=None, on_match=None, on_exit=None):
        """
        Create a new GPS.DocgenTagHandler instance handling the tag "tag". You
        need to register it afterwards using GPS.Docgen.register_tag_handler.

``on_match`` is a callback that is called each time a tag corresponding to the
GPS.DocgenTagHandler is analysed. It takes the following parameters:

-  $1 = the instance of GPS.Docgen.
-  $2 = the eventual attributes of the tag.
-  $3 = the value of the tag.
-  $4 = the entity name linked to the analysed tag.
-  $5 = the href to the entity documentation location.

``on_start`` is a callback that is called each time a documentation generation
starts. It takes the following parameters:

-  $1 = the instance of GPS.Docgen.

``on_exit`` is a callback that is called each time a documentation generation
finishes. It takes the following parameters:

-   $1 = the instance of GPS.Docgen.

Using the default values of the callbacks (e.g. None), the GPS.DocgenTagHandler
handler will translate comments of the form "-- <tag>value</tag>" by "<div
class="tag">value</div>".

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
    Deprecated interface to all editor-related commands
    """

    @staticmethod
    def add_blank_lines(file, start_line, number_of_lines, category=''):
        """
        OBSOLESCENT.

        Adds number_of_lines non-editable lines to the buffer editing file,
        starting at line start_line. If category is specified, use it for
        highlighting. Create a mark at beginning of block and return its ID

        :param file: A string
        :param start_line: An integer
        :param number_of_lines: An integer
        :param category: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def add_case_exception(name):
        """
        OBSOLESCENT.

        Add name into the case exception dictionary

        :param name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def block_fold(file, line=None):
        """
        OBSOLESCENT.

        Fold the block around line. If line is not specified, fold all blocks
        in the file.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_end(file, line):
        """
        OBSOLESCENT.

        Returns ending line number for block enclosing line

        :param file: A string
        :param line: An integer
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_level(file, line):
        """
        OBSOLESCENT.

        Returns nested level for block enclosing line

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

        Returns ending line number for block enclosing line

        :param file: A string
        :param line: An integer
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def block_get_type(file, line):
        """
        OBSOLESCENT.

        Returns type for block enclosing line

        :param file: A string
        :param line: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def block_unfold(file, line=None):
        """
        OBSOLESCENT.

        Unfold the block around line. If line is not specified, unfold all
        blocks in the file.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def close(file):
        """
        OBSOLESCENT.

        Close all file editors for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def copy():
        """
        OBSOLESCENT.

        Copy the selection in the current editor
        """
        pass  # implemented in Ada

    @staticmethod
    def create_mark(filename, line=1, column=1, length=0):
        """
        Create a mark for file_name, at position given by line and
        column. Length corresponds to the text length to highlight after the
        mark. The identifier of the mark is returned. Use the command goto_mark
        to jump to this mark

        :param filename: A string
        :param line: An integer
        :param column: An integer
        :param length: An integer
        :return: A string

        .. seealso::

             :func:`GPS.Editor.goto_mark`

             :func:`GPS.Editor.delete_mark`
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_center(file):
        """
        OBSOLESCENT.

        Scroll the view to center cursor

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_get_column(file):
        """
        OBSOLESCENT.

        Returns current cursor column number

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_get_line(file):
        """
        OBSOLESCENT.

        Returns current cursor line number

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def cursor_set_position(file, line, column=1):
        """
        OBSOLESCENT.

        Set cursor to position line/column in buffer file

        :param file: A string
        :param line: An integer
        :param column: An integer

        """
        pass  # implemented in Ada

    @staticmethod
    def cut():
        """
        OBSOLESCENT.

        Cut the selection in the current editor
        """
        pass  # implemented in Ada

    @staticmethod
    def delete_mark(identifier):
        """
        OBSOLESCENT.

        Delete the mark corresponding to identifier

        :param identifier: A string

        .. seealso:: :func:`GPS.Editor.create_mark`
        """
        pass  # implemented in Ada

    @staticmethod
    def edit(filename, line=1, column=1, length=0, force=False, position=5):
        """
        OBSOLESCENT.

        Open a file editor for file_name. Length is the number of characters to
        select after the cursor. If line and column are set to 0, then the
        location of the cursor is not changed if the file is already opened in
        an editor. If force is set to true, a reload is forced in case the file
        is already open. Position indicates the MDI position to open the child
        in (5 for default, 1 for bottom).

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

        Returns the text contained in the current buffer for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_chars(filename, line=0, column=1, before=-1, after=-1):
        """
        OBSOLESCENT.

        Get the characters around a certain position. Returns string between
        "before" characters before the mark and "after" characters after the
        position. If "before" or "after" is omitted, the bounds will be at the
        beginning and/or the end of the line.

        If the line and column are not specified, then the current selection is
        returned, or the empty string if there is no selection

        :param filename: A string
        :param line: An integer
        :param column: An integer
        :param before: An integer
        :param after: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_column(mark):
        """
        OBSOLESCENT.

        Returns the current column of mark

        :param mark: An identifier
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def get_file(mark):
        """
        OBSOLESCENT.

        Returns the current file of mark

        :param mark: An identifier
        :return: A file
        """
        pass  # implemented in Ada

    @staticmethod
    def get_last_line(file):
        """
        OBSOLESCENT.

        Returns the number of the last line in file

        :param file: A string
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def get_line(mark):
        """
        OBSOLESCENT.

        Returns the current line of mark

        :param mark: An identifier
        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def goto_mark(identifier):
        """
        OBSOLESCENT.

        Jump to the location of the mark corresponding to identifier

        :param identifier: A string

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

        Highlights a portion of a line in a file with the given category

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

        Indent the selection (or the current line if requested) in current
        editor. Do nothing if the current GPS window is not an editor

        :param current_line_only: A boolean

        """
        pass  # implemented in Ada

    @staticmethod
    def indent_buffer():
        """
        OBSOLESCENT.

        Indent the current editor. Do nothing if the current GPS window is not
        an editor
        """
        pass  # implemented in Ada

    @staticmethod
    def insert_text(text):
        """
        OBSOLESCENT.

        Insert a text in the current editor at the cursor position

        :param text: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def mark_current_location():
        """
        OBSOLESCENT.

        Push the location in the current editor in the history of locations.
        This should be called before jumping to a new location on a user's
        request, so that he can easily choose to go back to the previous
        location.
        """
        pass  # implemented in Ada

    @staticmethod
    def paste():
        """
        OBSOLESCENT.

        Paste the selection in the current editor
        """
        pass  # implemented in Ada

    @staticmethod
    def print_line_info(file, line):
        """
        OBSOLESCENT.

        Print the contents of the items attached to the side of a line. This is
        used mainly for debugging and testing purposes.

        :param file: A string
        :param line: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def redo(file):
        """
        OBSOLESCENT.

        Redo the last undone edition command for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def refill():
        """
        OBSOLESCENT.

        Refill selected (or current) editor lines. Do nothing if the current
        GPS window is not an editor
        """
        pass  # implemented in Ada

    @staticmethod
    def register_highlighting(category, color, speedbar=False):
        """
        OBSOLESCENT.

        Create a new highlighting category with the given color. The format for
        color is "#RRGGBB". If speedbar is true, then a mark will be inserted
        in the speedbar to the left of the editor to give a fast overview to
        the user of where the highlighted lines are.

        :param category: A string
        :param color: A string
        :param speedbar: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_blank_lines(mark, number=0):
        """
        OBSOLESCENT

        Remove blank lines located at mark. If number is specified, remove only
        the number first lines

        :param mark: A string
        :param number: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_case_exception(name):
        """
        OBSOLESCENT.

        Remove name from the case exception dictionary

        :param name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def replace_text(file, line, column, text, before=-1, after=-1):
        """
        OBSOLESCENT.

        Replace the characters around a certain position. "before" characters
        before (line, column), and up to "after" characters after are removed,
        and the new text is inserted instead. If "before" or "after" is
        omitted, the bounds will be at the beginning and/or the end of the line

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

        Save current or all files. If interactive is true, then prompt before
        each save. If all is true, then all files are saved

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
        will not be modified

        :param file: A string
        :param to_file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def select_all():
        """
        OBSOLESCENT.

        Select the whole editor contents
        """
        pass  # implemented in Ada

    @staticmethod
    def select_text(first_line, last_line, start_column=1, end_column=0):
        """
        OBSOLESCENT.

        Select a block in the current editor

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

        Set the background color for the editors for file

        :param file: A string
        :param color: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_synchronized_scrolling(file1, file2, file3=''):
        """
        OBSOLESCENT.

        Synchronize the scrolling between multiple editors

        :param file1: A string
        :param file2: A string
        :param file3: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_title(file, title, filename):
        """
        OBSOLESCENT.

        Change the title of the buffer containing the given file

        :param file: A string
        :param title: A string
        :param filename: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_writable(file, writable):
        """
        OBSOLESCENT.

        Change the Writable status for the editors for file

        :param file: A string
        :param writable: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def subprogram_name(file, line):
        """
        OBSOLESCENT.

        Returns the name of the subprogram enclosing line

        :param file: A string
        :param line: An integer
        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def undo(file):
        """
        OBSOLESCENT.

        Undo the last edition command for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def unhighlight(file, category, line=0):
        """
        OBSOLESCENT.

        Unmarks the line for the specified category. If line is not specified,
        unmark all lines in file

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

        Remove highlights for a portion of a line in a file

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
    associated with at least one view (a GPS.EditorView instance), which makes
    it visible to the user. The contents of the file can be manipulated through
    this class
    """

    def __init__(self):
        """
        This function prevents the direct creation of instances of
        EditorBuffer. Use :func:`GPS.EditorBuffer.get` instead
        """
        pass  # implemented in Ada

    def add_special_line(self, start_line, text, category='', name=''):
        """
        Adds one non-editable line to the buffer, starting at line start_line
        and contains string text. If category is specified, use it for
        highlighting. Create a mark at beginning of block and return it. If
        name is specified, retuned mark will have this name

        :param start_line: An integer
        :param text: A string
        :param category: A string
        :param name: A string
        :return: An instance of GPS.EditorMark

        .. seealso:: :func:`GPS.EditorBuffer.get_mark`
        """
        pass  # implemented in Ada

    def apply_overlay(self, overlay, frm='begining of buffer', to='end of buffer'):
        """
        Applies the overlay to the given range of text. This immediately
        changes the rendering of the text based on the properties of the
        overlay

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.EditorBuffer.remove_overlay`
        """
        pass  # implemented in Ada

    def beginning_of_buffer(self):
        """
        Returns a location pointing to the first character in the buffer

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def blocks_fold(self):
        """
        Folds all the blocks in all the views of the buffer. Block folding is a
        language-dependent feature, whereby one can hide part of the source
        code temporarily, by keeping only the first line of the block (for
        instance the first line of a subprogram body, the rest is hidden). A
        small icon is displayed to the left of the first line so that it can be
        unfolded later on

        .. seealso::

            :func:`GPS.EditorBuffer.blocks_unfold`

            :func:`GPS.EditorLocation.block_fold`
        """
        pass  # implemented in Ada

    def blocks_unfold(self):
        """
        Unfolds all the blocks that were previously folded in the buffer, ie
        make the whole source code visible. This is a language dependent
        feature

        .. seealso::

            :func:`GPS.EditorBuffer.blocks_fold`

            :func:`GPS.EditorLocation.block_unfold`
        """
        pass  # implemented in Ada

    def characters_count(self):
        """
        Returns the total number of characters in the buffer

        :return: An integer
        """
        pass  # implemented in Ada

    def close(self, force=False):
        """
        Closes the editor and all its views. If the buffer has been modified
        and not saved, a dialog is open asking the user whether to save. If
        force is True, do not save and do not ask the user. All changes are
        lost

        :param force: A boolean
        """
        pass  # implemented in Ada

    def copy(self, frm='beginning of buffer', to='end of buffer',
             append=False):
        """
        Copy the given range of text into the clipboard, so that it can be
        further pasted into other applications or other parts of GPS. If append
        is True, the text is appended to the last clipboard entry instead of
        generating a new one

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`
        :param append: A boolean

        .. seealso:: :func:`GPS.Clipboard.copy`
        """
        pass  # implemented in Ada

    def create_overlay(self, name=''):
        """
        Create a new overlay. Properties can be set on this overlay, which can
        then be applied to one or more ranges of text to changes its visual
        rqendering or to associate user data with it. If name is specified,
        this function will return an existing overlay with the same name in
        this buffer if any can be found. If the name is not specified, a new
        overlay is created. Changing the properties of an existing overlay
        results in an immediate graphical update of the views associated with
        the buffer.

        A number of predefined overlay exits. Among these are the ones used for
        syntax highlighting by GPS itself, which are "keyword", "comment",
        "string", "character". You can use these to navigate from one comment
        section to the next for instance.

        :param name: A string
        :return: An instance of :class:`GPS.EditorOverlay`
        """
        pass  # implemented in Ada

    def current_view(self):
        """
        Returns the last view used for this buffer, ie the last view that had
        the focus and through which the user might have edited the buffer's
        contents

        :return: An instance of :class:`GPS.EditorView`
        """
        pass  # implemented in Ada

    def cut(self, frm='beginning of buffer', to='end of buffer',
            append=False):
        """
        Copy the given range of text into the clipboard, so that it can be
        further pasted into other applications or other parts of GPS. The text
        is removed from the edited buffer. If append is True, the text is
        appended to the last clipboard entry instead of generating a new one

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`
        :param append: A boolean
        """
        pass  # implemented in Ada

    def delete(self, frm='beginning of buffer', to='end of buffer'):
        """
        Delete the given range of text from the buffer

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def end_of_buffer(self):
        """
        Returns a location pointing to the last character in the buffer

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def file(self):
        """
        Returns the name of the file edited in this buffer

        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def finish_undo_group(self):
        """
        Cancels the grouping of commands on the editor. See
        GPS.EditorBuffer.start_undo_group
        """
        pass  # implemented in Ada

    @staticmethod
    def get(file='current editor', force=False, open=True):
        """
        If file is already opened in an editor, get a handle on its
        buffer. This instance is then shared with all other buffers referencing
        the same file. As a result, you can for instance associate your own
        data with the buffer, and retrieve it at any time until the buffer is
        closed. If the file is not opened yet, it is loaded in a new editor,
        and a new view is opened at the same time (and thus the editor becomes
        visible to the user).  If file is not specified, the current editor is
        returned, ie the last one that had the keyboard focus.

        If the file is not currently open, the behavior depends on the open
        parameter: if true, a new editor is created for that file, otherwise
        None is returned.

        When a new file is open, it has received the focus. But if the editor
        already existed, it is not raised explicitly, and you need to do it
        yourself through a call to GPS.MDIWindow.raise_window (see the example
        below).

        If force is set to true, a reload is forced in case the file is already
        open.

        :param file: An instance of :class:`GPS.File`
        :param force: A boolean
        :param open: A boolean
        :return: An instance of :class:`GPS.EditorBuffer`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File ("a.adb"))
           GPS.MDI.get_by_child(ed.current_view()).raise_window()
           ed.data = "whatever"

           # ... Whatever, including modifying ed

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           ed.data   # => "whatever"
        """
        pass  # implemented in Ada

    def get_chars(self, frm='beginning of buffer', to='end of buffer'):
        """
        Returns the contents of the buffer between the two locations given in
        parameter. Modifying the returned value has no effect on the buffer

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`
        :return: A string
        """
        pass  # implemented in Ada

    def get_mark(self, name):
        """
        Check whether there is a mark with that name in the buffer, and return
        it. An exception is raised if there is no such mark

        :param name: A string
        :return: An instance of :class:`GPS.EditorMark`

        .. seealso:: :func:`GPS.EditorLocation.create_mark`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = GPS.EditorLocation(ed, 4, 5)
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
        Open a new editor on a blank file. This file has no name, and you'll
        have to provide one when you save it

        :return: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def indent(self, frm='beginning of buffer', to='end of buffer'):
        """
        Recompute the indentation of the given range of text. This feature is
        language-dependent

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def insert(self, location, text):
        """
        Inserts some text in the buffer

        :param location: An instance of :class:`GPS.EditorLocation`
        :param text: A string

        .. seealso:: :func:`GPS.EditorBuffer.delete`
        """
        pass  # implemented in Ada

    def is_modified(self):
        """
        Tests whether the buffer has been modified since it was last open or
        saved

        :return: A boolean
        """
        pass  # implemented in Ada

    def is_read_only(self):
        """
        Whether the buffer is editable or not.

        :return: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.set_read_only`
        """
        pass  # implemented in Ada

    def lines_count(self):
        """
        Returns the total number of lines in the buffer

        :return: An integer
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        This function returns the list of all editors that are currently open
        in GPS.

        :return: A list of instances of :class:`GPS.EditorBuffer`

        .. code-block:: python

           # It is possible to close all editors at once using a command like

           for ed in GPS.EditorBuffer.list():
               ed.close()
        """
        pass  # implemented in Ada

    def paste(self, location):
        """
        Paste the contents of the clipboard at the given location in the buffer

        :param location: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def redo(self):
        """Redo the last undone command on the editor"""
        pass  # implemented in Ada

    def refill(self, frm='beginning of buffer', to='end of buffer'):
        """
        Refill the given range of text, ie cut long lines if necessary so that
        they fit in the limit specified in the GPS preferences

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`

        """
        pass  # implemented in Ada

    def remove_overlay(self, overlay, frm='begining of buffer',
                       to='end of buffer'):
        """
        Removes all instances of the overlay in the given range of text. It
        isn't an error if the overlay is not applied to any of the character in
        the range, it just has no effect in that case.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.EditorBuffer.apply_overlay`

        """
        pass  # implemented in Ada

    def remove_special_lines(self, mark, lines):
        """
        Removes specified number of special lines at the specified mark. It
        doesn't delete the mark

        :param mark: An instance of :class:`GPS.EditorMark`
        :param lines: An integer
        """
        pass  # implemented in Ada

    def save(self, interactive=True, file='Same file as edited by the buffer'):
        """
        Saves the buffer to the given file. If interactive is true, a dialog is
        open to ask for confirmation from the user first, which gives him a
        chance to cancel the saving. "interactive" is ignored if file is
        specified.

        :param interactive: A boolean
        :param file: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def select(self, frm='beginning of buffer', to='end of buffer'):
        """
        Selects an area in the buffer. The boundaries are included in the
        selection. The order of the boundaries is irrelevant, but the cursor
        will be left on to

        :param frm: An instance of :class:`GPS.EditorLocation`
        :param to: An instance of :class:`GPS.EditorLocation`

        """
        pass  # implemented in Ada

    def selection_end(self):
        """
        Return the character after the end of the selection. This will always
        be located after the start of the selection, no matter the order of
        parameters given to GPS.EditorBuffer.select. If the selection is empty,
        EditorBuffer.selection_start and EditorBuffer.selection_end will be
        equal.

        :return: An instance of :class:`GPS.EditorLocation`

        .. code-block:: python

           # To get the contents of the current selection, one would use:

           buffer = GPS.EditorBuffer.get()
           selection = buffer.get_chars(
               buffer.selection_start(), buffer.selection_end() - 1)

        """
        pass  # implemented in Ada

    def selection_start(self):
        """
        Return the start of the selection. This will always be located before
        the end of the selection, no matter the order of parameters given to
        GPS.EditorBuffer.select

        :return: An instance of :class:`GPS.EditorLocation`

        """
        pass  # implemented in Ada

    def set_read_only(self, read_only=True):
        """
        Indicates whether the user should be able to edit the buffer
        interactively (through any view).

        :param read_only: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.is_read_only`

        """
        pass  # implemented in Ada

    def start_undo_group(self):
        """
        Starts grouping commands on the editor. All future editions will be
        considered as belonging to the same group. finish_undo_group should be
        called once for every call to start_undo_group.
        """
        pass  # implemented in Ada

    def undo(self):
        """Undo the last command on the editor"""
        pass  # implemented in Ada

    def unselect(self):
        """Cancel the current selection in the buffer"""
        pass  # implemented in Ada

    def views(self):
        """
        Returns the list of all views currently editing the buffer. There is
        always at least one such view. When the last view is destroyed, the
        buffer itself is destroyed

        :return: A list of :class:`GPS.EditorView` instances

        """
        pass  # implemented in Ada


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
             #  left-clicking on an URL will open the default browser to this URL
             #  middle-clicking will call "wget" to get the source of this URL and
             #    open the output in a new editor

             h=GPS.EditorHighlighter ("http(s)?://[^\s:,]*", view_html, 0, wget_url)

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
        Return a new location located count characters after self. If count is
        negative, this moves backward in the buffer. It is more conveniently
        used through the standard + operator in python

        :param count: An integer
        :return: An instance of GPS.EditorLocation

        .. seealso::

            :func:`GPS.EditorLocation.__sub__`

            :func:`GPS.EditorLocation.forward_char`

        .. code-block:: python

            ed   = GPS.EditorBuffer.get(GPS.File("a.adb"))
            loc  = GPS.EditorLocation(ed, line=4, column=5)
            loc2 = loc + 3
        """
        pass  # implemented in Ada

    def __cmp__(self, location):
        """
        Internal subprogram used to implement the comparison of two
        locations. It returns -1, 0, or 1 depending on whether the first
        location is before, equal or after the second one. This is more
        conveniently used through the usual <, == and > operators in most
        languages

        :param location: An instance of :class:`GPS.EditorLocation`
        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, buffer, line, column):
        """
        Initializes a new instance. Creating two instances at the same location
        will not return the same instance of GPS.EditorLocation, and therefore
        any user data you have stored in the location will not be available in
        the second instance

        :param buffer: The instance of GPS.EditorBuffer
        :param line: An integer
        :param column: An integer

        .. code-block:: python

           ed  = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = GPS.EditorLocation(ed, line=4, column=5)
           loc.data = "MY OWN DATA"
           loc2 = GPS.EditorLocation(ed, line=4, column=5)
           # loc2.data is not defined at this point

        """
        pass  # implemented in Ada

    def __sub__(self, count):
        """
        Return a new location located count characters before self, if count is
        an integer. If count is negative, moves forward instead. If count is
        another location, it returns the number of characters between the two
        locations. This function is more conveniently used through the standard
        "-" operator in python.

        :param count: An integer or another instance of GPS.EditorLocation
        :return: A new instance of GPS.EditorLocation

        .. seealso::

            :func:`GPS.EditorLocation.__add__`

            :func:`GPS.EditorLocation.forward_char`
        """
        pass  # implemented in Ada

    def backward_overlay(self, overlay=None):
        """
        Same as GPS.EditorLocation.forward_overlay, but moves backward
        instead. If there are no more changes, the location is left at the
        beginning of the buffer.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: An instance of GPS.EditorLocation
        """
        pass  # implemented in Ada

    def beginning_of_line(self):
        """
        Return a location located at the beginning of the line on which self
        is.

        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_end(self):
        """
        Return the location of the end of the current block

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_end_line(self):
        """
        Return the last line of the block surrounding the location. The
        definition of a block depends on the specific language of the source
        file

        :return: An integer
        """
        pass  # implemented in Ada

    def block_fold(self):
        """
        Fold the block containing the location, ie make it invisible on the
        screen, except for its first line. Clicking on the icon next to this
        first line will unfold the block and make it visible to the user

        .. seealso:: :func:`GPS.EditorLocation.block_unfold`
        """
        pass  # implemented in Ada

    def block_level(self):
        """
        Return the nesting level of the block surrounding the location. The
        definition of a block depends on the specific programming language

        :return: An integer
        """
        pass  # implemented in Ada

    def block_name(self):
        """
        Return the name of the bock surrounding the location. The definition of
        a block depends on the specific language of the source file

        :return: A string
        """
        pass  # implemented in Ada

    def block_start(self):
        """
        Return the location of the beginning of the current block

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def block_start_line(self):
        """
        Return the first line of the block surrounding the location. The
        definition of a block depends on the programming language

        :return: An integer
        """
        pass  # implemented in Ada

    def block_type(self):
        """
        Return the type of the block surrounding the location. This type
        indicates whether the block is a subprogram, an if statement,...

        :return: A string
        """
        pass  # implemented in Ada

    def block_unfold(self):
        """
        Unfold the block containing the location, ie make it visible any
        information that was hidden as a result of running
        GPS.EditorLocation.block_fold

        .. seealso:: :func:`GPS.EditorLocation.block_fold`
        """
        pass  # implemented in Ada

    def buffer(self):
        """
        Return the buffer in which the location is found

        :return: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def column(self):
        """
        Return the column of the location

        :return: An integer
        """
        pass  # implemented in Ada

    def create_mark(self, name=''):
        """
        Create a mark at that location in the buffer. The mark will stay
        permanently at that location, and follows if the buffer is modified. If
        the name is specified, this creates a named mark, which can be
        retrieved through a call to GPS.EditorBuffer.get_mark. If a mark with
        the same name already exists, it is moved to the new location, and then
        returned

        :param name: A string
        :return: An instance of :class:`GPS.EditorMark`

        .. seealso:: :func:`GPS.EditorBuffer.get_mark`

        .. code-block:: python

           buffer = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = GPS.EditorLocation(buffer, 3, 4)
           mark = loc.create_mark()
           buffer.insert(loc, "text")
           loc = mark.location()
           # loc.column() is now 8
        """
        pass  # implemented in Ada

    def end_of_line(self):
        """
        Return a location located at the end of the line on which self is.

        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def ends_word(self):
        """
        Return true if self is currently at the end of a word. The definition
        of a word depends on the language used

        :return: A boolean
        """
        pass  # implemented in Ada

    def forward_char(self, count):
        """
        Return a new location located count characters after self. If count is
        negative, the location is moved backward instead

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def forward_line(self, count):
        """
        Return a new location located count lines after self. The location is
        moved back to the beginning of the line. In case self is on the last
        line, the beginning of the last line is returned.

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def forward_overlay(self, overlay=''):
        """
        Moves to the next change in the list of overlays applying to the
        character. If overlay is specified, go to the next change for this
        specific overlay (ie the next beginning or end of range where it
        applies). If there are no more changes, the location is left at the end
        of the buffer.

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: An instance of :class:`GPS.EditorLocation`

        .. seealso:: :func:`GPS.EditorLocation.backward_overlay`
        """
        pass  # implemented in Ada

    def forward_word(self, count):
        """
        Return a new location located count words after self. If count is
        negative, the location is moved backward instead. The definition of a
        word depends on the language used

        :param count: An integer
        :return: A new instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def get_char(self):
        """
        Return the character at that location in the buffer. An exception is
        raised when trying to read past the end of the buffer. The character
        might be encoded on several bytes, since it is a UTF8 string.

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
        This function returns the list of all the overlays that apply at this
        specific location. The color and font of the text is composed through
        the contents of these overlays.

        :return: A list of :class:`GPS.EditorOverlay` instances

        """
        pass  # implemented in Ada

    def has_overlay(self, overlay):
        """
        This function returns True if the given overlay applies to the
        character at that location

        :param overlay: An instance of :class:`GPS.EditorOverlay`
        :return: A boolean
        """
        pass  # implemented in Ada

    def inside_word(self):
        """
        Return true if self is currently inside a word. The definition of a
        word depends on the language used

        :return: A boolean
        """
        pass  # implemented in Ada

    def line(self):
        """
        Return the line of the location

        :return: An integer
        """
        pass  # implemented in Ada

    def offset(self):
        """
        Return the offset of the location in the buffer, ie the number of
        characters from the beginning of the buffer to the location

        :return: An integer
        """
        pass  # implemented in Ada

    def search(self, pattern, backward=False, case_sensitive=False,
               regexp=False, whole_word=False, scope='Whole',
               dialog_on_failure=True):
        """
        This function searches for the next occurrence of Pattern in the
        editor, starting at the given location. If there is such a match, this
        function returns the two locations for the beginning of the match and
        the end of the match. Typically, these would be used to highlight the
        match in the editor.

        When no match is found, this function returns null. Additionally, if
        dialog_on_failure is true then a dialog is displayed to the user asking
        whether the search should restart at the beginning of the buffer.

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
        Return true if self is currently at the start of a word. The definition
        of a word depends on the language used

        :return: A boolean
        """
        pass  # implemented in Ada

    def subprogram_name(self):
        """
        Return the name of the subprogram enclosing the location

        :return: A string
        """
        pass  # implemented in Ada


###########################################################
# EditorMark
###########################################################

class EditorMark(object):
    """
    This class represents a specific location in an open editor. As opposed to
    the GPS.EditorLocation class, the exact location is updated whenever the
    buffer is modified. For instance, if you add a line before the mark, then
    the mark is moved one line forward as well, so that it still points to the
    same character in the buffer.

    The mark remains valid even if you close the buffer; or if you reopen it
    and modify it. It will always point to the same location in the file, while
    you have kept the python object.

    :func:`GPS.EditorLocation.create_mark` allows you to create named marks
    which you can then retrieve through GPS.EditorBuffer.get_mark. Such named
    marks are only valid while the editor exists. As soon as you close the
    editor, you can no longer use get_mark to retrieve it (but the mark is
    still valid if you have kept a python object referencing it).

    .. seealso:: :func:`GPS.EditorLocation`

    """

    def __del__(self):
        """
        This subprogram is automatically called whenever self is unreferenced
        by Python, and will destroy the physical mark in the buffer if the mark
        is unnamed, since there is no way to access it anyway afterward
        """
        pass  # implemented in Ada

    def __init__(self):
        """
        This subprogram will always raise an exception, thus preventing the
        direct creation of a mark. Instead, you should use
        :func:`GPS.EditorLocation.create_mark` to create such a mark
        """
        pass  # implemented in Ada

    def delete(self):
        """
        Delets the physical mark from the buffer. All instances referencing the
        same mark will no longer be valid. If you haven't given a name to the
        mark in the call to GPS.EditorLocation.create_mark(), it will
        automatically be destroyed when the last instance referencing it goes
        out of scope. Therefore, calling delete() is not mandatory in the case
        of unnamed marks, although it is still recommanded
        """
        pass  # implemented in Ada

    def is_present(self):
        """Returns True if mark's location is still present in the buffer"""
        pass  # implemented in Ada

    def location(self):
        """
        Returns the current location of the mark. This location will vary
        depending on the changes that take place in the buffer

        :return: An instance of :class:`GPS.EditorLocation`

        .. code-block:: python

           ed = GPS.EditorBuffer.get(GPS.File("a.adb"))
           loc = GPS.EditorLocation(ed, 3, 5)
           mark = loc.create_mark()
           # ...
           loc = mark.location()
        """
        pass  # implemented in Ada

    def move(self, location):
        """
        Moves the mark to a new location in the buffer. This is slightly less
        expensive that destroying the mark and creating a new one through
        :func:`GPS.EditorLocation.create_mark`, although the result is the same

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
    """

    def __init__(self):
        """
        This subprogram is used to prevent the direct creation of
        overlays. Overlays need to be created through
        :func:`GPS.EditorBuffer.create_overlay`

        .. seealso:: :func:`GPS.EditorBuffer.create_overlay`
        """
        pass  # implemented in Ada

    def get_property(self, name):
        """
        This subprogram is used to retrieve one of the predefined properties of
        the overlay. This list of these properties is described for
        GPS.EditorOverlay.set_property

        :param name: A string
        :return: A string or a boolean, depending on the property
        """
        pass  # implemented in Ada

    def name(self):
        """
        Return the name associated with this overlay, as given to
        GPS.EditorBuffer.create_overlay()

        :return: A string

        .. seealso:: :func:`GPS.EditorBuffer.create_overlay`
        """
        pass  # implemented in Ada

    def set_property(self, name, value):
        """
This function is used to change some of the predefined properties of the
overlay. These are mostly used to change the visual rendering of the
text,... The following attribute names are currently recognized:

- *foreground* (value is a string with the color name)

   Change the foreground color of the text.

- *background* (value is a string with the color name)

   Change the background color of the text.

- *font* (value is a string with the font name)

   Changes the font of the text

- *weight* (value is a string, one of "light", "normal" and "bold")

- *style* (value is a string, one of "normal", "oblique" and "italic")

- *editable* (value is a boolean): Indicates whether this range of text is
   editable or not

The set of predefined attributes is fixed. However, overlays are especially
useful to store your own user data in the usual python manner, which you can
retrieve later. This can be used to mark specially specific ranges of text
which you want to be able to find easily later on, even if the buffer has been
modified since then (see GPS.EditorLocation.forward_overlay)

        :param name: A string
        :param value: A string or a boolean, depending on the property
        """
        pass  # implemented in Ada


###########################################################
# EditorView
###########################################################

class EditorView(GUI):
    """
    One view of an editor, ie the visible part through which users can modify
    text files. A given GPS.EditorBuffer can be associated with multiple
    views. Closing the last view associated with a buffer will also close the
    buffer

    .. code-block:: python

       # To get a handle on the current editor, use the following code:
       view = GPS.EditorBuffer.get().current_view()
    """

    def __init__(self, buffer):
        """
        This constructor is called implicitly whenever you create a new
        view. It creates a new view for the given buffer, and is automatically
        inserted into the GPS MDI

        :param buffer: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def buffer(self):
        """
        Returns the buffer to which the view is attached. Editing the text of
        the file should be done through this instance

        :return: An instance of :class:`GPS.EditorBuffer`
        """
        pass  # implemented in Ada

    def center(self, location='location of cursor'):
        """
        Scrolls the view so that the location is centered

        :param location: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def cursor(self):
        """
        Return the current location of the cursor in this view

        :return: An instance of :class:`GPS.EditorLocation`
        """
        pass  # implemented in Ada

    def goto(self, location, extend_selection):
        """
        Moves the cursor at the given location. Each view of a particular
        buffer has its own cursor position, which is where characters typed by
        the user will be inserted. If extend_selection is True, extend the
        selection from the current bound to the new location.

        :param location: An instance of :class:`GPS.EditorLocation`
        :param extend_selection: A Boolean
        """
        pass  # implemented in Ada

    def is_read_only(self):
        """
        Whether the view is editable or not. This property is in fact shared by
        all views of the same buffer.

        :return: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.is_read_only`
        """
        pass  # implemented in Ada

    def set_read_only(self, read_only=True):
        """
        Indicates whether the user should be able to edit interactively through
        this view. Setting a view Writable/Read Only will also modify the
        status of the other views of the same buffer.xx

        :param read_only: A boolean

        .. seealso:: :func:`GPS.EditorBuffer.get_read_only`
        """
        pass  # implemented in Ada

    def title(self, short=False):
        """
        Returns the view's title, the short title is returned if short is set
        to True

        :param short: A boolean
        """
        pass  # implemented in Ada


###########################################################
# Entity
###########################################################

class Entity(object):
    """
    Represents an entity from the source, based on the location of its
    declaration

    .. seealso:: :func:`GPS.Entity.__init__()`
    """

    def __cmp__(self, file):
        """
        Compare two instances of GPS.Entity, and return -1, 0 or 1 depending on
        their relative sort order

        :param file: An instance of GPS.Entity
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Return a hash value suitable for storing self in a dictionary

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name, file=None, line=1, column=1):
        """
        Initializes a new instance of the Entity class, from any reference to
        the entity. The file parameter should only be omitted for a predefined
        entity of the language. This will only work for languages for which a
        cross-reference engine has been defined

        :param name: A string, the name of the entity
        :param file: An instance of GPS.File, in which the entity is referenced
        :param line: An integer, the line at which the entity is referenced
        :param column: An integer, the column at which the entity is referenced

        >>> GPS.Entity("foo", GPS.File("a.adb"), 10, 23).declaration().file().name()
        => will return the full path name of the file in which the entity "foo",
           referenced in a.adb at line 10, column 23, is defined.
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def attributes(self):
        """
        Return various boolean attributes of the entity: is the entity global,
        static, static for a class, protected,...

        :return: A htable
        """
        pass  # implemented in Ada

    def body(self, nth='1'):
        """
        Return the location at which the implementation of the entity is
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
        Display the list of entities that call the entity. The returned value
        is a dictionary whose keys are instances of Entity calling this entity,
        and whose value is a list of FileLocation instances where the entity is
        referenced. This command might take a while to execute, since GPS needs
        to get the cross-reference information for lots of source files.  If
        dispatching_calls is true, then calls to self that might occur through
        dispatching are also listed.

        :param dispatching_calls: A boolean
        :return: A dictionary, see below
        """
        pass  # implemented in Ada

    def called_by_browser(self):
        """Open the call graph browser to show what entities call self"""
        pass  # implemented in Ada

    def calls(self, dispatching_calls=False):
        """
        Display the list of entities called by the entity. The returned value
        is a dictionary whose keys are instances of Entity called by this
        entity, and whose value is a list of FileLocation instances where the
        entity is referenced.  If dispatching_calls is true, then calls done
        through dispatching will result in multiple entities being listed (ie
        all the possible subprograms that are called at that location)

        :param dispatching_calls: A boolean
        :return: A dictionary, see below

        .. seealso:: :func:`GPS.Entity.is_called_by()`
        """
        pass  # implemented in Ada

    def category(self):
        """
        Return the category of a given entity. Possible values include: label,
        literal, object, subprogram, package/namespace, type, unknown.

        :return: A string
        """
        pass  # implemented in Ada

    def declaration(self):
        """
        Return the location of the declaration for the entity. The file's name
        is is "<predefined>" for predefined entities

        :return: An instance of :class:`GPS.FileLocation`, where the entity is
           declared

        .. code-block:: python

           entity=GPS.Entity("integer")
           if entity.declaration().file().name() == "<predefined>":
              print "This is a predefined entity"

        """
        pass  # implemented in Ada

    def derived_types(self):
        """
        Return a list of all the entities that are derived from self. For
        object-oriented languages, this includes types that extend self. In
        Ada, this also includes subtypes of self.

        :return: List of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def discriminants(self):
        """
        Return the list of discriminants for entity. This is a list of
        entities, empty if the type has no discriminant or if this notion
        doesn't apply to that language

        :return: List of instances of :class:`GPS.Entity`

        """
        pass  # implemented in Ada

    def documentation(self, extended=False):
        """
        Return the documentation for the entity. This is the comment block
        found just before or just after the declaration of the entity (if any
        such block exists). This is also the documentation string displayed in
        the tooltips when you leave the mouse cursor over an entity for a
        while. If extended is true, then the returned documentation will
        include formatting and full entity description.

        :param extended: A boolean
        :return: A string
        """
        pass  # implemented in Ada

    def end_of_scope(self):
        """
        Return the location at which the end of the entity is found.

        :return: An instance of :class:`GPS.FileLocation`
        """
        pass  # implemented in Ada

    def fields(self):
        """
        Return the list of fields for entity. This is a list of entities. This
        applies to Ada record and tagged types, or C structs for instance.

        :return: List of instances of :class:`GPS.Entity`

        """
        pass  # implemented in Ada

    def find_all_refs(self, include_implicit=False):
        """
        Display in the location window all the references to the entity. If
        include_implicit is true, then implicit uses of the entity will also be
        referenced, for instance when the entity appears as an implicit
        parameter to a generic instantiation in Ada

        :param include_implicit: A boolean

        .. seealso:: :func:`GPS.Entity.references()`
        """
        pass  # implemented in Ada

    def full_name(self):
        """
        Return the full name of the entity that it to say the name of the
        entity prefixed with its callers and parent packages names. The casing
        of the name has been normalized to lower-cases for case-insensitive
        languages

        :return: A string, the full name of the entity
        """
        pass  # implemented in Ada

    def methods(self, include_inherited=False):
        """
        Return the list of primitive operations (aka methods) for self. This
        list is not sorted

        :param include_inherited: A boolean
        :return: A list of instances of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def name(self):
        """
        Return the name of the entity. The casing of the name has been
        normalized to lower-cases for case-insensitive languages

        :return: A string, the name of the entity
        """
        pass  # implemented in Ada

    def name_parameters(self, location):
        """
        Refactor the code at the location, to add named parameters. This only
        work if the language has support for such parameters, namely Ada for
        the time being

        :param location: An instance of :class:`GPS.FileLocation`

        .. code-block:: python

           GPS.Entity("foo", GPS.File("decl.ads")).rename_parameters(
               GPS.FileLocation(GPS.File("file.adb"), 23, 34))
        """
        pass  # implemented in Ada

    def parameters(self):
        """
        Return the list of parameters for entity. This is a list of
        entities. This applies to subprograms.

        :return: List of instances of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def pointed_type(self):
        """
        Return the type pointed to by entity. If self is not a pointer (or an
        Ada access type), None is returned. This function also applies to
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
        Return the type for which self is a primitive operation (or a method,
        in other languages than Ada)

        :return: An instance of :class:`GPS.Entity` or None

        """
        pass  # implemented in Ada

    def references(self, include_implicit=False, synchronous=True, show_kind=False, in_file='None', kind_in=''):
        """
        List all references to the entity in the project sources. If
        include_implicit is true, then implicit uses of the entity will also be
        referenced, for instance when the entity appears as an implicit
        parameter to a generic instantiation in Ada.

        If ``synchronous`` is True, then the result will be directly returned,
        otherwise a command will be returned and its result will be accessible
        with get_result(). The result, then, is either a list of locations (if
        show_kind is False), or a htable indexed by location, and whose value
        is a string indicating the kind of the reference (declaration, body,
        label, end-of-spec,...).  The parameter ``in_file`` can be used to
        limit the search to references in a particular file. This is a lot
        faster.  The parameter ``kind_in`` is a list of comma-separated list of
        reference kinds (as would be returned when show_kind is True). Only
        such references are returned, as opposed to all references.

        :param include_implicit: A boolean
        :param synchronous: A boolean
        :param show_kind: A boolean
        :param in_file: An instance of :class:`GPS.File`
        :param kind_in: A string
        :return: List of :class:`GPS.FileLocation`, htable
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
        Rename the entity every where in the application. The source files
        should have been compiled first, since this operation relies on the
        cross-reference information which have been generated by the
        compiler. If include_overriding is true, then subprograms that override
        or are overridden by self are also renamed. Likewise, if self is a
        parameter to a subprogram then parameters with the same name in
        overriding or overridden subprograms are also renamed.

        If some renaming should be performed in a read-only file, the behavior
        depends on the make_writable parameter: if true, the file is made
        writable and the renaming is performed; if false, no renaming is
        performed in that file, and a dialog is displayed asking whether you
        want to do the other renamings.

        The files will be saved automatically if auto_save is true, otherwise
        they are left edited.

        :param name: A string
        :param include_overriding: A boolean
        :param make_writable: A boolean
        :param auto_save: A boolean
        """
        pass  # implemented in Ada

    def return_type(self):
        """
        Return the return type for entity. This applies to subprograms.

        :return: An instance of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def show(self):
        """
        Display in the type browser the informations known about the entity:
        list of fields for records, list of primitive subprograms or methods,
        list of parameters, ...
        """
        pass  # implemented in Ada

    def type(self):
        """
        Return the type of the entity. For a variable, it is its type

        :return: An instance of :class:`GPS.Entity`
        """
        pass  # implemented in Ada


###########################################################
# EntityContext
###########################################################

class EntityContext(FileContext):
    """
    Represents a context that contains entity information

    .. seealso:: :func:`GPS.EntityContext.__init__()`
    """

    def __init__(self):
        """
        Dummy function, whose goal is to prevent user-creation of a
        GPS.EntityContext instance. Such instances can only be created
        internally by GPS
        """
        pass  # implemented in Ada

    def entity(self):
        """
        Return the entity stored in the context

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
    Represents a source file of your application

    .. seealso:: :func:`GPS.File.__init__()`
    """

    def __cmp__(self, file):
        """
        Compare two instances of GPS.File, and return -1, 0 or 1 depending on
        their relative sort order

        :param file: An instance of :class:`GPS.File`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Return a hash value suitable for storing self in a dictionary

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name, local=False):
        """
        Initializes a new instance of the class File. This doesn't need to be
        called explicitly, since GPS will call it automatically when you create
        such an instance. If name is a base file name (no directory is
        specified), then GPS will attempt to search for this file in the list
        of source directories of the project. If a directory is specified, or
        the base file name wasn't found in the source directories, then the
        file name is considered as relative to the current directory. If local
        is "true" the specified file name is to be considered as local to the
        current directory.

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
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def check_semantic(self):
        """
        Check the semantic for current file. This call will return only once
        the check is completed

        .. seealso::

             :func:`GPS.File.shadow_check_semantic()`

             :func:`GPS.File.check_syntax()`

             :func:`GPS.File.shadow_check_syntax()`
        """
        pass  # implemented in Ada

    def check_syntax(self):
        """
        Check the syntax for current file. This call will return only once the
        check is completed

        .. seealso::

             :func:`GPS.File.shadow_check_syntax()`

             :func:`GPS.File.shadow_check_semantic()`

             :func:`GPS.File.check_semantic()`
        """
        pass  # implemented in Ada

    def compile(self, extra_args=''):
        """
        Compile current file. This call will return only once the compilation
        is completed. Additional arguments can be added to the command line.

        :param extra_args: A string

        .. seealso:: :func:`GPS.File.make()`

        .. code-block:: python

           GPS.File("a.adb").compile()
        """
        pass  # implemented in Ada

    def directory(self):
        """
        Return the directory in which the file is found

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
        Return the list of entities that are either referenced (if local is
        false) or declared (if local is true) in self.

        :param local: A boolean
        :return: A list of :class:`GPS.Entity`
        """
        pass  # implemented in Ada

    def generate_doc(self):
        """
        Generate the documentation of the file, and display it with the default
        browser

        .. seealso:: :func:`GPS.Project.generate_doc`
        """
        pass  # implemented in Ada

    def get_property(self, name):
        """
        Return the value of the property associated with the file. This
        property might have been set in a previous GPS session if it is
        persistent. An exception is raised if no such property already exists
        for the file

        :param name: A string
        :return: A string

        .. seealso:: :func:`GPS.File.set_property`
        """
        pass  # implemented in Ada

    def imported_by(self, include_implicit=False, include_system=True):
        """
        Return the list of files that depends on file_name. This command might
        take some time to execute since GPS needs to parse the cross-reference
        information for multiple source files. If include_implicit is true,
        then implicit dependencies are also returned. If include_system is
        true, then system files from the compiler runtime are also returned.

        :param include_implicit: A boolean
        :param include_system: A boolean
        :return: A list of files

        .. seealso:: :func:`GPS.File.imports`
        """
        pass  # implemented in Ada

    def imports(self, include_implicit=False, include_system=True):
        """
        Return the the list of files that self depends on. If include_implicit
        is true, then implicit dependencies are also returned. If
        include_system is true, then system files from the compiler runtime are
        also returned.

        :param include_implicit: A boolean
        :param include_system: A boolean
        :return: A list of files

        .. seealso:: :func:`GPS.File.imported_by`
        """
        pass  # implemented in Ada

    def language(self):
        """
        Return the name of the language this file is written in. This is based
        on the file extension and the naming scheme defined in the project
        files or the XML files. The empty string is returned when the language
        is unknown

        :return: A string
        """
        pass  # implemented in Ada

    def make(self, extra_args=''):
        """
        Compile and link the file and all its dependencies. This call will
        return only once the compilation is completed. Additional arguments can
        be added to the command line.

        :param extra_args: A string

        .. seealso:: :func:`GPS.File.compile`
        """
        pass  # implemented in Ada

    def name(self, remote_server='GPS_Server'):
        """
        Return the name of the file associated with self. This is an absolute
        file name, including directories from the root of the filesystem.

        If remote_server is set, then the function returns the equivalent path
        on the specified server. GPS_Server (default) is always the local
        machine.

        :param remote_server: A string. Possible values are "GPS_Server"
            (or empty string), "Build_Server", "Debug_Server",
            "Execution_Server" and "Tools_Server".
        :return: A string, the name of the file
        """
        pass  # implemented in Ada

    def other_file(self):
        """
        Return the name of the other file semantically associated with this
        one. In Ada this is the spec or body of the same package depending on
        the type of this file. In C, this will generally be the .c or .h file with the same base name.

        :return: An instance of :class:`GPS.File`

        .. code-block:: python

           GPS.File("tokens.ads").other_file().name()
           => will print "/full/path/to/tokens.adb" in the context of the project
           => file used for the GPS tutorial.
        """
        pass  # implemented in Ada

    def project(self, default_to_root=True):
        """
        Return the project to which file belongs. If file is not one of the
        souces of the project, the returned value depends on default_to_none:
        if false, None is returned. Otherwise, the root project is returned.

        :param default_to_root: A boolean
        :return: An instance of :class:`GPS.Project`

        .. code-block:: python

           GPS.File("tokens.ads").project().name()
           => will print "/full/path/to/sdc.gpr" in the context of the project file
           => used for the GPS tutorial
        """
        pass  # implemented in Ada

    def remove_property(self, name):
        """
        Removes a property associated with a file

        :param name: A string

        .. seealso:: :func:`GPS.File.set_property`
        """
        pass  # implemented in Ada

    def search(self, pattern, case_sensitive=False, regexp=False, scope='whole'):
        """
        Return the list of matches for pattern in the file. Default values are
        False for case_sensitive and regexp. Scope is a string, and should be
        any of 'whole', 'comments', 'strings', 'code'. The latter will match
        only for text outside of comments

        :param pattern: A string
        :param case_sensitive: A boolean
        :param regexp: A boolean
        :param scope: One of ("whole", "comments", "strings", "code")
        :return: List of :class:`GPS.FileLocation` instances

        .. seealso::

           :func:`GPS.EditorLocation.search`

           :func:`GPS.File.search_next`
        """
        pass  # implemented in Ada

    def search_next(self, pattern, case_sensitive=False, regexp=False):
        """
        Return the next match for pattern in the file. Default values are False
        for case_sensitive and regexp. Scope is a string, and should be any of
        'whole', 'comments', 'strings', 'code'. The latter will match only for
        text outside of comments

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
        standard mechanism in that there is no garantee that the same instance
        of GPS.File will be created for each physical file on the disk, and
        therefore you would not be able to associate a property with the
        physical file itself

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
        Check the semantic for current file. The current file will not be
        saved, but a temporary extending project will be created, and deleted
        when the compilation ends. This call will launch a background process
        and return immediately

        .. seealso::

           :func:`GPS.File.check_semantic`

           :func:`GPS.File.check_syntax`

           :func:`GPS.File.shadow_check_syntax`
        """
        pass  # implemented in Ada

    def shadow_check_syntax(self):
        """
        Check the syntax for current file. The current file will not be saved,
        but a temporary extending project will be created, and deleted when the
        compilation ends. This call will launch a background process and return
        immediately

        .. seealso::

           :func:`GPS.File.check_syntax`

           :func:`GPS.File.check_semantic`

           :func:`GPS.File.shadow_check_semantic`

        """
        pass  # implemented in Ada

    def used_by(self):
        """
        Display in the dependency browser the list of files that depends on
        file_name. This command might take some time to execute since GPS needs
        to parse the cross-reference information for multiple source files

        .. seealso:: :func:`GPS.File.uses`
        """
        pass  # implemented in Ada

    def uses(self):
        """
        Display in the dependency browser the list of files that file_name
        depends on.

        .. seealso:: :func:`GPS.File.used_by`
        """
        pass  # implemented in Ada


###########################################################
# FileLocation
###########################################################

class FileLocation(object):
    """
    Represents a location in a file

    .. seealso:: :func:`GPS.FileLocation.__init__`
    """

    def __cmp__(self, file):
        """
        Compare two instances of GPS.FileLocation, and return -1, 0 or 1
        depending on their relative sort order

        :param file: An instance of :class:`GPS.FileLocation`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Return a hash value suitable for storing self in a dictionary

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, filename, line, column):
        """
        Initializes a new instance of GPS.FileLocation.

        :param filename: An instance of :class:`GPS.File`
        :param line: An integer
        :param column: An integer

        .. code-block:: python

           location = GPS.FileLocation(GPS.File("a.adb"), 1, 2)
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def column(self):
        """
        Return the column of the location

        :return: An integer, the column of the location

        .. seealso::

           :func:`GPS.FileLocation.file()`

           :func:`GPS.FileLocation.line()`
        """
        pass  # implemented in Ada

    def file(self):
        """
        Return the file of the location

        :return: An instance of GPS.File, the file of the location

        .. seealso::

           :func:`GPS.FileLocation.line()`

           :func:`GPS.FileLocation.column()`
        """
        pass  # implemented in Ada

    def line(self):
        """
        Return the line of the location

        :return: An integer, the line of the location

        .. seealso::

           :func:`GPS.FileLocation.file()`

           :func:`GPS.FileLocation.column()`
        """
        pass  # implemented in Ada


###########################################################
# HTML
###########################################################

class HTML(object):
    """
    This class gives access to the help system of GPS, as well as to the
    integrated browser
    """

    @staticmethod
    def add_doc_directory(directory):
        """
        Add a new directory to the GPS_DOC_PATH environment variable. This
        directory is searched for documentation files. If this directory
        contains a gps_index.xml file, it is parsed to find the list of
        documentation files to add to the Help menu. See the GPS documentation
        for more information on the format of the gps_index.xml files

        :param directory: Directory that contains the documentation
        """
        pass  # implemented in Ada

    @staticmethod
    def browse(URL, anchor='', navigation=True):
        """
        Open the GPS html viewer, and load the given URL. If anchor matches a
        <a> tag in this file, GPS will jump to it. If URL isn't an absolute
        file name, it is searched in the path set by the environment variable
        GPS_DOC_PATH.

        If navigation is True, then the URL is saved in the navigation list, so
        that users can move back and forward from and to this location later
        on.

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
    shell_commands.xml, part of the GPS installation, and is what you are
    currently seeing.

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

    .. seealso:: :func:`GPS.Help.__init__()`
    """

    def __init__(self):
        """
        Initializes the instance of the Help class. This parses the XML file
        that contains the description of all the commands. With python, the
        memory occupied by this XML tree will be automatically freed. However,
        with the GPS shell you need to explicitly call GPS.Help.reset()

        .. seealso:: :func:`GPS.Help.reset`
        """
        pass  # implemented in Ada

    def file(self):
        """
        Return the name of the file that contains the description of the shell
        commands. You shouldn't have to access it yourself, since you can do so
        through GPS.Help().getdoc() instead

        :return: A string

        .. seealso:: :func:`GPS.Help.getdoc`
        """
        pass  # implemented in Ada

    def getdoc(self, name, html=False):
        """
        Search, into the XML file shell_commands.xml, the documentation for
        this specific command or entity. If no documentation is found, an error
        is raised. If html is true, the documentation is formated in HTML

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
        Free the memory occupied by this instance. This frees the XML tree that
        is kept in memory. As a result, you can no longer call
        GPS.Help.getdoc() afterward.
        """
        pass  # implemented in Ada


###########################################################
# Hook
###########################################################

class Hook(object):
    """
General interface to hooks. Hooks are commands executed when some specific
events occur in GPS, and allow you to customize some of the aspects of GPS

.. seealso:: :func:`GPS.Hook.__init__`

The available hooks are:

- activity_checked_hook(hookname)

  Hook called when an activity has been checked, this is the last step done
  after the activity has been committed. It is at this point that the activity
  closed status is updated.

- after_character_added(hookname, file, character)

  Hook called when a character has been added in the editor. This hook is also
  called for the backspace key.

  :param file: An instance of GPS.File
  :param character: A character

  .. seealso::

     Hook: character_added

     Hook: word_added

- annotation_parsed_hook(hookname)

    Hook called when the last file annotation has been parsed after the
    corresponding VCS action.

- before_exit_action_hook(hookname)

  This hook is called when GPS is about to exit. If it returns 0, this exit
  will be prevented (it is recommended that you display a dialog to explain
  why, in such a case)

  :return: A boolean

- before_file_saved(hookname, file)

  Hook called right before a file is saved

  :param file: An instance of GPS.File

- bookmark_added(hookname, bookmark_name)

  Hook called when a new bookmark has been created by the user

   :param bookmark_name: A string, the name of the bookmark that has been added

- bookmark_removed(hookname, bookmark_name)

  Hook called when a new bookmark has been removed by the user

  :param bookmark_name: A string, the name of the bookmark that has been removed

- buffer_edited(hookname, file)

  Hook called after the user has stopped modifying the contents of an editor

  :param file: An instance of GPS.File

- build_server_connected_hook(hookname)

  Hook called when GPS connects to the build server in remote mode

- character_added(hookname, file, character)

  Hook called when a character is going to be added in the editor. It is also
  called when a character is going to be removed, in which case the last
  parameter is 8 (control-h)

   :param file: An instance of GPS.File
   :param character: A character

   .. seealso::

      Hook after_character_added

      Hook word_added

- clipboard_changed(hookname)

  Hook called when the contents of the clipboard has changed, either because
  the user has done a Copy or Cut operation, or because he called Paste
  Previous which changes the current entry in the multi-level clipboard.

- commit_done_hook(hookname)

  Hook called when a commit has been done.

- compilation_finished(hookname, category, target_name, mode_name, status)

  Hook called when a compile operation has finished.

  Among the various tasks that GPS connects to this hook are the automatic
  reparsing of all xref information, and the activation of the automatic-error
  fixes

  :param category: A string, the location/highlighting category that contains the compilation output.
  :param target_name: A string, name of the executed build target.
  :param mode_name: A string, name of the executed build mode.
  :param status: An integer, exit status of the execuded program.

- compilation_starting(hookname, category, quiet, shadow)

  Hook called when a compile operation is about to start.

  Among the various tasks that GPS connects to this hook are: check whether
  unsaved editors should be saved (asking the user), and stop the background
  task that parses all xref info. If quiet is True, then no visible
  modification should be done in the MDI, like raising consoles, clearing their
  content,..., since the compilation should happen in background mode.

  Funtions connected to this hook should return False if the compilation should
  not occur for some reason, True if it is OK to start the
  compilation. Typically, the reason to reject a compilation would be because
  the user has explicitly cancelled it through a graphical dialog, or because
  running a background compilation is not suitable at this time.

  :param category: A string, the location/highlighting category that contains
     the compilation output.

  :param quiet: A boolean, if True then the GUI should advertise the
     compilation, otherwise nothing should be reported to the user, unless there
     is an error.

  :param shadow: A boolean, indicates whether the build launched was a Shadow
     builds, ie a "secondary" build launched automatically by GPS after a "real"
     build.
     For instance, when the multiple toolchains mode is activated, the builds
     generating cross-references are Shadow builds.

  :return: A boolean

  .. code-block:: python

     # The following code adds a confirmation dialog to all
     # compilation commands.
     def on_compilation_started(hook, category, quiet, shadow):
        if not quiet:
           return MDI.yes_no_dialog("Confirm compilation ?")
        else:
           return True

     Hook("compilation_starting").add(on_compilation_started)

  .. code-block:: python

     # If you create a script to execute your own build script, you
     # should always do the following as part of your script. This
     # ensures a better integration in GPS (saving unsaved editors,
     # reloading xref information automatically in the end, raising
     # the GPS console, parsing error messages for automatically
     # fixable errors,...)

     if notHook ("compilation_starting").run_until_failure(
          "Builder results", False, False):
        return

     # ... spawn your command

     Hook("compilation_finished").run("Builder results")

- compute_build_targets(hookname, name)

  Hook called whenever GPS needs to compute a list of subtargets for a given
  build target. The handler should check whether name is a known build target,
  and if so, return a list of tuples, where each tuple corresponds to one
  target and contains a display name (used in the menus, for instance) and the
  name of the target. If name is not known, it should return an empty list.

  :param name: A string, the target type
  :return: A string

  .. code-block:: python

     def compute_targets(hook, name):
        if name == "my_target":
          return [(display_name_1, target_1),
                  (display_name_2, target_2)]
        return ""
     GPS.Hook("compute_build_targets").add(compute_targets)

- context_changed(hookname, context)

  Hook called when the current context changes in GPS, ie a new file is
  selected, or a new entity, or a new window,...

  :param context: An instance of GPS.Context

- contextual_menu_close(hookname)

  Hook called just before a contextual menu is destroyed. At this time, the
  value returned by GPS.contextual_context() is still the one used in the hook
  contextual_menu_open, and therefore you can still reference the data you
  stored in the context. This hook is called even if no action was selected by
  the user. However, it is always called before the action is executed, since
  the menu itself is closed first.

   .. seealso:: :func:`contextual_menu_open hook`

- contextual_menu_open(hookname)

  Hook called just before a contextual menu is created. It is called before any
  of the filters is evaluated, and can be used to precomputed data shared by
  multiple filters to speed up the computation. Use GPS.contextual_context() to
  get the context of the contextual menu and store precomputed data in it.

  .. seealso:: :func:`contextual_menu_close hook`

- debugger_breakpoints_changed(hookname, debugger)

  Hook called when the list of breakpoints has been refreshed. This might occur
  whether or not the list has changed, but is a good time to refresh any view
  that might depend on an up-to-date list

  :param debugger: An instance of :class:`GPS.Debugger`

- debugger_command_action_hook(hookname, debugger, command)

  This hook is emitted when the user types a command in the debugger console,
  or emits the console through the GPS.Debugger API. It gives you a chance to
  override the behavior for the command, or even define your own commands. Note
  that you must ensure that any debugger command you execute this way does
  finish with a prompt. The function should return the output of your custom
  command

  :param debugger: An instance of :class:`GPS.Debugger`
  :param command: A string, the command the user wants to execute
  :return: A boolean

  .. code-block:: python

      ## The following example implements a new gdb command, "hello". When the
      ## user types this command in the console, we end up executing "print A"
      ## instead. This can be used for instance to implement convenient
      ## macros

      def debugger_commands(hook, debugger, command):
         if command == "hello":
            return 'A=' + debugger.send("print A", False)
         else:
            return ""

      GPS.Hook("debugger_command_action_hook").add(debugger_commands)

- debugger_context_changed(hookname, debugger)

  Called when the debugger context has changed, for instance after the user has
  switched the current thread, has selected a new frame,...

  :param debugger: An instance of :class:`GPS.Debugger`

- debugger_executable_changed(hookname, debugger)

  Called when the file being debugged has changed

  :param debugger: An instance of :class:`GPS.Debugger`

- debugger_process_stopped(hookname, debugger)

  Called when the debugger ran and has stopped, for instance when hitting a
  breakpoint, or after a next command. If you need to know when the debugger
  just started processing a command, you can connect to the
  debugger_state_changed hook instead. Conceptually, you could connect to
  debugger_state_changed at all times instead of debugger_process_stopped and
  check when the state is now "idle"

  :param debugger: An instance of :class:`GPS.Debugger`

  .. seealso:: Hook debugger_state_changed

- debugger_process_terminated(hookname, debugger)

  Called when the program being debugged has terminated

  :param debugger: An instance of :class:`GPS.Debugger`

- debugger_question_action_hook(hookname, debugger, question)

  Action hook called just before displaying an interactive dialog, when the
  debugger is asking a question to the user. This hook can be used to disable
  the dialog (and send the rreply directly to the debugger instead). It should
  return a non-empty string to pass to the debugger if the dialog should not be
  displayed. You cannot send commands to the debugger when inside this hook,
  since the debugger is blocked waiting for an answer

  :param debugger: An instance of GPS.Debugger
  :param question: A string
  :return: A string

  .. code-block:: python

     def gps_question(hook, debugger, str):
        return "1"   ## Always choose choice 1

     GPS.Hook("debugger_question_action_hook").add(gps_question)

     debug=GPS.Debugger.get()
     deubg.send("print &foo")

- debugger_started(hookname, debugger)

   Hook called when a new debugger has been started

   :param debugger: An instance of GPS.Debugger

   .. seealso:: Hook debugger_state_changed

- debugger_state_changed(hookname, debugger, new_state)

  Indicates a change in the status of the debugger: new_state can be one of
  "none" (the debugger is now terminated), "idle" (the debugger is now waiting
  for user input) or "busy" (the debugger is now processing a command, and the
  process is running). As opposed to debugger_process_stopped, this hook is
  called when the command is just starting its executing (hence the debugger is
  busy while this hook is called, unless the process immediately stopped).

  This hook is in fact emitted also when internal commands are sent to the
  debugger, and thus much more often than if it was just reacting to user
  input. It is therefore recommended that the callback does the minimal amount
  of work, possibly doing the rest of the work in an idle callback to be
  executed when GPS is no longer busy.

  If the new state is "busy", you cannot send additional commands to the
  debugger.

  When the state is either "busy" or "idle", GPS.Debugger.command will return
  the command that is about to be executed or the command that was just
  executed and just completed.

  :param debugger: An instance of :class:`GPS.Debugger`
  :param new_state: A string

- debugger_terminated(hookname, debugger)


  Hook called when the debugger session has been terminated. It is now
  recommended that you connect to the debugger_state_changed hook and test
  whether the new state is "none".

  :param debugger: An instance of :class:`GPS.Debugger`

  .. seealso:: Hook debugger_state_changed

- diff_action_hook(hookname, vcs_file, orig_file, ref_file, diff_file, title)

  Hook called to request the display of the comparison window

  :param vcs_file: An instance of :class:`GPS.File`
  :param orig_file: An instance of :class:`GPS.File`
  :param ref_file: An instance of :class:`GPS.File`
  :param diff_file: An instance of :class:`GPS.File`
  :param title: Buffer title
  :return: A boolean

- file_changed_detected(hookname, file)

  Hook called whenever GPS detects that an opened file changed on the disk. You
  can connect to this hook if you want to change the default behavior, which is
  asking if the user wants to reload the file. Your function should return 1 if
  the action is handled by the function, and return 0 if the default behavior
  is desired.

  :param file: An instance of :class:`GPS.File`
  :return: A boolean

  .. code-block:: python

        import GPS

        def on_file_changed(hook, file):
            # automatically reload the file without prompting the user
            ed = GPS.EditorBuffer.get(file, force = 1)
            return 1

        # install a handler on "file_changed_detected" hook
        GPS.Hook("file_changed_detected").add(on_file_changed)


- file_changed_on_disk(hookname, file)

  Hook called when some external action has changed the contents of a file on
  the disk, such as a VCS operation. The parameter might be a directory instead
  of a file, indicating that any file in that directory might have changed

  :param file: An instance of :class:`GPS.File`


- file_closed(hookname, file)

  Hook called just before the last editor for a file is closed. You can still
  use EditorBuffer.get() and current_view() to access the last editor for file.

  :param file: An instance of :class:`GPS.File`


- file_deleted(hookname, file)

  Hook called whenever GPS detects that a file was deleted on the disk. The
  parameter might be a directory instead of a file, indicating that any file
  within that directory has been deleted.

  :param file: An instance of :class:`GPS.File`


- file_edited(hookname, file)

  Hook called when a file editor has been opened for a file that wasn't already
  opened before. Do not confuse with the hook open_file_action, which is used
  to request the opening of a file.

  :param file: An instance of :class:`GPS.File`

  .. seealso:: :func:`open_file_action hook`

- file_line_action_hook(hookname, identifier, file, every_line, normalize)

  Hook called to request the display of new information on the side of the
  editors. It isn't expected that you connect to this hook, but you might want
  to run it yourself to ask GPS to display some information on the side of its
  editors

  :param identifier: A string
  :param file: An instance of :class:`GPS.File`
  :param every_line: A boolean
  :param normalize: A boolean
  :return: A boolean

- file_renamed(hookname, file, renamed)

  Hook called whenever a GPS action renamed a file on the disk. The file
  parameter indicates the initial location of the file, while the renamed
  parameter indicates the new location. The parameters might be directories
  instead of files, indicating that the directory has been renamed, and thus
  any file within that directory have their path changed.

  :param file: An instance of :class:`GPS.File`
  :param renamed: An instance of :class:`GPS.File`

- file_saved(hookname, file)

  Hook called whenever a file has been saved

  :param file: An instance of :class:`GPS.File`

- file_status_changed_action_hook(hookname, file, status)

  Hook called when a file status has changed

  :param file: An instance of GPS.File
  :param status: A string, the new status for the file. This is the status has
     displyed into the GPS status line. The value is either Unmodified, Modified
     or Saved.
  :return: A boolean

- gps_started(hookname)

  Hook called when GPS is fully loaded, and its window is visible to the user.

  It isn't recommended to do any direct graphical action before this hook has
  been called, so it is recommended that in most cases your start scripts
  connect to this hook.

- html_action_hook(hookname, url_or_file, enable_navigation, anchor)

  Hook called to request the display of HTML files. It is generally useful if
  you want to open an HTML file, and let GPS handle it in the usual manner

  :param url_or_file: A string
  :param enable_navigation: A boolean
  :param anchor: A string
  :return: A boolean

- location_action_hook(hookname, identifier, category, file, line, column, message)

  Hook called to request the display of new information on the side of the
  location window

  :param identifier: A string
  :param category: A string
  :param file: An instance of :class:`GPS.File`
  :param line: An integer
  :param column: An integer
  :param message: A string
  :return: A boolean

- location_changed(hookname, file, line, column)

  Hook called when the location in the current editor has changed, and the
  cursor has stopped moving.

  :param file: An instance of :class:`GPS.File`
  :param line: An integer
  :param column: An integer

- log_parsed_hook(hookname)

  Hook called when the last file log has been parsed after the corresponding
  VCS action.

- marker_added_to_history(hookname)

  Hook called when a new marker is added to the history list of previous
  locations, where the user can navigate back and forward

- open_file_action_hook(hookname, file, line, column, column_end,
                        enable_navigation, new_file, force_reload,
                        focus=False)

  This hook is called when GPS needs to open a file. You can connect to this
  hook if you want to have your own editor open, instead of the internal editor
  of GPS. Your function should return 1 if it did open the file, 0 if the next
  function connected to this hook should be called.

  The file should be opened directly at line and column. If column_end is not
  0, the given range should be highlighted if possible. The enable_navigation
  parameter is set to True if the new location should be added to the history
  list, so that the user can navigate forward and backward across previous
  locations. new_file is set to True if a new file should be created when file
  is not found. If set to False, nothing should be done. force_reload is set to
  true if the file should be reloaded from the disk, discarding any change the
  user might have done. focus is set to true if the open editor should be given
  the keyboard focus

  :param file: An instance of :class:`GPS.File`
  :param line: An integer
  :param column: An integer
  :param column_end: An integer
  :param enable_navigation: A boolean
  :param new_file: A boolean
  :param force_reload: A boolean
  :param focus: A boolean
  :return: A boolean

  .. seealso:: :func:`file_edited hook`

  .. code-block:: python

      GPS.Hook('open_file_action_hook').run(
                GPS.File("gps-kernel.ads"),
                322, # line
                5,   # column
                9,   # column_end
                1,   # enable_navigation
                1,   # new_file
                0)   # force_reload

- preferences_changed(hookname)

  Hook called when the value of some of the preferences changes. Modules should
  refresh themselves dynamically

- project_changed(hookname)

  Hook called when the project has changed. A new project has been loaded, and
  all previous settings and caches are now obsolete. In the callbacks for this
  hook, the attribute values have not been computed from the project yet, and
  will only return the default values. Connect to the project_view_changed hook
  instead to query the actual values

  .. seealso:: Hook project_view_changed

- project_changing(hookname, file)

  Hook called just before a new project is loaded.

  :param file: An instance of :class:`GPS.File`

- project_editor(hookname)

  Hook called before the Project Editor is opened. This allows a custom module
  to perform specific actions before the actual creation of this dialog.

- project_saved(hookname, project)

  Hook called when a project is saved to disk. It is called for each project in
  the hierarchy

  :param project: An instance of GPS.Project

- project_view_changed(hookname)

  Hook called when the project view has been changed, for instance because one
  of the environment variables has changed. This means that the list of
  directories, files or switches might now be different. In the callbacks for
  this hook, you can safely query the new attribute values.

- revision_parsed_hook(hookname)

  Hook called when the last file revision has been parsed after the
  corresponding VCS action.

- rsync_action_hook(hookname)

  For internal use only

- search_functions_changed(hookname)

  Hook called when the list of registered search functions changes.

- search_regexps_changed(hookname)

  Hook called when a new regexp has been added to the list of predefined search
  patterns

- search_reset(hookname)

  Hook called when the current search pattern is reset or changed by the user,
  or when the current search is no longer possible because the setup of GPS has
  changed.

- server_config_hook(hookname, server_type, nickname)

  Hook called when a server is assigned to a server operations category.

  :param server_type: A string, the server operations category. Can take the
     values "BUILD_SERVER", "EXECUTION_SERVER" or "DEBUG_SERVER"
  :param nickname: A string, the server's nickname

- server_list_hook(hookname)

  Hook called when the list of configured servers changed.

- source_lines_revealed(hookname, context)

  Hook called when a range of line becomes visible on the screen

  :param context: An instance of :class:`GPS.Context`

- status_parsed_hook(hookname)

  Hook called when the last file status has been parsed after the corresponding
  VCS action.

- stop_macro_action_hook(hookname)

  You should run this hook to request that the macro currently being replayed
  be stopped. No more events should be processed as part of this macro

- variable_changed(hookname)

  Hook called when one of the scenario variables has been renamed, removed or
  when one of its possible values has changed.

- word_added(hookname, file)

  Hook called when a word has been added in the editor

  :param file: An instance of :class:`GPS.File`

  .. seealso:: Hook character_added
    """

    def __init__(self, name):
        """
        Create a new hook instance, which refers to one of the already defined
        hooks

        :param name: A string, the name of the hook
        """
        pass  # implemented in Ada

    def add(self, function_name, last=True):
        """
        Connect a new function to a specific hook. Any time this hook is run
        through run_hook, this function will be called with the same parameters
        passed to run_hook. If Last is True, then this function will be called
        after all functions currently added to this hook. If Last is False, it
        will be called before.

        :param function_name: A subprogram, see the "Subprogram Parameters"
            section in the GPS documentation
        :param last: A boolean

        .. seealso:: :func:`GPS.Hook.remove`

        An example using the GPS shell::

           # in the GPS shell:

           parse_xml '<action name="edited"><shell>echo "File edited hook=$1 file=$2"</shell></action>'
           Hook "file_edited"
           Hook.add %1 "edited"

        .. code-block:: python

           def filed_edited(hook_name, file):
               print "File edited (hook=" + hook_name + " file=" + file.name()
           GPS.Hook("file_edited").add(file_edited)

        """
        pass  # implemented in Ada

    def describe_functions(self):
        """
        List all the functions that are executed when the hook is executed. The
        returned list might contain <<internal> strings, which indicate that
        some Ada function is connected to this hook

        :return: A list of strings

        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        List all defined hooks. See also run_hook, register_hook and add_hook

        :return: A list of strings

        .. seealso:: :func:`GPS.Hook.list_types`
        """
        pass  # implemented in Ada

    @staticmethod
    def list_types():
        """
        List all defined type hooks

        :return: A list of strings

        .. seealso:: :func:`GPS.Hook.register`
        """
        pass  # implemented in Ada

    @staticmethod
    def register(name, type=''):
        """
        Defines a new hook. This hook can take any number of parameters, the
        default is none. The type and number of parameters is called the type
        of the hook, and this is described by the" optional second
        parameter. The value of this parameter should be either the empty
        string for a hook that doesn't take any parameter. Or it could be one
        of the predefined types exported by GPS itself (see
        list_hook_types). Finally, it could be the word ""generic"" if this is
        a new type of hook purely defined for this scripting language

        :param name: A string, the name of the hook to create
        :param type: A string, the type of the hook. See GPS.Hook.list_types()
        """
        pass  # implemented in Ada

    def remove(self, function_name):
        """
        Remove function_name from the list of functions executed when the hook
        is run. This is the reverse of GPS.Hook.add

        :param function_name: A subprogram, see the "Subprogram Parameters"
            section in the GPS documentation

        .. seealso:: :func:`GPS.Hook.add`
        """
        pass  # implemented in Ada

    def run(self, *args):
        """
        Run the hook. This will call all the functions that attached to that
        hook, and return the return value of the last callback (this depends on
        the type of the hook, most often this is always None). When the
        callbacks for this hook are expected to return a boolean, this command
        stops as soon as one the callbacks returns True

        :param args: Any number of parameters to pass to the hook.

        .. seealso::

           :func:`GPS.Hook.run_until_success`

           :func:`GPS.Hook.run_until_failure`
        """
        pass  # implemented in Ada

    def run_until_failure(self, *args):
        """
        This only applies to hooks returning a boolean. This executes all
        functions attached to this hook, until one returns False, in which case
        no further function is called. This returns the returned value of the
        last executed function.

        :param args: Any number of parameters to pass to the hook.
        :return: A boolean

        .. seealso::

           :func:`GPS.Hook.run_until_success`

           :func:`GPS.Hook.run`
        """
        pass  # implemented in Ada

    def run_until_success(self, *args):
        """
        This only applies to hooks returning a boolean. This executes all
        functions attached to this hook, until one returns True, in which case
        no further function is called. This returns the returned value of the
        last executed function. This is mostly the same as GPS.Hook.run, but
        makes the halt condition more explicit.

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
    expected, for instance)
    """
    pass  # implemented in Ada


###########################################################
# Locations
###########################################################

class Locations(object):
    """
    General interface to the locations window
    """

    @staticmethod
    def add(category, file, line, column, message,
            highlight='', length='0', look_for_secondary=False):
        """
        Add a new entry in the location window. Nodes are created as needed for
        the category or file. If Highlight is specified to a non-empty string,
        the whole line is highlighted in the file, with a color given by that
        highlight category (see register_highlighting for more
        information). Length is the length of the highlighting. The default
        value of 0 indicates that the whole line should be highlighted

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
        Dump the contents of the Locations View to the specified file, in XML format.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def list_categories():
        """
        Return the list of all categories currently displayed in the Locations
        window. These are the top-level nodes used to group information
        generally related to one command, like the result of a compilation.

        :return: A list of strings

        .. seealso:: :func:`GPS.Locations.remove_category`
        """
        pass  # implemented in Ada

    @staticmethod
    def list_locations(category, file):
        """
        Return the list of all file locations currently listed in the given
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
        Parse the contents of the string, which is supposedly the output of
        some tool, and add the errors and warnings to the locations window. A
        new category is created in the locations window if it doesn't
        exist. Preexisting contents for that category is not removed, see
        locations_remove_category.

        The regular expression specifies how locations are recognized. By
        default, it matches file:line:column. The various indexes indicate the
        index of the opening parenthesis that contains the relevant information
        in the regular expression. Set it to 0 if that information is not
        available. ``style_index`` and ``warning_index``, if they match, force
        the error message in a specific category.

        ``highlight_category``, ``style_category`` and ``warning_category``
        reference the colors to use in the editor to highlight the messages
        when the regexp has matched. If they are set to the empty string, no
        highlighting is done in the editor. The default values match those by
        GPS itself to highlight the error messages. Create these categories
        with GPS.Editor.register_highlighting().

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
        Remove a category from the location window. This removes all associated
        files

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
    This class provides an interface to the GPS logging mechanism. This can be
    used when debugging scripts, or even be left in production scripts for
    post-mortem analysis for instance. All output through this class is done in
    the GPS log file, in $HOME/.gps/log.

    GPS comes with some predefined logging streams, which can be used to
    configure the format of the log file, such as whether colors should be
    used, whether timestamps should be logged with each message,...
    """

    def __init__(self, name):
        """
        Create a new logging stream. Each stream is associated with a name,
        which is displayed before each line in the GPS log file, and is used to
        distinguish between various parts of GPS. Calling this constructor with
        the same name multiple times will create a new class instance.

        :param name: A string

        .. code-block:: python

           log = GPS.Logger("my_script")
           log.log("A message")
        """
        pass  # implemented in Ada

    def check(self, condition, error_message, success_message=''):
        """
        If condition evaluates to False, then error_message will be logged in
        the log file. If the condition evaluates to True, then success_message
        is logged if it was specified

        :param condition: A boolean
        :param error_message: A string
        :param success_message: A string

        .. code-block:: python

           log=GPS.Logger("my_script")
           log.check(1 == 2, "Invalid addition")

        """

    count = None

    def log(self, message):
        """
        Logs a message in the GPS log file

        :param message: A string
        """
        pass  # implemented in Ada

    def set_active(self, active):
        """
        Activate or deactivate a logging stream. The default for a sttream
        depends on the file $HOME/.gps/traces.cfg, and will generally be
        active. When a stream is inactive, no message is sent to the log file

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

    If you have installed the pygtk package (see GPS's documentation}, GPS will
    export a few more functions to python so that it is easier to interact with
    GPS itself. In particular, the GPS.MDI.add function allows you to put a
    widget created by pygtk under control of GPS's MDI, so that users can
    interact with it as with all other GPS windows.

    .. code-block:: python

       import GPS

       ## The following three lines are the usual to make pygtk visible
       import pygtk
       pygtk.require('2.0')
       import gtk

       def on_clicked(*args):
          GPS.Console().write("button was pressed\\n")

       def create():
          button=gtk.Button('press')
          button.connect('clicked', on_clicked)
          GPS.MDI.add(button, "From testgtk", "testgtk")
          win = GPS.MDI.get('testgtk')
          win.split()

       create()
    """

    @staticmethod
    def add(widget, title, short):
        """
        This function is only available if pygtk could be loaded in the python
        shell. You must install this library first, see the documentation for
        GPS.MDI itself.

        This function adds a widget inside the MDI of GPS. The resulting window
        can then be manipulated by the user like any other standard GPS
        window. It can be split, floated, resized,... Title is the string used
        in the title bar of the window, short is the string used in the
        notebook tabs. You can immediately retrieve a handle to the created
        window by calling GPS.MDI.get (short).

        :param widget: A widget, created by pygtk
        :param title: A string
        :param short: A string

        .. seealso::

           :func:`GPS.MDI.get`

           :func:`GPS.GUI.pywidget`

           :func:`GPS.MDI`
        """
        pass  # implemented in Ada

    @staticmethod
    def children():
        """
        Return all the windows currently in the MDI

        :return: A list of :class:`GPS.MDIWindow`
        """
        pass  # implemented in Ada

    @staticmethod
    def current():
        """
        Return the window that currently has the focus, or raise an error if
        there is none

        :return: An instance of :class:`GPS.MDIWindow`

        """
        pass  # implemented in Ada

    @staticmethod
    def dialog(msg):
        """
        Display a modal dialog to report information to a user. This blocks the
        interpreter until the dialog is closed

        :param msg: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def exit(force=False):
        """
        Exit GPS. If there are unsaved changes, a dialog is first displayed to
        ask whether these should be saved. If the user cancels the operation
        through the dialog, GPS will not exit. If force is true, then no dialog
        is open, and nothing is saved

        :param force: A boolean
        """
        pass  # implemented in Ada

    @staticmethod
    def file_selector(file_filter='empty'):
        """
        Display a modal file selector. The user selected file is returned, or a
        file with an empty name if 'Cancel' is pressed.

        A file filter can be defined (such as "\*.ads") to show only a category
        of files.

        :param file_filter: A string
        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    @staticmethod
    def get(name):
        """
        Return the window whose name is name. If there is no such window, None
        is returned

        :param name: A string
        :return: An instance of :class:`GPS.MDIWindow`
        """
        pass  # implemented in Ada

    @staticmethod
    def get_by_child(child):
        """
        Return the window that contains child, or raise an error if there is
        none

        :param child: An instance of GPS.GUI
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
        Display a modal dialog and request some input from the user. The
        message is displayed at the top, and one input field is displayed for
        each remaining argument. The arguments can take the form
        ""label=value"", in which case ""value"" is used as default for this
        entry. If argument is prepend with 'multiline:' prefix field is edited
        as multi-line text. The return value is the value that the user has
        input for each of these parameters.

        An empty list is returned if the user presses Cancel

        :param msg: A string
        :param args: Any number of strings
        :return: A list of strings

        .. code-block:: python

           a, b = GPS.MDI.input_dialog("Please enter values", "a", "b")
           print a, b

        """
        pass  # implemented in Ada

    @staticmethod
    def save_all(force=False):
        """
        Save all currently unsaved windows. This includes open editors, the
        project, and any other window that has registered some save callbacks.

        If the force parameter is false, then a confirmation dialog is
        displayed so that the user can select which windows to save

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
        Display a modal dialog to ask a question to the user. This blocks the
        interpreter until the dialog is closed. The dialog has two buttons Yes
        and No, and the selected button is returned to the caller

        :param msg: A string
        :return: A boolean

        .. code-block:: python

           if GPS.MDI.yes_no_dialog("Do you want to print?"):
               print "You pressed yes"

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
    another window.  Windows acts as containers for other widgets
    """

    def __init__(self):
        """
        Prevents the creation of instances of GPS.MDIWindow. This is done by
        calling the various subprograms in the GPS.MDI class
        """
        pass  # implemented in Ada

    def float(self, float=True):
        """
        Float the window, ie create a new toplevel window to display it. It is
        then under control of the user's operating system or window manager. If
        float is False, the window is reintegrated within the GPS MDI instead

        :param float: A boolean
        """
        pass  # implemented in Ada

    def get_child(self):
        """
        Return the child contained in the window. The returned value might be
        an instance of a subclass of GPS.GUI, if that window was created from a
        shell command

        :return: An instance of :class:`GPS.GUI`

        .. code-block:: python

            # Accessing the GPS.Console instance used for python can be done with:
            GPS.MDI.get("Python").get_child()
        """
        pass  # implemented in Ada

    def is_floating(self):
        """
        Return whether the window is currently floating (ie in its own toplevel
        window), or False if the window is integrated into the main GPS window

        :return: A boolean
        """
        pass  # implemented in Ada

    def name(self, short=False):
        """
        Return the name of the window. If short is False, the long name is
        returned, ie the one that appears in the title bar. If short is True,
        the short name is returned, ie the one that appears in the notebook
        tabs.

        :param short: A boolean
        :return: A string
        """
        pass  # implemented in Ada

    def next(self, visible_only=True):
        """
        Return the next window in the MDI, or window itself if there is no
        other window. If visible_only is true, then only the windows currently
        visible to the user are visible. This always returns floating windows

        :param visible_only: A boolean
        :return: An instance of GPS.MDIWindow
        """
        pass  # implemented in Ada

    def raise_window(self):
        """
        Raise the window so that it becomes visible to the user. The window
        also gains the focus
        """
        pass  # implemented in Ada

    def rename(self, name, short=''):
        """
        Change the title used for a window. Name is the long title, as it
        appears in the title bar for instance, and short, if specified, is the
        name that appears in the notebook tabs.

        Using this function might be dangereous in some contexts, since GPS
        keeps track of editors through their name.

        :param name: A string
        :param short: A string
        """
        pass  # implemented in Ada

    def split(self, vertically=True, reuse=False):
        """
        Split the window in two parts, either horizontally (side by side), or
        vertically (one below the other). If reuse is true, attempt to reuse an
        existing space rather than splitting the current window. This should be
        used to avoid ending up with too small windows

        :param vertically: A boolean
        :param reuse: A boolean

        .. seealso:: :func:`GPS.MDIWindow.single`
        """
        pass  # implemented in Ada


###########################################################
# Menu
###########################################################

class Menu(GUI):
    """
    This class is a general interface to the menu system in GPS. It gives you
    control over which menus should be active, what should be executed when the
    menu is selected by the user,...

    .. seealso:: :func:`GPS.Menu.__init__`

    """

    def __init__(self):
        """
        Prevents the creation of a menu instance. Such instances can only be
        created internally by GPS as a result of calling GPS.Menu.get or
        GPS.Menu.create. This is so that you always get the same instance of
        GPS.Menu when you are refering to a given menu in GPS, and so that you
        can store your own specific data with the menu
        """
        pass  # implemented in Ada

    @staticmethod
    def create(path, on_activate='', ref='', add_before=True,
               filter=None, group=''):
        """
        Create a new menu in the GPS system. The menu is added at the given
        location (see GPS.Menu.get for more information on the path
        parameter). Submenus are created as necessary so that path is valid.

        If ``on_activate`` is specified, it will be executed every time the user
        selects that menu. It is called with only one parameter, the instance
        of GPS.Menu that was just created.

        If ``ref`` and ``add_before`` are specified, they specify the name of
        another item in the parent menu (and not a full path) before or after
        which the new menu should be added.

        If the name of the menu starts with a '-' sign, as in "/Edit/-", then a
        menu separator is inserted instead. In this case, on_activate is
        ignored.

        Underscore characters ('_') need to be duplicated in the path. A single
        underscore indicates the mnemonic to be used for that menu. For
        instance, if you create the menu "/_File", then the user can open the
        menu by pressing alt-F. But the underscore itself will not be displayed
        in the name of the menu.

        If ``group`` is specified, create a radio menu item in given group.

        :param path: A string
        :param on_activate: A subprogram, see the GPS documentation on
            subprogram parameters
        :param ref: A string
        :param add_before: A boolean
        :param filter: A subprogram
        :param group: A string
        :return: The instance of :class:`GPS.Menu`

        .. code-block:: python

           def on_activate(self):
               print "A menu was selected: " + self.data

           menu = GPS.Menu.create("/Edit/My Company/My Action", on_activate)
           menu.data = "my own data"   ## Store your own data in the instance

        """
        pass  # implemented in Ada

    @staticmethod
    def get(path):
        """
        Return the menu found at the given path. Path is similar to what one
        finds on a hard disk, starting with the main GPS menu ('/'), down to
        each submenus. For instance, '/VCS/Directory/Update Directory' refers
        to the submenu 'Update Directory' of the submenu 'Directory' of the
        menu 'VCS'. Path is case-sensitive

        :param path: A string
        :return: The instance of GPS.Menu

        .. code-block:: python

           # The following example will prevent the user from using the VCS
           # menu and all its entries:

           GPS.Menu.get('/VCS').set_sensitive (False)
        """
        pass  # implemented in Ada

    def get_active(self):
        """
        Return True if the widget is a currently active radio menu item

        :return: A boolean
        """
        pass  # implemented in Ada

    def rename(self, name):
        """
        Change the name of a menu. The first underscore character seen in name
        will be used as the keyboard shortcut to access this menu from now
        on. If you actually want to insert an underscore in the name, you need
        to double it

        :param name: A string
        """
        pass  # implemented in Ada

    def set_active(self, is_active=True):
        """
        Set the active state of a radio menu item

        :param is_active: A boolean
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

    @staticmethod
    def __del__():
        """
        Called when the message instance is destroyed.
        """
        pass  # implemented in Ada

    def __init__(self, category, file, line, column, text, flags):
        """
        Add a Message in GPS.

        :param category: A String indicating the message category
        :param file: A File indicating the file
        :param line: An integer indicating the line
        :param column: An integer indicating the column
        :param text: A pango markup String containg the message text
        :param flags: An integer representing the location of the message

        .. code-block:: python

           # Create a message

           m=GPS.Message("default", GPS.File("gps-main.adb"),
                 1841, 20, "test message", 0)

           # Remove the message
           m.remove()
        """
        pass  # implemented in Ada

    def execute_action(self):
        """
        If the message has an associated action, execute it.
        """
        pass  # implemented in Ada

    def get_category(self):
        """
        Return the message's category.
        """
        pass  # implemented in Ada

    def get_column(self):
        """
        Return the message's column.
        """
        pass  # implemented in Ada

    def get_file(self):
        """
        Return the message's file.
        """
        pass  # implemented in Ada

    def get_flags(self):
        """
        Return an integer which represents the location of the message
        """
        pass  # implemented in Ada

    def get_line(self):
        """
        Return the message's line.
        """
        pass  # implemented in Ada

    def get_mark(self):
        """
        Return an EditorMark which was created with the message and keeps track
        of the location when the file is edited.
        """
        pass  # implemented in Ada

    def get_text(self):
        """
        Return the message's text.
        """
        pass  # implemented in Ada

    @staticmethod
    def list(file=None, category=None):
        """
        Return a list of all messages currently stored in GPS.

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
        """Remove the message from GPS."""
        pass  # implemented in Ada

    def set_action(self, action, image, tooltip=None):
        """
        Add an action item to the message. This will add an icon to the
        message, and clicking on this icon will execute action.

        :param action: A String corresponding to a registered GPS action.

        :param image: A String corresponding to the id of a registered GPS
           image. See icons.xml for an example of how to register icons in GPS.

        :param tooltip: A string which contains the tooltip to
           display when the mouse is on the icon.
        """
        pass  # implemented in Ada

    @staticmethod
    def set_sort_order_hint(category, hint):
        """
        Sets default sorting method for files in Locations view.

        :param category: Name of messages category
        :param hint: Default sorting method ("chronological" or
           "alphabetical")
        """
        pass  # implemented in Ada

    def set_style(self, style):
        """
        Set the style of the message. The second parameter indicates the length
        in number of characters to highlight. If 0, then highlight the whole
        line. If left out, this means the length of the message highlighting is
        not modified.

        :param style: An Integer
        """
        pass  # implemented in Ada

    def set_subprogram(self, subprogram, image, tooltip=None):
        """
        Add an action item to the message. This will add an icon to the
        message, and clicking on this icon will execute the subprogram, with
        the messaged passed as parameter of the subprogram.

        :param subprogram: A subprogram in the scripting language.
            This subprogram takes as a parameter one message.

        :param image: A String corresponding to the id of a registered GPS
           image. See icons.xml for an example of how to register icons in GPS.

        :param tooltip: A string which contains the tooltip to
           display when the mouse is on the icon.

        .. code-block:: python

           # This adds a "close" button to all the messages
           [msg.set_subprogram(lambda m : m.remove(), "gtk-close", "")
                               for msg in GPS.Message.list()]
        """
        pass  # implemented in Ada


###########################################################
# Missing_Arguments
###########################################################

class Missing_Arguments(Exception):
    """
    An exception raised by GPS. Raised when calling a subprogram from the GPS
    module with missing arguments
    """
    pass  # implemented in Ada


###########################################################
# Preference
###########################################################

class Preference(object):
    """
    Interface to the GPS preferences, as set in the Edit/Preferences
    dialog. New preferences are created through XML customization files (or
    calls to GPS.parse_xml(), see the GPS documentation

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
        Initializes an instance of the GPS.Preference class, associating it
        with the preference given in parameter. The name is the one that can be
        found in the $HOME/.gps/preferences file. When you are creating a new
        preference, this name can include '/' characters, which will result in
        subpages created in the Preferences dialog. The name after the last '/'
        should only include letters and '-' characters.

        :param name: A string
        """
        pass  # implemented in Ada

    def create(self, label, type, doc='', default='', *args):
        """
This function creates a new preference, and makes it visible in the preferences
dialog. In the dialog, the preference appears in the page given by the name
used when creating the instance of GPS.Preference. The label is used to qualify
the preference, and doc will appear as a tooltip to explain the preference to
users. The type describes the type of preference, and therefore how it should
be edited by users.

The additional parameters depend on the type of preference you are creating:

- For an "integer", the default value is 0, and the two additional parameters
  are the minimum and maximum possible values. These are integers.

- For a "boolean", the default is True.

- For a "string", the default is the empty string.

- A "multiline" behaves the same as a string except it is edited on multiple
  lines in the Preferences dialog.

- For a "color", the default is "black".

- For a "font", the default is "sans 9".

- For a "enum", any number of additional parameters can be specified. They are
  all the possible values of the preference. The default is the index in the
  list of possible values, starting at 0.

:param label: A string
:param type: A string, one of "integer", "boolean", "string",
    "color", "font", "enum", "multiline"
:param doc: A string
:param default: Depends on the type
:param args: Additional parameters depending on the type
        """
        pass  # implemented in Ada

    def get(self):
        """
        Get value for the given preference. The exact returned type depends on
        the type of the preference. Note that boolean values are returned as
        integers, for compatibility with older versions of Pythons

        :return: A string or an integer

        .. code-block:: python

           if GPS.Preference("MDI-All-Floating"):
              print "We are in all-floating mode"
        """
        pass  # implemented in Ada

    def set(self, value, save=True):
        """
        Set value for the given preference. The type of the parameter depends
        on the type of the preference. If the save parameter is true, the new
        value is immediately saved for future GPS sessions, and the new value
        is taken into account by GPS itself. Otherwise, if set to false, you
        will need to call the hook "preferences_changed" to force it

        :param value: A string, boolean or integer
        :param save: A boolean
        """
        pass  # implemented in Ada


###########################################################
# Process
###########################################################

class Process(Command):
    """
    Interface to expect-related commands. This class can be used to spawn new
    processes and communicate with them later on. It is similar to what GPS
    uses to communicate with gdb. This class is a subclass of GPS.Command.

    .. seealso::

       :func:`GPS.Process.__init__()`

       :func:`GPS.Command`

    .. code-block:: python

       # The following example launches a gdb process, let it print its welcome
       # message, and kills it as soon as a prompt is seen in the output.  In
       # addition, it displays debugging messages in a new GPS window.  As you
       # might note, some instance-specific data is stored in the instance of
       # the process, and can be retrieve in each callback.

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
       # slightly cleaner, since it doesn't pollute the global namespace.

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
                 case_sensitive_regexp=True, strip_cr=True):
        """
Spawn specified command. Command can include triple-quoted strings, similar to
python, which will always be preserved as one argument.

If ``regexp`` is not-empty and ``on_match_action`` is specified, launch
``on_match_action`` when ``regexp`` is found in the process output. If
``on_exit_action`` is specified, execute it when the process terminates. Return
the ID of the spawned process.

``regexp`` is always compiled with the multi_line option, so that "^" and "$"
also match at the beginning and end of each line, not just the whole
output. You can optionally compile it with the single_line option whereby "."
also matches the newline character. Likewise you can set the regexp to be case
insensitive by setting case_sensitive_regexp to False.

``on_match`` is a subprogram called with the parameters:

  - $1 = the instance of GPS.Process
  - $2 = the string which matched the regexp
  - $3 = the string since the last match

``before_kill`` is a subprogram called just before the process is about to be
killed. It is called when the user is interrupting the process through the task
manager, or when GPS exits. It is not called when the process terminates
normally. When it is called, the process is still valid and can be send
commands. Its parameters are:

  - $1 = the instance of GPS.Process
  - $2 = the entire output of the process

``on_exit`` is a subprogram called when the process has exited. You can no
longer send input to it at this stage. Its parameters are:

  - $1 = the instance of GPS.Process
  - $2 = the exit status
  - $3 = the output of the process since the last call to on_match

If ``task_manager`` is set to True, the process will be visible in the GPS task
manager, and can be interrupted or paused by users. Otherwise, it will simply
be running in the background, and never visible to the user.  If
``progress_regexp`` is specified, then the output of the process will be
scanned for this regexp. The part that match will not be returned to
``on_match``. Instead, they will be used to guess the current progress of the
command. Two groups of parenthesis are parsed, the one at ``progress_current``,
and the one at ``progress_total``. The number returned for each of these groups
indicate the current progress of the command, and the total that must be
reached for this command to complete. For instance, if your process outputs
lines like "done 2 out of 5", you should create a regular expression that
matches the 2 and the 5 to guess the current progress. As a result, a progress
bar is displayed in the task manager of GPS, and will allow users to monitor
commands.

``remote_server`` represents the server used to spawn the process. By default,
the GPS_Server is used, which is always the local machine. See the section
"Using GPS for Remote Development" in the GPS documentation for more
information on this field.

If ``show_command`` is set, then the command line used to spawn the new Process
is displayed in the "Messages" console.

If ``strip_cr`` is true, the output of the process will have all its ASCII.CR
removed before the string is passed on to GPS and your script. This in general
provides better portability to Windows systems, but might not be suitable for
applications for which CR is relevant (for instance those that drive an ANSI
terminal).

An exception is raised if the process could not be spawned.

:param command: A string
:param regexp: A string
:param on_match: A subprogram, see the section
     "Subprogram parameters" in the GPS documentation
:param on_exit: A subprogram
:param task_manager: A boolean
:param progress_regexp: A string
:param progress_current: An integer
:param progress_total: An integer
:param before_kill: A subprogram
:param remote_server: A string. Possible values are "GPS_Server",
    the empty string (equivalent to "GPS_Server"), "Build_Server",
    "Debug_Server", "Execution_Server" and "Tools_Server".
:param show_command: A boolean
:param single_line_regexp: A boolean
:param case_sensitive_regexp: A boolean
:param strip_cr: A boolean

.. seealso:: :func:`GPS.Process`
        """
        pass  # implemented in Ada

    def expect(self, regexp, timeout=-1):
        """
        Block the execution of the script until either regexp has been seen in
        the output of the command, or the timeout has expired. If the timeout
        is negative, wait forever until we see the regexp or the process
        finishes its execution.

        While in such a call, the usual ``on_match`` callback is called as
        usual, so you might need to add an explicit test in your on_match
        callback not to do anything in this case.

        This command returns the output of the process since the start of the
        call to expect and up to the end of the text that matched regexp. Note
        that it will also include the output that was sent to the on_match
        callback while expect was running. It will not however include output
        already returned by a previous call to expect (nor does it guarantee
        that two successive calls to expect will return the full output of the
        process, since some output might have been matched by on_match between
        the two calls, and would not be returned by the second expect).

        If a timeout occurred or the process terminated, an exception is raised

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
        Wait till the process terminates, and return its output. This is the
        output since the call to get_result, ie if you call get_result after
        performing some calls to expect, the returned string does not return
        the output that was already returned by expect.

        :return: A string
        """
        pass  # implemented in Ada

    def interrupt(self):
        """Interrupt a process controlled by GPS"""
        pass  # implemented in Ada

    def kill(self):
        """Terminate a process controlled by GPS"""
        pass  # implemented in Ada

    def send(self, command, add_lf=True):
        """
        Send a line of text to the process. If you need to close the input
        stream to an external process, it often works to send the character
        ASCII 4, for instance through the python command chr(4).

        :param command: A string
        :param add_lf: A boolean
        """
        pass  # implemented in Ada

    def set_size(self, rows, columns):
        """
        Tells the process about the size of its terminal. Rows and columns
        should (but need not) be the number of visible rows and columns of the
        terminal in which the process is running.

        :param rows: An integer
        :param columns: An integer
        """
        pass  # implemented in Ada

    def wait(self):
        """
        Block the execution of the script until the process has finished
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
Represents a project file. See also the GPS documentation on how to create
new project attributes.

.. seealso:: :func:`GPS.Project.__init__`

Related hooks:

- "project_view_changed": Called whenever the project is recomputed, ie one of
  its attributes was changed by the user, the environment variables are
  changed,...

  Then is a good time to test the list of languages (GPS.Project.languages())
  that the project supports, and do language-specific customizations

- "project_changed": A new project was loaded. The hook above will be called
  after this one
    """

    def __cmp__(self, file):
        """
        Compare two instances of GPS.Project, and return -1, 0 or 1 depending
        on their relative sort order

        :param file: An instance of :class:`GPS.Project`
        :return: An integer
        """
        pass  # implemented in Ada

    def __hash__(self):
        """
        Return a hash value suitable for storing self in a dictionary

        :return: An integer
        """
        pass  # implemented in Ada

    def __init__(self, name):
        """
        Initializes an instance of GPS.Project. The project must be currently
        loaded in GPS

        :param name: The project name

        .. seealso:: :func:`GPS.Project.name`
        """
        pass  # implemented in Ada

    def __repr__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def __str__(self):
        """
        Return a string suitable for the display of self on screen. This is
        called implicitly by GPS and Python

        :return: A string
        """
        pass  # implemented in Ada

    def add_attribute_values(self, attribute, package, index, value):
        """
         Add some values to an attribute. You can add as much as many values
         you need at the end of the param list.  If the package is not
         specified, the attribute at the toplevel of the project is queried.
         The index only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value of this attribute
        :param value: A string, the name of the first value to add

        .. seealso::

           :func:`GPS.Project.set_attribute_as_string`

           :func:`GPS.Project.remove_attribute_values`

           :func:`GPS.Project.clear_attribute_values`

        .. code-block:: python

           GPS.Project.root().add_attribute_values(
               "Default_Switches", "Compiler", "ada", "-gnatwa", "-gnatwe");

        """
        pass  # implemented in Ada

    def add_dependency(self, path):
        """
        This commands adds a new dependency from self to the project file
        pointed to by path. This is the equivalent of putting a with clause in
        self, and means that the source files in self can depend on source
        files from the imported project

        :param path: The path to another project to depend on

        .. seealso:: :func:`GPS.Project.remove_dependency`
        """
        pass  # implemented in Ada

    def add_main_unit(self, *args):
        """
        Add some main units to the current project, and for the current
        scenario. The project is not saved automatically

        :param args: Any number of arguments, at least one

        """
        pass  # implemented in Ada

    @staticmethod
    def add_predefined_paths(sources='', objects=''):
        """
        Add some predefined directories to the source path or the objects
        path. These will be searched when GPS needs to open a file by its base
        name, in particular from the File->Open From Project dialog.  The new
        paths are added in front, so that they have priorities over previously
        defined paths.

        :param sources: A list of directories separated by the appropriate
            separator (':' or ';' depending on the system
        :param objects: As above

        .. code-block:: python

           GPS.Project.add_predefined_paths(os.pathsep.join(sys.path))
        """
        pass  # implemented in Ada

    def add_source_dir(self, directory):
        """
        Add a new source directory to the project. The new directory is added
        in front of the source path. You should call recompute() after calling
        this method, to recompute the list of source files. The directory is
        added for the current value of the scenario variables only. Note that
        if the current source directory for the project is not specified
        explicitly in the .gpr file), it will be overriden by the new directory
        you are adding. If the directory is already part of the source
        directories for the project, it is not added a second time.

        :param directory: A string

        .. seealso::

           :func:`GPS.Project.source_dirs`

           :func:`GPS.Project.remove_source_dir`
        """
        pass  # implemented in Ada

    def ancestor_deps(self):
        """
        Return the list of projects that might contain sources that depend on
        the project's sources. When doing extensive searches it isn't worth
        checking other projects. Project itself is included in the list.

        This is also the list of projects that import self.

        :return: A list of instances of GPS.Project

        .. code-block:: python

           for p in GPS.Project("kernel").ancestor_deps():
               print p.name()

           # will print the name of all the projects that import kernel.gpr
        """
        pass  # implemented in Ada

    def clear_attribute_values(self, attribute, package, index):
        """
         Clear the values list of an attribute.

         If the package is not specified, the attribute at the toplevel of the
         project is queried.

         The index only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value of
           this attribute
        """
        pass  # implemented in Ada

    def dependencies(self, recursive=False):
        """
        Return the list of projects on which self depends (either directly if
        recursive is False, or including indirect dependencies if recursive is
        True).

        :param recursive: A boolean
        :return: A list of :class:`GPS.Project` instances

        """
        pass  # implemented in Ada

    def file(self):
        """
        Return the project file

        :return: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def generate_doc(self, recursive=False):
        """
        Generate the documentation of the project and its subprojects if
        recursive is True, and display it with the default browser

        :param recursive: A boolean

        .. seealso:: :func:`GPS.File.generate_doc`
        """
        pass  # implemented in Ada

    def get_attribute_as_list(self, attribute, package='', index=''):
        """
        Fetch the value of the attribute in the project.

        If the package is not specified, the attribute at the toplevel of the
        project is queried.

        The index only needs to be specified if it applies to that attribute.

        If the attribute value is stored as a simple string, a list with a
        single element is returned. This function always returns the value of
        the attribute in the currently selected scenario.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value of this attribute
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
        Fetch the value of the attribute in the project.

        If the package is not specified, the attribute at the toplevel of the
        project is queried.

        The index only needs to be specified if it applies to that attribute.

        If the attribute value is stored as a list, the result string is a
        concatenation of all the elements of the list. This function always
        returns the value of the attribute in the currently selected scenario.

        When the attribute is not explicitely overridden in the project, the
        default value is returned. This default value is the one described in
        an XML file (see the GPS documentation for more information). This
        default value is not necessarily valid, and could for instance be a
        string starting with a parenthesis, as explained in the GPS
        documentation.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value of this attribute
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
        Return the name of the executable, either read from the project or
        computed from main

        :param main: :class:`GPS.File`
        :return: A string

        """
        pass  # implemented in Ada

    def get_property(self, name):
        """
        Return the value of the property associated with the project. This
        property might have been set in a previous GPS session if it is
        persistent. An exception is raised if no such property already exists
        for the project

        :param name: A string
        :return: A string

        .. seealso:: :func:`GPS.Project.set_property`
        """
        pass  # implemented in Ada

    def get_tool_switches_as_list(self, tool):
        """
        Same as get_attribute_as_list, but specialized for the switches of a
        specific tool. Tools are defined through XML customization files, see
        the GPS documentation for more information

        :param tool: The name of the tool whose switches you want to get
        :return: A list of strings

        .. seealso::

           :func:`GPS.Project.get_attribute_as_list`

           :func:`GPS.Project.get_tool_switches_as_string`

        .. code-block:: python

           # If GPS has loaded a customization file that contains the following
           # tags:
           #
           #    <?xml version="1.0" ?>
           #    <toolexample>
           #       <tool name="Find">
           #          <switches>
           #             <check label="Follow links" switch="-follow" />
           #          </switches>
           #       </tool>
           #    </toolexample>

           # The user will as a result be able to edit the switches for Find in
           # the standard Project Properties editor.

           # Then the python command

           GPS.Project("default").get_tool_switches_as_list("Find")

           # will return the list of switches that were set by the user in the
           # Project Properties editor.

        """
        pass  # implemented in Ada

    def get_tool_switches_as_string(self, tool):
        """
        Same as GPS.Project.get_attribute_as_string, but specialized for a
        specific tool.

        :param tool: The name of the tool whose switches you want to get
        :return: A string

        .. seealso:: :func:`GPS.Project.get_tool_switches_as_list`

        """
        pass  # implemented in Ada

    def is_modified(self, recursive=False):
        """
        Return True if the project has been modified but not saved yet. If
        recursive is true, then the return value takes into account all
        projects imported by self

        :param recursive: A boolean
        :return: A boolean
        """
        pass  # implemented in Ada

    def languages(self, recursive=False):
        """
        Return the list of languages that are used for the sources of the
        project (and its subprojects if recursive is True). This can be used to
        detect whether some specific action in a module should be activated or
        not. Language names are always lowercase

        :param recursive: A boolean
        :return: A list of strings

        .. code-block:: python

           # The following example adds a new menu only if the current project
           # supports C. This is refreshed every time the project is changed by
           # the user.

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
        Load a new project, which replaces the current root project, and return
        a handle to it. All imported projects are also loaded at the same
        time. If the project is not found, a default project is loaded.

        If ``force`` is True, then the user will not be asked whether to save
        the current project, whether it was modified or not.

        If ``keep_desktop`` is False, then load saved desktop configuration,
        keep current otherwise

        :param filename: A string, the full path to a project file
        :param force: A boolean
        :param keep_desktop: A boolean
        :return: An instance of :class:`GPS.Project`
        """
        pass  # implemented in Ada

    def name(self):
        """
        Return the name of the project. This doesn't include directory
        information, see self.file().name() if you wish to access that
        information

        :return: A string, the name of the project
        """
        pass  # implemented in Ada

    def object_dirs(self, recursive=False):
        """
        Return the list of object directories for this project. If Recursive is
        True, the source directories of imported projects is also
        returned. There might be duplicate directories in the returned list

        :param recursive: A boolean
        :return: A list of strings
        """
        pass  # implemented in Ada

    def properties_editor(self):
        """Launch a graphical properties editor for the project"""
        pass  # implemented in Ada

    @staticmethod
    def recompute():
        """
        Recompute the contents of a project, including the list of source files
        that are automatically loaded from the source directories. The project
        file is not reloaded from the disk, and this should only be used if you
        have created new source files outside of GPS for instance

        .. code-block:: python

            GPS.Project.recompute()
        """
        pass  # implemented in Ada

    def remove_attribute_values(self, attribute, package, index, value):
        """
         Removes some specific values from an attribute. You can set as much as
         many values you need at the end of the param list.

         If the package is not specified, the attribute at the toplevel of the
         project is queried.

         The index only needs to be specified if it applies to that attribute.

        :param attribute: A string, the name of the attribute
        :param package: A string, the name of the attribute's package
        :param index: A string, the name of the index for the specific value of this attribute
        :param value: A string, the name of the first value to remove

        .. seealso::

           :func:`GPS.Project.set_attribute_as_string`

           :func:`GPS.Project.add_attribute_values`

           :func:`GPS.Project.clear_attribute_values`

        .. code-block:: python

           GPS.Project.root().remove_attribute_values(
               "Default_Switches", "Compiler", "ada", "-gnatwa", "-gnatwe");
        """
        pass  # implemented in Ada

    def remove_dependency(self, imported):
        """
        Remove a dependency between two projects. You must call
        GPS.Project.recompute() once you are done doing all the modifications
        on the projects

        :param imported: An instance of GPS.Project

        .. seealso:: :func:`GPS.Project.add_dependency`
        """
        pass  # implemented in Ada

    def remove_property(self, name):
        """
        Removes a property associated with a project

        :param name: A string

        .. seealso:: :func:`GPS.Project.set_property`
        """
        pass  # implemented in Ada

    def remove_source_dir(self, directory):
        """
        Remove a source directory from the project. You should call recompute()
        after calling this method, to recompute the list of source files. The
        directory is added for the current value of the scenario variables only

        :param directory: A string

        .. seealso:: :func:`GPS.Project.add_source_dir`
        """
        pass  # implemented in Ada

    def rename(self, name, path='<current path>'):
        """
        Rename and move a project file (the project will only be put in the new
        directory when it is saved, but will not be removed from its original
        directory). You must call GPS.Project.recompute() sometime after
        changing the name.

        :param name: A string
        :param path: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def root():
        """
        Return the root project currently loaded in GPS

        :return: An instance of GPS.Project

        .. code-block:: python

           print "Current project is " + GPS.Project.root().name()
        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables():
        """
        Return the list of scenario variables for the current project
        hierarchy, and their current value. These variables are visible at the
        top of the Project View in the GPS window. The initial value for these
        variables is set from the environment variables' value when GPS is
        started. However, changing the value of the environment variable later
        on doesn't change the value of the scenario variable.

        :return: hash table associating variable names and values

        .. seealso:: :func:`GPS.Project.set_scenario_variable`

        .. code-block:: python

           GPS.Project.scenario_variables()["foo"]
           => returns the current value for the variable foo

        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables_cmd_line(prefix=''):
        """
        Return a concatenation of VARIABLE=VALUE, each preceded by the given
        prefix. This string will generally be used when calling external tools,
        for instance make or GNAT

        :param prefix: String to print before each variable in the output
        :return: a string

        .. code-block:: python

           # The following GPS action can be defined in an XML file, and will launch
           # the make command with the appropriate setup for the environment
           # variables:
           #   <action name="launch make"> \
           #     <shell lang="python">GPS.scenario_variables_cmd_line()</shell>  \
           #     <external>make %1</external> \
           #   </action>
        """
        pass  # implemented in Ada

    @staticmethod
    def scenario_variables_values():
        """
        Return a hash table where keys are the various scenario variables
        defined in the current project and values the different values that
        this variable can get.

        :return: A hash table of strings
        """
        pass  # implemented in Ada

    def search(self, pattern, case_sensitive=False, regexp=False,
               scope='whole', recursive=True):
        """
        Return the list of matches for pattern in all the files belonging to
        the project (and its imported projects if recursive is true
        (default). Scope is a string, and should be any of 'whole', 'comments',
        'strings', 'code'. The latter will match only for text outside of
        comments

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
        single value.  If the package is not specified, the attribute at the
        toplevel of the project is queried.  The index only needs to be
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
        standard mechanism in that there is no garantee that the same instance
        of GPS.Project will be created for each physical project on the disk,
        and therefore you would not be able to associate a property with the
        physical project itself

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
        Change the value of a scenario variable. You need to call
        GPS.Project.recompute() to activate this change (so that multiple
        changes to the project can be grouped

        :param name: A string
        :param value: A string

        .. seealso:: :func:`GPS.Project.scenario_variables`
        """
        pass  # implemented in Ada

    def source_dirs(self, recursive=False):
        """
        Return the list of source directories for this project. If Recursive is
        True, the source directories of imported projects is also
        returned. There might be duplicate directories in the returned list

        :param recursive: A boolean
        :return: A list of strings

        .. seealso:: :func:`GPS.Project.add_source_dir`
        """
        pass  # implemented in Ada

    def sources(self, recursive=False):
        """
        Return the list of source files for this project. If recursive is true,
        then all sources from imported projects are also returned. Otherwise,
        only the direct sources are returned. The basenames of the returned
        files are always unique: not two files with the same basenames are
        returned, and the one returned is the first one see while traversing
        the project hierarchy

        :param recursive: A boolean
        :return: A list of instances of :class:`GPS.File`
        """
        pass  # implemented in Ada

    def update_xref(self, recursive=False):
        """
        Updates the cross-reference information in memory for all files of the
        project. This doesn't regenerate that information, just read all the
        .ali files found in the object directory of the project (and all
        imported projects if recursive is True). This should generally be
        called before calling GPS.freeze_xref, for efficiency.

        :param recursive: A boolean
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
        Add a directory to the path in which GPS looks for templates.  GPS will
        look for project templates in immediate subdirectories of this
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
    General interface to the revision browser
    """

    @staticmethod
    def add_link(file, revision_1, revision_2):
        """
        Create a link between revision_1 and revision_2 for the given file

        :param file: A string
        :param revision_1: A string
        :param revision_2: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def add_log(file, revision, author, date, log):
        """
        Add a new log entry into the revision browser

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
        Register a new symbolic name (tag or branches) corresponding to the
        specified revision of file

        :param file: A string
        :param revision: A string
        :param symbolic_name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def clear_view(file):
        """
        Clear file's revision view

        :param file: A string
        """
        pass  # implemented in Ada


###########################################################
# Style
###########################################################

class Style(object):
    """
    This class is used to manipulate GPS Styles, which are used for instance to
    represent graphical attributes given to Messages.
    """

    def __init__(self, name, create):
        """
        Create a Style

        :param name: A String indicating the name of the Style
        :param create: A File indicating the file

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
        Return a Boolean indicating whether this style is shown in
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
        Return a list of all styles currently registered in GPS.

        :return: a list of :class:`GPS.Style`
        """
        pass  # implemented in Ada

    def set_background(self, noname):
        """
        Set the background of style to the given color.

        :param noname: A string representing a color, for instance "blue" or
           "#0000ff"
        """
        pass  # implemented in Ada

    def set_foreground(self, noname):
        """
        Set the foreground of style to the given color.

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
    This class represents a gtk widget that can be used to edit a tool's
    command line.
    """

    def __init__(self, name, xml):
        """
        Creates a new SwitchesChooser widget from the tool's name and switch
        description in xml format.

        :param name: A string
        :param xml: A string
        """
        pass  # implemented in Ada

    def get_cmd_line(self):
        """
        Return the tool's command line parameter

        :return: A string
        """
        pass  # implemented in Ada

    def set_cmd_line(self, cmd_line):
        """
        Modify the widget's aspect to reflect the command line.

        :param cmd_line: A string
        """
        pass  # implemented in Ada


###########################################################
# Task
###########################################################

class Task(object):
    """
    This class provides an interface to the background tasks being handled by
    GPS, such as the build commands, the query of cross references, etc. These
    are the same tasks that are visible through the GPS Task Manager.

    Note that the classes represented with this class cannot be stored.
    """

    def interrupt(self):
        """Interrupt the task"""
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        :return: a list of :class:`GPS.Task`, all running tasks
        """
        pass  # implemented in Ada

    def name(self):
        """
        Return the name of the task

        :return: A string
        """
        pass  # implemented in Ada

    def pause(self):
        """Pause the task"""
        pass  # implemented in Ada

    def resume(self):
        """Resume the paused task"""
        pass  # implemented in Ada

    def status(self):
        """
        Return the status of the task

        :return: A string
        """
        pass  # implemented in Ada


###########################################################
# Timeout
###########################################################

class Timeout(object):
    """
    This class gives access to actions that must be executed regularly at
    specific intervals

    .. seealso:: :func:`GPS.Timeout.__init__`

    .. code-block:: python

       ## Execute callback three times and remove it
       import GPS;

       def callback(timeout):
          timeout.occur += 1
          print "A timeout occur=" + `timeout.occur`
          if timeout.occur == 3:
             timeout.remove()

       t = GPS.Timeout(500, callback)
       t.occur = 0
    """

    def __init__(self, timeout, action):
        """
        A timeout object executes a specific action repeatedly, at a specified
        interval, as long as it is registered.  The action takes a single
        argument, which is the instance of GPS.Timeout that called it.

        :param timeout: The timeout in milliseconds at which
           to execute the action
        :param action: A subprogram parameter to execute periodically
        """
        pass  # implemented in Ada

    def remove(self):
        """Unregister a timeout"""
        pass  # implemented in Ada


###########################################################
# ToolButton
###########################################################

class ToolButton(GUI):
    """
    This class represents a button that can be inserted in the toolbar

    .. seealso:: :func:`GPS.ToolButton.__init__`

    """

    def __init__(self, stock_id, label, on_click):
        """
Initializes a new button. When the button is pressed by the user, on_click is
called with the following single parameter:

   - $1 = The instance of GPS.Button

:param stock_id: A string identifying the icon
:param label: A string, the text that appears on the button
:param on_click: A subprogram, see the GPS documentation

.. code-block:: python

   b = GPS.ToolButton("gtk-new", "New File",
        lambda x : GPS.execute_action("/File/New"))
   GPS.Toolbar().insert(b, 0)

        """
        pass  # implemented in Ada


###########################################################
# Toolbar
###########################################################

class Toolbar(GUI):
    """
    Interface to commands related to the toolbar. This allows you to add new
    combo boxes to the GPS toolbars. Note that this can also be done through
    XML files, see the GPS documentation

    .. seealso:: :func:`GPS.Toolbar.__init__`

    .. code-block:: python

       import GPS

       def on_changed(entry, choice):
           print "changed " + choice + ' ' + entry.custom

       def on_selected(entry, choice):
           print "on_selected " + choice + ' ' + entry.custom

       ent = GPS.Combo("foo", label="Foo", on_changed=on_changed)

       GPS.Toolbar().append(ent, tooltip => "What it does")

       ent.custom = "Foo"  ##  Create any field you want
       ent.add(choice="Choice1", on_selected=on_selected)
       ent.add(choice="Choice2", on_selected=on_selected)
       ent.add(choice="Choice3", on_selected=on_selected)

    It is easier to use this interface through XML customization files,
    see the GPS documentation. However, this can also be done through
    standard GPS shell commands::

       Combo "foo" "Foo" "on_changed_action"
       Toolbar
       Toolbar.append %1 %2 "What it does"

       Toolbar
       Toolbar.get %1 "foo"
       Combo.add %1 "Choice1" "on_selected"action"
    """

    def __init__(self):
        """
        Initializes a new instance of the toolbar, associated with the default
        toolbar of GPS. This is called implicitly from python
        """
        pass  # implemented in Ada

    def append(self, widget, tooltip=''):
        """
        Add a new widget in the toolbar. This can be an instance of GPS.Combo,
        or a GPS.Button, or a GPS.ToolButton.

        :param widget: An instance of :class:`GPS.GUI`
        :param tooltip: A string
        """
        pass  # implemented in Ada

    def get(self, id):
        """
        Return the toolbar entry matching the given id. An error is raised if
        no such entry exists. The same instance of GPS.Combo is always returned
        for each specific id, therefore you can store your own fields in this
        instance and access it later.

        :param id: A string, the name of the entry to get
        :return: An instance of :class:`GPS.Combo`

        .. code-block:: python

           ent = GPS.Combo("foo")
           GPS.Toolbar().append(ent)
           ent.my_custom_field = "Whatever"

           print GPS.Toolbar().get("foo").my_custom_field
           =>  "Whatever"
        """
        pass  # implemented in Ada

    def get_by_pos(self, position):
        """
        Return the position-th widget in the toolbar. If the widget was created
        from a scripting language, its instance is returned. Otherwise, a
        generic instance of GPS.GUI is returned. This can be used to remove
        some items from the toolbar for instance

        :param position: An integer, starting at 0
        :return: An instance of a child of GPS.GUI

        .. code-block:: python

           GPS.Toolbar().get_by_pos(0).set_sensitive(False)
           # can be used to gray out the first item in the toolbar

        """
        pass  # implemented in Ada

    def insert(self, widget, pos=-1, tooltip=''):
        """
        Add a new widget in the toolbar. This can be an instance of GPS.Combo,
        or a GPS.Button, or a GPS.ToolButton.

        :param widget: An instance of :class:`GPS.GUI`
        :param pos: The position at which to insert the widget
        :param tooltip: A string

        """
        pass  # implemented in Ada


###########################################################
# Unexpected_Exception
###########################################################

class Unexpected_Exception(Exception):
    """
    An exception raised by GPS. It indicates an internal error in GPS, raised
    by the Ada code itself. This exception is unexpected and indicates a bug in
    GPS itself, not in the python script, although it might be possible to
    modify the latter to work around the issue
    """
    pass  # implemented in Ada


###########################################################
# VCS
###########################################################

class VCS(object):
    """
    General interface to version control systems
    """

    @staticmethod
    def annotate(file):
        """
        Display the annotations for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def annotations_parse(vcs_identifier, file, output):
        """
        Parses the output of the annotations command (cvs annotate for
        instance), and add the corresponding information to the left of the
        editor

        :param vcs_identifier: A string
        :param file: A string
        :param output: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def commit(file):
        """
        Commit file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def diff_head(file):
        """
        Show differences between local file and the head revision

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def diff_working(file):
        """
        Show differences between local file and the working revision

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_current_vcs():
        """
        Return the system supported for the current project

        :return: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_log_file(file):
        """
        Returns the GPS File corresponding to the log file for given file.

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def get_status(file):
        """
        Query the status for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def log(file, revision):
        """
        Get the revision changelog for file. If revision is specified, query
        the changelog for this specific revision, otherwise query the entire
        changelog

        :param file: A string
        :param revision: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def log_parse(vcs_identifier, file, string):
        """
        Parses string to find log entries for file. This command uses the
        parser in the XML description node for the VCS corresponding to
        vcs_identifier.

        :param vcs_identifier: A string
        :param file: A string
        :param string: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def remove_annotations(file):
        """
        Remove the annotations for file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def repository_dir(tag_name=''):
        """
        Returns the repository root directory, or if tag_name is specified the
        repository directory for the given tag or branch.

        :param tag_name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def repository_path(file, tag_name=''):
        """
        Returns the trunk repository path for file or if tag_name is specified
        the repository path on the given tag or branch path.

        :param file: A string
        :param tag_name: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def revision_parse(vcs_identifier, file, string):
        """
        Parses string to find revisions tags and branches information for
        file. This command uses the parser in the XML description node for the
        VCS corresponding to vcs_identifier.

        :param vcs_identifier: A string
        :param file: A string
        :param string: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def set_reference(file, reference):
        """
        Record a reference file (the file on which a diff buffer is based for
        example) for a given file

        :param file: A string
        :param reference: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def status_parse(vcs_identifier, string, clear_logs, local, dir=''):
        """
Parses a string for VCS status. This command uses the parsers defined in the
XML description node for the VCS corresponding to vcs_identifier.

- When local is FALSE, the parser defined by the node status_parser is used.

- When local is TRUE, the parser defined by the node local_status_parser is
  used.

If clear_logs is TRUE, the revision logs editors are closed for files that have
the VCS status "up-to-date".  Parameter dir indicates the directory in which
the files matched in string are located.

:param vcs_identifier: A string
:param string: A string
:param clear_logs: A boolean
:param local: A boolean
:param dir: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def supported_systems():
        """
        Show the list of supported VCS systems

        :return: List of strings
        """
        pass  # implemented in Ada

    @staticmethod
    def update(file):
        """
        Update file

        :param file: A string
        """
        pass  # implemented in Ada

    @staticmethod
    def update_parse(vcs_identifier, string, dir=''):
        """
        Parses a string for VCS status. This command uses the parsers defined
        in the XML description node for the VCS corresponding to
        vcs_identifier.

        Parameter dir indicates the directory in which the files matched in
        string are located.

        :param vcs_identifier: A string
        :param string: A string
        :param dir: A string
        """
        pass  # implemented in Ada


###########################################################
# Vdiff
###########################################################

class Vdiff(object):
    """
    This class provides access to the graphical comparison between two or three
    files or two versions of the same file within GPS. A visual diff is a group
    of two or three editors with synchronized scrolling. Differences are
    rendered using blank lines and color highlighting.
    """

    @staticmethod
    def __init__():
        """
        This function prevents the creation of a visual diff instance
        directly. You must use GPS.Vdiff.create() or GPS.Vdiff.get() instead.

        .. seealso::

           :func:`GPS.Vdiff.create`

           :func:`GPS.Vdiff.get`
        """
        pass  # implemented in Ada

    def close_editors(self):
        """
        Close all editors implied in a visual diff.
        """
        pass  # implemented in Ada

    @staticmethod
    def create(file1, file2, file3=''):
        """
        If none of the files given as parameter is already used in a visual
        diff, this function creates a new visual diff and returns
        it. Otherwise, None is returned.

        :param file1: An instance of :class:`GPS.File`
        :param file2: An instance of :class:`GPS.File`
        :param file3: An instance of :class:`GPS.File`
        :return: An instance of :class:`GPS.Vdiff`
        """
        pass  # implemented in Ada

    def files(self):
        """
        Return the list of files used in a visual diff.

        :return: A list of GPS.File

        """
        pass  # implemented in Ada

    @staticmethod
    def get(file1, file2='', file3=''):
        """
        Return an instance of an already exisiting visual diff. If an instance
        already exists for this visual diff, it is returned. All files passed
        as parameters have to be part of the visual diff but not all files of
        the visual diff have to be passed for the visual diff to be
        returned. For example if only one file is passed the visual diff that
        contains it, if any, will be returned no matter it is a two or three
        files visual diff.

        :param file1: An instance of :class:`GPS.File`
        :param file2: An instance of :class:`GPS.File`
        :param file3: An instance of :class:`GPS.File`
        """
        pass  # implemented in Ada

    @staticmethod
    def list():
        """
        This function returns the list of visual diff currently opened in GPS.

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
        Recompute a visual diff. The content of each editor used in the visual
        diff is saved. The files are recompared and the display is redone
        (blank lines and color highlighting).
        """
        pass  # implemented in Ada


###########################################################
# XMLViewer
###########################################################

class XMLViewer(object):
    """
    This class represents Tree-based views for XML files

    """

    def __init__(self, name, columns=3, parser=None, on_click=None,
                 on_select=None, sorted=False):
        """
Create a new XMLViewer, with the given name.

``columns`` is the number of columns that the table representation should
have. The first column is always the one used for sorting the table.

``parser`` is a subprogram called for each XML node that is parsed. It takes
three arguments: the name of the XML node being visited, its attributes (in the
form "attr='foo' attr="bar""), and the text value of that node. This subprogram
should return a list of strings, one per visible column create for the
table. Each element will be put in the corresponding column.

If ``parser`` is unspecified, the default is to display in the first column the
tag name, in the second column the list of attributes, and in the third column
when it exists the textual contents of the node.

``on_click`` is an optional subprogram. It is called every time the user
double-click on a line, and is passed the same arguments as Parser. It has no
return value.

``on_select`` has the same profile as ``on_click``, but is called when the user
has selected a new line, not double-clicked on it.

If ``sorted`` is True, then the resulting graphical list is sorted on the first
column.

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

    def on_click(node_Name, attrs, value):
       if node_name == "file":
          GPS.EditorBuffer.get(GPS.File(value))
        """
        pass  # implemented in Ada

    def create_metric(self, name):
        """
        Create a new XMLViewer for an XML file generated by gnatmetric.  Name
        is used as the name for the window

        :param name: A string

        """
        pass  # implemented in Ada

    def parse(self, filename):
        """
        Replace the contents of self by that of the XML file

        :param filename: An XML file
        """
        pass  # implemented in Ada

    def parse_string(self, str):
        """
        Replace the contents of self by that of the XML string

        :param str: A string
        """
        pass  # implemented in Ada


###########################################################
# Globals
###########################################################

def __run_hook__():
    """
    Internal function used for the support of hooks
    """
    pass  # implemented in Ada


def add_location_command(command):
    """
    Add a command to the navigation buttons in the toolbar. When the user
    presses the back button, this command will be executed, and should put GPS
    in a previous state. This is for instance used while navigating in the HTML
    browsers to handle the back button

    :param command: A string

    """
    pass  # implemented in Ada


def base_name(filename):
    """
    Returns the base name for the given full path name

    :param filename: A string

    """
    pass  # implemented in Ada


def cd(dir):
    """
    Change the current directory to dir

    :param dir: A string

    """
    pass  # implemented in Ada


def clear_cache():
    """
    Free the internal cache used for return values. This function needs to be
    called explicitly, or previously returned value are never freed. After
    calling this function, you can no longer use %1, %2,... to refer to
    previously returned values.
    """
    pass  # implemented in Ada


def compute_xref():
    """
    Update the cross-reference information stored in GPS. This needs to be
    called after major changes to the sources only, since GPS itself is able to
    work with partially up-to-date information

    """
    pass  # implemented in Ada


def compute_xref_bg():
    """
    Update in the background cross-reference information stored in GPS.

    .. seealso:: :func:`GPS.compute_xref`

    """
    pass  # implemented in Ada


def contextual_context():
    """
    Returns the context at the time the contextual menu was open.

    This function will only return a valid context while the menu is open, or
    while an action executed from that menu is executed. You can store your own
    data in the returned instance, so that for instance you can precompute some
    internal data in the filters for the contextual actions (see <filter> in
    the XML files), and reuse that precomputed data when the menu is executed.
    See also the documentation for "contextual_menu_open" hook.

    :return: An instance of GPS.FileContext, GPS.AreaContext,...

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
       # without relying on the hooks to initialize the value. We set the value
       # in the context the first time we need it, instead of every time the
       # menu is open.

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


def current_context():
    """
    Returns the current context in GPS. This is the currently selected file,
    line, column, project,... depending on what window is currently
    active. From one call of this function to the next, a different instance is
    returned, and therefore you shouldn't store your own data in the instance,
    since you will not be able to recover it later on

    :return: An instance of GPS.FileContext, GPS.AreaContext,...

    .. seealso::

       :func:`GPS.Editor.get_line`

       :func:`GPS.MDI.current:` Access the current window

       :func:`GPS.contextual_context`
    """
    pass  # implemented in Ada


def debug_memory_usage(size):
    """
    Dumps on stdout the size biggest memory allocators in GPS. This is really
    meant as a debug function for GPS developers

    :param size: An integer

    """
    pass  # implemented in Ada


def delete(name):
    """
    Delete file/directory name from the file system

    :param name: A string
    """
    pass  # implemented in Ada


def dir(pattern=''):
    """
    list files following pattern (all files by default)

    :param pattern: A string
    :return: A list of strings
    """
    pass  # implemented in Ada


def dir_name(filename):
    """
    Returns the directory name for the given full path name

    :param filename: A string
    """
    pass  # implemented in Ada


def dump(string, add_lf=False):
    """
    Dump string to a temporary file. Return the name of the file. If add_lf is
    TRUE, append a line feed at end of file

    :param string: A string
    :param add_lf: A boolean
    :return: A string, the name of the output file
    """
    pass  # implemented in Ada


def dump_file(text, filename):
    """
    Writes text to filename on the disk. This is mostly intended for poor
    shells like the GPS shell which do not have better solutions. In python, it
    is recommended to use python's own mechanisms

    :param text: A string
    :param filename: A string
    """
    pass  # implemented in Ada


def dump_xref_db():
    """
    Dump in the file $HOME/.gps/db_dump the current contents of the
    cross-references database. This is intended for debugging purposes only
    """
    pass  # implemented in Ada


def echo(*args):
    """
    Display a line of text. This command is specific to the GPS shell.

    :param args: Any number of parameters
    """
    pass  # implemented in Ada


def echo_error(*args):
    """
    Display a line of text. This command is specific to the GPS shell. It is
    designed to be used to output error messages. This command raises the shell
    windows.

    :param args: Any number of parameters
    """
    pass  # implemented in Ada


def exec_in_console(noname):
    """
    This function is specific to python. It executes the string given in
    argument in the context of the GPS Python console. If you use the standard
    python's exec() function instead, the latter will only modify the current
    context, which generally will have no impact on the GPS console itself.

    :param noname: A string

    .. code-block:: python

       # Import a new module transparently in the console, so that users can
       # immediately use it
       GPS.exec_in_console("import time")
    """
    pass  # implemented in Ada


def execute_action(action, *args):
    """
    Execute one of the actions defined in GPS. Such actions are either
    predefined by GPS or defined by the users through customization files.  See
    the GPS documentation for more information on how to create new actions.
    GPS will wait until the command completes to return the control to the
    caller, whether you execute a shell command, or an external process.

    The action's name can start with a '/', and be a full menu path. As a
    result, the menu itself will be executed, just as if the user had pressed
    it.

    The extra arguments must be strings, and are passed to the action, which
    can use them through $1, $2,...

    The list of existing actions can be found through the Edit->Actions menu.

    The action will not be executed if the current context is not appropriate
    for this action.

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
    This command is similar to GPS.execute_action. However, commands that
    execute external applications or menus are executed asynchronously:
    GPS.execute_asynchronous_action will immediately return, although the
    external application might not have completed its execution

    :param action: Name of the action to execute
    :param args: Any number of string parameters

    .. seealso:: :func:`GPS.execute_action`
    """
    pass  # implemented in Ada


def exit(force=False, status='0'):
    """
    Exit GPS, asking for confirmation if any file is currently modified and
    unsaved. If force is True, no check is done.

    Status is the exit status to return to the calling shell. 0 will generally
    mean success on all architectures.

    :param force: A boolean
    :param status: An integer

    """
    pass  # implemented in Ada


def extract_method(file, line_start, line_end, method_name='New_Method'):
    """
    Extract the code from line_start to line_end in the specified file into a
    new subprogram with the given name. All needed local variables are declared
    properly in this new subprogram, and it is given parameters if needed

    :param file: A string
    :param line_start: An integer
    :param line_end: An integer
    :param method_name: A string
    """
    pass  # implemented in Ada


def freeze_xref():
    """
    Forces GPS to use the cross-reference information it already has in
    memory. GPS will no longer check on the disk whether more recent
    information is available. This can provide a significant speedup in complex
    scripts or scripts that need to analyze the cross-reference information for
    lots of files. In such cases, the script should generally call
    GPS.Project.update_xref to first load all the required information in
    memory.

    You need to explicitly call GPS.thaw_xref to go back to the default GPS
    behavior. Note the use of the "finally" exception handling in the following
    example, which ensures that even if there is some unexpected exception, the
    script always restores properly the default behavior.

    .. seealso::

      :func:`GPS.Project.update_xref`

      :func:`GPS.thaw_xref`

    .. code-block:: python

       try:
          GPS.Project.root().update_xref(recursive=True)
          GPS.freeze_xref()
          ... complex computation

       finally:
          GPS.thaw_xref()
    """
    pass  # implemented in Ada


def get_build_mode():
    """
    Return the name of the current build mode. Return an empty string if no
    mode is registered.
    """
    pass  # implemented in Ada


def get_build_output(target_name, shadow, background, as_string):
    """
    Return the result of the last compilation command

    :param target_name: (optional) a string

    :param shadow: (optional) a Boolean, indicating whether we want the output
       of shadow builds

    :param background: (optional) a Boolean, indicating whether we want the
        output of background builds

    :param as_string: (optional) a Boolean, indicating whether the output
       should be returned as a single string. By default the output is returned as
       a list in script languages that support it.

    :return: A string or list, the output of the latest build for the corresponding target.

    .. seealso::

       :func:`GPS.File.make`

       :func:`GPS.File.compile`
    """
    pass  # implemented in Ada


def get_busy():
    """
    Return the "busy" state

    .. seealso::

       :func:`GPS.set_busy`

       :func:`GPS.unset_busy`
    """
    pass  # implemented in Ada


def get_home_dir():
    """
    Return the directory that contains the user-specific files. This directory
    always ends with a directory separator

    :return: The user's GPS directory

    .. seealso:: :func:`GPS.get_system_dir`

    .. code-block:: python

       log = GPS.get_home_dir() + "log"
       # will compute the name of the log file generated by GPS

    """
    pass  # implemented in Ada


def get_system_dir():
    """
    Return the installation directory for GPS.  This directory always ends with
    a directory separator

    :return: The install directory for GPS

    .. seealso:: :func:`GPS.get_home_dir`

    .. code-block:: python

       html = GPS.get_system_dir() + "share/doc/gps/html/gps.html"
       # will compute the location of GPS's documentation
    """
    pass  # implemented in Ada


def get_tmp_dir():
    """
    Return the directory where gps creates temporary files.  This directory
    always ends with a directory separator

    :return: The install directory for GPS
    """
    pass  # implemented in Ada


def help(command=''):
    """
    Return the description of the command given in parameter, or the list of
    all commands exported by GPS. GPS.help is specific to the GPS shell

    :param command: A string
    :return: A string
    """
    pass  # implemented in Ada


def insmod(shared_lib, module):
    """
Dynamically register a new module, reading its code from shared_lib.

The library must define the following two symbols:

- _init: This is called by GPS to initialize the library itself

- __register_module: This is called to do the actual module registration, and
  should call the Register_Module function in the GPS source code

This is work in progress, and not fully supported on all systems.

:param shared_lib: Library containing the code of the module
:param module: Name of the module

.. seealso:: :func:`GPS.lsmod`
    """
    pass  # implemented in Ada


def is_server_local(server):
    """
    Tell if the specified server is the local machine.

    :param server: The server. Possible values are "Build_Server",
       "Debug_Server", "Execution_Server" and "Tools_Server".
    :return: A boolean
    """
    pass  # implemented in Ada


def last_command():
    """
    This function returns the name of the last action executed by GPS. This
    name is not ultra-precise: it will be accurate only when the action is
    executed through a key binding. Otherwise, an empty string is
    returned. However, the intent here is for a command to be able to check
    whether it is called multiple times in a row. For this, this command will
    return the command set by GPS.set_last_command() if it was set.

    :return: A string

    .. seealso:: :func:`GPS.set_last_command`

    .. code-block:: python

       def kill_line():
          '''Emulates Emacs behavior: when called multiple times, the cut line must be
             appended to the previously cut one.'''

          # The name of the command below is unknown to GPS. This is just a
          # string we use in this implementation to detect multiple consecutive
          # calls to this function. Note that this works whether the function is
          # called from the same key binding or not, and from the same GPS action
          # or not

          append = GPS.last_command() == "my-kill-line":
          GPS.set_last_command("my-kill-line")
    """
    pass  # implemented in Ada


def load(filename):
    """
    Load and execute a script file. This command is specific to the GPS shell.

    :param filename: A string
    """
    pass  # implemented in Ada


def lookup_actions():
    """
    This command returns the list of all known GPS actions. This doesn't
    include menu names. All actions are lower-cased, but the order in the list
    is not significant.

    :return: A list of strings

    .. seealso:: :func:`GPS.lookup_actions_from_key`
    """
    pass  # implemented in Ada


def lookup_actions_from_key(key):
    """
    Given a key binding, for instance "control-x control-b", this function
    returns the list of actions that could be executed. Not all actions would
    be executed, though, since only the ones for which the filter matches are
    executed. The name of the actions is always in lower cases.

    :param key: A string
    :return: A list of strings

    .. seealso:: :func:`GPS.lookup_actions`
    """
    pass  # implemented in Ada


def ls(pattern=''):
    """
    list files following pattern (all files by default)

    :param pattern: A string
    :return: A list of strings
    """
    pass  # implemented in Ada


def lsmod():
    """
    Return the list of modules that are currently registered in GPS. Each
    facility in GPS is provided in a separate module, so that users can choose
    whether to activate specific modules or not. Some modules can also be
    dynamically loaded

    :return: List of strings

    .. seealso:: :func:`GPS.insmod`
    """
    pass  # implemented in Ada


def macro_load(file):
    """
    Load file containing a set of recorded events

    :param file: A string
    """
    pass  # implemented in Ada


def macro_play(speed='1.0'):
    """
    Play current set of events

    :param speed: A string
    """
    pass  # implemented in Ada


def macro_record():
    """
    Start recording set of events
    """
    pass  # implemented in Ada


def parse_xml(xml):
    """
    Load an XML customization string. This string should contain one or more
    toplevel tags similar to what is normally found in custom files, such as
    <key>, <alias>, <action>,..

    Optionally you can also pass the full contents of an XML file, starting
    from the <?xml?> header.

    :param xml: The XML string to parse

    .. code-block:: python

       GPS.parse_xml(
          '''<action name="A"><shell>my_action</shell></action>
             <menu action="A"><title>/Edit/A</title></menu>''')
       Adds a new menu in GPS, which executes the command my_action
    """
    pass  # implemented in Ada


def pwd():
    """
    Print name of current/working directory

    :return: A string

    This command will have the same return value as the standard Python command
    os.getcwd(). The current directory can also be changed through a call to
    os.chdir("dir").
    """
    pass  # implemented in Ada


def repeat_next(count):
    """
    This action will execute the next one <count> times.

    :param count: An integer
    """
    pass  # implemented in Ada


def reset_xref_db():
    """
    Reset the internal cross-reference database that GPS is using for most of
    its navigation facilities. You shouldn't have to call that yourself, since
    in general GPS should know by itself when it is necessary to refresh its
    database. However, this might be used as a workaround if you think you have
    troubles with the cross-reference information which isn't accurate.
    """
    pass  # implemented in Ada


def save_persistent_properties():
    """
    Forces an immediate save of the persistent properties that GPS maintains
    for files and projects (for instance the text encoding, the programming
    language, the debugger breakpoints,...).

    You do not have to call this subprogram explicitly in general, since this
    is done automatically by GPS on exit.
    """
    pass  # implemented in Ada


def set_build_mode(mode=''):
    """
    Set the current build mode. If specified mode is not a registered mode, do nothing.

    :param mode: Name of the mode to set
    """
    pass  # implemented in Ada


def set_busy():
    """
    Activate the "busy" state in GPS by animating the GPS icon. This command
    can be called recursively, and GPS.unset_busy should be called a
    corresponding number of time to stop the animation.

    .. seealso::

       :func:`GPS.unset_busy`

       :func:`GPS.get_busy`
    """
    pass  # implemented in Ada


def set_last_command(command):
    """
    This function overrides the name of the last command executed by GPS. This
    new name will be the one returned by GPS.last_command() until the user
    performs a different action. Thus, multiple calls of the same action in a
    row will always return the value of the command parameter. See the example
    in GPS.last_command()

    :param command: A string

    .. seealso:: :func:`GPS.last_command`
    """
    pass  # implemented in Ada


def supported_languages():
    """
    Return the list of languages for which GPS has special handling. Any file
    can be open in GPS, but some extensions are recognized specially by GPS to
    provide syntax highlighting, cross-references, or other special
    handling. See the GPS documentation on how to add support for new languages
    in GPS.

    The returned list is sorted alphabetically, and the name of the language
    has been normalized (start with an upper case, and use lowercases for the
    rest except after an underscore character)

    :return: List of strings

    .. code-block:: python

       GPS.supported_languages()[0]
       => return the name of the first supported language
    """
    pass  # implemented in Ada


def thaw_xref():
    """
    See GPS.freeze_xref for more information

    .. seealso:: :func:`GPS.freeze_xref`
    """
    pass  # implemented in Ada


def unset_busy():
    """
    Reset the "busy" state

    .. seealso::

       :func:`GPS.set_busy`

       :func:`GPS.get_busy`
    """
    pass  # implemented in Ada


def version():
    """
    Return GPS version as a string.

    :return: A string
    """
    pass  # implemented in Ada


def visual_diff(file1, file2, file3=''):
    """
    Open a Visual Diff between file1, file2 and file3

    :param file1: A string
    :param file2: A string
    :param file3: A string
    """
    pass  # implemented in Ada

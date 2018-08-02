"""
This package provides high-level features that help extend GPS.
It makes it easy to connect to the GPS hooks, or to create new
views that can be saved as part of the desktop and restored when
GPS is restarted.

Here is an example of use for this package::

  from gps_utils import remove_interactive, make_interactive
  from gi.repository import Gtk, GLib

  class My_Module(Module):
    def setup(self):
        # Create menus when this module is setup.
        # It is better to call make_interactive here than use the @interactive
        # decorator on the method. The latter would take effect even if the
        # module is never initialized.

        make_interactive(
            self.get_view,
            menu="/Tools/Views/%s" % self.view_title)

    def teardown(self):
        # Undo the effect of setup()
        remove_interactive(menu="/Tools/Views/%s" % self.view_title)

    def preferences_changed(self):
        # A method automatically connected to the homonym hook
        print "preferences changed"

    def create_view(self):
        box = Gtk.VBox()
        button = Gtk.Button("Button")
        box.pack_start(button, False, False, 0)

        self.stored_something = box
        return box

    def on_view_destroy(self):
        self.stored_something = None

Sometimes, the module is wrapping an GPS.GUI object that has been created
by GPS itself (for instance a :class:`GPS.Browsers.View`). Since
:func:`GPS.Browsers.View.create` is putting the view directly in the MDI,
the module does not get to call :func:`GPS.MDI.add`, and as such the modules
:func:`save_desktop` function is never called by default. To work around this,
you need to pass your module's :func:`_save_desktop` (note the leading
underscore) as a parameter to :func:`GPS.Browsers.View.create`.
"""


import GPS
import traceback
import sys

try:
    # While building the doc, we might not have access to this module
    from gi.repository import GLib
except ImportError:
    pass


class Module_Metaclass(type):

    """
    A metaclass which ensures that any class derived from Module will
    automatically be known to GPS, and which connects a number of hooks
    into GPS.
    """

    gps_started = False
    # Whether the "gps_started" hook has already run

    modules = []
    modules_instances = []

    def __new__(cls, name, bases, attrs):
        new_class = type.__new__(cls, name, bases, attrs)

        if not attrs.get('abstract', False):
            Module_Metaclass.modules.append(new_class)

            # If gps has already started, we should immediately setup the
            # plugin, but the class has not been fully setup yet...
            if Module_Metaclass.gps_started:
                inst = new_class()
                Module_Metaclass.modules_instances.append(inst)
                GLib.idle_add(lambda: inst._setup())

                # Simulate running the gps_started hook
                pref = getattr(inst, "gps_started", None)
                if pref:
                    pref()

        return new_class

    @staticmethod
    def setup_all_modules(hook):
        if not Module_Metaclass.gps_started:
            Module_Metaclass.gps_started = True
            for ModuleClass in Module_Metaclass.modules:
                inst = ModuleClass()
                Module_Metaclass.modules_instances.append(inst)
                inst._setup()

    @staticmethod
    def load_desktop(name, data):
        """
        Support for loading desktop data.
        This is called directly by Ada (python_module.adb)
        """
        for m in Module_Metaclass.modules:
            child = m()._load_desktop(name, data)
            if child:
                return child


GPS.Hook("gps_started").add(Module_Metaclass.setup_all_modules)


class Module(object):

    """
    A Module is a singleton, so this class also ensures that pattern is
    followed. As a result, a Module's __init__ method is only called once.
    """

    __metaclass__ = Module_Metaclass

    abstract = True
    # Not a real module, so should never call setup()

    auto_connect_hooks = (
        "context_changed",
        "buffer_edited",
        # Do not include "gps_started". Users should override setup() instead,
        # which is called as part of the gps_started hook already. Otherwise,
        # when GPS runs "gps_started", we end up in __connect_hooks below,
        # which adds the local gps_started function to the callbacks. This
        # callback is then called as part of running the same hook, but the
        # automatic tests might have already called exit() by then.

        "project_view_changed",
        "preferences_changed",
        "semantic_tree_updated",
        "file_edited",
        "location_changed",
        "project_changed",   # not called for the initial project
        "compilation_finished",
        "task_started")
    # As a special case, if you class has a subprogram with a name of any
    # of the hooks below, that function will automatically be connected to
    # the hook (and disconnected when the module is teared down.

    mdi_position = GPS.MDI.POSITION_BOTTOM
    # the initial position of the window in the MDI (see GPS.MDI.add)

    mdi_group = GPS.MDI.GROUP_CONSOLES
    # the group for this window. This is used in case the user has already
    # created windows in this group, and in this case the new view will be
    # put on top of the existing windows.

    mdi_flags = GPS.MDI.FLAGS_ALL_BUTTONS
    # the default MDI flags for this view

    view_title = None
    # The name of the view that will be created by GPS. By default, this is
    # the name of your class, but you can override this as a class attribute
    # or in __init__

    def setup(self):
        """
        This function should be overridden in your own class if you need to
        create menus, actions, connect to hooks,...
        When setup() is called, the project has already been loaded in GPS.

        Do not call this function directly.
        """
        pass

    def teardown(self):
        """
        This function should be overridden to undo the effects of setup().

        Do not call this function directly.
        """
        pass

    def create_view(self):
        """
        Creates a new widget to present information to the user graphically.
        The widget should be created with pygobject, i.e. using Gtk.Button,
        Gtk.Box,...
        """
        return None

    def on_view_destroy(self):
        """
        Called when the view is destroyed.
        """
        return None

    def save_desktop(self, child):
        """
        Returns the data to use when saving a view in the desktop. The
        returned value will be passed to load_desktop the next time GPS is
        started.

        :param GPS.MDIWindow view: the view you created and put in the MDI.
           Use view.get_child to get access to the actual widget.
        :return: A string, some additional data to save in the XML file.
        """
        return ""

    def load_desktop(self, data):
        """
        This function is called when loading a widget from the desktop. It
        receives the data returned by save_desktop() that can be used to
        initialize a new window. Exceptions are handled and logged
        automatically.

        :param str data: as returned by save_desktop
        :return: the GPS.MDIWindow that was loaded, or null if it could not
            be loaded.
        """
        pass

    #########################################
    # Singleton
    #########################################

    def __new__(cls, *args, **kwargs):
        """
        Implement the singleton pattern.
        """
        if '_singleton' not in vars(cls):
            cls._singleton = super(Module, cls).__new__(cls, *args, **kwargs)
            if cls.__init__ != Module.__tmpinit:
                cls.__oldinit = cls.__init__
                cls.__init__ = Module.__tmpinit
        return cls._singleton

    def __tmpinit(self, *args, **kwargs):
        """
        Temporary replacement for __init__ to ensure it is only called once.
        """
        if self.__oldinit:
            self.__oldinit(*args, **kwargs)
            self.__oldinit = None

    #########################################
    # Hooks
    #########################################

    def __connect_hook(self, hook_name):
        """
        Connects a method of the object with a hook, and ensure the function
        will be called without the hook name as a first parameter.
        """
        pref = getattr(self, hook_name, None)  # a bound method
        if pref:
            def internal(*args, **kwargs):
                if args:
                    hook = args[0]
                    args = args[1:]
                    if hook == hook_name:
                        return pref(*args, **kwargs)
                    else:
                        return pref(hook, *args, **kwargs)
                else:
                    return pref(*args, **kwargs)
            setattr(self, "__%s" % hook_name, internal)
            p = getattr(self, "__%s" % hook_name)
            if hook_name == "context_changed":
                GPS.Hook(hook_name).add_debounce(p)
            else:
                GPS.Hook(hook_name).add(p)

            # No need to call internal() when the hook is gps_started: this
            # function __connect_hook is called as part of running gps_started
            # so any function we just added to the hook will also be run later
            # on.
            #   if Module_Metaclass.gps_started and hook_name == "gps_started":
            #       p()

    def __connect_hooks(self):
        """
        Connect special methods with the corresponding hooks.
        The alternative is to use gps_utils.hook decorator.
        As opposed to that decorator though, the functions here are called
        without the hook name as a first parameter.
        """
        for h in self.auto_connect_hooks:
            self.__connect_hook(h)

    def __disconnect_hook(self, hook_name):
        """
        Disconnect a hook connected with __connect_hook.
        """
        p = getattr(self, "__%s" % hook_name, None)
        if p:
            GPS.Hook(hook_name).remove(p)
        p = getattr(self, hook_name, None)
        if p:
            GPS.Hook(hook_name).remove(p)

    #########################################
    # Views
    #########################################

    def _setup(self):
        """
        Internal version of setup
        """

        self.__connect_hooks()
        if not self.view_title:
            self.view_title = self.__class__.__name__.replace("_", " ")

        try:
            self.setup()

        except Exception as e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            GPS.Logger('MODULES').log('While loading module: %s' % (e, ))
            GPS.Console("Messages").write(
                "warning: could not load module %s\n" % self.name())
            GPS.Console("Messages").write(
                "%s\n" % '\n'.join(
                    traceback.format_exception(exc_type, exc_value,
                                               exc_traceback)))

        # Catch "gps_started" implementation in legacy or user-defined plugins
        if hasattr(self, "gps_started"):
            GPS.Console("Messages").write(
                "warning: Python module '%s' defines class '%s' with a"
                " method 'gps_started': this is no longer supported: instead,"
                " you should override 'setup'\n." % (
                    self.__module__, self.__class__.__name__))

    def _teardown(self):
        for h in self.auto_connect_hooks:
            self.__disconnect_hook(h)
        self.teardown()

    def name(self):
        """
        Return the name of the module
        """
        return "%s.%s" % (self.__class__.__module__, self.__class__.__name__)

    def _save_desktop(self, child):
        return (self.name(), self.save_desktop(child) or "")

    def _load_desktop(self, name, data):
        if name == self.name():
            try:
                c = self.load_desktop(data)
                if not c:
                    # Default behavior
                    c = self.get_child(allow_create=True)
                return c
            except Exception as e:
                GPS.Logger('MODULES').log('While loading desktop: %s' % (e, ))

    def _on_view_destroy(self, data):
        self.on_view_destroy()

    def get_child(self, allow_create=True):
        """
        Retrieve an existing view. If none exists and allow_create is True,
        a new one is created by calling create_view, and adding it to the MDI.

        :return: an instance of GPS.MDIWindow
        """

        if self.view_title:
            child = GPS.MDI.get(self.view_title)
            if child:
                child.raise_window()
                return child
            elif allow_create:
                view = self.create_view()
                if view:
                    # The following has no effect if create_view has already
                    # put the view in the MDI. This means that save_desktop
                    # will not be called unless create_view has done the
                    # necessary setup.
                    child = GPS.MDI.add(
                        view,
                        position=self.mdi_position,
                        group=self.mdi_group,
                        title=self.view_title,
                        save_desktop=self._save_desktop,
                        flags=self.mdi_flags)
                    child.pywidget().connect("destroy", self._on_view_destroy)
                    return child
        return None

    def get_view(self, allow_create=True):
        """
        Retrieve an existing view. See get_child()

        :return: an instance of Gtk.Widget
        """
        child = self.get_child(allow_create=allow_create)
        if child:
            return child.get_child()
        return None

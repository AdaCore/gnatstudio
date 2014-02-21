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
        return box
"""


import GPS
from gi.repository import GLib


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
                GLib.idle_add(lambda: new_class()._setup())

        return new_class

    @staticmethod
    def setup_all_modules(hook):
        if not Module_Metaclass.gps_started:
            Module_Metaclass.gps_started = True
            for ModuleClass in Module_Metaclass.modules:
                module_instance = ModuleClass()
                Module_Metaclass.modules_instances.append(module_instance)
                module_instance._setup()

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
        "gps_started",
        "project_view_changed",
        "preferences_changed",
        "file_edited",
        "project_changed",   # not called for the initial project
        "compilation_finished")
    auto_connect_hooks_with_no_hook_arg = (
        "task_started",
        "task_changed",
        "task_terminated")
    # As a special case, if you class has a subprogram with a name of any
    # of the hooks below, that function will automatically be connected to
    # the hook (and disconnected when the module is teared down.

    mdi_position = GPS.MDI.POSITION_BOTTOM
    # the initial position of the window in the MDI (see GPS.MDI.add)

    mdi_group = GPS.MDI.GROUP_CONSOLES
    # the group for this window. This is used in case the user has already
    # created windows in this group, and in this case the new view will be
    # put on top of the existing windows.

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

    def save_desktop(self, view):
        """
        Returns the data to use when saving a view in the desktop. The
        returned value will be passed to load_desktop the next time GPS is
        started.

        :param view: an instance of GPS.MDIWindow matching the view you created
           and put in the MDI. Use view.get_child to get access to the
           actual widget.
        """
        return ""

    def load_desktop(self, child, data):
        """
        This function is called when loading a widget from the desktop. It
        receives a newly created widget, and the data returned by
        save_desktop() that can be used to initialize the widget.

        :param child: a newly created instance of GPS.MDIWindow, that
            contains the view created by self.create_view()
        :param data: a string, as returned by save_desktop
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
        will be called without the hook as a first parameter.
        """
        pref = getattr(self, hook_name, None)  # a bound method
        if pref:
            def internal(hook, *args, **kwargs):
                return pref(*args, **kwargs)
            setattr(self, "__%s" % hook_name, internal)
            GPS.Hook(hook_name).add(getattr(self, "__%s" % hook_name))

    def __connect_hook_with_no_hook_arg(self, hook_name):
        """
        Connects a method of the object with a hook, and ensure the function
        will be called without the hook as a first parameter.
        """
        pref = getattr(self, hook_name, None)  # a bound method
        if pref:
            GPS.Hook(hook_name).add(pref)

    def __disconnect_hook(self, hook_name):
        """
        Disconnect a hook connected with __connect_hook.
        """
        p = getattr(self, "__%s" % hook_name, None)
        if p:
            GPS.Hook(hook_name).remove(p)

    def __disconnect_hook(self, hook_name):
        """
        Disconnect a hook connected with __connect_hook.
        """
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

        if not self.view_title:
            self.view_title = self.__class__.__name__.replace("_", " ")
        for h in self.auto_connect_hooks:
            self.__connect_hook(h)
        for h in self.auto_connect_hooks_with_no_hook_arg:
            self.__connect_hook_with_no_hook_arg(h)
        self.setup()

    def _teardown(self):
        for h in self.auto_connect_hooks:
            self.__disconnect_hook(h)
        for h in self.auto_connect_hooks_with_no_hook_arg:
            self.__disconnect_hook_with_no_hook_arg(h)
        self.teardown()

    def _save_desktop(self, child):
        return ("%s.%s" % (self.__class__.__module__,
                           self.__class__.__name__),
                self.save_desktop(child) or "")

    def _load_desktop(self, name, data):
        if name == "%s.%s" % (self.__class__.__module__,
                              self.__class__.__name__):
            child = self.get_child(allow_create=True)
            self.load_desktop(child, data)
            return child

    def get_child(self, allow_create=True):
        """
        Retrieve an existing view. If none exists and allow_create is True,
        a new one is created by calling create_view, and adding it to the MDI.

        :return: an instance of GPS.MDIWindow
        """

        # ??? Should store the view directly in self, but we need to monitor
        # its destruction.
        if self.view_title:
            child = GPS.MDI.get(self.view_title)
            if child:
                child.raise_window()
                return child
            elif allow_create:
                view = self.create_view()
                if view:
                    child = GPS.MDI.add(
                        view,
                        position=self.mdi_position,
                        group=self.mdi_group,
                        title=self.view_title,
                        save_desktop=self._save_desktop)
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

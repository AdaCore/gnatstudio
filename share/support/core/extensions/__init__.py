import GPS
#########################################
# Decorators and auto submodules import #
#########################################


class extend_module:
    """
    A decorator that should be used on a class.
    All methods in this class will override the methods for the
    homonym class in module.
    For instance:

        @extend_module(GPS)
        class File:
            def a_new_method(self):
                # A new method
                pass

            @override_gps_method
            def project(self):
                # Overrides, but call GPS-exported method
                return self._internal_project()

    """

    def __init__(self, module):
        """
        :type module: The module that contains the class to extend
        """
        self.module = module

    def __call__(self, kls):
        """
        :type kls: The type to extend
        """
        class_name = kls.__name__
        orig_class = getattr(self.module, class_name, None)

        if not orig_class:
            setattr(self.module, class_name, kls)
        else:
            for name, method in kls.__dict__.iteritems():
                # Do not override built-in methods
                if name in ("__module__", "__dict__", "__doc__",
                            "__weakref__"):
                    continue

                is_override = hasattr(method, "override_gps_method")

                if not hasattr(orig_class, name):
                    if is_override:
                        GPS.Console().write(
                            "Method %s.%s is not overriding\n" % (
                                class_name, name))
                    setattr(orig_class, name, method)

                else:
                    if not is_override:
                        GPS.Console().write(
                            "Method %s.%s is overriding, ignored\n" % (
                                class_name, name))
                    else:
                        # Save the gps internal method (can be useful to call
                        # it in the new implementation for example)

                        original_method = getattr(orig_class, name)
                        backup_name = "_internal_" + name
                        setattr(orig_class, backup_name, original_method)

                        # Add the new method
                        setattr(orig_class, name, method)

        return kls


def extend_gps(kls):
    """
    Extends classes from the GPS module.
    See `extend_module` for more information.
    """
    return extend_module(GPS)(kls)


def override_gps_method(method):
    """
    For use with the `extend_module` or `extend_gps` decorators.
    This should be used on methods that override the ones exported
    by GPS.
    """
    method.override_gps_method = True
    return method

############
# Contexts #
############

GPS.FileContext = GPS.Context
GPS.AreaContext = GPS.Context
GPS.EntityContext = GPS.Context
GPS.MessageContext = GPS.Context


######################
# General extensions #
######################

@extend_gps
class EditorView(object):

    def set_extend_selection(self, extend):
        """
        See :func:`GPS.EditorBuffer.extend_existing_selection`
        """
        self.buffer().extend_existing_selection = extend

    def get_extend_selection(self):
        """
        See :func:`GPS.EditorBuffer.extend_existing_selection`
        """
        return self.buffer().extend_existing_selection


class _UndoRedoContext(object):
    """Helper class to implement an undo/redo context manager.
    """
    def __init__(self, buffer):
        self.buffer = buffer

    def __enter__(self):
        self.buffer._start_undo_group()

    def __exit__(self, type, exc, tb):
        self.buffer._finish_undo_group()


@extend_gps
class EditorBuffer(object):

    __warned_about_undo_redo = False
    # Whether we have already warned about the deprecated undo grouping API

    def insert(self, loc_or_text, text=None):
        """
        Inserts some text in the buffer.

        :param EditorLocation loc_or_text: Either where to insert the text,
            or the text to insert in the buffer
        :type loc_or_text: string|EditorLocation

        :param string text: If the first passed parameter was a location,
            this is the text to be inserted. Else, it can be ignored.
        :type text: string|None

        .. seealso:: :func:`GPS.EditorBuffer.delete`
        """
        if isinstance(loc_or_text, GPS.EditorLocation):
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(loc_or_text, text)
        else:
            text = loc_or_text
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(self.current_view().cursor(),
                                     loc_or_text)

    def entity_under_cursor(self):
        """
        Shortcut to return a :class:`GPS.Entity` instance corresponding to the
        entity under cursor

        :rtype: :class:`GPS.Entity`
        """
        return self.main_cursor().location().entity()

    def start_undo_group(self):
        """This is deprecated. Use GPS.EditorBuffer.new_undo_group

           This is done via a context manager:

                with buffer.new_undo_group():
                    action 1
                    ...
                    action N
        """
        if not GPS.EditorBuffer.__warned_about_undo_redo:
            GPS.EditorBuffer.__warned_about_undo_redo = True
            GPS.Console().write(
                "GPS.EditorBuffer.start_undo_group is deprecated:"
                " use GPS.EditorBuffer.new_undo_group instead:\n\n")
            GPS.Console().write(self.new_undo_group.__doc__)
        self._start_undo_group()
        if hasattr(self, "undo_group"):
            self.undo_group += 1
        else:
            self.undo_group = 1

    def finish_undo_group(self):
        """This is deprecated, use GPS.EditorBuffer.new_undo_group
        """
        global warned_about_undo_redo
        if not warned_about_undo_redo:
            warned_about_undo_redo = True
            GPS.Console().write(
                "GPS.EditorBuffer.finish_undo_group is deprecated:"
                " use GPS.EditorBuffer.new_undo_group instead.\n")
        if not hasattr(self, "undo_group"):
            GPS.Console().write(
                "Error: 'finish_undo_group' not matching 'start_undo_group'\n")
        else:
            self.undo_group -= 1
            if self.undo_group >= 0:
                self._finish_undo_group()
            else:
                GPS.Console().write(
                    "Error: more calls to 'finish_undo_group'"
                    " than to 'start_undo_group'\n")

    def new_undo_group(self):
        """Create a new undo group.

           This returns an object which should be used as a context manager.
           If you would like N actions to be considered as atomic for
           undo/redo, use this:

                 with buffer.new_undo_group():
                     action 1
                     ...
                     action N
        """
        return _UndoRedoContext(self)


@extend_gps
class Cursor(object):
    def location(self):
        """
        Returns the cursor's location
        :rtype: :class:`GPS.EditorLocation`
        """
        return self.mark().location()


@extend_gps
class EditorLocation(object):
    def get_word(self):
        """
        This will return the word that contains this location, if there
        is one, the empty string otherwise. This is a shortcut method that uses
        the inside_word, starts_word and ends_word methods of
        `GPS.EditorLocation`.

        :returns: A tuple (word, start location, end location)
        :rtype: (unicode,
                 :class:`GPS.EditorLocation`, :class:`GPS.EditorLocation`)
        """

        word = ""
        start_loc = None
        end_loc = None

        if self.inside_word():
            start_loc = self
            while not start_loc.starts_word():
                start_loc = start_loc.forward_char(-1)

            end_loc = self
            while not end_loc.ends_word():
                end_loc = end_loc.forward_char()

            word = self.buffer().get_chars(start_loc, end_loc).strip()
            word = word.decode("utf8")  # make unicode-string

        return word, start_loc, end_loc

    def entity(self):
        """
        Returns a :class:`GPS.Entity` instance at the given location.

        If there is no entity that can be resolved at this location,
        returns None

        :rtype: :class:`GPS.Entity`
        """
        try:
            word, start_loc, end_loc = self.get_word()
            return GPS.Entity(word, self.buffer().file(),
                              start_loc.line(), start_loc.column())
        except Exception:
            return None


@extend_gps
class BuildTarget(object):

    @override_gps_method
    def __init__(self, name):
        self.target_name = name
        self._internal___init__(name)

    @override_gps_method
    def execute(self, main_name='', file=None, force=False,
                extra_args='', build_mode='', synchronous=True,
                directory='', quiet=False, on_exit=None):

        import workflows as wf

        if self.target_name in wf.workflows_target_name_set:
            idt = (self.target_name, main_name)
            if idt in wf.exit_handlers_table:
                GPS.Logger("BUILDTARGET").log(
                    "Workflow {} already in execution".format(idt))
                return
            wf.exit_handlers_table[idt] = on_exit
            on_exit = None

        # Call the internal execute with given parameters
        self._internal_execute(
            main_name, file, force, extra_args, build_mode,
            synchronous, directory, quiet, on_exit)


@extend_gps
class Language(object):

    @override_gps_method
    def __init__(self):
        """
        This constructor is provided to prevent the initialisation of any
        object of the Language class, because it is abstract. The
        consequence of this is that subclassers of Language must reimplement
        __init__ to avoid having an exception raised at instance construction
        time
        """
        raise NotImplementedError


@extend_gps
class Libclang(object):

    @staticmethod
    def get_translation_unit(file, project=None):
        """
        Returns the clang translation unit corresponding to this file. You can
        use that as a full libclang translation unit.

        :param GPS.File file: The file to get the translation unit for
        :rtype: clang.cindex.TranslationUnit
        """
        from clang.cindex import TranslationUnit, c_object_p, Index
        from ctypes import cast

        project = project or file.project()
        tu_ptr, index_ptr = GPS.Libclang._get_translation_unit(file, project)

        # If tu_ptr or index_ptr are 0, then we return nothing
        if tu_ptr and index_ptr:
            return TranslationUnit(
                cast(tu_ptr, c_object_p), Index(cast(index_ptr, c_object_p))
            )


@extend_gps
class Contextual(object):

    def create(
            self, on_activate, label=None, filter=None, ref='',
            add_before=True,
            group='0', visibility_filter=None, action=None):
        """
        OBSOLETE: please use GPS.Action.contextual() instead. All contextual
        menus should be associated with a specific action, so that this action
        can also be associated with a key shortcut, or reused in toolbars,...

        Creates a new contextual menu entry.  Whenever this menu entry is
        selected by the user, GPS executes :func:`on_activate`, passing one
        parameter which is the context for which the menu is displayed (this
        is usually the same as :func:`GPS.current_contextual`).

        If ``on_activate`` is None, a separator is created.

        The ``filter`` parameter can be used to filter when the entry should
        be displayed in the menu. It is a function that receives one
        parameter, an instance of :class:`GPS.Context`, and returns a
        boolean. If it returns True, the entry is displayed, otherwise it is
        hidden.

        The ``label`` parameter can be used to control the text displayed in
        the contextual menu.  By default, it is the same as the contextual
        name (used in the constructor to :func:`GPS.Contextual.__init__`).
        If specified, it must be a subprogram that takes an instance of
        :class:`GPS.Context` in a parameter and returns a string, which is
        displayed in the menu.

        The parameters ``group``, ``ref`` and ``add_before`` can be used to
        control the location of the entry within the contextual
        menu. ``group`` allows you to create groups of contextual menus that
        will be put together.  Items of the same group appear before all
        items with a greater group number.  ``ref`` is the name of another
        contextual menu entry, and add_before indicates whether the new entry
        is put before or after that second entry.

        :param self: An instance of GPS.Contextual
        :param on_activate: A subprogram with one parameter context
        :param label: A subprogram
        :param ref: A string
        :param add_before: A boolean
        :param filter: A subprogram
        :param group: An integer
        :param GPS.Action action: An action instance to be executed on menu
            activation

        .. code-block:: python

           ## This example demonstrates how to create a contextual
           ## menu with global functions

           def on_contextual(context):
              GPS.Console("Messages").write("You selected the custom entry")

           def on_filter(context):
              return context.entity_name() is not None

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
                 return context.entity_name() is not None

              def on_label(self, context):
                 return self.data

              def __init__(self):
                 GPS.Contextual.__init__(self, "Custom")
                 self.data = "Menu Name"
                 self.create(on_activate=self.on_contextual,
                             filter=self.on_filter,
                             label=self.on_label)
        """

        # Unless we are creating a separator
        if not label or not label.endswith('-'):
            GPS.Console().write(
                'GPS.Contextual("%s").create is deprecated.' % self.name +
                ' Please use GPS.Action.contextual()\n')
            if not action:
                # Create a dummy action
                action = GPS.Action('__%s' % self.name)
                action.create(
                    on_activate=lambda: on_activate(GPS.contextual_context()),
                    category='',   # hidden
                    filter=filter)

            # Unused parameters: 'group' and 'visibility_filter'
            action.contextual(
                path=label or self.name,
                ref=ref,
                add_before=add_before)

        else:
            action = GPS.Action('__separator')
            action.contextual(
                path=label or self.name,
                ref=ref,
                add_before=add_before)


@extend_gps
class Menu(GPS.GUI):

    def set_sensitive(self, sensitive=True):
        """Disable the action associated with the menu"""
        self.action.disable(not sensitive)

    def destroy(self):
        """
        Remove the menu and all other graphical elements linked to
        the same action.
        """
        self.action.destroy_ui()

    def hide(self):
        """Disable the action associated with the menu"""
        self.action.disable(True)

    def show(self):
        """Enable the action associated with the menu"""
        self.action.disable(False)

    def pywidget(self):
        GPS.Console().write(
            "GPS.Menu.pywidget is no longer supported." +
            " Use GPS.Menu.action to interacte with the action" +
            " directly")

    @staticmethod
    def create(
            path, on_activate='', ref='', add_before=True,
            filter=None, group=''):
        """
        Creates a new menu in the GPS system. The menu is added at the given
        location (see :func:`GPS.Menu.get` for more information on the
        ``path`` parameter). Submenus are created as necessary so ``path``
        is valid.

        It is recommended now to use :class:`gps_utils.interactive`
        instead of creating menus explicitly. The latter creates GPS
        actions, to which keybindings can be associated with the
        user. They can also be executed more conveniently using
        keyboard only with the omni-search.

        If ``on_activate`` is specified, it is executed every time the user
        selects that menu. It is called with only one parameter, the instance
        of :class:`GPS.Menu` that was just created.

        If ``ref`` and ``add_before`` are specified, they specify the name of
        another item in the parent menu (and not a full path) before or after
        which the new menu should be added.

        If the name of the menu starts with a '-' sign, as in "/Edit/-", a
        menu separator is inserted instead. In this case, on_activate is
        ignored.

        Underscore characters ('_') need to be duplicated in the path. A
        single underscore indicates the mnemonic to be used for that
        menu. For example, if you create the menu "/_File", then the user can
        open the menu by pressing :kbd:`Alt-f`. But the underscore itself
        is not be displayed in the name of the menu.

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

        if on_activate:
            GPS.Console().write(
                'GPS.Menu.create("%s") is deprecated.' % path +
                ' Please use gps_utils.interactive()\n')
            # Ignore 'group'
            m = None
            a = GPS.Action(path)
            a.create(lambda: on_activate(m),
                     filter=filter)
            m = a.menu(path, add_before=add_before, ref=ref)


@extend_gps
class ToolButton(GPS.GUI):
    """Represents a Button that can be placed in the Toolbar.

       This class is provided for backwards compatibility only.
       Instead of using this, use GPS.Action and GPS.Action.button().
    """

    def __init__(self, stock_id, label, on_click):
        """This class is provided for backwards compatibility only.

           Instead of using this, use GPS.Action and GPS.Action.button().
        """
        self.action_name = "ToolButton {}:{}".format(label, on_click.__name__)
        self.on_click = on_click
        self.stock_id = stock_id


@extend_gps
class Button(GPS.GUI):
    """Represents a Button that can be placed in the Toolbar.

       This class is provided for backwards compatibility only.
       Instead of using this, use GPS.Action and GPS.Action.button().
    """

    def __init__(self, id, label, on_click):
        """This class is provided for backwards compatibility only.

           Instead of using this, use GPS.Action and GPS.Action.button().
        """
        self.action_name = label
        self.on_click = on_click
        self.stock_id = ""

    def set_text(self, label):
        """Set the text of the button"""
        self.action_name = label


@extend_gps
class Toolbar(GPS.GUI):
    """The main GPS Toolbar.

       This class is provided for backwards compatibility only.
       Instead of using this, use GPS.Action and GPS.Action.button().
    """

    def __init__(self):
        """This class is provided for backwards compatibility only.

           Instead of using this, use GPS.Action and GPS.Action.button().
        """
        pass

    def append(self, widget, tooltip=''):
        """Append a widget to the Toolbar.

           The widget must be either a GPS.ToolButton or a GPS.Button."""
        self.action = GPS.Action(widget.action_name)
        self.action.create(on_activate=lambda: widget.on_click(widget),
                           filter="",
                           category="Custom ToolButtons",
                           description=tooltip,
                           icon=widget.stock_id)
        self.action.button()

    def insert(self, widget, pos, tooltip):
        """Append a widget to the Toolbar.

           To insert a widget at a specific position in the toolbar, use
           GPS.Action.button instead, and set a value to the 'section'
           parameter.
        """
        GPS.Console("Messages").write(
            "GPS.Toolbar.insert() is deprecated: use GPS.Action.button.\n"
        )
        self.append(widget, tooltip)

    def get(self, id):
        """Does nothing and writes an error message

           This function is to help detect compatibility errors in plugins.
        """
        GPS.Console("Messages").write("GPS.Toolbar.get() is deprecated.\n")

    def get_by_pos(self, pos):
        """Does nothing and writes an error message

           This function is to help detect compatibility errors in plugins.
        """
        GPS.Console("Messages").write(
            "GPS.Toolbar.get_by_pos() is deprecated.\n"
        )

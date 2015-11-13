from GPS import *
import GPS
import gps_utils

#########################################
# Decorators and auto submodules import #
#########################################


def extend_gps(kls):
    """
    :type kls: type
    """
    class_name = kls.__name__
    gps_class = getattr(GPS, class_name, None)

    if not gps_class:
        setattr(GPS, class_name, kls)
    else:
        for name, method in kls.__dict__.iteritems():
            if not hasattr(gps_class, name):
                setattr(gps_class, name, method)
            elif hasattr(method, "override_gps_method"):
                # Save the gps internal method (can be useful to call it in the
                # new implementation for example)
                original_method = getattr(gps_class, name)
                backup_name = "_internal_" + name
                setattr(gps_class, backup_name, original_method)

                # Add the new method
                setattr(gps_class, name, method)

    return kls


def override_gps_method(method):
    method.override_gps_method = True
    return method


######################
# General extensions #
######################

@extend_gps
class EditorBuffer(object):

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
                    "Workflow {} already in execution".format(idt)
                )
                return
            wf.exit_handlers_table[idt] = on_exit
            on_exit = None

        # Call the internal execute with given parameters
        self._internal_execute(
            main_name, file, force, extra_args, build_mode,
            synchronous, directory, quiet, on_exit
        )


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
class Contextual(object):

    @override_gps_method
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

    @override_gps_method
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

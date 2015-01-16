__author__ = 'amiard'
from extensions.private.xml import X
from GPS import *
import GPS

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
            if (hasattr(method, "override_gps_method")
                    or not hasattr(gps_class, name)):
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
        if isinstance(loc_or_text, EditorLocation):
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(loc_or_text, text)
        else:
            text = loc_or_text
            assert isinstance(text, str) or isinstance(text, unicode)
            self._insert_at_location(self.current_view().cursor(),
                                     loc_or_text)


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

        GPS.Console().write(
            'GPS.Contextual("%s").create is deprecated.' % self.name +
            ' Please use GPS.Action.contextual()\n')

        if not action:
            # Create a dummy action
            action = GPS.Action('__%s' % self.name)
            action.create(
                on_activate=lambda: on_activate(GPS.current_context()),
                category='',   # hidden
                filter=filter)

        # Unused parameters: 'group' and 'visibility_filter'
        action.contextual(
            path=label or self.name,
            ref=ref,
            add_before=add_before)

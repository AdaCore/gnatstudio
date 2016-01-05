"""List all primitive operations/methods of current entity

This script adds a contextual menu when you click on an Ada tagged
type or C++ class (or an instance of these).
The submenu will list all the primitive operations (aka methods) of
that object, and clicking on any of these will jump to the body of
that operation.

Worth noting in this example is the way the list of methods is
computed only once, and then stored in the context for later
reuse.
Also worth noting is how a dynamic contextual menu is created, and
can be set up to properly handle overloaded submenu entries, as might
happen when several methods have the same name.
"""


############################################################################
# No user customization below this line
############################################################################

import GPS
from gps_utils import hook


class Methods_Contextual (GPS.Contextual):

    def __init__(self):
        GPS.Contextual.__init__(self, "Methods")
        self.create_dynamic(on_activate=self.on_activate,
                            label="References/Methods of %e",
                            filter=self.filter,
                            factory=self.factory)

    def filter(self, context):
        # Store the methods in the context, so that we do not have to
        # recompute them if the menu is selected, and so that we can
        # handle overridden methods as well
        if context.entity_name() is not None and context.entity():
            context.methods_list = context.entity().methods(
                include_inherited=True)

            # if we have an access to a tagged type, behave as if we had the
            # type itself
            if context.methods_list == [] \
               and context.entity().pointed_type():
                context.methods_list = context.entity(
                ).pointed_type().methods()

            return context.methods_list != []
        else:
            return False

    def factory(self, context):
        own = set(context.entity().methods())  # overridden methods
        context.methods_list.sort()
        result = []
        for m in context.methods_list:
            if m in own:
                result.append("%s" % m.name())
            else:
                result.append("%s (inherited)" % m.name())
        return result

    def on_activate(self, context, choice, choice_index):
        decl = context.methods_list[choice_index].body()
        buffer = GPS.EditorBuffer.get(decl.file())
        buffer.current_view().goto(
            buffer.at(decl.line(), decl.column()))


@hook('gps_started')
def __on_gps_started():
    Methods_Contextual()

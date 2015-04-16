"""
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
from gps_utils import hook

# Named constants
LABEL = 0
ACTION = 1
SEPARATOR = {ACTION: "", LABEL: ""}


last_menu = "Version Control/VCS Reference"
last_explorer_menu = "Edit file"


class Labeller(object):

    """ This object is used to give a dynamic name to contextual menu items.
    """

    def __init__(self, label):
        self.label = label

        def labeller(context):
            if context.module_name.startswith("VCS"):
                return "%s" % self.label.replace("_", "")
            else:
                return "Version Control/%s" % self.label.replace("_", "")

        self.labeller = labeller


class Launcher(object):

    """ This object is used to launch GPS actions.
    """

    def __init__(self, action):
        self.action = action

        def launcher(context):
            GPS.execute_action(self.action)

        self.launcher = launcher


def only_in_submenu(context):
    """ Return True if items for this context should be in the
        'Version Control' submenu. """

    if type(context) not in [GPS.FileContext,
                             GPS.EntityContext, GPS.AreaContext]:
        return False

    try:
        file = context.file()

        if GPS.File(context.directory()) == file:
            return False

        if file.name().endswith("$log"):
            return False
    except:
        return False

    return not context.module_name.startswith("VCS_Explorer")


def only_in_explorer(context):
    """ Return True if the context is a VCS explorer. """

    if type(context) not in [GPS.FileContext, GPS.EntityContext]:
        return False

    return context.module_name.startswith("VCS_Explorer")


def not_in_explorer(context):
    """ Return True if the context is not VCS explorer. """

    return not only_in_explorer(context)

global_vcs_menus = []
contextual_menu_labels = {}

actions_via_revision_log = ["Commit"]


def create_menus(system_name, actions):
    global global_vcs_menus
    global contextual_menu_labels

    need_to_create_contextual_menus = system_name not in contextual_menu_labels

    if need_to_create_contextual_menus:
        contextual_menu_labels[system_name] = []

    separator_count = 0
    for dict in actions:
        label = dict[LABEL]
        action_name = dict[ACTION]

        if action_name:
            action = GPS.Action(action_name)

            # Add item in the global menu
            menu = action.menu("/_VCS/%s" % label)

            global_vcs_menus += [menu]

            # Add item in the contextual menu

            contextual_label = "Version Control/%s" % label.replace("_", "")

            if need_to_create_contextual_menus:
                GPS.Contextual(contextual_label).create(
                    on_activate=Launcher(action_name).launcher,
                    ref=last_menu,
                    filter=not_in_explorer,
                    action=action,   # Additional filter
                    add_before=True)

                contextual_menu_labels[system_name] += [contextual_label]
            else:
                GPS.Contextual(contextual_label).show()

            # Add item in the VCS Explorer's contextual menu

            contextual_label = label.replace("_", "")

            if need_to_create_contextual_menus:
                GPS.Contextual(contextual_label).create(
                    on_activate=Launcher(action_name).launcher,
                    ref=last_explorer_menu,
                    action=action,   # Additional filter
                    filter=only_in_explorer,
                    add_before=True)

                contextual_menu_labels[system_name] += [contextual_label]
            else:
                GPS.Contextual(contextual_label).show()

        else:

            separator_count += 1

            # Add a separator in the global menu
            menu = GPS.Menu.create(
                "/_VCS/%s-separator%s" % (label, separator_count))

            global_vcs_menus += [menu]

            # Add a separator in the contextual menu

            label = "%s-%s" % (label.replace("_", ""), separator_count)

            # Add a separator in the VCS menu

            contextual_label = "Version Control/" + label

            if need_to_create_contextual_menus:
                GPS.Contextual(contextual_label).create(
                    on_activate=None,
                    filter=only_in_submenu,
                    ref=last_menu,
                    add_before=True)

                contextual_menu_labels[system_name] += [contextual_label]
            else:
                GPS.Contextual(contextual_label).show()

            # Add a separator in the VCS explorer's menu

            contextual_label = label

            if need_to_create_contextual_menus:
                GPS.Contextual(contextual_label).create(
                    on_activate=None,
                    filter=only_in_explorer,
                    ref=last_explorer_menu,
                    add_before=True)

                contextual_menu_labels[system_name] += [contextual_label]
            else:
                GPS.Contextual(contextual_label).show()


def remove_old_menus(system_name):
    global global_vcs_menus

    if system_name in contextual_menu_labels:
        # Remove old menus
        for m in global_vcs_menus:
            m.destroy()

        global_vcs_menus = []

        # Hide contextual menus
        for m in contextual_menu_labels[system_name]:
            GPS.Contextual(m).hide()

registered_vcs_actions = {}


def register_vcs_actions(system_name, actions):
    """ Associate actions to the given VCS system.
          actions is a list of dictionaries of the form
            { ACTION: <name of the VCS action>, LABEL: <menu label> }
    """
    global registered_vcs_actions
    registered_vcs_actions[system_name] = actions

old_name = ""


@hook("project_view_changed")
def __on_project_changed():
    global old_name
    # First remove/hide old menus
    remove_old_menus(old_name)

    name = GPS.VCS.get_current_vcs()
    old_name = name

    # Then recreate the menus for the current VCS, if any.
    if name in registered_vcs_actions:
        create_menus(name, registered_vcs_actions[name])

    GPS.Contextual(last_menu).hide()

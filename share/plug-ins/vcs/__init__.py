"""
"""

############################################################################
# No user customization below this line
############################################################################

# Do nothing if old vcs is not loader
import GPS
if hasattr(GPS, "VCS"):
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

        try:
            file = context.file()

            if GPS.File(context.directory()) == file:
                return False

            if file.path.endswith("$log"):
                return False

            return not context.module_name.startswith("VCS_Explorer")

        except:
            return False

    def only_in_explorer(context):
        """ Return True if the context is a VCS explorer. """

        return context.file() is not None and \
            context.module_name.startswith("VCS_Explorer")

    def not_in_explorer(context):
        """ Return True if the context is not VCS explorer. """

        return not only_in_explorer(context)

    global_vcs_menus = []    # list of menu items
    contextual_menu_labels = {}

    actions_via_revision_log = ["Commit"]

    def create_menus(system_name, actions):
        global global_vcs_menus
        global contextual_menu_labels

        need_to_create_contextual_menus = \
            system_name not in contextual_menu_labels
        need_to_create_menus = len(global_vcs_menus) == 0

        if need_to_create_contextual_menus:
            contextual_menu_labels[system_name] = []

        separator_count = 0
        for dict in actions:
            label = dict[LABEL]
            action_name = dict[ACTION]

            if action_name:
                action = GPS.Action(action_name)

                # Add item in the global menu
                if need_to_create_menus:
                    menu = action.menu("/_VCS/%s" % label)
                    global_vcs_menus.append(menu)

                # Add item in the contextual menu

                contextual_label = "Version Control/%s" % label.replace(
                    "_", "")

                if need_to_create_contextual_menus:
                    action.contextual(
                        path=contextual_label,
                        # filter=not_in_explorer,
                        # ref=last_menu,
                        add_before=True)

                    contextual_menu_labels[system_name] += [contextual_label]
                else:
                    GPS.Contextual(contextual_label).show()

            else:

                separator_count += 1

                # Add a separator in the global menu
                a = GPS.Action('__separator')
                menu = a.menu('/VCS/-%s-' % (separator_count, ))
                global_vcs_menus.append(menu)

                # Add a separator in the contextual menu

                if need_to_create_contextual_menus:
                    GPS.Contextual('').create(
                        on_activate=None,
                        label='Version Control/-',
                        #  filter=only_in_submenu,
                        ref=last_menu,
                        add_before=True)

    def remove_old_menus(system_name):
        global global_vcs_menus

        if system_name in contextual_menu_labels:
            # Remove old menus
            for menu in global_vcs_menus:
                menu.destroy()

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

        # GPS.Contextual(last_menu).hide()

    from . import clearcase
    from . import cvs
    from . import git
    from . import mercurial
    from . import subversion

"""This provides the Help/GNAT Runtime menu"""

import os
import re
import time
import GPS
from modules import Module

R = re.compile("^(generic )?(function|package|procedure) ([^\s]+).*$")
# How to get the unit name

ALLOWED_PACKAGES = ["Ada", "GNAT", "Interfaces", "System"]
# The toplevel packages that we want to list in the menu

SLICE_MAX_TIME = 0.09
# Seconds after which to stop processing and go back to the main loop


class RuntimeAction(GPS.Action):
    """Represents an action to open a runtime file"""

    def __init__(self, file):
        GPS.Action.__init__(self, 'open runtime file {}'.format(file.name()))
        self.file = file

        self.create(
            on_activate=self.on_activate,
            filter='',
            category='Runtime ({})'.format(GPS.get_runtime() or 'native'),
            description='Open runtime file {}'.format(file.name()))

    def on_activate(self):
        GPS.EditorBuffer.get(self.file, force=True, open=True)


class RuntimeMenu(Module):

    def setup(self):
        self.known_actions = {}
        # The GPS.Actions created by this module, indexed by GPS.File

        self.actions_to_consider = []
        # The list of actions to consider for inclusion in menu

        self.actions_with_a_menu = []
        # The list of GPS.Actions for which a menu has been created

        self.files_to_process = []
        # The files that we have in the queue to process

        self.current_processing = None
        # The source_id of the current background processing

        self._regenerate_runtime_menu()

    def project_view_changed(self):
        self._regenerate_runtime_menu()

    def _on_timeout(self, timeout):
        t0 = time.time()
        while True:
            # Nothing left to process? we're done!
            if not self.files_to_process:
                break

            # Let's process one file
            file = self.files_to_process.pop()
            base = os.path.basename(file.name())

            # Get the associated action
            if file in self.known_actions:
                action = self.known_actions[file]
            else:
                action = RuntimeAction(file)
                self.known_actions[file] = action

            # Find the package name
            # TODO: this is a regexp - maybe we can do better.
            package_name = None
            with open(file.name()) as f:
                for l in f:
                    if (l.startswith('private package') or
                            l.startswith('pragma Compiler_Unit_Warning')):
                        package_name = None
                        break
                    m = R.match(l)
                    if m:
                        package_name = m.group(3)
                        break

            # Add the action  the list of actions_to_consider, if relevant
            if package_name and package_name.split('.')[0] in ALLOWED_PACKAGES:
                action.menu_name = package_name.replace(
                    '.', '/').replace('_', '__')
                self.actions_to_consider.append(action)

            # Took too long time? Let's stop here and come back later.
            if time.time() - t0 > SLICE_MAX_TIME:
                return True

        # We're done! process the menu hierarchy...

        action_for_menu = {a.menu_name: a for a in self.actions_to_consider}
        self.actions_to_consider = []
        keys = action_for_menu.keys()

        # Do this small gymnastics to transform menu
        #       Ada/Containers
        #   to  Ada/Containers/<Containers>
        keys.sort()
        keys.reverse()
        prev = ""
        for k in keys:
            if prev.startswith(k):
                action_for_menu[k].menu_name += "/<{}>".format(
                    action_for_menu[k].menu_name.split('/')[-1])
            prev = k
        keys.reverse()
        for k in keys:
            action = action_for_menu[k]
            action.menu(path='/Help/GNAT Runtime/' + action.menu_name)
            self.actions_with_a_menu.append(action)

        # ... and unregister the timeout
        self.current_processing = None
        return False

    def _regenerate_runtime_menu(self):
        # First, interrupt any current processing
        if self.current_processing:
            self.current_processing.remove()
            self.current_processing = None

        # Remove all actions that have a menu
        for a in self.actions_with_a_menu:
            a.destroy_ui()

        # Get the list of files to process
        self.files_to_process = filter(lambda x: x.name().endswith('.ads'),
                                       GPS.get_runtime_files())

        # Found some files to process? do it in the background
        if self.files_to_process:
            self.current_processing = GPS.Timeout(100, self._on_timeout)

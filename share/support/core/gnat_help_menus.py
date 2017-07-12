"""Generates the help menus for a number of tools found on the PATH"""

import os
import os_utils
import GPS
from modules import Module

# The known doc entries.
# This is a dictionary indexed by the name of the base executable.
#
# For each executable, we provide a dictionary that has:
#    key: the doc description, used as menu label
#    value: a tuple containing
#
#      - the path containing the help main file relative to
#           the executable/../share/doc/
#        This can also be a list of paths, in which case the first path
#        in the list which corresponds to a file on disk is used.
#
#      - the menu path to in GPS, relative to /Help/, in which to create
#        the new menu
#
_DOC_ENTRIES = {
   # GPRbuild
   'gprbuild': {"GPR Tools User's Guide":
                ('gprbuild/html/gprbuild_ug.html', 'GPR/')},
   'gnatls': {
       # Ada RMs
       "Ada 2005 Reference Manual": ('gnat/html/arm05.html', 'Ada/'),
       "Ada 2012 Reference Manual": ('gnat/html/arm12.html', 'Ada/'),
       "Ada 95 Reference Manual": ('gnat/html/arm95.html', 'Ada/'),

       # GNU toolchain
       "Using AS": ('gnat/html/as.html', 'GNU Tools/'),
       "GNU Binary Utilities": ('gnat/html/binutils.html', 'GNU Tools/'),
       "Using GCC": ('gnat/html/gcc.html', 'GNU Tools/'),
       "Using the GNU Debugger": ('gnat/html/gdb.html', 'GNU Tools/'),
       "GNU gprof": ('gnat/html/gprof.html', 'GNU Tools/'),
       "GNU ld": ('gnat/html/ld.html', 'GNU Tools/'),

       # GNAT Native
       "GNAT Reference Manual": ('gnat/html/gnat_rm/gnat_rm.html', 'GNAT/'),
       "GNAT User's Guide for Native Platforms": (
            'gnat/html/gnat_ugn/gnat_ugn.html', 'GNAT/'),

       # GNAT Cross
       "GNAT User's Guide Supplement For Cross Platforms": (
           'gnat-cross/html/gnat_ugx/gnat_ugx.html', 'GNAT/'),
       "GNAT User's Guide Supplement for GNAT Pro Safety-Critical"
       " and GNAT Pro High-Security": (
           'gnat-cross/html/gnathie_ug/gnathie_ug.html', 'GNAT/'),
    },

   # GNATcheck
   'gnatcheck': {"GNATcheck Reference Manual":
                 ('gnat/html/gnatcheck_rm/gnatcheck_rm.html', 'GNAT/')},

   # Spark2c
   'c-gcc': {"SPARK to C User's Guide Supplement":
             ('gnat/html/spark2c/spark2c.html', 'GNAT/')},
}


class HTMLAction(GPS.Action):

    def __init__(self, description, file, menu_path):
        """ Create an action to launch a browser for the specific file """
        GPS.Action.__init__(self, description)
        self.menu_path = menu_path
        self.file = file
        self.create(self.on_activate, filter='', category='Help',
                    description=description)

    def on_activate(self):
        """ Activate the action """
        GPS.HTML.browse('file://{}'.format(self.file))


class GNATMenus(Module):

    def _populate_menu(self):
        """ Populate the Help menu for the AdaCore tools """

        help_actions = []

        for exec_name in _DOC_ENTRIES.keys():
            executable = exec_name
            if exec_name == 'gnatls' and GPS.get_target():
                executable = '{}-gnatls'.format(GPS.get_target())
            ex = os_utils.locate_exec_on_path(executable)
            if ex:
                for descr, tup in _DOC_ENTRIES[exec_name].iteritems():
                    html_files, menu_base = tup
                    menu_path = menu_base + '/' + descr
                    action_descr = 'display documentation {}'.format(descr)

                    # Do not create a menu if the action already exists
                    if GPS.Action(action_descr).exists():
                        continue

                    # As a convenience, html_files can either be a string or a
                    # list of strings. Deal with this here.
                    if type(html_files) != list:
                        html_files = [html_files]

                    for file in html_files:
                        path = os.path.realpath(
                            os.path.join(os.path.dirname(ex),
                                         '..', 'share', 'doc', file)
                        )
                        if os.path.isfile(path):
                            action = HTMLAction(action_descr, path,
                                                '/Help/{}'.format(menu_path))
                            help_actions.append(action)
                            break

        help_actions.sort(key=lambda x: x.menu_path)

        for a in help_actions:
            a.menu(a.menu_path, ref='About', add_before=False)

    def setup(self):
        self._populate_menu()

    def project_view_changed(self):
        self._populate_menu()

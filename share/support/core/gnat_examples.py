import glob
import os
import os_utils
import re
import distutils.dir_util
import GPS
from modules import Module


class ExampleAction(GPS.Action):

    def __init__(self, example_name, directory, gpr_file, readme):
        """ Create an action to load one example in GPS.

            example_name is the name to use for the example,
            directory is the directory containing the expample project,
            gpr_file is the project file to load, and
            readme (if any) the file to edit after project load.
        """
        GPS.Action.__init__(self, 'open example ' + example_name)
        self.gpr_file = gpr_file
        self.readme = readme
        self.example_name = example_name
        self.directory = directory
        self.create(self.on_activate, filter='', category='Help',
                    description='Load project for example ' + example_name)

    def on_activate(self):
        """ Activate the action.

            Ask the user for a destination directory and copy all the files
            contained in the example project's directory in it.
            Load the copied project file and display the README, if any.
        """

        new_proj_dir = str(GPS.MDI.directory_selector())

        distutils.dir_util.copy_tree(self.directory, new_proj_dir)

        new_gpr_file = os.path.join(
            new_proj_dir, os.path.basename(self.gpr_file))
        GPS.Project.load(new_gpr_file)

        if self.readme:
            new_readme_file = os.path.join(
                    new_proj_dir, os.path.basename(self.readme))
            GPS.EditorBuffer.get(GPS.File(new_readme_file))


class GNATExamples(Module):

    def _process_examples_dir(self, submenu_name, example_directory):
        """ Process a directory and place any valid examples found there.

            example_directory is the directory in which to look for examples
            submenu_name is the name of the menu to create beneath
              Help/GNAT
        """

        actions = []
        title_regexp = re.compile('<title>GNAT \&prefix;(.+)</title>')

        for subdir in (glob.glob(os.path.join(example_directory, '*')) +
                       glob.glob(os.path.join(example_directory, '*', '*'))):
            gprs = glob.glob(os.path.join(subdir, '*.gpr'))
            if len(gprs) == 1:
                # There is only one GPR file in this project: we can
                # create the action that opens this.

                # Find the readme
                readmes = glob.glob(os.path.join(subdir, 'README*'))
                readme = None
                if readmes:
                    readme = readmes[0]

                # Find the name of the action, attempting to read it in the
                # xml file
                name = os.path.basename(subdir)
                xmlfile = os.path.join(subdir, name + '.xml')
                if os.path.isfile(xmlfile):
                    with open(xmlfile, 'r') as f:
                        matches = title_regexp.findall(f.read())
                        if len(matches) == 1:
                            name = matches[0]
                actions.append(
                    ExampleAction(name, subdir, gprs[0], readme)
                )
        # sort the actions, and create the menus
        actions.sort(key=lambda x: x.example_name)

        ref_menu = ''
        for a in actions:
            menu_name = "/Help/GNAT/Examples/{}/{}".format(
                submenu_name, a.example_name)
            a.menu(menu_name, ref_menu)
            ref_menu = menu_name

    def _populate_menu(self):
        """ Populate the Help menu with the GNAT examples """

        # For now, we look for the location of "gnat" from the PATH.
        # TODO: get the path from toolchains, if any (this requires
        # exposing the toolchains API to Python, or providing another
        # high-level service).

        # Create the native examples menu, if any
        if "gnat" not in self.menus_created_for_compiler:
            self.menus_created_for_compiler.append("gnat")

            gnat = os_utils.locate_exec_on_path("gnat")
            examples_root = os.path.join(
                os.path.dirname(gnat), "..", "share", "examples", "gnat"
            )
            self._process_examples_dir("Native", examples_root)

        # TODO: support cross gnat. Right now the examples that ship with
        # the cross compilers require massaging with a Makefile, and therefore
        # are not suited to GPS integration.

    def setup(self):
        self.menus_created_for_compiler = []
        # The list of menu paths that have already been created

        self._populate_menu()

    def project_view_changed(self):
        self._populate_menu()

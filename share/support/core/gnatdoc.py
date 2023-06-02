""" Provides integration with gnatdoc.

This provides with a top-level menu to launch GNATdoc, along with the
definition of project attributes.

When this is loaded, the following extra API are available:

    def GPS.Project.generate_doc(self, recursive=False):
        '''
        Generates the documentation for the project (and its subprojects if
        ``recursive`` is True) and displays it in the default browser.

        :param recursive: A boolean
        '''

"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import modules
import os.path
from gs_utils import interactive


project_attributes = """
  <project_attribute
   name="Output_Dir"
   label="Documentation directory"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="In which subdirectory of the object dir to generate the doc"
  >
    <index/>
    <string type="directory" />
  </project_attribute>

  <project_attribute
   name="Resources_Dir"
   label="Resources directory"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="In which subdirectory to lookup alternative resources for doc generation"
  >
    <index/>
    <string type="directory" />
  </project_attribute>

  <project_attribute
   name="Documentation_Pattern"
   label="Documentation pattern"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="Regular expression identifying documentation comments"
  >
    <string />
  </project_attribute>

  <project_attribute
   name="Excluded_Project_Files"
   label="Ignored subprojects"
   list="true"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="List of subprojects to omit from documentation"
  >
    <string />
  </project_attribute>
"""

targets = """
  <target-model name="gnatdoc" category="">
    <description>Run GNATdoc</description>
    <command-line>
      <arg>gnatdoc4</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
    <iconname>gps-syntax-check-symbolic</iconname>
    <switches command="%(tool_name)s" columns="1" lines="3">
       <check
        label="Leading documentation"
        switch="--style=leading"
        column="1"
        tip="Look for documentation located above the code"
       />
       <check
        label="Process private parts"
        switch="--generate=private"
        column="1"
        tip="Process the private part of packages"
       />
       <check
        label="Missing doc warnings"
        switch="--warnings"
        column="1"
        tip="Emit warnings for missing documentation"
       />
    </switches>
  </target-model>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc"
   menu="/Tools/"
  >
    <iconname>gps-syntax-check-symbolic</iconname>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc4</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc project"
   menu="/Tools/"
  >
    <iconname>gps-syntax-check-symbolic</iconname>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc4</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
  </target>
"""
# This has to be done at GPS start, before the project is actually loaded.
GPS.parse_xml(project_attributes)

SPAWN_BROWSER_PREF = "Documentation:GNATdoc/Doc-Spawn-Browser"
GPS.Preference(SPAWN_BROWSER_PREF).create(
    "Spawn a browser",
    "boolean",
    "Load generated documentation in a browser automatically.",
    True)


@interactive(name="documentation generate for project", category="GNATdoc")
def doc_for_project():
    """Launch GNATdoc on the current project"""
    run_gnatdoc("gnatdoc project")


def generate_doc_project(self, recursive=False):
    """
    Generates the documentation for the project (and its subprojects if
    ``recursive`` is True) and displays it in the default browser.

    :param recursive: A boolean

    .. seealso:: :func:`GPS.File.generate_doc`
    """

    run_gnatdoc(
        target="gnatdoc",
        force=True,
        extra_args=["-P", self.file().path, recurse_flag])


def run_gnatdoc(target, force=False, extra_args=[]):
    """
    Runs GNATdoc using given target and extra arguments.
    """
    extra = list(extra_args)

    GPS.BuildTarget(target).execute(
        synchronous=False, force=force, extra_args=extra)


GPS.Project.generate_doc = generate_doc_project


class GNATdoc_Module(modules.Module):
    # Whether we trust that there are no links in the project hierarchy
    trusted_mode = True

    def setup(self):
        GPS.parse_xml(targets)
        GPS.Hook("compilation_finished").add(self.on_compilation_finished)
        GPS.Hook("preferences_changed").add(self.on_preferences_changed)

        # Initialize trusted_mode and other preferences
        self.on_preferences_changed(None)

    def on_compilation_finished(self, hook, category,
                                target_name="", *args):

        if target_name.startswith("gnatdoc"):
            p = GPS.Project.root()
            doc_dir = p.get_attribute_as_string(
                package="Documentation", attribute="Output_Dir")

            if not doc_dir:
                object_dirs = p.object_dirs()
                if object_dirs:
                    doc_dir = os.path.join(object_dirs[0], "gnatdoc", "html")
                else:
                    doc_dir = "gnatdoc"

            if GPS.Preference(SPAWN_BROWSER_PREF).get():
                GPS.HTML.browse(os.path.join(doc_dir, "index.html"))

    def on_preferences_changed(self, hook_name):
        GNATdoc_Module.trusted_mode = GPS.Preference(
            "Prj-Editor-Trusted-Mode").get()

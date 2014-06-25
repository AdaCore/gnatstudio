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


    def GPS.File.generate_doc(self):
        '''
        Generates the documentation of the file and displays it in the
        default browser.
        '''

"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import modules
import os.path
from gps_utils import interactive

project_attributes = """
  <project_attribute
   name="Documentation_Dir"
   label="Documentation directory"
   package="IDE"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="In which subdirectory of the object dir to generate the doc"
  >
    <string type="directory" />
  </project_attribute>

  <project_attribute
   name="Image_Dir"
   label="Images directory"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="Directory containing image files"
  >
    <string type="directory" />
  </project_attribute>

  <project_attribute
   name="Doc_Pattern"
   label="Documentation pattern"
   package="Documentation"
   editor_page="GNATdoc"
   hide_in="wizard library_wizard"
   description="Regular expression identifying documentation comments"
  >
    <string />
  </project_attribute>

  <project_attribute
   name="Ignored_Subprojects"
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
      <arg>gnatdoc</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
    <icon>gps-syntax-check</icon>
    <switches command="%(tool_name)s" columns="1" lines="3">
       <check
        label="Leading documentation"
        switch="-l"
        column="1"
        tip="Look for documentation located above the code"
       />
       <check
        label="Process private parts"
        switch="-p"
        column="1"
        tip="Process the private part of packages"
       />
       <check
        label="Missing doc warnings"
        switch="-w"
        column="1"
        tip="Emit warnings for missing documentation"
       />
       <check
        label="Rebuild"
        switch="--enable-build"
        column="1"
        tip="Rebuild the project before generating the documentation"
       />
    </switches>
  </target-model>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc"
   menu="/Tools/"
  >
    <icon>gps-syntax-check</icon>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc project recursive"
   menu="/Tools/"
  >
    <icon>gps-syntax-check</icon>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc project"
   menu="/Tools/"
  >
    <icon>gps-syntax-check</icon>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc</arg>
      <arg>-P%PP</arg>
      <arg>--no-subprojects</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="gnatdoc" category="Documentation"
   name="gnatdoc file"
   menu="/Tools/"
  >
    <icon>gps-syntax-check</icon>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>gnatdoc</arg>
      <arg>-P%PP</arg>
      <arg>--single-file</arg>
      <arg>%fp</arg>
      <arg>%X</arg>
    </command-line>
  </target>
"""

SPAWN_BROWSER_PREF = "Documentation/Doc-Spawn-Browser"
GPS.Preference(SPAWN_BROWSER_PREF).create(
    "Spawn a browser",
    "boolean",
    "Whether GNATdoc should spawn a browser after having generated"
    " the documentation.",
    True)


@interactive(name="documentation generate for project", category="GNATdoc")
def doc_for_project():
    """Launch GNATdoc on the current project"""
    GPS.BuildTarget("gnatdoc project").execute(synchronous=False)


@interactive(name="documentation generate for project and subprojects",
             category="GNATdoc")
def doc_for_project_and_subprojects():
    """Launch GNATdoc on the the project, recursively"""
    GPS.BuildTarget("gnatdoc project recursive").execute(synchronous=False)


@interactive(name="documentation generate for current file",
             category="GNATdoc")
def doc_for_file():
    """Launch GNATdoc on the current project"""
    GPS.BuildTarget("gnatdoc file").execute(synchronous=False)


def generate_doc_file(self):
    """
    Generates the documentation of the file and displays it in the
    default browser.

    .. seealso:: :func:`GPS.Project.generate_doc`
    """
    GPS.BuildTarget("gnatdoc project").execute(
        synchronous=False,
        force=True,
        extra_args="--single-file %s" % os.path.basename(self.name()))


def generate_doc_project(self, recursive=False):
    """
    Generates the documentation for the project (and its subprojects if
    ``recursive`` is True) and displays it in the default browser.

    :param recursive: A boolean

    .. seealso:: :func:`GPS.File.generate_doc`
    """
    if recursive:
        recurse_flag = ""
    else:
        recurse_flag = "--no-subprojects"

    GPS.BuildTarget("gnatdoc").execute(
        synchronous=False,
        force=True,
        extra_args="-P %s %s" % (self.file().name(), recurse_flag))

GPS.File.generate_doc = generate_doc_file
GPS.Project.generate_doc = generate_doc_project


class GNATdoc_Module(modules.Module):
    def setup(self):
        GPS.parse_xml(project_attributes)
        GPS.parse_xml(targets)
        GPS.Hook("compilation_finished").add(self.on_compilation_finished)

    def on_compilation_finished(self, hook, category,
                                target_name="", mode_name="", status=""):

        if target_name.startswith("gnatdoc"):
            p = GPS.Project.root()
            doc_dir = p.get_attribute_as_string(
                package="IDE", attribute="Documentation_Dir")

            if not doc_dir:
                object_dirs = p.object_dirs()
                if object_dirs:
                    doc_dir = os.path.join(object_dirs[0], "gnatdoc")
                else:
                    doc_dir = "gnatdoc"

            if GPS.Preference(SPAWN_BROWSER_PREF).get():
                GPS.HTML.browse(os.path.join(doc_dir, "index.html"))

"""
This plug-in adds support for GMC (the GNAT Modeling Compiler) which generates
Ada (SPARK 2014?) and C code from Simulink models.

=========================================
THIS IS WORK IN PROGRESS
As it is this module does not perform anything useful. It defines
the Simulink language, which you can use in your project, but expects
.mdl files to be a JSON definition compatible with
GPS.Browsers.Diagram.load_json. The JSON is loaded into a browser when
you open a .mdl file, for instance from the Project view.
=========================================

"""

import GPS
import GPS.Browsers
import modules
import os.path
import os_utils

# Constants

gmc_name = 'gmc'
gmc_exec = os_utils.locate_exec_on_path(gmc_name)

# Language definitions

matlab_def = """<?xml version='1.0' ?>
  <GPS>
    <Language>
      <Name>Matlab</Name>
      <Body_Suffix>.m</Body_Suffix>
      <Obj_Suffix>-</Obj_Suffix>
    </Language>
  </GPS>"""

simulink_def = """<?xml version='1.0' ?>
  <GPS>
    <Language>
      <Name>Simulink</Name>
      <Body_Suffix>.mdl</Body_Suffix>
      <Obj_Suffix>-</Obj_Suffix>
    </Language>
  </GPS>"""

# Project-related definitions

project_attributes = """<?xml version='1.0' ?>
  <GPS>
    <project_attribute
     package="GMC"
     name="Root_Models"
     editor_page="GMC"
     label="Root models"
     description="Ask Matteo 1"
     hide_in="wizard library_wizard">
       <string type="file" filter="project"/>
    </project_attribute>

    <project_attribute
     package="GMC"
     name="Output_Dir"
     editor_page="GMC"
     label="Output directory"
     description="Ask Matteo 2"
     hide_in="wizard library_wizard">
       <string type="directory"/>
    </project_attribute>
  </GPS>"""

project_switches = """<?xml version='1.0' ?>
  <GPS>
    <tool
     name="GMC"
     package="GMC"
     index="Simulink">
      <language>Simulink</language>
      <initial-cmd-line>-i -l ada</initial-cmd-line>
      <switches lines="3">
        <title line="1">Output</title>
        <title line="2">Generation</title>
        <title line="3">Files</title>

        <radio line="1">
          <radio-entry
           label="Delete"
           switch="-c"
           tip="Delete contents of output directory between compilations"/>
          <radio-entry
           label="Preserve"
           switch="-i"
           tip="Preserve contents of output directory between compilations"/>
        </radio>

        <combo
         line="2"
         label="Target language"
         switch="-l"
         separator=" "
         tip="Ask Matteo 3">
           <combo-entry label="Ada" value="ada"/>
           <combo-entry label="C" value="c"/>
        </combo>

        <field
         line="3"
         label="Matlab file"
         switch="-m"
         separator=" "
         as-file="true"
         tip="Ask Matteo 4"/>
        <field
         line="3"
         label="Decoration file"
         switch="-t"
         separator=" "
         as-file="true"
         tip="Ask Matteo 5"/>
        <field
         line="3"
         label="Reference file"
         switch="-b"
         separator=" "
         as-file="true"
         tip="Ask Matteo 6"/>
      </switches>
    </tool>
  </GPS>
"""


class GMC_Module(modules.Module):

    def __on_open_file_action_hook(self, hook, file, *args):
        """Handles "open file" events"""
        if file.language() == 'simulink':
            v = self.open_file(file)
            return True
        return False

    def load_desktop(self, view, data):
        v = self.open_file(GPS.File(data))
        return GPS.MDI.get_by_child(v)

    def save_desktop(self, child):
        # ??? Should save position and scaling factor too
        return child.get_child().file.name()

    def open_file(self, file):
        """
        Open a .mdl file as a browser, and puts it in the MDI.
        :param GPS.File file: the file to open
        :return: The created GPS.Browsers.View
        """
        diagrams = GPS.Browsers.Diagram.load_json(file.name())
        v = GPS.Browsers.View.create(
            diagrams[0],
            title=os.path.basename(file.name()),
            save_desktop=self._save_desktop)
        v.file = file
        v.set_background(
            GPS.Browsers.View.Background.GRID,
            GPS.Browsers.Style(stroke="rgba(200,200,200,0.8)"))
        v.scale_to_fit(max_scale=1.0)
        return v

    # Setup the module only when the GMC executable is available on the path.
    # This action registeres the Matlab and Simulink languages along with the
    # various project attributes.

    def setup(self):
        if gmc_exec:
            # ??? Multiple calls to parse_xml are less efficient, would be
            # better to merge the XML strings into one.
            GPS.parse_xml(matlab_def)
            GPS.parse_xml(simulink_def)
            GPS.parse_xml(project_attributes)
            GPS.parse_xml(project_switches)

            GPS.Hook('open_file_action_hook').add(
                self.__on_open_file_action_hook, last=False)

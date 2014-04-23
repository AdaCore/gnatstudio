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

language_def = r"""<?xml version='1.0' ?>
  <GPS>
    <Language>
      <Name>Matlab</Name>
      <Body_Suffix>.m</Body_Suffix>
      <Obj_Suffix>-</Obj_Suffix>
    </Language>
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


class GMC_Canvas_View(GPS.Browsers.View):
    def __init__(self, file, module):
        """
        A browser that shows the contents of a simulink file.
        :param GPS.File file: the file associated with the browser.
        :param modules.Module module: the module
        """
        self.file = file
        diagrams = GPS.Browsers.Diagram.load_json(file.name())
        self.create(
            diagrams[0],
            title=os.path.basename(file.name()),
            save_desktop=module._save_desktop)
        self.set_background(
            GPS.Browsers.View.Background.GRID,
            GPS.Browsers.Style(stroke="rgba(200,200,200,0.8)"))
        self.scale_to_fit(max_scale=1.0)

    def on_item_clicked(self, item, button, x, y, *args):
        GPS.Console().write("clicked on %s with %s, at %s,%s\n" %
                            (item, button, x, y))

    def on_item_double_clicked(self, item, button, x, y, *args):
        GPS.Console().write("double_clicked on %s with %s, at %s,%s\n" %
                            (item, button, x, y))

class GMC_Module(modules.Module):

    def __on_open_file_action_hook(self, hook, file, *args):
        """Handles "open file" events"""
        if file.language() == 'simulink':
            v = GMC_Canvas_View(file=file, module=self)
            return True
        return False

    def load_desktop(self, view, data):
        v = GMC_Canvas_View(file=GPS.File(data), module=self)
        return GPS.MDI.get_by_child(v)

    def save_desktop(self, child):
        # ??? Should save position and scaling factor too
        return child.get_child().file.name()

    # Setup the module only when the GMC executable is available on the path.
    # This action registeres the Matlab and Simulink languages along with the
    # various project attributes.

    def setup(self):
        if gmc_exec:
            # ??? Multiple calls to parse_xml are less efficient, would be
            # better to merge the XML strings into one.
            GPS.parse_xml(project_attributes)
            GPS.parse_xml(project_switches)
            GPS.Hook('open_file_action_hook').add(
                self.__on_open_file_action_hook, last=False)

# Need to load the new language definition right away, since GPS loads
# the project before setting up the module, so the extensions would not
# be known otherwise

GPS.parse_xml(language_def)

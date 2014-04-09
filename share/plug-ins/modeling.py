"""
This plug-in adds support for the GMC compiler, generating
code from Simulink models.

=========================================
THIS IS WORK IN PROGRESS
As it is this module does not perform anything useful. It defines
the Simulink language, which you can use in your project, but expects
.mdl files to be a JSON definition compatible with
GPS.Browsers.Diagram.load_json. The JSON is loaded into a browser when
you open a .mdl file, for instance from the Project view.
=========================================

"""

import modules
import GPS
import GPS.Browsers
import os.path


class GMC_Module(modules.Module):
    def setup(self):
        GPS.Hook('open_file_action_hook').add(
            self.__on_open_file_action_hook, last=False)

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

    def __on_open_file_action_hook(self, hook, file, *args):
        """Called for open_file_action_hook"""

        if file.language() == 'simulink':
            v = self.open_file(file)
            return True
        return False

    def save_desktop(self, child):
        # ??? Should save position and scaling factor too
        view = child.get_child()
        return view.file.name()

    def load_desktop(self, view, data):
        v = self.open_file(GPS.File(data))
        return GPS.MDI.get_by_child(v)

# Need to load the new language definition right away, since GPS loads
# the project before setting up the module, so the extensions would not
# be known otherwise

GPS.parse_xml(r"""<?xml version='1.0' ?>
<GPS>
   <Language>
       <Name>Simulink</Name>
       <Body_Suffix>.mdl</Body_Suffix>
       <Obj_Suffix>-</Obj_Suffix>
   </Language>
   <Language>
       <Name>Matlab</Name>
       <Body_Suffix>.m</Body_Suffix>
       <Obj_Suffix>-</Obj_Suffix>
   </Language>
</GPS>""")

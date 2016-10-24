import GPS
import os


class Project_Support(object):
    """
    This class provides an interface to the project facilities, to be
    used by QGen.
    """

    @staticmethod
    def register_tool():
        """Register the QGENC tool and its switches"""

        GPS.parse_xml("""<?xml version='1.0' ?>
           <GPS>
             <project_attribute
              package="QGen"
              name="Output_Dir"
              editor_page="QGen"
              label="Output directory"
              description="The location of all generated source code files"
              hide_in="wizard library_wizard">
                <string type="directory"/>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Switches"
              editor_page="QGen"
              list="true"
              label="Switches"
              hide_in="wizard library_wizard">
                <index attribute='Languages'>
                   <string />
                </index>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Target"
              editor_page="QGen"
              list="true"
              label="Target"
              hide_in="wizard library_wizard">
                <index attribute='Languages'>
                   <string />
                </index>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Debug_Args"
              editor_page="QGen"
              label="Debug arguments"
              description="Launch arguments for generated debug session"
              hide_in="wizard library_wizard">
                <string />
             </project_attribute>

             <target-model name="QGenc" category="">
               <description>Generic launch of QGen</description>
               <iconname>gps-build-all-symbolic</iconname>
               <switches>
               </switches>
             </target-model>

             <target model="QGenc" category="_File_" name="QGen for file">
               <in-toolbar>FALSE</in-toolbar>
               <in-menu>FALSE</in-menu>
               <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
               <read-only>TRUE</read-only>
               <command-line>
                 <arg>qgenc</arg>
                 <arg>--trace</arg>
                 <arg>-i</arg>
                 <arg>-l</arg>
                 <arg>ada</arg>
               </command-line>
             </target>

             <tool
              name="QGENC"
              package="QGen"
              index="Simulink">
               <language>Simulink</language>
               <switches>
                 <title line="1">Files</title>
                 <title line="2">Generation</title>
                 <title line="3">Output</title>

                 <field
                  line="1"
                  label="Matlab file"
                  switch="-m"
                  separator=" "
                  as-file="true"
                 tip="Provides variable declarations of the Matlab workspace"/>
                 <field
                  line="1"
                  label="Typing file"
                  switch="-t"
                  separator=" "
                  as-file="true"
                  tip="Provides Simulink block typing information"/>
                 <field
                  line="1"
                  label="Library directory"
                  switch="-b"
                  separator=" "
                  as-directory="true"
                  tip=""/>

                 <combo
                  line="2"
                  label="Target language"
                  switch="-l"
                  separator=" "
             tip="The language used by QGENC to produce the generated files">
                    <combo-entry label="Ada" value="ada"/>
                    <combo-entry label="C" value="c"/>
                 </combo>
                 <check
                  line="2"
                  label="Flatten model"
                  switch="--full-flattening"
                  tip=""/>

                 <radio line="3">
                   <radio-entry
                    label="Delete"
                    switch="-c"
             tip="Delete contents of output directory between compilations"/>
                   <radio-entry
                    label="Preserve"
                    switch="-i"
             tip="Preserve contents of output directory between compilations"/>
                 </radio>
               </switches>
             </tool>
           </GPS>""")

    @staticmethod
    def get_output_dir(file):
        """
        Return the output directory to use when generating code for file.
        It default to the project's object directory.

        :param GPS.File file: the .mdl file
        """
        if file is None:
            return None

        p = file.project()
        dir = p.get_attribute_as_string(
            package='QGen', attribute='Output_Dir')
        if dir:
            # Get absolute directory from Output_Dir
            dir = os.path.join(os.path.dirname(p.file().path), dir)
        else:
            try:
                return p.object_dirs()[0]
            except:
                return GPS.Project.root().object_dirs()[0]
        return dir

    @staticmethod
    def get_models(filename):
        """
        Return the models to generated code for
        for a specific target
        :param string filename: the target file
        :return GPS.File list: the list of model files
        """
        f = GPS.File(filename)
        models_files = []
        try:
            models = f.project().get_attribute_as_list(
                attribute='Target', package='QGen',
                index=os.path.basename(filename))
            for mod in models:
                models_files.append(GPS.File(mod))
        except:
            models_files = []

        return models_files

    @staticmethod
    def get_switches(file):
        """
        Return the wswitches to use for a specific file
        :param GPS.File file: the .mdl file
        :return str: the list of switches
        """
        try:
            switches = file.project().get_attribute_as_string(
                attribute='Switches', package='QGen',
                index=os.path.basename(file.path))
            if not switches:
                switches = file.project().get_attribute_as_string(
                    attribute='Switches', package='QGen',
                    index='simulink')
        except:
            switches = ''

        return switches

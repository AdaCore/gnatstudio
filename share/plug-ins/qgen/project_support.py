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
              editor_section="Default Arguments"
              label="Output directory"
              description="The default location for QGen generated files"
              hide_in="wizard library_wizard">
                <string type="directory"/>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Target"
              editor_page="QGen"
              list="true"
              label="Target"
              hide_in="wizard library_wizard">
              <index attribute="main">
                 <string type="file" filter="project" default="(Simulink model
 file(s))"/>
              </index>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Debug_Args"
              editor_page="QGen"
              editor_section="Default Arguments"
              label="Debug arguments"
              description="Arguments to supply to gdb `run` when starting the
 QGen debugger"
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
               </command-line>
             </target>

             <tool
              name="QGen"
              package="QGen"
              index="QGen">
               <language>QGen</language>
               <switches>
                 <title line="1">Input Files</title>
                 <title line="2">Generation Configuration</title>
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
                  label="Arithmetic operations configuration file"
                  switch="--arith-conf"
                  separator=" "
                  as-file="true"
                 tip="Specify a file containing the configuration for \
arithmetic operations"/>

                 <field
                  line="1"
                  label="Custom block implementations configuration file"
                  switch="--block-conf"
                  separator=" "
                  as-file="true"
                 tip="Specify a file containing the configuration for \
custom block implementations"/>

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
                  label="Generate unoptimized code"
                  switch="--debug"
                 tip=""/>

                 <check
                  line="2"
                  label="Generate traceability infos for model debugging"
                  switch="--trace"
                 tip=""/>

                 <radio
                  line="2"
                  label="MISRA violations"
                 tip="The behavior when detecting MISRA Simulink violations">
                   <radio-entry label="Detect as errors"
                    switch = ""/>
                   <radio-entry label="Detect as warnings"
                    switch="--wmisra"/>
                   <radio-entry label="Accept"
                    switch="--no-misra"/>
                 </radio>

                 <check
                  line="2"
                  label="Generate code for all model variants"
                  switch="--all-variants"
                 tip=""/>

                 <check
                  line="2"
                  label="Generate entry point source file"
                  switch="--gen-entrypoint"
                 tip=""/>

                 <check
                  line="2"
                  label="Always generate system variables as variables"
                  switch="--consts-as-vars"
                 tip=""/>

                 <check
                  line="2"
                  label="Remove code for assertions blocks"
                  switch="--remove-assertions"
                 tip=""/>

                 <check
                  line="2"
                  label="Remove goto statements"
                  switch="--no-jump"
                 tip=""/>

                 <check
                  line="2"
                  label="Generate main subsystem's IO as global variables"
                  switch="--global-io"
                 tip=""/>

                 <radio line = "2" label="Flattening strategy"
                    tip="Controls the way source files are generated">
                    <radio-entry label="No flattening"
                                 switch=""/>
                    <radio-entry label="Full flattening"
                                 switch="--full-flattening" />
                    <radio-entry label="Flattening by reference"
                                 switch="--ref-flattening"/>
                    <radio-entry label="Flatten non reusable functions"
                                 switch="--noreuse-flattening" />
                 </radio>

                 <field
                  line="3"
                  label="Output Directory"
                  switch="-o"
                  separator=" "
                  as-directory="true"
                 tip="Specify the output directory"/>

                 <radio line="3" label="Output directory cleaning policy">
                   <radio-entry
                    label="Ensure non existent"
                    switch=""
             tip="Only generates code if the output directory does not exist"/>
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
        It defaults to project_root/MODEL_NAME.[mdl|slx]_generated if
        not found in attribute or switches.
        :param GPS.File file: the .mdl file
        """

        if file is None:
            return None

        p = file.project()
        dir = p.get_attribute_as_string(
            package='QGen', attribute='Output_Dir')
        if not dir:
            # Defaulting because no output directory specified
            # in project attributes
            dir = file.path + '_generated'

        # Get absolute directory for the output directory
        dir = os.path.join(os.path.dirname(p.file().path), dir)

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
        except Exception:
            models_files = []

        return models_files

    @staticmethod
    def get_switches(file):
        """
        Return the switches to use for a specific file.
        :param GPS.File file: the model file
        :return str list: the list of switches
        """
        try:
            switches = file.project().get_attribute_as_list(
                attribute='Switches', package='QGen',
                index=os.path.basename(file.path))
            if not switches:
                switches = file.project().get_attribute_as_list(
                    attribute='Switches', package='QGen',
                    index='simulink')
        except Exception:
            switches = ''

        return switches

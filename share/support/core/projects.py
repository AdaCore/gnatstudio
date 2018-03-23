"""
Defines the standard project attributes.

THIS PLUGIN MUST BE LOADED.

This plugin defines the standard project attributes, and how they should
be edited graphically in the project properties editor. If you do not
load this script, GPS will display a number of warnings in its Messages
window when a project is loaded
"""


import GPS

XML = r"""<?xml version="1.0" ?>
<GPS>
    <!-- Sources directories -->

    <project_attribute
        name="source_dirs"
        editor_page="Sources/Directories"
        editor_section="Source directories"
        list="true"
        ordered="true"
        description="List of directories that contain the source files. You can specify that a directory and all its subdirectories should be included by checking the Recursive checkbox."
        label="">
        <string type="directory" default="." />
    </project_attribute>

    <!--  Source files -->

    <mutually_exclusive name="Sources Provider" editor_page="Sources/Files" description="Choose the way to provide the project sources files.">
        <project_attribute
            name="source_list_file"
            editor_page="Sources/Files"
            editor_section="Source List File"
            description="Name of a file that contains the list of source files for this project. The names should appear one per line. The names should not include any directory information, since this is taken from the list of source directories. This attribute is ignored if an explicit list of sources is given."
            disable_if_not_set="true"
            hide_in="wizard library_wizard"
            label="Source list file">
            <string type="file" default="" />
        </project_attribute>

        <project_attribute
            name="source_files"
            editor_page="Sources/Files"
            list="true"
            base_name_only="true"
            disable_if_not_set="true"
            hide_in="wizard library_wizard"
            label="Source files"
            description="List of source files for this project. These are the base name of the files, and should not include any directory information. This attribute is not compatible with Source List File.">
            <string type="file" default="project source files" />
        </project_attribute>
    </mutually_exclusive>

    <!--  Locally removed files -->

    <project_attribute
        name="locally_removed_files"
        label="files"
        editor_page="Sources/Files"
        editor_section="Locally removed files"
        description="List of source files from the extended project that should no longer be visible to the compiler when compiling the extending project. This can be used for instance when a source file has become obsolete due to other changes in the project."
        disable_if_not_set="true"
        hide_in="library_wizard wizard"
        list="true"
        base_name_only="true">
        <string type="file" filter="extended_project" default="" />
    </project_attribute>

   <project_attribute
       name="languages"
       editor_page=""
       editor_section="Languages"
       label=""
       description="List of languages for the source files of this project."
       hide_in="properties"
       list="true">
       <shell default="Ada">supported_languages</shell>
       <string />
   </project_attribute>

   <project_attribute
      name="connection_tool"
      package="IDE"
      editor_page="Embedded"
      editor_section="Tools"
      description="Executable used to interface with a remote target when debugging. GPS currently supports OpenOCD, st-util or pyOCD. You can leave this attribute empty if you are using a custom tool spawned outside of GPS."
      hide_in="wizard library_wizard"
      label="Connection tool">
      <choice>st-util</choice>
      <choice>openocd</choice>
      <choice>pyocd</choice>
      <string />
   </project_attribute>

   <project_attribute
      name="connection_config_file"
      package="IDE"
      editor_page="Embedded"
      editor_section="Tools"
      description="File used to configure the IDE'Connection_Tool. Used only when OpenOCD is set."
      hide_in="wizard library_wizard"
      label="Connection configuration file">
      <string type="file"/>
   </project_attribute>

   <project_attribute
       name="program_host"
       package="IDE"
       editor_page="Embedded"
       editor_section="Communication Settings"
       description="Name or IP address of the embedded target. This field should be left blank if you are not working on an embedded application."
       hide_in="wizard library_wizard"
       label="Program host">
       <string />
   </project_attribute>

   <project_attribute
       name="communication_protocol"
       package="IDE"
       editor_page="Embedded"
       editor_section="Communication Settings"
       description="Protocol used to connect to the embedded target. This field should be left blank if you are not working on an embedded application."
       hide_in="wizard library_wizard"
       label="Protocol">
       <choice>remote</choice>
       <choice>extended-remote</choice>
       <choice>wtx</choice>
       <choice>dfw</choice>
       <choice>dfw-rtp</choice>
       <choice>qnx</choice>
       <string />
   </project_attribute>

   <project_attribute
       name="Compiler_Command"
       package="IDE"
       editor_page=""
       editor_section="Tools"
       description="The command to compile the source files for a given language."
       hide_in="all"
       label="compiler">
       <index attribute="Languages">
          <string />
       </index>
       <specialized_index value="Ada">
          <choice default="true" >gnatmake</choice>
       </specialized_index>
   </project_attribute>

   <project_attribute
       name="Xref_Database"
       package="IDE"
       description="Location of the xref database."
       hide_in="all"
       label="xref database">
   </project_attribute>

   <project_attribute
       name="gnatlist"
       package="IDE"
       editor_page=""
       editor_section="Tools"
       description="The gnatls command used to find where the Ada run time files are installed (including optional arguments, e.g. gnatls --RTS=sjlj)."
       hide_in="all"
       label="Gnatls">
       <choice default="true" >gnatls</choice>
       <string />
   </project_attribute>

   <project_attribute
       name="gnat"
       package="IDE"
       editor_page=""
       editor_section="Tools"
       description="The gnat driver used to run the various commands associated with the GNAT toolchain."
       hide_in="all"
       label="Gnat">
       <choice default="true" >gnat</choice>
       <string />
   </project_attribute>

   <project_attribute
       name="debugger_command"
       package="IDE"
       editor_page=""
       editor_section="Tools"
       description="The command line to use when debugging applications (including optional arguments). Only gdb and its variants are currently supported."
       hide_in="all"
       label="Debugger">
       <choice default="true" >gdb</choice>
       <string />
   </project_attribute>

   <project_attribute
       name="global_configuration_pragmas"
       package="Builder"
       editor_page="Build"
       editor_section="External configuration"
       description="External file that contains the configuration pragmas to use for Ada sources. This file will be used both for this project and all its imported projects."
       label="Global pragmas"
       hide_in="wizard library_wizard">
       <string type="file" />
   </project_attribute>

   <project_attribute
       name="local_configuration_pragmas"
       package="Compiler"
       editor_page="Build"
       editor_section="External configuration"
       description="External file that contains the configuration pragmas to use for Ada sources in this project. This is the combined with the pragmas found in the Global pragmas attribute of the root project."
       label="Local pragmas"
       hide_in="wizard library_wizard">
       <string type="file" />
   </project_attribute>

   <project_attribute
       name="global_compilation_switches"
       package="Builder"
       editor_page="Build"
       editor_section="External configuration"
       description="List of compiler switches to use when compiling any source file in the project hierarchy."
       label="Global Compilation Switches"
       list="true"
       hide_in="wizard library_wizard">
       <index attribute="Languages">
          <string />
       </index>
   </project_attribute>

   <!--  VCS attributes -->

   <project_attribute
       name="vcs_kind"
       package="IDE"
       editor_page="Version Control"
       editor_section="System"
       description="Name of the version control system that you are using."
       label="System">
       <shell default="None" lang="python">
GPS.VCS2.supported_systems() if hasattr(GPS, 'VCS2') else GPS.VCS.supported_systems()
       </shell>
   </project_attribute>

   <project_attribute
       name="vcs_log_check"
       package="IDE"
       editor_page="Version Control"
       editor_section="Actions"
       description="Application run on the log file/revision history just before commiting a file. If it returns anything other than 0, the commit will not be performed. The only parameter to this script is the name of the log file."
       label="Log checker">
       <string />
   </project_attribute>

   <project_attribute
       name="vcs_file_check"
       package="IDE"
       editor_page="Version Control"
       editor_section="Actions"
       description="Application run on the source file just before commiting a file. If it returns anything other than 0, the commit will not be performed. The only parameter to this script is the name of the source file."
       label="File checker">
       <string />
    </project_attribute>

    <project_attribute
       name="vcs_repository_root"
       package="IDE"
       editor_page="Version Control"
       editor_section="Path"
       description="The repository root path."
       label="Repository">
       <string />
    </project_attribute>

    <project_attribute
       name="vcs_patch_root"
       package="IDE"
       editor_page="Version Control"
       editor_section="Path"
       description="The root directory to use for building patch file. The root project directory is used if this value is not defined."
       label="Patch">
       <string />
    </project_attribute>

    <!--  Object directories -->

    <project_attribute
        name="object_dir"
        editor_page="Build/Directories"
        editor_section="Build Directories"
        description="The name of the directories in which the files generated by the compiler will be put. This include object files and any other file that your compiler generates as a by-product of the compilation. If you need multiple object directories, you must create multiple project files that import each other. The directory is relative to the project directory (thus '.' means the project directory)."
        label="Build directory">
        <string type="directory" default="." allow_empty="False"/>
    </project_attribute>

    <project_attribute
        name="exec_dir"
        editor_page="Build/Directories"
        editor_section="Build Directories"
        description="Directory in which the executable will be copied. By default, this is the same as the object directory, and doesn't need to be further specified."
        label="Exec directory"
        omit_if_default="true">
        <string type="directory" default="(same as build directory)" allow_empty="False" />
    </project_attribute>

    <project_attribute
        name="Artifacts_Dir"
        package="IDE"
        editor_page="Build/Directories"
        editor_section="GPS Artifacts"
        description="The directory in which the files generated by GPS for this project (cross-references database, locations etc.) are stored by default. Defaults to Object_Dir if not specified."
        label="Artifacts Directory">
        <string type="directory" default="(same as object directory)" allow_empty="False"/>
    </project_attribute>

    <project_attribute
        name="documentation_dir"
        package="IDE"
        editor_page="Build/Directories"
        editor_section="Documentation Directories"
        description="Directory in which the documentation will be generated. By default, this is a subdirectory 'doc' in the object directory, and doesn't need to be further specified."
        label="Doc directory"
        omit_if_default="true">
        <string type="directory" default="(subdir doc in build directory)" allow_empty="False" />
    </project_attribute>

    <!--  Main units -->

    <project_attribute
        name="main"
        editor_page="Sources/Main"
        editor_section="Main files"
        list="true"
        description="List of source files that contain the application's entry point. These units will appear in the Build menu, so that you can easily share a single project hierarchy to build a whole set of applications."
        label=""
        base_name_only="true" >
        <string type="file" filter="project" />
   </project_attribute>

   <project_attribute
       name="executable_suffix"
       package="builder"
       editor_page="Sources/Main"
       editor_section="Executable Suffix"
       label="Default suffix"
       description="The default suffix for executables generated by the builder. This default can be overridden by modifying the list of executable names below.">
       <string />
   </project_attribute>

   <project_attribute
       name="executable"
       package="Builder"
       editor_page="Sources/Main"
       editor_section="Executable names"
       description="Name of the executable generated when compiling each of the main units."
       case_sensitive_index="file"
       label="">
       <index attribute="main">
          <string default="(same as main unit, minus extension)"/>
       </index>
   </project_attribute>

   <project_attribute
       name="library_name"
       editor_page="Library"
       editor_section="General"
       label="Library Name"
       hide_in="wizard"
       description="Name of the library that will contain all the object files from this project. You must chose a name which is valid for your platform.">
       <string />
   </project_attribute>

   <project_attribute
       name="library_dir"
       editor_page="Library"
       editor_section="General"
       label="Library Directory"
       hide_in="wizard"
       description="Directory in which the library will be copied as a result of the compilation. Some extra files will be copied in this directory as well, for instance the ALI file for Ada sources. This directory must be different from the object directory.">
       <string type="directory"/>
   </project_attribute>

   <project_attribute
       name="library_ali_dir"
       editor_page="Library"
       editor_section="General"
       label="Library ALI Directory"
       hide_in="wizard"
       description="Directory in which the ALI files needed by the GNAT compiler will be copied when the library is installed. By default, this will be the same as the Library Directory. This attribute requires a recent version of GNAT.">
       <string type="directory"/>
   </project_attribute>

   <project_attribute
       name="library_kind"
       editor_page="Library"
       editor_section="General"
       label="Library Kind"
       hide_in="wizard"
       description="The kind of library that should be built. If you need to build different kinds of libraries, it is recommend to select a different library directory for each, since the object files might need to be compiled differently in each case.">
       <choice default="true">static</choice>
       <choice>dynamic</choice>
   </project_attribute>

   <project_attribute
       name="library_version"
       editor_page="Library"
       editor_section="General"
       label="Version"
       hide_in="wizard"
       description="The version of the library. This is a platform depend string, and you should make sure it is valid for your platform.">
       <string />
   </project_attribute>

   <project_attribute
       name="linker_options"
       package="linker"
       editor_page="Library"
       editor_section="General"
       label="Linker options"
       hide_in="wizard"
       list="true"
       description="Additional linker switches used when linking an executable. This attribute is ignored in the main project, and only used from dependent projects.">
       <string />
   </project_attribute>

   <project_attribute
       name="externally_built"
       editor_page="Library"
       editor_section="General"
       label="Externally Built"
       hide_in="wizard"
       description="Set this attribute to True if the compiler should not check whether the library is up-to-date. As a result, the sources do not need to be
available, apart of course for the spec files and the bodies of generic packages.">
       <choice default="true">False</choice>
       <choice>True</choice>
   </project_attribute>

   <project_attribute
       name="library_interface"
       editor_page="Library/Standalone"
       editor_section="Standalone library"
       label="Library Interface"
       hide_in="wizard"
       description="This attribute is optional. If it is defined, it should contain the list of units that are the public interface to the library. All other units will not be accessible by users of the library. If this attribute is defined, the library will be a standalone library."
       list="true">
       <string type="unit" />
   </project_attribute>

   <project_attribute
       name="interfaces"
       editor_page="Library/Standalone"
       editor_section="Standalone library"
       label="Interfaces"
       hide_in="wizard"
       description="This attribute defines an explicit subset of the source files of a project. Sources from projects importing this project, can only depend on sources from this subset. This attribute can be used on non library projects. It can also be used as a replacement for attribute Library_Interface, in which case, units have to be replaced by source files. For multi-language library projects, it is the only way to make the project a Stand-Alone Library project whose interface is not purely Ada."
       list="true">
       <string type="file" filter="all_projects" default=""/>
   </project_attribute>

   <project_attribute
       name="library_standalone"
       editor_page="Library/Standalone"
       editor_section="Standalone library"
       label="Stand-alone library kind"
       hide_in="wizard"
       description="This attribute defines the kind of stand-alone library to build. Values are either 'standard' (the default), 'no' or 'encapsulated'. Choose 'encapsulated' if your library depend only on static libraries (including the GNAT runtime) or 'no' to make it clear that your library should not be stand-alone.">
       <choice default="true">standard</choice>
       <choice>encapsulated</choice>
       <choice>no</choice>
   </project_attribute>

   <project_attribute
       name="library_auto_init"
       editor_page="Library/Standalone"
       editor_section="Standalone library"
       label="Auto Init"
       hide_in="wizard"
       description="If this attribute is true, then the standalone library will be automatically initialized when part of the project. If this attribute is false, you need to call some special elaboration code yourself. You must define the interface to the library as well.">
       <choice default="true">True</choice>
       <choice>False</choice>
   </project_attribute>

   <project_attribute
       name="library_src_dir"
       editor_page="Library/Standalone"
       editor_section="Standalone library"
       label="Source directory"
       hide_in="wizard"
       description="The directory in which the sources required for Ada clients of the library will be copied. This include public interfaces, as well as bodies for inline subprograms,... This directory can be the same as the library directory, but must be different from the object directory.">
       <string type="directory" />
   </project_attribute>
</GPS>
"""

GPS.parse_xml(XML)

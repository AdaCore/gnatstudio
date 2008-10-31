"""Provides support for building through make or ant make using the GPS
   Build Manager.

   This script defines the following new project attributes in the "make"
   package of a .gpr file. They can be edited graphically through
   the project properties editor.
       "make.makefile": name of the Makefile to use. This can be an absolute
                    name, or a name relative to the directory containing the
                    root project file (ie the one loaded in GPS). This
                    attribute is optional. If unspecified, GPS will look for
                    either "Makefile" or "makefile" in the directory
                    containing the root project.

   This script defines the following new project attributes in the "ant"
   package of a .gpr file:
       "ant.antfile": name of the Antfile to use. This can be an absolute
                    name, or a name relative to the directory containing the
                    root project file (ie the one loaded in GPS). This
                    attribute is optional. If unspecified, GPS will look for
                    "build.xml" in the directory containing the root project.

   GPS will systematically compile the application by passing the scenario
   variables (see the menu /Tools/Views/Scenario). For instance, this
   will result in calling
       make -f Makefile VARIABLE1=VALUE target
   or
       ant -buildfile build.xml -DVARIABLE1=VALUE target

   These scenario variables are defined in the project file, and should have
   the same name as in the Makefile or the AntFile.

   By default, the name in the menu will be the name of the targets found in
   the build file.
   As a special case, when the comment is "IGNORE", as in:

      target: dependency1 dependency2 # IGNORE

   then that target is not displayed in the menu

   A similar behavior is applied for build.xml files for ant, where the
   "description" attribute of the <target> node is taken into account.

   When you select one of the new menus, GPS will run ant or make, and parse
   error messages to display them in the locations window as usual.
"""



import GPS, sys, traceback, re, os, os_utils
from os.path import *
from GPS import *

# This is an XML model for make/gnumake
Make_Model = """
<target-model name="make" category="">
   <description>Build with make</description>
   <command-line>
      <arg>make</arg>
      <arg>%vars</arg>
      <arg>-f</arg>
      <arg>%attr(make'makefile,Makefile)</arg>
      <arg>%T</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <check label="Quiet mode" switch="-s" tip="Don't echo commands" />
     <check label="Project variables" switch="%vars"
           tip="Pass project variables to make" />
     <spin label="Multiprocessing" switch="-j" min="1" max="100" default="1"
           tip="Use N processes to carry out the compilations. On a multiprocessor machine compilations will occur in parallel" />
   </switches>
</target-model>

<target model="make" category="_Makefile" name="Make">
    <in-toolbar>FALSE</in-toolbar>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <target-type>make</target-type>
    <command-line>
       <arg>make</arg>
       <arg>%vars</arg>
       <arg>-f</arg>
       <arg>%attr(make'makefile,Makefile)</arg>
       <arg>%T</arg>
    </command-line>
</target>
"""

# This is an XML model for ant
Ant_Model_Template = """
<target-model name="ant" category="">
   <description>Build with ant</description>
   <command-line>
      <arg>ant</arg>
      <arg>%vars(-D)</arg>
       <arg>-buildfile</arg>
       <arg>%attr(ant'antfile,build.xml)</arg>
      <arg>%T</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <check label="Quiet mode" switch="-quiet" tip="Be extra quiet" />
     <check label="Project variables" switch="%vars"
           tip="Pass project variables to make" />
     <spin label="Multiprocessing" switch="-j" min="1" max="100" default="1"
           tip="Use N processes to carry out the compilations. On a multiprocess
or machine compilations will occur in parallel" />
   </switches>
</target-model>

<target model="ant" category="_Ant" name="Ant">
    <in-toolbar>FALSE</in-toolbar>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <target-type>ant</target-type>
    <command-line>
       <arg>ant</arg>
       <arg>%vars(-D)</arg>
       <arg>-buildfile</arg>
       <arg>%attr(ant'antfile,build.xml)</arg>
       <arg>%T</arg>
    </command-line>
</target>
"""

class Builder:
   def compute_buildfile (self):
      """Compute the build file to use. By default, we look in the project
         itself. If none is specified there, we default on the build file
         found in the same directory as the root project"""

      root_dir = dirname (Project.root().file().name())
      self.buildfile = Project.root().get_attribute_as_string \
           (self.build_file_attr, self.pkg_name)

      self.buildfile = join (root_dir, self.buildfile)
      if not isfile (self.buildfile):
         for f in self.default_build_files:
            self.buildfile = join (root_dir, f)
            if isfile (self.buildfile): break
            self.buildfile = None
      Logger ("MAKE").log ("Build file for " + self.pkg_name + " is " + `self.buildfile`)

   def read_targets (self):
      """Read all targets from the build file, and return a list targets"""
      return ""

   def compute_build_targets (self, name):
      return ""

   def on_compute_build_targets (self, hook, name):
      """Called when the a build target needs to be computed"""
      try:
         return self.compute_build_targets (name)

      except:
         Logger ("MAKE").log (traceback.format_exc())
         return ""

   def __init__ (self):
      self.targets = []
      Hook ("compute_build_targets").add (self.on_compute_build_targets)

class Makefile (Builder):
   def __init__ (self):
      self.pkg_name = "make"
      self.build_file_attr = "makefile"
      self.default_build_files = ["Makefile", "makefile"]
      Builder.__init__ (self)

   def read_targets (self):
      targets = ""
      matcher=re.compile ("^([^#.=%][^#=\(\)%]*?):[^#=:]*(#(.+))?$")
      f = file (self.buildfile)
      for line in f:
         matches=matcher.match (line)
         if matches:
            if matches.group (3) and matches.group (3) != "IGNORE":
               targets += " " + matches.group (1)
            else:
               ## Handle multiple targets on same line
               for target in matches.group (1).split():
                 targets += " " + target
      f.close ()
      return targets

   def compute_build_targets (self, name):
      if name == "make":
        self.compute_buildfile ()
        if self.buildfile:
          return self.read_targets ().strip()
      return ""

class Antfile (Builder):
   def __init__ (self):
      self.pkg_name = "ant"
      self.build_file_attr = "antfile"
      self.default_build_files = ["build.xml"]
      Builder.__init__ (self)

   def read_targets (self):
      targets = ""
      class MySaxDocumentHandler (handler.ContentHandler):
         def startElement (self, name, attrs):
            if name == "target":
               target=None
               description=None
               for attrName in attrs.keys():
                  if attrName=="name":
                      target=attrs.get(attrName)
                  if attrName=="description":
                      description=attrs.get(attrName)
               data += " " + str(target)

      parser = make_parser ()
      parser.setContentHandler (MySaxDocumentHandler ())
      inFile = open (self.buildfile, 'r')
      parser.parse (inFile)
      inFile.close ()
      return targets

   def compute_build_targets (self, name):
      if name == "ant":
        self.compute_buildfile ()
        if self.buildfile:
          return self.read_targets ().strip()
      return ""

ant_support=False

def on_gps_started (hook_name):
   Makefile()
   if ant_support:
      Antfile()

parse_xml (Make_Model)
parse_xml ("""
  <project_attribute
    name="Makefile"
    package="Make"
    editor_page="Make"
    editor_section="Make"
    hide_in="wizard library_wizard"
    description="Makefile to use for this project">
    <string type="file"/>
  </project_attribute>
  <project_attribute
    name="Make"
    package="Make"
    editor_page="Make"
    editor_section="Make"
    hide_in="wizard library_wizard properties"
    description="Deprecated, will be ignored">
    <string type=""/>
  </project_attribute>
  <project_attribute
    name="Switches"
    package="Make"
    editor_page="Make"
    editor_section="Make"
    hide_in="wizard library_wizard properties">
    <string type=""/>
  </project_attribute>""")
Hook ("gps_started").add (on_gps_started)

if os_utils.locate_exec_on_path ("ant"):
   try:
      from xml.sax import handler, make_parser
      ant_support=True
      parse_xml ("""
  <project_attribute
    name="Antfile"
    package="Ant"
    editor_page="Ant"
    editor_section="Ant"
    hide_in="wizard library_wizard"
    description="Ant build file to use for this project">
    <string type="file"/>
  </project_attribute>
  <project_attribute
    name="Ant"
    package="Ant"
    editor_page="Ant"
    editor_section="Ant"
    hide_in="wizard library_wizard properties">
    <string type=""/>
 </project_attribute>
 <project_attribute
    name="Switches"
    package="Ant"
    editor_page="Ant"
    editor_section="Ant"
    hide_in="wizard library_wizard properties">
    <string type=""/>
 </project_attribute>""")

   except:
      pass

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
                    Set this attribute to "<disabled>" to remove support for
                    makefiles in this mode.

   This script defines the following new project attributes in the "ant"
   package of a .gpr file:
       "ant.antfile": name of the Antfile to use. This can be an absolute
                    name, or a name relative to the directory containing the
                    root project file (ie the one loaded in GPS). This
                    attribute is optional. If unspecified, GPS will look for
                    "build.xml" in the directory containing the root project.
                    Set this attribute to "<disabled>" to remove support for
                    ant in this mode.

   GPS will systematically compile the application by passing the scenario
   variables (see the menu /Tools/Views/Scenario). For instance, this
   will result in calling
       make -f Makefile VARIABLE1=VALUE target
   or
       ant -buildfile build.xml -DVARIABLE1=VALUE target

   These scenario variables are defined in the project file, and should have
   the same name as in the Makefile or the AntFile.

   Any time a project is loaded or reloaded, GPS will update the /Build/Ant
   and /Build/Makefile menus to add entries corresponding to the targets
   available in the Makefile or AntFile.
   You can conveniently set key shortcuts for these entries through the menu
      /Edit/Key shortcuts

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
Make_Model_Template = """
<target-model name="make" category="">
   <description>Build with make</description>
   <command-line>
      <arg>make</arg>
      <arg>%vars</arg>
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
"""

# This is an XML model for ant
Ant_Model_Template = """
<target-model name="ant" category="">
   <description>Build with ant</description>
   <command-line>
      <arg>ant</arg>
      <arg>%vars(-D)</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
     <check label="Keep going" switch="-k"
            tip="Continue as much as possible after a compilation error" />
     <check label="Quiet mode" switch="-quiet" tip="Be extra quiet" />
     <check label="Project variables" switch="%vars"
           tip="Pass project variables to make" />
     <spin label="Multiprocessing" switch="-j" min="1" max="100" default="1"
           tip="Use N processes to carry out the compilations. On a multiprocessor machine compilations will occur in parallel" />
   </switches>
</target-model>
"""

# XML used to register new project attributes
Project_Attributes="""
  <project_attribute
    name="makefile"
    package="Make"
    editor_section="Make"
    description="Makefile to use for this project">
    <string type="file"/>
  </project_attribute>
  <project_attribute
    name="Make"
    package="Make"
    editor_section="Make"
    description="Make command to use when parsing Makefile">
    <string type="" default="make"/>
  </project_attribute>
  <project_attribute
    name="Switches"
    package="Make"
    editor_section="Make"
    description="Switches for the make command">
    <string type="" default="-k"/>
  </project_attribute>""")

  <project_attribute
    name="antfile"
    package="Ant"
    editor_section="Ant"
    description="Ant build file to use for this project">
    <string type="file"/>
  </project_attribute>
  <project_attribute
    name="Ant"
    package="Ant"
    editor_section="Ant"
    description="Ant command to use when parsing Makefile">
    <string type="" default="ant"/>
 </project_attribute>
 <project_attribute
    name="Switches"
    package="Ant"
    editor_section="Ant"
    description="Switches for the ant command">
    <string type="" default="-e"/>
 </project_attribute>""")
"""

class Builder:
   def remove_targets (self):
      for d in self.targets:
         t = d[1].strip ()
         if t != "IGNORE":
            t = t.replace ("\\", '\\\\')
            t = t.replace ("/", '\/')
            t = t.replace ("_", "__")
            try:
               BuildTarget (self.target + t).remove()
            except:
               pass

   def compute_buildfile (self):
      """Compute the build file to use. By default, we look in the project
         itself. If none is specified there, we default on the build file
         found in the same directory as the root project"""

      root_dir = dirname (Project.root().file().name())
      self.buildfile = Project.root().get_attribute_as_string \
           (self.build_file_attr, self.pkg_name)
      if self.buildfile == "<disabled>":
         self.buildfile = None
         return

      self.buildfile = join (root_dir, self.buildfile)
      if not isfile (self.buildfile):
         for f in self.default_build_files:
            self.buildfile = join (root_dir, f)
            if isfile (self.buildfile): break
            self.buildfile = None
      Logger ("MAKE").log ("Build file for " + self.pkg_name + " is " + `self.buildfile`)

   def read_targets (self):
      """Read all targets from the build file, and return a list of tuples
         (target, description)"""
      return []

   def add_target (self, target):
      """Add a given build target to the builder targets"""
      xml = "<target model=" + '"' + self.pkg_name + '"' + " category=" + \
          '"' + self.category + '"' + " name=" + '"' + self.target + target + \
          '"' + """>
      <icon>gps-build-all</icon>
      <launch-mode>MANUALLY</launch-mode>
      <read-only>TRUE</read-only>
      <command-line>
         <arg>""" + self.pkg_name + """</arg>
         <arg>""" + self.vars + """</arg>
         <arg>""" + self.file_switch + """</arg>
         <arg>""" + self.buildfile + """</arg>
         <arg>""" + target + """</arg>
      </command-line></target>"""
      parse_xml (xml)


   def on_project_view_changed (self, hook):
      """Called when the project view has changed, and thus we should
         reparse the build file"""
      try:
         self.remove_targets ()
         self.compute_buildfile ()
         if self.buildfile:
            self.read_targets ()
            for d in self.targets:
                t = d[1].strip ()
                if t != "IGNORE":
                   t = t.replace ("\\", '\\\\')
                   t = t.replace ("/", '\/')
                   t = t.replace ("_", "__")
                   try:
                      BuildTarget (self.target + t).remove()
                   except:
                      pass
                   self.add_target (t)

      except:
         Logger ("MAKE").log (traceback.format_exc())
         pass

   def __init__ (self):
      self.targets = []
      Hook ("project_view_changed").add (self.on_project_view_changed)
      self.on_project_view_changed ("project_view_changed")

class Makefile (Builder):
   def __init__ (self):
      self.pkg_name = "make"
      self.category = "_Makefile"
      self.target = "Make "
      self.build_file_attr = "makefile"
      self.vars = "%vars"
      self.file_switch = "-f"
      self.default_build_files = ["Makefile", "makefile"]
      Builder.__init__ (self)

   def read_targets (self):
      self.targets = []
      matcher=re.compile ("^([^#.=%][^#=\(\)%]*?):[^#=:]*(#(.+))?$")
      f = file (self.buildfile)
      for line in f:
         matches=matcher.match (line)
         if matches:
            if matches.group (3):
               self.targets.append ([matches.group (1), matches.group (3)])
            else:
               ## Handle multiple targets on same line
               for target in matches.group (1).split():
                 self.targets.append ([target, target])
      f.close ()

class Antfile (Builder):
   def __init__ (self):
      self.pkg_name = "ant"
      self.category = "_Ant"
      self.target = "Ant "
      self.build_file_attr = "antfile"
      self.vars = "%vars(-D)"
      self.file_switch = "-buildfile"
      self.default_build_files = ["build.xml"]
      Builder.__init__ (self)

   def read_targets (self):
      data = []
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
               if description:
                  data.append ([str(target), str(description)])
               else:
                  data.append ([str(target), str(target)])

      parser = make_parser ()
      parser.setContentHandler (MySaxDocumentHandler ())
      inFile = open (self.buildfile, 'r')
      parser.parse (inFile)
      inFile.close ()
      return data

ant_support=False

def on_gps_started (hook_name):
   Makefile()
   if ant_support:
      Antfile()

parse_xml (Make_Model_Template)
parse_xml (Project_Attributes)
Hook ("gps_started").add (on_gps_started)

if os_utils.locate_exec_on_path ("ant"):
   try:
      from xml.sax import handler, make_parser
      ant_support=True
   except:
      pass


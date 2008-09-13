"""Provides support for building through make or ant

   This script defines the following new project attributes, in the "make"
   package of a .gpr file. These can of course be edited graphically through
   the project properties editor.
       "make.make": Name of the make command to run. It defaults to "make",
                    although on some systems it should be "gmake".
       "make.makefile": name of the Makefile to use. This can be an absolute
                    name, or a name relative to the directory containing the
                    root project file (ie the one loaded in GPS). This
                    attribute is optional. If unspecified, GPS will look for
                    either "Makefile" or "makefile" in the directory
                    containing the root project.
                    Set this attribute to "<disabled>" to remove support for
                    makefiles in this mode.
       "make.switches": list of extra switches to pass the Makefile. By
                    default, "-k" is used. If you want to have the progress bar
                    working for a project containing Ada sources, it is a good
                    idea to pass gnatmake the -d switch, which can be done
                    through a make variable.

   This script defines the following new project attributes, in the "ant"
   package of a .gpr file. These can of course be edited graphically through
   the project properties editor.
       "ant.ant": Name of the make command to run. It defaults to "ant"
       "ant.antfile": name of the Antfile to use. This can be an absolute
                    name, or a name relative to the directory containing the
                    root project file (ie the one loaded in GPS). This
                    attribute is optional. If unspecified, GPS will look for
                    "build.xml" in the directory containing the root project.
                    Set this attribute to "<disabled>" to remove support for
                    ant in this mode.
       "ant.switches": list of extra switches to pass the Makefile. By
                    default, "-e" is used. If you want to have the progress bar
                    working for a project containing Ada sources, it is a good
                    idea to pass gnatmake the -d switch, which can be done
                    through an ant variable.

   GPS will systematically compile the application by passing the scenario
   variables (see the menu /Tools/Views/Scenario). For instance, this
   will result in calling
       make -f Makefile VARIABLE1=VALUE target
   or
       ant -f build.xml VARIABLE1=VALUE target

   These scenario variables are defined in the project file, and should have
   the same name as in the Makefile or the AntFile.

   Any time a project is loaded or reloaded, GPS will update the /Build/Ant
   and /Build/Makefile menus to add entries corresponding to the targets
   available in the Makefile or AntFile.
   You can conveniently set key shortcuts for these entries through the menu
      /Edit/Key shortcuts

   By default, the name in the menu will be the name of the targets found in
   the build file. However, if your makefile contains lines similar to:

      target: dependency1 dependency2 # menu name

   then the part after the '#' sign will be used as the menu name
   As a special case, when the comment is "IGNORE", as in:

      target: dependency1 dependency2 # IGNORE

   then that target is now displayed in the menu

   A similar behavior is applied for build.xml files for ant, where the
   "description" attribute of the <target> node is taken into account.

   When you select one of the new menus, GPS will run ant or make, and parse
   error messages to display them in the locations window as usual.
"""


############################################################################
## No user customization below this line
############################################################################

locations_category = "Builder results"
## Name of the category in the Locations window in which error messages
## will be displayed

from GPS import *
from os.path import *
import re
import os
import traceback

Preference ("Plugins/Makefile/ant_support").create (
  "Ant support", "boolean",
  """If enabled, the Makefile plugin will have support for ant, ie it will support an ant file found in the toplevel directory.
This is disabled by default because adding this support will implicitly load the libexpat library from the system, which conflicts in some cases with the one linked with the gtk+ distribution. This can result in fatal errors crashing GPS. However, it appears to work fine on some systems.
You need to restart GPS to take changes into account.""",
  False)

Preference ("Plugins/Makefile/ant switches").create (
  "Ant switches", "string",
  """Extra switches that need to be passed to ant""",
  "")
Preference ("Plugins/Makefile/make switches").create (
  "Make switches", "string",
  """Extra switches that need to be passed to make""",
  "")

if Preference ("Plugins/Makefile/ant_support").get():
   from xml.sax import handler, make_parser

from gps_utils.console_process import Console_Process

class Makefile_Process (Console_Process):
   # This regexp matches the output of the GNAT compiler to report its progress. Change
   # this if your compiler outputs this information differently.
   progress_regexp = "^completed ([0-9]+) out of ([0-9]+) \\(([^\n]*)%\\)\\.\\.\\.\\n"

   def on_output (self, unmatched, matched):
      """Called when new output is available from make"""
      Console_Process.on_output (self, unmatched, matched)
      self.cumulated_output = self.cumulated_output + unmatched + matched
      Locations.parse \
        (output   = unmatched + matched,
         category = locations_category)

   def on_exit (self, status, remaining_output):
      Console_Process.on_exit (self, status, remaining_output)

      # Among other things, this automatically reparses xref if the user
      # has set the preference for it
      Hook ("compilation_finished").run (locations_category)
      Codefix.parse (category=locations_category,
                     output=self.cumulated_output)

   def __init__ (self, process, args=""):
      Locations.remove_category (locations_category)
      Console_Process.__init__ (self, process, args, close_on_exit=False)
      self.clear()
      self.write (process + " " + args + "\n")
      MDI.get ("Messages").raise_window()
      self.cumulated_output = ""

class Builder:
   def compute_buildfile (self):
      """Return the build file to use. By default, we look in the project
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

   def spawn (self, target):
      """Spawn the compilation command"""

      # Make sure everything if saved if needed
      if not Hook ("compilation_starting").run_until_failure \
         (locations_category, False):
         return

      project  = Project.root()
      switches = project.get_attribute_as_string\
        (self.switches_attr,   self.pkg_name)
      make     = project.get_attribute_as_string\
        (self.build_cmd_attr, self.pkg_name)

      # Change the directory instead of using -C, so that we are compatible
      # with any version of make, not only GNU make
      os.chdir (dirname (self.buildfile))

      args = switches + " " + self.extra_switches + " " \
         + " -f " + basename (self.buildfile) + " " + target + " " \
         + self.get_scenario_variables(project)
      return Makefile_Process (make, args)

   def get_scenario_variables(self,project):
       return ""

   def destroy_menus (self):
      """Destroy all menus associated with the build file"""

      if self.menus:
        for m in self.menus:
           m.destroy()
        self.menus = []

   def edit_buildfile (self, menu):
      """Open an editor to edit the current Makefile"""
      EditorBuffer.get (File (self.buildfile))

   def on_build_target (self, menu):
      """Build a specific target"""
      self.spawn (menu.target)

   def read_targets (self):
      """Read all targets from the build file, and return a list of tuples
         (target, description)"""
      return []

   def on_project_view_changed (self, hook):
      """Called when the project view has changed, and thus we should
         reparse the build file"""
      try:
         self.destroy_menus()
         self.compute_buildfile ()
         if self.buildfile:
            self.menus.append \
              (Menu.create \
                  (self.menu_name + self.edit_menu,
                   ref = "Make", add_before=False,
                   on_activate=self.edit_buildfile))

            data = self.read_targets ()
            for d in data:
                t = d[1].strip ()
                if t != "IGNORE":
                   t = t.replace ("\\", '\\\\')
                   t = t.replace ("/", '\/')
                   m = Menu.create \
                     (self.menu_name + t,
                      on_activate = self.on_build_target)
                   m.target = d[0]
                   self.menus.append (m)
            self.menus.append (Menu.get (self.menu_name))

      except:
         Logger ("MAKE").log (traceback.format_exc())
         pass

   def __init__ (self):
      self.menus = []
      Hook ("project_view_changed").add (self.on_project_view_changed)
      self.on_project_view_changed ("project_view_changed")

#########################################
## Specialized builder for Makefile
#########################################

class Makefile (Builder):
    def __init__ (self):
       self.pkg_name = "make"
       self.build_file_attr = "makefile"
       self.build_cmd_attr = "make"
       self.switches_attr = "switches"
       self.default_build_files = ["Makefile", "makefile"]
       self.extra_switches = Preference ("Plugins/Makefile/make switches").get()
       self.menu_name = "/Build/Makefile/"
       self.edit_menu = "Edit Makefile"
       Builder.__init__ (self)
    def get_scenario_variables(self,project):
        return project.scenario_variables_cmd_line ("")
    def read_targets (self):
       data = []
       matcher=re.compile ("^([^#.=%][^#=\(\)%]*?):[^#=:]*(#(.+))?$")
       f = file (self.buildfile)
       for line in f:
          matches=matcher.match (line)
          if matches:
             if matches.group (3):
                data.append ([matches.group (1), matches.group (3)])
             else:
                ## Handle multiple targets on same line
                for target in matches.group (1).split():
                   data.append ([target, target])
       f.close ()
       return data

#########################################
## Specialized builder for Ant
#########################################

class Antfile (Builder):
    def __init__ (self):
       self.pkg_name = "ant"
       self.build_file_attr = "antfile"
       self.build_cmd_attr = "ant"
       self.switches_attr = "switches"
       self.default_build_files = ["build.xml"]
       self.extra_switches = Preference ("Plugins/Makefile/ant switches").get()
       self.menu_name = "/Build/Ant/"
       self.edit_menu = "Edit ant file"
       Builder.__init__ (self)
    def get_scenario_variables(self,project):
        return project.scenario_variables_cmd_line ("-D")

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

def register_project_attributes ():
      parse_xml("""
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

      parse_xml("""
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

def on_gps_started (hook_name):
   Makefile()
   if Preference ("Plugins/Makefile/ant_support").get():
      Antfile()


## Register the project attributes early so that the project loaded
## from the command line doesn't display warnings

register_project_attributes ()

Hook ("gps_started").add (on_gps_started)

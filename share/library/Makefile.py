"""Provides support for building through Makefiles

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
                    containing the root project
       "make.switches": list of extra switches to pass the Makefile. By
                    default, "-k" is used.
   GPS will systematically set the Makefile variables corresponding with the
   scenario variables (see the menu /Tools/Views/Scenario). For instance, this
   will result in calling
       make -f Makefile VARIABLE1=VALUE target
   These scenario variables are defined in the project file, and should have
   the same name as in the Makefile.
      
   Any time a project is loaded or reloaded, GPS will update the /Build/Make
   menu to add entries corresponding to the targets available in the Makefile.
   You can conveniently set key shortcuts for these entries through the menu
      /Edit/Key shortcuts
   By default, the name in the menu will be the name of the Makefile target.
   However, if your makefile contains lines similar to:
      target: dependency1 dependency2 # menu name
   then the part after the '#' sign will be used as the menu name

   When you select one of the new menus, GPS will run make, and parse error
   messages to display them in the locations window as usual.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

locations_category = "Builder results"
## Name of the category in the Locations window in which error messages
## will be displayed

extra_make_switches = ""
## List of switches that should always be added to make.
## We use this variable instead of overriding the default value for the
## project attribute because the new attribute is declared as soon as this
## script is loaded, and therefore changing the python variable afterward
## has no impact


############################################################################
## No user customization below this line
############################################################################

from GPS import *
from os.path import *
import re
import os

class Console_Process (Console, Process):

   # This regexp matches the output of the GNAT compiler to report its progress. Change
   # this if your compiler outputs this information differently.
   progress_regexp = "^completed ([0-9]+) out of ([0-9]+) \\(([^\n]*)%\\)\\.\\.\\.\\n"

   def on_output (self, unmatched, matched):
      """Called when new output is available from make"""
      self.write (unmatched + matched)
      self.cumulated_output = self.cumulated_output + unmatched + matched
      Locations.parse \
        (output   = unmatched + matched,
         category = locations_category)

   def on_input (self, input):
      """Called when the user has sent input in the console"""
      self.send (input)

   def on_exit (self, status, remaining_output):
      self.write (remaining_output)
      self.write ("exit status: " + `status`)

      # Among other things, this automatically reparses xref if the user
      # has set the preference for it
      Hook ("compilation_finished").run (locations_category)
      Codefix.parse (category=locations_category,
                     output=self.cumulated_output)

   def on_destroy (self):
      """If the console is destroyed, we kill the make process as well"""
      self.kill ()

   def __init__ (self, process, args=""):
      Locations.remove_category (locations_category)
      Console.__init__ (self, "",
                        on_input=Console_Process.on_input,
                        on_destroy=Console_Process.on_destroy,
                        force=True)
      self.clear()
      self.write (process + " " + args + "\n")
      MDI.get ("Messages").raise_window()
      self.cumulated_output = ""
      Process.__init__ (self, process + ' ' + args + "",
                        regexp="^.+\\n",
                        progress_regexp=Console_Process.progress_regexp,
                        on_exit=Console_Process.on_exit,
                        on_match=Console_Process.on_output)

class Makefile:
   def compute_makefile (self):
      """Return the Makefile to use. By default, we look in the project itself.
         If none is specified there, we default on the Makefile or makefile found
         in the same directory as the root project"""
      root_dir = dirname (Project.root().file().name())
      self.makefile = Project.root().get_attribute_as_string ("makefile","make")
      if not isfile (self.makefile):
         self.makefile = join (root_dir, self.makefile)
      if not isfile (self.makefile):
         self.makefile = join (root_dir, "Makefile")
      if not isfile (self.makefile):
         self.makefile = join (root_dir, "makefile")
      if not isfile (self.makefile):
         self.makefile = None
      Logger ("Makefile").log ("Makefile used is " + `self.makefile`)

   def spawn (self, target):
      # Make sure everything if saved if needed
      if not Hook ("compilation_starting").run_until_failure \
         (locations_category, False):
         return

      project  = Project.root()
      switches = project.get_attribute_as_string ("switches", "make")
      make     = project.get_attribute_as_string ("make",     "make")

      # Change the directory instead of using -C, so that we are compatible
      # with any version of make, not only GNU make
      os.chdir (dirname (project.file().name()))

      args = switches + " " + extra_make_switches + " " \
         + " -f " + self.makefile + " " + target \
         + project.scenario_variables_cmd_line ("")
      return Console_Process (make, args) 

   def destroy_menus (self):
      """Destroy all menus associated with the Makefile"""
      if self.menus:
        for m in self.menus:
           m.destroy()
        self.menus = None

   def edit_makefile (self, menu):
      """Open an editor to edit the current Makefile"""
      EditorBuffer.get (File (self.makefile))

   def on_build_target (self, menu):
      self.spawn (menu.target)

   def on_project_view_changed (self, hook):
      """Called when the project view has changed, and thus we should
         reparse the Makefile"""
      try:
         self.destroy_menus()
         if self.makefile:
            self.menus.append \
               (Menu.create \
                  ("/Build/Makefile/Edit Makefile",
                   ref = "Make", add_before=False,
                   on_activate=self.edit_makefile))
             
            matcher=re.compile ("^([^#.=%][^#=\(\)%]*?):[^#]*(#(.+))?")
            f = file (self.makefile)
            for line in f:
               matches=matcher.match (line)
               if matches:
                   if matches.group (3):
                      m = Menu.create \
                        ("/Build/Makefile/" + matches.group (3).strip(),
                         on_activate = self.on_build_target)
                      m.target = matches.group (1)
                      self.menus.append (m)
                   else:
                      ## Handle multiple targets on same line
                      for target in matches.group (1).split():
                        m = Menu.create \
                         ("/Build/Makefile/" + target.strip(),
                         on_activate = self.on_build_target)
                        m.target = target
                        self.menus.append (m)
            
            f.close()
      except:
         pass

   def on_project_changed (self, hook):
      self.destroy_menus ()
      self.compute_makefile ()

   def __init__ (self):
      self.menus = []
      self.compute_makefile ()
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

      Hook ("project_view_changed").add (self.on_project_view_changed)
      Hook ("project_changed").add (self.on_project_changed)
      self.on_project_changed ("project_changed")
      self.on_project_view_changed ("project_view_changed")

def on_gps_started (hook_name):
   Makefile()
   

Hook ("gps_started").add (on_gps_started)

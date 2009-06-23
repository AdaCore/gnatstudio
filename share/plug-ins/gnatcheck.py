"""gnatcheck support for GPS

This plug-in adds support for gnatcheck, a coding standard checker
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os, os.path, re, string, pygtk, traceback
import os_utils
pygtk.require('2.0')
import gobject, gtk
from gps_utils.gnatcheck_rules_editor import *

gnatcheck = None

class rulesSelector(gtk.Dialog):
   """Dialog used to select a coding standard file before launching gnatcheck."""

   def __init__ (self, projectname, defaultfile):
      gtk.Dialog.__init__ (self, title="Select a coding standard file", parent=GPS.MDI.current().pywidget().get_toplevel(), flags=gtk.DIALOG_MODAL, buttons=None)

      # OK - Cancel buttons
      self.okButton=gtk.Button ('OK')
      self.okButton.connect ('clicked', self.on_ok)
      self.okButton.show()
      self.action_area.pack_start(self.okButton, True, True, 0)

      self.cancelButton=gtk.Button ('Cancel')
      self.cancelButton.connect ('clicked', self.on_cancel)
      self.cancelButton.show()
      self.action_area.pack_start(self.cancelButton, True, True, 0)

      label=gtk.Label("No check switches are defined for project " + projectname + "\n" +
                      "Please enter a coding standard file containing the desired gnatcheck rules:");
      label.show()
      self.vbox.pack_start (label, False, False, 0)

      hbox = gtk.HBox()
      hbox.show()
      self.vbox.pack_start (hbox, False, False, 0)

      self.fileEntry = gtk.Entry()
      self.fileEntry.set_editable(True)
      self.fileEntry.show()
      hbox.pack_start (self.fileEntry, True, True, 0)

      if None != defaultfile:
         self.fileEntry.set_text (defaultfile)
      self.fileEntry.connect ('changed', self.on_file_entry_changed)
      self.on_file_entry_changed()

      button=gtk.Button ('Browse')
      button.connect ('clicked', self.on_coding_standard_file_browse)
      button.show()
      hbox.pack_start (button, False, False, 0)

   def get_file (self):
      return self.fileEntry.get_text()

   def on_file_entry_changed (self, *args):
      """Callback when the file entry changed"""
      name = self.fileEntry.get_text()
      if name == "":
        self.okButton.set_sensitive(False)
      else:
        self.okButton.set_sensitive(True)

   def on_coding_standard_file_browse (self, *args):
      """Callback to coding standard 'Browse' button"""
      file = GPS.MDI.file_selector ()
      if file.name() != "":
         self.fileEntry.set_text (file.name())

   def on_ok (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_OK)

   def on_cancel (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_CANCEL)

class gnatCheckProc:
   """This class controls the gnatcheck execution"""
   def __init__ (self):
      self.rules_file = None
      self.rules = None

      self.locations_string = "Coding Standard violations"
      self.gnatCmd = ""

   def updateGnatCmd(self):
      self.gnatCmd = GPS.Project.root().get_attribute_as_string("gnat", "ide")

      if self.gnatCmd == "":
         self.gnatCmd = "gnat"
      if not os.path.isfile (self.gnatCmd):
         self.gnatCmd = os_utils.locate_exec_on_path (self.gnatCmd)
      if self.gnatCmd == "":
         GPS.Console ("Messages").write ("Error: 'gnat' is not in the path.\n")
         GPS.Console ("Messages").write ("Error: Could not initialize the gnatcheck module.\n")

   def edit(self):
      global ruleseditor
      prev_cmd = self.gnatCmd
      self.updateGnatCmd()

      if self.gnatCmd == "":
         return

      # gnat check command changed: we reinitialize the rules list
      if prev_cmd != self.gnatCmd:
         self.rules = get_supported_rules(self.gnatCmd)

      # we retrieve the coding standard file from the project
      for opt in GPS.Project.root().get_attribute_as_list("default_switches", package="check", index="ada"):
        res = re.split ("^\-from\=(.*)$", opt)
        if len(res)>1:
          self.rules_file = GPS.File (res[1]).name()

      try:
        ruleseditor = rulesEditor(self.rules, self.rules_file)
        ruleseditor.run()
        fname = ruleseditor.get_filename()
        if fname != "":
           self.rules_file = fname
        ruleseditor.destroy()
      except:
         GPS.Console ("Messages").write ("Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc()))

   def parse_output (self, msg):
     # gnatcheck sometimes displays incorrectly formatted warnings (not handled by GPS correctly then)
     # let's reformat those here:
     # expecting "file.ext:nnn:nnn: msg"
     # receiving "file.ext:nnn:nnn msg"
     res = re.split ("^([^:]*[:][0-9]+:[0-9]+)([^:0-9].*)$", msg)
     if len (res) > 3:
       msg = res[1]+":"+res[2]
     GPS.Locations.parse (msg, self.locations_string)
     GPS.Codefix.parse (msg, self.locations_string)

   def on_match (self, process, matched, unmatched):
      if unmatched == "\n":
         GPS.Console ("Messages").write (self.msg+unmatched)
         self.parse_output (self.msg)
         self.msg = ""
      self.msg += matched

   def on_exit (self, process, status, remaining_output):
      if self.msg != "":
         GPS.Console ("Messages").write (self.msg)
         GPS.Locations.parse (self.msg, self.locations_string)
         self.parse_output (self.msg)
         self.msg = ""

   def internalSpawn (self, filestr, project, recursive=False):
      need_rules_file = False
      opts = project.get_attribute_as_list("default_switches", package="check", index="ada")
      if len(opts) == 0:
         need_rules_file = True
         opts = GPS.Project.root().get_attribute_as_list("default_switches", package="check", index="ada")
         for opt in opts:
           res = re.split ("^\-from\=(.*)$", opt)
           if len(res)>1:
             rootdir = GPS.Project.root().file().directory()
             self.rules_file = rootdir+res[1]

      if need_rules_file:
         selector = rulesSelector (project.name(), self.rules_file)

         if selector.run() == gtk.RESPONSE_OK:
            self.rules_file = selector.get_file()
            selector.destroy()
         else:
            selector.destroy()
            return;

      self.updateGnatCmd()

      if self.gnatCmd == "":
         GPS.Console ("Messages").write ("Error: could not find gnatcheck");
         return
      # launch gnat check with current project
      cmd = self.gnatCmd + " check -P" + project.file().name()
      # also analyse subprojects ?
      if recursive:
        cmd += " -U"
      # define the scenario variables
      scenario = GPS.Project.scenario_variables()
      if scenario != None:
         for i, j in scenario.iteritems():
            cmd += " -X" + i + "=" + j
      # use progress
      cmd +=  " -dd"

      # now specify the files to check
      cmd += " " + filestr

      if need_rules_file:
         cmd += " -rules -from=" + self.rules_file

      # clear the Checks category in the Locations view
      if GPS.Locations.list_categories().count (self.locations_string) > 0:
         GPS.Locations.remove_category (self.locations_string)

      self.msg = ""
      process = GPS.Process (cmd, "^.+$",
                             on_match=self.on_match,
                             on_exit=self.on_exit,
                             progress_regexp="^ *completed (\d*) out of (\d*) .*$",
                             progress_current = 1,
                             progress_total = 2,
                             remote_server = "Tools_Server",
                             show_command = True)

   def check_project (self, project, recursive=False):
      try:
         self.internalSpawn ("", project, recursive)
      except:
         GPS.Console ("Messages").write ("Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc()))

   def check_file (self, file):
      try:
         self.internalSpawn (file.name(), file.project())
      except:
         GPS.Console ("Messages").write ("Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc()))

   def check_files (self, files):
      try:
         filestr = ""
         for f in files:
            filestr += f.name() + " "
         self.internalSpawn (filestr, files[0].project());
      except:
         GPS.Console ("Messages").write ("Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc()))

# Contextual menu for checking files
class contextualMenu (GPS.Contextual):
   def __init__ (self):
      GPS.Contextual.__init__ (self, "Check Coding Standard")
      self.create (on_activate = self.on_activate,
                   filter      = self.filter,
                   label       = self.label)

   def filter (self, context):
      global gnatcheckproc
      self.desttype = "none"
      if not isinstance(context, GPS.FileContext):
         return False
      try:
         # might be a file
         self.desttype = "file"
         self.file = context.file()
         if self.file.language().lower() != "ada":
            return False
         srcs = GPS.Project.root().sources (recursive = True)
         found = False
         for f in srcs:
            if f.name().lower() == self.file.name().lower():
               found = True
               break
         return found
      except:
         try:
           self.desttype = "dir"
           # verify this is a dir
           self.dir = context.directory()
           # check this directory contains ada sources
           srcs = GPS.Project.root().sources (recursive = True)
           found = False
           self.files = []
           for f in srcs:
              if f.name().lower().find (dir.lower()) == 0:
                 if f.language().lower() == "ada":
                   self.files.append (f)
                   found = True
           return found
         except:
            try:
               # this is a project file
               self.desttype = "project"
               self.project = context.project()
               srcs = self.project.sources (recursive = False)
               found = False
               self.files = []
               for f in srcs:
                  if f.language().lower() == "ada":
                     self.files.append (f)
                     found = True
               return found
            except:
               # Weird case where we have a FileContext with neither file,
               # dir or project information...
               # This may happen if the file is newly created, and has not
               # been saved yet, thus does not exist on the disk.
               return False

   def label (self, context):
      if self.desttype == "file":
         return "Check Coding standard of " + os.path.basename(self.file.name())
      elif self.desttype == "dir":
         return "Check Coding standard of files in " + os.path.basename (self.dir)
      elif self.desttype == "project":
         return "Check Coding standard of files in " + self.project.name()
      return ""

   def on_activate (self, context):
      global gnatcheckproc
      if self.desttype == "file":
         gnatcheckproc.check_file(self.file)
      elif self.desttype == "project":
         gnatcheckproc.check_project(self.project)
      else:
         gnatcheckproc.check_files(self.files)

# create the menus instances.

def on_gps_started (hook_name):
   global gnatcheckproc
   gnatcheckproc = gnatCheckProc()
   contextualMenu()

   GPS.parse_xml ("""
  <tool name="GnatCheck" package="Check" index="Ada" override="false">
     <language>Ada</language>
     <switches lines="1" sections="-rules">
        <check label="process RTL units" switch="-a" line="1"/>
        <check label="debug mode" switch="-d" line="1"/>
        <field label="Coding standard file" switch="-from" separator="=" as-file="true" line="1" section="-rules"/>
     </switches>
  </tool>
  <action name="gnatcheck root project" category="Coding Standard" output="none">
    <description>Check Coding Standard of the root project</description>
    <shell lang="python">gnatcheck.gnatcheckproc.check_project (GPS.Project.root())</shell>
  </action>
  <action name="gnatcheck root project recursive" category="Coding Standard" output="none">
    <description>Check Coding Standard of the root project and its subprojects</description>
    <shell lang="python">gnatcheck.gnatcheckproc.check_project (GPS.Project.root(), True)</shell>
  </action>
  <action name="gnatcheck file" category="Coding Standard" output="none">
    <description>Check Coding Standard of the selected file</description>
    <filter id="Source editor"/>
    <shell lang="python">gnatcheck.gnatcheckproc.check_file (GPS.EditorBuffer.get().file())</shell>
  </action>
  <action name="edit gnatcheck rules" category="Coding Standard" output="none">
    <description>Edit the Coding Standard file (coding standard)</description>
    <shell lang="python">gnatcheck.gnatcheckproc.edit ()</shell>
  </action>
  <submenu>
    <title>Tools</title>
    <submenu after="Browsers">
      <title>Coding _Standard</title>
      <menu action="edit gnatcheck rules">
        <title>_Edit rules file</title>
      </menu>
      <menu action="gnatcheck root project recursive">
        <title>Check root project and _subprojects</title>
      </menu>
      <menu action="gnatcheck root project">
        <title>Check root _project</title>
      </menu>
      <menu action="gnatcheck file">
        <title>Check current _file</title>
      </menu>
    </submenu>
  </submenu>""");

GPS.Hook ("gps_started").add (on_gps_started)

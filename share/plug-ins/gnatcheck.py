#
#  gnatcheck Python support for GPS integration
#
import GPS, os, os.path, re, string, pygtk
pygtk.require('2.0')
import gobject, gtk

gnatcheck = None


def locate_exec_on_path (prog):
    """Utility function to locate an executable on path."""

    alldirs = string.split (os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir,prog) for dir in alldirs]:
        if os.path.isfile(file) or os.path.isfile(file+".exe"):
            return file
    return ""

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
      self.fileEntry.set_text (file.name())

   def on_ok (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_OK)

   def on_cancel (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_CANCEL)

class rulesEditor(gtk.Dialog):
   """Dialog used to edit the coding standard file."""

   def __init__ (self, rules, warnings, projectfile):
      # call parent __init__
      gtk.Dialog.__init__ (self, title="Coding Standard editor", parent=GPS.MDI.current().pywidget().get_toplevel(), flags=gtk.DIALOG_MODAL, buttons=None)
      self.set_default_size (600, 400)
      self.set_name ("CodingStandardEditor")

      # rules_list contains the list of rules extracted from gnatcheck -h
      self.rules_list = rules
      self.warnings_list = warnings
      # list of widgets (de)activating the different rules
      self.rules_widgets = []
      self.warnings_widgets = []
      # additional switches that might be defined in the coding standard file
      self.additional_switches = []

      # Selection of a coding standard file
      hbox = gtk.HBox()
      hbox.show()
      self.vbox.pack_start (hbox, False, False, 0)

      label=gtk.Label("coding standard file:")
      label.show()
      hbox.pack_start (label, False, False, 0)

      hbox = gtk.HBox()
      hbox.show()
      self.vbox.pack_start (hbox, False, False, 0)

      self.fileEntry = gtk.Entry()
      # Connect callbacks on the file entry modifications
      self.fileEntry.set_editable(True)
      self.fileEntry.show()
      if None != projectfile:
         self.fileEntry.set_text (projectfile.name())
      self.fileEntry.connect ('changed', self.on_file_entry_changed)
      hbox.pack_start (self.fileEntry, True, True, 0)

      button=gtk.Button ('Browse')
      button.connect ('clicked', self.on_coding_standard_file_browse)
      button.show()
      hbox.pack_start (button, False, False, 0)

      # Scrolled window containing the rules
      scrolled = gtk.ScrolledWindow ()
      scrolled.set_policy (gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
      scrolled.show()
      self.vbox.pack_start (scrolled, True, True, 0);

      self.switchvbox = gtk.VBox()
      self.switchvbox.show()
      scrolled.add_with_viewport (self.switchvbox)

      # Convenient button to uncheck all rules at once
      hbox = gtk.HBox()
      hbox.show()
      self.switchvbox.pack_start (hbox, False, False, 0)
      button=gtk.Button('Unselect all rules')
      button.connect ('clicked', self.on_unckeck_all)
      button.show()
      hbox.pack_start (button, False, False, 0)

      label=gtk.Label()
      label.set_markup("<span weight='bold' size='large'>Check rules</span>")
      label.show()
      self.switchvbox.pack_start (label, False, False, 0)

      self.tips = gtk.Tooltips()

      # Add rules widgets
      for j in self.rules_list:
        hbox = gtk.HBox()
        hbox.show()
        res = re.split ("^([^\(]*)\(([^\)]*)\)$", j[0])
        if len(res) > 1:
          # rule requiring a parameter value
          adj = gtk.Adjustment (0, 0, 99, 1, 1, 1)
          spin = gtk.SpinButton (adj)
          spin.show()
          hbox.pack_start (spin, False, False, 0)
          switch = res[1] + ':' + res[2]
          self.rules_widgets.append ([spin, switch, ">"])
        else:
          # simple on/off rule
          check = gtk.CheckButton()
          check.show()
          hbox.pack_start (check, False, False, 0)
          switch = j[0]
          self.rules_widgets.append ([check, switch, ""])
        label = gtk.Label (j[1])
        label.show()
        hbox.pack_start (label, False, False, 0)

        self.tips.set_tip (hbox, switch)

        self.switchvbox.pack_start (hbox, True, True, 0)

      # Add rule for compiler Warnings
      hbox = gtk.HBox()
      hbox.show()
      self.warnings_check = gtk.CheckButton()
      self.warnings_check.show()
      hbox.pack_start (self.warnings_check, False, False, 0)
      label = gtk.Label ("Activate compiler warnings")
      label.show()
      hbox.pack_start (label, False, False, 0)
      self.tips.set_tip (hbox, "+RWarnings")
      self.switchvbox.pack_start (hbox, True, True, 0)

      label=gtk.Label()
      label.set_markup("<span weight='bold' size='large'>Compiler warnings</span>")
      label.show()
      self.switchvbox.pack_start (label, False, False, 0)

      # Add compiler warnings widgets
      for j in self.warnings_list:
        hbox = gtk.HBox()
        hbox.show()

        check = gtk.CheckButton()
        check.show()
        hbox.pack_start (check, False, False, 0)
        switch = j[0]
        if j[0] == "a":
           check.connect('toggled', self.on_gnatwa_toggled)
        self.warnings_widgets.append ([check, switch, j[2]])

        label = gtk.Label ()
        label.set_markup (re.sub('^turn on', "turn <span color='blue'>on</span>", re.sub('^turn off', "turn <span color='red'>off</span>", j[1])))
        label.show()
        hbox.pack_start (label, False, False, 0)
        self.tips.set_tip (hbox, switch)

        self.switchvbox.pack_start (hbox, True, True, 0)

      self.warnings_check.connect('toggled', self.on_warnings_toggled)
      self.on_warnings_toggled()

      # Save - Cancel buttons
      self.saveButton=gtk.Button ('Save')
      self.saveButton.connect ('clicked', self.on_save)
      self.saveButton.show()
      self.action_area.pack_start(self.saveButton, True, True, 0)

      self.cancelButton=gtk.Button ('Cancel')
      self.cancelButton.connect ('clicked', self.on_cancel)
      self.cancelButton.show()
      self.action_area.pack_start(self.cancelButton, True, True, 0)

      self.on_file_entry_changed()

   def on_warnings_toggled (self, *args):
      """Callback when the compiler warnings switch is toggled"""
      for sw in self.warnings_widgets:
        sw[0].set_sensitive(self.warnings_check.get_active())

   def on_gnatwa_toggled (self, *args):
      """Callback when the gnatwa switch is toggled"""
      for sw in self.warnings_widgets:
        if sw[1] == "a":
          if not sw[0].get_active():
             return
        elif sw[2]:
          sw[0].set_active(True)

   def on_file_entry_changed (self, *args):
      """Callback when the file entry changed"""
      name = self.fileEntry.get_text()
      if name == "":
        self.tips.disable()
        self.switchvbox.foreach (deactivate)
        self.saveButton.set_sensitive(False)
      else:
        self.tips.enable()
        self.switchvbox.foreach (activate)
        self.saveButton.set_sensitive(True)
        if os.path.isfile (name):
           f = open (name, "r")
           content = f.read()
           f.close ()
           self.parse (content)

   def parse (self, content):
      """Parse the content of a coding standard file, and apply the values to the editor"""
      content = re.sub (' +',' ',content)
      content = re.sub ('\n+',' ',content)
      ok = False
      if content[1:4] == "ALL":
        ok = True
      elif content[0:2] == "+R":
        ok = True
      elif content[0:2] == "-R":
        ok = True
      if not ok:
        if not GPS.MDI.yes_no_dialog ("The selected file does not seem to be a valid Coding Standard file.\nAre you sure you want to override it ?"):
           self.fileEntry.set_text("")
           return
      switches = re.findall("[^ ]*", content)

      # all switches are on by default, so we need to check all checkboxes
      self.check_all (True)
      # we now parse the switches to initialize the different check states
      # We also store unknown switches in a separate string list
      for sw in switches:
        if sw == "-ALL":
           self.check_all (False)

        elif sw == "+ALL":
           self.check_all (True)

        elif sw[0:10] == "+RWarnings":
           self.warnings_check.set_active (True)
           res = re.split (".RWarnings:?([.a-zA-Z]*)", sw);
           w_sw_list = re.findall("[.]?[a-zA-Z]", res[1])
           for w_sw in w_sw_list:
             if w_sw == "a":
               for widg in self.warnings_widgets:
                 if widg[1] == "a":
                   widg[0].set_active (True)
                 elif widg[2]:
                   widg[0].set_active (True)
             else:
               for widg in self.warnings_widgets:
                 if widg[1] == w_sw:
                   widg[0].set_active (True)
                 elif widg[1].lower() == w_sw or widg[1] == w_sw.lower():
                   widg[0].set_active (False)

        elif sw[0:2] == "+R" or sw[0:2] == "-R":
           activate = False
           if sw[0] == "+":
              activate = True

           res = re.split ('^([^<>]*)[<>](.*)$',sw[2:])
           found = False
           for elem in self.rules_widgets:
              if sw[2:] == elem[1]:
                 found = True
                 if elem[2] == "":
                    elem[0].set_active(activate)
                 else:
                    elem[0].set_value (0)
                 break
              elif len (res) > 1:
                 if res[1] == elem[1]:
                    found = True
                    elem[0].set_value (float(res[2]))
                    break
           if not found:
              self.additional_switches.append (sw)
        elif sw != "":
           self.additional_switches.append (sw)

   def check_all (self, value):
      """Change all check states for the switches to 'value'"""
      self.warnings_check.set_active (True)
      for elem in self.rules_widgets:
         if elem[2] != "":
            if value:
               elem[0].set_value(1)
            else:
               elem[0].set_value(0)
         else:
            elem[0].set_active(value)

   def on_unckeck_all (self, *args):
      """Uncheck all switches"""
      self.check_all (False)

   def on_coding_standard_file_browse (self, *args):
      """Callback to coding standard 'Browse' button"""
      file = GPS.MDI.file_selector ()
      self.fileEntry.set_text (file.name())

   def on_cancel (self, *args):
      """Callback to 'Cancel' button"""
      self.response(gtk.RESPONSE_NONE)

   def on_save (self, *args):
      """Callback to 'Save' button"""
      file = GPS.File (self.fileEntry.get_text ())
      f = open (file.name(), "w")

      f.write ("-ALL\n")
      for elem in self.rules_widgets:
         if elem[2] == "":
            if elem[0].get_active():
               f.write ("+R%s\n" % (elem[1]))
         else:
            if elem[0].get_value_as_int() > 0:
               f.write ("+R%s%s%i\n" % (elem[1],elem[2],elem[0].get_value_as_int()))

      if self.warnings_check.get_active():
         f.write ("+RWarnings")
         first_switch = True
         a_switch = False
         for elem in self.warnings_widgets:
            if elem[0].get_active():
              if first_switch:
                f.write (":")
                first_switch = False

            if elem[1] == "a" and elem[0].get_active():
              a_switch = True
              f.write (elem[1])
            elif a_switch and elem[2] and not elem[0].get_active():
              f.write (elem[1].upper())
            elif a_switch and not elem[2] and elem[0].get_active():
              f.write (elem[1])
            elif not a_switch and elem[0].get_active():
              f.write (elem[1])
         f.write ("\n")

      for sw in self.additional_switches:
         f.write (sw)

      f.close ()
      GPS.EditorBuffer.get(file)
      self.response(gtk.RESPONSE_NONE)

def deactivate (widg):
   """Utility function to deactivate a widget. Used as callback in foreach loop"""
   widg.set_sensitive(False)
def activate (widg):
   """Utility function to activate a widget. Used as callback in foreach loop"""
   widg.set_sensitive(True)

class gnatCheckProc:
   """This class controls the gnatcheck execution"""
   def __init__ (self):
      self.rules_list = []
      self.warnings_list = []
      self.style_checks_list = []
      self.restrictions_list = []

      self.locations_string = "Coding Standard violations"
      self.gnatCmd = ""
      self.projectfile = None
      self.projectChanged (None)

   def isValid (self):
      return self.gnatCmd != ""

   def projectChanged (self, p):
      prev_cmd = self.gnatCmd
      self.gnatCmd = GPS.Project.root().get_attribute_as_string("gnat", "ide")

      if self.gnatCmd == "":
         self.gnatCmd = "gnat"
      if not os.path.isfile (self.gnatCmd):
         self.gnatCmd = locate_exec_on_path (self.gnatCmd)
      if self.gnatCmd == "":
         GPS.Console ("Messages").write ("Error: 'gnat' is not in the path.\n")
         GPS.Console ("Messages").write ("Error: Could not initialize the gnatcheck module.\n")
         return

      # gnat check command changed: we reinitialize the rules list
      if prev_cmd != self.gnatCmd:
         self.get_supported_rules()

      # we retrieve the coding standard file from the project
      self.projectfile = None
      for opt in GPS.Project.root().get_attribute_as_list("default_switches", package="check", index="ada"):
        res = re.split ("^\-from\=(.*)$", opt)
        if len(res)>1:
          self.projectfile = GPS.File (res[1])

   def edit(self):
      global ruleseditor
      ruleseditor = rulesEditor(self.rules_list, self.warnings_list, self.projectfile)
      ruleseditor.run()
      ruleseditor.destroy()
      return

   def add_rule (self, process, matched, unmatched):
      if unmatched == "\n":
        if re.search ("GNAT", self.msg):
          # do not take into account GNAT compiler warnings handling as they
          # require parameters and another module (check syntax) allows this
          self.check_rules_analysis = False
          return

        if self.check_rules_analysis:
          res = re.split ("^ *([^ ]+) +[-] +(.+) *$", self.msg)
          if len(res) > 1:
            if res[1] != "Metrics_Violation":
              self.rules_list.append([res[1], res[2]])
          elif len(self.rules_list) > 0:
            # Explanation was continuing on next line. Append it to previously
            # inserted rule
            self.rules_list [len (self.rules_list) - 1] [1] += ' ' + self.msg.strip()
        self.msg = ""
      self.msg += matched

   def add_gnat_rule (self, process, matched, unmatched):
      if unmatched == "\n":
        if re.search ("^ +[-]gnatwxx", self.msg):
          self.warnings_rules_analysis = True
        elif re.search ("^ +[-]gnatyxx", self.msg):
          self.style_rules_analysis = True
        elif re.search ("^ +[-]gnat", self.msg):
          self.warnings_rules_analysis = False
          self.style_rules_analysis = False
        else:
          if self.style_rules_analysis or self.warnings_rules_analysis:
            res = re.split ("^ *([^ *]+)([*]?) +(.+) *$", self.msg)
            if self.style_rules_analysis:
              # ??? todo
              # report = "New style rule: "
              # GPS.Console ("Messages").write ("(dbg)" + report + "["+res[1]+"] ["+res[3]+"]\n")
              self.style_rules_analysis = self.style_rules_analysis

            else:
              if len (res) > 2:
                if res[1] == "a":
                  # retrieve the list of warnings not activated by -gnatwa
                  exception = re.split ("\(except ([a-zA-Z.]*)\) *$", res[3])
                  self.all_warnings_exception_list = re.findall("[.]?[a-zA-Z]", exception[1])

                # ignore the comment made for '*', and include only on/off warnings: this ignores '-gnatwe' which we don't want in gnatcheck
                if res[2] != "*" and re.search ("turn", res[3]):
                  if res[1] != "a":
                    # search all warnings not covered by -gnatwa (this list was retrieved just above)
                    if re.search ("turn off", res[3]):
                      found = True
                    else:
                      found = False
                      for ex in self.all_warnings_exception_list:
                        if ex == res[1]:
                          found = True
                          break
                  else:
                    # do not include -gnatwa in the gnatwa alias list !
                    found = True
                  if res[1] != "A":
                    self.warnings_list.append ([res[1], res[3], not found])

        self.msg = ""
      self.msg += matched

   def get_supported_rules (self):
      # Verify we have the correct gnatcheck executable
      self.rules_list = []
      self.warnings_list = []
      self.all_warnings_exception_list = []
      self.style_checks_list = []
      self.restrictions_list = []

      self.check_rules_analysis = True
      self.warnings_rules_analysis = False
      self.style_rules_analysis = False

      if self.gnatCmd != "":
         # First get gnatcheck rules
         self.msg = ""
         process = GPS.Process (self.gnatCmd + " check -h", "^.+$",
                                on_match=self.add_rule)
         process.get_result()

         # verify the we had a correct execution of gnat check
         if len (self.rules_list) == 0:
            GPS.Console ("Messages").write ("Error: Gnatcheck not found, the gnatcheck module is disabled")
            self.gnatCmd = ""
            return

         # Then retrieve warnings/style/restriction checks from gnatmake
         self.msg = ""
         process = GPS.Process (self.gnatCmd + " make -h", "^.+$",
                                on_match=self.add_gnat_rule)
         process.get_result()
      return True

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
         GPS.Locations.parse (self.msg, self.locations_string)
         self.parse_output (self.msg)
         self.msg = ""

   def internalSpawn (self, filestr, project, recursive=False):
      rules_file = None
      need_rules_file = False
      opts = project.get_attribute_as_list("default_switches", package="check", index="ada")
      if len(opts) == 0:
         need_rules_file = True
         opts = GPS.Project.root().get_attribute_as_list("default_switches", package="check", index="ada")
         for opt in opts:
           res = re.split ("^\-from\=(.*)$", opt)
           if len(res)>1:
             rootdir = GPS.Project.root().file().directory()
             rules_file = rootdir+res[1]

      if need_rules_file:
         selector = rulesSelector (project.name(), rules_file)
         if selector.run() == gtk.RESPONSE_OK:
            rules_file = selector.get_file()
            selector.destroy()
         else:
            selector.destroy()
            return;

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

      if need_rules_file:
         cmd += " -rules -from=" + rules_file

      # now specify the files to check
      cmd += " " + filestr

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
      if not gnatcheckproc.isValid():
         return False
      elif not isinstance(context, GPS.FileContext):
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

   GPS.Hook("project_view_changed").add (gnatcheckproc.projectChanged);
   GPS.parse_xml ("""
  <tool name="GnatCheck" package="Check" index="Ada" override="false">
     <language>Ada</language>
     <switches lines="1" use_scrolled_window="true" sections="-rules">
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
    <title>Build</title>
    <submenu after="Check Semantic">
      <title>Check Coding Standard</title>
      <menu action="gnatcheck root project">
        <title>Check root _project</title>
      </menu>
      <menu action="gnatcheck root project recursive">
        <title>Check root _project and subprojects</title>
      </menu>
      <menu action="gnatcheck file">
        <title>Check current _file</title>
      </menu>
    </submenu>
  </submenu>
  <submenu>
    <title>Tools</title>
    <submenu after="Documentation">
      <title>Coding Standard</title>
      <menu action="edit gnatcheck rules">
        <title>Edit coding standard file</title>
      </menu>
    </submenu>
  </submenu>""");

GPS.Hook ("gps_started").add (on_gps_started)

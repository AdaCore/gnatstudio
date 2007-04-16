#
#  gnatcheck Python support for GPS integration
#
import GPS, os, os.path, re, string
try:
   import gtk, gobject
except ImportError:
   print ("Could not load gnatcheck module: PyGTK not available")
   raise

def locate_exec_on_path (prog):
    alldirs = string.split (os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir,prog) for dir in alldirs]:
        if os.path.isfile(file) or os.path.isfile(file+".exe"):
            return file
    return ""

# Dialog allowing selection of the different gnatcheck rules
class rulesDialog (gtk.Dialog):
   def __init__ (self):
      self.rules_list = gnatcheckproc.get_supported_rules()
      self.rules = self.get_project_rules()

      # copy the initial set of rules
      self.initial_rules = []
      for r in self.rules:
         self.initial_rules.append (r)

      gtk.Dialog.__init__ (self)
      self.set_default_size (400, 450)
      self.vbox.pack_start (gtk.Label("Gnat check options"), expand=False, fill=False)

      hbox = gtk.HBox(homogeneous=True, spacing=10)
      self.vbox.pack_start (hbox, expand=False, fill=False)

      self.allChecksButton = gtk.Button ("Activate all checks")
      self.allChecksButton.connect ("clicked", self.on_all_checks_clicked)
      hbox.pack_start (self.allChecksButton, expand=True, fill=True)

      self.noChecksButton = gtk.Button ("Deactivate all checks")
      self.noChecksButton.connect ("clicked", self.on_no_checks_clicked)
      hbox.pack_start (self.noChecksButton, expand=True, fill=True)

      scrolledWindow = gtk.ScrolledWindow()
      scrolledWindow.set_policy (gtk.POLICY_NEVER, gtk.POLICY_ALWAYS)
      self.vbox.pack_start (scrolledWindow, expand=True, fill=True)
      vbox = gtk.VBox()
      scrolledWindow.add_with_viewport (vbox)

      self.rulesButton = []
      none_active = True;
      all_active = True;
      for rule in self.rules_list:
         button = gtk.CheckButton (rule[1])
         # check if the rule is in activeRules
         if self.rules.count (rule[0]) > 0:
            none_active = False
            button.set_active (True)
         else:
            all_active = False
         # update the rules list upon toggle of the button
         button.connect ("toggled", self.on_toggled, rule[0])
         self.rulesButton.append(button)
         vbox.pack_start (button, expand=False, fill=False)

      # udpate sensitivity of all/no checks buttons
      self.allChecksButton.set_sensitive (not all_active)
      self.noChecksButton.set_sensitive (not none_active)

      self.vbox.show_all()

      self.add_buttons (gtk.STOCK_OK, gtk.RESPONSE_OK,
                        gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
      self.connect ("response", self.on_response)

   def on_response (self, dialog, response_id):
      if (response_id == gtk.RESPONSE_OK) and (self.initial_rules != self.rules):
         self.set_project_rules()

   def on_all_checks_clicked (self, button):
      for button in self.rulesButton:
         button.set_active (True)
      self.allChecksButton.set_sensitive (False)
      self.noChecksButton.set_sensitive (True)
      return False

   def on_no_checks_clicked (self, button):
      for button in self.rulesButton:
         button.set_active (False)
      self.allChecksButton.set_sensitive (True)
      self.noChecksButton.set_sensitive (False)
      return False

   def on_toggled (self, button, rule):
      if button.get_active():
         self.noChecksButton.set_sensitive (True)
         self.rules.append (rule)
      else:
         self.allChecksButton.set_sensitive (True)
         self.rules.remove (rule)
      if len(self.rules) == 0:
         self.noChecksButton.set_sensitive (False)
      elif len(self.rules) == len(self.rules_list):
         self.allChecksButton.set_sensitive (False)

   def get_project_rules (self):
      rules = []
      opts = GPS.Project.root().get_tool_switches_as_string ("GnatCheck")
      if re.search ("[+]ALL", opts) != None:
         for j in self.rules_list:
            rules.append (j[0])

      defined = re.findall ("[+]R([^ ]+)", opts)
      for j in defined:
         if not rules.count (j):
            rules.append (j)
         else:
            GPS.Console ("Messages").write ("Error: gnatcheck rule " + j + " defined twice")

      undefined = re.findall ("[-]R([^ ]+)", opts)
      for j in undefined:
         if rules.count(j) > 0:
            rules.remove (j)

      return rules

   def set_project_rules (self):
      switches = []
      GPS.Project.root().clear_attribute_values ("Default_Switches", "IDE", "gnatcheck")
      if len (self.rules) == 0:
         return
      if len (self.rules) < (len (self.rules_list) / 2):
         switches.append("-ALL");
         for j in self.rules:
            switches.append("+R"+j);
      else:
         switches.append("+ALL");
         for j in self.rules_list:
            if self.rules.count (j[0]) == 0:
               switches.append("-R"+j[0]);
      j = len (switches) - 1
      while j >= 0:
         GPS.Project.root().add_attribute_values ("Default_Switches", "IDE", "gnatcheck", switches[j])
         j-=1

# class controling the gnatcheck execution
class gnatCheck:
   def __init__ (self):
      self.project = None
      self.rules_list = []
      self.locations_string = "gnatcheck rules violations"
      self.activeRules = []

   def init_gnatcheck_cmd (self):
      driver = GPS.Project.root().get_attribute_as_string("gnat", "ide")
      if driver == "":
         driver = "gnat"
      if not os.path.isfile (driver):
         driver = locate_exec_on_path (driver)
      if driver == "":
         GPS.Console ("Messages").write ("Error: 'gnat' is not in the path")
         self.gnatcheckCmd = ""
      else:
         self.gnatcheckCmd = driver + " check"

   def add_rule (self, process, matched, unmatched):
      if re.search ("GNAT", matched):
         # do not take into account GNAT compiler warnings handling as they
         # require parameters and another module (check syntax) allows this
         self.rules_analysis_finished = True
         return

      if not self.rules_analysis_finished:
         res = re.split ("^ *([^ ]+) +[-] +(.+) *$", matched)
         if len(res) > 1:
            self.rules_list.append([res[1], res[2]])
         elif len(self.rules) > 0:
            # Explanation was continuing on next line. Append it to previously
            # inserted rule
            self.rules_list [len (self.rules_list) - 1] [1] += ' ' + matched.strip()

   def get_supported_rules (self):
      # Verify we have the correct gnatcheck executable
      self.init_gnatcheck_cmd ()
      self.rules = []
      self.rules_analysis_finished = False
      if self.gnatcheckCmd != "":
         process = GPS.Process (self.gnatcheckCmd + " -h", "^.+$",
                                on_match=self.add_rule)
         process.get_result()
      return self.rules_list

   def output (self, process, matched, unmatched):
      GPS.Locations.parse (matched, self.locations_string)
      GPS.Codefix.parse (self.locations_string, matched)
      print (matched)

   def check_file (self, file):
      self.init_gnatcheck_cmd ()
      if self.gnatcheckCmd == "":
         return
      cmd = self.gnatcheckCmd + " -P" + GPS.Project.root().file().name() + " -dd " + file.name() + " -rules "
      opts = GPS.Project.root().get_tool_switches_as_string ("GnatCheck")
      if opts == "":
         print "Gnat check: no rules to check"
         return
      cmd += opts

      # clear the Checks category in the Locations view
      if GPS.Locations.list_categories().count (self.locations_string) > 0:
         GPS.Locations.remove_category (self.locations_string)

      print cmd
      process = GPS.Process (cmd, "^.+$",
                             on_match=self.output,
                             progress_regexp="^ *completed (\d*) out of (\d*) .*$",
                             progress_current = 1,
                             progress_total = 2)

# Contextual menu for checking files
class contextualMenu (GPS.Contextual):
   def __init__ (self):
      GPS.Contextual.__init__ (self, "Check with gnatcheck")
      self.create (on_activate = self.on_activate,
                   filter      = self.filter,
                   label       = self.label)

   def filter (self, context):
      if not isinstance(context, GPS.FileContext):
         return False
      try:
         self.file = context.file()
      except:
         return False
      if self.file.language().lower() != "ada":
         return False
      self.project = GPS.Project.root()
      srcs = self.project.sources (recursive = True)
      found = False
      for f in srcs:
         if f.name().lower() == self.file.name().lower():
            found = True
            break
      return found

   def label (self, context):
      return "Check "+os.path.basename(self.file.name())+" with gnatcheck"

   def on_activate (self, context):
      gnatcheckproc.check_file (self.file)

# runs the configuration dialog
def rundialog():
   d = rulesDialog()
   d.run()
   d.destroy()
   return

# create the gnatcheck and menus instances.
gnatcheckproc = gnatCheck()
gnatcheckmenu = contextualMenu()
GPS.parse_xml ("""
  <tool name="GnatCheck" package="Ide" index="GnatCheck" >
     <language>Ada</language>
  </tool>
  <action name="gnatcheck_flags" show-command="false" category="Projects">
     <shell lang="python" show-command="false" show-task-manager="false">gnatcheck.rundialog()</shell>
  </action>
  <menu action="gnatcheck_flags">
     <title>/Project/Gnat Check rules</title>
  </menu>
  """)

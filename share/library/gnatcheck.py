#
#  gnatcheck Python support for GPS integration
#
import GPS, os, os.path, re, string

def locate_exec_on_path (prog):
    alldirs = string.split (os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir,prog) for dir in alldirs]:
        if os.path.isfile(file) or os.path.isfile(file+".exe"):
            return file
    return ""

# class controling the gnatcheck execution
class gnatCheck:
   def __init__ (self):
      self.rules_list = []
      self.initXml()
      self.locations_string = "gnatcheck rules violations"
      self.activeRules = []

   def initXml (self):
      self.get_supported_rules()
      xml = """
  <tool name="GnatCheck" package="Ide" index="gnatcheck" override="true">
     <language>Ada</language>
     <switches lines="1" use_scrolled_window="true">
        <title line="1" >Rules</title>
      """
      for j in self.rules_list:
        xml += '<check label="' + j[1] + '" switch="+R' + j[0] + '" line="1"/>'
      xml += """
     </switches>
   </tool>
  """
      GPS.parse_xml (xml)

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
         elif len(self.rules_list) > 0:
            # Explanation was continuing on next line. Append it to previously
            # inserted rule
            self.rules_list [len (self.rules_list) - 1] [1] += ' ' + matched.strip()

   def get_supported_rules (self):
      # Verify we have the correct gnatcheck executable
      self.init_gnatcheck_cmd ()
      self.rules_list = []
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

   def internalSpawn (self, filestr):
      self.init_gnatcheck_cmd ()
      if self.gnatcheckCmd == "":
         return
      # launch gnat check with current project
      cmd = self.gnatcheckCmd + " -P" + GPS.Project.root().file().name()
      # define the scenario variables
      scenario = GPS.Project.scenario_variables()
      for i, j in scenario.iteritems():
         cmd += " -X" + i + "=" + j
      # use progress, specify the file name to check
      cmd +=  " -dd " + filestr
      # retrieve the rules to check, from project
      opts = GPS.Project.root().get_tool_switches_as_list ("GnatCheck")
      if opts == []:
         print "Gnat check: no rules to check"
         return

      if len(opts) > (len(self.rules_list)/2):
         # opts is a long list: define by default all rules, and remove the
         # ones not present in this list
         cmd += " -rules +ALL"
         for r in self.rules_list:
            if opts.count("+R"+r[0]) == 0:
               cmd += " -R"+r[0]
      else:
        cmd += " -rules -ALL"
        for o in opts:
           cmd += " "+o

      # clear the Checks category in the Locations view
      if GPS.Locations.list_categories().count (self.locations_string) > 0:
         GPS.Locations.remove_category (self.locations_string)

      print cmd
      process = GPS.Process (cmd, "^.+$",
                             on_match=self.output,
                             progress_regexp="^ *completed (\d*) out of (\d*) .*$",
                             progress_current = 1,
                             progress_total = 2)

   def check_file (self, file):
      self.internalSpawn (file.name())

   def check_files (self, files):
      filestr = ""
      for f in files:
         filestr += f.name() + " "
      self.internalSpawn (filestr);

# Contextual menu for checking files
class contextualMenu (GPS.Contextual):
   def __init__ (self):
      GPS.Contextual.__init__ (self, "Check file with gnatcheck")
      self.create (on_activate = self.on_activate,
                   filter      = self.filter,
                   label       = self.label)

   def filter (self, context):
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
           dir = context.directory()
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
            project = context.project()
            srcs = project.sources (recursive = False)
            found = False
            self.files = []
            for f in srcs:
               if f.language().lower() == "ada":
                  self.files.append (f)
                  found = True
            return found

   def label (self, context):
      if self.desttype == "file":
         return "Check "+os.path.basename(self.file.name())+" with gnatcheck"
      elif self.desttype == "dir":
         return "Check directory with gnatcheck"
      elif self.desttype == "project":
         return "Check project with gnatcheck"
      return ""

   def on_activate (self, context):
      global gnatcheckproc
      if self.desttype == "file":
         gnatcheckproc.check_file(self.file)
      else:
         gnatcheckproc.check_files(self.files)

# create the gnatcheck and menus instances.
gnatcheckproc = None;
def init(p):
   global gnatcheckproc;
   gnatcheckproc = gnatCheck()
   contextualMenu()

def on_project_change(p):
   global gnatcheckproc;
   if gnatcheckproc != None:
      gnatcheckproc.initXml();

GPS.Hook ("gps_started").add (init)
GPS.Hook("project_view_changed").add (on_project_change);
GPS.parse_xml ("""<tool name="GnatCheck" package="Ide" index="gnatcheck"/>""");

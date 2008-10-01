""" Provides the "Tools/Compute coverage files" and "Tools/Remove coverage files"
menu, which executes gcov automatically.

This script will also perform checks along the way to guide through the
procedure of obtaining gcov info.
The output of the gcov process is displayed in a separate console.
At the end of the processing, the open editors are decorated with coverage
information.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os
from GPS import *
from os import *

# A class to display the output of gcov in a separate console.

class Gcov_Process (GPS.Console, GPS.Process):

   def on_output (self, unmatched, matched):
      self.write (unmatched + matched)

   def on_exit (self, status, remaining_output):
      self.write (remaining_output)
      if status == 0:
         self.write ("process terminated successfully")
      else:
         self.write ("process terminated [" + str(status) + "]")

      # Show coverage report
      analysis = CodeAnalysis.get ("Coverage")
      analysis.add_all_gcov_project_info()
      analysis.show_analysis_report()

      self.kill()

   def on_input (self, input):
      self.send (input)

   def on_destroy (self):
      self.kill ()

   def __init__ (self, process, args=""):
      GPS.Console.__init__ \
         (self, "Executing gcov", \
          on_input=Gcov_Process.on_input, \
          on_destroy=Gcov_Process.on_destroy, \
          force=True)

      GPS.Process.__init__ \
         (self, process + ' ' + args, ".+", \
          remote_server="Build_Server", is_compiler_cmd=True, \
          on_exit=Gcov_Process.on_exit, \
          on_match=Gcov_Process.on_output)

# Actual menu implementation

def run_gcov(menu):
   # Verify that the version of gcov is recent enough to support response
   # files and reading of .gc?? data in multiple directories.

   try:
      p = Process ("gcov --version")
      out = p.get_result()
      paren = out.find ("(", out.find ("GNAT"))
      date = int (out[paren+1:paren+9])

      if date < 20071005:
         MDI.dialog ("Your version of gcov is dated " + str (date) +
         ".\nThis plug-in requires gcov for GNAT dated 20071005 or later.")
         return;

   except:
      MDI.dialog ("""Could not read gcov version number.

Make sure you are using gcov for GNAT dated 20071005 or later.""")

   # Determine the root project
   root_project = Project.root()
   previous_dir = pwd()

   # Determine where to create the gcov info
   GCOV_ROOT = getenv ("GCOV_ROOT")

   if GCOV_ROOT == None or GCOV_ROOT == "":
      gcov_dir = root_project.object_dirs (False)[0]

   else:
      gcov_dir = GCOV_ROOT

   if not access (gcov_dir, R_OK and W_OK):
      MDI.dialog (""" Could not access the directory:

   """ + gcov_dir + """

Please point the environment variable GCOV_ROOT to a directory
on which you have permission to read and write.
         """)

   cd (gcov_dir)

   # List all the projects
   projects = root_project.dependencies(True)

   # Write the response file
   res=file("gcov_input.txt", 'wb')

   gcda_file_found = False
   gcno_file_found = False

   for p in projects:
      if p.object_dirs (False) == []:
         object_dir = None
      else:
         object_dir = p.object_dirs(False)[0]

      if object_dir != None and object_dir != "":
         sources = p.sources(False)

         for s in sources:
            n = s.name()
            basename=n[max(n.rfind('\\'), n.rfind('/'))+1:len(n)]
            unit=basename[0:basename.rfind('.')]

            gcda = object_dir + sep + unit + ".gcda"

            # If we have not yet found at least one .gcno file, attempt to
            # find one. This is to improve the precision of error messages,
            # and detect the case where compilation was successful but the
            # executable has never been run.

            if not gcno_file_found:
               gcno = object_dir + sep + unit + ".gcno"
               if access (gcno, F_OK):
                  gcno_file_found = True

            if access (gcda, F_OK):
               gcda_file_found = True
               # Write one entry in response file

               # Escape all backslashes.
               gcda = gcda.replace('\\', '\\\\')

               res.write ('"' + gcda + '"' +"\n")

   res.close()

   if not gcno_file_found:
      # No gcno file was found: display an appropriate message.
      MDI.dialog (""" No ".gcno" file was found in any of the object directories.

Make sure you have compiled the sources of interest with
the "Code coverage" flags.""")

   else:
      if not gcda_file_found:
         # Some gcno files were found, but no gcna files.
         MDI.dialog (""" No ".gcda" file was found in any of the object directories.

Make sure you have run the executable(s) at least once.
""")

      else:
         # Run gcov
         Gcov_Process ("gcov", "@gcov_input.txt")

   cd (previous_dir)

def remove_gcov(menu):
   if not MDI.yes_no_dialog (
      "This will remove all .gcov and .gcda files, are you sure ?"):
      return

   # Look in all the projects

   for p in Project.root().dependencies(True):
      object_dir = p.object_dirs(False)[0]

      # Browse in the object dirs
      for f in listdir (object_dir):
         #  if f is a .gcda or a .gcov, remove it
         if f.find (".gcda") != -1 or f.find (".gcov") != -1:
            remove (object_dir + sep + f)



def on_gps_started (hook):
   Menu.create ("/Tools/Covera_ge/Compute coverage files",
                on_activate=run_gcov,
                ref="Show report",
                add_before=True)

   Menu.create ("/Tools/Covera_ge/Remove coverage files",
                on_activate=remove_gcov,
                ref="Show report",
                add_before=True)

   Menu.create ("/Tools/Covera_ge/-",
                ref="Remove coverage files",
                add_before=False)


GPS.Hook ("gps_started").add (on_gps_started)

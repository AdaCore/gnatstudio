"""This file provides support for using the CodePeer tool
   Note that this is a work in progress.

   CodePeer is a static analysis tool for Ada code.
   This package allows the user to perform an automatic code review of
   a project and integrates its output into GPS:
     - menu Tools/CodePeer/Run code review
     - menu Tools/CodePeer/Regenerate reports

"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Scripts)


############################################################################
## No user customization below this line
############################################################################

import GPS, sys, os.path, os_utils, string, re

def project_path():
    return GPS.Project.root().object_dirs()[0]

def output_dir():
    return os.path.join(project_path(),
                        GPS.Project.root().name() + ".output")

def library_file():
    return os.path.join(project_path(),
                        GPS.Project.root().name() + ".library")

def fileExists(f):
    try:
       file = open(f)
    except IOError:
       exists = 0
    else:
       exists = 1
    return exists

def check_toolchain():
    global gnat_toolchain_found

    gnat_toolchain_found = False

    # Check for GNAT toolchain: codepeer, gps_codepeer_bridge

    gnat_toolchain_found = \
      os_utils.locate_exec_on_path("codepeer") != "" \
      and os_utils.locate_exec_on_path("gps_codepeer_bridge") != ""

#----------------- Helper routines to run CodePeer ---------------------------

def inspection_output (proc, matches, since_last):
   if ('0' <= matches[0] <= '9'):
      # Messages that start with a number are probably the "progress bar"
      GPS.Console().write("Inspecting...\n")
   else:
      GPS.Console().write(matches + "\n")

def inspection_exit(self, status, remaining_output):
   GPS.Console().write("Inspection completed.\n")

def regenerate_output (proc, matches, since_last):
   GPS.Console().write("Regenerating reports...\n")  

def regenerate_exit(self, status, remaining_output):
   GPS.Console().write("Finished generating reports \n")
  
#----------------- Create CodePeer library file ------------------------------

def create_library_file():
  try:
      projectname = GPS.Project.root().name()
      if not os.path.exists(project_path()):
         os.mkdir(project_path())
      
      database_dir = os.path.join(project_path(), projectname + ".db")
      class_dir = os.path.join(project_path(), "ada_classes")

      f = open(library_file(), 'w') 

      f.write("--  Specify name of directory where codepeer output will be" \
                + " created.\n")
      f.write("Output_Dir := \"" + output_dir() + "\";\n\n")

      f.write("--  Specify name of database directory where codepeer" \
                + " messages will be archived.\n")
      f.write("Database_Dir := \"" + database_dir + "\";\n\n")

      #  Generate GNAT toolchain specific information

#--      project_object_dirs = GPS.Project.root().object_dirs()
#--      for dir in project_object_dirs:
      f.write('Source (Directory => "SCIL",\n')
#--            f.write('Source (Directory => "' \
#--                    + os.path.join(dir, "SCIL") \
#--                   + '",\n')
      f.write('  Include_Subdirectories => True,\n')
      f.write('        Files     => ("*.scil"),\n')
      f.write('        Language  => SCIL);\n')

      f.close()

  except:
      GPS.Console().write("Problem while creating the library file\n", "error")
      return

#--------------- check parameters before regenerating reports ----------------

def check_params_for_reports():
  try:  
     info_file = os.path.join(output_dir(), "Inspection_Info.xml")
     if not fileExists(info_file):
        GPS.Console().write (info_file + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return
      
     if not fileExists(library_file()):
         GPS.Console().write (library_file() + " does not exist.\n" +
            "Please inspect the project first.\n", "error")
         return  

  except:
      GPS.Console().write(
          "Problem while checking parameters before for reporting\n", "error")
      return

#--------------------------- Run CodePeer ------------------------------------

def run_inspection(menu):
  try:  
      regenerate_menu.set_sensitive(False)
      inspect_menu.set_sensitive(False)
      create_library_file()

      projectname = GPS.Project.root().name()
      partition_dir = os.path.join(project_path(), projectname + ".partitions")

      savedir = os.getcwd()     
      os.chdir(project_path());

      ins_cmd = "codepeer -all -background -lib \"" + library_file() \
        + "\" -dbg-partition-library-location \"" + partition_dir + '"'
      proc = GPS.Process(ins_cmd, regexp=".+", on_match=inspection_output,
        on_exit=inspection_exit, show_command = True)
      proc.get_result()
      os.chdir(savedir)

      regenerate_menu.set_sensitive(True)
      inspect_menu.set_sensitive(True)     

  except:
      GPS.Console().write("Problem while running CodePeer\n", "error")
      return

#----------------- Regenerate Reports ------------------------------------
def regenerate_report(menu):
  try:  
      regenerate_menu.set_sensitive(False)
      inspect_menu.set_sensitive(False)
      check_params_for_reports()

      projectname = GPS.Project.root().name()
      partition_dir = os.path.join(project_path(), projectname + ".partitions")
      ins_cmd = "codepeer -all -background -output-only -lib " + '"' + \
        library_file() + '"' + " -dbg-partition-library-location " + '"' + \
        partition_dir + '"'
      proc = GPS.Process(ins_cmd, regexp=".+", on_match=regenerate_output,
        on_exit=regenerate_exit, show_command = True)
      proc.get_result()

      regenerate_menu.set_sensitive(True)
      inspect_menu.set_sensitive(True)
  except:
      GPS.Console().write("Problem while regenerating reports\n", "error")
      return


def check_valid_project():
    #check that the CodePeer library exists and that the CodePeer has been run
    if not fileExists(library_file()):
        GPS.Console().write (library_file() + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return

    info_file = os.path.join(output_dir(), "Inspection_Info.xml")
    if not fileExists(info_file):
        GPS.Console().write (info_file + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return

#----------------- toolbar menus -----------------------------------
def create_codepeer_menu():
    # separation

    global inspect_menu
    inspect_menu = GPS.Menu.create ("Tools/CodePeer/Run code review",
            on_activate=run_inspection)

    global regenerate_menu
    regenerate_menu = GPS.Menu.create ("Tools/CodePeer/Regenerate reports",
            on_activate=regenerate_report)

def on_gps_started (hook_name):
    check_toolchain()

    global gnat_toolchain_found

    if not (gnat_toolchain_found):
        return

    #toolbar menus
    create_codepeer_menu ()

GPS.Hook ("gps_started").add (on_gps_started)

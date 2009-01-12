"""This file provides support for using the CodePeer tool

   CodePeer is a static analysis tool for Ada code.
   This package allows the user to inspect a project and integrates
   its output into GPS:
     - menu Tools/CodePeer/(Re)Inspect Project
     - menu Tools/CodePeer/Regenerate Reports

"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Scripts)


############################################################################
## No user customization below this line
############################################################################

import GPS, sys, os.path, os_utils, string, re

def si_project_path():
    return os.path.join(GPS.Project.root().object_dirs()[0],
                        "SI_" + GPS.Project.root().name())

def si_output_dir():
    return os.path.join(si_project_path(),
                        GPS.Project.root().name() + ".output")

def si_library_file():
    return os.path.join(si_project_path(),
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
    global si_gnat_toolchain_found
    global si_root_dir
    global si_gnat_rtl_dir

    si_gnat_toolchain_found = False
    si_root_dir = ""
    si_gnat_rtl_dir = ""

    # Check SOFCHECK_INSPECTOR environment variable

    si_root_dir = os.environ['SOFCHECK_INSPECTOR']
    if si_root_dir == "":
        return

    # Check for GNAT toolchain: codepeer, gps_codepeer_bridge, gnat2scil

    p = GPS.Process("gnatls -v",
                    regexp=".*adainclude.*", on_match=on_gnat_rtl_match)
    p.get_result()

    si_gnat_toolchain_found = \
      os_utils.locate_exec_on_path("codepeer") != "" \
      and os_utils.locate_exec_on_path("gps_codepeer_bridge") != "" \
      and os_utils.locate_exec_on_path("gnat2scil") != "" \
      and si_gnat_rtl_dir != ""

    if si_gnat_toolchain_found:
        GPS.Console().write("CodePeer: GNAT toolchain found\n")

    else:
        GPS.Console().write("CodePeer: no toolchain found\n",
                            "error")

def on_gnat_rtl_match(proc, matches, since_last):
    global si_gnat_rtl_dir
    r = re.compile("\s*(.*)")
    si_gnat_rtl_dir = r.match(matches).group(1)

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
      global si_root_dir

      projectname = GPS.Project.root().name()
      if not os.path.exists(si_project_path()):
         os.mkdir(si_project_path())
      
      database_dir = os.path.join(si_project_path(), projectname + ".db")
      message_patterns_file = \
        os.path.join(si_root_dir, "doc", "MessagePatterns.xml")
      class_dir = os.path.join(si_project_path(), "ada_classes")

      f = open(si_library_file(), 'w') 

      f.write("--  Specify name of directory where Inspector output will be" \
                + " created.\n")
      f.write("Output_Dir := \"" + si_output_dir() + "\";\n\n")

      f.write("--  Specify name of database directory where Inspector" \
                + " messages will be archived.\n")
      f.write("Database_Dir := \"" + database_dir + "\";\n\n")

      f.write("--  Specify name of message pattern file.\n")
      f.write("Message_Patterns := \"" + message_patterns_file + "\";\n\n")

      #  Generate GNAT toolchain specific information

      project_object_dirs = GPS.Project.root().object_dirs()
      for dir in project_object_dirs:
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

#-------------------------- Run gnat2scil ------------------------------------
#  This is temporal implementation, it will be replaced by the use of the
#  new build module with SCIL mode.
#
def run_gnat2scil():
  global si_gnat_rtl_dir

  try:
     sources = GPS.Project.root().sources(recursive=True)

     for i in sources:
       proc = GPS.Process("gnat2scil -c -gnata -gnatVim -I" + si_gnat_rtl_dir + " " + i.name(), show_command=True)
       GPS.Console().write (proc.get_result())

  except:
      GPS.Console().write("Problem while running gnat2scil\n", "error")
      return

#--------------- check parameters before regenerating reports ----------------

def check_params_for_reports():
  try:  
     info_file = os.path.join(si_output_dir(), "Inspection_Info.xml")
     if not fileExists(info_file):
        GPS.Console().write (info_file + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return
      
     if not fileExists(si_library_file()):
         GPS.Console().write (si_library_file() + " does not exist.\n" +
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
      partition_dir = os.path.join(si_project_path(), projectname + ".partitions")

      savedir = os.getcwd()     
      os.chdir(si_project_path());
      run_gnat2scil()

      source_dir = GPS.Project.root().source_dirs(recursive=True)
      src_path = " "
      ##  TBD : directory names with spaces in it
      for i in source_dir:
          src_path = src_path + " -source-path \"" + i + '"'

      ins_cmd = "codepeer -all -background -lib \"" + si_library_file() \
        + "\" -dbg-partition-library-location \"" + partition_dir + '"' \
        + src_path
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
      partition_dir = os.path.join(si_project_path(), projectname + ".partitions")
      ins_cmd = "codepeer -all -background -output-only -lib " + '"' + \
        si_library_file() + '"' + " -dbg-partition-library-location " + '"' + \
        partition_dir + '"'
      proc = GPS.Process(ins_cmd, regexp=".+", on_match=regenerate_output,
        on_exit=regenerate_exit, show_command = True)
      proc.get_result()

      regenerate_menu.set_sensitive(True)
      inspect_menu.set_sensitive(True)
  except:
      GPS.Console().write("Problem while regenerating reports\n", "error")
      return


def check_valid_si_project():
    #check that the CodePeer library exists and that the CodePeer has been run
    if not fileExists(si_library_file()):
        GPS.Console().write (si_library_file() + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return

    info_file = os.path.join(si_output_dir(), "Inspection_Info.xml")
    if not fileExists(info_file):
        GPS.Console().write (info_file + " does not exist.\n" +
            "Please Inspect the project first.\n", "error")
        return

#----------------- toolbar menus -----------------------------------
def create_codepeer_menu():
    # separation

    global inspect_menu
    inspect_menu = GPS.Menu.create ("Tools/CodePeer/(_Re)Inspect Project",
            on_activate=run_inspection)

    global regenerate_menu
    regenerate_menu = GPS.Menu.create ("Tools/CodePeer/Regenerate Reports",
            on_activate=regenerate_report)

def on_gps_started (hook_name):
    check_toolchain()

    global si_gnat_toolchain_found

    if not (si_gnat_toolchain_found):
        return

    #toolbar menus
    create_codepeer_menu ()

GPS.Hook ("gps_started").add (on_gps_started)

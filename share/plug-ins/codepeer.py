"""This file provides support for using the CodePeer tool
   Note that this is a work in progress.

   CodePeer is a static analysis tool for Ada code.
   This package allows the user to perform an automatic code review of
   a project and integrates its output into GPS.
   See menu Tools->CodePeer.
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

#----------------- Helper routines to run CodePeer ---------------------------

def regenerate_output (proc, matches, since_last):
   GPS.Console().write("Regenerating reports...\n")  

def regenerate_exit(self, status, remaining_output):
   GPS.Console().write("Finished generating reports\n")

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

def reset_messages():
   GPS.Console().clear ()
   GPS.MDI.get ("Messages").raise_window ()

#----------------- Regenerate Reports ------------------------------------
def regenerate_report(menu):
  try:  
      regenerate_menu.set_sensitive(False)
      check_params_for_reports()

      projectname = GPS.Project.root().name()
      reset_messages()
      ins_cmd = 'codepeer -all -global -background -output-only -lib "' + \
        library_file() + '"'
      proc = GPS.Process(ins_cmd, regexp=".+", on_match=regenerate_output,
        on_exit=regenerate_exit, show_command = True)
      proc.get_result()

      regenerate_menu.set_sensitive(True)

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
def on_gps_started (hook_name):
    global regenerate_menu
    regenerate_menu = GPS.Menu.create ("Tools/CodePeer/Regenerate reports",
      on_activate=regenerate_report, ref="Advanced", add_before=1)

# Check for GNAT toolchain: codepeer, gps_codepeer_bridge

if os_utils.locate_exec_on_path("codepeer") != "" \
  and os_utils.locate_exec_on_path("gps_codepeer_bridge") != "":
  GPS.Hook ("gps_started").add (on_gps_started)
  GPS.parse_xml ("""
    <builder-mode name="codepeer">
      <description>Build SCIL for code review</description>
      <subdir>codepeer</subdir>
      <supported-model>builder</supported-model>
      <supported-model>gnatmake</supported-model>
      <supported-model>gprbuild</supported-model>
      <extra-args>
        <arg>--subdirs=%subdir</arg>
        <arg>-k</arg>
        <arg>-c</arg>
        <arg>-gnatc</arg>
        <arg>-cargs</arg>
        <arg>-gnatC</arg>
      </extra-args>
    </builder-mode>""")

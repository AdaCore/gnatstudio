"""Ease creation of new projects

This file adds some new menu entries in Project, to facilitate the
creation of new basic project files:
  - Either by just asking the name of the main unit
  - Or by automatically guessing the name of this unit by looking at the
    current source editor
  - Or by asking for a name, and creating both the main unit and the project
"""


############################################################################
## No user customization below this line
############################################################################

import GPS, os.path

GPS.parse_xml ("""
  <action name="simple_project_from_dialog" show-command="false" category="Projects">
     <shell lang="python">simple_project.create_from_dialog()</shell>
  </action>

  <action name="simple_project_from_current" show-command="false" category="Projects">
    <filter id="Source editor" />
    <shell lang="python">simple_project.create_from_context()</shell>
  </action>

  <action name="simple_project_and_main" show-command="false" category="Projects">
    <shell lang="python">simple_project.create_project_and_main()</shell>
  </action>

  <menu action="simple_project_from_dialog">
     <title>/Project/Create from Dialog</title>
  </menu>
   
  <menu action="simple_project_from_current">
     <title>/Project/Create from Current File</title>
  </menu>
   
  <menu action="simple_project_and_main">
     <title>/Project/Create Project and Main Unit</title>
  </menu>
  """)

def create_from_main (main_file, create_main=0):
  print "file = " + main_file
  if main_file != "":
     main = os.path.splitext (os.path.basename(main_file)) [0]
     out_file = os.path.dirname (os.path.abspath (main_file)) + os.sep + main + ".gpr"
     out = file (out_file, "w")
     out.write ("project " + main + " is\n")
     out.write ("   for Source_Dirs use (\".\");\n")
     out.write ("   for Main use (\"" + main_file + "\");\n")
     out.write ("end " + main + ";\n")
     out.close()

     if create_main:
        out = file (os.path.splitext (os.path.abspath (main_file))[0] + ".adb", "w")
        out.write ("procedure " + main + " is\n")
        out.write ("begin\n")
        out.write ("   null;\n")
        out.write ("end " + main + ";\n")
        out.close ()

     GPS.Project.load (out_file)


def create_from_dialog():
   """Create a new project file, asking the user for the name of the main unit"""
   main = GPS.MDI.input_dialog \
      ("Please enter file name that contains the main unit", "main unit") [0]
   create_from_main (main)

def create_from_context():
   """Create a new project file, using the current source as main"""
   try:
      create_from_main (GPS.current_context().file().name())
   except:
      pass

def create_project_and_main():
   """Ask the user for a project name, and create a main unit and project file"""
   dir, main = GPS.MDI.input_dialog \
      ("Enter the name of the project", "directory", "name")
   dir = os.path.abspath (dir)
   if not os.path.isdir (dir): os.mkdir (dir)
   os.chdir (dir)
   create_from_main (main, create_main=1)


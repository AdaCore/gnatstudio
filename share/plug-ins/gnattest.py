"""This file provides support for gnattest.
"""



import os.path, GPS

def is_harness_project ():
   # Determine the root project
   root_project = GPS.Project.root()
   mapping = root_project.get_attribute_as_string ("GNATTest_Mapping_File",
                                                   package="GNATtest")
   return mapping.strip() != ""

def open_harness_project ():
   if is_harness_project():
      return

   cur = GPS.current_context().project()
   harness_dir = cur.get_attribute_as_string("Harness_Dir", "GNATtest")

   if harness_dir == "" :
      harness_dir = "gnattest/harness"

   prj = os.path.join (cur.object_dirs()[0], harness_dir, "test_driver.gpr")
   GPS.Project.load (prj, False, True)
   GPS.Console ("Messages").write ("Switched to harness project: " +
      GPS.Project.root().file().name() +"\n")

def exit_harness_project ():
   # Determine the root project
   root_project = GPS.Project.root()

   for p in root_project.dependencies():
      if p.name() != "AUnit":
         for d in p.dependencies():
            if d.name() != "AUnit":
               user_project = d
               break

   GPS.Project.load (user_project.file().name(), False, True)
   GPS.Console ("Messages").write ("Exit harness project: " +
      GPS.Project.root().file().name() +"\n")

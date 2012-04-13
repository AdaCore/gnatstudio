"""This file provides support for gnattest.
"""



import os.path, GPS

last_gnattest_project = None

def run (project, target, extra_args=""):
   """ Run gnattest and switch to harness if success. """
   global last_gnattest_project
   last_gnattest_project = project
   GPS.BuildTarget(target).execute(extra_args=extra_args)

def is_harness_project ():
   """ Check if root project is harness project. """
   root_project = GPS.Project.root()
   mapping = root_project.get_attribute_as_string ("GNATtest_Mapping_File",
                                                   package="GNATtest")
   return mapping.strip() != ""

def open_harness_project (cur):
   """ Open harness project if it hasn't open yet."""
   if is_harness_project():
      return

   harness_dir = cur.get_attribute_as_string("Harness_Dir", "GNATtest")

   if harness_dir == "" :
      harness_dir = "gnattest/harness"

   prj = os.path.join (cur.object_dirs()[0], harness_dir, "test_driver.gpr")
   GPS.Project.load (prj, False, True)
   GPS.Console ("Messages").write ("Switched to harness project: " +
      GPS.Project.root().file().name() +"\n")

def exit_harness_project ():
   """ Leave harness project and open user's project. """
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

def on_compilation_finished(hook, category,
    target_name="", mode_name="", status=""):

   global last_gnattest_project

   if not target_name.startswith("GNATtest"):
      return

   if status:
      return

   open_harness_project (last_gnattest_project)

GPS.Hook("compilation_finished").add(on_compilation_finished)

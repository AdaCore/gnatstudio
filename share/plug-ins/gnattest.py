"""This file provides support for gnattest.
"""



import os.path, GPS

GPS.Preference ("Plugins/gnattest/read_only_color").create (
  "Highlight color", "color",
   """Background color for read-only areas""",
   "#e0e0e0")

last_gnattest_project = None

def run (project, target, extra_args=""):
   """ Run gnattest and switch to harness if success. """
   global last_gnattest_project
   last_gnattest_project = project
   GPS.BuildTarget(target).execute(synchronous=False, extra_args=extra_args)

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

def on_project_view_changed(hook):
   """ Replace run target in harness project. """
   test_run_target=GPS.BuildTarget ("Run a test-driver")
   run_main_target=GPS.BuildTarget ("Run Main")

   if is_harness_project():
      run_main_target.hide()
      test_run_target.show()
   else:
      run_main_target.show()
      test_run_target.hide()
      return

   # Update read-only areas in already opened files
   buffer_list = GPS.EditorBuffer.list()
   for buffer in buffer_list:
      mark_read_only_areas (buffer)

def on_file_edited (hook,file):
   """ Find read-only areas and apply an overlay on them. """
   if not is_harness_project():
      return

   buffer = GPS.EditorBuffer.get (file)
   mark_read_only_areas (buffer)

def mark_read_only_areas (buffer):
   read_only_overlay = None
   loc = buffer.beginning_of_buffer ()

   # Iterate over read-only areas
   while loc:
      found = loc.search ("--  begin read only", dialog_on_failure=False)

      if found:
         from_line,last = found
         found = last.search ("--  end read only", dialog_on_failure=False)

         if found:
            to_line,loc = found
         else:
            loc = None

      else:
         loc = None

      # if area found
      if loc:
         from_line = from_line.beginning_of_line ()
         to_line = to_line.end_of_line ()

         # if overlay hasn't exist yet, create one
         if read_only_overlay == None:
            read_only_overlay = buffer.create_overlay ()
            color = GPS.Preference ("Plugins/gnattest/read_only_color").get ()
            read_only_overlay.set_property ("paragraph-background", color)
            read_only_overlay.set_property ("editable", False)

         buffer.apply_overlay (read_only_overlay, from_line, to_line)
   # No more read-only areas

GPS.Hook("file_edited").add (on_file_edited)
GPS.Hook("compilation_finished").add(on_compilation_finished)
GPS.Hook("project_view_changed").add(on_project_view_changed)

# This is a plug-in that handles cleaning project trees. It drives the
# appropriate tool depending on the user preferences and project properties to
# configure the builder.

from GPS import *

def on_exit(process, status, output):
   # Raise the Messages window and write the cleaning output
   msg = Console ("Messages", accept_input=False)
   MDI.get_by_child (msg).raise_window()
   msg.write (output)

def get_cleaner (root):
   multi_lang_build = Preference ("General-Multi-Language-Build").get() == 1
   multi_lang_builder = Preference ("General-Multi-Language-Builder").get().lower()
   languages = root.languages (recursive=True)

   gprbuild=0
   gnatclean=1

   # -eL requires a recent version of gnatclean/gprclean, but this preference
   # is not enabled by default, so that's a reasonable requirement.

   if Preference ("Prj-Editor-Trusted-Mode").get() == 1:
      symlink_opt=""
   else:
      symlink_opt=" -eL"

   builder=None

   if (len (languages) == 1 and languages [0].lower() == "ada") \
       or (multi_lang_build and multi_lang_builder == "gprmake"):
      builder=gnatclean

   elif multi_lang_build and multi_lang_builder == "gprbuild":
      builder=gprbuild

   else:
      builder=gprbuild
      for lang in languages:
         if lang.lower() == "ada":
            builder=gnatclean

   if builder == gprbuild:
      gnatmake = root.get_attribute_as_string ("compiler_command",
                                               package="ide", index="ada")
      length = len (gnatmake)

      if length > 9 and gnatmake [length - 9:] == "-gnatmake":
         return "gprclean --target=" + gnatmake [:length - 9] + symlink_opt
      else:
         return "gprclean" + symlink_opt
   else:
      return root.get_attribute_as_string ("gnat", package="ide") + " clean" + symlink_opt

def clean_project (recursively=False, root=None):
   prj = root
   if not prj:
      prj = Project.root()

   # Clear the Messages window
   msg = Console ("Messages", accept_input=False)
   msg.clear()

   # Get the cleaning tool
   scenario_variables = Project.scenario_variables_cmd_line (prefix="-X")
   cleaner = get_cleaner (prj)
   cmd = cleaner + " -P" + prj.file().name("Build_Server")
   if recursively:
      cmd = cmd + " -r"
   cmd = cmd + " " + scenario_variables
   p = Process (cmd, on_exit=on_exit, remote_server="Build_Server", show_command=True)

def clean_root_project (menu):
   clean_project()

def clean_all_root_project (menu):
   clean_project (recursively=True)

def clean_context_project (context):
   clean_project (root=context.project())

def on_filter (context):
   if isinstance (context, FileContext) and not isinstance (context, EntityContext) and not isinstance (context, AreaContext):
      try:
         context.directory()
      except:
         try:
            context.file()
         except:
            return True
   return False

Menu.create ("/Build/C_lean/_Root Project",
             on_activate=clean_root_project,
             ref="Make",
             add_before=False)
Menu.create ("/Build/C_lean/_All",
             on_activate=clean_all_root_project,
             ref="Clean",
             add_before=False)
Contextual ("Build/Clean").create (on_activate=clean_context_project,
                                   filter=on_filter)

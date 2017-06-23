"""
Automatically saves the contents of the Messages window when a
compilation has finished.

The output is saved in a file called "messages.txt" (or some other name set in
the preferences) in the root project's object directory. If there is no object
directory for the root project, then the file is saved in the directory of the
project file itself.

"""

from GPS import Preference, Project, Console
from os.path import dirname, join
from gps_utils import hook


file_name_pref = Preference("Plugins/save_on_compile/file_name").create(
    "File Name", "string",
    "Name of the file you want to save the messages into",
    "messages.txt")


@hook('compilation_finished')
def on_compilation_finished(category, target_name, mode_name, status):
    obj_dirs = Project.root().object_dirs(False)
    path = obj_dirs[0] if obj_dirs else dirname(Project.root().file().path)
    base = file_name_pref.get()

    if not base:
        Console().write(
            "plugin save_on_compile.py: no file name is specified in the"
            " preferences\n")
    else:
        try:
            full = join(path, base)
            with open(full, "w") as f:
                f.write(Console().get_text())
            Console().write("Output saved in %s\n" % (full, ))
        except:
            Console().write(
                "plugin save_on_compile.py: error saving in '%s'\n" % (
                    full, ))

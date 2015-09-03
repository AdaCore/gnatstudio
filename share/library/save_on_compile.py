"""
Automatically saves the contents of the Messages window when a
compilation has finished.
The output is saved in a file called "messages.txt" in the root project's
object directory. If there is no object directory for the root project, then
the file will be saved at the root directory of the project itself.
"""

from GPS import Preference, Project, Console, Hook
from os.path import dirname, join


file_name_pref = Preference("Plugins/Save on compile/file_name").create(
    "File Name", "string",
    "Name of the file you want to save the messages into"
    "messages.txt"
)


def on_compilation_finished(hook, category, target_name, mode_name, status):
    obj_dirs = Project.root().object_dirs(False)
    path = obj_dirs[0] if obj_dirs else dirname(Project.root().file().name())

    with file(join(path, file_name_pref.get()), "w") as f:
        f.write(Console().get_text())

Hook("compilation_finished").add(on_compilation_finished)

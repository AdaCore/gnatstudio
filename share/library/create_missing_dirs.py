"""
This script will create missing directories for a project tree when loading
it into GPS. It is different from the gnatmake switch -p, which will also
create the directories but only when the project is compiled. Since the project
is first loaded in GPS, the latter would return errors when creating the
project if some directories are missing.

A new preference is created that allows you to disable the creation
of directories, and the default is to create missing directories.
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
import os
import string

attempted = []  # List of directories that this plugin has attempted to create


def on_project_changed(self):
    global attempted
    CreatedDirs = False
    created = []
    try:
        must_create = GPS.Preference("Auto-Create-Dirs").get()
    except Exception:
        must_create = True

    if must_create:
        prjs = GPS.Project.root().dependencies(True)
        prjs.append(GPS.Project.root())
        for i in prjs:
            dirs = [i.get_attribute_as_string("Exec_Dir"),
                    i.get_attribute_as_string("Library_Dir"),
                    i.get_attribute_as_string("Object_Dir"),
                    i.get_attribute_as_string("Library_Src_Dir"),
                    i.get_attribute_as_string("Library_ALI_Dir"),
                    i.get_attribute_as_string("Artifacts_Dir", "IDE"),
                    i.get_attribute_as_string("Documentation_Dir",
                                              "Documentation")]
            for j in dirs:
                if i and i not in [".", "", " "]:
                    dir = os.path.join(
                        os.path.dirname(i.file().path), j).strip()
                    # Only attempt to create each directory once
                    if dir not in attempted:
                        if not os.path.exists(dir):
                            os.makedirs(dir)
                            attempted.append(dir)
                            created.append(dir)
                            CreatedDirs = True
        if CreatedDirs:
            GPS.Console("Messages").write("Created missing dirs\n")
            GPS.Console("Messages").write(string.join(created, "\n"))
            GPS.Console("Messages").write("\n")
            GPS.Project.recompute()


GPS.parse_xml("""
<preference name="Auto-Create-Dirs"
   page="Plugins/create_missing_dirs"
   label="Auto Create Missing Dirs"
   tip="Automatically creates missing directories when a project is loaded."
   default="True"
   type="boolean"/>
""")

GPS.Hook("project_view_changed").add(on_project_changed)

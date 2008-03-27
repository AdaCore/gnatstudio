"""This script will create missing object-directories for a
 project tree when loading it into GPS.
 It is different from the gnatmake switch -p, which will also create
 the directories but only when the project is compiled. Since the project
 is first loaded in GPS, the latter would return errors when creating the
 project if some directories are missing.

 A new preference is created that allows you to disable the creation
 of directories, and the default is to create missing directories.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Plug-ins)


#############################################################################
## No user customization below this line
#############################################################################

import GPS
import os
from os.path import *
import string

def on_project_changed(self):
  CreatedDirs=False
  try:
     must_create = GPS.Preference ("Auto-Create-Dirs").get()
  except:
     must_create = True

  if must_create:
     prjs = GPS.Project.root().dependencies (True)
     prjs.append (GPS.Project.root())
     created=[]
     for i in prjs :        
        dirs=[i.get_attribute_as_string("Exec_Dir"),
              i.get_attribute_as_string("Library_Dir"),
              i.get_attribute_as_string("Object_Dir"),
              i.get_attribute_as_string("Library_Src_Dir")]
        for j in dirs:
            if i and i not in [".",""," "]:
               dir = join (dirname (i.file().name()), j).strip()
               if dir[-1:] not in ["\\","/"]:
                   if not exists(dir):
                       os.makedirs(dir)
                       created.append(dir)
                       CreatedDirs=True
     if CreatedDirs:
        GPS.Console("").clear()
        GPS.Console("").write("Created missing dirs\n")
        GPS.Console("").write(string.join(created,"\n"))
        GPS.Console("").write("\n")
        GPS.Project.recompute()

GPS.parse_xml("""   
<preference name="Auto-Create-Dirs"
   page="General"
   label="Auto Create Missing Dirs"
   tip="Automatically creates missing directories when a project is loaded."
   default="True"
   type="boolean"/>
""")

GPS.Hook ("project_view_changed").add (on_project_changed)


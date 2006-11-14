"""Local history for files

This script provides a local history for files: every time a file is saved,
it is also committed in a local RCS directory, which can later be used to
easily revert to a previous version.
Compared to the standard undo feature in GPS, this provides a persistent
undo across GPS sessions.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

local_rcs_dir = ".gpsrcs"
## Name of the local directory created to store history. Several such
## directories might be created, one per project

max_revisions = 30
## Keep at most that many revision histories for each file


###########################################################################
## No user customization below this line
############################################################################

from GPS import *
import os.path, shutil

def create_RCS_dir (for_file):
   """Create the RCS directory for the given GPS.File instance.
      The name of the directory is returned"""
   project = for_file.project (default_to_root = False)
   if project:
      dir = project.object_dirs (recursive = False)[0]
   else:
      dir = os.path.dirname (for_file.name())

   dir = os.path.join (dir, local_rcs_dir)

   try:
     os.makedirs (dir)
   except:
     pass

   Logger ("LocalHist").log ("creating directory " + `dir`)
   return dir

def add_to_history (file, dir):
   """Expand the local history for file, to include the current version"""
   pwd = os.getcwd()
   os.chdir (dir)
   shutil.copy2 (file.name(), dir)
   proc = Process ("ci -l " + os.path.basename (file.name()))
   proc.send (".\n")
   proc.wait ()
   os.unlink (os.path.join (dir, os.path.basename (file.name())))
   os.chdir (pwd)

def cleanup_history (file_name, dir):
   """Keep at most max_revisions histories for file"""
   f = file (os.path.join (dir, os.path.basename (file_name.name()) + ",v"))
   version = f.readline().split()[1]
   f.close()

   version = version[2:-1]  # Omit "1." at start, and ";" at end
   version = int (version) - max_revisions
   if version > 0:
      pwd = os.getcwd()
      os.chdir (dir)
      proc = Process ("rcs -o:1." + `version` + " " + \
                      os.path.basename (file_name.name())) 
      proc.wait ()
      os.chdir (pwd)

def has_RCS_on_path():
   """True if RCS was found on the PATH"""
   for path in os.getenv ("PATH").split (os.pathsep):
      if os.path.isfile (os.path.join (path, "ci")) \
       or os.path.isfile (os.path.join (path, "ci.exe")):
        return True
   return False

def on_file_saved (hook, file):
   """Called when a file has been saved"""
   dir = create_RCS_dir (file)
   add_to_history (file, dir)
   cleanup_history (file, dir)

def register_module (hook):
   """Activate this local history module if RCS is found on the path"""

   if has_RCS_on_path():
     Hook ("file_saved").add (on_file_saved, last = True)     
   
Hook ("gps_started").add (register_module)

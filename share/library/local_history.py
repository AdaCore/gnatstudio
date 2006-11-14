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

max_days = 2
## Keep revisions for that many days at most. See also max_revisions

max_revisions = 200
## Maximal number of completions to keep. See also max_days


###########################################################################
## No user customization below this line
############################################################################

from GPS import *
import os.path, shutil, datetime, traceback, time, stat

def create_RCS_dir (for_file, allow_create=True):
   """Create the RCS directory for the given GPS.File instance.
      Return a tuple (dir, full rcs file path).
      If allow_create is False, and the directory doesn't exists, None
      is returned."""
   project = for_file.project (default_to_root = False)
   if project:
      dir = project.object_dirs (recursive = False)[0]
   else:
      dir = os.path.dirname (for_file.name())

   dir = os.path.join (dir, local_rcs_dir)

   if not allow_create and not os.path.isdir (dir):
      return None

   if allow_create:
     try:
       os.makedirs (dir)
       Logger ("LocalHist").log ("creating directory " + `dir`)
     except: pass

   return (dir, os.path.join (dir, os.path.basename (for_file.name())) + ",v")

def add_to_history (file, dir):
   """Expand the local history for file, to include the current version"""
   pwd = os.getcwd()
   os.chdir (dir)
   shutil.copy2 (file.name(), dir)
   proc = Process ("rcs -l " + os.path.basename (file.name()))
   if proc.wait() == 0:
      proc = Process ("ci " + os.path.basename (file.name()))
      proc.send (".\n")
      proc.wait ()
   os.chdir (pwd)

def get_revisions (rcs_file):
   """Given a ,v file, extract all revisions and associated dates.
      Result is a list of tuples: (revision_number, date), where
      revision is the RCS revision number less the "1." prefix.
      First in the list is the most recent revision."""
   f = file (rcs_file)
   result = []

   for line in f.readlines():
     if line.startswith ("log"): break
     if line.startswith ("date\t"):
       date = line.split()[1]
       result.append ((int (previous[2:]), date[:-1]))
     previous = line

   f.close ()
   return result

def cleanup_history (dir, rcs_file):
   """Keep at most max_revisions histories for file"""
   older = datetime.datetime.now() - datetime.timedelta (days = max_days)
   older = older.strftime ("%Y.%m.%d.%H.%M.%S")

   revisions = get_revisions (rcs_file)

   version = max (0, revisions[0][0] - max_revisions)
   for r in revisions:
     if r[1] < older:
        version = max (version, r[0])
        break

   if version >= 1:
      Logger ("LocalHist").log \
        ("Truncating file " + rcs_file + " to revision " + `version`)
      pwd = os.getcwd()
      os.chdir (dir)
      proc = Process ("rcs -o:1." + version + " " + os.path.basename (rcs_file))
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
   (dir, rcs_file) = create_RCS_dir (file)
   add_to_history (file, dir)
   cleanup_history (dir, rcs_file)

def has_local_history (file):
   """Whether there is local history information for file"""
   rcs = create_RCS_dir (file, allow_create = False)
   return rcs and os.path.isfile (rcs[1])

def contextual_filter (context):
   try:    return has_local_history (context.file())
   except: 
     Logger ("LocalHist").log ("Unexpected exception " + traceback.format_exc())
     return False

def revert_file (file, revision):
   """Revert file to a local history revision"""
   Logger ("LocalHist").log ("revert " + file.name() + " to " + revision)
   (dir, rcs_file) = create_RCS_dir (file, allow_create = False)
   pwd = os.getcwd()
   os.chdir (dir)

   try: os.unlink (os.path.basename (file.name()))
   except: pass

   proc = Process ("co -r" + revision + " " + os.path.basename (rcs_file))
   if proc.wait() == 0:
      shutil.copymode (file.name(), os.path.basename (file.name()))
      shutil.move (os.path.basename (file.name()), file.name())
      EditorBuffer.get (file, force = True)
   os.chdir (pwd)

def contextual_factory (context):
   (dir, rcs_file) = create_RCS_dir (context.file())
   revisions = get_revisions (rcs_file)
   context.revisions = ["1." + `a[0]` for a in revisions]

   result = []
   for a in revisions:
     date = datetime.datetime (*(time.strptime (a[1], "%Y.%m.%d.%H.%M.%S")[0:6]))
     result.append (date.strftime ("%Y/%m/%d %H:%M:%S"))
   return result

def on_revert (context, choice, choice_index):
   file = context.file ()
   revert_to = context.revisions [choice_index]
   revert_file (file, revert_to)

def register_module (hook):
   """Activate this local history module if RCS is found on the path"""

   if has_RCS_on_path():
     Hook ("file_saved").add (on_file_saved, last = True)     
     Contextual ("Local History Revert to").create_dynamic \
       (factory     = contextual_factory,
        on_activate = on_revert,
        label       = "Local History/Revert To",
        filter      = contextual_filter)
   
Hook ("gps_started").add (register_module)

"""Local history for files

This script provides a local history for files: every time a file is saved,
it is also committed in a local RCS directory, which can later be used to
easily revert to a previous version.
Compared to the standard undo feature in GPS, this provides a persistent
undo across GPS sessions.

You must install RCS. On Unix systems, this is generally available by
default. On Windows, this is available through the cygwin environment.
If RCS is not detected on your PATH, this module will do nothing.

A new contextual menu is shown for files that have a local history. This
menu allows you to view the diff between the current version of the file
and the version at the selected time, or to revert to a specific version.

Revert doesn't ask for confirmation! But if you have saved the file before
the revert, the current version is in the local revision history, and can
therefore be reverted.
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
from os.path import *
import os, shutil, datetime, traceback, time, stat

class LocalHistory:
  """This class provides access to the local history of a file"""

  def __init__ (self, file):
     """Create a new instance of LocalHistory.
        File must be an instance of GPS.File"""

     self.file = file.name ()
     project   = file.project (default_to_root = False)
     if project:
        dir = project.object_dirs (recursive = False)[0]
     else:
        dir = dirname (self.file)

     self.rcs_dir = join (dir, local_rcs_dir)
     self.rcs_file = join (self.rcs_dir, basename (self.file)) + ",v"

  def get_revisions (self):
     """Extract all revisions and associated dates.
        Result is a list of tuples: (revision_number, date), where
        revision is the RCS revision number less the "1." prefix.
        First in the list is the most recent revision."""
     f = file (self.rcs_file)
     result = []

     for line in f.readlines():
       if line.startswith ("log"): break
       if line.startswith ("date\t"):
         date = line.split()[1]
         result.append ((int (previous[2:]), date[:-1]))
       previous = line

     f.close ()
     return result

  def add_to_history (self):
     """Expand the local history for file, to include the current version"""
     if not isdir (self.rcs_dir):
       os.makedirs (self.rcs_dir)
       Logger ("LocalHist").log ("creating directory " + `self.rcs_dir`)

     shutil.copy2 (self.file, self.rcs_dir)
     proc = Process ("rcs -l " + self.rcs_file)
     if proc.wait() == 0:
        pwd = os.getcwd()
        os.chdir (self.rcs_dir)
	proc = Process ("ci " + self.rcs_file)
	proc.send (".\n")
	proc.wait ()
        os.chdir (pwd)

  def cleanup_history (self):
     """Remove the older revision histories for self"""
     older = datetime.datetime.now() - datetime.timedelta (days = max_days)
     older = older.strftime ("%Y.%m.%d.%H.%M.%S")

     revisions = self.get_revisions ()

     version = max (0, revisions[0][0] - max_revisions)
     for r in revisions:
       if r[1] < older:
	  version = max (version, r[0])
	  break

     if version >= 1:
	Logger ("LocalHist").log \
	  ("Truncating file " + self.rcs_file + " to revision " + `version`)
	proc = Process ("rcs -o:1." + version + " " + self.rcs_file)
	proc.wait ()

  def local_checkout (self, revision):
     """Do a local checkout of file at given revision in the RCS directory.
	Return the name of the checked out file"""
     if isdir (self.rcs_dir):
       try:    os.unlink (join (self.rcs_dir, basename (self.file)))
       except: pass

       pwd = os.getcwd ()
       os.chdir (self.rcs_dir)
       proc = Process ("co -r" + revision + " " + self.rcs_file)
       os.chdir (pwd)
       if proc.wait() == 0:
         return join (self.rcs_dir, basename (self.file))
     return None

  def revert_file (self, revision):
     """Revert file to a local history revision"""
     Logger ("LocalHist").log ("revert " + self.file + " to " + revision)
     local = self.local_checkout (revision)
     if local:
	shutil.copymode (self.file, local)
	shutil.move (local, self.file)
	EditorBuffer.get (File (self.file), force = True)

  def diff_file (self, revision, file_ext="old"):
     """Compare the current version of file with the given revision.
        The referenced file will have a name ending with file_ext"""
     local = self.local_checkout (revision)
     local2 = basename (local) + " " + file_ext
     local2 = join (self.rcs_dir, local2)
     shutil.move (local, local2)
     Vdiff.create (File (self.file), File (local2))
     try: os.unlink (local2)
     except: pass

  def has_local_history (self):
     """Whether there is local history information for self"""
     return isfile (self.rcs_file)


def has_RCS_on_path():
   """True if RCS was found on the PATH"""
   for path in os.getenv ("PATH").split (os.pathsep):
      if os.path.isfile (os.path.join (path, "ci")) \
       or os.path.isfile (os.path.join (path, "ci.exe")):
        return True
   return False

def on_file_saved (hook, file):
   """Called when a file has been saved"""
   hist = LocalHistory (file)
   hist.add_to_history ()
   hist.cleanup_history ()

def contextual_filter (context):
   try: 
     hist = LocalHistory (context.file())
     return hist.has_local_history ()
   except: 
     Logger ("LocalHist").log ("Unexpected exception " + traceback.format_exc())
     return False

def contextual_factory (context):
   hist = LocalHistory (context.file())
   revisions = hist.get_revisions ()

   # Save in the context the result of parsing the file. This factory is
   # used for multiple contextual menus, so this saves some processing.
   # Part of this parsing is also needed when performing the action.

   try:
      return context.revisions_menu
   except:
      context.revisions = ["1." + `a[0]` for a in revisions]
      result = []
      for a in revisions:
        date = datetime.datetime (*(time.strptime (a[1], "%Y.%m.%d.%H.%M.%S")[0:6]))
        result.append (date.strftime ("%Y-%m-%d/%H:%M:%S"))
      context.revisions_menu = result
      return context.revisions_menu

def on_revert (context, choice, choice_index):
   hist = LocalHistory (context.file())
   hist.revert_file (context.revisions [choice_index])

def on_diff (context, choice, choice_index):
   hist = LocalHistory (context.file())
   hist.diff_file (context.revisions [choice_index], choice)

def register_module (hook):
   """Activate this local history module if RCS is found on the path"""

   if has_RCS_on_path():
     Hook ("file_saved").add (on_file_saved, last = True)     
     Contextual ("Local History Revert to").create_dynamic \
       (factory     = contextual_factory,
        on_activate = on_revert,
        label       = "Local History/Revert To",
        filter      = contextual_filter)
     Contextual ("Local History Diff").create_dynamic \
       (factory     = contextual_factory,
        on_activate = on_diff,
        label       = "Local History/Diff",
        filter      = contextual_filter)
   
Hook ("gps_started").add (register_module)

"""
This plugin adds a new contextual menu in the Project View and editors, which
allow you to create a new file in the corresponding directory.
In some cases, this is more convenient than using the /File/New menu, and
then have to navigate to the directory in which you want to create the new
file.
"""



from GPS import *
import os.path

class Create_File_Contextual (Contextual):

   def __init__ (self):
      Contextual.__init__ (self, "Create File From Dir")
      self.create (
         on_activate = self.on_activate,
         label       = self.label,
         filter      = self.filter)

   def label (self, context):
      return "File operations/New File..."

   def on_activate (self, context):
      dir = context.directory ()
      name, = MDI.input_dialog (
         "Enter file name (in directory \n%s)" % dir, "Name")
      if name:
         name = os.path.join (dir, name)
         if not os.path.isfile (name):
            f = file (name, "w")
            f.write ("")
            f.close ()
         EditorBuffer.get (File (name))
         Project.recompute ()

   def filter (self, context):
      try:
         dir = context.directory ()
         return True
      except:
         return False

Create_File_Contextual ()




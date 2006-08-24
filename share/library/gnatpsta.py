"""Display Ada standard package

This script adds a new menu in /Help/GNAT Runtime/Standard to display the
standard.ads package. This package contains the definition of basic types
for the Ada runtime, but doesn't exist as a file on the disk since it can't
be described in pure Ada.
It properly handles older versions of GNAT, which came with a gnatpsta
command line utility, as well as new versions where the gcc driver itself
must be used.
"""


###########################################################################
## No user customization below this line
############################################################################

from GPS import *
import os, tempfile

def on_gps_started (hook):
  parse_xml ("""
   <action name="Display standard.ads" output="none">
      <shell lang="python">gnatpsta.display()</shell>
   </action>
   <menu action="Display standard.ads">
      <title>/Help/GNAT Runtime/Standard</title>
   </menu>""")

def on_exit (process, exit_status, output):
   if exit_status == 0:
      f = file (process.standard, "w")
      f.write (output)
      f.close ()
      buffer = EditorBuffer.get (File (process.standard))
      buffer.current_view().set_read_only (True)
      os.unlink (process.standard)

def display():
   # Two possible ways here: older versions of GNAT still have the
   # gnatpsta utility, whereas for more recent versions with need
   # to compile a file with -gnatS. Try the second solution first:

   dir  = tempfile.mkdtemp ()
   path = None

   try:
      proc = Process ("gnatpsta", on_exit=on_exit)
   except:
      path = dir + "/p.ads"
      f = open (path, "w")
      f.write ("package p is end p;")
      f.close ()
      proc = Process ("gcc -c -xada -gnatc -gnatS " + path, on_exit=on_exit)

   proc.standard = dir + "/standard.ads"
   proc.wait()

   if path: os.unlink (path)
   os.rmdir  (dir)


Hook ("gps_started").add (on_gps_started)

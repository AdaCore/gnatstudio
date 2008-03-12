## This file demonstrates how one could implement special highlighting
## rules in the editors.
## This examples highlights the SPARK annotations, ie special Ada comments
## that start with "--#". This code is not actually useful since GPS has its
## own built-in mechanism for these highlights now, but it might still be
## used for special annotations, like the convention we use in the GNAT and
## GPS sources with ??? in comments to indicate code to be reviewed

spark_comments_fg_color = "red"

highlight_on_edit = True
## Set to False to only highlight when a file is loaded or saved. This
## is more efficient in case the default setting is too slow

from GPS import *

def highlight_spark_comments (buffer):
   try:
      buffer.remove_overlay (buffer.spark_overlay)
   except:
      buffer.spark_overlay = buffer.create_overlay ("spark")
      buffer.spark_overlay.set_property ("foreground", spark_comments_fg_color)

   loc = buffer.beginning_of_buffer()

   while True:
     loc = loc.search ("--#", dialog_on_failure=False)
     if not loc:
        return
     loc = loc[0]
     endloc = loc.end_of_line()
     buffer.apply_overlay (buffer.spark_overlay, loc, endloc)
     loc = endloc + 1

def on_file_edited (hook, file):
   highlight_spark_comments (EditorBuffer.get (file))

def on_character_added (hook, file):
   highlight_spark_comments (EditorBuffer.get (file))

def on_gps_started (hook):
   Hook ("file_edited").add (on_file_edited)
   Hook ("file_saved").add (on_file_edited)
   Hook ("file_changed_on_disk").add (on_file_edited)

   if highlight_on_edit:
      Hook ("character_added").add (on_character_added)

   for l in EditorBuffer.list():
      highlight_spark_comments (l)
      

Hook ("gps_started").add (on_gps_started)

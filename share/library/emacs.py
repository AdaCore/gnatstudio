"""
Provides Emacs-like keybindings.

This file overrides a number of the standard keybindings in GPS, to
be as close as possible to the Emacs editor.
No additional feature is provided in this package, and therefore you
can easily use for instance the interactive search even if you do not
like the Emacs key shortcuts.

You can override any of these key shortcuts through the menu
/Edit/Key Shortcuts.

See the source of this script for the full list of key shortcuts.
"""



import GPS
import isearch
import rectangles
import text_utils
import navigation_utils
import sys

alt = 'alt' if sys.platform != 'darwin' else 'cmd'

XML = r"""<?xml version="1.0" ?>
<GPS>
  <!--  disable GPS key shortcuts which conflict with emacs shortcuts -->

  <key action="/Edit/Smart Completion" />

  <key action="">control-x</key>
  <key action="">control-c</key>

  <!--  define key shortcuts -->

  <key action="subprogram box">control-c n</key>
  <key action="kill line">control-k</key>
  <key action="kill forward">control-d</key>
  <key action="Delete word forward">%(alt)s-d</key>
  <key action="transpose chars">control-t</key>
  <key action="Transpose lines">control-x control-t</key>
  <key action="/File/Save">control-x control-s</key>
  <key action="/File/Save As..." >control-x control-w</key>
  <key action="goto beginning of line">control-a</key>
  <key action="goto end of line">control-e</key>
  <key action="goto beginning of buffer">%(alt)s-less</key>
  <key action="goto end of buffer">%(alt)s-shift-greater</key>
  <key action="Format selection" >tab</key>
  <key action="/File/Open...">control-x control-f</key>
  <key action="/Edit/Undo">control-_</key>
  <key action="/Edit/Refill">%(alt)s-q</key>
  <key action="repeat next">control-u</key>

  <key action="goto declaration or body">control-c control-d</key>
  <key action="goto other file">control-c o</key>

  <key action="/Navigate/Back">control-c control-s</key>
  <key action="open line">control-o</key>
  <key action="Join line">%(alt)s-j</key>
  <key action="Upper case word">%(alt)s-u</key>
  <key action="Lower case word">%(alt)s-l</key>
  <key action="Capitalize word">%(alt)s-c</key>
  <key action="/Navigate/Goto Line...">%(alt)s-g</key>
  <key action='zap to char'>%(alt)s-z</key>
  <key action='just one space'>%(alt)s-space</key>

  <key action="set mark command">control-space</key>
  <key action="Cancel mark command">control-g</key>

  <key action="Global Search in context: actions">%(alt)s-x</key>
  <key action="Global Search in context: opened windows">control-x b</key>

  <key action="/Edit/Paste">control-y</key>
  <key action="/Edit/Paste Previous">%(alt)s-y</key>
  <key action="/Edit/Copy">%(alt)s-w</key>
  <key action="/Edit/Cut">control-w</key>

  <key action="Macro Start Keyboard">control-x parenleft</key>
  <key action="Macro Stop">control-x parenright</key>
  <key action="Macro Play">control-x e</key>

  <key action="/Edit/Undo">shift-control-underscore</key>

  <key action="/File/Close">control-x k</key>

  <key action="/File/Exit">control-x control-c</key>

  <key action="/Navigate/Find or Replace...">control-s</key>

  <key action="delete horizontal space">%(alt)s-backslash</key>

  <key action="New View Vertical">control-x 2</key>
  <key action="New View Horizontal">control-x 3</key>

  <key action="Move to next window">control-x control-b</key>
  <key action="Move to previous window" />

  <key action="Select other window">control-x o</key>

  <key action="Center cursor on screen">control-l</key>

  <key action="Delete word backward">%(alt)s-BackSpace</key>

  <key action="Move to previous word">%(alt)s-Left</key>
  <key action="Move to next word">%(alt)s-Right</key>
  <key action="Move to previous page">%(alt)s-v</key>
  <key action="Move to next page">control-v</key>

  <key action="/Edit/Rectangle/Cut">control-x r k</key>
  <key action="/Edit/Rectangle/Paste">control-x r y</key>
  <key action="/Edit/Rectangle/Delete">control-x r d</key>
  <key action="/Edit/Rectangle/Clear">control-x r c</key>
  <key action="/Edit/Rectangle/Open">control-x r o</key>
  <key action="/Edit/Rectangle/Replace with Text">control-x r t</key>

  <key action="/Navigate/Find Incremental">control-s</key>
  <key action="/Navigate/Find Previous Incremental">control-r</key>
</GPS>
""" % {"alt": alt}

GPS.parse_xml(XML)

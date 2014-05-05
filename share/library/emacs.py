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

  <key action="complete identifier (advanced)" />

  <key action="">control-x</key>
  <key action="">control-c</key>

  <!--  define key shortcuts -->

  <key action="subprogram box">control-c n</key>
  <key action="kill line">control-k</key>
  <key action="kill forward">control-d</key>
  <key action="delete word forward">%(alt)s-d</key>
  <key action="transpose chars">control-t</key>
  <key action="transpose lines">control-x control-t</key>
  <key action="save">control-x control-s</key>
  <key action="save as" >control-x control-w</key>
  <key action="goto beginning of line">control-a</key>
  <key action="goto end of line">control-e</key>
  <key action="goto beginning of buffer">%(alt)s-less</key>
  <key action="goto end of buffer">%(alt)s-shift-greater</key>
  <key action="format selection" >tab</key>
  <key action="open file">control-x control-f</key>
  <key action="refill">%(alt)s-q</key>
  <key action="repeat next">control-u</key>

  <key action="goto declaration or body">control-c control-d</key>
  <key action="goto other file">control-c o</key>

  <key action="backward locations history">control-c control-s</key>
  <key action="open line">control-o</key>
  <key action="join line">%(alt)s-j</key>
  <key action="Upper case word">%(alt)s-u</key>
  <key action="Lower case word">%(alt)s-l</key>
  <key action="Capitalize word">%(alt)s-c</key>
  <key action="goto line">%(alt)s-g</key>
  <key action='zap to char'>%(alt)s-z</key>
  <key action='just one space'>%(alt)s-space</key>

  <key action="set mark command">control-space</key>
  <key action="cancel mark command">control-g</key>

  <key action="global search in context: actions">%(alt)s-x</key>
  <key action="global search in context: opened windows">control-x b</key>

  <key action="paste from clipboard">control-y</key>
  <key action="paste previous from clipboard">%(alt)s-y</key>
  <key action="copy to clipboard">%(alt)s-w</key>
  <key action="cut to clipboard">control-w</key>

  <key action="macro start Keyboard">control-x parenleft</key>
  <key action="macro stop">control-x parenright</key>
  <key action="macro play">control-x e</key>

  <key action="undo">control-underscore</key>
  <key action="redo">shift-control-underscore</key>

  <key action="close current window">control-x k</key>

  <key action="exit">control-x control-c</key>

  <key action="delete horizontal space">%(alt)s-backslash</key>

  <key action="new view vertical">control-x 2</key>
  <key action="new view horizontal">control-x 3</key>

  <key action="move to next window">control-x control-b</key>
  <key action="move to previous window" />

  <key action="select other window">control-x o</key>

  <key action="center cursor on screen">control-l</key>

  <key action="delete word backward">%(alt)s-BackSpace</key>

  <key action="move to previous word">%(alt)s-Left</key>
  <key action="move to next word">%(alt)s-Right</key>
  <key action="move to previous page">%(alt)s-v</key>
  <key action="move to next page">control-v</key>

  <key action="rectangle_cut">control-x r k</key>
  <key action="rectangle_paste">control-x r y</key>
  <key action="rectangle_delete">control-x r d</key>
  <key action="rectangle_clear">control-x r c</key>
  <key action="rectangle_open">control-x r o</key>
  <key action="rectangle_string">control-x r t</key>

  <key action="isearch">control-s</key>
  <key action="isearch backward">control-r</key>
</GPS>
""" % {"alt": alt}

GPS.parse_xml(XML)

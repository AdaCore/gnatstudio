plug-ins enhancements
---------------------

New :file:`formatfile.py` :feature:`(GPS -- 2013-06-21 -- M621-005)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This optional plug-in adds a new menu `/Edit/Format Whole File` which reformats
the whole file using GPS internal formatting, while preserving the current
cursor location.

This is a simple plug-in that can be used as an example on how to create
new menus and preserve the current location.

Speed up in :file:`highlighter.py` :feature:`(GPS -- 2013-03-27 -- M307-008)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This python module is used as the basis for all python scripts that need to
highlight (part of) editors. In particular, you can easily extend it to
highlight specific text or regular expressions in your buffer. This script
now provides an `OverlayStyle` class, which supports more properties than
before (in particular, you can strike through or underline specific text
in your editors with just a few lines of code).

This improves two user-visible plugins, :file:`dispatching.py` and
:file:`auto_highlight_occurrences.py`.

Removed: :file:`execute_extended.py` :feature:`(GPS -- 2013-05-29 -- M529-012)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This plugin allows one to easily execute any of the GPS commands via the
keyboard. This behavior is now directly available via the new global search box
in the GPS toolbar.


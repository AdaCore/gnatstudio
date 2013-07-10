plug-ins and python enhancements
--------------------------------

CodePeer
~~~~~~~~

Simplified CodePeer report window :feature:`(GPS -- 2013-03-11 -- M129-023)`
............................................................................

The CodePeer report window has been simplified to improve usability and remove
unneeded information. Access to the various filters is also improved.

New review classification of CodePeer messages :feature:`(GPS -- 2013-06-13 -- M301-031)`
.........................................................................................

When doing a manual review of a CodePeer message, the user can
no longer change the message ranking, but instead specify a review
classification and the name of the reviewer. New message review dialog is
added and new filter was added for CodePeer Report view to manage visibility
of messages by review status.

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

GPS.EditorOverlay can highlight a whole line :feature:`(GPS -- 2012-12-05 -- LB15-037)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The EditorOverlay class can now be used in Python plugins to highlight an
entire line, through the property "paragraph-background"::

    # Create an overlay for an editor:
    b = GPS.EditorBuffer.get(GPS.File ("my_file.adb"))
    o = b.create_overlay("my_overlay_name")

    # Set the paragraph-background property to pink
    o.set_property ("paragraph-background", "#f0c0c0")

    # This highlights the entire line 317 in my file
    b.apply_overlay (o,
       GPS.EditorLocation(b, 317, 1),
       GPS.EditorLocation(b, 317, 1))

GPS.MDI.add to put on top of consoles :feature:`(GPS -- 2013-06-07 -- M218-036)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The python function `GPS.MDI.add` now has additional parameters to specify
the initial location of the new widget. This allows you to put widgets on
top of the existing GPS consoles for instance.

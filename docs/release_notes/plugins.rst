Plug-ins and python enhancements
--------------------------------

Plug-ins dialog
~~~~~~~~~~~~~~~

..  NF-60-M705-018 GPS: clean up Plug-ins dialog (2013-07-05)

The plug-ins dialog has been simplified. It no longer shows the python files
that are mandatory for GPS. It no longer supports custom initialization
commands for the modules (since standard python can be used for this). The
implementation of the plug-ins is now accessible via an hyper link, instead of
being displayed in this dialog.

Spark
~~~~~

..  NF-60-M531-032 SPARK 2014 support (2013-07-30)

The SPARK 2014 toolset is now supported from both GPS and GNATbench.  A
:menuselection:`Prove` menu will be available if the SPARK 2014 toolset is
installed and found on the PATH at start up.

CodePeer
~~~~~~~~

Progress bar when running gps_codepeer_bridge
.............................................

..  NF-60-LB28-008 GPS: progress bar when running gps_codepeer_bridge (2013-08-08)

After running codepeer, :program:`gps_codepeer_bridge` is launched to load
messages, which can take a long time on large sources. A progress bar is now
displayed during this phase to give feedback to the user.

Separate file for audit data
............................

..  NF-60-LC17-029 GPS: use separate file for CodePeer's audit data (2013-09-16)

CodePeer's audit data for messages are stored in separate file, to improve
performance of loading it on subsequence GPS run.

Simplified CodePeer report window
.................................

.. NF-60-M129-023 GPS: simplified CodePeer report window (2013-03-11)

The CodePeer report window has been simplified to improve usability and remove
unneeded information. Access to the various filters is also improved.

New review classification of CodePeer messages
..............................................

.. NF-60-M301-031 New review classification of CodePeer messages (2013-06-13)

When doing a manual review of a CodePeer message, the user can
no longer change the message ranking, but instead specify a review
classification and the name of the reviewer. New message review dialog is
added and new filter was added for CodePeer Report view to manage visibility
of messages by review status.

Generate CSV report
...................

..  NF-60-M808-017 GPS: New menu CodePeer>Generate CSV Report (2013-09-22)

As part of the CodePeer integration, a new menu
:menuselection:`CodePeer-->Generate CSV Report` is provided to generate a CSV
report file directly from GPS.

New :file:`formatfile.py`
~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M621-005 GPS: new plugin formatfile.py (2013-06-21)

This optional plug-in adds a new menu :menuselection:`Edit-->Format Whole File`
which reformats the whole file using GPS internal formatting, while preserving
the current cursor location.

This is a simple plug-in that can be used as an example on how to create
new menus and preserve the current location.

New :file:`modules.py`
~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M807-066 GPS: new plug-in modules.py (2013-08-12)

A new support python script named :file:`modules.py` has been added to GPS.  It
provides a high-level interface to extending GPS. In particular, it makes it
possible to create new views that are saved in the desktop and restored when
GPS is restarted.

Align end of line comments in :file:`align.py`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M913-030 GPS: align end-of-line comments (2013-09-13)

The :file:`align.py` plugin now provides a solution to align end of line comments.

..  NF-60-M711-042 GPS: <tab> also aligns selected lines (2013-09-12)

By default, if multiple lines are selected and you press :kbd:`<tab>`, GPS will
also align the colons (:), use clauses (use), arrows (=>) and assignments (:=)
in these lines. This behavior can be deactivated by a new preference in the
:guilabel:`Editors` section.

Speed up in :file:`highlighter.py`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-M307-008 GPS: refactoring of highlighter.py (2013-03-27)

This python module is used as the basis for all python scripts that need to
highlight (part of) editors. In particular, you can easily extend it to
highlight specific text or regular expressions in your buffer. This script
now provides an `OverlayStyle` class, which supports more properties than
before (in particular, you can strike through or underline specific text
in your editors with just a few lines of code).

This improves two user-visible plugins, :file:`dispatching.py` and
:file:`auto_highlight_occurrences.py`.

Removed: :file:`execute_extended.py`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M529-012 GPS: execute_extended.py plugin removed (2013-05-29)

This plugin allows one to easily execute any of the GPS commands via the
keyboard. This behavior is now directly available via the new global search box
in the GPS toolbar.

GPS.EditorOverlay can highlight a whole line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-LB15-037 GPS: EditorOverlay can now highlight a whole line (2012-12-05)

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

GPS.MDI.add to put on top of consoles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-M218-036 GPS: GPS.MDI.add to put on top of consoles (2013-06-07)

The python function `GPS.MDI.add` now has additional parameters to specify
the initial location of the new widget. This allows you to put widgets on
top of the existing GPS consoles for instance.

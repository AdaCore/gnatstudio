*********************
Building applications
*********************

Select the icon `Build Main: sdc.adb` on the toolbar (fourth icon from the
right): this will launch a complete build of the *sdc* application. Note also
that you can use a key binding directly instead of this tool bar button
(:kbd:`F4`), or use the corresponding menu item `Build->Project->Sdc->sdc.adb`.
If you use the menu item, an extra intermediate dialog is displayed showing the
actual command line that will be used by GPS. Pressing :kbd:`Execute` will
launch the build as well.

The build has generated a number of errors in a new window: the *Locations*
tree, displayed in the bottom area. The errors are also highlighted in the
corresponding source editor.

GPS has automatically jumped to the first error message (*sdc.adb, 28:6  :
(style) bad indentation*), at the line (28) and column (6) of the error.

Fix the error by hand by inserting a space.

Now you can fix the next error by moving the cursor to the line 30 (press the
:kbd:`down` arrow twice), and by using :kbd:`Tab`: this key
shortcut asks the source editor to automatically re-indent the current line.

Note that you can change this shortcut from the key shortcuts section of the
Preferences dialog (menu `Edit->Preferences`, `General/Key shortcuts` section,
`Format Selection` item).

You can then fix all the remaining errors by selecting the whole block (from
line 28 to line 40) and pressing :kbd:`Tab`. To select a block, you can
either click on the left mouse button and select the area while holding the
button, or using the keyboard by pressing the :kbd:`Shift` key and moving the
cursor using the :kbd:`Up` or :kbd:`Down` keys.

Press the :kbd:`F4` key to build again. GPS will automatically save the
modified files, and start a build. This behavior (automatic saving of files
before building) can be configured in the preferences dialog.

If you look to the right of the toolbar in the GPS window, next to the
omni-search entry, you will notice that a progress bar has appeared,
displaying the current number of files compiled, and the number of remaining
files. This progress bar disappears when the build is finished.

This should now report a successful build.

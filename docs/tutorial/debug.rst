*****
Debug
*****

Open the preferences dialog (menu `Edit->Preferences...`) and click on the
`Debugger` item on the left; set the button `Break on exceptions` to *Enabled*:
this will enable by default a special breakpoint every time an exception is
raised. Click on `Close` to close dialog.

Now select the menu `Debug->Initialize->Sdc->sdc`: GPS automatically switches
to the *Debug* perspective as shown in the menu `Window->Perspectives`, and new
windows have appeared: the variables window, the breakpoints view and
the debugger console at the bottom, and the call stack window on the right.

You can also look at the various debug menu item and tool bar buttons which are
now activated.

On the call stack window (you can use the menu `Debug->Data->Call Stack` to
open it if you do not have it displayed), select the local configuration menu:
various pieces of information can be displayed or removed in the call stack.
From this local configuration menu, add the `Frame Number` info by clicking on
it.

Now select the menu `Debug->Run...` and type `input.txt` in the text input
field. Check that 'Stop at beginning of main subprogram' and 'Use exec dir
instead of current dir' are not selected. Click on `OK`: the debugger should
stop on an exception (`Constraint_Error` in the file :file:`stack.adb`, at line
49).

Go up in the call stack by clicking on the `tokens.process` frame (frame number
will vary, depending on your GNAT version and platform).

If you move the mouse over the parameter `T` at line 64, a tool tip is
displayed showing the value of `T`. You have probably noticed that tool tips,
like menus, are contextual: depending on the current session and on the entity
selected, different information is displayed.

Select the contextual menu `Debug->Display T`: this will highlight the data
window, with a new box displaying graphically the contents of the different
fields of `T`, each clearly separated.

Move your mouse over the `1:T` box, select the contextual menu `Display->Show
Value + Type`: this displays for all fields both their type and value.

Special colors are used in the data display: blue for pointers that can be
dereferenced by a double-click (double click on `T.val`); red for fields that
have been modified since last step.

From the T box, right-click to display the contextual menu and select `View
memory at address of T`: a memory view is opened on top of the source editors.
Use the `up` and `down` arrows on the right to visit memory.

Click in the memory dump, and modify it by typing numbers. Notice the red color
for modified values; click on `Undo Changes` to cancel the modifications; then
close the memory window by e.g. clicking on the `x` icon in the tab or pressing
:kbd:`Ctrl-W`.

In the call stack, go back to the `stack.push` frame.  Move the mouse
over `Last` and let the debugger display its value: 0.  From the contextual
menu, select `Goto declaration of Last`: this will jump to the line 16 of
:file:`stack.adb`, where you can see that `Last` is a `Natural`. Now click on
the `Goto Previous Location` button in the tool bar: we're now back at line 49
where we can see that for a `Push` procedure, `Last` should be incremented, and
not decremented.

Fix the line to `Last := Last + 1;`

Save the file (:kbd:`Ctrl-S`); End the debug session: menu `Debug->Terminate`;
Rebuild (press :kbd:`F4` key); Rerun (menu `Build->Run->sdc`, click on
`Execute`): the program now completes as expected. Close the execution window.

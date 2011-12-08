***************
Code Completion
***************

Go on the line 38 of sdc.adb. You can see that there is a null instruction for
the case of Stack.Overflow. We are going to add some code here, using the code
assist capabilities.

Type :kbd:`enter` to create a new line, and then `Scr`, and hit
:kbd:`Ctrl+Space`.  A completion popup will be displayed, showing all the
entities of the project begining with `Scr`. Double click on `Screen_Output`:
the code is automatically completed in the editor. Then add a dot in your code.
The completion popup will be automatically triggered, and will offer you to
complete your code with the entities contained in the `Screen_Output` package.
Select `Msg`, add a space, and then an open parenthesis. Once again, the
completion windows will pop up, and show you the possible parameters for msg.
If you choose the first entry of the completion list ("params of Msg"), the
call will be automatically completed by a list of named parameters. Complete
the list by giving e.g. `"The stack is full."` for `S1`, `""` for `S2`, and
`True` for `End_Line`.

Don't forget to add a semicolon at the end of the instruction. Then hit
:kbd:`F4` in order to rebuild the application.


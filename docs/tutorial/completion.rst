***************
Code Completion
***************

Go to line 38 of sdc.adb. You can see there is a null instruction for the case
of Stack.Overflow. We are going to add some code there, using the code assist
capabilities.

Type :kbd:`Enter` to create a new line, then type `Scr`. A completion popup is
displayed, showing all the entities of the project begining with `Scr`. Double
click on `Screen_Output`: the code is automatically completed in the editor.
Then add a dot in your code. The completion popup is triggered automatically
and will offer you the option of completing your code with the entities
contained in the `Screen_Output` package. Select `Msg`, add a space, and then
add an open parenthesis. Once again, the completion windows pops up and shows
the possible parameters for msg. If you choose the first entry of the
completion list ("params of Msg"), the call is automatically completed by a
list of named parameters. Complete the list by giving e.g. `"The stack is
full."` for `S1`, `""` for `S2`, and `True` for `End_Line`.

Don't forget to add a semicolon at the end of the statement. Then hit :kbd:`F4`
to rebuild the application.

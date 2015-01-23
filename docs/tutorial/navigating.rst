*****************
Source Navigation
*****************

Now let's try to understand a little bit about how the program is working by
looking at the :file:`sdc.adb` editor: there's a loop, the main processing is
done by the functions Process and Next (at line 30).

Click around line 30, move the mouse over `Process` and let a tool tip appear
(`Tokens.Process global procedure declared at tokens.ads:19`): this gives
information about the kind of entity and the location (file and line) of the
declaration of this procedure, the profile of the parameters, and documentation
for this function, as extracted from the comments surrounding the procedure
declaration.

Do the same for `Next` (`Tokens.Next global function declared at
tokens.ads:15`).

Keeping the mouse over `Next`, display the contextual menu by clicking on the
right mouse button, then click on `Goto declaration of Next`: we're now in the
package `Tokens`, in file :file:`tokens.ads`; but where is this file in the
project?

A simple way to locate a file in the `Project` view is to use the contextual
menu from the source editor: `Locate in Project View: tokens.ads`.

You can also use the filter entry located at the top of the `Project` view.

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

Now let's see another way of navingating amoung your sources: move the mouse
over the `Next` identifier in :file:`tokens.ads` editor, and then hold the
:kbd:`Control` key: while you're holding the key, move the mouse over entities:
these entities now become clickable hyperlinks. Clicking on the first mouse
button will go to the declaration of the entity highlighted (or the body if you
are already on the declaration), and clicking on the middle mouse button will go
to the body directly: move the mouse back to `Next` and click.  Alternatively,
you can use the contextual menu and select `Goto body of Next`; then scroll
through the procedure `Next`, move the mouse on `Instructions.Read` at line 46,
hold :kbd:`control` again and click with the middle mouse button (or from the
contextual menu, select `Goto body of Read`).

We've now navigated quite a bit through the application source code, which you
can verify by clicking on the left arrow in the tool bar, to go back to the
previous locations visited.

Repeat the operation until you're back in :file:`sdc.adb`.  As with the
undo/redo capability in the source editor, the `goto previous/next location` is
infinite.

**********
Call Graph
**********

Now go back to the file :file:`sdc.adb`, move the mouse over the procedure
*sdc* at line 8, select the contextual menu `Browsers->Sdc calls`: this will
open a new window titled *Call graph browser*.

Note that there is also a top level contextual menu (`Sdc calls`)
which provides a tree view of the callers/callees.

In the call graph, click on the right arrow of `Process` (one of the
first items on the top). Also click on the right arrow of `Error_Msg`.

The call graph contains a tool bar; the button on the right of this
tool bar brings up the options menu.

You may want to play with the zoom (:kbd:`=` and :kbd:`-` keys).

Click on right arrow of `Process` (`(Decl) instructions.ads:12`).

The items can also be moved: move e.g `Msg` item around.

You can also recompute the layout of all the current items by using
the `Refresh layout` button (the sixth button from the left on the local
tool bar).

Click on left arrow of `Msg`
to display who is calling `Msg`. Notice that `View` calls `Msg`.

Click on left arrow of `View`: the arrow disappears, and no
new items are created, which means that `View` isn't called by anyone,
so we're now going to remove this procedure.


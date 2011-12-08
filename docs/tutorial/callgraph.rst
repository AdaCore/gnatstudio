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

Select `Orthogonal links` in the contextual menu of the graph to change the way
links are displayed in the graph.  You may then play with the zoom (:kbd:`=`
and :kbd:`-` keys).

If you select `Hide links` from `Error_Msg` contextual menu, this will hide all
the links that are related to this item: the link between the callers and
callees of `Error_Msg` are no longer displayed. This can be useful when the
graph becomes complex, to hide some parts. If you go back to the contextual
menu, you can now select `Show links` to show the links again.

Click on right arrow of `Process` (`(Decl) instructions.ads:12`).

The items can also be moved: move e.g `Msg` item around.

You can also recompute the layout of all the current items by using
the browser's contextual menu `Refresh layout` (move the mouse on the
browser's background, with no box underneath, and right click).

Click on left arrow of `Msg`
to display who is calling `Msg`. Notice that `View` calls `Msg`.

Click on left arrow of `View`: the arrow disappears, and no
new items are created, which means that `View` isn't called by anyone,
so we're now going to remove this procedure.


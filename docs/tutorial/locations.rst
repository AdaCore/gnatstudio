**************
Locations View
**************

From `Call Graph Browser`, select the contextual menu `Goto declaration of
View`, this will open the file :file:`stack.ads` at line 32.  Then from the
source editor (file :file:`stack.ads`), select the contextual menu
`References->Find all references to View`: this highlights the `Locations` tree
which now contains all the references for `View`, grouped by files
(:file:`stack.ads` and :file:`stack.adb`).

The first location is highlighted automatically: this is the spec of the
procedure `View`. Now click in the tree on the triangle at the
left of :file:`stack.adb`: two locations are listed, at line 90 and 97.  Click
on each of these locations: they correspond to the procedure body.

The `Find all references` capability is another way to list all the uses of an
entity, and it confirms that `View` isn't called in our project.

Remove *View* body by e.g selecting it, and pressing the :kbd:`Delete` key,
then save the file (:kbd:`Ctrl-S`).

Do the same for the spec, save the file.

Close the :file:`stack.ads` and :file:`stack.adb` files (menu File->Close, or
using the shortcut :kbd:`Ctrl-W`).  Rebuild by pressing the :kbd:`F4` key.

Let's now see how to create a project corresponding to the *sdc*
project we've used in this tutorial.


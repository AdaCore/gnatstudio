"""Provides sort functions in the editors

This file provides two sort functions, which can be used to sort lines
in a source file.
To use: first select the lines that you wish to sort, and then select
one of the two menus:
  - /Edit/Selection/Sort
  - /Edit/Selection/Sort Reverse
"""


############################################################################
## No user customization below this line
############################################################################

import GPS
import string
from gps_utils import *

@interactive ("Editor", filter="Source editor",
              name="sort selected lines descending",
              menu="/Edit/Selection/Sort Reverse", after="Refill")
def sort_selection_revert ():
   sort_selection (revert=True)

@interactive ("Editor", filter="Source editor",
              name="sort selected lines ascending",
              menu="/Edit/Selection/Sort", after="Refill")
def sort_selection (revert=False):
   """Sorts the current selection, in ascending order by default"""
   context = GPS.current_context ();
   ed      = GPS.EditorBuffer.get (context.file())
   start   = ed.selection_start()
   to      = ed.selection_end()

   # If the end is at the first column we really want to sort the lines
   # before the current one.

   if to.column() == 1:
	to = to.forward_char (-1)

   selection = ed.get_chars (start, to)

   if selection == "" or context.__class__ == GPS.EntityContext:
      return;

   lines = string.split (selection,"\n");
   # strip off extraneous trailing "" line
   lines = lines[:-1];
   lines.sort ();
   if revert:
      lines.reverse ();
   ed.start_undo_group()
   ed.delete (start, to)
   ed.insert (start, "\n".join (lines) + "\n")
   ed.finish_undo_group()

-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides visual utilities to handle differences between
--  files.

with Gtk.Widget; use Gtk.Widget;
with Gtk.Clist; use Gtk.Clist;
with Diff_Utils; use Diff_Utils;

package Vdiff_Utils is

   procedure Fill_Diff_Lists
     (List1 : access Gtk_Clist_Record'Class;
      List2 : access Gtk_Clist_Record'Class;
      File1 : String;
      File2 : String;
      Diff  : Diff_Occurrence_Link);
   --  Fill List1 and List2 with lines of File given a result of diff Diff.

   procedure Value1_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the value_changed signal on the first
   --  scrolled window of a Vdiff_Access.

   procedure Value2_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the value_changed signal on the second
   --  scrolled window of a Vdiff_Access.

end Vdiff_Utils;

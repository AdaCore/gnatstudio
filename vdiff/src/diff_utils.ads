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

--  This package provides low-level utilities to handle differences between
--  files.

package Diff_Utils is

   type Diff_Range is record
      First : Natural;
      Last  : Natural;
   end record;

   type Diff_Action is (Append, Change, Delete);

   type Diff_Occurrence;

   type Diff_Occurrence_Link is access Diff_Occurrence;
   --  Linked list of diff occurrences

   type Diff_Occurrence is record
      Range1 : Diff_Range;
      Range2 : Diff_Range;
      Action : Diff_Action;
      Next   : Diff_Occurrence_Link;
   end record;

   function Diff (File1, File2 : String) return Diff_Occurrence_Link;
   --  Execute diff on File1 and File2 and return a list of differences.

   function Diff
     (Orig_File : String;
      New_File  : String;
      Diff_File : String;
      Revert    : Boolean := False) return Diff_Occurrence_Link;
   --  Compute the differences from Diff_File.
   --  If Revert is False, create New_File from Orig_File and Diff_File.
   --  If Revert is True, create Orig_File from New_File and Diff_File.

   procedure Free (Link : in out Diff_Occurrence_Link);
   --  Free the memory associated with each node of the list Link.

   function Fine_Diff (Line1, Line2 : String) return Diff_Occurrence_Link;
   --  Do a fine diff between two lines.
   --  The only fields set in the resulting list is Range1 and Next, other
   --  fields should be ignored.

end Diff_Utils;

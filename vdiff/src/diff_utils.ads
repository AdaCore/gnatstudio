-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides low-level utilities to handle differences between
--  files.
with Generic_List;
with Glide_Kernel;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with VFS;

package Diff_Utils is

   type Diff_Range is record
      First : Natural;
      Last  : Natural;
      Mark  : String_Access := null;
   end record;

   type Diff_Action is (Append, Change, Delete, Nothing);

   type Diff_Occurrence;

   type Diff_Occurrence_Link is access Diff_Occurrence;
   --  Linked list of diff occurrences

   type Diff_Occurrence is record
      Range1 : Diff_Range;
      Range2 : Diff_Range;
      Action : Diff_Action;
      Next   : Diff_Occurrence_Link;
      Prev   : Diff_Occurrence_Link;
   end record;

   type Diff_Pair is record
      List21 : Diff_Occurrence_Link;
      List23 : Diff_Occurrence_Link;
   end record;
   --  structure for the diff3

   type Diff_List_Head is record
      List         : Diff_Occurrence_Link;
      List2        : Diff_Occurrence_Link := null;
      File1        : String_Access;
      File2        : String_Access;
      File3        : String_Access        := null;
      Current_Diff : Diff_Occurrence_Link := null;
   end record;

   type Diff_Head_Access is access all Diff_List_Head;

   function Diff
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File1, File2 : VFS.Virtual_File) return Diff_Occurrence_Link;
   --  Execute diff on File1 and File2 and return a list of differences.

   function Diff
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Orig_File : VFS.Virtual_File;
      New_File  : VFS.Virtual_File;
      Diff_File : VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_Occurrence_Link;
   --  Compute the differences from Diff_File.
   --  If Revert is False, create New_File from Orig_File and Diff_File.
   --  If Revert is True, create Orig_File from New_File and Diff_File.

   function Diff3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File1, File2, File3 : VFS.Virtual_File) return Diff_Pair;
   --  Execute diff on File1, File2 and File3 and return a list of differences.

   procedure Free (Link : in out Diff_Occurrence_Link);

   --  Free the memory associated with each node of the list Link.
   procedure Free (Link : in out Diff_List_Head);
   --  Free the memory of this Link. just for use generic_list

   procedure Free is
        new Ada.Unchecked_Deallocation (Diff_List_Head, Diff_Head_Access);
   --  Free the memory associated with the head of the list Link.
   package Diff_Occurrence_List is new Generic_List (Diff_List_Head, Free);

   type Diff_Occurrence_List_Access is access all Diff_Occurrence_List.List;
   procedure Free_List (List : in out Diff_Occurrence_List.List);
   --  Free all content of node of the list. overide the standard Free

   function Fine_Diff (Line1, Line2 : String) return Diff_Occurrence_Link;
   --  Do a fine diff between two lines.
   --  The only fields set in the resulting list is Range1 and Next, other
   --  fields should be ignored.

end Diff_Utils;

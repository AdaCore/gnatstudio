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
use Glide_Kernel;

package Diff_Utils2 is

   type Diff_Action is (Append, Change, Delete, Nothing);
   subtype T_Loc is Natural range 0 .. 3;
   Everywhere : constant T_Loc := 0;

   type Diff_Range is record
      First       : Natural := 0;
      Last        : Natural := 0;
      Action      : Diff_Action := Nothing;
      Mark        : String_Access := null;
      Blank_Lines : String_Access := null;
   end record;

   Null_Range : constant Diff_Range := (0, 0, Nothing, null, null);

   type   T_VRange  is array (1 .. 3) of Diff_Range;
   type   T_VStr    is array (1 .. 3) of String_Access;
   type   T_VOffset is array (1 .. 3) of Natural;

   procedure Free (V : in out T_VStr);
   --  free memory of each element of vector V

   type Diff_Chunk is record
      Range1   : Diff_Range;
      Range2   : Diff_Range;
      Range3   : Diff_Range;
      Location : T_Loc := 0;
      Conflict : Boolean := False;
   end record;
   --  structure for diff3 and diff
   type Diff_Chunk_Access is access all Diff_Chunk;

   procedure Free is
      new Ada.Unchecked_Deallocation (Diff_Chunk, Diff_Chunk_Access);
   --  Free the memory associated with the head of the list Link.

   package Diff_Chunk_List is new Generic_List (Diff_Chunk_Access, Free);
   subtype Diff_List is Diff_Chunk_List.List;
   --  Linked list of diff occurrences
   subtype Diff_List_Node is Diff_Chunk_List.List_Node;
   procedure Free_List (Link : in out Diff_List);
   --  Free the memory associated with each node of the list Link.

   type Diff_Head is tagged record
      List         : Diff_List;
      File1        : String_Access;
      File2        : String_Access;
      File3        : String_Access := null;
      Current_Node : Diff_List_Node;
      Ref_File     : T_Loc := 2;
   end record;
   type Diff_Head_Access is access all Diff_Head;

   procedure Free (Link : in out Diff_Head);
   --  Free the memory of this Link.

   procedure Free_All (Link : in out Diff_Head);
   --  Free all content of Head of the list.

   procedure Diff3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head);
   --  Execute diff on Item

   procedure Free is
      new Ada.Unchecked_Deallocation (Diff_Head, Diff_Head_Access);
   --  Free the memory associated with the head of the list Link.

   package Diff_Head_List is new Generic_List (Diff_Head, Free);
   type Diff_Head_List_Access is access all Diff_Head_List.List;

   procedure Free_List (List : in out Diff_Head_List.List);
   --  Free all content of node of the list. overide the standard Free

   function Diff
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ref_File, New_File : String) return Diff_List;
   --  Execute diff on File1 and File2 and return a list of differences.

   function Diff
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Orig_File : String;
      New_File  : String;
      Diff_File : String;
      Revert    : Boolean := False) return Diff_List;
   --  Compute the differences from Diff_File.
   --  If Revert is False, create New_File from Orig_File and Diff_File.
   --  If Revert is True, create Orig_File from New_File and Diff_File.

   function Diff3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      My_Change, Old_File, Your_Change : String)
      return Diff_List;
   --  Execute diff on File1,File2 and File3 and return a list of differences.

   function Diff
     (Ref_File, New_File : String) return Diff_List;
   --  Just for Testing

   function Diff3
     (My_Change, Old_File, Your_Change : String)
      return Diff_List;
   --  Just for Testing

   function Simplify (Diff : Diff_List;
      Ref_File : T_Loc) return Diff_List;
   --  calculate the displayable version of Diff with reference file Ref_File

private

   function Diff3 (Diff3_Command  : String;
                   My_Change, Old_File, Your_Change : String)
                   return Diff_List;
   --  Execute diff3 on File1, File2, File3 and return list of Chunk

   function Diff
     (Diff_Command  : String;
      Ref_File, New_File : String) return Diff_List;
   --  Execute diff on File1 and File2 and return a list of differences.

end Diff_Utils2;




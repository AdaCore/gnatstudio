-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with Generic_List;
with GPS.Kernel;         use GPS.Kernel;
with GNATCOLL.VFS;                use GNATCOLL.VFS;

package Diff_Utils2 is

   type Diff_Action is (Append, Change, Delete, Nothing);
   subtype T_Loc is Natural range 0 .. 3;
   subtype T_VFile_Index is Natural range 1 .. 3;
   Everywhere : constant T_Loc := 0;

   Invalid_Mark : constant Natural := Natural'Last;

   type Diff_Range is record
      First            : Natural := 0;
      Last             : Natural := 0;
      Action           : Diff_Action := Nothing;
      Blank_Lines_Mark : Natural := Invalid_Mark;
   end record;

   Null_Range : constant Diff_Range := (0, 0, Nothing, 0);

   type T_VRange  is array (1 .. 3) of Diff_Range;
   type T_VStr    is array (1 .. 3) of String_Access;
   type T_VOffset is array (1 .. 3) of Natural;
   type T_VFile   is array (T_VFile_Index) of Virtual_File;

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

   Null_Chunk : constant Diff_Chunk :=
     (Null_Range, Null_Range, Null_Range, 0, False);

   procedure Free is
      new Ada.Unchecked_Deallocation (Diff_Chunk, Diff_Chunk_Access);
   --  Free the memory associated with the head of the list Link

   package Diff_Chunk_List is new Generic_List (Diff_Chunk_Access, Free);
   subtype Diff_List is Diff_Chunk_List.List;
   --  Linked list of diff occurrences
   subtype Diff_List_Node is Diff_Chunk_List.List_Node;
   procedure Free_List (Link : in out Diff_List);
   --  Free the memory associated with each node of the list Link

   type Diff_Head is record
      List           : Diff_List;
      Files          : T_VFile;
      Current_Node   : Diff_List_Node;
      Ref_File       : T_VFile_Index := 2;
      In_Destruction : Boolean := False;
      Instances      : Instance_List_Access := null;
   end record;
   type Diff_Head_Access is access all Diff_Head;
   --  Data structure that represents a visual diff

   procedure Free (Link : in out Diff_Head);
   --  Free the memory associated with Link

   procedure Diff3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head);
   --  Execute diff on Item

   Null_Head : constant Diff_Head :=
                 (List           => Diff_Chunk_List.Null_List,
                  Files          => (others => GNATCOLL.VFS.No_File),
                  Current_Node   => Diff_Chunk_List.Null_Node,
                  Ref_File       => 2,
                  In_Destruction => False,
                  Instances      => null);

   procedure Free (Vdiff : in out Diff_Head_Access);
   --  Free the memory associated with the head of the list Link

   package Diff_Head_List is new Generic_List (Diff_Head_Access, Free);
   type Diff_Head_List_Access is access all Diff_Head_List.List;

   procedure Free (Vdiff_List : in out Diff_Head_List_Access);
   --  Free the memory associated with a list of Vdiff

   Vdiff_Class_Name : constant String := "Vdiff";

   type Vdiff_Property is new Instance_Property_Record with record
      Vdiff : Diff_Head_Access;
   end record;
   type Vdiff_Property_Access is access all Vdiff_Property'Class;
   --  Vdiff structure that is stored in a class instance

   procedure Set_Vdiff_Data
     (Instance : Class_Instance;
      Data     : Diff_Head_Access);
   --  Set the data assiociated with an instance of the Vdiff classs

   function Diff
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Ref_File, New_File : GNATCOLL.VFS.Virtual_File) return Diff_List;
   --  Execute diff on File1 and File2 and return a list of differences

   function Diff
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Orig_File : GNATCOLL.VFS.Virtual_File;
      New_File  : GNATCOLL.VFS.Virtual_File;
      Diff_File : GNATCOLL.VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_List;
   --  Compute the differences from Diff_File.
   --  If Revert is False, create New_File from Orig_File and Diff_File.
   --  If Revert is True, create Orig_File from New_File and Diff_File.

   function Diff3
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      My_Change, Old_File, Your_Change : Virtual_File) return Diff_List;
   --  Execute diff on File1,File2 and File3 and return a list of differences

   function Simplify
     (Diff : Diff_List; Ref_File : T_Loc) return Diff_List;
   --  Calculate the displayable version of Diff with reference file Ref_File

   function Horizontal_Diff
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Line1, Line2 : String) return Diff_List;
   --  Do a fine diff between two lines.
   --  The only fields set in the resulting list is Range1 and Next, other
   --  fields should be ignored.

private

   function Diff3
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Diff3_Command : String;
      My_Change, Old_File, Your_Change : GNATCOLL.VFS.Virtual_File)
      return Diff_List;
   --  Execute diff3 on File1, File2, File3 and return list of Chunk

   function Diff
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Diff_Command : String;
      Ref_File, New_File : GNATCOLL.VFS.Virtual_File) return Diff_List;
   --  Execute diff on File1 and File2 and return a list of differences

end Diff_Utils2;

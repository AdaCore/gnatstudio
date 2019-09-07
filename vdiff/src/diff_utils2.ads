------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides low-level utilities to handle differences between
--  files.

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with GPS.Kernel;                  use GPS.Kernel;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GPS.Editors;                 use GPS.Editors;
with GPS.Kernel.Preferences;
with GPS.Scripts;                 use GPS.Scripts;
with GPS_Vectors;

package Diff_Utils2 is

   type Diff_Action is (Append, Change, Delete, Nothing);
   subtype T_Loc is Natural range 0 .. 3;
   subtype T_VFile_Index is Natural range 1 .. 3;
   Everywhere : constant T_Loc := 0;

   Invalid_Mark : constant Natural := Natural'Last;

   type Diff_Range is record
      First              : Natural := 0;
      Last               : Natural := 0;
      Action             : Diff_Action := Nothing;
      Blank_Lines_Mark   : Editor_Mark_Holders.Holder;
      Special_Lines_Mark : Editor_Mark_Holders.Holder;
   end record;

   Null_Range : constant Diff_Range :=
     (First              => 0,
      Last               => 0,
      Action             => Nothing,
      Blank_Lines_Mark   => <>,
      Special_Lines_Mark => <>);

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

   package Diff_Chunk_List is new GPS_Vectors (Diff_Chunk_Access, Free);
   subtype Diff_List is Diff_Chunk_List.Vector;
   --  Linked list of diff occurrences
   subtype Diff_List_Node is Diff_Chunk_List.Std_Vectors.Cursor;
   procedure Free_List (Link : in out Diff_List);
   --  Free the memory associated with each node of the list Link

   Vdiff_Class_Name : constant String := "Vdiff";
   type Diff_Script_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Diff_Script_Proxy) return String
      is (Vdiff_Class_Name) with Inline;

   type Diff_Head is record
      List           : Diff_List;
      Files          : T_VFile;
      Current_Node   : Diff_List_Node;
      Ref_File       : T_VFile_Index := 2;
      In_Destruction : Boolean := False;
      Instances      : Diff_Script_Proxy;
      Mode           : GPS.Kernel.Preferences.Vdiff_Modes;
   end record;
   type Diff_Head_Access is access all Diff_Head;
   --  Data structure that represents a visual diff

   package Diff_Script_Proxies is new Script_Proxies
      (Diff_Head_Access, Diff_Script_Proxy);

   procedure Free (Link : in out Diff_Head);
   --  Free the memory associated with Link

   procedure Diff3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head);
   --  Execute diff on Item

   Null_Head : constant Diff_Head :=
                 (List           => Diff_Chunk_List.Empty_Vector,
                  Files          => (others => GNATCOLL.VFS.No_File),
                  Current_Node   => Diff_Chunk_List.Std_Vectors.No_Element,
                  Ref_File       => 2,
                  In_Destruction => False,
                  Instances      => <>,
                  Mode           => GPS.Kernel.Preferences.Side_By_Side);

   procedure Free (Vdiff : in out Diff_Head_Access);
   --  Free the memory associated with the head of the list Link

   package Diff_Head_List is new GPS_Vectors (Diff_Head_Access, Free);
   type Diff_Head_List_Access is access all Diff_Head_List.Vector;

   procedure Free (Vdiff_List : in out Diff_Head_List_Access);
   --  Free the memory associated with a list of Vdiff

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

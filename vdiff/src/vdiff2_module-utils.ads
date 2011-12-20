------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package provides visual utilities to handle differences between
--  files.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPS.Kernel;
with GPS.Kernel.Preferences;

with Traces;      use Traces;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

package Vdiff2_Module.Utils is

   procedure Show_Merge
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Merge  : Virtual_File);
   --  Show a result of a Merge

   function Process_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item      : Diff_Head;
      Diff_List : Diff_Head_List_Access) return Diff_Head_Access;
   --  Verify that Item is not in Diff_List then show differences and append
   --  Item to diff_List.

   procedure Show_Differences3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head);
   --  Show a result of diff Item

   procedure Hide_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head);
   --  Remove the hightlighting of all files used in a visual diff

   procedure Unhighlight_Block
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Range1 : in out Diff_Range;
      Style  : String := "");
   --  Remove the highlighting on Diff block

   procedure Visual_Diff
     (Mode  : GPS.Kernel.Preferences.Vdiff_Modes;
      File1 : Virtual_File;
      File2 : Virtual_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File);
   function Visual_Diff
     (Mode  : GPS.Kernel.Preferences.Vdiff_Modes;
      File1 : Virtual_File;
      File2 : Virtual_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File) return Diff_Head_Access;
   --  Create a new visual diff

   function Get_Vdiff
     (File1 : Virtual_File;
      File2 : Virtual_File := GNATCOLL.VFS.No_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File) return Diff_Head_Access;
   --  Return the visual diff that contains all the specified files. If no
   --  visual diff is found null is returned.

   function Get_Vdiff_List return Diff_Head_List_Access;
   --  Get the list of visual diff associated with the module

   function Visual_Patch
     (Mode      : GPS.Kernel.Preferences.Vdiff_Modes;
      Orig_File : GNATCOLL.VFS.Virtual_File;
      New_File  : GNATCOLL.VFS.Virtual_File;
      Diff_File : GNATCOLL.VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_Head_Access;
   --  Compute the differences from Diff_File.
   --  If Revert is False, create New_File from Orig_File and Diff_File.
   --  If Revert is True, create Orig_File from New_File and Diff_File.
   --  Display the result in the editor

   function Get_Diff_Node
     (Selected_File : GNATCOLL.VFS.Virtual_File;
      List          : Diff_Head_List.List)
      return Diff_Head_List.List_Node;
   --  Return the first Diff that contains Selected_File.

   function Is_In_3Diff_List
     (Selected_File : GNATCOLL.VFS.Virtual_File;
      List          : Diff_Head_List.List)
      return Boolean;
   --  Return true if Selected_File is used in a 3 files visual diff.
   --  Return False otherwise.

   type Text_Iterator;

   type Text_Iterator_Access is access all Text_Iterator;

   type Text_Iterator is record
      New_Line, Old_Line : String_Access;
      Original_Position  : Natural;
      Action             : Diff_Action := Nothing;
      Color_Enabled      : Boolean := True;
      Next               : Text_Iterator_Access;
      File_Caption       : Boolean := False;
   end record;
   --  In this structure, Original_Position is the number of the line in the
   --  old text. If Action is Append, then New_Line should be null and
   --  Original_Position must be the number of the previous line in the old
   --  text. When  Action is Delete, New_Line should be null.

   procedure Free (This : in out Text_Iterator_Access);
   --  Free the memory associated to a Text_Iterator_Access, and all next
   --  objects linked to this one.

private

   Me                   : constant Debug_Handle := Create ("VDiff2_Utils");
   Default_Style        : constant String       := "default_diff";
   Old_Style            : constant String       := "old_diff";
   Append_Style         : constant String       := "append_diff";
   Remove_Style         : constant String       := "remove_diff";
   Change_Style         : constant String       := "change_diff";
   Fine_Change_Style    : constant String       := "fine_change_diff";
   Id_Col_Vdiff         : constant String       := "vdiff2_col_merge";

end Vdiff2_Module.Utils;

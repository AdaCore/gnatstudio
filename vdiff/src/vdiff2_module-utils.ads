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

--  This package provides visual utilities to handle differences between
--  files.

with Diff_Utils2; use Diff_Utils2;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Glide_Kernel;

package Vdiff2_Module.Utils is

   procedure Show_Merge
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Merge  : String;
      Item   : Diff_Head);
   --  Show a result of a Merge

   procedure Process_Differences
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item      : Diff_Head;
      Diff_List : Diff_Head_List_Access);
   --  Verify if Item is  not in Diff_List
   --  then show differences and append Item to diff_List

   procedure Modify_Differences
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item      : Diff_Head;
      Diff_List : Diff_Head_List_Access);
   --  Verify if Item is  not in Diff_List
   --  then show differences and modify Item to diff_List

   procedure Show_Differences3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head);
   --  Show a result of diff Item

   procedure Hide_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head);
   --  Remove the hightlighting of all file of Diff

   procedure Goto_Difference
     (Kernel : Kernel_Handle;
      Link   : Diff_Chunk_Access);

   procedure Register_Highlighting
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register color preferences

   procedure Unhighlight_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Range1 : in out Diff_Range;
      Style  : String := "");
   --  Remove the highlighting on Diff block

   procedure Move_Block
     (Kernel       : Kernel_Handle;
      Source_File  : String;
      Dest_File    : String;
      Source_Range : Diff_Range;
      Dest_Range   : Diff_Range := Null_Range);
   --  Copy the text in the range Source_Range to Dest_Range in Dest_File

   function Is_In_Diff_List
     (Selected_File : String_Access;
      List          : Diff_Head_List.List)
      return Diff_Head_List.List_Node;
   --  ???

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

end Vdiff2_Module.Utils;


-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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

--  This Package provide the structure for all command for Vdiff2

with Glide_Kernel;         use Glide_Kernel;
with Commands;             use Commands;
with Diff_Utils2;          use Diff_Utils2;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Gdk.Event;

package Vdiff2_Command_Line is

   type Handler_Action_Line is access procedure
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : String := "");
      --  Is an access for the action executed by an Diff_Command

   type Diff_Command_Line is new Root_Command with record
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      Action    : Handler_Action_Line;
      File : String_Access;
      Line : Natural;
      Head : Diff_Head_List.List_Node;
   end record;

   type Diff_Command_Line_Access is access all Diff_Command_Line;

   procedure Create
     (Item      : out Diff_Command_Line_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      File      : String_Access;
      Line      : Natural;
      Action    : Handler_Action_Line);

   function Execute
     (Command : access Diff_Command_Line;
      Event   : Gdk.Event.Gdk_Event) return Command_Return_Type;

   function Execute
     (Command : access Diff_Command_Line) return Command_Return_Type;
   --  Execute the command Command
   --  Search in the global List of Diff the current diff end apply Action on
   --  this

   function Is_In_Diff_Chunk_List
     (Selected_File : String_Access;
      Item          : Diff_Head;
      Line          : Natural)
      return Diff_Chunk_List.List_Node;
   --  ???

   procedure test_button
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : String := "");

   procedure Move_On_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : String := "");

end Vdiff2_Command_Line;
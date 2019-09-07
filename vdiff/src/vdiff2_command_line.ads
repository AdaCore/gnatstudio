------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This Package provide the structure for all command for Vdiff2

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Diff_Utils2;          use Diff_Utils2;
with GPS.Kernel;           use GPS.Kernel;
with Vdiff2_Command;       use Vdiff2_Command;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

package Vdiff2_Command_Line is

   type Handler_Action_Line is access procedure
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : Virtual_File := GNATCOLL.VFS.No_File);
      --  Is an access for the action executed by an Diff_Command

   type Diff_Command_Line is new Diff_Command with record
      Action    : Handler_Action_Line;
      File      : Virtual_File;
      Line      : Natural;
      Head      : Diff_Head_List.Std_Vectors.Cursor;
   end record;

   type Diff_Command_Line_Access is access all Diff_Command_Line;

   procedure Create
     (Item      : out Diff_Command_Line_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      File      : Virtual_File;
      Line      : Natural;
      Action    : Vdiff2_Command_Line.Handler_Action_Line);

   overriding function Execute
     (Command : access Diff_Command_Line;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Execute the command Command
   --  Search in the global List of Diff the current diff end apply Action on
   --  this

   overriding function Execute
     (Command : access Diff_Command_Line) return Command_Return_Type;
   --  Execute the command Command
   --  Search in the global List of Diff the current diff end apply Action on
   --  this

   function Is_In_Diff_Chunk_List
     (Selected_File : Virtual_File;
      Item          : Diff_Head;
      Line          : Natural)
      return Diff_Chunk_List.Std_Vectors.Cursor;
   --  Search in diff list the node corresponding to Item

   procedure Move_On_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : Virtual_File := GNATCOLL.VFS.No_File);
   --  Move a diff block to the Ref_File

   procedure Delete_From_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : Virtual_File := GNATCOLL.VFS.No_File);

end Vdiff2_Command_Line;

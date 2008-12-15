-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with GNATCOLL.VFS;
with GPS.Kernel;

package Code_Peer.Shell_Commands is

   function File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : GNATCOLL.VFS.Virtual_File) return String;
   --  Executes File operation

   function Editor_Buffer_Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : String) return String;
   --  Executes EditorBuffer::get operation

   procedure Editor_Buffer_Add_Special_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Line   : Positive;
      Text   : String;
      Name   : String := "");
   --  Executes EditorBuffer::add_special_line operation

   procedure Editor_Buffer_Remove_Special_Lines
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Mark   : String;
      Lines  : Natural);
   --  Executes EditorBuffer::remove_special_lines operation

   function Editor_Buffer_Get_Mark
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Name   : String) return String;
   --  Executes Editor_Buffer::get_mark operation

   function Editor_Mark_Is_Present
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : String) return Boolean;
   --  Executes EditorMark::is_present operation

   procedure Editor_Mark_Delete
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : String);
   --  Executes EditorMark::delete operation

end Code_Peer.Shell_Commands;

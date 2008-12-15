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

with GNAT.OS_Lib;

with GNATCOLL.Utils;
with GPS.Kernel.Scripts;

package body Code_Peer.Shell_Commands is

   ------------------------------------
   -- Editor_Buffer_Add_Special_Line --
   ------------------------------------

   procedure Editor_Buffer_Add_Special_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Line   : Positive;
      Text   : String;
      Name   : String := "")
   is
      use type GNAT.OS_Lib.Argument_List;

      Args : GNAT.OS_Lib.Argument_List_Access;

   begin
      if Name = "" then
         Args :=
           new GNAT.OS_Lib.Argument_List'
                 (1 => new String'(Buffer),
                  2 => new String'(Integer'Image (Line)),
                  3 => new String'(Text));

      else
         Args :=
           new GNAT.OS_Lib.Argument_List'
                 (1 => new String'(Buffer),
                  2 => new String'(Integer'Image (Line)),
                  3 => new String'(Text),
                  4 => new String'(""),
                  5 => new String'(Name));
      end if;

      GPS.Kernel.Scripts.Execute_GPS_Shell_Command
        (Kernel, "EditorBuffer.add_special_line", Args.all);

      GNATCOLL.Utils.Free (Args.all);
      GNAT.OS_Lib.Free (Args);
   end Editor_Buffer_Add_Special_Line;

   -----------------------
   -- Editor_Buffer_Get --
   -----------------------

   function Editor_Buffer_Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : String) return String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                (1 => new String'(File),
                 2 => new String'("false"),
                 3 => new String'("false"));
      Result : constant String :=
                 GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                   (Kernel, "EditorBuffer.get", Args);

   begin
      GNATCOLL.Utils.Free (Args);

      return Result;
   end Editor_Buffer_Get;

   ----------------------------
   -- Editor_Buffer_Get_Mark --
   ----------------------------

   function Editor_Buffer_Get_Mark
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Name   : String) return String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                (1 => new String'(Buffer),
                 2 => new String'(Name));
      Result : constant String :=
                 GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                   (Kernel, "EditorBuffer.get_mark", Args);

   begin
      GNATCOLL.Utils.Free (Args);

      return Result;
   end Editor_Buffer_Get_Mark;

   ----------------------------------------
   -- Editor_Buffer_Remove_Special_Lines --
   ----------------------------------------

   procedure Editor_Buffer_Remove_Special_Lines
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : String;
      Mark   : String;
      Lines  : Natural)
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                (1 => new String'(Buffer),
                 2 => new String'(Mark),
                 3 => new String'(Integer'Image (Lines)));

   begin
      GPS.Kernel.Scripts.Execute_GPS_Shell_Command
        (Kernel, "EditorBuffer.remove_special_lines", Args);
      GNATCOLL.Utils.Free (Args);
   end Editor_Buffer_Remove_Special_Lines;

   ------------------------
   -- Editor_Mark_Delete --
   ------------------------

   procedure Editor_Mark_Delete
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : String)
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                (1 => new String'(Mark));

   begin
      GPS.Kernel.Scripts.Execute_GPS_Shell_Command
        (Kernel, "EditorMark.delete", Args);
      GNATCOLL.Utils.Free (Args);
   end Editor_Mark_Delete;

   ----------------------------
   -- Editor_Mark_Is_Present --
   ----------------------------

   function Editor_Mark_Is_Present
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : String) return Boolean
   is
      Args   : GNAT.OS_Lib.Argument_List := (1 => new String'(Mark));
      Result : constant Boolean :=
                 Boolean'Value
                   (GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                      (Kernel, "EditorMark.is_present", Args));

   begin
      GNATCOLL.Utils.Free (Args);

      return Result;
   end Editor_Mark_Is_Present;

   ----------
   -- File --
   ----------

   function File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : GNATCOLL.VFS.Virtual_File) return String
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'(Name.Full_Name.all));
      Result : constant String :=
        GPS.Kernel.Scripts.Execute_GPS_Shell_Command (Kernel, "File", Args);

   begin
      GNATCOLL.Utils.Free (Args);

      return Result;
   end File;

end Code_Peer.Shell_Commands;

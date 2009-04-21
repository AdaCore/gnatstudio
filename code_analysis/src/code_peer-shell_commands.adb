-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with GPS.Kernel.Scripts;

package body Code_Peer.Shell_Commands is

   ------------------
   -- Build_Target --
   ------------------

   function Build_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Name   : String)
      return String
   is
      Args   : GNAT.OS_Lib.Argument_List := (1 => new String'(Name));
      Result : constant String :=
                 GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                  (Kernel, "BuildTarget", Args);

   begin
      GNATCOLL.Utils.Free (Args);

      return Result;
   end Build_Target;

   --------------------------
   -- Build_Target_Execute --
   --------------------------

   procedure Build_Target_Execute
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Object      : String;
      Main_Name   : String       := "";
      File        : Virtual_File := GNATCOLL.VFS.No_File;
      Force       : Boolean      := False;
      Extra_Args  : String       := "";
      Build_Mode  : String       := "";
      Synchronous : Boolean      := True)
   is
      Args   : GNAT.OS_Lib.Argument_List :=
                 (1 => new String'(Object),
                  2 => new String'(Main_Name),
                  3 => new String'(+Full_Name (File)),
                  4 => new String'(Boolean'Image (Force)),
                  5 => new String'(Extra_Args),
                  6 => new String'(Build_Mode),
                  7 => new String'(Boolean'Image (Synchronous)));
      Result : constant String :=
                 GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                  (Kernel, "BuildTarget.execute", Args);
      pragma Unreferenced (Result);

   begin
      GNATCOLL.Utils.Free (Args);
   end Build_Target_Execute;

   --------------------
   -- Get_Build_Mode --
   --------------------

   function Get_Build_Mode (Kernel : GPS.Kernel.Kernel_Handle) return String is
      Args   : GNAT.OS_Lib.Argument_List (1 .. 0);

   begin
      return
        GPS.Kernel.Scripts.Execute_GPS_Shell_Command
          (Kernel, "get_build_mode", Args);
   end Get_Build_Mode;

   --------------------
   -- Set_Build_Mode --
   --------------------

   procedure Set_Build_Mode
     (Kernel : GPS.Kernel.Kernel_Handle;
      Mode   : String)
   is
      Args   : GNAT.OS_Lib.Argument_List := (1 => new String'(Mode));
      Result : constant String :=
                 GPS.Kernel.Scripts.Execute_GPS_Shell_Command
                   (Kernel, "set_build_mode", Args);
      pragma Unreferenced (Result);

   begin
      GNATCOLL.Utils.Free (Args);
   end Set_Build_Mode;

end Code_Peer.Shell_Commands;

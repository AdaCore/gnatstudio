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

with GNATCOLL.Command_Lines; use GNATCOLL.Command_Lines;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

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
      CL : Command_Line;

   begin
      CL := Create ("BuildTarget");
      Append_Argument (CL, Name, One_Arg);
      return GPS.Kernel.Scripts.Execute_GPS_Shell_Command (Kernel, CL);
   end Build_Target;

   --------------------------
   -- Build_Target_Execute --
   --------------------------

   procedure Build_Target_Execute
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Target_ID   : String;
      Main_Name   : String       := "";
      File        : Virtual_File := GNATCOLL.VFS.No_File;
      Force       : Boolean      := False;
      Extra_Args  : String       := "";
      Build_Mode  : String       := "";
      Synchronous : Boolean      := True;
      Dir         : Virtual_File := GNATCOLL.VFS.No_File)
   is
      CL : Command_Line := Create ("BuildTarget.execute");

   begin
      Append_Argument (CL, Target_ID, One_Arg);
      Append_Argument (CL, Main_Name, One_Arg);
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Boolean'Image (Force), One_Arg);
      Append_Argument (CL, Extra_Args, One_Arg);
      Append_Argument (CL, Build_Mode, One_Arg);
      Append_Argument (CL, Boolean'Image (Synchronous), One_Arg);
      Append_Argument (CL, +Full_Name (Dir), One_Arg);
      declare
         Result : constant String :=
           GPS.Kernel.Scripts.Execute_GPS_Shell_Command (Kernel, CL);
         pragma Unreferenced (Result);
      begin
         null;
      end;
   end Build_Target_Execute;

   --------------------
   -- Get_Build_Mode --
   --------------------

   function Get_Build_Mode (Kernel : GPS.Kernel.Kernel_Handle) return String is
   begin
      return GPS.Kernel.Scripts.Execute_GPS_Shell_Command
        (Kernel, Create ("get_build_mode"));
   end Get_Build_Mode;

   --------------------
   -- Set_Build_Mode --
   --------------------

   procedure Set_Build_Mode
     (Kernel : GPS.Kernel.Kernel_Handle;
      Mode   : String)
   is
      CL : Command_Line := Create ("set_build_mode");

   begin
      Append_Argument (CL, Mode, One_Arg);
      declare
         Result : constant String :=
           GPS.Kernel.Scripts.Execute_GPS_Shell_Command (Kernel, CL);
         pragma Unreferenced (Result);
      begin
         null;
      end;
   end Set_Build_Mode;

end Code_Peer.Shell_Commands;

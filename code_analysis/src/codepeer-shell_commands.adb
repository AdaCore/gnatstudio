------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with GPS.Kernel.Scripts;

package body CodePeer.Shell_Commands is

   ------------------
   -- Build_Target --
   ------------------

   function Build_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Name   : String)
      return String
   is
      CL : Arg_List;

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
      CL : Arg_List := Create ("BuildTarget.execute");

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

end CodePeer.Shell_Commands;

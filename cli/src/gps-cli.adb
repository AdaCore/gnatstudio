------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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
--  Driver for command line version of GPS

with Ada.Command_Line;
with Ada.Text_IO;                      use Ada.Text_IO;

with GNAT.Command_Line;                use GNAT.Command_Line;
with GNAT.Strings;                     use GNAT.Strings;

with GNATCOLL.VFS;                     use GNATCOLL.VFS;

with GPS.CLI_Kernels;
with GPS.Core_Kernels;
with GPS.Python_Core;

procedure GPS.CLI is
   procedure Parse_Command_Line (Switch, Parameter, Section : String);
   --  Handles some switches from the command line. Other switches are handled
   --  directly by Getopt and will set the corresponding local variables.

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Switch, Parameter, Section : String) is
      pragma Unreferenced (Section);
   begin
      null;
   end Parse_Command_Line;

   Cmdline               : Command_Line_Configuration;
   Project_Name          : aliased GNAT.Strings.String_Access;
   Kernel                : aliased GPS.CLI_Kernels.CLI_Kernel;
begin
   Set_Usage
     (Cmdline,
      Help => "GPS command line interface");

   Define_Switch
     (Cmdline,
      Output      => Project_Name'Access,
      Switch      => "-P:",
      Long_Switch => "--project=",
      Help        => "Load the given project (mandatory)");

   Getopt (Cmdline, Parse_Command_Line'Unrestricted_Access);

   if Project_Name.all = "" then
      Free (Project_Name);
      Project_Name := new String'(GNAT.Command_Line.Get_Argument);

      if Project_Name.all = "" then
         Put_Line ("No project file specified");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end if;

   GPS.Core_Kernels.Initialize (Kernel'Access);
   GPS.Python_Core.Register_Python (Kernel'Access);

   declare
      Path : Virtual_File := Create (+Project_Name.all);
   begin
      if not Path.Is_Regular_File then
         Path := Create (+Project_Name.all & ".gpr");

         if not Path.Is_Regular_File then
            Put_Line ("No such file: " & Project_Name.all);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
      end if;

      Kernel.Registry.Tree.Load
        (Root_Project_Path => Path);
   end;

   --  Destroy all
   GPS.Core_Kernels.Destroy (Kernel'Access);
end GPS.CLI;

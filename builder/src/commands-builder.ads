-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2008, AdaCore              --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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

--  This package handles build commands.

with GPS.Kernel;
with Glib;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Commands.Builder is

   Error_Category   : constant String := "Builder results";
   --  -"Builder results"
   Warning_Category : constant String := "Builder warnings";
   --  -"Builder warnings"
   Style_Category   : constant String := "Style errors";
   --  -"Style errors"
   Shadow_Category  : constant String := "Syntax check";
   --  -"Syntax check"

   procedure Process_Builder_Output
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : Commands.Command_Access;
      Output  : Glib.UTF8_String;
      Quiet   : Boolean);
   --  Process the builder output: update the progress bar in Command as
   --  necessary, hide the progress output, and display the other outputs in
   --  the console. Error messages are displayed in the locations window.
   --
   --  If Quiet is False, output will be displayed in the Messages window
   --  Output can contain multiple lines.

   procedure Launch_Build_Command
     (Kernel         : GPS.Kernel.Kernel_Handle;
      CL             : GNAT.OS_Lib.String_List_Access;
      Locations_Name : String);
   --  Launch a build command.
   --  CL is the command line. The first item in CL should be the executable
   --  and the rest are arguments.
   --  Locations_Name is the name used to display locations in the Locations
   --  View.

end Commands.Builder;

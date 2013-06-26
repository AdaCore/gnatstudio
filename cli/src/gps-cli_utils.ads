------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GPS.CLI_Kernels;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;

--  This package gathered utility procedures and functions related to:
--     - the initialisation of core kernels
--     - and process of basics command line arguments such as
--       -P (project file) and -X (scenarios variables)
package GPS.CLI_Utils is

   procedure Create_Kernel_Context
     (Kernel                  : access GPS.CLI_Kernels.CLI_Kernel_Record;
      Install_Semantic_Parser : Boolean := True);
   --  Build kernel context.
   --
   --  When kernel context is no longer needed, a call to
   --  Destroy_Kernel_Context procedure has to be done in order
   --  to free all allocated memory.
   --  Add support for semantic parsers if Install_Semantic_Parser is True.

   procedure Destroy_Kernel_Context
     (Kernel : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Free allocated memory of variable related to the kernel context.

   procedure Parse_Command_Line
     (Command_Line : Command_Line_Configuration;
      Kernel       : access GPS.CLI_Kernels.CLI_Kernel_Record);
   --  Handles -X switch for scenario variable.
   --  Change directly the environment of the Kernel passed as a parameter,
   --  with scenario variable retrieve from command line.

   function Is_Project_Path_Specified
     (Path : in out GNAT.Strings.String_Access) return Boolean;
   --  Check if the project file path given as a parameter is empty or not.
   --  If it is empty, then try to retrieve the next element on the command
   --  line that is not a switch.
   --
   --  Return: False if no project file has been specified,
   --          True if a non empty string has been found as the project file

   function Project_File_Path_Exists
     (Path : in out GNAT.Strings.String_Access) return Boolean;
   --  Check existance of the given Path. Add the project file extension if
   --  not mentionned in the Path string.
   --
   --  Return: Wheter the file exists or not.

end GPS.CLI_Utils;

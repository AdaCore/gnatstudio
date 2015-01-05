------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2015, AdaCore                     --
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

--  This package defines the module for GNATTest integration.

with Basic_Types;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with GPS.Kernel;

with Ada.Strings.Unbounded;

package GNATTest_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Find_Tested
     (File_Name       : GNATCOLL.VFS.Virtual_File;
      Unit_Name       : out Ada.Strings.Unbounded.Unbounded_String;
      Subprogram_Name : out Ada.Strings.Unbounded.Unbounded_String;
      Line            : out Natural;
      Column          : out Basic_Types.Visible_Column_Type);
   --  Find tested subprogram for given test unit

   procedure Open_File
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : GNATCOLL.Projects.Project_Type;
      Unit_Name       : String;
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Subprogram_Name : String := "");
   --  Open unit in editor and place cursor to given Line and Column.
   --  Project is recommended for cross-references in the case of aggregate
   --  projects.

end GNATTest_Module;

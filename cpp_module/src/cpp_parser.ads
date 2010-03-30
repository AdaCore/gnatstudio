-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2003-2010, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Entities;
with Projects;           use Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

package CPP_Parser is

   CPP_LI_Handler_Name : constant String := "c/c++";
   --  The name the source navigator is registered under.

   function Create_CPP_Handler
     (Db       : Entities.Entities_Database;
      Registry : Project_Registry)
      return Entities.LI_Handler;
   --  Create a new ALI handler

   function Set_Executables
     (System_Dir : Virtual_File;
      Handler    : access Entities.LI_Handler_Record'Class) return String;
   --  Locate the external executables required by C and C++ handling.
   --  Return an error message to display in the console (or the empty string
   --  if all executables where correctly located).
   --  System_Dir is the installation directory of GPS.

   procedure On_Project_View_Changed
     (Handler : access Entities.LI_Handler_Record'Class);
   --  Must be called whenever the contents of the project changes, including
   --  the list of files.
   --  Handler is the result of Create_CPP_Handler

end CPP_Parser;

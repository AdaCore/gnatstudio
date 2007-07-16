-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2007, AdaCore                     --
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

with GNAT.Expect;

with Docgen2_Backend;
with GPS.Kernel;
with Projects;
with VFS;

package Docgen2 is

   type Docgen_Options is record
      Process_Body_Files      : Boolean := False;
      --  Create also the body documentation
      Comments_Filter         : GNAT.Expect.Pattern_Matcher_Access;
      --  Filter comments
      Show_Private            : Boolean := False;
      --  Show also private entities
      References              : Boolean := False;
      --  True if the program should search for the references
      --  Adding information like "subprogram called by..."
      Process_Up_To_Date_Only : Boolean := True;
   end record;

   procedure Generate
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend : Docgen2_Backend.Backend_Handle;
      File    : VFS.Virtual_File;
      Options : Docgen_Options);
   --  Generate documentation for a single file using Backend.

   procedure Generate
     (Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend   : Docgen2_Backend.Backend_Handle;
      Project   : Projects.Project_Type;
      Options   : Docgen_Options;
      Recursive : Boolean := False);
   --  Generate documentation for a project using Backend.
   --  If Recusive is False (default) documentation is generated only for
   --  direct source files. Otherwise, documentation is generated for sources
   --  of imported projects as well.

end Docgen2;

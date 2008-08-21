-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

--  <summary>Main package for the docgen2 module</summary>
--  <description>
--  Docgen2 is the module handling automatic documentation generation in GPS.
--
--  The documentation analysis is preformed in 3 different steps:
--
--  * A first step analyse the involved file/project and collects various
--  informations on all entitities
--
--  * A second step dumps those information to document the APIs
--
--  * The last step preforms a pretty print pass on all source files, and
--  creates hyperlinks when an entity references a documented API.
--
--  Finaly, index files are created, and the appropriate viewer is launched.
--
--  Docgen2 is intended to support several backend for documentation generation
--  but only an HTML backend is implemented for now.
--  </description>

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Expect;

with Docgen2_Backend;
with GPS.Kernel;
with Projects;
with GNATCOLL.VFS;

package Docgen2 is

   package User_Tags_List is new Ada.Containers.Vectors
     (Natural,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

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
      --  True if docgen should process only files having up-to-date cross refs
      Keep_Formatting         : Boolean := True;
      --  If true, then comments that are not inside user tags will be kept
      --  as is in the final document (line returns are kept, as well as
      --  leading spaces).
      User_Tags               : User_Tags_List.Vector;
      --  The list of user-defined tags
   end record;

   procedure Generate
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend : Docgen2_Backend.Backend_Handle;
      File    : GNATCOLL.VFS.Virtual_File;
      Options : Docgen_Options);
   --  <summary>
   --  Generate documentation for a single file using Backend.
   --  </summary>
   --  <parameter name="Kernel">
   --    The GPS kernel object
   --  </parameter>
   --  <parameter name="Backend">
   --    The backend used for generating the doc
   --  </parameter>
   --  <parameter name="File">
   --    The file that is to be documented
   --  </parameter>
   --  <parameter name="Options">
   --    Docgen user options
   --  </parameter>

   procedure Generate
     (Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend   : Docgen2_Backend.Backend_Handle;
      Project   : Projects.Project_Type;
      Options   : Docgen_Options;
      Recursive : Boolean := False);
   --  <summary>
   --  Generate documentation for a project using Backend.
   --  </summary>
   --  <parameter name="Kernel">
   --    The GPS kernel object
   --  </parameter>
   --  <parameter name="Backend">
   --    The backend used for generating the doc
   --  </parameter>
   --  <parameter name="Project">
   --    The project that is to be documented
   --  </parameter>
   --  <parameter name="Options">
   --    Docgen user options
   --  </parameter>
   --  <parameter name="Recursive">
   --    If false, then only the project's source files are documented.
   --    Else, imported project's source files are also documented.
   --  </parameter>

end Docgen2;

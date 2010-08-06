-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2010, AdaCore                 --
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

with Ada.Containers.Vectors;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GPS.Kernel;

package Docgen2.Scripts is

   package Custom_CSS_File_Vectors is new Ada.Containers.Vectors
     (Natural, GNATCOLL.VFS.Virtual_File);

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register script commands and hooks

   function Get_Custom_CSS_Files return Custom_CSS_File_Vectors.Vector;
   --  Retrieve the list of user-registered css files

   function Get_Main_Index return Virtual_File;
   --  Retrieve the file identified by the user as index.html, or No_File if
   --  undefined.

   function Is_Custom_Tag (Tag : String) return Boolean;
   --  Tell if Tag is a user-defined custom tag

   procedure On_Documentation_Start
     (Object : Docgen_Object);
   --  Notifies the custom tag handlers that documentation generation starts

   procedure On_Documentation_Finished
     (Object : Docgen_Object);
   --  Notifies the custom tag handlers that documentation generation finishes

   function On_Custom_Tag
     (Object      : Docgen_Object;
      Tag         : String;
      Attrs       : String;
      Value       : String;
      Entity_Name : String;
      Entity_Href : String) return String;
   --  Filters a custom tag using the user-defined handler.

end Docgen2.Scripts;

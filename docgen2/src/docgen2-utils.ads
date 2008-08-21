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

with Entities;         use Entities;
with GNATCOLL.VFS;
with GPS.Kernel;
with Language;         use Language;

package Docgen2.Utils is

   procedure Warning
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Source_File;
      Loc    : Source_Location;
      Msg    : String);
   --  Prints a warning

   function Is_Spec_File
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether File is a spec file

   function Get_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language.Language_Access)
      return Entity_Information;
   --  Retrieve the entity corresponding to construct, or create a new one.

   function Get_Declaration_Entity
     (Construct : String;
      Loc       : Source_Location;
      File      : Source_File;
      Db        : Entities_Database;
      Lang      : Language.Language_Access)
      return Entity_Information;
   --  Retrieve the entity declaration corresponding to construct.

end Docgen2.Utils;

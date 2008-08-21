-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2001-2008, AdaCore               --
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

--  This package implements commands related to navigation in any
--  location viewable by the user: source locations, documentation, etc.

with Basic_Types; use Basic_Types;
with GPS.Kernel;  use GPS.Kernel;
with GNATCOLL.VFS;

package Commands.Locations is

   type Source_Location_Command_Type is new Root_Command with private;
   type Source_Location_Command is access all Source_Location_Command_Type;
   --  Commands related to navigation in source files.

   procedure Create
     (Item           : out Source_Location_Command;
      Kernel         : Kernel_Handle;
      Filename       : GNATCOLL.VFS.Virtual_File;
      Line           : Natural := 0;
      Column         : Visible_Column_Type := 0;
      Column_End     : Visible_Column_Type := 0);
   --  Create a new Source_Location_Command with the specified
   --  coordinates. Filename must be an absolute file name.

   procedure Set_Location
     (Item       : access Source_Location_Command_Type;
      New_Line   : Natural;
      New_Column : Visible_Column_Type);
   --  Set the current location in Item.

   function Get_File
     (Item : access Source_Location_Command_Type)
      return GNATCOLL.VFS.Virtual_File;
   function Get_Line
     (Item : access Source_Location_Command_Type) return Natural;
   function Get_Column
     (Item : access Source_Location_Command_Type) return Visible_Column_Type;
   --  Basic accessors.

   overriding function Execute
     (Command : access Source_Location_Command_Type)
      return Command_Return_Type;

private

   type Source_Location_Command_Type is new Root_Command with record
      Kernel         : Kernel_Handle;
      Filename       : GNATCOLL.VFS.Virtual_File;
      Line           : Natural := 0;
      Column         : Visible_Column_Type := 0;
      Column_End     : Visible_Column_Type := 0;
   end record;

end Commands.Locations;

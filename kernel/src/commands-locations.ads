-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Glide_Kernel;  use Glide_Kernel;
with VFS;

package Commands.Locations is

   type Source_Location_Command_Type is new Root_Command with private;
   type Source_Location_Command is access all Source_Location_Command_Type;
   --  Commands related to navigation in source files.

   type Html_Location_Command_Type is new Root_Command with private;
   type Html_Location_Command is access all Html_Location_Command_Type;
   --  Commands related to navigation in html files.

   type Generic_Location_Command_Type is new Root_Command with private;
   type Generic_Location_Command is access all Generic_Location_Command_Type;

   procedure Create
     (Item    : out Generic_Location_Command;
      Kernel  : Kernel_Handle;
      Args    : GNAT.OS_Lib.Argument_List);
   --  Create a new generic command corresponding to command-line Command.

   procedure Create
     (Item     : out Html_Location_Command;
      Kernel   : Kernel_Handle;
      Filename : VFS.Virtual_File);
   --  Create a new Html_Location_Command with the specified
   --  coordinates. Filename must be an absolute file name.

   procedure Create
     (Item           : out Source_Location_Command;
      Kernel         : Kernel_Handle;
      Filename       : VFS.Virtual_File;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Column_End     : Natural := 0);
   --  Create a new Source_Location_Command with the specified
   --  coordinates. Filename must be an absolute file name.

   procedure Set_Location
     (Item       : access Source_Location_Command_Type;
      New_Line   : Natural;
      New_Column : Natural);
   --  Set the current location in Item.

   function Get_File
     (Item : access Source_Location_Command_Type) return VFS.Virtual_File;
   function Get_Line
     (Item : access Source_Location_Command_Type) return Natural;
   function Get_Column
     (Item : access Source_Location_Command_Type) return Natural;
   --  Basic accessors.

   function Execute
     (Command : access Source_Location_Command_Type)
      return Command_Return_Type;

   function Execute
     (Command : access Html_Location_Command_Type) return Command_Return_Type;

   function Execute
     (Command : access Generic_Location_Command_Type)
      return Command_Return_Type;

   procedure Free (X : in out Generic_Location_Command_Type);
   --  Free memory associated to X.

private

   type Generic_Location_Command_Type is new Root_Command with record
      Kernel  : Kernel_Handle;
      Args    : String_List_Access;
   end record;

   type Source_Location_Command_Type is new Root_Command with record
      Kernel         : Kernel_Handle;
      Filename       : VFS.Virtual_File;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Column_End     : Natural := 0;
   end record;

   type Html_Location_Command_Type is new Root_Command with record
      Kernel         : Kernel_Handle;
      Filename       : VFS.Virtual_File;
   end record;

end Commands.Locations;

----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package implements commands related to navigation in any
--  location viewable by the user: source locations, documentation, etc.

with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Glide_Kernel;  use Glide_Kernel;

package Commands.Locations is

   type Source_Location_Command_Type is new Root_Command with private;
   type Source_Location_Command is access all Source_Location_Command_Type;
   --  Commands related to navigation in source files.

   procedure Create
     (Item           : out Source_Location_Command;
      Kernel         : Kernel_Handle;
      Filename       : String;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Highlight_Line : Boolean := True);
   --  Create a new Source_Location_Command with the specified
   --  coordinates. Filename must be an absolute file name.

   function Execute
     (Command : access Source_Location_Command_Type) return Boolean;

   function Undo
     (Command : access Source_Location_Command_Type) return Boolean
     renames Execute;
   --  Undoing a location command is the same as doing it, with the
   --  standard back/forward interfaces.

private

   type Source_Location_Command_Type is new Root_Command with record
      Kernel         : Kernel_Handle;
      Filename       : String_Access;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Highlight_Line : Boolean := True;
   end record;

end Commands.Locations;

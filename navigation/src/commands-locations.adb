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

with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

package body Commands.Locations is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Source_Location_Command;
      Kernel         : Kernel_Handle;
      Filename       : String;
      Line           : Natural := 0;
      Column         : Natural := 0;
      Highlight_Line : Boolean := True) is
   begin
      Item := new Source_Location_Command_Type;
      Item.Kernel := Kernel;
      Item.Filename := new String' (Filename);
      Item.Line := Line;
      Item.Column := Column;
      Item.Highlight_Line := Highlight_Line;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Source_Location_Command_Type) return Boolean
   is
   begin
      Open_File_Editor (Command.Kernel,
                        Command.Filename.all,
                        Command.Line,
                        Command.Column,
                        Command.Highlight_Line,
                        False);
      Command_Finished (Command, True);
      return True;
   end Execute;

end Commands.Locations;

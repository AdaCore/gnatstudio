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
      Other_Command    : Command_Access;
      Location_Command : Source_Location_Command;
      Open_At_Line     : Boolean := True;
   begin
      case Command.Mode is
         when Normal =>
            Open_At_Line := True;

         when Done =>
            Open_At_Line := False;

            Other_Command := Get_Next_Command (Command.Queue);

            if Other_Command /= null
              and then Other_Command.all in Source_Location_Command_Type'Class
            then
               Location_Command := Source_Location_Command (Other_Command);

               if Location_Command.Filename.all = Command.Filename.all then
                  Open_At_Line := True;
               end if;
            end if;

         when Undone =>
            Other_Command := Get_Previous_Command (Command.Queue);

            if Other_Command /= null
              and then Other_Command.all in Source_Location_Command_Type'Class
            then
               Open_At_Line := False;

               Location_Command := Source_Location_Command (Other_Command);

               if Location_Command.Filename.all = Command.Filename.all then
                  Open_At_Line := True;
               end if;
            end if;
      end case;

      if Open_At_Line then
         Open_File_Editor (Command.Kernel,
                           Command.Filename.all,
                           Command.Line,
                           Command.Column,
                           Command.Highlight_Line,
                           False);
      else
         Open_File_Editor (Command.Kernel,
                           Command.Filename.all,
                           Enable_Navigation => False);
      end if;

      Command_Finished (Command, True);

      return True;
   end Execute;

end Commands.Locations;

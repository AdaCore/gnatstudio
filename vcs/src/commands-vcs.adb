-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

package body Commands.VCS is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Commit_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List) is
   begin
      Item := new Commit_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
      Item.Logs      := Copy_String_List (Logs);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Commit_Command_Type) return Boolean is
   begin
      Commit (Command.Rep, Command.Filenames, Command.Logs);
      Command_Finished (Command.Queue, Command, True);

      return True;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List) is
   begin
      Item := new Get_Status_Command_Type;
      Item.Rep       := Rep;
      Item.Filenames := Copy_String_List (Filenames);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Get_Status_Command_Type) return Boolean is
   begin
      Get_Status (Command.Rep, Command.Filenames);
      Command_Finished (Command.Queue, Command, True);
      return True;
   end Execute;

end Commands.VCS;

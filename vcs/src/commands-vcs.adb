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

   function Create
     (Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List) return Commit_Command
   is
      Result : Commit_Command;
   begin
      Result.Rep       := Rep;
      Result.Filenames := Copy_String_List (Filenames);
      Result.Logs      := Copy_String_List (Logs);
      return Result;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Commit_Command) return Boolean is
   begin
      Commit (Command.Rep, Command.Filenames, Command.Logs);
      return True;
   end Execute;

end Commands.VCS;

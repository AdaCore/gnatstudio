-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001 - 2002                     --
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

with VCS;            use VCS;
with String_List;

package Commands.VCS is

   type Commit_Command is new Root_Command with private;

   function Create
     (Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List)
     return Commit_Command;
   --  Create a new Commit_Command.
   --  The user must free Filenames and Logs after calling Create.

   function Execute (Command : access Commit_Command) return Boolean;

private
   type Commit_Command is record
      Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List
   end record;

end Commands.VCS;

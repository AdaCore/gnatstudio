-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with VCS; use VCS;
with String_List_Utils; use String_List_Utils;

package Commands.VCS is

   type Commit_Command_Type is new Root_Command with private;
   type Commit_Command_Access is access all Commit_Command_Type;

   type Get_Status_Command_Type is new Root_Command with private;
   type Get_Status_Command_Access is access all Get_Status_Command_Type;

   procedure Create
     (Item      : out Commit_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List);
   --  Create a new Commit_Command.
   --  The user must free Filenames and Logs after calling Create.

   function Execute (Command : access Commit_Command_Type) return Boolean;

   procedure Create
     (Item      : out Get_Status_Command_Access;
      Rep       : VCS_Access;
      Filenames : String_List.List);
   --  Create a new Get_Status_Command.
   --  The user must free Filenames and Logs after calling Create.
   --  The log files for files that are up-to-date will be erased
   --  after this command completes.

   function Execute (Command : access Get_Status_Command_Type) return Boolean;

private
   type Get_Status_Command_Type is new Root_Command with record
      Rep       : VCS_Access;
      Filenames : String_List.List;
   end record;

   type Commit_Command_Type is new Root_Command with record
      Rep       : VCS_Access;
      Filenames : String_List.List;
      Logs      : String_List.List;
   end record;

end Commands.VCS;

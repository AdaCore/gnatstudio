-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

--  This package describes the use of Actions in GPS.
--
--  Actions are named commands (or list of commands) in GPS. These can
--  be associated with menus, keys and toolbar buttons among other things.

with Commands.Interactive;
with GNAT.OS_Lib;

package Glide_Kernel.Actions is

   type Action_Record is record
      Command     : Commands.Interactive.Interactive_Command_Access;
      Filter      : Action_Filter;
      Description : GNAT.OS_Lib.String_Access;
      Modified    : Boolean;
      Overriden   : Boolean;
   end record;
   --  Command is freed automatically by the kernel.
   --  Context indicates when the action can be executed. If null, this means
   --  the action can always be executed. The context mustn't deallocated
   --  in the life of GPS, since there might be actions bound to it at any
   --  time.

   type Action_Record_Access is access Action_Record;

   procedure Register_Action
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Filter      : Action_Filter := null);
   --  Register a new named action in GPS.
   --  Only the actions that can be executed interactively by the user
   --  should be registered.
   --  Name must be unique in GPS.
   --  Action will be freed automatically by the kernel.

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Action_Record_Access;
   --  Lookup a command by name. Return null if no such action has been
   --  registered.
   --  If Name represents the absolute path to a menu (starting with /), then
   --  an action is created dynamically as appropriate.

   type Action_Iterator is private;

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator;
   --  Return the first action registered in the kernel (this is in no
   --  particular order).

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator);
   --  Move to the next action

   function Get (Iter : Action_Iterator) return String;
   function Get (Iter : Action_Iterator) return Action_Record_Access;
   --  Return the current action. The empty string or No_Action is returned if
   --  there are no more actions.

private

   procedure Free (Action : in out Action_Record_Access);
   --  Free the memory occupied by the action

   package Actions_Htable is new String_Hash
     (Action_Record_Access, Free, null);

   type Actions_Htable_Record is new Root_Table with record
      Table : Actions_Htable.String_Hash_Table.HTable;
   end record;
   type Actions_Htable_Access is access all Actions_Htable_Record'Class;

   procedure Reset (X : access Actions_Htable_Record);
   --  Reset the table.

   type Action_Iterator is record
      Iterator : Actions_Htable.String_Hash_Table.Iterator;
   end record;

end Glide_Kernel.Actions;

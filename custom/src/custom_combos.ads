-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package is used to define custom combo entries in the GPS toolbar

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Actions; use Glide_Kernel.Actions;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;

package Custom_Combos is

   procedure Register_Combo
     (Kernel : access Kernel_Handle_Record'Class;
      Title  : String;
      Id     : String);
   --  Register a button bar combo entry.
   --  Title serves both as label and identifier.

   procedure Set_Combo_Changed_Action
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Command : Action_Record_Access);
   --  Set an action to be executed when the value in the
   --  combo identified by Id changes.

   procedure Add_Combo_Entry
     (Kernel  : access Kernel_Handle_Record'Class;
      Id      : String;
      Label   : String;
      Command : Action_Record_Access);
   --  Add a combo entry and a command that should be
   --  executed whenever this entry is selected.

   procedure Custom_Entry_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands dealing with toolbar combos.

end Custom_Combos;

-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
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

with Ada.Exceptions; use Ada.Exceptions;

with Glib;           use Glib;
with Glib.Values;    use Glib.Values;

with Gtk.Widget;     use Gtk.Widget;

with Traces;         use Traces;

package body Commands.Controls is

   type Queue_Change_Command is new Root_Command with record
      The_Queue : Command_Queue;
      UR        : Undo_Redo;
   end record;
   type Queue_Change_Access is access all Queue_Change_Command;

   function Execute
     (Command : access Queue_Change_Command) return Command_Return_Type;

   package Command_Callback is new User_Callback
     (Gtk_Widget_Record, Queue_Change_Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Free (X : in out Queue_Change_Command);
   --  Free memory associated to X.

   procedure On_Undo
     (Widget  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Command : Queue_Change_Access);
   --  Callback for the undo widgets.

   procedure On_Redo
     (Widget  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Command : Queue_Change_Access);
   --  Callback for the redo widgets.

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Queue_Change_Command) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   -------------
   -- On_Undo --
   -------------

   procedure On_Undo
     (Widget  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Command : Queue_Change_Access)
   is
      pragma Unreferenced (Widget);
      pragma Unreferenced (Params);
   begin
      Undo (Command.The_Queue);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Undo;

   -------------
   -- On_Redo --
   -------------

   procedure On_Redo
     (Widget  : access Gtk_Widget_Record'Class;
      Params  : GValues;
      Command : Queue_Change_Access)
   is
      pragma Unreferenced (Widget);
      pragma Unreferenced (Params);
   begin
      Redo (Command.The_Queue);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Redo;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Queue_Change_Command) return Command_Return_Type is
   begin
      if Command.UR.Undo_Button /= null then
         Set_Sensitive (Command.UR.Undo_Button,
                        not Undo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.UR.Redo_Button /= null then
         Set_Sensitive
           (Command.UR.Redo_Button, not Redo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.UR.Undo_Menu_Item /= null then
         Set_Sensitive
           (Command.UR.Undo_Menu_Item,
            not Undo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.UR.Redo_Menu_Item /= null then
         Set_Sensitive
           (Command.UR.Redo_Menu_Item,
            not Redo_Queue_Empty (Command.The_Queue));
      end if;

      return Success;
   end Execute;

   ------------------
   -- Set_Controls --
   ------------------

   procedure Set_Controls
     (Queue : Command_Queue;
      UR    : Undo_Redo)
   is
      Command : Queue_Change_Access;
   begin
      if Queue = null then
         Set_Sensitive (UR.Undo_Button, False);
         Set_Sensitive (UR.Redo_Button, False);
         Set_Sensitive (UR.Undo_Menu_Item, False);
         Set_Sensitive (UR.Redo_Menu_Item, False);
      else
         Command := new Queue_Change_Command;
         Command.The_Queue := Queue;
         Command.UR := UR;

         Command.UR.Undo_Button_Handler_ID := Command_Callback.Connect
           (UR.Undo_Button, Signal_Clicked, On_Undo'Access, Command, True);
         Command.UR.Redo_Button_Handler_ID := Command_Callback.Connect
           (UR.Redo_Button, Signal_Clicked, On_Redo'Access, Command, True);

         Command.UR.Undo_Menu_Item_Handler_ID := Command_Callback.Connect
           (UR.Undo_Menu_Item, Signal_Activate, On_Undo'Access, Command, True);
         Command.UR.Redo_Menu_Item_Handler_ID := Command_Callback.Connect
           (UR.Redo_Menu_Item, Signal_Activate, On_Redo'Access, Command, True);

         Execute (Command);
         Add_Queue_Change_Hook (Queue, Command_Access (Command), "Controls");
      end if;
   end Set_Controls;

   --------------------
   -- Unset_Controls --
   --------------------

   procedure Unset_Controls (Queue : Command_Queue) is
      The_Command : Command_Access := null;
      Queue_Node  : Command_Queues.List_Node;
      Command     : Queue_Change_Access;

      use type Command_Queues.List_Node;
   begin
      Queue_Node := Command_Queues.First (Get_Queue_Change_Hook (Queue));

      while Queue_Node /= Command_Queues.Null_Node loop
         The_Command := Command_Queues.Data (Queue_Node);

         if The_Command /= null
           and then The_Command.all in Queue_Change_Command'Class
         then
            exit;
         else
            The_Command := null;
         end if;

         Queue_Node := Command_Queues.Next (Queue_Node);
      end loop;

      if The_Command = null then
         return;
      end if;

      Command := Queue_Change_Access (The_Command);

      if Command.UR.Undo_Button_Handler_ID.Id /= Null_Handler_Id then
         Disconnect
           (Command.UR.Undo_Button, Command.UR.Undo_Button_Handler_ID);
         Disconnect
           (Command.UR.Redo_Button, Command.UR.Redo_Button_Handler_ID);
         Disconnect
           (Command.UR.Undo_Menu_Item, Command.UR.Undo_Menu_Item_Handler_ID);
         Disconnect
           (Command.UR.Redo_Menu_Item, Command.UR.Redo_Menu_Item_Handler_ID);
         Command.UR.Undo_Button_Handler_ID.Id := Null_Handler_Id;
      end if;

      if Command.UR.Undo_Button /= null then
         Set_Sensitive (Command.UR.Undo_Button, False);
      end if;

      if Command.UR.Redo_Button /= null then
         Set_Sensitive (Command.UR.Redo_Button, False);
      end if;

      if Command.UR.Undo_Menu_Item /= null then
         Set_Sensitive (Command.UR.Undo_Menu_Item, False);
      end if;

      if Command.UR.Redo_Menu_Item /= null then
         Set_Sensitive (Command.UR.Redo_Menu_Item, False);
      end if;
   end Unset_Controls;

end Commands.Controls;

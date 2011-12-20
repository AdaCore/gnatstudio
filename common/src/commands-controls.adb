------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

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

   overriding function Execute
     (Command : access Queue_Change_Command) return Command_Return_Type;

   package Command_Callback is new User_Callback
     (Gtk_Widget_Record, Queue_Change_Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   overriding procedure Free (X : in out Queue_Change_Command);
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

   overriding procedure Free (X : in out Queue_Change_Command) is
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

   overriding function Execute
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

   function Set_Controls
     (Queue : Command_Queue;
      UR    : Undo_Redo) return Command_Access
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

      return Command_Access (Command);
   end Set_Controls;

   --------------------
   -- Unset_Controls --
   --------------------

   procedure Unset_Controls
     (Command : Command_Access)
   is
      use Command_Lists;
      C : Queue_Change_Access;
   begin
      if Command = null
        or else Command.all not in Queue_Change_Command'Class
      then
         return;
      end if;

      C := Queue_Change_Access (Command);

      if C.UR.Undo_Button_Handler_ID.Id /= Null_Handler_Id then
         Disconnect (C.UR.Undo_Button, C.UR.Undo_Button_Handler_ID);
         Disconnect (C.UR.Redo_Button, C.UR.Redo_Button_Handler_ID);
         Disconnect (C.UR.Undo_Menu_Item, C.UR.Undo_Menu_Item_Handler_ID);
         Disconnect (C.UR.Redo_Menu_Item, C.UR.Redo_Menu_Item_Handler_ID);
         C.UR.Undo_Button_Handler_ID.Id := Null_Handler_Id;
      end if;

      if C.UR.Undo_Button /= null then
         Set_Sensitive (C.UR.Undo_Button, False);
      end if;

      if C.UR.Redo_Button /= null then
         Set_Sensitive (C.UR.Redo_Button, False);
      end if;

      if C.UR.Undo_Menu_Item /= null then
         Set_Sensitive (C.UR.Undo_Menu_Item, False);
      end if;

      if C.UR.Redo_Menu_Item /= null then
         Set_Sensitive (C.UR.Redo_Menu_Item, False);
      end if;
   end Unset_Controls;

end Commands.Controls;

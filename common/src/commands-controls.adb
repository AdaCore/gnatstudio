-----------------------------------------------------------------------
--                              G P S                                --
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

with Glib.Values;     use Glib.Values;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Handlers;    use Gtk.Handlers;
with Traces;          use Traces;
with Ada.Exceptions;  use Ada.Exceptions;

package body Commands.Controls is

   Me : constant Debug_Handle := Create ("Commands.Controls");

   type Queue_Change_Command is new Root_Command with record
      The_Queue                 : Command_Queue;
      Undo_Button               : Gtk_Button;
      Redo_Button               : Gtk_Button;
      Undo_Menu_Item            : Gtk_Menu_Item;
      Redo_Menu_Item            : Gtk_Menu_Item;
      Undo_Button_Handler_ID    : Handler_Id;
      Redo_Button_Handler_ID    : Handler_Id;
      Undo_Menu_Item_Handler_ID : Handler_Id;
      Redo_Menu_Item_Handler_ID : Handler_Id;
   end record;
   type Queue_Change_Access is access all Queue_Change_Command;

   function Execute (Command : access Queue_Change_Command) return Boolean;

   package Command_Callback is new User_Callback
     (Gtk_Widget_Record, Queue_Change_Access);

   -----------------------
   -- Local subprograms --
   -----------------------

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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Redo;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Queue_Change_Command) return Boolean is
   begin
      if Command.Undo_Button /= null then
         Set_Sensitive (Command.Undo_Button,
                        not Undo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.Redo_Button /= null then
         Set_Sensitive
           (Command.Redo_Button, not Redo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.Undo_Menu_Item /= null then
         Set_Sensitive
           (Command.Undo_Menu_Item, not Undo_Queue_Empty (Command.The_Queue));
      end if;

      if Command.Redo_Menu_Item /= null then
         Set_Sensitive
           (Command.Redo_Menu_Item, not Redo_Queue_Empty (Command.The_Queue));
      end if;

      return True;
   end Execute;

   ------------------
   -- Set_Controls --
   ------------------

   procedure Set_Controls
     (Queue       : Command_Queue;
      Undo_Button : Gtk_Button;
      Redo_Button : Gtk_Button;
      Undo_Menu_Item : Gtk_Menu_Item;
      Redo_Menu_Item : Gtk_Menu_Item)
   is
      Command : Queue_Change_Access;
   begin
      pragma Assert (Queue /= null);

      Command := new Queue_Change_Command;
      Command.The_Queue := Queue;
      Command.Undo_Button := Undo_Button;
      Command.Redo_Button := Redo_Button;
      Command.Undo_Menu_Item := Undo_Menu_Item;
      Command.Redo_Menu_Item := Redo_Menu_Item;

      Command.Undo_Button_Handler_ID := Command_Callback.Connect
        (Undo_Button, "clicked", On_Undo'Access, Command, True);
      Command.Redo_Button_Handler_ID := Command_Callback.Connect
        (Redo_Button, "clicked", On_Redo'Access, Command, True);

      Command.Undo_Menu_Item_Handler_ID := Command_Callback.Connect
        (Undo_Menu_Item, "activate", On_Undo'Access, Command, True);
      Command.Redo_Menu_Item_Handler_ID := Command_Callback.Connect
        (Redo_Menu_Item, "activate", On_Redo'Access, Command, True);

      Execute (Command);
      Add_Queue_Change_Hook (Queue, Command_Access (Command));
   end Set_Controls;

   --------------------
   -- Unset_Controls --
   --------------------

   procedure Unset_Controls (Queue : Command_Queue) is
      The_Command : Command_Access;
      Command     : Queue_Change_Access;

   begin
      The_Command := Get_Queue_Change_Hook (Queue);

      if The_Command = null then
         return;
      end if;

      if The_Command.all in Queue_Change_Command'Class then
         Command := Queue_Change_Access (The_Command);
         Disconnect
           (Command.Undo_Button, Command.Undo_Button_Handler_ID);
         Disconnect
           (Command.Redo_Button, Command.Redo_Button_Handler_ID);
         Disconnect
           (Command.Undo_Menu_Item, Command.Undo_Menu_Item_Handler_ID);
         Disconnect
           (Command.Redo_Menu_Item, Command.Redo_Menu_Item_Handler_ID);

         if Command.Undo_Button /= null then
            Set_Sensitive (Command.Undo_Button, False);
         end if;

         if Command.Redo_Button /= null then
            Set_Sensitive (Command.Redo_Button, False);
         end if;

         if Command.Undo_Menu_Item /= null then
            Set_Sensitive (Command.Undo_Menu_Item, False);
         end if;

         if Command.Redo_Menu_Item /= null then
            Set_Sensitive (Command.Redo_Menu_Item, False);
         end if;

      else
         raise Program_Error;
      end if;
   end Unset_Controls;

end Commands.Controls;

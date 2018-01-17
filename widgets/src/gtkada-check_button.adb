------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with System;
with Glib.Object;      use Glib.Object;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;

package body Gtkada.Check_Button is

   procedure Redraw_State (Check : access Gtkada_Check_Button_Record'Class);
   --  Redraw the state of Check

   procedure On_Button_Clicked (Obj : System.Address);
   pragma Convention (C, On_Button_Clicked);
   --  Callback for the clicked event

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Check   : out Gtkada_Check_Button;
      Label   : UTF8_String := "";
      Default : Boolean := False) is
   begin
      Check := new Gtkada_Check_Button_Record;
      Initialize (Check, Label, Default);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   procedure Initialize
     (Check : access Gtkada_Check_Button_Record'Class;
      Label : UTF8_String := "";
      Default : Boolean := False)
   is
      procedure Install_Clicked_Handler
        (Obj     : GType;
         Handler : System.Address);
      pragma Import (C, Install_Clicked_Handler,
                     "gtkada_check_button_install_handler");

   begin
      if Initialize_Class_Record
        (Ancestor     => Gtk.Check_Button.Get_Type,
         Signals      => (1 .. 0 => <>),
         Class_Record => Class_Record'Access,
         Type_Name    => "GtkadaCheckButton")
      then
         --  We replace the class handler for 'clicked' because this signal
         --  has the flag G_SIGNAL_RUN_FIRST which makes the class handler
         --  always being called first even when connecting a new signal
         --  handler with 'Last' set to false.
         --  We absolutely need to be called before the class handler.
         Install_Clicked_Handler
            (Class_Record.The_Type, On_Button_Clicked'Address);
      end if;

      Glib.Object.G_New (Check, Class_Record.The_Type);
      Check.Default := False;
      Check.State := State_Unchecked;
      Check.Internal := False;
      Check.Forcing_Update := False;

      Check.Set_Label (Label);
      Set_Default (Check, Default);
      Redraw_State (Check);
   end Initialize;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Check : access Gtkada_Check_Button_Record;
      State : Boolean)
   is
      Send_Signal : Boolean := False;

   begin
      if State = Check.Default then
         return;
      end if;

      Check.Default := State;

      case Check.State is
         when State_Unchecked =>
            --  Old default was 'unset', new is 'set' so change the state to
            --  checked default
            if Check.Default then
               Check.State := State_Checked_Default;
            end if;

            --  Force the toggled signal to be sent anyway as the user might
            --  want to be aware that the button is now in its default state.
            Send_Signal := True;

         when State_Checked_Default =>
            Check.State := State_Unchecked;
            Send_Signal := True;

         when State_Checked =>
            --  Special case: the checked state indicates that the button is
            --  forced (not Checked_Default). So we don't send signals in this
            --  case.
            null;

      end case;

      if Send_Signal then
         --  Set Internal so that On_Clicked won't change the state.
         Check.Internal := True;
         Clicked (Check);
         Check.Internal := False;
      end if;
   end Set_Default;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default
     (Check : access Gtkada_Check_Button_Record) return Boolean is
   begin
      return Check.Default;
   end Get_Default;

   ----------------
   -- Set_Active --
   ----------------

   overriding procedure Set_Active
     (Check      : access Gtkada_Check_Button_Record;
      State      : Boolean)
   is
   begin
      if (State and then Check.State = State_Checked)
        or else (not State and then Check.State = State_Unchecked)
      then
         return;
      end if;

      if State then
         Check.State := State_Checked;
      else
         Check.State := State_Unchecked;
      end if;

      --  Set Internal so that On_Clicked won't change the state.
      Check.Internal := True;
      Clicked (Check);
      Check.Internal := False;

      Redraw_State (Check);
   end Set_Active;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Check : access Gtkada_Check_Button_Record) return State_Type is
   begin
      return Check.State;
   end Get_State;

   -----------------------
   -- On_Button_Clicked --
   -----------------------

   procedure On_Button_Clicked (Obj : System.Address)
   is
      procedure Force_State (Check : System.Address; Val : Integer);
      pragma Import (C, Force_State, "gtkada_check_button_force_state");
      procedure Original_Handler (Check : System.Address);
      pragma Import (C, Original_Handler, "gtkada_check_button_clicked");

      Stub  : Gtkada_Check_Button_Record;
      pragma Warnings (Off, Stub);
      Check : constant Gtkada_Check_Button :=
                Gtkada_Check_Button (Get_User_Data (Obj, Stub));
      Underlying_State : Boolean;

   begin
      --  Prevent recursion loop
      if Check.Forcing_Update then
         Original_Handler (Get_Object (Check));
         return;
      end if;

      --  Change state only when not an internal call, where the state is
      --  already in the desired state
      if not Check.Internal then
         case Check.State is
         when State_Checked_Default =>
            Check.State := State_Unchecked;

         when State_Checked =>
            if Check.Default then
               Check.State := State_Checked_Default;
            else
               Check.State := State_Unchecked;
            end if;

         when State_Unchecked =>
            Check.State := State_Checked;

         end case;
      end if;

      Underlying_State := Check.State = State_Checked;
      --  The desired underlying GtkCheckButton state. We keep
      --  State_Checked_Default as unchecked to display an inconsistent icon.

      Check.Forcing_Update := True;

      --  We want the underlying check_button state be different from the
      --  current state, as the original handler will toggle it.
      if Underlying_State = Get_Active (Check) then
         if Underlying_State then
            --  We force it deactivated
            Force_State (Get_Object (Check), 0);
         else
            Force_State (Get_Object (Check), 1);
         end if;
      end if;

      Check.Forcing_Update := False;

      Original_Handler (Get_Object (Check));

      Redraw_State (Check);
   end On_Button_Clicked;

   ------------------
   -- Redraw_State --
   ------------------

   procedure Redraw_State (Check : access Gtkada_Check_Button_Record'Class) is
   begin
      case Check.State is
         when State_Checked | State_Unchecked =>
            Check.Set_Inconsistent (False);
         when State_Checked_Default =>
            Check.Set_Inconsistent (True);
      end case;
   end Redraw_State;

end Gtkada.Check_Button;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Glib.Main; use Glib.Main;

with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Gtk.Handlers; use Gtk.Handlers;

with GNATCOLL.Traces; use GNATCOLL.Traces;

with GPS.Kernel.Preferences;

package body GPS.Kernel.Hyper_Mode is

   Watch_Timeout : constant := 100;
   --  The interval at which to watch for hyper mode, in milliseconds

   Me : constant Trace_Handle := Create
     ("GPS.KERNEL.HYPER_MODE", GNATCOLL.Traces.Off);

   type Hyper_Mode_Data_Record is record
      Kernel                   : Kernel_Handle;
      --  The kernel

      Widget                   : Gtk_Widget;
      --  The widget for which we enable Hyper Mode

      Hyper_Mode               : Boolean := False;
      --  Whether Hyper Mode has actually been enabled for this widget

      Hyper_Mode_Motion_Watch  : Boolean := False;
      Hyper_Mode_Watch_Timeout : Glib.Main.G_Source_Id := 0;
      On_Enter                 : Simple_Callback;
      On_Leave                 : Simple_Callback;
   end record;

   type Hyper_Mode_Data is access Hyper_Mode_Data_Record;

   package Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Hyper_Mode_Data);
   use Return_Callback;

   package Widget_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Hyper_Mode_Data);

   package Hyper_Mode_Timeout is new Glib.Main.Generic_Sources
     (Hyper_Mode_Data);

   ---------------
   -- Callbacks --
   ---------------

   function Enter_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean;

   function Leave_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Hyper_Mode_Data) return Boolean;

   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Hyper_Mode_Data);

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean;

   function Key_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean;

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean;

   function Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean;

   function Watch_Timeout_Cb (Data : Hyper_Mode_Data) return Boolean;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Hyper_Mode_Enter (Data : Hyper_Mode_Data);
   --  Enter hyper mode

   procedure Hyper_Mode_Leave (Data : Hyper_Mode_Data);
   --  Leave hyper mode

   ----------------------
   -- Hyper_Mode_Enter --
   ----------------------

   procedure Hyper_Mode_Enter (Data : Hyper_Mode_Data) is
   begin
      if Data.Hyper_Mode then
         return;
      end if;

      Data.Hyper_Mode := True;
      Data.On_Enter (Data.Widget);
   end Hyper_Mode_Enter;

   ----------------------
   -- Hyper_Mode_Leave --
   ----------------------

   procedure Hyper_Mode_Leave (Data : Hyper_Mode_Data) is
   begin
      if not Data.Hyper_Mode then
         return;
      end if;

      Data.Hyper_Mode := False;
      Data.On_Leave (Data.Widget);
   end Hyper_Mode_Leave;

   ----------------------
   -- Watch_Timeout_Cb --
   ----------------------

   function Watch_Timeout_Cb (Data : Hyper_Mode_Data) return Boolean is
   begin
      if Data.Kernel.In_Hyper_Mode then
         if not Data.Hyper_Mode then
            Hyper_Mode_Enter (Data);
         end if;
      else
         if Data.Hyper_Mode then
            Hyper_Mode_Leave (Data);
         end if;
      end if;
      return True;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Watch_Timeout_Cb;

   ---------------------------
   -- Enter_Notify_Event_Cb --
   ---------------------------

   function Enter_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Trace (Me, "enter_notify");

      if Data.Kernel.In_Hyper_Mode then
         --  Safety check: it might happen that we leave GPS without receiving
         --  a Leave_Notify event. In this case, we could enter the window with
         --  the Ctrl key released but GPS still in hyper mode.

         if (Get_State (Event) and Control_Mask) = 0 then
            Hyper_Mode_Leave (Data);
            return False;
         end if;

         Hyper_Mode_Enter (Data);
      end if;

      if not Data.Widget.Has_Focus
        and then not Data.Hyper_Mode_Motion_Watch
      then
         --  We don't have the focus on this widget: therefore we must activate
         --  the motion_notify event to detect when the global hyper mode is
         --  being entered.
         Data.Hyper_Mode_Motion_Watch := True;
         Data.Hyper_Mode_Watch_Timeout := Hyper_Mode_Timeout.Timeout_Add
           (Watch_Timeout, Watch_Timeout_Cb'Access, Data);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Enter_Notify_Event_Cb;

   ---------------------------
   -- Leave_Notify_Event_Cb --
   ---------------------------

   function Leave_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Hyper_Mode_Data) return Boolean is
      pragma Unreferenced (Widget);
   begin
      Trace (Me, "leave_notify");

      if Data.Hyper_Mode_Motion_Watch then
         Glib.Main.Remove (Data.Hyper_Mode_Watch_Timeout);
         Data.Hyper_Mode_Motion_Watch := False;
      end if;

      if Data.Hyper_Mode then
         Hyper_Mode_Leave (Data);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Leave_Notify_Event_Cb;

   -----------------------
   -- Enable_Hyper_Mode --
   -----------------------

   procedure Enable_Hyper_Mode
     (Kernel              : Kernel_Handle;
      Widget              : access Gtk_Widget_Record'Class;
      On_Hyper_Mode_Enter : Simple_Callback;
      On_Hyper_Mode_Leave : Simple_Callback)
   is
      Data : Hyper_Mode_Data;
   begin
      if not GPS.Kernel.Preferences.Hyper_Mode.Get_Pref then
         return;
      end if;

      Data := new Hyper_Mode_Data_Record;

      Data.Kernel := Kernel;
      Data.Widget := Gtk_Widget (Widget);
      Data.On_Enter := On_Hyper_Mode_Enter;
      Data.On_Leave := On_Hyper_Mode_Leave;

      --  Connect to pointer motion

      Set_Events (Widget, Get_Events (Widget) or Enter_Notify_Mask or
                    Leave_Notify_Mask or Pointer_Motion_Hint_Mask);

      Connect
        (Widget, Signal_Enter_Notify_Event,
         Marsh     => To_Marshaller (Enter_Notify_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Focus_In_Event,
         Marsh     => To_Marshaller (Enter_Notify_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Leave_Notify_Event,
         Marsh     => To_Marshaller (Leave_Notify_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Focus_Out_Event,
         Marsh     => To_Marshaller (Leave_Notify_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      --  Connect to key press/releases

      Connect
        (Widget, Signal_Key_Press_Event,
         Marsh     => To_Marshaller (Key_Press_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Key_Release_Event,
         Marsh     => To_Marshaller (Key_Release_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Button_Press_Event,
         Marsh     => To_Marshaller (Button_Press_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      Connect
        (Widget, Signal_Button_Release_Event,
         Marsh     => To_Marshaller (Button_Release_Event_Cb'Access),
         User_Data => Data,
         After     => False);

      --  Lifecycle management

      Widget_Callback.Connect
        (Widget, Signal_Destroy, On_Destroy'Access,
         After => False,
         User_Data => Data);
   end Enable_Hyper_Mode;

   ------------------------
   -- Key_Press_Event_Cb --
   ------------------------

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      case Get_Key_Val (Event) is
         when GDK_Control_L | GDK_Control_R =>
            Hyper_Mode_Enter (Data);
         when others =>
            null;
      end case;

      return False;
   end Key_Press_Event_Cb;

   --------------------------
   -- Key_Release_Event_Cb --
   --------------------------

   function Key_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      case Get_Key_Val (Event) is
         when GDK_Control_L | GDK_Control_R =>
            Hyper_Mode_Leave (Data);
         when others =>
            null;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Key_Release_Event_Cb;

   ---------------------------
   -- Button_Press_Event_Cb --
   ---------------------------

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      if Get_Button (Event) = 3 then
         --  We are potentially bringing up a contextual menu: deactivate
         --  hyper mode.
         Hyper_Mode_Leave (Data);
      else
         --  If the kernel is in hyper mode, enter it on a click
         if Data.Kernel.In_Hyper_Mode
           and then not Data.Hyper_Mode
         then
            --  Check for the pressed state of the control key. If it is not
            --  pressed, leave the hyper mode.
            if (Get_State (Event) and Control_Mask) = 0 then
               Hyper_Mode_Leave (Data);
               return False;
            end if;

            Hyper_Mode_Enter (Data);
         end if;
      end if;
      return False;
   end Button_Press_Event_Cb;

   -----------------------------
   -- Button_Release_Event_Cb --
   -----------------------------

   function Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event;
      Data   : Hyper_Mode_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      if Get_Button (Event) = 3 then
         --  Leaving from contextual menu: reactivate hyper mode if it is still
         --  relevant

         if (Get_State (Event) and Control_Mask) /= 0 then
            Hyper_Mode_Enter (Data);
         end if;
      end if;

      return False;
   end Button_Release_Event_Cb;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Hyper_Mode_Data)
   is
      pragma Unreferenced (Widget);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hyper_Mode_Data_Record, Hyper_Mode_Data);
      X : Hyper_Mode_Data := Data;
   begin
      Hyper_Mode_Leave (Data);

      if Data.Hyper_Mode_Motion_Watch then
         Glib.Main.Remove (Data.Hyper_Mode_Watch_Timeout);
      end if;

      Unchecked_Free (X);
   exception
      when E : others =>
         Trace (Me, E);
   end On_Destroy;

end GPS.Kernel.Hyper_Mode;

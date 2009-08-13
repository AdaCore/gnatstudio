-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009, AdaCore                        --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Cursor;        use Gdk.Cursor;
with Gdk.Event;         use Gdk.Event;
with Gdk.Window;        use Gdk.Window;

with Gtk.Handlers;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;

with Gtkada.Handlers; use Gtkada.Handlers;

with Traces; use Traces;
with GNATCOLL.Traces;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Src_Editor_Buffer.Hyper_Mode; use Src_Editor_Buffer.Hyper_Mode;
with Gtk.Text_Iter; use Gtk.Text_Iter;

with GPS.Kernel.Hyper_Mode; use GPS.Kernel.Hyper_Mode;

package body Src_Editor_View.Hyper_Mode is

   use type Gtk.Main.Timeout_Handler_Id;
   use type Gtk.Handlers.Handler_Id;

   Me : constant Debug_Handle := Create ("hyper_mode", GNATCOLL.Traces.Off);

   ---------------
   -- Callbacks --
   ---------------

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   function Motion_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Hyper_Mode_Enter (Widget : access Gtk_Widget_Record'Class);
   --  Put View in hyper mode

   procedure Hyper_Mode_Leave (Widget : access Gtk_Widget_Record'Class);
   --  Leave hyper mode

   function Name (View : Source_View) return String;
   --  Return a name for View, for debugging purposes

   procedure Highlight_On (View : Source_View; X, Y : Gint);
   --  Highlight text at position X, Y

   ----------
   -- Name --
   ----------

   function Name (View : Source_View) return String is
   begin
      return (+Base_Name (Source_Buffer (Get_Buffer (View)).Get_Filename));
   end Name;

   ----------------------
   -- Hyper_Mode_Enter --
   ----------------------

   procedure Hyper_Mode_Enter (Widget : access Gtk_Widget_Record'Class) is
      View   : constant Source_View   := Source_View (Widget);
   begin
      if View.Hyper_Mode then
         return;
      end if;

      View.Hyper_Mode := True;

      if Active (Me) then
         Trace (Me, "HYPER MODE ENTER " & Name (View));
      end if;

      Set_Cursor (Get_Window (View, Text_Window_Text), null);

      Hyper_Mode_Enter (Source_Buffer (Get_Buffer (View)));

      --  Highlight the current word, if any
      declare
         X, Y : Gint;
      begin
         Get_Pointer (View, X, Y);
         Highlight_On (View, X, Y);
      end;

      --  Connect the motion handler
      View.Hyper_Mode_Motion_Handler :=
        Return_Callback.Connect
          (View, Signal_Motion_Notify_Event,
           Marsh => Return_Callback.To_Marshaller
             (Motion_Notify_Event_Cb'Access),
           After => False);

      --  Connect to a button press on the view

      View.Hyper_Mode_Button_Handler :=
        Return_Callback.Connect
          (View, Signal_Button_Press_Event,
           Marsh => Return_Callback.To_Marshaller
             (Button_Press_Event_Cb'Access),
           After => False);
   end Hyper_Mode_Enter;

   ----------------------
   -- Hyper_Mode_Leave --
   ----------------------

   Text_View_Cursor : Gdk.Cursor.Gdk_Cursor;
   --  This is set as a global variable in order to avoid memory leaks.
   --  See comments in body of GUI_Utils.Set_Busy_Cursor.

   procedure Hyper_Mode_Leave (Widget : access Gtk_Widget_Record'Class) is
      View   : constant Source_View   := Source_View (Widget);
   begin
      if not View.Hyper_Mode then
         return;
      end if;

      View.Hyper_Mode := False;

      if Active (Me) then
         Trace (Me, "HYPER MODE LEAVE " & Name (View));
      end if;

      if Text_View_Cursor = null then
         Gdk_New (Text_View_Cursor, Xterm);
      end if;

      if not In_Destruction_Is_Set (View) then
         Set_Cursor (Get_Window (View, Text_Window_Text), Text_View_Cursor);
      end if;

      Hyper_Mode_Leave (Source_Buffer (Get_Buffer (View)));

      --  Disconnect the motion handler
      Gtk.Handlers.Disconnect (View, View.Hyper_Mode_Motion_Handler);
      View.Hyper_Mode_Motion_Handler :=
        (Gtk.Handlers.Null_Handler_Id, null);

      --  Disconnect the button handler
      Gtk.Handlers.Disconnect (View, View.Hyper_Mode_Button_Handler);
      View.Hyper_Mode_Button_Handler :=
        (Gtk.Handlers.Null_Handler_Id, null);
   end Hyper_Mode_Leave;

   -------------------------
   -- Activate_Hyper_Mode --
   -------------------------

   procedure Activate_Hyper_Mode (View : access Source_View_Record) is
   begin
      Enable_Hyper_Mode (Kernel              => View.Kernel,
                         Widget              => Gtk_Widget (View),
                         On_Hyper_Mode_Enter => Hyper_Mode_Enter'Access,
                         On_Hyper_Mode_Leave => Hyper_Mode_Leave'Access);
   end Activate_Hyper_Mode;

   ---------------------------
   -- Button_Press_Event_Cb --
   ---------------------------

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View      : constant Source_View   := Source_View (Widget);
      Button    : Guint;
   begin
      if not View.Hyper_Mode then
         return False;
      end if;

      if Get_Event_Type (Event) /= Button_Press then
         return False;
      end if;

      Button := Get_Button (Event);

      if not (Button = 1 or else Button = 2) then
         return False;
      end if;

      Hyper_Mode_Click_On
        (Source_Buffer (Get_Buffer (View)),
         Alternate => Button = 2);

      return True;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press_Event_Cb;

   ------------------
   -- Highlight_On --
   ------------------

   procedure Highlight_On (View : Source_View; X, Y : Gint) is
      Line, Column : Gint;
      Iter         : Gtk_Text_Iter;
      Out_Of_Bounds : Boolean := False;
   begin
      Window_To_Buffer_Coords
        (View,
         X - Get_Border_Window_Size (View, Text_Window_Left),
         Y - Get_Border_Window_Size (View, Text_Window_Top),
         Line, Column, Out_Of_Bounds);
      Get_Iter_At_Line_Offset
        (Source_Buffer (Get_Buffer (View)), Iter, Line, Column);

      if Out_Of_Bounds then
         Remove_Highlight (Source_Buffer (Get_Buffer (View)));
      else
         Hyper_Mode_Highlight_On (Source_Buffer (Get_Buffer (View)), Iter);
      end if;
   end Highlight_On;

   ----------------------------
   -- Motion_Notify_Event_Cb --
   ----------------------------

   function Motion_Notify_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);

      View  : constant Source_View   := Source_View (Widget);

      X, Y         : Gint;
   begin
      if Active (Me) then
         Trace (Me, "motion_notify " & Name (View));
      end if;

      --  Call Get_Pointer first, since Gtk+ is waiting for this call in order
      --  to be able to propagate further "motion_notify" events.
      Get_Pointer (View, X, Y);

      if not View.Hyper_Mode then
         return False;
      end if;

      Highlight_On (View, X, Y);

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Motion_Notify_Event_Cb;

end Src_Editor_View.Hyper_Mode;

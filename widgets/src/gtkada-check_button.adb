-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--               Copyright (C) 2008, AdaCore                         --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Glib.Object;      use Glib.Object;
with Gdk.Event;        use Gdk.Event;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;

package body Gtkada.Check_Button is

   package Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtkada_Check_Button_Record, Boolean);

   procedure On_Clicked (Obj : System.Address);
   pragma Convention (C, On_Clicked);
   --  Callback for the clicked event

   function On_Expose
     (Check : access Gtkada_Check_Button_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the expose event

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

   Class_Record : GObject_Class := Uninitialized_Class;

   procedure Initialize
     (Check : access Gtkada_Check_Button_Record'Class;
      Label : UTF8_String := "";
      Default : Boolean := False)
   is
      procedure Install_Clicked_Handler
        (Obj     : System.Address;
         Handler : System.Address);
      pragma Import (C, Install_Clicked_Handler,
                     "gtkada_check_button_install_handler");

      Set_Handler : Boolean := False;
   begin
      Gtkada_Check_Button_Record (Check.all) :=
        (Gtk.Check_Button.Gtk_Check_Button_Record with
         Default  => False,
         State    => State_Unchecked,
         Internal => False);
      Gtk.Check_Button.Initialize (Check, Label);
      Set_Default (Check, Default);

      --  We need to create a new Class Record for this widget, as we are
      --  going to replace the default handler for the 'clicked' signal.
      if Class_Record = Uninitialized_Class then
         Set_Handler := True;
      end if;

      Initialize_Class_Record
        (Object       => Check,
         Signals      => (1 .. 0 => <>),
         Class_Record => Class_Record,
         Type_Name    => "GtkadaCheckButton");

      if Set_Handler then
         --  We replace the class handler for 'clicked' because this signal
         --  has the flag G_SIGNAL_RUN_FIRST which makes the class handler
         --  always being called first even when connecting a new signal
         --  handler with 'Last' set to false.
         --  We absolutely need to be called before the class handler.
         Install_Clicked_Handler (Get_Object (Check), On_Clicked'Address);
      end if;

      Return_Callback.Connect
        (Check,
         Gtk.Widget.Signal_Expose_Event,
         Return_Callback.To_Marshaller (On_Expose'Access));
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

   procedure Set_Active
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
   end Set_Active;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Check : access Gtkada_Check_Button_Record) return State_Type is
   begin
      return Check.State;
   end Get_State;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked (Obj : System.Address)
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

      Underlying_State := Check.State /= State_Unchecked;
      --  the desired underlying GtkCheckButton state

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

      Original_Handler (Get_Object (Check));
   end On_Clicked;

   ---------------
   -- On_Expose --
   ---------------

   function On_Expose
     (Check : access Gtkada_Check_Button_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Area           : constant Gdk_Rectangle := Get_Area (Event);
      State          : Gtk.Enums.Gtk_State_Type;
      Shadow         : Gtk_Shadow_Type;
      X, Y           : Glib.Gint;
      Ind_Size       : Glib.Gint;
      Ind_Spacing    : Glib.Gint;
      Border_Width   : Glib.Gint;
      Interior_Focus : Glib.Gint;
      Focus_Width    : Glib.Gint;
      Focus_Pad      : Glib.Gint;
      Child          : Gtk_Widget;

      procedure Get_Style_Prop
        (Widget        : System.Address;
         Property_Name : String;
         Value         : out Gint);
      pragma Import (C, Get_Style_Prop, "ada_gtk_widget_style_get_int");

   begin
      --  Draw only when the Drawable state is set.
      if not Drawable_Is_Set (Check) then
         return False;
      end if;

      Get_Style_Prop
        (Get_Object (Check),
         "indicator-size" & ASCII.NUL,
         Ind_Size);
      Get_Style_Prop
        (Get_Object (Check),
         "indicator-spacing" & ASCII.NUL,
         Ind_Spacing);
      Get_Style_Prop
        (Get_Object (Check),
         "interior-focus" & ASCII.NUL,
         Interior_Focus);
      Get_Style_Prop
        (Get_Object (Check),
         "focus-line-width" & ASCII.NUL,
         Focus_Width);
      Get_Style_Prop
        (Get_Object (Check),
         "focus-padding" & ASCII.NUL,
         Focus_Pad);
      Border_Width := Gint (Get_Border_Width (Check));

      Child := Get_Child (Check);

      X := Get_Allocation_X (Check) + Ind_Spacing + Border_Width;
      Y := Get_Allocation_Y (Check) +
        (Get_Allocation_Height (Check) - Ind_Size) / 2;

      if Interior_Focus = 0
        or else not (Child /= null and then Visible_Is_Set (Child))
      then
         X := X + Focus_Width + Focus_Pad;
      end if;

      if Get_State (Check) = State_Prelight then
         declare
            Restrict_Area, New_Area : Gdk_Rectangle;
            Intersect_Result        : Boolean;
         begin
            Restrict_Area :=
              (X => Get_Allocation_X (Check) + Border_Width,
               Y => Get_Allocation_Y (Check) + Border_Width,
               Width => Get_Allocation_Width (Check) - (2 * Border_Width),
               Height => Get_Allocation_Height (Check) - (2 * Border_Width));
            Intersect (Area, Restrict_Area, New_Area, Intersect_Result);

            if Intersect_Result then
               Paint_Flat_Box
                 (Style       => Get_Style (Check),
                  Window      => Get_Window (Check),
                  State_Type  => State_Prelight,
                  Shadow_Type => Shadow_Etched_Out,
                  Area        => Area,
                  Widget      => Check,
                  Detail      => "checkbutton",
                  X           => New_Area.X,
                  Y           => New_Area.Y,
                  Width       => New_Area.Width,
                  Height      => New_Area.Height);
            end if;
         end;
      end if;

      State := Get_State (Check);

      case Check.State is
         when State_Checked_Default =>
            Shadow := Shadow_In;
            State := Gtk.Enums.State_Insensitive;

         when State_Checked =>
            Shadow := Shadow_In;

         when State_Unchecked =>
            Shadow := Shadow_None;
      end case;

      Paint_Check
        (Get_Style (Check),
         Get_Window (Check),
         State,
         Shadow,
         Area,
         Check,
         "checkbutton",
         X           => X,
         Y           => Y,
         Width       => Ind_Size,
         Height      => Ind_Size);

      if Has_Focus_Is_Set (Check) then
         if Interior_Focus = 1
           and then Child /= null
           and then Visible_Is_Set (Child)
         then
            Paint_Focus
              (Style      => Get_Style (Check),
               Window     => Get_Window (Check),
               State_Type => Get_State (Check),
               Area       => Area,
               Widget     => Check,
               Detail     => "checkbutton",
               X          =>
                 Get_Allocation_X (Child) - Focus_Width - Focus_Pad,
               Y          =>
                 Get_Allocation_Y (Child) - Focus_Width - Focus_Pad,
               Width      =>
                 Get_Allocation_Width (Child) + 2 * (Focus_Width + Focus_Pad),
               Height     =>
                 Get_Allocation_Height (Child) +
                 2 * (Focus_Width + Focus_Pad));
         else
            Paint_Focus
              (Style      => Get_Style (Check),
               Window     => Get_Window (Check),
               State_Type => Get_State (Check),
               Area       => Area,
               Widget     => Check,
               Detail     => "checkbutton",
               X          => Get_Allocation_X (Check) + Border_Width,
               Y          => Get_Allocation_Y (Check) + Border_Width,
               Width      => Get_Allocation_Width (Check) - 2 * Border_Width,
               Height     => Get_Allocation_Height (Check) - 2 * Border_Width);
         end if;
      end if;

      if Child /= null then
         Propagate_Expose (Check, Child, Event);
      end if;

      --  Stop the signal here: prevent the original expose handler to be
      --  called
      return True;
   end On_Expose;

end Gtkada.Check_Button;

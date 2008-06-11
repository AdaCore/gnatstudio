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
with Gtk.Button;       use Gtk.Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;

package body Gtkada.Check_Button is

   package Callback is new
     Gtk.Handlers.Callback (Gtkada_Check_Button_Record);

   package Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtkada_Check_Button_Record, Boolean);

   procedure On_Clicked
     (Check : access Gtkada_Check_Button_Record'Class);
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

   procedure Initialize
     (Check : access Gtkada_Check_Button_Record'Class;
      Label : UTF8_String := "";
      Default : Boolean := False) is
   begin
      Gtk.Check_Button.Initialize (Check, Label);
      Check.Default := False;
      Check.State   := State_Unchecked;
      Check.In_Handle := False;
      Set_Default (Check, Default);
      Callback.Connect
        (Check, Gtk.Button.Signal_Clicked,
         Callback.To_Marshaller (On_Clicked'Access));
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
      Old_State   : constant State_Type := Check.State;
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

         when State_Checked_Default =>
            Check.State := State_Unchecked;

         when State_Checked =>
            null;
      end case;

      if Check.State /= Old_State then
         Draw (Check);
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
      Old_State   : constant State_Type := Check.State;

   begin
      if State then
         Check.State := State_Checked;
      else
         Check.State := State_Unchecked;
      end if;

      if Check.State /= Old_State then
         Draw (Check);
      end if;
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

   procedure On_Clicked (Check : access Gtkada_Check_Button_Record'Class) is
   begin
      if Check.In_Handle then
         return;
      end if;

      Check.In_Handle := True;

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

      Draw (Check);
      Check.In_Handle := False;
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

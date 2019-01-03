------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Cairo;           use Cairo;
with Gdk;             use Gdk;
with Gdk.Cairo;       use Gdk.Cairo;
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.Main;       use Glib.Main;
with Glib.Properties; use Glib.Properties;
with Gtk.Box;         use Gtk.Box;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Image;       use Gtk.Image;
with Gtk.Label;       use Gtk.Label;
with Gtk.Revealer;    use Gtk.Revealer;
with Gtk.Settings;    use Gtk.Settings;
with Gtk.Widget;      use Gtk.Widget;
with GNATCOLL.Traces; use GNATCOLL.Traces;

with Gtkada.Handlers; use Gtkada.Handlers;

package body Informational_Popups is
   Me : constant Trace_Handle := Create ("GPS.COMMON.POPUPS");

   Informational_Popup_Display_Time : constant Guint := 1_000;
   --  The amount of time during which an informational popup is displayed, in
   --  milliseconds.

   type Informational_Popup_Record is new Gtk_Window_Record with record
      Supports_Transparency : Boolean;
      --  True if the screen's visual supports transparency, False otherwise.

      No_Transparency_Color : Gdk_RGBA;
      --  Color used for the informational popup's background when transparency
      --  is not supported.

      Timeout               : G_Source_Id := No_Source_Id;
      --  When gtk+ animations are disabled
   end record;
   type Informational_Popup is access all Informational_Popup_Record'Class;
   --  Type representing informational popups

   function On_Draw
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean;
   --  Called when an informational popup is about to be drawn.
   --  Used to set the transparency of the informational popup.

   procedure On_Child_Revealed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the 'child-revealed' property of the given Gtk_Revealer
   --  widget has changed.
   --  Used to hide the child just after it has been revealed so that the
   --  informational is only displayed for a brief time.

   procedure On_Destroyed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the widget is destroyed

   function On_Timeout (Self : Informational_Popup) return Boolean;
   --  Simulating the revealer

   package Popup_Sources is new Generic_Sources (Informational_Popup);

   ------------------
   -- On_Destroyed --
   ------------------

   procedure On_Destroyed (Widget : access Gtk_Widget_Record'Class) is
      Popup : constant Informational_Popup := Informational_Popup (Widget);
   begin
      if Popup.Timeout /= No_Source_Id then
         Remove (Popup.Timeout);
         Popup.Timeout := No_Source_Id;
      end if;
   end On_Destroyed;

   ----------------
   -- On_Timeout --
   ----------------

   function On_Timeout (Self : Informational_Popup) return Boolean is
   begin
      Self.Destroy;
      Self.Timeout := No_Source_Id;
      return False;  --  do not execute again
   end On_Timeout;

   -----------------------
   -- On_Child_Revealed --
   -----------------------

   procedure On_Child_Revealed (Widget : access Gtk_Widget_Record'Class) is
      Revealer       : constant Gtk_Revealer := Gtk_Revealer (Widget);
      Child_Revealed : constant Boolean := Revealer.Get_Child_Revealed;
   begin
      Revealer.Set_Reveal_Child (not Child_Revealed);

      --  Destroy the window itself once the child is not revealed anymore
      if not Child_Revealed then
         Revealer.Get_Parent.Destroy;
      end if;
   end On_Child_Revealed;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Self : access Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean
   is
      pragma Unreferenced (Cr);
      Info_Popup : constant Informational_Popup := Informational_Popup (Self);
      New_Cr     : constant Cairo.Cairo_Context := Create (Self.Get_Window);
      Alpha      : constant Gdouble :=
                     (if Info_Popup.Supports_Transparency then
                         0.0
                      else
                         1.0);
   begin
      Set_Source_Rgba
        (New_Cr,
         Red   => Info_Popup.No_Transparency_Color.Red,
         Green => Info_Popup.No_Transparency_Color.Green,
         Blue  => Info_Popup.No_Transparency_Color.Blue,
         Alpha => Alpha);

      Set_Operator (New_Cr, Op => Cairo_Operator_Source);
      Paint (New_Cr);

      Destroy (New_Cr);

      return False;
   end On_Draw;

   ---------------------------------
   -- Display_Informational_Popup --
   ---------------------------------

   procedure Display_Informational_Popup
     (Parent                : not null access Gtk_Window_Record'Class;
      Icon_Name             : String;
      No_Transparency_Color : Gdk_RGBA := Black_RGBA;
      Text                  : String := "")
   is
      Info_Popup : Informational_Popup;
      Icon       : Gtk_Image;
      Revealer   : Gtk_Revealer;
      Screen     : Gdk_Screen;
      Visual     : Gdk_Visual;
      Box        : Gtk_Box;
      Label      : Gtk_Label;
   begin
      Info_Popup := new Informational_Popup_Record;
      Info_Popup.No_Transparency_Color := No_Transparency_Color;
      Initialize (Info_Popup);
      Info_Popup.Set_Decorated (False);
      Info_Popup.Set_Transient_For (Parent);
      Info_Popup.Set_Position (Win_Pos_Center_On_Parent);
      Info_Popup.Set_Accept_Focus (False);
      Info_Popup.Set_App_Paintable (True);
      Info_Popup.Set_Focus_On_Map (False);
      Info_Popup.On_Draw (On_Draw'Access);

      Screen := Info_Popup.Get_Screen;
      if Screen = null then
         Screen := Get_Default;
      end if;

      Visual := Get_Rgba_Visual (Screen);
      Info_Popup.Supports_Transparency := Visual /= null;

      if Visual /= null then
         Info_Popup.Set_Visual (Visual);
      end if;

      Gtk_New_Hbox (Box, Homogeneous => False);

      Gtk_New_From_Icon_Name
        (Icon,
         Icon_Name => Icon_Name,
         Size      => Icon_Size_Dialog);
      Box.Pack_Start (Icon, Expand => False, Fill => False);

      if Text /= "" then
         Gtk_New (Label, Text);
         Label.Set_Padding (20, 20);
         Box.Pack_Start (Label, Expand => False, Fill => False);
      end if;

      if Get_Property (Get_Settings (Parent),
                       Gtk_Enable_Animations_Property)
      then
         Gtk_New (Revealer);
         Widget_Callback.Object_Connect
           (Revealer,
            Name        => "notify::child-revealed",
            Cb          => On_Child_Revealed'Access,
            Slot_Object => Revealer);
         Info_Popup.Add (Revealer);
         Revealer.Add (Box);
         Revealer.Set_Transition_Duration
           (Informational_Popup_Display_Time / 2);
         Revealer.Set_Transition_Type (Revealer_Transition_Type_Crossfade);

         Info_Popup.Show_All;
         Revealer.Set_Reveal_Child (True);
      else
         Trace (Me, "Animations are disabled, using fallback");
         Info_Popup.On_Destroy (On_Destroyed'Access);
         Info_Popup.Add (Box);
         Info_Popup.Timeout := Popup_Sources.Timeout_Add
           (Informational_Popup_Display_Time,
            On_Timeout'Access,
            Info_Popup);

         Info_Popup.Show_All;
      end if;
   end Display_Informational_Popup;

end Informational_Popups;

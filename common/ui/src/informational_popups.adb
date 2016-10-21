------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2016, AdaCore                     --
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
with Gdk.Screen;      use Gdk.Screen;
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Image;       use Gtk.Image;
with Gtk.Revealer;    use Gtk.Revealer;
with Gtk.Widget;      use Gtk.Widget;

with Gtkada.Handlers; use Gtkada.Handlers;

package body Informational_Popups is

   Informational_Popup_Display_Time : constant Guint := 1_000;
   --  The amount of time during which an informational popup is displayed, in
   --  milliseconds.

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
      pragma Unreferenced (Self);
   begin
      Set_Source_Rgba
        (Cr,
         Red   => 0.0,
         Green => 0.0,
         Blue  => 0.0,
         Alpha => 0.0);

      Set_Operator (Cr, Op => Cairo_Operator_Source);
      Paint (Cr);

      return False;
   end On_Draw;

   ---------------------------------
   -- Display_Informational_Popup --
   ---------------------------------

   procedure Display_Informational_Popup
     (Parent     : not null access Gtk_Window_Record'Class;
      Icon_Name  : String)
   is
      Info_Popup : Gtk_Window;
      Icon       : Gtk_Image;
      Revealer   : Gtk_Revealer;
      Screen     : Gdk_Screen;
      Visual     : Gdk_Visual;
   begin
      Gtk_New (Info_Popup);
      Info_Popup.Set_Decorated (False);
      Info_Popup.Set_Transient_For (Parent);
      Info_Popup.Set_Position (Win_Pos_Center_On_Parent);
      Info_Popup.Set_Accept_Focus (False);
      Info_Popup.Set_App_Paintable (True);
      Info_Popup.On_Draw (On_Draw'Access);

      Screen := Info_Popup.Get_Screen;
      if Screen = null then
         Screen := Get_Default;
      end if;

      Visual := Get_Rgba_Visual (Screen);
      if Visual /= null then
         Info_Popup.Set_Visual (Visual);
      end if;

      Gtk_New_From_Icon_Name
        (Icon,
         Icon_Name => Icon_Name,
         Size      => Icon_Size_Dialog);

      Gtk_New (Revealer);
      Widget_Callback.Object_Connect
        (Revealer,
         Name        => "notify::child-revealed",
         Cb          => On_Child_Revealed'Access,
         Slot_Object => Revealer);
      Info_Popup.Add (Revealer);
      Revealer.Add (Icon);
      Revealer.Set_Transition_Duration (Informational_Popup_Display_Time / 2);
      Revealer.Set_Transition_Type (Revealer_Transition_Type_Crossfade);

      Info_Popup.Show_All;
      Revealer.Set_Reveal_Child (True);
   end Display_Informational_Popup;

end Informational_Popups;

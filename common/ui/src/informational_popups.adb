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

with Glib;            use Glib;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Image;       use Gtk.Image;
with Gtk.Revealer;    use Gtk.Revealer;
with Gtk.Widget;      use Gtk.Widget;
with Gdk.Window;      use Gdk.Window;

with Gtkada.Handlers; use Gtkada.Handlers;

package body Informational_Popups is

   Informational_Popup_Display_Time : constant Guint := 1_000;
   --  The amount of time during which an informational popup is displayed, in
   --  milliseconds.

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
   begin
      Gtk_New (Info_Popup);
      Info_Popup.Set_Decorated (False);
      Info_Popup.Set_Resizable (False);
      Info_Popup.Set_Type_Hint (Window_Type_Hint_Notification);
      Info_Popup.Set_Transient_For (Parent);
      Info_Popup.Set_Position (Win_Pos_Center_On_Parent);

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

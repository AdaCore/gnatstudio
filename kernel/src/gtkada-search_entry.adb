------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Gdk.Window; use Gdk, Gdk.Window;
with Gdk.Rectangle, Glib; use Glib, Gdk.Rectangle;
with Gtk.Style_Context;  use Gtk.Style_Context;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.Handlers;    use Gtkada.Handlers;
with GPS.Intl;           use GPS.Intl;

package body Gtkada.Search_Entry is

   procedure On_Clear_Entry
      (Self  : access Gtk_Entry_Record'Class;
       Pos   : Gtk_Entry_Icon_Position;
       Event : Gdk_Event_Button);
   --  Called when the user presses the "clear" icon

   procedure On_Changed (Self : access Gtk_Widget_Record'Class);
   --  Called when the text of the entry has changed.

   -----------------------
   -- Get_Icon_Position --
   -----------------------

   function Get_Icon_Position
     (Self   : access Gtkada_Search_Entry_Record'Class;
      Event  : Gdk_Event_Button) return Gtk_Entry_Icon_Position
   is
      Alloc : Gtk_Allocation;
      Rect  : Gdk_Rectangle;
      X, Y  : Gint;
   begin
      Self.Get_Allocation (Alloc);
      Get_Position (Event.Window, X, Y);

      Self.Get_Icon_Area (Gtk_Entry_Icon_Primary, Rect);

      if X - Alloc.X = Rect.X then
         return Gtk_Entry_Icon_Primary;
      else
         return Gtk_Entry_Icon_Secondary;
      end if;
   end Get_Icon_Position;

   --------------------
   -- On_Clear_Entry --
   --------------------

   procedure On_Clear_Entry
      (Self  : access Gtk_Entry_Record'Class;
       Pos   : Gtk_Entry_Icon_Position;
       Event : Gdk_Event_Button)
   is
      pragma Unreferenced (Pos);  --  unreliable with gtk+ 3.8
   begin
      if Gtkada_Search_Entry (Self).Get_Icon_Position (Event) =
        Gtk_Entry_Icon_Secondary
      then
         Self.Set_Text ("");

         --  in case the filter is activated only on activate
         Widget_Callback.Emit_By_Name (Self, Signal_Activate);
      end if;
   end On_Clear_Entry;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : access Gtk_Widget_Record'Class) is
      S  : constant Gtkada_Search_Entry := Gtkada_Search_Entry (Self);
   begin
      if S.Get_Text /= "" then
         S.Set_Icon_From_Icon_Name
            (Gtk_Entry_Icon_Secondary, "gps-clear-entry-symbolic");
         S.Set_Icon_Activatable (Gtk_Entry_Icon_Secondary, True);
         S.Set_Icon_Tooltip_Text
            (Gtk_Entry_Icon_Secondary, -"Clear the pattern");
      else
         S.Set_Icon_From_Icon_Name (Gtk_Entry_Icon_Secondary, "");
         S.Set_Icon_Activatable (Gtk_Entry_Icon_Secondary, False);
      end if;
   end On_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtkada_Search_Entry;
       Placeholder : String := "") is
   begin
      Self := new Gtkada_Search_Entry_Record;
      Gtk.GEntry.Initialize (Self);

      if Placeholder /= "" then
         Self.Set_Placeholder_Text (Placeholder);
      end if;

      Get_Style_Context (Self).Add_Class ("search");

      Self.On_Icon_Press (On_Clear_Entry'Access);
      Widget_Callback.Connect (Self, Signal_Changed, On_Changed'Access);
   end Gtk_New;

end Gtkada.Search_Entry;

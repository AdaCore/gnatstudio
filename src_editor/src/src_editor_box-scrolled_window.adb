------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Gdk.Event;             use Gdk.Event;
with Gdk.Window;            use Gdk.Window;
with Glib.Object;           use Glib, Glib.Object;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Main;              use Gtk.Main;
with Gtk.Style_Context;     use Gtk.Style_Context;
with Gtk.Text_Iter;         use Gtk.Text_Iter;
with Gtk.Text_View;         use Gtk.Text_View;
with Gtk.Widget;            use Gtk.Widget;
with GNATCOLL.Symbols;      use GNATCOLL.Symbols;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with Pango.Layout;          use Pango.Layout;
with Src_Editor_Buffer;     use Src_Editor_Buffer;

package body Src_Editor_Box.Scrolled_Window is

   Display_Popup_On_Wheel : constant Boolean := False;
   --  Whether to display the popup when scrolling is done with mouse
   --  wheel or trackpad.

   function On_Button_Press
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean;
   function On_Button_Release
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean;
   --  Called when the user interacts with the scrolled window with the mouse.

   procedure On_Changed (Self  : access GObject_Record'Class);
   --  Called when the window is scrolled

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self  : access GObject_Record'Class) is
      S : constant Tooltip_Scrolled_Window := Tooltip_Scrolled_Window (Self);
      Adj  : constant Gtk_Adjustment := S.Get_Vadjustment;
      View : constant Gtk_Text_View := Gtk_Text_View (S.Get_Child);
      Scrollbar_Width : constant Gint := 10;
      Buffer : Source_Buffer;
      Iter : Gtk_Text_Iter;
      Trailing : aliased Gint;
      X, Y, Width, Height : Gint;
      Alloc : Gtk_Allocation;
      Min, Nat : Gtk_Requisition;
      Event : Gdk_Event;
      In_Text : Boolean;

   begin
      --  Is the scroll due to a wheel or touchpad event ?
      if not S.Has_Button_Press then
         Event := Gtk.Main.Get_Current_Event;
         if Event = null  --  would be a programmatic change
            or else Get_Event_Type (Event) /= Scroll
            or else not Display_Popup_On_Wheel
         then
            return;
         end if;

         --  For a Scroll event, we either need to remove the dialog
         --  after a timeout or grab the mouse to detect a
         --  change. But if we do that, the scroll window no longer
         --  gets the event.
      end if;

      --  Else the user is scrolling the scroll bar with the mouse

      View.Get_Iter_At_Position
        (Iter, Trailing, X => 0,
         Y => Gint (Adj.Get_Value + Adj.Get_Page_Size / 2.0),
         In_Text => In_Text);

      Buffer := Source_Buffer (View.Get_Buffer);

      if S.Window = null then
         Gtk_New (S.Label);
      end if;

      S.Label.Set_Markup
         ("<b>File:</b> " & Buffer.Get_Filename.Display_Base_Name
          & ASCII.LF
          & "<b>Line:</b> " & Gint'Image (Get_Line (Iter) + 1)
          & " ("
          & Image (Integer (100.0 * Adj.Get_Value / Adj.Get_Upper),
                   Min_Width => 1)
          & "%)"
          & ASCII.LF
          & "<b>Entity:</b> "
          & Get (Buffer.Get_Subprogram_Block
             (Line => Editable_Line_Type (Get_Line (Iter))).Name).all);

      if S.Window = null then
         Gtk_New (S.Window, Window_Popup);
         Get_Style_Context (S.Window).Add_Class ("tooltip");
         S.Window.Set_Type_Hint (Window_Type_Hint_Combo);
         S.Window.Set_Resizable (False);
         S.Window.Set_Skip_Taskbar_Hint (True);
         S.Window.Set_Skip_Pager_Hint (True);
         S.Window.Set_Transient_For (Gtk_Window (S.Get_Toplevel));

         S.Label.Set_Ellipsize (Ellipsize_End);
         S.Label.Set_Alignment (0.0, 0.5);
         S.Label.Set_Padding (5, 5);
         S.Window.Add (S.Label);

         S.Label.Get_Preferred_Size (Min, Nat);
         Width := Gint'Max (Nat.Width, 200);
         Height := Gint'Max (Nat.Height, 40);

         Get_Origin (View.Get_Window, X, Y);
         View.Get_Allocation (Alloc);
         X := X + Alloc.X + Alloc.Width - Width - Scrollbar_Width;
         Y := Y + Alloc.Y + (Alloc.Height - Height) / 2;

         S.Window.Move (X, Y);
         S.Window.Set_Size_Request (Width, Height);
         S.Window.Queue_Resize;
         S.Window.Set_Screen (S.Get_Screen);
         S.Window.Show_All;
      end if;
   end On_Changed;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);
      S : constant Tooltip_Scrolled_Window := Tooltip_Scrolled_Window (Self);
   begin
      S.Has_Button_Press := True;
      On_Changed (S);
      return False;
   end On_Button_Press;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
      (Self  : access GObject_Record'Class;
       Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);
      S : constant Tooltip_Scrolled_Window := Tooltip_Scrolled_Window (Self);
   begin
      S.Has_Button_Press := False;

      if S.Window /= null then
         S.Window.Destroy;
         S.Window := null;
      end if;

      return False;
   end On_Button_Release;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Tooltip_Scrolled_Window) is
   begin
      Self := new Tooltip_Scrolled_Window_Record;
      Gtk.Scrolled_Window.Initialize (Self);
      Self.Set_Policy (Policy_Automatic, Policy_Automatic);

      Self.Get_Vscrollbar.On_Button_Press_Event (On_Button_Press'Access, Self);
      Self.Get_Vscrollbar.On_Button_Release_Event
         (On_Button_Release'Access, Self);
      Self.Get_Vadjustment.On_Value_Changed (On_Changed'Access, Self);
   end Gtk_New;

end Src_Editor_Box.Scrolled_Window;

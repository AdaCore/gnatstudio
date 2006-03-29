-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2006                            --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Object; use Glib.Object;

with Gdk;        use Gdk;
with Gdk.Color;  use Gdk.Color;
with Gdk.GC;     use Gdk.GC;
with Gdk.Event;  use Gdk.Event;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gdk.Window; use Gdk.Window;

with Gtk.Box;       use Gtk.Box;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Label;     use Gtk.Label;
with Gtk.Notebook;  use Gtk.Notebook;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Enums; use Gtk.Enums;

package body Collapsing_Pane is

   Collapsed_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("10 10 3 1"),
      New_String ("      c None"),
      New_String (".     c #FFFFFF"),
      New_String ("+     c #000000"),
      New_String ("          "),
      New_String (" ++       "),
      New_String (" + ++     "),
      New_String (" +   ++   "),
      New_String (" +     ++ "),
      New_String (" +     ++ "),
      New_String (" +   ++   "),
      New_String (" + ++     "),
      New_String (" ++       "),
      New_String ("          "));

   Expanded_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("10 10 3 1"),
      New_String ("      c None"),
      New_String (".     c #FFFFFF"),
      New_String ("+     c #000000"),
      New_String ("          "),
      New_String (" ++++++++ "),
      New_String (" +      + "),
      New_String ("  +    +  "),
      New_String ("  +    +  "),
      New_String ("   +  +   "),
      New_String ("   +  +   "),
      New_String ("    ++    "),
      New_String ("    ++    "),
      New_String ("          "));

   procedure On_Change_State (Object : access Gtk_Widget_Record'Class);
   --  Called when the user clicks on the label or the collapse icon

   procedure On_Destroy (Object : access Gtk_Widget_Record'Class);
   --  Called when the widget is destroyed

   procedure On_Map (Widget : access Gtk_Widget_Record'Class);
   --  Called in order to create the GC associated to this widget

   function On_Expose_Pixmap
     (Object : access Glib.Object.GObject_Record'Class)
      return Boolean;
   --  Called when the pixmap has to be redrawed

   procedure Refresh_Pixmap (Pane : access Collapsing_Pane_Record'Class);
   --  Forces the redraw of the pixmap

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Pane : out Collapsing_Pane; Label : UTF8_String) is
   begin
      Pane := new Collapsing_Pane_Record;
      Initialize (Pane, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Pane : Collapsing_Pane; Label : UTF8_String) is
      Label_Hbox                       : Gtk_Box;
      Original_Color, Background_Color : Gdk.Color.Gdk_Color;
      Success                          : Boolean;
   begin
      Pane.Collapsed_Pixbuf :=
        Gdk.Pixbuf.Gdk_New_From_Xpm_Data (Collapsed_Xpm);
      Pane.Expanded_Pixbuf :=
        Gdk.Pixbuf.Gdk_New_From_Xpm_Data (Expanded_Xpm);

      Gtk.Event_Box.Initialize (Pane);
      Widget_Callback.Object_Connect
        (Pane,
         "destroy", On_Destroy'Access, Pane);
      Original_Color := Get_Bg (Get_Style (Pane), State_Normal);
      Alloc_Color
        (Get_Colormap (Pane),
         Background_Color,
         Success => Success);
      Set_Rgb
        (Background_Color,
         Guint16 (Float (Red (Original_Color)) / 1.1),
         Guint16 (Float (Green (Original_Color)) / 1.1),
         Guint16 (Float (Blue (Original_Color)) / 1.1));
      Modify_Bg (Pane, State_Normal, Background_Color);
      Modify_Fg (Pane, State_Normal, Background_Color);
      Modify_Base (Pane, State_Normal, Background_Color);

      Gtk_New_Vbox (Pane.Main_Box, Homogeneous => False);
      Add (Pane, Pane.Main_Box);
      Set_Border_Width (Pane.Main_Box, 3);

      Gtk_New (Pane.Label_Box);
      Add_Events (Pane.Label_Box, Button_Release_Mask);
      Widget_Callback.Object_Connect
        (Pane.Label_Box,
         "button_release_event", On_Change_State'Access, Pane);
      Pack_Start
        (Pane.Main_Box, Pane.Label_Box, Expand => False, Fill => False);

      Gtk_New_Hbox (Label_Hbox, Homogeneous => False);
      Add (Pane.Label_Box, Label_Hbox);

      Gtk_New_Vbox (Pane.Label_Image);
      Gtkada.Handlers.Object_Return_Callback.Object_Connect
        (Pane.Label_Image, "expose_event",
         On_Expose_Pixmap'Access, Pane);
      Widget_Callback.Connect
        (Pane, "map",
         Marsh => Widget_Callback.To_Marshaller (On_Map'Access),
         After => True);
      Set_Size_Request
        (Pane.Label_Image,
         Gdk.Pixbuf.Get_Width (Pane.Expanded_Pixbuf),
         Gdk.Pixbuf.Get_Height (Pane.Expanded_Pixbuf));
      Pack_Start
        (Label_Hbox,
         Pane.Label_Image,
         Padding => 3,
         Expand => False,
         Fill => False);

      Gtk_New (Pane.Label, Label);
      Pack_Start
        (Label_Hbox, Pane.Label, Padding => 3, Expand => False, Fill => False);

      Gtk_New (Pane.Notebook);
      Set_Show_Tabs (Pane.Notebook, False);
      Set_Show_Border (Pane.Notebook, False);
      Pack_Start (Pane.Main_Box, Pane.Notebook, Expand => True, Fill => False);
   end Initialize;

   -------------------------
   -- Set_Expanded_Widget --
   -------------------------

   procedure Set_Expanded_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      if Pane.Page_Expanded /= -1 then
         Remove_Page (Pane.Notebook, Pane.Page_Expanded);
      end if;

      Append_Page (Pane.Notebook, Widget);
      Pane.Page_Expanded := Page_Num (Pane.Notebook, Widget);
      Pane.Expanded_Widget := Gtk_Widget (Widget);
   end Set_Expanded_Widget;

   --------------------------
   -- Set_Collapsed_Widget --
   --------------------------

   procedure Set_Collapsed_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      if Pane.Page_Collapsed /= -1 then
         Remove_Page (Pane.Notebook, Pane.Page_Collapsed);
      end if;

      Append_Page (Pane.Notebook, Widget);
      Pane.Page_Collapsed := Page_Num (Pane.Notebook, Widget);
      Pane.Collapsed_Widget := Gtk_Widget (Widget);
   end Set_Collapsed_Widget;

   -----------------------
   -- Set_Folding_State --
   -----------------------

   procedure Set_State
     (Pane : access Collapsing_Pane_Record'Class; State : Foldable_Box_State)
   is
   begin
      if State = Pane.State then
         return;
      end if;

      --  We need to explicitely hide the notebook, otherwise the size is
      --  incorrectly recomputed by gtk+. Looks like a bug to me..

      if State = Collapsed then
         if Pane.Page_Collapsed /= -1 then
            Set_Child_Visible (Pane.Notebook, True);
            Show (Pane.Notebook);
            Set_Current_Page (Pane.Notebook, Pane.Page_Collapsed);

            if Pane.Expanded_Widget /= null then
               Set_Size_Request (Pane.Expanded_Widget, 0, 0);
            end if;

            Set_Size_Request (Pane.Collapsed_Widget, -1, -1);
            Set_Size_Request (Pane.Notebook, -1, -1);
         else
            Set_Child_Visible (Pane.Notebook, False);
            Hide (Pane.Notebook);
            Set_Size_Request (Pane.Notebook, 0, 0);
         end if;
      elsif State = Expanded then
         if Pane.Page_Expanded /= -1 then
            Set_Child_Visible (Pane.Notebook, True);
            Show (Pane.Notebook);
            Set_Current_Page (Pane.Notebook, Pane.Page_Expanded);

            if Pane.Collapsed_Widget /= null then
               Set_Size_Request (Pane.Collapsed_Widget, 0, 0);
            end if;

            Set_Size_Request (Pane.Expanded_Widget, -1, -1);
            Set_Size_Request (Pane.Notebook, -1, -1);
         else
            Set_Child_Visible (Pane.Notebook, False);
            Hide (Pane.Notebook);
            Set_Size_Request (Pane.Notebook, 0, 0);
         end if;
      end if;

      if Pane.Reduce_Window then
         declare
            Win : Gtk_Widget;
         begin
            Win := Gtk_Widget (Get_Toplevel (Pane));

            if Win /= null and then Win.all in Gtk_Window_Record'Class then
               Resize (Gtk_Window (Win), -1, -1);
            end if;
         end;
      end if;

      Pane.State := State;

      Refresh_Pixmap (Pane);
   end Set_State;

   ---------------------
   -- On_Change_State --
   ---------------------

   procedure On_Change_State (Object : access Gtk_Widget_Record'Class) is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      if Pane.State = Collapsed then
         Set_State (Pane, Expanded);
      else
         Set_State (Pane, Collapsed);
      end if;
   end On_Change_State;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Object : access Gtk_Widget_Record'Class) is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      Unref (Pane.Collapsed_Pixbuf);
      Unref (Pane.Expanded_Pixbuf);
   end On_Destroy;

   ------------
   -- On_Map --
   ------------

   procedure On_Map (Widget : access Gtk_Widget_Record'Class) is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Widget);
   begin
      Gdk_New (Pane.GC, Get_Window (Pane.Label_Image));
   end On_Map;

   ----------------------
   -- On_Expose_Pixmap --
   ----------------------

   function On_Expose_Pixmap
     (Object : access Glib.Object.GObject_Record'Class)
    return Boolean
   is
      Pane             : constant Collapsing_Pane := Collapsing_Pane (Object);
      Pixbuf_Displayed : Gdk.Pixbuf.Gdk_Pixbuf;
   begin

      if Pane.State = Collapsed then
         Pixbuf_Displayed := Pane.Collapsed_Pixbuf;
      else
         Pixbuf_Displayed := Pane.Expanded_Pixbuf;
      end if;

      Gdk.Pixbuf.Render_To_Drawable
        (Pixbuf   => Pixbuf_Displayed,
         Drawable => Get_Window (Pane.Label_Image),
         Gc       => Pane.GC,
         Src_X    => 0,
         Src_Y    => 0,
         Dest_X   => 0,
         Dest_Y   =>
           (Get_Allocation_Height (Pane.Label) -
              Gdk.Pixbuf.Get_Height (Pixbuf_Displayed))
           / 2,
         Width    => Gdk.Pixbuf.Get_Width (Pixbuf_Displayed),
         Height   => Gdk.Pixbuf.Get_Height (Pixbuf_Displayed));

      return True;
   end On_Expose_Pixmap;

   -----------------------
   -- Set_Reduce_Window --
   -----------------------

   procedure Set_Reduce_Window
     (Pane : access Collapsing_Pane_Record'Class; Reduce_Window : Boolean)
   is
   begin
      Pane.Reduce_Window := Reduce_Window;
   end Set_Reduce_Window;

   --------------------
   -- Refresh_Pixmap --
   --------------------

   procedure Refresh_Pixmap (Pane : access Collapsing_Pane_Record'Class)
   is
      X, Y, W, H, D : Gint;
   begin
      if Pane.GC /= Null_GC then
         Get_Geometry (Get_Window (Pane.Label_Image), X, Y, W, H, D);
         Gdk.Window.Invalidate_Rect
           (Get_Window (Pane.Label_Image), (0, 0, W, H), True);
      end if;
   end Refresh_Pixmap;

end Collapsing_Pane;

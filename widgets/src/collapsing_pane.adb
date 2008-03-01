-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2006-2008, AdaCore                --
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
with Gtk.Object;    use Gtk.Object;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Enums; use Gtk.Enums;

package body Collapsing_Pane is

   Collapsed_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("11 11 5 1"),
      New_String ("      c None"),
      New_String (".     c #FFFFFF"),
      New_String ("+     c #000000"),
      New_String ("-     c #C0C0C0"),
      New_String ("=     c #818181"),
      New_String ("    -      "),
      New_String ("    +-     "),
      New_String ("    ++-    "),
      New_String ("    + +-   "),
      New_String ("    +  +-  "),
      New_String ("    +  =+  "),
      New_String ("    +  +-  "),
      New_String ("    + +-   "),
      New_String ("    ++-    "),
      New_String ("    +-     "),
      New_String ("    -      "));

   Expanded_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("11 11 5 1"),
      New_String ("      c None"),
      New_String (".     c #FFFFFF"),
      New_String ("+     c #000000"),
      New_String ("-     c #C0C0C0"),
      New_String ("=     c #818181"),
      New_String ("           "),
      New_String ("           "),
      New_String ("           "),
      New_String ("           "),
      New_String ("-+++++++++-"),
      New_String (" -+-   -+- "),
      New_String ("  -+- -+-  "),
      New_String ("   -+=+-   "),
      New_String ("    -+-    "),
      New_String ("           "),
      New_String ("           "));

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;
   --  A pointer to the 'class record'

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
               (1 => New_String (String (Signal_Toggled)));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_None, GType_None));
   --  The parameters associated to each new signal

   function On_Change_State
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user clicks on the label or the collapse icon

   function On_Enter_Label
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the cursor enters in the head label of the pane

   function On_Exit_Label
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the cursor exits the head label of the pane

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
      Prelight_Color                   : Gdk.Color.Gdk_Color;
      Success                          : Boolean;
   begin
      Pane.Collapsed_Pixbuf :=
        Gdk.Pixbuf.Gdk_New_From_Xpm_Data (Collapsed_Xpm);
      Pane.Expanded_Pixbuf :=
        Gdk.Pixbuf.Gdk_New_From_Xpm_Data (Expanded_Xpm);

      Gtk.Event_Box.Initialize (Pane);

      Glib.Object.Initialize_Class_Record
        (Pane, Signals, Class_Record, "CollapsingPane",
         Signal_Parameters);

      Widget_Callback.Object_Connect
        (Pane,
         Signal_Destroy, On_Destroy'Access, Pane);
      Original_Color := Get_Bg (Get_Style (Pane), State_Normal);
      Set_Rgb
        (Background_Color,
         Guint16 (Float (Red (Original_Color)) / 1.1),
         Guint16 (Float (Green (Original_Color)) / 1.1),
         Guint16 (Float (Blue (Original_Color)) / 1.1));
      Alloc_Color
        (Get_Colormap (Pane),
         Background_Color,
         Success => Success);
      Modify_Bg (Pane, State_Normal, Background_Color);
      Modify_Fg (Pane, State_Normal, Background_Color);
      Modify_Base (Pane, State_Normal, Background_Color);

      Gtk_New_Vbox (Pane.Main_Box, Homogeneous => False);
      Add (Pane, Pane.Main_Box);
      Set_Border_Width (Pane.Main_Box, 1);

      Gtk_New (Pane.Label_Box);
      Add_Events (Pane.Label_Box, Button_Release_Mask);
      Return_Callback.Object_Connect
        (Pane.Label_Box,
         Signal_Button_Release_Event, On_Change_State'Access, Pane);
      Return_Callback.Object_Connect
        (Pane.Label_Box,
         Signal_Leave_Notify_Event, On_Exit_Label'Access, Pane);
      Return_Callback.Object_Connect
        (Pane.Label_Box,
         Signal_Enter_Notify_Event, On_Enter_Label'Access, Pane);
      Set_Rgb
        (Prelight_Color,
         16#FFFF#,
         16#FFFF#,
         16#FFFF#);
      Alloc_Color
        (Get_Colormap (Pane.Label_Box),
         Prelight_Color,
         Success => Success);
      Modify_Bg
        (Pane.Label_Box, State_Prelight, Prelight_Color);
      Pack_Start
        (Pane.Main_Box, Pane.Label_Box, Expand => False, Fill => False);

      Gtk_New_Hbox (Label_Hbox, Homogeneous => False);
      Add (Pane.Label_Box, Label_Hbox);

      Gtk_New_Vbox (Pane.Label_Image);
      Gtkada.Handlers.Object_Return_Callback.Object_Connect
        (Pane.Label_Image, Signal_Expose_Event,
         On_Expose_Pixmap'Access, Pane);
      Widget_Callback.Connect
        (Pane, Signal_Map,
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

      Gtk_New_Vbox (Pane.Expanded_Box, Homogeneous => False);
      Gtk_New_Vbox (Pane.Collapsed_Box, Homogeneous => False);
      Pack_Start (Pane.Main_Box, Pane.Expanded_Box,
                  Expand => False, Fill => False);
      Pack_Start (Pane.Main_Box, Pane.Collapsed_Box,
                  Expand => False, Fill => False);
   end Initialize;

   -------------------------
   -- Set_Expanded_Widget --
   -------------------------

   procedure Set_Expanded_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Child : Gtk_Widget;
   begin
      Child := Get_Child (Pane.Expanded_Box, 0);

      if Child /= null then
         Remove (Pane.Expanded_Box, Child);
      end if;

      Pack_Start (Pane.Expanded_Box, Widget, Expand => False, Fill => False);

      if Pane.State = Collapsed then
         Set_Child_Visible (Pane.Expanded_Box, False);
         Set_Size_Request (Pane.Expanded_Box, 0, 0);
      else
         Set_Child_Visible (Pane.Expanded_Box, True);
         Set_Size_Request (Pane.Expanded_Box, -1, -1);
      end if;
   end Set_Expanded_Widget;

   --------------------------
   -- Set_Collapsed_Widget --
   --------------------------

   procedure Set_Collapsed_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Child : Gtk_Widget;
   begin
      Child := Get_Child (Pane.Collapsed_Box, 0);

      if Child /= null then
         Remove (Pane.Collapsed_Box, Child);
      end if;

      Pack_Start (Pane.Collapsed_Box, Widget, Expand => False, Fill => False);

      if Pane.State = Collapsed then
         Set_Child_Visible (Pane.Collapsed_Box, True);
         Set_Size_Request (Pane.Collapsed_Box, -1, -1);
      else
         Set_Child_Visible (Pane.Collapsed_Box, False);
         Set_Size_Request (Pane.Collapsed_Box, 0, 0);
      end if;
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
         Set_Child_Visible (Pane.Expanded_Box, False);
         Set_Size_Request (Pane.Expanded_Box, 0, 0);
         Set_Child_Visible (Pane.Collapsed_Box, True);
         Set_Size_Request (Pane.Collapsed_Box, -1, -1);

      else
         Set_Child_Visible (Pane.Expanded_Box, True);
         Set_Size_Request (Pane.Expanded_Box, -1, -1);
         Set_Child_Visible (Pane.Collapsed_Box, False);
         Set_Size_Request (Pane.Collapsed_Box, 0, 0);
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

      Widget_Callback.Emit_By_Name (Pane, Signal_Toggled);
   end Set_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Pane : access Collapsing_Pane_Record'Class) return Foldable_Box_State is
   begin
      return Pane.State;
   end Get_State;

   ---------------------
   -- On_Change_State --
   ---------------------

   function On_Change_State
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      if Pane.State = Collapsed then
         Set_State (Pane, Expanded);
      else
         Set_State (Pane, Collapsed);
      end if;

      return True;
   end On_Change_State;

   --------------------
   -- On_Enter_Label --
   --------------------

   function On_Enter_Label
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      Set_State (Pane.Label_Box, State_Prelight);
      return False;
   end On_Enter_Label;

   -------------------
   -- On_Exit_Label --
   -------------------

   function On_Exit_Label
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      Set_State (Pane.Label_Box, State_Normal);
      return False;
   end On_Exit_Label;

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
         GC       => Pane.GC,
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

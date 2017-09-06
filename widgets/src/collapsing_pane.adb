------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Object;          use Glib.Object;

with Gdk;                  use Gdk;
with Gdk.Event;            use Gdk.Event;

with Gtk.Arrow;            use Gtk.Arrow;
with Gtk.Box;              use Gtk.Box;
with Gtk.Event_Box;        use Gtk.Event_Box;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Label;            use Gtk.Label;
with Gtk.Widget;           use Gtk.Widget;

with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtk.Enums;            use Gtk.Enums;

package body Collapsing_Pane is

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;
   --  A pointer to the 'class record'

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
               (1 => New_String (String (Signal_Toggled)));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_None, GType_None));
   --  The parameters associated to each new signal

   procedure Set_Child_Widget
     (Pane : access Collapsing_Pane_Record'Class; Widget : Gtk_Widget);
   --  Sets the child to be displayed

   function On_Change_State
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Called when the user clicks on the label or the collapse icon

   procedure On_Destroy
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the collapsing pane is destroyed

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
      Label_Hbox : Gtk_Box;
      Frame      : Gtk_Frame;

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Event_Box.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CollapsingPane",
         Parameters   => Signal_Parameters);
      Glib.Object.G_New (Pane, Class_Record);

      Gtk.Frame.Gtk_New (Frame);
      Frame.Set_Border_Width (5);
      Add (Pane, Frame);

      Gtk_New (Pane.Label_Box);
      Add_Events (Pane.Label_Box, Button_Release_Mask);
      Frame.Set_Label_Widget (Pane.Label_Box);

      Gtk_New_Hbox (Label_Hbox, Homogeneous => False);
      Add (Pane.Label_Box, Label_Hbox);
      Gtk.Arrow.Gtk_New (Pane.Arrow, Arrow_Right, Shadow_None);
      Pack_Start (Label_Hbox, Pane.Arrow, False, False, 1);
      Gtk_New (Pane.Label, Label);
      Pack_Start
        (Label_Hbox, Pane.Label, Padding => 3, Expand => False, Fill => False);

      Return_Callback.Object_Connect
        (Pane.Label_Box,
         Signal_Button_Release_Event, On_Change_State'Access, Pane);

      Gtk_New_Vbox (Pane.Main_Box, Homogeneous => False);
      Add (Frame, Pane.Main_Box);
      Pane.Main_Box.Set_Border_Width (3);

      Set_State (Pane, Collapsed);

      Widget_Callback.Connect
        (Pane, Signal_Destroy, On_Destroy'Access);
   end Initialize;

   -------------------------
   -- Set_Expanded_Widget --
   -------------------------

   procedure Set_Expanded_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      if Pane.Expanded_Box /= null then
         Pane.Expanded_Box.Unref;
      end if;

      Widget.Ref;
      Pane.Expanded_Box := Gtk_Widget (Widget);

      Pane.Set_State (Pane.State);
   end Set_Expanded_Widget;

   --------------------------
   -- Set_Collapsed_Widget --
   --------------------------

   procedure Set_Collapsed_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      if Pane.Collapsed_Box /= null then
         Pane.Collapsed_Box.Unref;
      end if;

      Widget.Ref;
      Pane.Collapsed_Box := Gtk_Widget (Widget);

      Pane.Set_State (Pane.State);
   end Set_Collapsed_Widget;

   ----------------------
   -- Set_Child_Widget --
   ----------------------

   procedure Set_Child_Widget
     (Pane : access Collapsing_Pane_Record'Class; Widget : Gtk_Widget)
   is
      Child : Gtk_Widget;
   begin
      Child := Pane.Main_Box.Get_Child (0);

      if Child = Widget then
         return;
      end if;

      if Child /= null then
         Pane.Main_Box.Remove (Child);
      end if;

      if Widget /= null then
         Pane.Main_Box.Pack_Start (Widget, False, False);
         Widget.Show_All;
      end if;
   end Set_Child_Widget;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Pane : access Collapsing_Pane_Record'Class; State : Foldable_Box_State)
   is
   begin
      Pane.State := State;

      --  We need to explicitely hide the notebook, otherwise the size is
      --  incorrectly recomputed by gtk+. Looks like a bug to me..

      if State = Expanded then
         Set_Child_Widget (Pane, Pane.Expanded_Box);
         Pane.Arrow.Set
           (Arrow_Down, Shadow_None);
      else
         Set_Child_Widget (Pane, Pane.Collapsed_Box);
         Pane.Arrow.Set
           (Arrow_Right, Shadow_None);
      end if;

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

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Object : access Gtk_Widget_Record'Class)
   is
      Pane : constant Collapsing_Pane := Collapsing_Pane (Object);
   begin
      if Pane.Expanded_Box /= null then
         Pane.Expanded_Box.Unref;
         Pane.Expanded_Box := null;
      end if;

      if Pane.Collapsed_Box /= null then
         Pane.Collapsed_Box.Unref;
         Pane.Collapsed_Box := null;
      end if;
   end On_Destroy;

end Collapsing_Pane;

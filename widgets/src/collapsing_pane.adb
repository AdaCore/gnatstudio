-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2006-2011, AdaCore                --
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
with Gdk.Event;  use Gdk.Event;
with Gdk.Window; use Gdk.Window;

with Gtk.Arrow;     use Gtk.Arrow;
with Gtk.Box;       use Gtk.Box;
with Gtk.Event_Box; use Gtk.Event_Box;
with Gtk.Frame;     use Gtk.Frame;
with Gtk.Label;     use Gtk.Label;
with Gtk.Object;    use Gtk.Object;
with Gtk.Widget;    use Gtk.Widget;

with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Enums; use Gtk.Enums;

package body Collapsing_Pane is

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
      Gtk.Event_Box.Initialize (Pane);
      Glib.Object.Initialize_Class_Record
        (Pane, Signals, Class_Record, "CollapsingPane",
         Signal_Parameters);

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
         Pane.Arrow.Set (Arrow_Right, Shadow_None);

      else
         Set_Child_Visible (Pane.Expanded_Box, True);
         Set_Size_Request (Pane.Expanded_Box, -1, -1);
         Set_Child_Visible (Pane.Collapsed_Box, False);
         Set_Size_Request (Pane.Collapsed_Box, 0, 0);
         Pane.Arrow.Set (Arrow_Down, Shadow_None);
      end if;

      Pane.State := State;

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

end Collapsing_Pane;

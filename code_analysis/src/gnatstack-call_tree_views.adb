-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Interfaces.C.Strings;

with Glib.Object;
with Gdk.Event;
with Gtk.Cell_Renderer_Text;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with GPS.Intl;
with Traces;

package body GNATStack.Call_Tree_Views is

   use GPS.Intl;

   function On_Button_Press
     (Self  : access Call_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handles double mouse click events.

   package Call_Tree_View_Callbacks is
     new Gtk.Handlers.Callback (Call_Tree_View_Record);

   package Call_Tree_View_Boolean_Callbacks is
     new Gtk.Handlers.Return_Callback (Call_Tree_View_Record, Boolean);

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array (1 .. 1) :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Double_Clicked)));

   Signals_Parameters : constant
     Glib.Object.Signal_Parameter_Types (1 .. 1, 1 .. 1) :=
       (1 => (others => Glib.GType_None));

   --------------------
   -- Double_Clicked --
   --------------------

   procedure Double_Clicked
     (Self : not null access Call_Tree_View_Record'Class) is
   begin
      Call_Tree_View_Callbacks.Emit_By_Name (Self, Signal_Double_Clicked);
   end Double_Clicked;

   -----------------------------
   -- Get_Selected_Subprogram --
   -----------------------------

   function Get_Selected_Subprogram
     (Self : not null access Call_Tree_View_Record'Class)
      return GNATStack.Data_Model.Subprogram_Information_Access
   is
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Self.View.Get_Selection.Get_Selected (Model, Iter);

      return Self.Model.Subprogram_At (Iter);
   end Get_Selected_Subprogram;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item       : out Call_Tree_View;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access) is
   begin
      Item := new Call_Tree_View_Record;
      Initialize (Item, Subprogram);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access Call_Tree_View_Record'Class;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access)
   is
      Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy    : Glib.Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk.Box.Initialize_Hbox (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "GNATStackCallTreeView",
         Signals_Parameters);

      GNATStack.Call_Tree_Models.Gtk_New (Self.Model, Subprogram);
      Gtk.Tree_View.Gtk_New (Self.View, Self.Model);
      Call_Tree_View_Boolean_Callbacks.Object_Connect
        (Self.View,
         Gtk.Widget.Signal_Button_Press_Event,
         Call_Tree_View_Boolean_Callbacks.To_Marshaller
           (On_Button_Press'Access),
         Self,
         False);
      Self.Model.Unref;
      Self.Pack_Start (Self.View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Subprogram");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", 0);
      Dummy := Self.View.Append_Column (Column);
   end Initialize;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self  : access Call_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      use type Glib.Guint;
      use type Gdk.Event.Gdk_Event_Type;

   begin
      if Gdk.Event.Get_Button (Event) = 1
        and then Gdk.Event.Get_Event_Type (Event) = Gdk.Event.Gdk_2button_Press
      then
         Self.Double_Clicked;

         return True;

      else
         return False;
      end if;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);

         return False;
   end On_Button_Press;

end GNATStack.Call_Tree_Views;

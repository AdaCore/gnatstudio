--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Variable_Editors; use Variable_Editors;

package body New_Variable_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   --------------------------------
   -- On_Get_Environment_Toggled --
   --------------------------------

   procedure On_Get_Environment_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
   begin
      Set_Sensitive
        (E.Environment_Table, Get_Active (E.Get_Environment));
      Set_Sensitive
        (E.Untyped_List_Variable, not Get_Active (E.Get_Environment));
      Set_Sensitive
        (E.Untyped_Single_Variable, not Get_Active (E.Get_Environment));
      if Get_Active (E.Get_Environment) then
         Set_Active (E.Typed_Variable, True);
      end if;
   end On_Get_Environment_Toggled;

   -------------------------------
   -- On_Typed_Variable_Toggled --
   -------------------------------

   procedure On_Typed_Variable_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
   begin
      Set_Sensitive (E.List_Value, Get_Active (E.Untyped_List_Variable));
      Set_Sensitive (E.Single_Value, Get_Active (E.Untyped_Single_Variable));
      Set_Sensitive (E.Enumeration_Value, Get_Active (E.Typed_Variable));
   end On_Typed_Variable_Toggled;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
   begin
      Destroy (E);
   end On_Cancel_Clicked;

end New_Variable_Editor_Pkg.Callbacks;

-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Glib;       use Glib;
with Gtk.Widget; use Gtk.Widget;
with Gtk.List;   use Gtk.List;
with Gtk.List_Item;  use Gtk.List_Item;
with Variable_Editors; use Variable_Editors;

package body New_Variable_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   ------------------------------
   -- On_Variable_Name_Changed --
   ------------------------------

   procedure On_Variable_Name_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Name_Changed (New_Var_Edit (Object));
   end On_Variable_Name_Changed;

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

   ------------------------------------
   -- On_Env_Must_Be_Defined_Toggled --
   ------------------------------------

   procedure On_Env_Must_Be_Defined_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
      Must : constant Boolean := Get_Active (E.Env_Must_Be_Defined);
   begin
      Set_Sensitive (E.Default_Value_Label, not Must);
      Set_Sensitive (E.Default_Env_Variable, not Must);
   end On_Env_Must_Be_Defined_Toggled;

   -------------------------------
   -- On_Typed_Variable_Toggled --
   -------------------------------

   procedure On_Typed_Variable_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
   begin
      Set_Sensitive (E.List_Value,
                     Get_Active (E.Untyped_List_Variable)
                     and then not Get_Active (E.Get_Environment));
      Set_Sensitive (E.Single_Value,
                     Get_Active (E.Untyped_Single_Variable)
                     and then not Get_Active (E.Get_Environment));
      Set_Sensitive (E.Enumeration_Value, Get_Active (E.Typed_Variable));
   end On_Typed_Variable_Toggled;

   ----------------------------------
   -- On_Enumeration_Value_Changed --
   ----------------------------------

   procedure On_Enumeration_Value_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
      List : Gtk_List := Get_List (E.Default_Env_Variable);
      Text : constant String := Get_Chars (E.Enumeration_Value);
      Label : Gtk_List_Item;
      Index : Natural := Text'First;
      Index_End : Natural;
   begin
      Clear_Items (List, 0, -1);

      while Index <= Text'Last loop
         Index_End := Index;
         while Index_End <= Text'Last
           and then Text (Index_End) /= ASCII.LF
         loop
            Index_End := Index_End + 1;
         end loop;

         if Index_End > Index then
            Gtk_New (Label, Text (Index .. Index_End - 1));
            Add (List, Label);
            Show (Label);
         end if;
         Index := Index_End + 1;
      end loop;
   end On_Enumeration_Value_Changed;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      E : New_Var_Edit := New_Var_Edit (Object);
   begin
      Update_Variable (E);
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

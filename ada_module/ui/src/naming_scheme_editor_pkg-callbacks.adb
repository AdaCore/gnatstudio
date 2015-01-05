------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada_Naming_Editors; use Ada_Naming_Editors;

with Gdk.Event;          use Gdk.Event;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Gdk.Types;          use Gdk.Types;

with Glib;               use Glib;

with Gtk.Tree_Model;     use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Store;     use Gtk.Tree_Store;
with Gtk.Tree_View;      use Gtk.Tree_View;

package body Naming_Scheme_Editor_Pkg.Callbacks is

   Custom_Scheme : constant Gint := 3;
   --  Index of the "<custom>" choice in the list of predefined schemes.

   procedure Handle_Key
     (E : Naming_Scheme_Editor_Access; Event : Gdk_Event; Field : Gtk_Entry);
   --  Handle a keypress (escape,...) in Field

   use Gtk.Arguments;

   --------------------------------
   -- On_Standard_Scheme_Changed --
   --------------------------------

   procedure On_Standard_Scheme_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      E     : constant Naming_Scheme_Editor_Access :=
                Naming_Scheme_Editor_Access (Object);
      Value : Gint;

   begin
      if Get_Active_Iter (E.Standard_Scheme) /= Null_Iter then
         Value := Get_Active (E.Standard_Scheme);

         if Value /= Custom_Scheme then
            Set_Predefined_Scheme (E, Natural (Value));

            --  Restore the contents of the standard scheme buttons, that has
            --  been changed through callbacks when the changed the contents of
            --  the GUI.

            Set_Active (E.Standard_Scheme, Value);
         end if;
      end if;
   end On_Standard_Scheme_Changed;

   ----------------
   -- Customized --
   ----------------

   procedure Customized
     (Object : access Gtk_Widget_Record'Class)
   is
      E : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
   begin
      Set_Active (E.Standard_Scheme, Custom_Scheme);
   end Customized;

   -----------------------------------
   -- On_Exceptions_List_Select_Row --
   -----------------------------------

   procedure On_Exceptions_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);
      E         : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      Get_Selected (Get_Selection (E.Exception_List), Model, Iter);

      if Iter /= Null_Iter then
         Set_Text
           (E.Unit_Name_Entry, Get_String (E.Exception_List_Model, Iter, 0));

         declare
            Spec_Name : constant String := Get_String
              (E.Exception_List_Model, Iter, 1);
            Body_Name : constant String := Get_String
              (E.Exception_List_Model, Iter, 2);
         begin
            if Spec_Name = "" then
               Reset_Exception_Fields (E, E.Spec_Filename_Entry);
            else
               Set_Text (E.Spec_Filename_Entry, Spec_Name);
            end if;

            if Body_Name = "" then
               Reset_Exception_Fields (E, E.Body_Filename_Entry);
            else
               Set_Text (E.Body_Filename_Entry, Body_Name);
            end if;
         end;
      else
         Reset_Exception_Fields (E);
      end if;
   end On_Exceptions_List_Select_Row;

   ---------------------------------------
   -- On_Exception_List_Key_Press_Event --
   ---------------------------------------

   function On_Exception_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Event : constant Gdk_Event := To_Event (Params, 1);
      E     : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;

   begin
      Get_Selected (Get_Selection (E.Exception_List), Model, Iter);

      if Iter /= Null_Iter
        and then Get_Key_Val (Event) = GDK_Delete
      then
         Remove (E.Exception_List_Model, Iter);
         return True;
      end if;
      return False;
   end On_Exception_List_Key_Press_Event;

   -----------------------
   -- On_Update_Clicked --
   -----------------------

   procedure On_Update_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Add_New_Exception (Naming_Scheme_Editor_Access (Object));
   end On_Update_Clicked;

   ----------------
   -- Handle_Key --
   ----------------

   procedure Handle_Key
     (E : Naming_Scheme_Editor_Access; Event : Gdk_Event; Field : Gtk_Entry) is
   begin
      if Get_Key_Val (Event) = GDK_Escape then
         Reset_Exception_Fields (E, Field);
      end if;
   end Handle_Key;

   ----------------------------------------
   -- On_Unit_Name_Entry_Key_Press_Event --
   ----------------------------------------

   function On_Unit_Name_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      E    : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
   begin
      Clear_Unit_Name (E);
      Handle_Key (E, Arg1, E.Unit_Name_Entry);
      return False;
   end On_Unit_Name_Entry_Key_Press_Event;

   --------------------------------------------
   -- On_Spec_Filename_Entry_Key_Press_Event --
   --------------------------------------------

   function On_Spec_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      E    : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
   begin
      Clear_Spec_Name (E);
      Handle_Key (E, Arg1, E.Spec_Filename_Entry);
      return False;
   end On_Spec_Filename_Entry_Key_Press_Event;

   --------------------------------------------
   -- On_Body_Filename_Entry_Key_Press_Event --
   --------------------------------------------

   function On_Body_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
      E    : constant Naming_Scheme_Editor_Access :=
        Naming_Scheme_Editor_Access (Object);
   begin
      Clear_Body_Name (E);
      Handle_Key (E, Arg1, E.Body_Filename_Entry);
      return False;
   end On_Body_Filename_Entry_Key_Press_Event;

end Naming_Scheme_Editor_Pkg.Callbacks;

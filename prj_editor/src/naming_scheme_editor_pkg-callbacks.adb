-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Combo;  use Gtk.Combo;
with Gtk.List;   use Gtk.List;
with Naming_Editors; use Naming_Editors;

package body Naming_Scheme_Editor_Pkg.Callbacks is

   use Gtk.Arguments;

   Custom_Scheme : constant Gint := 3;
   --  Index of the "<custom>" choice in the list of predefined schemes.

   procedure Handle_Key
     (E : Naming_Editor; Event : Gdk_Event; Field : Gtk_Entry);
   --  Handle a keypress (escape,...) in Field

   --------------------------------
   -- On_Standard_Scheme_Changed --
   --------------------------------

   procedure On_Standard_Scheme_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      E     : constant Naming_Editor := Naming_Editor (Object);
      List  : constant Gtk_List      := Get_List (E.Standard_Scheme);
      Value : Gint;

   begin
      if Get_Selection (List) /= Widget_List.Null_List then
         Value := Child_Position (List, Get_Data (Get_Selection (List)));

         if Value /= Custom_Scheme then
            Set_Predefined_Scheme (E, Natural (Value));

            --  Restore the contents of the standard scheme buttons, that has
            --  been changed through callbacks when the changed the contents of
            --  the GUI.

            Select_Item (Get_List (E.Standard_Scheme), Value);
         end if;
      end if;
   end On_Standard_Scheme_Changed;

   ----------------
   -- Customized --
   ----------------

   procedure Customized
     (Object : access Gtk_Widget_Record'Class)
   is
      E : Naming_Editor := Naming_Editor (Object);
   begin
      Select_Item (Get_List (E.Standard_Scheme), Custom_Scheme);
   end Customized;

   -----------------------------------
   -- On_Exceptions_List_Select_Row --
   -----------------------------------

   procedure On_Exceptions_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      E : Naming_Editor := Naming_Editor (Object);
      Row : Gint := To_Gint (Params, 1);
--        Column : Gint := To_Gint (Params, 2);
--        Arg3 : Gdk_Event := To_Event (Params, 3);
      Spec_Name : constant String := Get_Text (E.Exception_List, Row, 1);
      Body_Name : constant String := Get_Text (E.Exception_List, Row, 2);
   begin
      Set_Text (E.Unit_Name_Entry, Get_Text (E.Exception_List, Row, 0));
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
   end On_Exceptions_List_Select_Row;

   ---------------------------------------
   -- On_Exception_List_Key_Press_Event --
   ---------------------------------------

   function On_Exception_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Event : Gdk_Event := To_Event (Params, 1);
      use Gint_List;
      E : Naming_Editor := Naming_Editor (Object);
      List : constant Gint_List.Glist := Get_Selection (E.Exception_List);
   begin
      if Get_Key_Val (Event) = GDK_Delete then
         if List /= Null_List then
            Remove (E.Exception_List, Get_Data (List));
         end if;
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
      Add_New_Exception (Naming_Editor (Object));
   end On_Update_Clicked;

   ----------------
   -- Handle_Key --
   ----------------

   procedure Handle_Key
     (E : Naming_Editor; Event : Gdk_Event; Field : Gtk_Entry) is
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
      Arg1 : Gdk_Event := To_Event (Params, 1);
      E : Naming_Editor := Naming_Editor (Object);
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
      Arg1 : Gdk_Event := To_Event (Params, 1);
      E : Naming_Editor := Naming_Editor (Object);
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
      Arg1 : Gdk_Event := To_Event (Params, 1);
      E : Naming_Editor := Naming_Editor (Object);
   begin
      Clear_Body_Name (E);
      Handle_Key (E, Arg1, E.Body_Filename_Entry);
      return False;
   end On_Body_Filename_Entry_Key_Press_Event;

end Naming_Scheme_Editor_Pkg.Callbacks;

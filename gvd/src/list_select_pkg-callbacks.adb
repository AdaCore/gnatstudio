------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Gtk.Main;          use Gtk.Main;
with Gtkada.Dialogs;    use Gtkada.Dialogs;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Widget; use Gtk.Widget;

package body List_Select_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------
   -- On_Clist_Select_Row --
   -------------------------

   procedure On_Clist_Select_Row
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      pragma Unreferenced (Params);

      List        : constant Gtk_Widget := Gtk_Widget (Object);
      List_Select : constant List_Select_Access :=
        List_Select_Access (Get_Toplevel (List));
      S : constant Gtk_Tree_Selection := Get_Selection (List_Select.Tree_View);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      use Ada.Strings;
   begin
      Get_Selected (S, Model, Iter);
      Set_Text
        (List_Select.The_Entry, Trim (Get_String (Model, Iter, 0), Left));
   end On_Clist_Select_Row;

   ---------------------------
   -- On_Clist_Button_Press --
   ---------------------------

   function On_Clist_Button_Press
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Object);
   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Gtk.Main.Main_Quit;
      end if;

      return False;
   end On_Clist_Button_Press;

   ---------------------------
   -- On_The_Entry_Activate --
   ---------------------------

   procedure On_The_Entry_Activate (Object : access Gtk_Entry_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end On_The_Entry_Activate;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked (Object : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end On_Ok_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked (Object : access Gtk_Button_Record'Class) is
      List_Select : constant List_Select_Access :=
                      List_Select_Access (Get_Toplevel (Object));

   begin
      Set_Text (List_Select.The_Entry, "");
      Gtk.Main.Main_Quit;
   end On_Cancel_Clicked;

   ---------------------
   -- On_Help_Clicked --
   ---------------------

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Dummy       : Message_Dialog_Buttons;
      pragma Unreferenced (Dummy);

      List_Select : constant List_Select_Access :=
                      List_Select_Access (Get_Toplevel (Object));
   begin
      Dummy := Message_Dialog
        (List_Select.Help_Text.all,
         Buttons => Button_OK,
         Parent  => Gtk_Window (Object.Get_Toplevel));
   end On_Help_Clicked;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   function On_Delete_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean is
   begin
      Set_Text (List_Select_Access (Object).The_Entry, "");
      Gtk.Main.Main_Quit;

      --  The widget must not be destroyed here since it will still be accessed
      --  by the subprogram that created it.
      return True;
   end On_Delete_Event;

end List_Select_Pkg.Callbacks;

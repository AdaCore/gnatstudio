-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Object; use Gtk.Object;
with Gtk.Main; use Gtk.Main;
with Gtk.Label; use Gtk.Label;
with Gtk; use Gtk;
with Gtk.List; use Gtk.List;
with Open_Session_Pkg; use Open_Session_Pkg;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Type_Conversion;
pragma Warnings (Off, Gtk.Type_Conversion);

package body Open_Session_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------
   -- On_List_Selection_Changed --
   -------------------------------

   procedure On_List_Selection_Changed
     (Object : access Gtk_List_Record'Class)
   is
   begin
      null;
   end On_List_Selection_Changed;

   --------------------------
   -- On_List_Select_Child --
   --------------------------

   procedure On_List_Select_Child
     (Object : access Gtk_List_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gtk_Widget := Gtk_Widget (To_Object (Params, 1));
      use Widget_List;
   begin
      Set_Text (Open_Session_Access (Get_Toplevel (Object)).Entry1,
                Get (Gtk_Label
                     (Get_Data
                      (Children
                       (Gtk_Container (Arg1))))));
   end On_List_Select_Child;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Set_Text (Open_Session_Access (Get_Toplevel (Object)).Entry1, "");
      Main_Quit;
      Hide_All (Get_Toplevel (Object));
   end On_Cancel_Button_Clicked;

   ------------------------------
   -- On_Ok_Button_Clicked --
   ------------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
      Hide_All (Get_Toplevel (Object));
   end On_Ok_Button_Clicked;

   ----------------------------
   -- On_Help_Button_Clicked --
   ----------------------------

   procedure On_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Button_Clicked;

end Open_Session_Pkg.Callbacks;

-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                             ACT-Europe                            --
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


with Glib;            use Glib;
with Gtk.Main;        use Gtk.Main;
with Gtkada.Dialogs;  use Gtkada.Dialogs;

package body List_Select_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------
   -- On_Clist_Select_Row --
   -------------------------

   procedure On_Clist_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      List_Select : List_Select_Access
        := List_Select_Access (Get_Toplevel (Object));
      Arg1 : Gint := To_Gint (Params, 1);
   begin
      Set_Text (List_Select.The_Entry,
                Get_Text (Object, Arg1, 0));
   end On_Clist_Select_Row;

   ---------------------------
   -- On_The_Entry_Activate --
   ---------------------------

   procedure On_The_Entry_Activate
     (Object : access Gtk_Entry_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_The_Entry_Activate;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Ok_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      List_Select : List_Select_Access
        := List_Select_Access (Get_Toplevel (Object));
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
      List_Select : List_Select_Access
        := List_Select_Access (Get_Toplevel (Object));
   begin
      Dummy := Message_Dialog
        (List_Select.Help_Text.all,
         Buttons => Button_OK);
   end On_Help_Clicked;

end List_Select_Pkg.Callbacks;

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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.List;  use Gtk.List;

with Ada.Text_IO; use Ada.Text_IO;

package body Print_Dialog_Pkg.Callbacks is

   -----------------------------
   -- On_Print_Button_Clicked --
   -----------------------------

   procedure On_Print_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog : Print_Dialog_Access := Print_Dialog_Access (Object);
      Label  : Gtk_List_Item;
   begin
      Dialog.Variable := new String' (Get_Text (Dialog.Combo_Entry1));
      Gtk_New (Label, Get_Text (Dialog.Combo_Entry1));
      Show (Label);
      Add (Get_List (Dialog.Combo1), Label);
      Main_Quit;
   end On_Print_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog : Print_Dialog_Access := Print_Dialog_Access (Object);
   begin
      Free (Dialog.Variable);
      Main_Quit;
   end On_Cancel_Button_Clicked;

end Print_Dialog_Pkg.Callbacks;

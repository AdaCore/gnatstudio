-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
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

with Gtk.Main; use Gtk.Main;
with Gtkada.File_Selection; use Gtkada.File_Selection;
with GVD.Strings; use GVD.Strings;

package body Open_Program_Pkg.Callbacks is

   -----------------------------
   -- On_Radio_Button_Toggled --
   -----------------------------

   procedure On_Radio_Button_Toggled
     (Object : access Gtk_Radio_Button_Record'Class)
   is
   begin
      null;
   end On_Radio_Button_Toggled;

   ----------------------------
   -- On_Open_Button_Clicked --
   ----------------------------

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      S : constant String := To_Unix_Pathname (File_Selection_Dialog);
   begin
      if S /= "" then
         Set_Text
           (Get_Entry
             (Open_Program_Access (Get_Toplevel (Object)).Program_Combo), S);
      end if;
   end On_Open_Button_Clicked;

   ------------------------
   -- On_Ok_Open_Clicked --
   ------------------------

   procedure On_Ok_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Open_Program_Access (Get_Toplevel (Object)).Valid := True;
      Main_Quit;
   end On_Ok_Open_Clicked;

   ----------------------------
   -- On_Cancel_Open_Clicked --
   ----------------------------

   procedure On_Cancel_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Open_Program_Access (Get_Toplevel (Object)).Valid := False;
      Main_Quit;
   end On_Cancel_Open_Clicked;

end Open_Program_Pkg.Callbacks;

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

with Gtk.Main; use Gtk.Main;
with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;

package body Breakpoints_Pkg.Callbacks is

   ----------------------------
   -- On_Advanced_Bp_Clicked --
   ----------------------------

   procedure On_Advanced_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Descriptor : Advanced_Breakpoint_Descriptor;
   begin
      Advanced_Breakpoint_Editor (Descriptor);
      Free (Descriptor);
   end On_Advanced_Bp_Clicked;

   -----------------------
   -- On_Add_Bp_Clicked --
   -----------------------

   procedure On_Add_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Add_Bp_Clicked;

   --------------------------
   -- On_Remove_Bp_Clicked --
   --------------------------

   procedure On_Remove_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Remove_Bp_Clicked;

   ------------------------------
   -- On_Remove_All_Bp_Clicked --
   ------------------------------

   procedure On_Remove_All_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Remove_All_Bp_Clicked;

   ----------------------
   -- On_Ok_Bp_Clicked --
   ----------------------

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
   end On_Ok_Bp_Clicked;

   --------------------------
   -- On_Cancel_Bp_Clicked --
   --------------------------

   procedure On_Cancel_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
   end On_Cancel_Bp_Clicked;

end Breakpoints_Pkg.Callbacks;

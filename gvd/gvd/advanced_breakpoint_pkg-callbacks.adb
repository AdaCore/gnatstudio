-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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

package body Advanced_Breakpoint_Pkg.Callbacks is

   -----------------------------
   -- On_Start_Record_Clicked --
   -----------------------------

   procedure On_Start_Record_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Start_Record_Clicked;

   ----------------------------
   -- On_Stop_Record_Clicked --
   ----------------------------

   procedure On_Stop_Record_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      null;
   end On_Stop_Record_Clicked;

end Advanced_Breakpoint_Pkg.Callbacks;

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

package Advanced_Breakpoint_Pkg.Callbacks is
   procedure On_Start_Record_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Stop_Record_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Ok_Advanced_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

   procedure On_Cancel_Advanced_Bp_Clicked
     (Object : access Gtk_Button_Record'Class);

end Advanced_Breakpoint_Pkg.Callbacks;

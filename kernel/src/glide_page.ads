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

with GVD.Process;
with Glide_Consoles;
with Glide_Main_Window; use Glide_Main_Window;

package Glide_Page is

   type Glide_Page_Record is new GVD.Process.Debugger_Process_Tab_Record with
   record
      Console    : Glide_Consoles.Glide_Console;
   end record;
   type Glide_Page is access all Glide_Page_Record'Class;

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class);

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class);

end Glide_Page;

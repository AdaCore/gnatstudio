-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GVD.Process;
with Glide_Consoles;
with Glide_Interactive_Consoles;
with Glide_Main_Window; use Glide_Main_Window;
with Glide_Result_View; use Glide_Result_View;

package Glide_Page is

   type Glide_Page_Record is new
     GVD.Process.Debugger_Process_Tab_Record with
   record
      Console : Glide_Consoles.Glide_Console;
      Interactive_Console :
         Glide_Interactive_Consoles.Glide_Interactive_Console;
      Results : Glide_Result_View.Result_View;
   end record;
   type Glide_Page is access all Glide_Page_Record'Class;

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class);

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class);

   procedure Set_Busy
     (Page          : access Glide_Page_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);

   procedure Load_Desktop (Window : access Glide_Window_Record'Class);
   --  Load a saved desktop, if any, and create the console if needed.

end Glide_Page;

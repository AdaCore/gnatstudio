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

with Gtk; use Gtk;
with Gtk.Main;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Gtkada.Intl; use Gtkada.Intl;
with Gtk.Window;  use Gtk.Window;
with GNAT.Os_Lib; use GNAT.Os_Lib;
with Odd.Process; use Odd.Process;

procedure Odd_Main is
   Process_Tab : Debugger_Process_Tab;
   List : Argument_List (1 .. 0);
begin
   Bind_Text_Domain ("GtkAda", "/usr/local/share/locale");
   Bind_Text_Domain ("Odd", "/usr/local/share/locale");
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Debug_Window);
   Process_Tab := Create_Debugger (List, "");
   Show_All (Main_Debug_Window);
   Gtk.Main.Main;
end Odd_Main;

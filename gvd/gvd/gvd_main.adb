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

with Glib;
with Gdk.Types; use Gdk.Types;
with Gtk; use Gtk;
with Gtk.Main;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Gtkada.Intl; use Gtkada.Intl;
with Odd.Process; use Odd.Process;
with Debugger;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Odd_Main is
   Process_Tab       : Debugger_Process_Tab;
   List              : Argument_List (1 .. Argument_Count);
   Main_Debug_Window : Main_Debug_Window_Access;
   Id                : Glib.Gint;
   Index             : Natural := 0;

begin
   Bind_Text_Domain ("GtkAda", "/usr/local/share/locale");
   Bind_Text_Domain ("Odd", "/usr/local/share/locale");
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Debug_Window);

   for J in 1 .. Argument_Count loop
      if Argument (J) = "--tty" then
         --  Install input handler to receive commands from an external
         --  IDE while handling GtkAda events.

         Id := Standard_Input_Package.Add
           (0, Input_Read, Input_Available'Access,
            Main_Debug_Window.all'Access);

      else
         Index := Index + 1;
         List (Index) := new String' (Argument (J));
      end if;
   end loop;

   --  ??? Need to add command line parsing to handle other debuggers by
   --  default.
   Process_Tab := Create_Debugger
     (Main_Debug_Window, Debugger.Gdb_Type, "", List (1 .. Index));
   Show_All (Main_Debug_Window);
   Gtk.Main.Main;
exception
   when E : others =>
      Put_Line ("Bug detected in odd");
      Put_Line ("Exception Information: " & Exception_Information (E));
end Odd_Main;

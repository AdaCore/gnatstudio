-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Diff_Utils; use Diff_Utils;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Label; use Gtk.Label;
with Vdiff_Pkg; use Vdiff_Pkg;
with Vdiff_Utils; use Vdiff_Utils;

procedure Vdiff is
   Vdiff  : Vdiff_Access;
   Result : Diff_Occurrence_Link;

begin
   if Argument_Count /= 2 then
      Put_Line ("incorrect number of parameters. exiting.");
      return;
   end if;

   Result := Diff (Argument (1), Argument (2));
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Vdiff);
   Set_Text (Vdiff.File_Label1, Argument (1));
   Set_Text (Vdiff.File_Label2, Argument (2));

   Fill_Diff_Lists
     (Vdiff.Clist1, Vdiff.Clist2,
      Argument (1), Argument (2),
      Result);
   Show_All (Vdiff);
   Gtk.Main.Main;

   Free (Result);
end Vdiff;

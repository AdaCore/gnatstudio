-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk; use Gtk;
with Gtk.Main;

with Hyper_Grep; use Hyper_Grep;

with Language;      use Language;
with Language.Ada;  use Language.Ada;
with Language.C;    use Language.C;
with Language.Cpp;  use Language.Cpp;
with Language.Java; use Language.Java;

procedure Hypergrep_Main is
   HG_Window : Hyper_Grep_Access;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   --  Be careful with *.h files: both C and C++ !

   Add_File_Extensions (Ada_Lang,  ".ads;.adb");
   Add_File_Extensions (C_Lang,    ".c;.h");
   Add_File_Extensions (Cpp_Lang,  ".cxx;.cpp;.h");
   Add_File_Extensions (Java_Lang, ".java");

   Gtk_New (HG_Window, null);
   Show_All (HG_Window);

   Gtk.Main.Main;
end Hypergrep_Main;

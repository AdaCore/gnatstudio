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

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Regpat;       use GNAT.Regpat;
with Glide_Main_Window; use Glide_Main_Window;
with Glide_Page;        use Glide_Page;
with Glide_Consoles;    use Glide_Consoles;
with Gdk.Color;         use Gdk.Color;
with Gtk.Text;          use Gtk.Text;
with Gtk.Widget;        use Gtk.Widget;
with GVD.Process;       use GVD.Process;

package body Glide_Kernel.Console is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True)
   is
      Top       : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Console   : constant Glide_Console :=
        Glide_Page.Glide_Page (Get_Current_Process (Top)).Console;
   begin
      Insert (Console, Text, Highlight_Sloc, Add_LF);
   end Insert;

end Glide_Kernel.Console;

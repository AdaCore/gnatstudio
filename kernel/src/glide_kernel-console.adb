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

with Glide_Main_Window; use Glide_Main_Window;
with Glide_Page;        use Glide_Page;
with Glide_Consoles;    use Glide_Consoles;
with GVD.Process;       use GVD.Process;
with Ada.Text_IO;       use Ada.Text_IO;

package body Glide_Kernel.Console is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel         : access Kernel_Handle_Record'Class;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info)
   is
      Top       : constant Glide_Window := Glide_Window (Kernel.Main_Window);
      Console   : Glide_Console;
   begin
      if Top = null
        or else Get_Current_Process (Top) = null
      then
         Put_Line (Text);
      else
         Console := Glide_Page.Glide_Page (Get_Current_Process (Top)).Console;
         Insert (Console, Text, Highlight_Sloc, Add_LF, Mode);
      end if;
   end Insert;

end Glide_Kernel.Console;

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

with Glib; use Glib;

package Odd is

   ---------------
   -- Constants --
   ---------------

   Version : constant String := "0.5.2";
   --  Version number of this software.

   Editor_Font_Size : constant Gint := 12;
   --  Size of the font used in the editor.

   Editor_Font : constant String := "Courier";
   --  Font used in the editor.

   Comments_Color : constant String := "#FF0000";
   --  Color used for comments.

   Strings_Color  : constant String := "#A52A2A";
   --  Color used for strings (brown).

   Keywords_Color : constant String := "#0000FF";
   --  Color used for keywords (blue).

   Debugger_Highlight_Color : constant String := "#0000FF";
   --  Color used for highlighting in the debugger window (blue).

   Debugger_Font_Size : constant Gint := 12;
   --  Size of the font used in the debugger text window.

   Debugger_Font : constant String := "Courier";
   --  Font used in the debugger text window.

   Editor_Show_Line_Nums : constant Boolean := True;
   --  Whether line numbers should be shown in the code editor

   Editor_Show_Line_With_Code : constant Boolean := False;
   --  Whether dots should be shown in the code editor for lines that
   --  contain code.

   Max_Frame : constant := 500;
   --  Maximum number of frames displayed in the backtrace window.

   Align_Items_On_Grid : constant Boolean := True;
   --  Should items be aligned on the grid.

   Max_Column_Width : constant := 200;
   --  Maximum width (in pixels) for a column in a list.

end Odd;

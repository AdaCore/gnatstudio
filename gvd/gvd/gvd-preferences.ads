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

package Odd.Preferences is

   -------------------------
   -- General Preferences --
   -------------------------

   Hide_Delay : constant Guint32 := 5000;
   --  Delay (in ms) after which the current message is hidden

   Use_Ptys : constant Boolean := True;
   --  True if we should connect to the debugger through pseudo-terminals
   --  (at least on Unix machines). False if we should use pipes.
   --  Although the latter is more portable, and should be used for machines
   --  that don't support ptys, we recommend using the former since this
   --  provides a better support for remote debuggers and the "interrupt"
   --  button, among other things.

   ---------------------
   -- Explorer Window --
   ---------------------

   Display_Explorer : constant Boolean := True;
   --  True if we should associate an explorer tree to each editor.

   File_Name_Bg_Color : constant String := "#BEBEBE";
   --  Color used for the background of the file name in the editor (grey).

   -------------------
   -- Source Window --
   -------------------

   Editor_Font : constant String := "Courier";
   --  Font used in the editor.

   Editor_Font_Size : constant Gint := 12;
   --  Size of the font used in the editor.

   Editor_Show_Line_Nums : constant Boolean := True;
   --  Whether line numbers should be shown in the code editor

   Editor_Show_Line_With_Code : constant Boolean := True;
   --  Whether dots should be shown in the code editor for lines that
   --  contain code.

   Do_Color_Highlighting : constant Boolean := True;
   --  Indicate whether the editor should provide color highlighting.

   Comments_Color : constant String := "#FF0000";
   --  Color used for comments.

   Strings_Color  : constant String := "#A52A2A";
   --  Color used for strings (brown).

   Keywords_Color : constant String := "#0000FF";
   --  Color used for keywords (blue).

   Editor_Highlight_Current_Line : constant Boolean := True;
   --  If True, the current line is displayed with a background color, in
   --  addition to the arrow that indicates the current line

   Editor_Highlight_Color : constant String := "#00CC00";
   --  The color to use to highlight the current line in the editor

   Tab_Size : constant Natural := 8;
   --  Horizontal Tab size.
   --  Please note : the implemented tab behaviour is to jump at the next
   --  column with a number equal to a multiple of Tab_Size.

   type Tooltips_In_Source_Type is (None, Simple, Full);
   --  The types of tooltips that can be displayed in the source window:
   --    None: no tooltips will be displayed.
   --    Simple: the output of gdb is displayed, no post-processing
   --    Full: the variable is parsed and the tooltip will contain the
   --     equivalent of the canvas'items.

   Tooltips_In_Source : constant Tooltips_In_Source_Type := Simple;
   --  What kind of tooltips we want in the source window

   ---------------------
   -- Assembly Window --
   ---------------------

   Asm_Highlight_Color : constant String := "#FF0000";
   --  Color to use to highlight the assembly code for the current line (red)

   -----------------
   -- Data Window --
   -----------------

   Xref_Color : constant String := "#0000FF";
   --  Color to use for the items that are clickable (blue).

   Title_Color : constant String := "#BEBEBE";
   --  Color to use for the background of the title (grey).

   Change_Color : constant String := "#FF0000";
   --  Color used to highlight fields that have changed since the last update
   --  (red).

   Thaw_Bg_Color : constant String := "#FFFFFF";
   --  Color used for auto-refreshed items (white)

   Freeze_Bg_Color : constant String := "#AAAAAA";
   --  Color used for frozen items (light grey)

   Look_3d : constant Boolean := True;
   --  Should the items have a 3d look ?

   Title_Font_Name : constant String := "Helvetica-Bold";
   --  Font used for the name of the item.

   Title_Font_Size : constant Gint := 10;
   --  Size of the font used for the name of the item.

   Value_Font_Name : constant String := "Helvetica";
   --  Font used to display the value of the item.

   Command_Font_Name : constant String := "Courier";
   --  Font used to display the value for the commands
   --    graph print `...`  or graph display `...`

   Type_Font_Name : constant String := "Helvetica-Oblique";
   --  Font used to display the type of the item.

   Value_Font_Size : constant Gint := 10;
   --  Size of the font used to display the value of the item.

   Type_Font_Size : constant Gint := 10;
   --  Size of the font used to display the type of the item.

   Hide_Big_Items : constant Boolean := True;
   --  If True, items higher than a given limit will start in a hidden state.

   Big_Item_Height : constant Glib.Gint := 150;
   --  Items taller than this value will start hidden.

   Default_Detect_Aliases : constant Boolean := True;
   --  If True, do not create new items when a matching item is already
   --  present in the canvas.

   Display_Grid : constant Boolean := True;
   --  Whether the grid should be displayed in the canvas.

   Align_Items_On_Grid : constant Boolean := True;
   --  Should items be aligned on the grid.

   --------------------
   -- Command Window --
   --------------------

   Debugger_Highlight_Color : constant String := "#0000FF";
   --  Color used for highlighting in the debugger window (blue).

   Debugger_Font : constant String := "Courier";
   --  Font used in the debugger text window.

   Debugger_Font_Size : constant Gint := 12;
   --  Size of the font used in the debugger text window.

   -------------------
   -- Memory Window --
   -------------------

   Memory_View_Font_Name    : constant String := "Courier";
   --  Font use in the memory view window.

   Memory_View_Font_Size    : constant Gint := 12;
   --  Size of the font used in the memory view window.

   Memory_View_Color        : constant String := "#333399";
   --  Color used by default in the memory view window.

   Memory_Highlighted_Color : constant String := "#DDDDDD";
   --  Color used for highlighted items in the memory view.

   Memory_Selected_Color    : constant String := "#00009c";
   --  Color used for selected items in the memory view.

   Memory_Modified_Color    : constant String := "#FF0000";
   --  Color used for modified items in the memory view.

   -------------
   -- Helpers --
   -------------

   List_Processes : constant String :=
     "ps x 2> /dev/null || ps -ef 2> /dev/null || ps";

   External_Editor : constant String := "glide %f -emacs +%l";
   --  External editor to use.
   --  %f is replaced by the full path name for the file to edit.
   --  %l is the line number to show in the editor.

end Odd.Preferences;

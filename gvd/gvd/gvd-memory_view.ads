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

with Glib;                  use Glib;
with Gdk.Window;            use Gdk.Window;
with Gtk.Widget;            use Gtk.Widget;
with Memory_View_Pkg;       use Memory_View_Pkg;
with Odd.Types;             use Odd.Types;

package Odd.Memory_View is

   type Display_Type is (Hex, Decimal, Octal, Text);

   type Data_Size is (Byte, Halfword, Word);

   type Odd_Memory_View_Record is new Memory_View_Record with record
      Window : Gtk_Widget;
      --  The associated main window;

      Display : Display_Type := Hex;
      --  The current display mode.

      Data : Data_Size := Byte;
      --  The size of data to display;

      Starting_Address : Long_Long_Integer;
      --  The first address that is being explored.

      Values : String_Access;
      --  The values that are to be shown in the window.
      --  This is a string of hexadecimal digits.

      Flags : String_Access;
      --  A string the same size as Values used to set markers on the values.

      Number_Of_Bytes : Integer := 256;
      --  The size of the pages that are currently stored.

      Number_Of_Columns : Integer := 16;
      --  The number of columns that are to be displayed.

      Selection_Start : Integer;
      Selection_End   : Integer;
      --  These numbers refer to indexes in Values pointing to the bytes that
      --  are currently selected.

      Unit_Size : Integer;
      --  The size, in number of elements from Values, of the current
      --  grouping unit (ie 2 for Bytes, 4 for Halfword, 8 for Word....)

      Trunc : Integer;
      --  The size of a separate element in the view (ie 2 for a Byte displayed
      --  in Hex, 3 for a Byte displayed in Decimal ...)

      Cursor_Position : Gint;
      --  Locates the cursor position within the view.

      Cursor_Index : Integer;
      --  Locates the cursor position within the values array;
   end record;

   type Odd_Memory_View is access all Odd_Memory_View_Record'Class;

   function Create_Memory_View (Window : Gtk_Widget) return Odd_Memory_View;
   --  Create a new memory view.

   procedure Display_Memory
     (View : Odd_Memory_View; Address : Long_Long_Integer);
   --  Display the contents of the memory into the text area.

   procedure Display_Memory (View : Odd_Memory_View; Address : String);
   --  Display the contents of the memory into the text area.
   --  Address is a string that represents an address in hexadecimal,
   --  it should be made of the "0x" prefix followed by hexadecimal.

   procedure Apply_Changes (View : Odd_Memory_View);
   --  Write the changes into memory.

   procedure Page_Down (View : Odd_Memory_View);
   procedure Page_Up (View : Odd_Memory_View);
   --  Move up or down one page in the view.

   procedure Init_Graphics (Window : Gdk.Window.Gdk_Window);
   --  Initialize fonts and graphics used for this widget.

   procedure Update (View : Odd_Memory_View; Process : Gtk_Widget);
   --  Updates the dialog.
   --  Process is the new Debugger_Process_Tab.

   procedure Update_Display (View : Odd_Memory_View);
   --  Refreshes the view.

   function Position_To_Index
     (View     : in Odd_Memory_View;
      Position : in Gint) return Integer;
   --  Returns the position of a value in View.Values from a position in
   --  View.View.

   type Dir is (Up, Down, Left, Right);
   procedure Move_Cursor
     (View : Odd_Memory_View; Where : in Dir);
   --  Moves the cursor.

   procedure Insert (View : Odd_Memory_View; Char : String);
   --  Inserts string at the current location.

end Odd.Memory_View;

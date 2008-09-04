-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2000-2008, AdaCore               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Text_IO;              use Ada.Text_IO;

with Glib;                     use Glib;
with Gdk.Color;                use Gdk.Color;
with Glib.Properties;          use Glib.Properties;

with Gtk;                      use Gtk;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtk.Text_Tag_Table;       use Gtk.Text_Tag_Table;
with Gtk.Text_View;            use Gtk.Text_View;

with Pango.Font;               use Pango.Font;

with Debugger;                 use Debugger;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GUI_Utils;                use GUI_Utils;
with GVD.Preferences;          use GVD.Preferences;
with GVD.Process;              use GVD.Process;
with GVD.Scripts;              use GVD.Scripts;
with String_Utils;             use String_Utils;

package body GVD.Memory_View is

   use GNAT.Strings;

   --------------------
   -- Local packages --
   --------------------

   package Long_Int_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   use Long_Int_IO;

   ---------------------
   -- Local constants --
   ---------------------

   Address_Length       : constant Integer := 16;
   Address_Separator    : constant String  := ": ";
   Data_Separator       : constant String  := " ";
   ASCII_Separator      : constant String  := " ";
   Data_ASCII_Separator : constant String  := " -  ";

   Non_Valid_Character  : constant String  := "-";

   End_Of_Line       : constant String  := (1 => ASCII.LF);
   Hex_Header        : constant String  := "16#";
   Hex_Footer        : constant String  := "#";

   Line_Base_Size    : constant Integer := 16;
   --  Number of bytes per line.

   -----------------------
   -- Local subprograms --
   -----------------------

   function Conversion
     (S        : String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer) return String;
   --  Converts a string of hexadecimal digits into a string representing
   --  the same number in Format, with a constant size.

   function To_Standard_Base
     (Address  : Long_Long_Integer;
      Base     : Integer;
      Trunc_At : Integer := -1) return String;
   --  Conversion from a Long_Long_Integer to a based representation.
   --  Output is truncated to Trunc_At characters if Trunc_At /= -1.

   procedure Clear_View (View : access GVD_Memory_View_Record'Class);
   --  Removes everything from the view.

   procedure Get_Coordinates
    (View     : access GVD_Memory_View_Record'Class;
     Position : Gint;
     Row      : out Integer;
     Column   : out Integer);
   --  Gives the bloc coordinates from a given position.

   function Position_To_Bloc
     (View     : access GVD_Memory_View_Record'Class;
      Position : Gint) return Integer;
   --  Gives the bloc number at the given position.

   procedure Swap_Blocks
     (View : access GVD_Memory_View_Record'Class; Size : Data_Size);
   --  Swap blocks of size Size in the View's values to swap endianness.

   procedure Insert_ASCII (View : access GVD_Memory_View_Record'Class);
   --  Insert the ASCII representation of the memory shown on the current line.

   -----------------
   -- Swap_Blocks --
   -----------------

   procedure Swap_Blocks
     (View : access GVD_Memory_View_Record'Class; Size : Data_Size)
   is
      Index     : Integer := 0;
      Unit_Size : Integer;

   begin
      if View.Values = null or else View.Flags = null then
         return;
      end if;

      case Size is
         when Byte =>
            return;
         when Halfword =>
            Unit_Size := 4;
         when Word =>
            Unit_Size := 8;
      end case;

      declare
         Buffer : String (1 .. Unit_Size);
      begin
         while Index <= View.Number_Of_Bytes * 2 - Unit_Size loop
            Buffer (1 .. Unit_Size) :=
              View.Values
                (View.Values'First + Index ..
                     View.Values'First + Index + Unit_Size - 1);

            for J in 1 .. Unit_Size / 2 loop
               View.Values
                 (View.Values'First + Index + (J - 1) * 2
                    .. View.Values'First + Index + (J - 1) * 2 + 1) :=
                 Buffer (Buffer'Last - (J - 1) * 2 - 1
                           .. Buffer'Last - (J - 1) * 2);
            end loop;

            Buffer (1 .. Unit_Size) :=
              View.Flags
                (View.Flags'First + Index ..
                     View.Flags'First + Index + Unit_Size - 1);

            for J in 1 .. Unit_Size / 2 loop
               View.Flags
                 (View.Flags'First + Index + (J - 1) * 2
                    .. View.Flags'First + Index + (J - 1) * 2 + 1) :=
                 Buffer (Buffer'Last - (J - 1) * 2 - 1
                           .. Buffer'Last - (J - 1) * 2);
            end loop;

            Index := Index + Unit_Size;
         end loop;
      end;
   end Swap_Blocks;

   ---------------------
   -- Get_Coordinates --
   ---------------------

   procedure Get_Coordinates
     (View     : access GVD_Memory_View_Record'Class;
      Position : Gint;
      Row      : out Integer;
      Column   : out Integer)
   is
      Row_Length : Integer;
      ASCII_Size : Integer := 0;

   begin
      if Get_Active (View.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length +
           Line_Base_Size +
           ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      Row_Length :=
        Address_Length + Address_Separator'Length +
        (View.Number_Of_Columns * (View.Trunc + Data_Separator'Length)) +
        ASCII_Size + End_Of_Line'Length;

      Row := Integer (Position) / Row_Length;

      Column :=
        (Integer (Position) - Row * Row_Length -
          (Address_Length + Address_Separator'Length)) + 1;

      if Column <= 0 then
         Column := -1;
         return;
      end if;

      Column := Column / (View.Trunc + Data_Separator'Length);

   end Get_Coordinates;

   ----------------------
   -- Position_To_Bloc --
   ----------------------

   function Position_To_Bloc
     (View     : access GVD_Memory_View_Record'Class;
      Position : Gint) return Integer
   is
      Row    : Integer;
      Column : Integer;

   begin
      Get_Coordinates (View, Position, Row, Column);
      return Column + Row * View.Number_Of_Columns;
   end Position_To_Bloc;

   ---------------------------
   -- Watch_Cursor_Location --
   ---------------------------

   procedure Watch_Cursor_Location (View : access GVD_Memory_View_Record'Class)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Row    : Integer;
      Column : Integer;
      Iter   : Gtk_Text_Iter;
      Result : Boolean;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
      Get_Coordinates (View, Get_Offset (Iter), Row, Column);

      --  If the cursor is found at a place where text is not editable,
      --  reinitialize its position.

      if Column >= View.Number_Of_Columns
        or else Column < 0
      then
         Set_Offset (Iter, Address_Separator'Length + Gint (Address_Length));
         Place_Cursor (Buffer, Iter);
      else
         while not Editable (Iter, False) loop
            Forward_Cursor_Position (Iter, Result);
         end loop;
         Place_Cursor (Buffer, Iter);
      end if;

   end Watch_Cursor_Location;

   ----------------
   -- Conversion --
   ----------------

   function Conversion
     (S        : String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer) return String
   is
      pragma Unreferenced (Size);

      Long  : Long_Long_Integer;
      Test  : Integer := S'First;
      Dummy : constant String := "------------------------";

   begin
      Skip_To_String (S, Test, Non_Valid_Character);

      if Test < S'Last then
         if Trunc_At /= -1 then
            return Dummy (1 .. Trunc_At);
         else
            return Non_Valid_Character;
         end if;
      end if;

      Long := Long_Long_Integer'Value (Hex_Header & S & Hex_Footer);

      case Format is
         when Hex =>
            return S;
         when Octal =>
            return To_Standard_Base (Long, 8, Trunc_At);
         when Decimal =>
            return To_Standard_Base (Long, 10, Trunc_At);
         when Text =>
            declare
               Result : String (1 .. S'Length / 2);
               Value  : Integer;
            begin
               for J in 1 .. Result'Last loop
                  Value :=
                    Integer'Value
                    (Hex_Header
                     & S (S'First + 2 * J - 2 .. S'First + 2 * J - 1)
                     & Hex_Footer);

                  if Value > 31 and then Value < 128 then
                     Result (J) := Character'Val (Value);
                  else
                     Result (J) := '.';
                  end if;
               end loop;

               return Result;
            end;
      end case;
   end Conversion;

   ----------------
   -- Clear_View --
   ----------------

   procedure Clear_View (View : access GVD_Memory_View_Record'Class) is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Delete (Buffer, Start_Iter, End_Iter);
   end Clear_View;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics
     (View   : access GVD_Memory_View_Record'Class;
      Window : Gdk_Window)
   is
      pragma Unreferenced (Window);
      Buffer    : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Tag_Table : constant Gtk_Text_Tag_Table := Get_Tag_Table (Buffer);
      Font      : constant Pango_Font_Description :=
        Default_Style.Get_Pref_Font;
   begin
      --  Tag used to display not modified memory
      Gtk_New (View.Default_Tag);
      Set_Property (View.Default_Tag, Background_Gdk_Property, Null_Color);
      Set_Property (View.Default_Tag, Foreground_Gdk_Property, Null_Color);
      Set_Property (View.Default_Tag, Font_Desc_Property, Font);
      Add (Tag_Table, View.Default_Tag);

      --  Tag used to display modified memory
      Gtk_New (View.Modified_Tag);
      Set_Property (View.Modified_Tag, Background_Gdk_Property, Null_Color);
      Set_Property (View.Modified_Tag, Foreground_Gdk_Property,
                    Change_Color.Get_Pref);
      Set_Property (View.Modified_Tag, Font_Desc_Property, Font);
      Add (Tag_Table, View.Modified_Tag);

      --  Tag used to display memory addresses
      Gtk_New (View.Address_Tag);
      Set_Property (View.Address_Tag, Background_Gdk_Property,
                    Memory_Highlighted_Color.Get_Pref);
      Set_Property (View.Address_Tag, Foreground_Gdk_Property,
                    Memory_View_Color.Get_Pref);
      Set_Property (View.Address_Tag, Font_Desc_Property, Font);
      Set_Property (View.Address_Tag, Text_Tag.Editable_Property, False);
      Add (Tag_Table, View.Address_Tag);

      --  Tag used to display editable text
      Gtk_New (View.Editable_Tag);
      Set_Property (View.Editable_Tag, Text_Tag.Editable_Property, True);
      Add (Tag_Table, View.Editable_Tag);

   end Init_Graphics;

   ----------------------
   -- To_Standard_Base --
   ----------------------

   function To_Standard_Base
     (Address  : Long_Long_Integer;
      Base     : Integer;
      Trunc_At : Integer := -1) return String
   is
      Index   : Integer := 1;
      Result  : String (1 .. 64);
      Mapping : constant Character_Mapping :=
                  To_Mapping ("ABCDEF ", "abcdef0");

   begin
      Put (Result, Address, Base);
      Skip_To_String (Result, Index, Hex_Footer);

      if Index > 3
        and then Index < Result'Length + 1
      then
         Result (Index - 3 .. Index) := "    ";
      end if;

      if Trunc_At = -1 then
         return Translate (Result (1 .. Result'Length - 1), Mapping);
      else
         if Base = 10 then
            return
              Translate
                (Result (Result'Last - Trunc_At + 1 .. Result'Last), Mapping);
         else
            return
              Translate
                (Result (Result'Last - Trunc_At .. Result'Last - 1), Mapping);
         end if;
      end if;
   end To_Standard_Base;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (View : access GVD_Memory_View_Record'Class) is
      Buffer          : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Number_Of_Lines : constant Integer :=
                          Integer (Get_Value_As_Int (View.Lines_Spin));
      Endianness      : constant Endian_Type :=
                          Get_Endian_Type
                            (Get_Current_Process (View.Window).Debugger);
      Old_Size        : constant Data_Size := View.Data;
      Index           : Integer;
      Tag             : Gtk_Text_Tag;
      Current         : String_Access;
      Start_Mark      : Gtk_Text_Mark;
      Start_Iter      : Gtk_Text_Iter;
      End_Iter        : Gtk_Text_Iter;

   begin
      if View.Values = null then
         return;
      end if;

      --  Use if/elsif statements instead of 'Value here to handle
      --  internationalization of strings properly.

      declare
         Size : constant String := Get_Text (View.Size_Entry);
         Data : constant String := Get_Text (View.Data_Entry);
      begin
         if Size = -"Byte" then
            View.Data := Byte;
         elsif Size = -"Halfword" then
            View.Data := Halfword;
         elsif Size = -"Word" then
            View.Data := Word;
         else
            --  May happen if e.g. Size is null, while its value is being
            --  updated.

            return;
         end if;

         if Data = -"Hex" then
            View.Display := Hex;
         elsif Data = -"Decimal" then
            View.Display := Decimal;
         elsif Data = -"Octal" then
            View.Display := Octal;
         elsif Data = -"ASCII" then
            View.Display := Text;
         else
            return;
         end if;
      end;

      if Endianness = Little_Endian
        and then Old_Size /= View.Data
      then
         --  Swap back to original.
         Swap_Blocks (View, Old_Size);

         --  Swap again to new size.
         Swap_Blocks (View, View.Data);
      end if;

      case View.Data is
         when Byte =>
            View.Unit_Size := 2;
         when Halfword =>
            View.Unit_Size := 4;
         when Word =>
            View.Unit_Size := 8;
      end case;

      case View.Display is
         when Hex =>
            View.Trunc := View.Unit_Size;
         when Text =>
            View.Trunc := View.Unit_Size / 2;
         when Decimal =>
            View.Trunc := Integer (Float (View.Unit_Size) * 1.2 + 0.5);
         when Octal =>
            View.Trunc := View.Unit_Size * 2;
      end case;

      View.Number_Of_Columns := Line_Base_Size * 2 / View.Unit_Size;

      if Number_Of_Lines * View.Number_Of_Columns * View.Unit_Size >
        View.Values'Length
      then
         Display_Memory (View, View.Starting_Address);
         return;
      end if;

      Begin_User_Action (Buffer);
      Clear_View (View);
      Index := 1;

      for Line_Index in 1 .. Number_Of_Lines loop
         Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
         Start_Mark := Create_Mark (Buffer, Where => End_Iter);

         Insert
           (Buffer,
            End_Iter,
            To_Standard_Base
              (View.Starting_Address +
                 Long_Long_Integer
                   ((Line_Index - 1) * View.Number_Of_Columns *
                      View.Unit_Size / 2),
               16, Address_Length) & Address_Separator);

         Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);

         Apply_Tag (Buffer, View.Address_Tag, Start_Iter, End_Iter);
         Delete_Mark (Buffer, Start_Mark);
         Place_Cursor (Buffer, End_Iter);

         for Column_Index in 1 .. View.Number_Of_Columns loop
            Index := (Line_Index - 1) *
              View.Number_Of_Columns * View.Unit_Size
              + (Column_Index - 1) * View.Unit_Size + 1;

            if View.Values (Index .. Index + View.Unit_Size - 1) /=
              View.Flags (Index .. Index + View.Unit_Size - 1)
            then
               Tag := View.Modified_Tag;
               Current := View.Flags;
            else
               Tag := View.Default_Tag;
               Current := View.Values;
            end if;

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert
              (Buffer,
               End_Iter,
               Conversion
                 (Current (Index .. Index + View.Unit_Size - 1),
                  View.Unit_Size,
                  View.Display,
                  View.Trunc));

            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
            Apply_Tag (Buffer, View.Editable_Tag, Start_Iter, End_Iter);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert (Buffer, End_Iter, Data_Separator);
            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);
         end loop;

         if Get_Active (View.Show_Ascii) then
            Insert_ASCII (View);
         end if;

         if Line_Index /= Number_Of_Lines then
            Insert_At_Cursor (Buffer, End_Of_Line);
         end if;
      end loop;

      End_User_Action (Buffer);
   end Update_Display;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access GVD_Memory_View_Record'Class;
      Address : Long_Long_Integer)
   is
      Process         : constant Visual_Debugger :=
                          Get_Current_Process (View.Window);
      Number_Of_Lines : constant Integer :=
                          Integer (Get_Value_As_Int (View.Lines_Spin));
   begin
      View.Number_Of_Columns := Line_Base_Size * 2 / View.Unit_Size;

      if View.Values = null
        or else Number_Of_Lines * View.Number_Of_Columns * View.Unit_Size
          /= View.Values'Length
      then
         View.Number_Of_Bytes := Number_Of_Lines * View.Number_Of_Columns
           * View.Unit_Size / 2;
      end if;

      declare
         Values : String (1 .. 2 * View.Number_Of_Bytes);
      begin
         Set_Busy (Process, True);
         Set_Busy_Cursor (Get_Window (View), True);
         Values := Get_Memory
           (Process.Debugger,
            View.Number_Of_Bytes,
            "0x" & To_Standard_Base (Address, 16));
         View.Starting_Address := Address;
         Free (View.Values);
         Free (View.Flags);
         View.Values := new String'(Values);
         View.Flags  := new String'(Values);
         View.Data   := Byte;
         Update_Display (View);
         Set_Text (View.Address_Entry,
                   "0x" & To_Standard_Base (Address, 16, Address_Length));
         Set_Busy (Process, False);
         Set_Busy_Cursor (Get_Window (View), False);
      end;
   end Display_Memory;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access GVD_Memory_View_Record'Class;
      Address : String)
   is
      Real_Address : Long_Long_Integer;
      Index        : Integer;
      Process      : constant Visual_Debugger :=
                       Get_Current_Process (View.Window);

   begin
      if Address'Length > 2
        and then Address (Address'First .. Address'First + 1) = "0x"
      then
         Index := Address'First + 2;

         while Index <= Address'Last
           and then Is_Hexadecimal_Digit (Address (Index))
         loop
            Index := Index + 1;
         end loop;

         begin
            Real_Address := Long_Long_Integer'Value
              (Hex_Header &
               Address (Address'First + 2 .. Index - 1) &
               Hex_Footer);
            Display_Memory (View, Real_Address);
         exception
            when Constraint_Error =>
               Display_Memory (View, 0);
         end;

      else
         declare
            New_Address : constant String :=
                            Get_Variable_Address (Process.Debugger, Address);
         begin
            if New_Address'Length > 2
              and then New_Address
                (New_Address'First .. New_Address'First + 1) = "0x"
            then
               Display_Memory (View, New_Address);
               Set_Text (View.Address_Entry, Address);
            else
               Display_Memory (View, 0);
            end if;
         end;
      end if;
   end Display_Memory;

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes (View : access GVD_Memory_View_Record'Class) is
   begin
      if Get_Endian_Type
        (Get_Current_Process (View.Window).Debugger) = Little_Endian
      then
         Swap_Blocks (View, View.Data);
      end if;

      for J in 1 .. View.Number_Of_Bytes loop
         if View.Flags (J * 2 - 1 .. J * 2) /=
           View.Values (J * 2 - 1 .. J * 2)
         then
            Put_Memory_Byte
              (Get_Current_Process (View.Window).Debugger,
               "0x" &
               To_Standard_Base
                 (View.Starting_Address + Long_Long_Integer (J - 1),
                  16),
               View.Flags (J * 2 - 1 .. J * 2));
         end if;
      end loop;

      Display_Memory (View, View.Starting_Address);
      Run_Debugger_Hook
        (Get_Current_Process (View.Window),
         Debugger_Process_Stopped_Hook);
   end Apply_Changes;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out GVD_Memory_View;
      Window : Gtk_Widget) is
   begin
      View := new GVD_Memory_View_Record;
      Initialize (View);
      Init_Graphics (View, Get_Window (Window));
      View.Window := Window;
      Set_Wrap_Mode (View.View, Wrap_None);
   end Gtk_New;

   -------------
   -- Page_Up --
   -------------

   procedure Page_Up (View : access GVD_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address -
           Long_Long_Integer
             (Integer (Get_Value_As_Int (View.Lines_Spin)) * Line_Base_Size));
   end Page_Up;

   ---------------
   -- Page_Down --
   ---------------

   procedure Page_Down (View : access GVD_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address +
           Long_Long_Integer
             (Integer (Get_Value_As_Int (View.Lines_Spin)) * Line_Base_Size));
   end Page_Down;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor
     (View  : access GVD_Memory_View_Record'Class;
      Where : Dir)
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Position   : Gint;
      ASCII_Size : Integer := 0;

   begin
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Position := Get_Offset (Start_Iter);

      if Get_Active (View.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length +
           Line_Base_Size +
           ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      case Where is
         when Right =>
            Get_Iter_At_Offset (Buffer, Start_Iter, Position + 1);
            Get_Iter_At_Offset
              (Buffer, End_Iter, Position + 1 + Data_Separator'Length);

            if Get_Text (Buffer, Start_Iter, End_Iter) = Data_Separator then
               --  Are we on the last bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = View.Number_Of_Columns - 1
               then
                  --  Is it the last bloc in the view ?

                  if Position_To_Bloc (View, Position) =
                    View.Number_Of_Columns
                    * Integer (Get_Value_As_Int (View.Lines_Spin)) - 1
                  then
                     Get_Iter_At_Offset (Buffer, Start_Iter, Position - 1);
                     Place_Cursor (Buffer, Start_Iter);
                  else
                     Get_Iter_At_Offset
                       (Buffer, Start_Iter,
                        Position
                        + Gint (Address_Length)
                        + Address_Separator'Length
                        + Data_Separator'Length
                        + Gint (ASCII_Size)
                        + End_Of_Line'Length);
                     Place_Cursor (Buffer, Start_Iter);
                  end if;
               else
                  Get_Iter_At_Offset
                    (Buffer, Start_Iter, Position + Data_Separator'Length);
                  Place_Cursor (Buffer, Start_Iter);
               end if;
            end if;

         when Left =>
            Get_Iter_At_Offset
              (Buffer, Start_Iter, Position - Data_Separator'Length);
            Get_Iter_At_Offset (Buffer, End_Iter, Position);

            if Get_Text (Buffer, Start_Iter, End_Iter) = Data_Separator then
               --  Are we on the first bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = 0
               then
                  --  Is it the first bloc in the view ?

                  if Position_To_Bloc (View, Position) = 0 then
                     Get_Iter_At_Offset (Buffer, Start_Iter, Position + 1);
                     Place_Cursor (Buffer, Start_Iter);
                  else
                     Get_Iter_At_Offset
                       (Buffer, Start_Iter,
                        Position
                        - Gint (Address_Length)
                        - Address_Separator'Length
                        - Data_Separator'Length
                        - Gint (ASCII_Size)
                        - End_Of_Line'Length);
                     Place_Cursor (Buffer, Start_Iter);
                  end if;
               else
                  Get_Iter_At_Offset
                    (Buffer, Start_Iter, Position - Data_Separator'Length);
                  Place_Cursor (Buffer, Start_Iter);
               end if;
            end if;
         when others =>
            null;
      end case;
   end Move_Cursor;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View : access GVD_Memory_View_Record'Class;
      Char : String)
   is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Position    : Gint;
      Prefix      : String (1 .. 3);
      Success     : Boolean;
      Value_Index : Integer;
      Start_Mark  : Gtk_Text_Mark;
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Bloc_Begin  : Gint;
      Bloc_End    : Gint;
   begin
      pragma Assert (Char'Length = 1);

      --  Get the cursor position in the buffer
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Position := Get_Offset (Start_Iter);

      if not Editable (Start_Iter, Default_Setting => False) then
         --  The cursor should not be located in a position where text cannot
         --  be inserted.
         Watch_Cursor_Location (View);
         return;
      end if;

      Get_Start_Iter (Buffer, Start_Iter);
      Get_End_Iter (Buffer, End_Iter);

      declare
         Text : constant String :=
                  Get_Text (Buffer, Start_Iter, End_Iter, False);
      begin
         if View.View = null or else Text'Length <= 0 then
            return;
         end if;
      end;

      --  Check whether the character to insert is in an acceptable range

      case View.Display is
         when Hex =>
            if Is_Hexadecimal_Digit (Char (Char'First)) then
               Prefix := "16#";
            else
               return;
            end if;
         when Decimal =>
            if Is_Decimal_Digit (Char (Char'First)) then
               Prefix := "10#";
            else
               return;
            end if;
         when Octal =>
            if Char (Char'First) in '0' .. '7' then
               Prefix := "08#";
            else
               return;
            end if;
         when Text =>
            null;
      end case;

      Begin_User_Action (Buffer);

      Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
      Start_Mark := Create_Mark (Buffer, Where => End_Iter);

      --  Delete the character following the cursor

      Copy (End_Iter, Start_Iter);
      Forward_Cursor_Position (Start_Iter, Success);
      Delete (Buffer, Start_Iter, End_Iter);

      --  Insert the new character

      Insert (Buffer, End_Iter, Char);

      Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
      Bloc_Begin := Get_Offset (Start_Iter);
      Bloc_End := Bloc_Begin;
      Place_Cursor (Buffer, Start_Iter);

      --  Find the beginning of the bloc

      while Bloc_Begin > 0 loop
         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_Begin - 1);

         exit when Get_Text (Buffer, Start_Iter, End_Iter) =
           Data_Separator (Data_Separator'Last .. Data_Separator'Last);

         Bloc_Begin := Bloc_Begin - 1;
      end loop;

      --  Find the end of the bloc

      loop
         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_End);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End + 1);

         declare
            S : constant String := Get_Text (Buffer, Start_Iter, End_Iter);
         begin
            exit when S = ""
              or else S = Data_Separator
                   (Data_Separator'First .. Data_Separator'First);
         end;

         Bloc_End := Bloc_End + 1;
      end loop;

      --  Mark the bloc as modified

      Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
      Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);

      Apply_Tag (Buffer, View.Modified_Tag, Start_Iter, End_Iter);
      Apply_Tag (Buffer, View.Editable_Tag, Start_Iter, End_Iter);

      --  Update the flags

      if View.Display = Text then
         declare
            Row        : Integer;
            Column     : Integer;
            ASCII_Size : Integer := 0;
         begin
            if Get_Active (View.Show_Ascii) then
               ASCII_Size :=
                 Data_ASCII_Separator'Length
                 + Line_Base_Size
                 + ASCII_Separator'Length * View.Number_Of_Columns;
            end if;

            Get_Coordinates (View, Position, Row, Column);
            Value_Index :=
              (Integer (Position)
               - Row *
                 (Address_Length + Address_Separator'Length + ASCII_Size
                  + End_Of_Line'Length
                  + View.Number_Of_Columns * Data_Separator'Length)
               - Address_Length - Address_Separator'Length
               - (Column - 1) * (Data_Separator'Length)) * 2 - 1;

            View.Flags (Value_Index .. Value_Index + 1) :=
              To_Standard_Base
                (Long_Long_Integer (Character'Pos (Char (Char'First))),
                 16, 2);
         end;
      else
         Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));

         Value_Index :=
           Position_To_Bloc (View, Get_Offset (Start_Iter))
           * Line_Base_Size / View.Number_Of_Columns * 2 + 1;

         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);

         declare
            S : constant String := Get_Text (Buffer, Start_Iter, End_Iter);
         begin
            if View.Flags (Value_Index .. Value_Index) /=
              Non_Valid_Character
            then
               View.Flags (Value_Index .. Value_Index + View.Unit_Size - 1) :=
                 To_Standard_Base
                   (Long_Long_Integer'Value (Prefix & S & Hex_Footer),
                    16,
                    View.Unit_Size);
            end if;

         end;
      end if;

      if Get_Active (View.Show_Ascii) then
         --  Update the ASCII view

         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);
         Copy (End_Iter, Start_Iter);
         Forward_Cursor_Positions
           (End_Iter, Gint (Data_ASCII_Separator'Length), Success);

         loop
            declare
               Text : constant String :=
                        Get_Text (Buffer, Start_Iter, End_Iter);
            begin
               exit when Text = Data_ASCII_Separator;
               Forward_Cursor_Position (Start_Iter, Success);
               Forward_Cursor_Position (End_Iter, Success);
            end;
         end loop;

         Forward_To_Line_End (End_Iter, Success);

         Delete (Buffer, Start_Iter, End_Iter);

         Place_Cursor (Buffer, Start_Iter);
         Insert_ASCII (View);
      end if;

      End_User_Action (Buffer);

      --  Update the position of the cursor

      Get_Iter_At_Offset (Buffer, Start_Iter, Position);
      Place_Cursor (Buffer, Start_Iter);
      Move_Cursor (View, Right);
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Forward_Cursor_Position (Start_Iter, Success);
      Place_Cursor (Buffer, Start_Iter);
   end Insert;

   ------------------
   -- Insert_ASCII --
   ------------------

   procedure Insert_ASCII (View : access GVD_Memory_View_Record'Class) is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (View.View);
      Endianness  : constant Endian_Type :=
                      Get_Endian_Type
                        (Get_Current_Process (View.Window).Debugger);
      Start_Mark  : Gtk_Text_Mark;
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Tag         : Gtk_Text_Tag;
      Index       : Natural;
      Line_Index  : Natural;
      Current     : String_Access;
   begin
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Line_Index := Natural (Get_Line (Start_Iter)) + 1;
      Insert_At_Cursor (Buffer, Data_ASCII_Separator);

      for Column_Index in 1 .. View.Number_Of_Columns loop
         Index := (Line_Index - 1) *
           (View.Number_Of_Columns * View.Unit_Size)
           + (Column_Index - 1) * View.Unit_Size + 1;

         if View.Values (Index .. Index + View.Unit_Size - 1) /=
           View.Flags (Index .. Index + View.Unit_Size - 1)
         then
            Tag := View.Modified_Tag;
            Current := View.Flags;
         else
            Tag := View.Default_Tag;
            Current := View.Values;
         end if;

         declare
            S : String (1 .. View.Unit_Size);
         begin
            if Endianness = Little_Endian then
               declare
                  B : constant String (1 .. View.Unit_Size) :=
                        Current (Index .. Index + View.Unit_Size - 1);
               begin
                  for J in 0 .. View.Unit_Size / 2 - 1 loop
                     S (S'First + J * 2 .. S'First + J * 2 + 1) :=
                       B (B'Last -  J * 2 - 1 .. B'Last -  J * 2);
                  end loop;
               end;
            else
               S := Current (Index .. Index + View.Unit_Size - 1);
            end if;

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert
              (Buffer,
               End_Iter,
               Conversion (S, View.Unit_Size, Text, View.Trunc) &
               Data_Separator);

            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);
         end;
      end loop;
   end Insert_ASCII;

end GVD.Memory_View;

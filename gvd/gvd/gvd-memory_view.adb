-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with GNAT.OS_Lib;

with Glib;             use Glib;

with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gdk.Window;       use Gdk.Window;

with Gtk;              use Gtk;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Text;         use Gtk.Text;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Frame;        use Gtk.Frame;
with Gtkada.Canvas;    use Gtkada.Canvas;

with Debugger;         use Debugger;
with Memory_View_Pkg;  use Memory_View_Pkg;

with Odd_Intl;         use Odd_Intl;
with GVD.Strings;      use GVD.Strings;
with GVD.Types;        use GVD.Types;
with GVD.Process;      use GVD.Process;
with GVD.Preferences;  use GVD.Preferences;
with Process_Proxies;  use Process_Proxies;

package body GVD.Memory_View is

   --------------------
   -- Local packages --
   --------------------

   package Long_Int_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   use Long_Int_IO;

   package Memory_View_Register is new Register_Generic
     (Long_Long_Integer, GVD_Memory_View_Record);

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
     (S        : in String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer) return String;
   --  Converts a string of hexadecimal digits into a string representing
   --  the same number in Format, with a constant size.

   function To_Standard_Base
     (Address  : in Long_Long_Integer;
      Base     : Integer;
      Trunc_At : Integer := -1) return String;
   --  Conversion from a Long_Long_Integer to a based representation.
   --  Output is truncated to Trunc_At characters if Trunc_At /= -1.

   procedure Clear_View (View : access GVD_Memory_View_Record'Class);
   --  Removes everything from the view.

   procedure Get_Coordinates
    (View     : access GVD_Memory_View_Record'Class;
     Position : in Gint;
     Row      : out Integer;
     Column   : out Integer);
   --  Gives the bloc coordinates from a given position.

   function Position_To_Bloc
     (View     : access GVD_Memory_View_Record'Class;
      Position : in Gint) return Integer;
   --  Gives the bloc number at the given position.

   procedure Swap_Blocks
     (View : access GVD_Memory_View_Record'Class;
      Size : Data_Size);
   --  Swap blocks of size Size in the View's values to swap endianness.

   -----------------
   -- Swap_Blocks --
   -----------------

   procedure Swap_Blocks
     (View : access GVD_Memory_View_Record'Class;
      Size : Data_Size)
   is
      Index     : Integer := 0;
      Unit_Size : Integer;
   begin
      if View.Values = null
        or else View.Flags = null
      then
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
         Buffer     : String (1 .. Unit_Size);
      begin
         while Index <= View.Number_Of_Bytes * 2 loop
            Buffer (1 .. Unit_Size)
              := View.Values.all
              (View.Values.all'First + Index
               .. View.Values.all'First + Index + Unit_Size - 1);
            for I in 0 .. Unit_Size / 2 - 1 loop
               View.Values.all
                 (View.Values.all'First + Index + I * 2
                  .. View.Values.all'First + Index + I * 2 + 1)
                 := Buffer (Buffer'Last -  I * 2 - 1 .. Buffer'Last -  I * 2);
            end loop;
            Buffer (1 .. Unit_Size)
              := View.Flags.all
              (View.Flags.all'First + Index
               .. View.Flags.all'First + Index + Unit_Size - 1);
            for I in 0 .. Unit_Size / 2 - 1 loop
               View.Flags.all
                 (View.Flags.all'First + Index + I * 2
                  .. View.Flags.all'First + Index + I * 2 + 1)
                 := Buffer (Buffer'Last -  I * 2 - 1 .. Buffer'Last -  I * 2);
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
      Position : in Gint;
      Row      : out Integer;
      Column   : out Integer)
   is
      Row_Length : Integer;
      ASCII_Size : Integer := 0;

   begin
      if Get_Active (View.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length
           + Line_Base_Size
           + ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      Row_Length := (Address_Length
                     + Address_Separator'Length
                     + (View.Number_Of_Columns
                        * (View.Trunc + Data_Separator'Length))
                     + ASCII_Size
                     + End_Of_Line'Length);

      Row := Integer (Position) / Row_Length;

      Column := ((Integer (Position)
                  - Row * Row_Length
                  - (Address_Length + Address_Separator'Length)) + 1);

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
      Position : in Gint) return Integer
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

   procedure Watch_Cursor_Location
     (View : access GVD_Memory_View_Record'Class)
   is
      Row    : Integer;
      Column : Integer;
   begin
      Get_Coordinates (View, Get_Position (View.View), Row, Column);

      if Column >= View.Number_Of_Columns or else Column < 0 then
         Set_Position
           (View.View, Address_Separator'Length + Gint (Address_Length));
      end if;
   end Watch_Cursor_Location;

   ----------------
   -- Conversion --
   ----------------

   function Conversion
     (S        : in String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer) return String
   is
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
   begin
      Delete_Text (View.View);
   end Clear_View;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics
     (View   : access GVD_Memory_View_Record'Class;
      Window : Gdk_Window) is
   begin
      View.View_Font   := Get_Gdkfont
        (Get_Pref (Memory_View_Font), Get_Pref (Memory_View_Font_Size));
      View.View_Color       := Get_Pref (Memory_View_Color);
      View.Highlighted      := Get_Pref (Memory_Highlighted_Color);
      View.White_Color      := White (Get_System);
      View.Selected         := Get_Pref (Memory_Selected_Color);
      View.Modified_Color   := Get_Pref (Memory_Modified_Color);
   end Init_Graphics;

   ----------------------
   -- To_Standard_Base --
   ----------------------

   function To_Standard_Base
     (Address  : in Long_Long_Integer;
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
         return (Translate (Result (1 .. Result'Length - 1), Mapping));
      else
         if Base = 10 then
            return (Translate
                    (Result (Result'Last - Trunc_At + 1 .. Result'Last),
                     Mapping));
         else
            return (Translate
                    (Result (Result'Last - Trunc_At .. Result'Last - 1),
                     Mapping));
         end if;
      end if;
   end To_Standard_Base;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (View : access GVD_Memory_View_Record'Class) is
      Index      : Integer;
      Width      : Gint;
      Height     : Gint;
      Background : Gdk_Color := Null_Color;
      Foreground : Gdk_Color := Null_Color;
      Current    : String_Access;
      Old_Size   : Data_Size := View.Data;

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
            raise Program_Error;
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
            raise Program_Error;
         end if;
      end;

      if Get_Endian_Type (Get_Current_Process (View.Window).Debugger)
        = Little_Endian
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

      Get_Size (Get_Text_Area (View.View), Width, Height);

      View.Number_Of_Lines := Integer (Height) /
        Integer (Get_Ascent (View.View_Font)
                 + Get_Descent (View.View_Font) + 1);

      View.Number_Of_Columns := Line_Base_Size * 2 / View.Unit_Size;

      Freeze (View.View);
      Clear_View (View);
      Index := 1;

      for Line_Index in 1 .. View.Number_Of_Lines loop
         Insert
           (View.View,
            Fore  => View.View_Color,
            Back  => View.Highlighted,
            Font  => View.View_Font,
            Chars =>
              To_Standard_Base
                (View.Starting_Address +
                   Long_Long_Integer
                     ((Line_Index - 1) * View.Number_Of_Columns *
                      View.Unit_Size / 2),
                 16, Address_Length) & Address_Separator);

         for Column_Index in 1 .. View.Number_Of_Columns loop
            Index := (Line_Index - 1) *
              View.Number_Of_Columns * View.Unit_Size
              + (Column_Index - 1) * View.Unit_Size + 1;

            if View.Values (Index .. Index + View.Unit_Size - 1) /=
              View.Flags (Index .. Index + View.Unit_Size - 1)
            then
               Foreground := View.Modified_Color;
               Current    := View.Flags;
            else
               Foreground := Null_Color;
               Current    := View.Values;
            end if;

            Insert
              (View.View,
               Font  => View.View_Font,
               Fore  => Foreground,
               Back  => Background,
               Chars =>
                 Conversion (Current (Index .. Index + View.Unit_Size - 1),
                             View.Unit_Size,
                             View.Display,
                             View.Trunc));

            Insert
              (View.View,
               Font  => View.View_Font,
               Fore  => Foreground,
               Back  => Background,
               Chars => Data_Separator);
         end loop;

         if Get_Active (View.Show_Ascii) then
            Insert (View.View, Chars => Data_ASCII_Separator);

            for Column_Index in 1 .. View.Number_Of_Columns loop
               Index := (Line_Index - 1) *
                 (View.Number_Of_Columns * View.Unit_Size)
                 + (Column_Index - 1) * View.Unit_Size + 1;

               if View.Values (Index .. Index + View.Unit_Size - 1) /=
                 View.Flags (Index .. Index + View.Unit_Size - 1)
               then
                  Foreground := View.Modified_Color;
                  Current := View.Flags;
               else
                  Foreground := Null_Color;
                  Current := View.Values;
               end if;

               Insert
                 (View.View,
                  Font  => View.View_Font,
                  Fore  => Foreground,
                  Back  => Background,
                  Chars =>
                    Conversion (Current (Index .. Index + View.Unit_Size - 1),
                                View.Unit_Size,
                                Text,
                                View.Unit_Size / 2));
               Insert
                 (View.View,
                  Font  => View.View_Font,
                  Fore  => Foreground,
                  Back  => Background,
                  Chars => Data_Separator);
            end loop;
         end if;

         if Line_Index /= View.Number_Of_Lines then
            Insert (View.View, Chars => End_Of_Line);
         end if;
      end loop;

      Thaw (View.View);
      Set_Position (View.View, Gint (View.Cursor_Position));
   end Update_Display;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access GVD_Memory_View_Record'Class;
      Address : Long_Long_Integer)
   is
      Process : constant Debugger_Process_Tab :=
        Get_Current_Process (View.Window);
   begin
      if Memory_View_Register.Register_Post_Cmd_If_Needed
           (Get_Process (Process.Debugger),
            View,
            Display_Memory'Access,
            Address)
      then
         return;
      end if;

      declare
         Values  : String (1 .. 2 * View.Number_Of_Bytes);
      begin

         Values := Get_Memory (Process.Debugger,
                               View.Number_Of_Bytes,
                               "0x"
                               & To_Standard_Base (Address, 16));

         View.Starting_Address := Address;
         Free (View.Values);
         Free (View.Flags);
         View.Values := new String' (Values);
         View.Flags  := new String' (Values);
         View.Data   := Byte;
      end;
      Update_Display (View);

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
      Process      : constant Debugger_Process_Tab :=
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

         Real_Address := Long_Long_Integer'Value
           (Hex_Header
            & Address (Address'First + 2 .. Index - 1)
            & Hex_Footer);
         Display_Memory (View, Real_Address);

      else
         declare
            New_Address : constant String
              := Get_Variable_Address (Process.Debugger, Address);
         begin
            if New_Address'Length > 2
              and then New_Address
              (New_Address'First .. New_Address'First + 1) = "0x"
            then
               Display_Memory (View, New_Address);
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
      if Get_Endian_Type (Get_Current_Process (View.Window).Debugger)
        = Little_Endian
      then
         Swap_Blocks (View, View.Data);
      end if;

      for J in 1 .. View.Number_Of_Bytes loop
         if View.Flags (J * 2 - 1 .. J * 2)
           /= View.Values (J * 2 - 1 .. J * 2)
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

      Refresh_Canvas
        (Interactive_Canvas (Get_Current_Process (View.Window).Data_Canvas));
   end Apply_Changes;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out GVD_Memory_View;
      Window : in Gtk_Widget) is
   begin
      View := new GVD_Memory_View_Record;
      Initialize (View);
      Init_Graphics (View, Get_Window (Window));
      View.Window := Window;
      Set_Line_Wrap (View.View, False);
   end Gtk_New;

   -------------
   -- Page_Up --
   -------------

   procedure Page_Up (View : access GVD_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address -
          Long_Long_Integer (View.Number_Of_Lines * Line_Base_Size));
   end Page_Up;

   ---------------
   -- Page_Down --
   ---------------

   procedure Page_Down (View : access GVD_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address +
          Long_Long_Integer (View.Number_Of_Lines * Line_Base_Size));
   end Page_Down;

   ------------
   -- Update --
   ------------

   procedure Update
     (View    : access GVD_Memory_View_Record'Class;
      Process : Gtk_Widget)
   is
      Tab : Debugger_Process_Tab := Debugger_Process_Tab (Process);
      use type GNAT.OS_Lib.String_Access;

   begin
      if Tab.Descriptor.Program /= null
        and then Tab.Descriptor.Program.all /= ""
      then
         Set_Label (View.Frame, Tab.Descriptor.Program.all);
      else
         Set_Label (View.Frame, "(no executable)");
      end if;
      Display_Memory (View, View.Starting_Address);
   end Update;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor
     (View  : access GVD_Memory_View_Record'Class;
      Where : in Dir)
   is
      Move       : Gint := 0;
      Position   : constant Gint := Get_Position (View.View);
      ASCII_Size : Integer := 0;

   begin
      if Get_Active (View.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length
           + Line_Base_Size
           + ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      case Where is
         when Right =>
            if Get_Chars (View.View,
                          Position + 1,
                          Position + 1 + Data_Separator'Length)
              = Data_Separator
            then
               --  Are we on the last bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = View.Number_Of_Columns - 1
               then
                  --  Is it the last bloc in the view ?

                  if Position_To_Bloc (View, Position)
                    = View.Number_Of_Columns * View.Number_Of_Lines - 1
                  then
                     Set_Position (View.View, Position - 1);
                  else
                     Set_Position
                       (View.View,
                        Position
                        + Gint (Address_Length)
                        + Address_Separator'Length
                        + Data_Separator'Length
                        + Gint (ASCII_Size)
                        + End_Of_Line'Length);
                  end if;
               else
                  Set_Position
                    (View.View, Position + Data_Separator'Length);
               end if;

            end if;

            if Get_Chars (View.View, Position + 2, Position + 3) (1)
              = End_Of_Line (End_Of_Line'First)
            then
               Move := 21;
            end if;

         when Left =>

            if Get_Chars (View.View, Position
                          - Data_Separator'Length, Position)
              = Data_Separator
            then
               --  Are we on the first bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = 0
               then
                  --  Is it the first bloc in the view ?

                  if Position_To_Bloc (View, Position) = 0 then
                     Set_Position (View.View, Position + 1);
                  else
                     Set_Position
                       (View.View,
                        Position
                        - Gint (Address_Length)
                        - Address_Separator'Length
                        - Data_Separator'Length
                        - Gint (ASCII_Size)
                        - End_Of_Line'Length);
                  end if;
               else
                  Set_Position (View.View, Position - Data_Separator'Length);
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
      Prefix      : String (1 .. 3);
      Success     : Boolean;
      Background  : Gdk_Color := Null_Color;
      Value_Index : Integer;
      Position    : constant Gint := Get_Position (View.View);
      Bloc_Begin  : Gint := Position;
      Bloc_End    : Gint := Position;

   begin
      if View.View = null or else Get_Length (View.View) <= 0 then
         return;
      end if;

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

      Freeze (View.View);
      Set_Point (View.View, Guint (Position));
      Success := Forward_Delete (View.View, 1);
      Insert (View.View,
              Font => View.View_Font,
              Back => Background,
              Fore => View.Modified_Color,
              Chars => Char);

      while Get_Chars (View.View,
                       Bloc_Begin - 1,
                       Bloc_Begin)
        /= Data_Separator (Data_Separator'Last .. Data_Separator'Last)
      loop
         Bloc_Begin := Bloc_Begin - 1;
      end loop;

      while Get_Chars (View.View,
                       Bloc_End,
                       Bloc_End + 1)
        /= Data_Separator (Data_Separator'First .. Data_Separator'First)
      loop
         Bloc_End := Bloc_End + 1;
      end loop;

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

            View.Flags (Value_Index .. Value_Index + 1)
              := To_Standard_Base (Long_Long_Integer
                                   (Character'Pos (Char (Char'First))),
                                   16,
                                   2);
         end;
      else
         Value_Index :=
           Position_To_Bloc (View, Get_Position (View.View))
           * Line_Base_Size / View.Number_Of_Columns * 2 + 1;

         declare
            S : String := Get_Chars (View.View, Bloc_Begin, Bloc_End);
         begin
            if View.Flags (Value_Index .. Value_Index)
              /= Non_Valid_Character
            then
               View.Flags (Value_Index .. Value_Index + View.Unit_Size - 1) :=
                 To_Standard_Base
                 (Long_Long_Integer'Value (Prefix & S & Hex_Footer),
                  16,
                  View.Unit_Size);
            end if;

         end;
      end if;

      Update_Display (View);
      Thaw (View.View);
      Set_Position (View.View, Position);
      Move_Cursor (View, Right);
      Set_Position (View.View, Get_Position (View.View) + 1);
   end Insert;

end GVD.Memory_View;

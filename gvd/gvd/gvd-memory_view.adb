-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Text;         use Gtk.Text;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Frame;        use Gtk.Frame;
with Gtkada.Canvas;    use Gtkada.Canvas;

with Debugger;              use Debugger;
with Memory_View_Pkg;       use Memory_View_Pkg;

with GVD.Strings;     use GVD.Strings;
with Odd.Types;       use Odd.Types;
with Odd.Process;     use Odd.Process;
with Odd.Preferences; use Odd.Preferences;
with Process_Proxies; use Process_Proxies;

package body Odd.Memory_View is

   --------------------
   -- Local packages --
   --------------------

   package Long_Int_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   use Long_Int_IO;

   package Memory_View_Register is new Register_Generic
     (Long_Long_Integer, Odd_Memory_View_Record);

   ---------------------
   -- Local constants --
   ---------------------

   Address_Length    : constant Integer := 16;
   Address_Separator : constant String := ": ";
   Data_Separator    : constant String := " ";
   End_Of_Line       : constant String := ASCII.CR & ASCII.LF;
   Hex_Header        : constant String := "16#";
   Hex_Footer        : constant String := "#";
   Scrollbar_Size    : constant Integer := 25;
   Column_Base_Num   : constant Integer := 16;

   View_Font      : Gdk_Font;
   View_Color     : Gdk_Color;
   Highlighted    : Gdk_Color;
   Selected       : Gdk_Color;
   White_Color    : Gdk_Color;
   Modified_Color : Gdk_Color;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Highlighted
     (View     : access Odd_Memory_View_Record'Class;
      Position : Gint) return Boolean;
   --  Tells whether a given position in the view should be highlighted or not.

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

   procedure Clear_View (View : access Odd_Memory_View_Record'Class);
   --  Removes everything from the view.

   --------------------
   -- Is_Highlighted --
   --------------------

   function Is_Highlighted
     (View     : access Odd_Memory_View_Record'Class;
      Position : Gint) return Boolean
   is
      Row_Length : Integer;
      Row        : Integer;
      Column     : Integer;
   begin
      Row_Length := (Address_Length
                     + Address_Separator'Length
                     + (View.Number_Of_Columns
                        * (View.Trunc + Data_Separator'Length))
                     + End_Of_Line'Length);

      Row := Integer (Position) / Row_Length;

      Column := Integer (Position)
        - Row * Row_Length
        - (Address_Length + Address_Separator'Length);

      if (Column / (View.Trunc + 1) mod 2) = 1 then
         return True;
      else
         return False;
      end if;
   end Is_Highlighted;

   -----------------------
   -- Position_To_Index --
   -----------------------

   function Position_To_Index
     (View     : access Odd_Memory_View_Record'Class;
      Position : in Gint) return Integer
   is
      Row_Length : Integer;
      Row        : Integer;
      Column     : Integer;
      Unit       : Integer;

   begin
      Row_Length := (Address_Length
                     + Address_Separator'Length
                     + (View.Number_Of_Columns
                        * (View.Trunc + Data_Separator'Length))
                     + End_Of_Line'Length);

      Row := Integer (Position) / Row_Length;

      Column := Integer (Position)
        - Row * Row_Length
        - (Address_Length + Address_Separator'Length);

      Unit := Column / (View.Trunc + Data_Separator'Length) +
        Row * View.Number_Of_Columns;

      return Unit * View.Unit_Size + 1;
   end Position_To_Index;

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
      Skip_To_String (S, Test, "-");

      if Test < S'Last then
         if Trunc_At /= -1 then
            return Dummy (1 .. Trunc_At);
         else
            return "-";
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

   procedure Clear_View (View : access Odd_Memory_View_Record'Class) is
   begin
      Delete_Text (View.View);
   end Clear_View;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics (Window : Gdk_Window) is
      Success : Boolean;
   begin
      View_Font  := Get_Gdkfont (Memory_View_Font_Name, Memory_View_Font_Size);
      View_Color := Parse (Memory_View_Color);

      Alloc_Color (Get_System, View_Color, True, True, Success);

      Highlighted := Parse (Memory_Highlighted_Color);
      Alloc_Color (Get_System, Highlighted, True, True, Success);

      White_Color := White (Get_System);

      Selected := Parse (Memory_Selected_Color);
      Alloc_Color (Get_System, Selected, True, True, Success);

      Modified_Color := Parse (Memory_Modified_Color);
      Alloc_Color (Get_System, Modified_Color, True, True, Success);
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
      Mapping : Character_Mapping := To_Mapping ("ABCDEF ", "abcdef0");
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
         return (Translate
                 (Result (Result'Last - Trunc_At .. Result'Last - 1),
                  Mapping));
      end if;
   end To_Standard_Base;

   ---------------------
   --  Update_Display --
   ---------------------

   procedure Update_Display (View : access Odd_Memory_View_Record'Class) is
      Index      : Integer;
      Width      : Gint;
      Height     : Gint;
      Count      : Integer := 0;
      Background : Gdk_Color := Null_Color;
      Foreground : Gdk_Color := Null_Color;
      Current    : String_Access;

   begin
      if View.Values = null then
         return;
      end if;

      View.Data := Data_Size'Value (Get_Text (View.Size_Entry));

      if Get_Text (View.Data_Entry) = "ASCII" then
         View.Display := Text;
      else
         View.Display := Display_Type'Value (Get_Text (View.Data_Entry));
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

      View.Number_Of_Columns :=
        Integer ((Width / Char_Width (View_Font, Character' ('m'))
                  - Gint (Scrollbar_Size)))
        / (View.Trunc + Data_Separator'Length);

      if View.Number_Of_Columns < Column_Base_Num * 2 / View.Unit_Size then
         View.Number_Of_Columns := Column_Base_Num * 2 / View.Unit_Size;
      else
         View.Number_Of_Columns := View.Number_Of_Columns
           - (View.Number_Of_Columns mod (Column_Base_Num * 2
                                          / View.Unit_Size));
      end if;

      Freeze (View.View);
      Clear_View (View);
      Index := 1;

      Insert
        (View.View,
         Fore  => View_Color,
         Back  => Highlighted,
         Font  => View_Font,
         Chars => To_Standard_Base (View.Starting_Address, 16,
                                    Address_Length) & Address_Separator);

      while Index + View.Unit_Size - 1 <= View.Number_Of_Bytes * 2 loop
         if Count mod 2 = 0 then
            Background := Null_Color;
         else
            Background := Highlighted;
         end if;

         if View.Values (Index .. Index + View.Unit_Size - 1) /=
           View.Flags (Index .. Index + View.Unit_Size - 1)
         then
            Foreground := Modified_Color;
            Current := View.Flags;
         else
            Foreground := Null_Color;
            Current := View.Values;
         end if;

         if View.Cursor_Index >= Index
           and then View.Cursor_Index < Index + View.Unit_Size
         then
            View.Cursor_Position := Gint (Get_Length (View.View));
         end if;

         Insert
           (View.View,
            Font  => View_Font,
            Fore  => Foreground,
            Back  => Background,
            Chars =>
              Conversion (Current (Index .. Index + View.Unit_Size - 1),
                          View.Unit_Size,
                          View.Display,
                          View.Trunc));
         Insert
           (View.View,
            Font  => View_Font,
            Fore  => Foreground,
            Back  => Background,
            Chars => Data_Separator);

         Count := Count + 1;

         if ((Index + View.Unit_Size) / View.Unit_Size
             mod View.Number_Of_Columns) = 0
           and then Index + View.Unit_Size * 2 - 1 <= View.Number_Of_Bytes * 2
         then
            Count := 0;
            Insert (View.View, Chars => ASCII.CR & ASCII.LF);
            Insert
              (View.View,
               Fore => View_Color,
               Back => Highlighted,
               Font => View_Font,
               Chars =>
                 To_Standard_Base
               (View.Starting_Address
                + Long_Long_Integer
                ((Index + View.Unit_Size) / 2), 16, Address_Length)
               & Address_Separator);
         end if;

         Index := Index + View.Unit_Size;
      end loop;

      Thaw (View.View);
      Set_Position (View.View, Gint (View.Cursor_Position));
   end Update_Display;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access Odd_Memory_View_Record'Class;
      Address : Long_Long_Integer)
   is
      Process : constant Debugger_Process_Tab
        := Get_Current_Process (View.Window);
      Values  : String (1 .. 2 * View.Number_Of_Bytes);
   begin
      if Memory_View_Register.Register_Post_Cmd_If_Needed
        (Get_Process (Process.Debugger),
         View,
         Display_Memory'Access,
         Address)
      then
         return;
      end if;

      Values := Get_Memory (Process.Debugger,
                            View.Number_Of_Bytes,
                            "0x"
                            & To_Standard_Base (Address, 16));

      View.Starting_Address := Address;
      Free (View.Values);
      Free (View.Flags);
      View.Values := new String' (Values);
      View.Flags  := new String' (Values);
      Update_Display (View);
   end Display_Memory;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View : access Odd_Memory_View_Record'Class;
      Address : String)
   is
      Real_Address : Long_Long_Integer;
      Index        : Integer;
      Process : constant Debugger_Process_Tab :=
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

   procedure Apply_Changes (View : access Odd_Memory_View_Record'Class) is
   begin
      for J in 1 .. View.Number_Of_Bytes loop
         Put_Memory_Byte
           (Get_Current_Process (View.Window).Debugger,
            "0x" &
              To_Standard_Base
                (View.Starting_Address + Long_Long_Integer (J - 1), 16),
            View.Flags (J * 2 - 1 .. J * 2));
      end loop;

      Free (View.Values);
      View.Values := new String' (View.Flags.all);
      Update_Display (View);
      Refresh_Canvas
        (Interactive_Canvas (Get_Current_Process (View.Window).Data_Canvas));
   end Apply_Changes;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Odd_Memory_View;
      Window : in Gtk_Widget) is
   begin
      View := new Odd_Memory_View_Record;
      Initialize (View);
      Init_Graphics (Get_Window (Window));
      View.Window := Window;
      Set_Line_Wrap (View.View, False);
   end Gtk_New;

   -------------
   -- Page_Up --
   -------------

   procedure Page_Up (View : access Odd_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address -
          Long_Long_Integer (View.Number_Of_Bytes));
   end Page_Up;

   ---------------
   -- Page_Down --
   ---------------

   procedure Page_Down (View : access Odd_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address +
          Long_Long_Integer (View.Number_Of_Bytes));
   end Page_Down;

   ------------
   -- Update --
   ------------

   procedure Update
     (View : access Odd_Memory_View_Record'Class;
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
     (View : access Odd_Memory_View_Record'Class;
      Where : in Dir)
   is
      Move     : Gint := 0;
      Position : Gint := Get_Position (View.View);

   begin
      case Where is
         when Right =>
            if Position > Gint (Get_Length (View.View)) - 3 then
               Move := -1;
            else
               if Get_Chars (View.View, Position + 1, Position + 2)
                 = Data_Separator
               then
                  Move := 1;
               end if;
               if Get_Chars
                 (View.View, Position + 2, Position + 3) (1) = ASCII.CR
               then
                  Move := 21;
               end if;
            end if;

         when Left =>
            if Get_Chars (View.View, Position - 1, Position)
              = Data_Separator
            then
               if Position_To_Index (View, Position)
                 mod View.Number_Of_Columns = 1
               then
                  if Position_To_Index (View, Position) = 1 then
                     Move := 1;
                  else
                     Move := -21;
                  end if;
               else
                  Move := -1;
               end if;
            end if;
         when others =>
            null;
      end case;

      Set_Position (View.View, Position + Move);
   end Move_Cursor;


   ------------
   -- Insert --
   ------------

   procedure Insert
     (View : access Odd_Memory_View_Record'Class;
      Char : String)
   is
      Prefix      : String (1 .. 3);
      Success     : Boolean;
      Background  : Gdk_Color := Null_Color;
      Value_Index : Integer;

   begin
      if View.View = null then
         return;
      end if;

      Value_Index := Position_To_Index (View, Get_Position (View.View));
      View.Cursor_Position := Get_Position (View.View);
      View.Cursor_Index := Position_To_Index (View, View.Cursor_Position);

      if Get_Position (View.View) > Gint (Get_Length (View.View) - 2)
        or else Get_Chars (View.View,
                           Get_Position (View.View),
                           Get_Position (View.View) + 1) = "-"
      then
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

      Success := Forward_Delete (View.View, 1);

      if Is_Highlighted (View, Get_Position (View.View)) then
         Background := Highlighted;
      end if;

      declare
         Index      : Integer := 1;
         Next_Index : Integer := 1;
         Current    : String (1 .. 2 * (View.Trunc + Data_Separator'Length));

      begin
         --  Insert new char.

         Insert (View.View,
                 Font => View_Font,
                 Back => Background,
                 Fore => Modified_Color,
                 Chars => Char);

         --  Grab the new value
         Set_Position (View.View, Get_Position (View.View) - 1);

         Current := Get_Chars (View.View,
                               Get_Position (View.View)
                               - Gint (View.Trunc) - 2,
                               Get_Position (View.View)
                               + Gint (View.Trunc));
         Skip_To_String (Current, Index, Data_Separator);
         Next_Index := Index + 1;
         Skip_To_String (Current, Next_Index, Data_Separator);

         --  Modify flags string to match the new value.
         View.Flags (Value_Index .. Value_Index + View.Unit_Size - 1) :=
           (To_Standard_Base
             (Long_Long_Integer'Value
               (Prefix & Current (Index + 1 .. Next_Index - 1) & Hex_Footer),
              16, View.Unit_Size));
      end;

      Move_Cursor (View, Right);
      Set_Position (View.View, Get_Position (View.View) + 1);
   end Insert;

end Odd.Memory_View;

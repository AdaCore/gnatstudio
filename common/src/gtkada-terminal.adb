-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2008-2008, AdaCore                 --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Unicode;         use Glib.Unicode;
with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with System;               use System;
with GNATCOLL.Traces;      use GNATCOLL.Traces;

package body Gtkada.Terminal is
   Me : constant Trace_Handle := Create ("TERM");

   type c_char_array is array (size_t) of char;
   for c_char_array'Component_Size use CHAR_BIT;

   type c_char_array_access is access c_char_array;

   function UC is new Ada.Unchecked_Conversion
     (System.Address, c_char_array_access);

   type Insert_Callback is access procedure
     (Widget : System.Address;
      Pos    : access Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : System.Address;
      Length : Gint);
   pragma Convention (C, Insert_Callback);

   type Capability is
     (Self_Insert,
      Do_Nothing,
      Insert_Lines,              --  "AL"
      Delete_Chars,              --  "DC"
      End_Alternative_Charset,   --  "ae"
      Start_Alternative_Charset, --  "as"
      Delete_Lines,              --  "DL"
      Cursor_Down,               --  "DO"
      Insert_Chars,              --  "IC"
      Keypad_Center_Key,         --  "K2"
      Cursor_Left_Multiple,      --  "LE"
      Cursor_Right_Multiple,     --  "RI"
      Normal_Scroll,             --  "SF"
      Scroll_Back,               --  "SR"
      Cursor_Up_Multiple,        --  "UP"
      Insert_One_Line,           --  "al"
      Audio_Bell,                --  "bl"
      Move_To_Prev_Tab,          --  "bt"
      Clear_To_End_Of_Screen,    --  "cd"
      Clear_To_End_Of_Line,      --  "ce"
      Clear_Screen_And_Home,     --  "cl"
      Move_Cursor,               --  "cm"
      Scroll_Region,             --  "cs"
      Clear_Tabs,                --  "ct"
      Delete_One_Char,           --  "dc"
      Delete_One_Line,           --  "dl"
      Erase_Chars_At_Cursor,     --  "ec"
      End_Insert_Mode,           --  "ei"
      Cursor_Home,               --  "ho"
      Begin_Insert_Mode,         --  "im"
      Initialize_String,         --  "is"
      Function_1,                --  "k1"
      Function_2,                --  "k2"
      Function_3,                --  "k3"
      Function_4,                --  "k4"
      Function_5,                --  "k5"
      Function_6,                --  "k6"
      Function_7,                --  "k7"
      Function_8,                --  "k8"
      Function_9,                --  "k9"
      Key_For_Delete_Char,       --  "kD"
      Key_Insert_Mode,           --  "kI"
      Key_Next_Page,             --  "kN"
      Key_Prev_Page,             --  "kP"
      Key_Cursor_Down,           --  "kd"
      Turn_Keypad_Off,           --  "ke"
      Key_Cursor_Home,           --  "kh"
      Key_Cursor_Left,           --  "kl"
      Key_Cursor_Right,          --  "kr"
      Turn_Keypad_On,            --  "ks"
      Key_Cursor_Up,             --  "ku"
      Start_Blinking,            --  "mb"
      Start_Bold_Mode,           --  "md"
      End_All_Modes,             --  "me"
      Meta_Mode_On,              --  "mm"
      Meta_Mode_Off,             --  "mo"
      Start_Reverse_Mode,        --  "mr"
      Cursor_Right,              --  "nd"
      Restore_Saved_Position,    --  "rc"
      Save_Position,             --  "sc"
      End_Standout_Mode,         --  "se"
      Start_Standout_Mode,       --  "so"
      Reverse_Scroll,            --  "sr"
      Set_Tabulator_Stop,        --  "st"
      End_Program_Using_Cursor,  --  "te"
      Begin_Program_Using_Cursor, -- "ti"
      End_Underlining,            -- "ue"
      Cursor_Up,                  -- "up"
      Start_Underlining,          -- "us"
      Visible_Bell,               -- "vb"
      Normal_Cursor_Visible,      -- "ve"
      Cursor_Invisible,           -- "vi"
      Standout_Cursor,            -- "vs"
      Memory_Unlock,
      Memory_Lock,
      Set_Char_Attribute,
      Display_In_Status_Line);

   type FSM_Transition;
   type FSM_Transition_Access is access FSM_Transition;
   type FSM_State is record
      Callback       : Capability;

      Any_Number     : FSM_Transition_Access;
      --  State reached after reading a %d. If set, this will not check for
      --  '0' .. '9' entries in Transitions table

      String_Stopper : char := char'Val (127);
      --  If this is set to a value other than 127, start reading a whole
      --  string until String_Stopper is encountered. The string read acts as
      --  an argument to the capability. When the end of the string is reached,
      --  Capability is executed.
      --  eg/   a%sb
      --         FSM.Transitions ('a') :=
      --           (string_stopped := 'b',
      --            Callback := ...)

      Transitions : FSM_Transition_Access;
   end record;
   pragma Pack (FSM_State);
   Null_State : constant FSM_State :=
     (Self_Insert, null, char'Val (127), null);
   subtype Escape_Chars is char range nul .. char'Val (127);
   type FSM_Transition is array (Escape_Chars) of FSM_State;

   type GtkAda_Terminal_Class is record
      C_Class : GObject_Class := Uninitialized_Class;
      Default_Insert_Callback : Insert_Callback := null;
      FSM     : FSM_Transition_Access;
   end record;
   Class : GtkAda_Terminal_Class;

   Alternate_Charset : constant array (Character) of Gunichar :=
     ('+'  => 16#2192#, --  arrow pointing right
      ','  => 16#2190#, --  arrow pointing left
      '-'  => 16#2191#, --  arrow pointing up
      '.'  => 16#2193#, --  arrow pointing down
      '0'  => 16#2588#, --  solid square block
      '`'  => 16#25C6#, --  diamond
      'a'  => 16#2592#, --  checker board (stipples)
      'b'  => 16#2409#, --  HT symbol
      'c'  => 16#240C#, --  FF symbol
      'd'  => 16#240D#, --  CR symbol
      'e'  => 16#240A#, --  LF symbol
      'f'  => 16#00B0#, --  degree symbol
      'g'  => 16#00B1#, --  plus/minus
      'h'  => 16#2424#, --  board of squares
      'i'  => 16#240B#, --  lantern symbol
      'j'  => 16#2518#, --  lower right corner
      'k'  => 16#2510#, --  upper right corner
      'l'  => 16#250C#, --  upper left corner
      'm'  => 16#2514#, --  lower left corner
      'n'  => 16#253C#, --  large plus or crossover
      'o'  => 16#23BA#, --  scan line 1
      'p'  => 16#23BB#, --  scan line 3
      'q'  => 16#2500#, --  horizontal line
      'r'  => 16#23BC#, --  scan line 7
      's'  => 16#23BD#, --  scan line 9
      't'  => 16#251C#, --  tee pointing right
      'u'  => 16#2524#, --  tee pointing left
      'v'  => 16#2534#, --  tee pointing up
      'w'  => 16#252C#, --  tee pointing down
      'x'  => 16#2502#, --  vertical line
      'y'  => 16#2264#, --  less-than-or-equal-to
      'z'  => 16#2265#, --  greater-than-or-equal-to
      '{'  => 16#03C0#, --  greek pi
      '|'  => 16#2260#, --  not-equal
      '}'  => 16#00A3#, --  UK pound sign
      '~'  => 16#00B7#, --  bullet
      others => 0
      );
   --  Translates one set of characters into unicode. This is the alternate
   --  charset mode used by terminals to display graphical 8bit characters

   procedure On_Insert_Text
     (Widget : System.Address;
      Pos    : access Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : System.Address;
      Length : Gint);
   pragma Convention (C, On_Insert_Text);
   --  Callback to insert text in the buffer. This replaces the predefined
   --  version in the gtk+ source code

   procedure Add_Sequence
     (FSM      : in out FSM_Transition_Access;
      Sequence : String;
      Func     : Capability);
   --  Add an extra sequence to a finite state machine

   function Parse_Xterm_Termcap return FSM_Transition_Access;
   --  Create a state machine to analyze an xterm's escape sequences

   ------------------
   -- Add_Sequence --
   ------------------

   procedure Add_Sequence
     (FSM      : in out FSM_Transition_Access;
      Sequence : String;
      Func     : Capability) is
   begin
      if FSM = null then
         FSM := new FSM_Transition'(others => Null_State);
      end if;

      Assert (Me,
              char (Sequence (Sequence'First)) in Escape_Chars,
              "Invalid sequence: " & Sequence);

      if Sequence'Length = 1 then
         FSM (char (Sequence (Sequence'First))).Callback := Func;
      else
         --  If the current char is followed by any string

         if Sequence'Length >= 4
           and then Sequence (Sequence'First + 1) = '%'
           and then Sequence (Sequence'First + 2) = 's'
         then
            FSM (char (Sequence (Sequence'First))).Callback := Func;
            FSM (char (Sequence (Sequence'First))).String_Stopper :=
              char (Sequence (Sequence'First + 3));

         --  If the current char can be followed by any digit

         elsif Sequence'Length >= 3
           and then Sequence (Sequence'First + 1) = '%'
           and then Sequence (Sequence'First + 2) = 'd'
         then
            Add_Sequence (FSM (char (Sequence (Sequence'First))).Any_Number,
                          Sequence (Sequence'First + 3 .. Sequence'Last),
                          Func);

         else
            Add_Sequence (FSM (char (Sequence (Sequence'First))).Transitions,
                          Sequence (Sequence'First + 1 .. Sequence'Last),
                          Func);
         end if;
      end if;
   end Add_Sequence;

   -------------------------
   -- Parse_Xterm_Termcap --
   -------------------------

   function Parse_Xterm_Termcap return FSM_Transition_Access is
      FSM : FSM_Transition_Access;
   begin
      Add_Sequence (FSM, ASCII.ESC & "[%dL", Insert_Lines);
      Add_Sequence (FSM, ASCII.ESC & "[%dP", Delete_Chars);
      Add_Sequence (FSM, ASCII.ESC & "[%dM", Delete_Lines);
      Add_Sequence (FSM, ASCII.ESC & "[%dB", Cursor_Down);
      Add_Sequence (FSM, ASCII.ESC & "[%d@", Insert_Chars);
      Add_Sequence (FSM, ASCII.ESC & "OE",   Keypad_Center_Key);
      Add_Sequence (FSM, ASCII.ESC & "[%dD", Cursor_Left_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[%dC", Cursor_Right_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[%dS", Normal_Scroll);
      Add_Sequence (FSM, ASCII.ESC & "[%dT", Scroll_Back);
      Add_Sequence (FSM, ASCII.ESC & "[%dA", Cursor_Up_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "(B",   End_Alternative_Charset);
      Add_Sequence (FSM, ASCII.ESC & "[L",   Insert_One_Line);
      Add_Sequence (FSM, ASCII.ESC & "(0",   Start_Alternative_Charset);
      Add_Sequence (FSM, ASCII.BEL & "",     Audio_Bell);
      Add_Sequence (FSM, ASCII.ESC & "[Z",   Move_To_Prev_Tab);
      Add_Sequence (FSM, ASCII.ESC & "[J",   Clear_To_End_Of_Screen);
      Add_Sequence (FSM, ASCII.ESC & "[K",   Clear_To_End_Of_Line);
      Add_Sequence (FSM, ASCII.ESC & "[H" & ASCII.ESC & "[2J",
                    Clear_Screen_And_Home);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dH", Move_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dr", Scroll_Region);
      Add_Sequence (FSM, ASCII.ESC & "[3g",     Clear_Tabs);
      Add_Sequence (FSM, ASCII.ESC & "[P",      Delete_One_Char);
      Add_Sequence (FSM, ASCII.ESC & "[M",      Delete_One_Line);
      Add_Sequence (FSM, ASCII.ESC & "[%dX",    Erase_Chars_At_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[4l",     End_Insert_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[H",      Cursor_Home);
      Add_Sequence (FSM, ASCII.ESC & "[4h",     Begin_Insert_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[!p"
                    & ASCII.ESC & "[?3;4l"
                    & ASCII.ESC & "[41"
                    & ASCII.ESC & ">",           Initialize_String);
      Add_Sequence (FSM, ASCII.ESC & "OP",       Function_1);
      Add_Sequence (FSM, ASCII.ESC & "OQ",       Function_2);
      Add_Sequence (FSM, ASCII.ESC & "OR",       Function_3);
      Add_Sequence (FSM, ASCII.ESC & "OS",       Function_4);
      Add_Sequence (FSM, ASCII.ESC & "[15~",     Function_5);
      Add_Sequence (FSM, ASCII.ESC & "[17~",     Function_6);
      Add_Sequence (FSM, ASCII.ESC & "[18~",     Function_7);
      Add_Sequence (FSM, ASCII.ESC & "[19~",     Function_8);
      Add_Sequence (FSM, ASCII.ESC & "[20~",     Function_9);
      Add_Sequence (FSM, ASCII.ESC & "[3~",      Key_For_Delete_Char);
      Add_Sequence (FSM, ASCII.ESC & "[2~",      Key_Insert_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[6~",      Key_Next_Page);
      Add_Sequence (FSM, ASCII.ESC & "[5~",      Key_Prev_Page);
      Add_Sequence (FSM, ASCII.ESC & "OB",       Key_Cursor_Down);
      Add_Sequence (FSM, ASCII.ESC & "[?1l"
                    & ASCII.ESC & ">",           Turn_Keypad_Off);
      Add_Sequence (FSM, ASCII.ESC & "OH",       Key_Cursor_Home);
      Add_Sequence (FSM, ASCII.ESC & "OD",       Key_Cursor_Left);
      Add_Sequence (FSM, ASCII.ESC & "OC",       Key_Cursor_Right);
      Add_Sequence (FSM, ASCII.ESC & "[?1h"
                    & ASCII.ESC & "=",           Turn_Keypad_On);
      Add_Sequence (FSM, ASCII.ESC & "OA",       Key_Cursor_Up);
      Add_Sequence (FSM, ASCII.ESC & "[5m",      Start_Blinking);
      Add_Sequence (FSM, ASCII.ESC & "[1m",      Start_Bold_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[0m",      End_All_Modes);
      Add_Sequence (FSM, ASCII.ESC & "[?1034h",  Meta_Mode_On);
      Add_Sequence (FSM, ASCII.ESC & "[?1034l",  Meta_Mode_Off);
      Add_Sequence (FSM, ASCII.ESC & "[7m",      Start_Reverse_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[C",       Cursor_Right);
      Add_Sequence (FSM, ASCII.ESC & "8",        Restore_Saved_Position);
      Add_Sequence (FSM, ASCII.ESC & "7",        Save_Position);
      Add_Sequence (FSM, ASCII.ESC & "[27m",     End_Standout_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[7m",      Start_Standout_Mode);
      Add_Sequence (FSM, ASCII.ESC & "M",        Reverse_Scroll);
      Add_Sequence (FSM, ASCII.ESC & "H",        Set_Tabulator_Stop);
      Add_Sequence (FSM, ASCII.ESC & "[?1049l",  End_Program_Using_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[?1049h",  Begin_Program_Using_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[24m",     End_Underlining);
      Add_Sequence (FSM, ASCII.ESC & "[A",       Cursor_Up);
      Add_Sequence (FSM, ASCII.ESC & "[4m",      Start_Underlining);
      Add_Sequence (FSM, ASCII.ESC & "[?5h"
                    & ASCII.ESC & "[?51",        Visible_Bell);
      Add_Sequence (FSM, ASCII.ESC & "[?12l"
                    & ASCII.ESC & "[?25h",       Normal_Cursor_Visible);
      Add_Sequence (FSM, ASCII.ESC & "[?25l",    Cursor_Invisible);
      Add_Sequence (FSM, ASCII.ESC & "[?12;25h", Standout_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[l",       Memory_Lock);
      Add_Sequence (FSM, ASCII.ESC & "[m",       Memory_Unlock);

      --  No meaning in GUI mode (?)
      --  do=^J            Cursor down one line
      --  kb=^H                        Backspace key
      --  le=^H                        Cursor left one character
      --  sf=^J                        Normal scroll one line
      --  ta=^I                        Move to next hardware tab

      --  Not in termcap, only in terminfo apparently
      Add_Sequence (FSM, ASCII.ESC & "]0;%s" & ASCII.BEL,
                    Display_In_Status_Line);
      Add_Sequence (FSM, ASCII.ESC & "[%dm", Set_Char_Attribute);

      return FSM;
   end Parse_Xterm_Termcap;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
     (Widget : System.Address;
      Pos    : access Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : System.Address;
      Length : Gint)
   is
      Stub : GtkAda_Terminal_Record;
      Term : constant GtkAda_Terminal :=
        GtkAda_Terminal (Get_User_Data (Widget, Stub));
      Txt  : constant c_char_array_access := UC (Text);

      procedure Insert_Substr (Frm, To : size_t);
      --  Write a specific substring to the buffer

      function To_String (Frm, To : size_t) return String;
      --  Return the specified substring of Text

      procedure Perform (Func : Capability);
      --  Perform an action requested by the user

      ---------------
      -- To_String --
      ---------------

      function To_String (Frm, To : size_t) return String is
         S : String (Integer (Frm + 1) .. Integer (To + 1));
      begin
         for C in S'Range loop
            S (C) := Character (Txt (size_t (C) - 1));
            if S (C) = ASCII.ESC then
               S (C) := '^';
            end if;
         end loop;
         return S;
      end To_String;

      -------------------
      -- Insert_Substr --
      -------------------

      procedure Insert_Substr (Frm, To : size_t) is
      begin
         if Term.Alternate_Charset then
            --  We need to translate the string into the alternate charset.
            --  These might be unicode chars encoded into multiple bytes. We
            --  therefore convert things into block (to try and reduce the
            --  number of calls to gtk+'s insertion procedure while having
            --  the capability to translate one incoming char into multiple
            --  unicode bytes).
            declare
               S     : aliased String (1 .. 1024);
               Index : Natural := S'First;
               Uni   : Gunichar;
            begin
               for C in Frm .. To loop
                  Uni := Alternate_Charset (Character (Txt (C)));
                  if Uni < 127 then
                     S (Index) := Character'Val (Uni);
                     Index := Index + 1;
                  else
                     Unichar_To_UTF8 (Uni, S (Index .. S'Last), Index);
                     Index := Index + 1;
                  end if;

                  --  Is the block full ?
                  if Index >= S'Last - 4 then
                     Class.Default_Insert_Callback
                       (Widget, Pos, S'Address, Gint (Index - 1));
                     Index := S'First;
                     Trace (Me, S (S'First .. Index - 1));
                  end if;
               end loop;

               if Index > S'First then
                  Class.Default_Insert_Callback
                    (Widget, Pos, S'Address, Gint (Index - 1));
                  Trace (Me, S (S'First .. Index - 1));
               end if;
            end;

         else
            Class.Default_Insert_Callback
              (Widget, Pos, Txt (Frm .. To)'Address, Gint (To - Frm + 1));
            Trace (Me, To_String (Frm, To));
         end if;
      end Insert_Substr;

      -------------
      -- Perform --
      -------------

      procedure Perform (Func : Capability) is
      begin
         case Func is
            when Start_Alternative_Charset =>
               Term.Alternate_Charset := True;
            when End_Alternative_Charset =>
               Term.Alternate_Charset := False;
            when others =>
               Trace (Me, "Unhandled capability: " & Func'Img);
         end case;
      end Perform;

      Current           : FSM_Transition_Access := Class.FSM;
      C                 : size_t := 0;
      Param_Start       : size_t;
      Stopper           : char;
      Arg1              : Integer;

      Start_Of_Sequence : size_t := 0;
      --  The last char for which current was Class.FSM, ie the last
      --  self-insert char. This is used to rollback when an escape sequence
      --  could not be interpreted after all.

--        Str : Unbounded_String;

   begin
--        for S in 0 .. size_t (Length - 1) loop
--           Append (Str, char'Image (Txt (S)));
--        end loop;
--        Trace (Me, To_String (Str));

      if Current = null then
         --  No special terminal setup ? Send the string as is
         Insert_Substr (Txt'First, Txt'First + size_t (Length) - 1);
         return;
      end if;

      while C < size_t (Length) loop
         if Txt (C) in Escape_Chars then
            if Current (Txt (C)).String_Stopper /= char'Val (127) then
               --  Read a string parameter
               Param_Start := C + 1;
               Stopper := Current (Txt (C)).String_Stopper;
               while C < size_t (Length)
                 and then Txt (C) /= Stopper
               loop
                  C := C + 1;
               end loop;

               --  A command with a string parameter
               Trace (Me, Current (Txt (Param_Start - 1)).Callback'Img
                      & " (" & To_String (Param_Start, C - 1) & ")");
               Current := Class.FSM;
               Start_Of_Sequence := C + 1;

            elsif Current (Txt (C)).Callback /= Self_Insert then
               --  A special command was found
               Perform (Current (Txt (C)).Callback);
               Current := Class.FSM;
               Start_Of_Sequence := C + 1;

            elsif Current (Txt (C)).Any_Number /= null
              and then C < size_t (Length) - 1
              and then Txt (C + 1) in '0' .. '9'
            then
               C := C + 1;
               Param_Start := C;
               while C < size_t (Length)
                 and then Txt (C) in '0' .. '9'
               loop
                  C := C + 1;
               end loop;

               Arg1 := Integer'Value (To_String (Param_Start, C - 1));
               Trace (Me, "Read argument:" & Arg1'Img);
               Current := Current (Txt (Param_Start - 1)).Any_Number;

               C := C - 1;  --  So that we test the character after %d

            elsif Current (Txt (C)).Transitions /= null then
               --  In the middle of an escape sequence
               Current := Current (Txt (C)).Transitions;

            else
               --  A self-insert character
               Insert_Substr (Start_Of_Sequence, C);
               Start_Of_Sequence := C + 1;
               Current := Class.FSM;
            end if;
         else
            Insert_Substr (Start_Of_Sequence, C);
         end if;

         C := C + 1;
      end loop;
   end On_Insert_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out GtkAda_Terminal) is
   begin
      Self := new GtkAda_Terminal_Record;
      Gtkada.Terminal.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : access GtkAda_Terminal_Record'Class)
   is
      function Replace_Insert_Text
        (Class : GObject_Class; Func : Insert_Callback)
         return Insert_Callback;
      pragma Import (C, Replace_Insert_Text, "replace_insert_text");

   begin
      Gtk.Text_Buffer.Initialize (Self);
      Initialize_Class_Record
        (Object       => Self,
         Signals      => (1 .. 0 => Null_Ptr),
         Class_Record => Class.C_Class,
         Type_Name    => "GtkAdaTerminal",
         Parameters   => Null_Parameter_Types);

      if Class.Default_Insert_Callback = null then
         Class.Default_Insert_Callback := Replace_Insert_Text
           (Class.C_Class, On_Insert_Text'Access);
         Class.FSM := Parse_Xterm_Termcap;

         --  Initialize charset tables
         for C in Alternate_Charset'Range loop
            if Alternate_Charset (C) = 0 then
               Alternate_Charset (C) := Character'Pos (C);
            end if;
         end loop;
      end if;
   end Initialize;

end Gtkada.Terminal;

--        function Parse_Escape_Sequence (Str : String) return Integer;
--        function Parse_Escape_Sequence (Str : String) return Integer is
--           Index : Integer := Str'First + 1;  --  Skip ASCII.ESC
--           Arg1  : Long_Integer;
--        begin
--           case Str (Index) is
--              when '['  =>
--                 Index := Index + 1;
--                 case Str (Index) is
--                    when 'H' =>
--                       if Str (Index + 1 .. Index + 5) =
--                         ASCII.ESC & "[2J"
--                       then
--                          Index := Index + 5;
--                          Trace (Me, "MANU clear screeen and cursor home");
--                       end if;
--
--                    when 'J' =>
--                       Trace (Me, "MANU Clear to end of screen");
--                    when 'K' =>
--                       Trace (Me, "MANU Clear to end of line");
--                    when 'L' =>
--                       Trace (Me, "MANU (Insert one line)");
--                    when 'Z' =>
--                       Trace (Me, "MANU move to previous tab stop");
--
--                    when '0' .. '9' =>
--                       Parse_Num (Str, Index, Arg1);
--                       case Str (Index) is
--                          when 'A' =>
--                             Trace
--                               (Me, "MANU (cursor up" & Arg1'Img & " lines");
--                          when 'B' =>
--                             Trace
--                             (Me, "MANU (cursor down" & Arg1'Img & " lines");
--                          when 'C' =>
--                             Trace
--                            (Me, "MANU (cursor right" & Arg1'Img & " chars");
--                          when 'D' =>
--                             Trace
--                             (Me, "MANU (cursor left" & Arg1'Img & " chars");
--                          when 'L' =>
--                           Trace (Me, "MANU (insert" & Arg1'Img & " lines)");
--                          when 'M' =>
--                           Trace (Me, "MANU (delete" & Arg1'Img & " lines");
--                          when 'P' =>
--                           Trace (Me, "MANU (delete" & Arg1'Img & " chars");
--                          when 'S' =>
--                             Trace
--                           (Me, "MANU (normal scroll" & Arg1'Img & " lines");
--                          when 'T' =>
--                             Trace
--                             (Me, "MANU (scroll back" & Arg1'Img & " lines");
--                          when '@' =>
--                           Trace (Me, "MANU (insert" & Arg1'Img & " chars");
--                          when others =>
--                             Trace
--                            (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--                       end case;
--
--                    when others =>
--                     Trace (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--                 end case;
--
--              when ']' =>
--                 Index := Index + 1;
--                 case Str (Index) is
--                    when '0' =>
--                       Index := Index + 1;
--                       if Str (Index) = ';' then
--                          null;
--                       end if;
--
--                    when others =>
--                     Trace (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--                 end case;
--
--
--              when '(' =>
--                 Index := Index + 1;
--                 case Str (Index) is
--                    when 'B' =>
--                       Trace (Me, "MANU (end alternative charset");
--
--                    when '0' =>
--                       Trace (Me, "MANU (start alternative charset");
--
--                    when others =>
--                     Trace (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--                 end case;
--
--              when 'O' =>
--                 Index := Index + 1;
--                 case Str (Index) is
--                    when 'E' =>
--                       Trace (Me, "MANU (center key on keypad)");
--                    when others =>
--                     Trace (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--                 end case;
--
--              when others =>
--                 Trace (Me, "MANU ??? " & Str (Str'First + 1 .. Index));
--
--           end case;
--
--           return Index + 1;
--        end Parse_Escape_Sequence;
--

--        for U in UTF8'Range loop
--           if UTF8 (U) = ASCII.ESC and then U > Skip then
--              if U > Skip then
--                 Trace (Me, "MANU >" & UTF8 (Skip .. U - 1));
--              end if;
--
--              Skip := Parse_Escape_Sequence (UTF8 (U .. UTF8'Last));
--           end if;
--        end loop;
--
--        if UTF8'Last /= Skip then
--           Trace (Me, "MANU >" & UTF8 (Skip .. UTF8'Last));
--        end if;
--

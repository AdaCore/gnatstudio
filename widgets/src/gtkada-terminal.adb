------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Types;
with Glib.Properties;      use Glib.Properties;
with Glib.Unicode;         use Glib.Unicode;
with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Text_Mark;        use Gtk.Text_Mark;
with Gtk.Text_Tag;         use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;   use Gtk.Text_Tag_Table;
with Gtk.Widget;           use Gtk.Widget;
with Pango.Enums;          use Pango.Enums;
with Interfaces.C;         use Interfaces.C;
with System;               use System;
with GNATCOLL.Traces;      use GNATCOLL.Traces;

package body Gtkada.Terminal is
   Me : constant Trace_Handle := Create ("GPS.WIDGETS.TERMINAL", Off);

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
      Beginning_Of_Line,
      Insert_Lines,              --  "AL"
      Delete_Chars,              --  "DC"
      End_Alternative_Charset,   --  "ae"
      Start_Alternative_Charset, --  "as"
      Delete_Lines,              --  "DL"
      Cursor_Down,               --  "DO"
      Cursor_Down_Multiple,
      Insert_Chars,              --  "IC"
      Keypad_Center_Key,         --  "K2"
      Cursor_Left,
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
      Newline,                   --  "sf"
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
      End_All_Modes,             --  "me"
      Meta_Mode_On,              --  "mm"
      Meta_Mode_Off,             --  "mo"
      Cursor_Right,              --  "nd"
      Restore_Saved_Position,    --  "rc"
      Save_Position,             --  "sc"
      Reverse_Scroll,            --  "sr"
      Set_Tabulator_Stop,        --  "st"
      End_Program_Using_Cursor,  --  "te"
      Begin_Program_Using_Cursor, -- "ti"
      Cursor_Up,                  -- "up"
      Visible_Bell,               -- "vb"
      Normal_Cursor_Visible,      -- "ve"
      Cursor_Invisible,           -- "vi"
      Standout_Cursor,            -- "vs"
      Memory_Unlock,
      Memory_Lock,
      Enable_Line_Wrap,
      Disable_Line_Wrap,
      Set_Char_Attribute,
      Reset_Char_Attribute,
      Display_In_Status_Line,
      Cursor_Horizontal_Absolute  -- CHA
     );

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
      C_Class : Ada_GObject_Class := Uninitialized_Class;
      Default_Insert_Callback : Insert_Callback := null;
   end record;
   Class : GtkAda_Terminal_Class;

   Alternate_Charset : array (Character) of Gunichar :=
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
      Iter   : access Gtk.Text_Iter.Gtk_Text_Iter;
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

   procedure Free (FSM : in out FSM_Transition_Access);
   --  Free memory used by FSM (recursively)

   function Parse_Xterm_Termcap return FSM_Transition_Access;
   --  Create a state machine to analyze an xterm's escape sequences

   procedure On_Destroy (Data : System.Address; Self : System.Address);
   pragma Convention (C, On_Destroy);
   --  The terminal is being destroyed, reclaim memory

   procedure On_Move_Cursor_Left
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count : Positive);
   procedure On_Move_Cursor_Right
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count : Natural);
   procedure On_Move_Cursor_Up
     (Term            : access Gtkada_Terminal_Record'Class;
      Iter            : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count           : Natural;
      Preserve_Column : Boolean);
   procedure On_Move_Cursor_Down
     (Term            : access Gtkada_Terminal_Record'Class;
      Iter            : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count           : Natural;
      Preserve_Column : Boolean);
   procedure On_Set_Attribute
     (Term  : access Gtkada_Terminal_Record'Class;
      Ansi  : Integer);
   procedure On_Clear_Screen_And_Home
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter);
   procedure On_Clear_To_End_Of_Screen
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter);
   procedure On_Clear_To_End_Of_Line
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter);
   procedure On_Newline
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter);
   procedure On_Move_Cursor
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter   : in out Gtk_Text_Iter;
      Line   : Integer;
      Column : Integer);
   --  Callbacks for various capabilities

   procedure End_All_Modes (Term : access Gtkada_Terminal_Record'Class);
   --  Terminates all special highlighting modes and colors

   procedure Set_Col_In_Line
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter   : in out Gtk_Text_Iter;
      Col    : Gint);
   --  Set the cursor onto a specific column in its current line

   procedure Default_Insert
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      S     : String;
      Overwrite_Mode : Boolean := True);
   procedure Default_Insert
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      S      : System.Address;
      Length : Gint;
      Overwrite_Mode : Boolean := True);
   --  Insert a string using the default insert method of GtkTextBuffer,
   --  bypassing our own handler for terminals

   ----------
   -- Free --
   ----------

   procedure Free (FSM : in out FSM_Transition_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (FSM_Transition, FSM_Transition_Access);
   begin
      if FSM /= null then
         for S in FSM'Range loop
            if FSM (S).Any_Number /= null then
               Free (FSM (S).Any_Number);
            end if;

            if FSM (S).Transitions /= null then
               Free (FSM (S).Transitions);
            end if;
         end loop;

         Unchecked_Free (FSM);
      end if;
   end Free;

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
      --  The following are ANSI sequences which seem to work fine
      Add_Sequence (FSM, ASCII.ESC & "[u",       Restore_Saved_Position);
      Add_Sequence (FSM, ASCII.ESC & "[s",       Save_Position);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dH",  Move_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[H",       Cursor_Home);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%df",  Move_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[%dm",     Set_Char_Attribute);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dm",  Set_Char_Attribute);
      Add_Sequence (FSM, ASCII.ESC & "[m",       Reset_Char_Attribute);
      Add_Sequence (FSM, ASCII.ESC & "[%dD",     Cursor_Left_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[D",       Cursor_Left);
      Add_Sequence (FSM, ASCII.ESC & "[%dC",     Cursor_Right_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[C",       Cursor_Right);
      Add_Sequence (FSM, ASCII.ESC & "[%dA",     Cursor_Up_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[A",       Cursor_Up);
      Add_Sequence (FSM, ASCII.ESC & "[%dB",     Cursor_Down_Multiple);
      Add_Sequence (FSM, ASCII.ESC & "[B",       Cursor_Down);
      Add_Sequence (FSM, ASCII.ESC & "[2J",      Clear_Screen_And_Home);
      Add_Sequence (FSM, ASCII.ESC & "]0;%s" & ASCII.BEL,
                    Display_In_Status_Line);
      Add_Sequence (FSM, ASCII.ESC & "]1;%s" & ASCII.BEL,
                    Do_Nothing);  --  Output by bash
      Add_Sequence (FSM, ASCII.ESC & "]2;%s" & ASCII.BEL,
                    Do_Nothing);  --  Output by bash
      Add_Sequence (FSM, ASCII.LF & "",               Newline);
      Add_Sequence (FSM, ASCII.CR & "",               Beginning_Of_Line);
      Add_Sequence (FSM, ASCII.BS & "",               Cursor_Left);
      Add_Sequence (FSM, ASCII.BEL & "",              Do_Nothing);
      Add_Sequence (FSM, ASCII.ESC & "[J",            Clear_To_End_Of_Screen);
      Add_Sequence (FSM, ASCII.ESC & "[K",            Clear_To_End_Of_Line);
      Add_Sequence (FSM, ASCII.ESC & "[7h",           Enable_Line_Wrap);
      Add_Sequence (FSM, ASCII.ESC & "[7l",           Disable_Line_Wrap);
      Add_Sequence (FSM, ASCII.ESC & "[%dr",          Scroll_Region);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dr",       Scroll_Region);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%d;%dr",    Scroll_Region);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%d;%d;%dr", Scroll_Region);

      Add_Sequence (FSM, ASCII.ESC & "[?1h"
                    & ASCII.ESC & "=",           Turn_Keypad_On);

      --  These are sequences from termcap entries, but some of them seem
      --  incorrect

      Add_Sequence (FSM, ASCII.ESC & "(0",   Start_Alternative_Charset);
      Add_Sequence (FSM, ASCII.ESC & "(B",   End_Alternative_Charset);

      --  Sequences used by vi

      Add_Sequence (FSM, ASCII.ESC & "[>c", Do_Nothing); --  ??? What is this
      Add_Sequence (FSM, ASCII.ESC & "[?12l", Do_Nothing); --  ??? What is this
      Add_Sequence (FSM, ASCII.ESC & "[?1h" & ASCII.ESC & "=", Turn_Keypad_On);
      Add_Sequence (FSM, ASCII.ESC & "[?1049l",  End_Program_Using_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[?1049h",  Begin_Program_Using_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[?25h",    Normal_Cursor_Visible);
      Add_Sequence (FSM, ASCII.ESC & "[?25l",    Cursor_Invisible);
      Add_Sequence (FSM, ASCII.ESC & "[?12;25h", Standout_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[?1034h",  Meta_Mode_On);
      Add_Sequence (FSM, ASCII.ESC & "[?1034l",  Meta_Mode_Off);

      Add_Sequence (FSM, ASCII.ESC & "[%dL",     Insert_Lines);
      Add_Sequence (FSM, ASCII.ESC & "[%dP",     Delete_Chars);
      Add_Sequence (FSM, ASCII.ESC & "[%d;%dP",  Delete_Chars);
      Add_Sequence (FSM, ASCII.ESC & "[%dM",     Delete_Lines);
      Add_Sequence (FSM, ASCII.ESC & "[%d@",     Insert_Chars);
      Add_Sequence (FSM, ASCII.ESC & "OE",       Keypad_Center_Key);
      Add_Sequence (FSM, ASCII.ESC & "[%dS",     Normal_Scroll);
      Add_Sequence (FSM, ASCII.ESC & "[%dT",     Scroll_Back);
      Add_Sequence (FSM, ASCII.ESC & "[L",       Insert_One_Line);
      Add_Sequence (FSM, ASCII.BEL & "",         Audio_Bell);
      Add_Sequence (FSM, ASCII.ESC & "[Z",       Move_To_Prev_Tab);
      Add_Sequence (FSM, ASCII.ESC & "[3g",      Clear_Tabs);
      Add_Sequence (FSM, ASCII.ESC & "[P",       Delete_One_Char);
      Add_Sequence (FSM, ASCII.ESC & "[M",       Delete_One_Line);
      Add_Sequence (FSM, ASCII.ESC & "[%dX",     Erase_Chars_At_Cursor);
      Add_Sequence (FSM, ASCII.ESC & "[4l",      End_Insert_Mode);
      Add_Sequence (FSM, ASCII.ESC & "[4h",      Begin_Insert_Mode);
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
      Add_Sequence (FSM, ASCII.ESC & "OA",       Key_Cursor_Up);
      Add_Sequence (FSM, ASCII.ESC & "[C",       Cursor_Right);
      Add_Sequence (FSM, ASCII.ESC & "M",        Reverse_Scroll);
      Add_Sequence (FSM, ASCII.ESC & "H",        Set_Tabulator_Stop);
      Add_Sequence (FSM, ASCII.ESC & "[A",       Cursor_Up);
      Add_Sequence (FSM, ASCII.ESC & "[?5h"
                    & ASCII.ESC & "[?51",        Visible_Bell);

      Add_Sequence (FSM, ASCII.ESC & "[l",       Memory_Lock);
      Add_Sequence (FSM, ASCII.ESC & "[G",       Cursor_Horizontal_Absolute);
      Add_Sequence (FSM, ASCII.ESC & "[%dG",     Cursor_Horizontal_Absolute);

      --  No meaning in GUI mode (?)
      --  do=^J            Cursor down one line
      --  sf=^J                        Normal scroll one line
      --  ta=^I                        Move to next hardware tab

      return FSM;
   end Parse_Xterm_Termcap;

   ------------------
   -- On_Set_Title --
   ------------------

   procedure On_Set_Title
     (Term  : access Gtkada_Terminal_Record;
      Title : String)
   is
      pragma Unreferenced (Term);
   begin
      Trace (Me, "Set_Title: " & Title);
   end On_Set_Title;

   ------------------
   -- Place_Cursor --
   ------------------

   overriding procedure Place_Cursor
     (Self   : access Gtkada_Terminal_Record;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
   begin
      if Self.Cursor_Mark /= null then
         Move_Mark (Self, Self.Cursor_Mark, Where);
      end if;

      Gtk.Text_Buffer.Place_Cursor
        (Gtk_Text_Buffer_Record (Self.all)'Access, Where);
   end Place_Cursor;

   -------------------------
   -- On_Move_Cursor_Left --
   -------------------------

   procedure On_Move_Cursor_Left
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count : Positive)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Backward_Chars (Iter, Count => Gint (Count), Result => Success);
      Place_Cursor (Term, Iter);
   end On_Move_Cursor_Left;

   ---------------------
   -- Set_Col_In_Line --
   ---------------------

   procedure Set_Col_In_Line
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter   : in out Gtk_Text_Iter;
      Col    : Gint)
   is
      C : constant Gint := Col + Term.Region.Min_Col;
   begin
      if Get_Line_Offset (Iter) > C then
         On_Move_Cursor_Left
           (Term, Iter, Integer (Get_Line_Offset (Iter) - C));
      elsif Get_Line_Offset (Iter) < C then
         On_Move_Cursor_Right
           (Term, Iter, Integer (C - Get_Line_Offset (Iter)));
      else
         Place_Cursor (Term, Iter);
      end if;
   end Set_Col_In_Line;

   --------------------------
   -- On_Move_Cursor_Right --
   --------------------------

   procedure On_Move_Cursor_Right
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count : Natural)
   is
      Line_Chars : Gint := Get_Chars_In_Line (Iter);
      --  Number of chars in current line, including trailing '\n' if any

      Offset     : constant Gint := Get_Line_Offset (Iter);
      --  Offset in current line, starting at 0

      Iter2 : Gtk_Text_Iter;
      Success : Boolean;

   begin
      --  One small difficulty here: if the line does not contain enough
      --  characters, gtk+ will not move the cursor. But a terminal should
      --  behave as if it always had a character at any of its position, so we
      --  need to create them on the fly

      if Count /= 0 then
         Copy (Source => Iter, Dest => Iter2);
         if not Ends_Line (Iter2) then
            Forward_To_Line_End (Iter2, Success);
         end if;

         if Get_Offset (Iter) + Gint (Count) > Get_Offset (Iter2) then
            if Line_Chars /= Offset then
               Line_Chars := Line_Chars - 1;
            end if;

            declare
               S : aliased constant
                 String (1 .. Integer
                   (Get_Offset (Iter) + Gint (Count)
                      - Get_Offset (Iter2)))
                 := (others => ' ');
            begin
               --  We must test whether we are already on line ends, since
               --  otherwise the call to Forward_To_Line_End would jump to next
               --  line.
               if not Ends_Line (Iter) then
                  Forward_To_Line_End (Iter, Success);
               end if;

               Class.Default_Insert_Callback
                 (Get_Object (Term),
                  Iter'Unrestricted_Access, S'Address, S'Length);
            end;

         else
            Forward_Chars (Iter, Count => Gint (Count), Result => Success);
         end if;
      end if;

      Place_Cursor (Term, Iter);
   end On_Move_Cursor_Right;

   -------------------------
   -- On_Move_Cursor_Down --
   -------------------------

   procedure On_Move_Cursor_Down
     (Term            : access Gtkada_Terminal_Record'Class;
      Iter            : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count           : Natural;
      Preserve_Column : Boolean)
   is
      Start     : constant Gint := Get_Line_Offset (Iter);
      Success   : Boolean;
      Missing   : Integer;
      Eob       : Gtk_Text_Iter;
   begin
      Get_End_Iter   (Term, Eob);

      if Count = 0 then
         --  Already on the right line
         null;

      elsif Get_Line (Eob) + 1 < Get_Line (Iter) + Gint (Count) + 1 then
         --  Lines missing in the buffer, add them
         Missing := Integer (Get_Line (Iter)) + 1
           + Count - Integer (Get_Line (Eob) + 1);
         Default_Insert (Term, Eob, (1 .. Missing => ASCII.LF));
         Copy (Source => Eob, Dest => Iter);

      else
         Forward_Lines (Iter, Gint (Count), Success);
         if not Success then
            Trace (Me,
                   "Error: could not move" & Integer'Image (Count) & " down");
         end if;
      end if;

      if Preserve_Column then
         Set_Col_In_Line (Term, Iter, Start);
      else
         Place_Cursor (Term, Iter);
      end if;
   end On_Move_Cursor_Down;

   -----------------------
   -- On_Move_Cursor_Up --
   -----------------------

   procedure On_Move_Cursor_Up
     (Term            : access Gtkada_Terminal_Record'Class;
      Iter            : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count           : Natural;
      Preserve_Column : Boolean)
   is
      Start     : constant Gint := Get_Line_Offset (Iter);
      Success   : Boolean;
   begin
      Backward_Lines (Iter, Gint (Count), Success);

      if Preserve_Column then
         Set_Col_In_Line (Term, Iter, Start);
      else
         Place_Cursor (Term, Iter);
      end if;
   end On_Move_Cursor_Up;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : Integer;
      Column : Integer)
   is
      Success : Boolean;
      Missing : Integer;
      Eob     : Gtk_Text_Iter;
      L       : constant Gint := Gint (Line) + Term.Region.Min_Line;
   begin
      --  ??? Tricky here: the first line should be the first visible line, but
      --  we do not have access to the view, so we don't know exactly which it
      --  is. For now we'll assume the user did a Clear_Screen first and the
      --  buffer only contains the visible part anyway

      --  If a scroll region is in place and we are moving to a line outside
      --  of the region, we must scroll.

      if Gint (Line) > Term.Region.Max_Line then
         declare
            Bor : Gtk_Text_Iter;  --  beginning of region
            Bos : Gtk_Text_Iter;  --  beginning of scroll
            Result : Boolean;
         begin
            --  If the region does not extend to the end of the buffer, add
            --  blank lines as needed
            Term.Get_Start_Iter (Bor);
            Forward_Lines (Bor, Term.Region.Max_Line + 1, Result);
            Set_Line_Offset (Bor, 0);
            Default_Insert
               (Term, Bor,
                (1 .. Line - Integer (Term.Region.Max_Line) => ASCII.LF),
                Overwrite_Mode => False);

            --  Remove the no longer needed lines at the beginning of the
            --  region (preserve the ones in the buffer before the beginning
            --  of the region)
            Term.Get_Start_Iter (Bor);
            Forward_Lines (Bor, Term.Region.Min_Line, Result);
            Copy (Source => Bor, Dest => Bos);
            Forward_Lines (Bos, Gint (Line) - Term.Region.Max_Line, Result);
            Term.Delete (Bor, Bos);
         end;
      end if;

      --  Move down, creating lines as needed
      Get_Start_Iter (Term, Iter);
      Get_End_Iter   (Term, Eob);

      if Get_Line (Iter) + 1 = L then
         --  Already on the right line
         null;

      elsif Get_Line (Eob) + 1 < L then
         --  Lines missing in the buffer, add them
         Missing := Integer (L - Get_Line (Eob) + 1);
         Default_Insert (Term, Eob, (1 .. Missing => ASCII.LF));
         Copy (Source => Eob, Dest => Iter);

      else
         Forward_Lines (Iter, L - 1, Success);
         if not Success then
            Trace (Me,
                   "Error: could not move"
                   & Gint'Image (L - 1) & " down");
         end if;
      end if;

      Set_Col_In_Line (Term, Iter, Gint (Column - 1));
   end On_Move_Cursor;

   ----------------
   -- On_Newline --
   ----------------

   procedure On_Newline
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter)
   is
   begin
      On_Move_Cursor_Down (Term, Iter, 1, Preserve_Column => False);
      Set_Line_Index (Iter, 0);
      Place_Cursor (Term, Iter);
   end On_Newline;

   ------------------------------
   -- On_Clear_Screen_And_Home --
   ------------------------------

   procedure On_Clear_Screen_And_Home
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter)
   is
      Frm : Gtk_Text_Iter;
   begin
      --  ??? Should we preserve the rest of the window before the visible part
      --  Ideally yes, but hard to do since we have to know what is the
      --  visible area and we don't have access to the view

      Get_Start_Iter (Term, Frm);
      Get_End_Iter   (Term, Iter);
      Delete (Term, Frm, Iter);
   end On_Clear_Screen_And_Home;

   -------------------------------
   -- On_Clear_To_End_Of_Screen --
   -------------------------------

   procedure On_Clear_To_End_Of_Screen
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter)
   is
      To : Gtk_Text_Iter;
   begin
      Get_End_Iter (Term, To);
      Delete (Term, Iter, To);
   end On_Clear_To_End_Of_Screen;

   -----------------------------
   -- On_Clear_To_End_Of_Line --
   -----------------------------

   procedure On_Clear_To_End_Of_Line
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk_Text_Iter)
   is
      To      : Gtk_Text_Iter;
      Success : Boolean;
   begin
      Copy (Source => Iter, Dest => To);
      Forward_To_Line_End (To, Success);
      Delete (Term, Iter, To);
   end On_Clear_To_End_Of_Line;

   -------------------
   -- End_All_Modes --
   -------------------

   procedure End_All_Modes (Term : access Gtkada_Terminal_Record'Class) is
   begin
      Term.Bold := False;
      Term.Standout := False;
      Term.Current_Foreground := -1;
      Term.Current_Background := -1;
   end End_All_Modes;

   ----------------------
   -- On_Set_Attribute --
   ----------------------

   procedure On_Set_Attribute
     (Term  : access Gtkada_Terminal_Record'Class;
      Ansi  : Integer)
   is
   begin
      case Ansi is
         when 0 =>
            End_All_Modes (Term);

         --  See https://en.wikipedia.org/wiki/ANSI_escape_code#graphics
         when 1  =>  --  bold mode
            End_All_Modes (Term);
            Term.Bold := True;
         when 4  => null;  --  underline: single
         when 5  => null;  --  blink: slow
         when 7  =>  --  start standout mode
            End_All_Modes (Term);
            Term.Standout := True;
         when 24 => null;  --  underline: none
         when 27 =>   --  end standout mode
            End_All_Modes (Term);
            Term.Standout := False;
         when 30 | 90 => Term.Current_Foreground := Tag_Array'First;
         when 31 | 91 => Term.Current_Foreground := Tag_Array'First + 1;
         when 32 | 92 => Term.Current_Foreground := Tag_Array'First + 2;
         when 33 | 93 => Term.Current_Foreground := Tag_Array'First + 3;
         when 34 | 94 => Term.Current_Foreground := Tag_Array'First + 4;
         when 35 | 95 => Term.Current_Foreground := Tag_Array'First + 5;
         when 36 | 96 => Term.Current_Foreground := Tag_Array'First + 6;
         when 37 | 97 => Term.Current_Foreground := Tag_Array'First + 7;
         when 38 | 98 => Term.Current_Foreground := Tag_Array'First;
         when 39 | 99 => Term.Current_Foreground := -1;

         when 40 | 100 => Term.Current_Background := Tag_Array'First;
         when 41 | 101 => Term.Current_Background := Tag_Array'First + 1;
         when 42 | 102 => Term.Current_Background := Tag_Array'First + 2;
         when 43 | 103 => Term.Current_Background := Tag_Array'First + 3;
         when 44 | 104 => Term.Current_Background := Tag_Array'First + 4;
         when 45 | 105 => Term.Current_Background := Tag_Array'First + 5;
         when 46 | 106 => Term.Current_Background := Tag_Array'First + 6;
         when 47 | 107 => Term.Current_Background := Tag_Array'First + 7;
         when 48 | 108 => Term.Current_Background := Tag_Array'First;
         when 49 | 109 => Term.Current_Background := -1;

         when others =>
            Trace (Me, "Set_Attribute:" & Ansi'Img);
      end case;
   end On_Set_Attribute;

   --------------------
   -- Default_Insert --
   --------------------

   procedure Default_Insert
     (Term   : access Gtkada_Terminal_Record'Class;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      S      : System.Address;
      Length : Gint;
      Overwrite_Mode : Boolean := True)
   is
      Start_Offset : constant Gint := Get_Offset (Iter);
      Off     : Gint;
      Iter2   : Gtk_Text_Iter;
      Eol     : Gtk_Text_Iter;
      Success : Boolean;
   begin
      --  Simulate overwrite mode. Rather than computing how many UTF8 chars we
      --  have inserted we rely on Offset for that. However, we must make sure
      --  not to erase beyond the end of line

      Class.Default_Insert_Callback
        (Get_Object (Term), Iter'Unrestricted_Access, S, Length);

      if Term.Bold then
         Get_Iter_At_Offset (Term, Iter2, Start_Offset);
         Apply_Tag (Term, Term.Bold_Tag, Iter2, Iter);
      end if;

      if Term.Standout then
         Get_Iter_At_Offset (Term, Iter2, Start_Offset);
         Apply_Tag (Term, Term.Standout_Tag, Iter2, Iter);
      end if;

      if Term.Current_Foreground in Tag_Array'Range then
         Get_Iter_At_Offset (Term, Iter2, Start_Offset);
         Apply_Tag
           (Term, Term.Foreground_Tags (Term.Current_Foreground), Iter2, Iter);
      end if;

      if Term.Current_Background in Tag_Array'Range then
         Get_Iter_At_Offset (Term, Iter2, Start_Offset);
         Apply_Tag
           (Term, Term.Background_Tags (Term.Current_Background), Iter2, Iter);
      end if;

      if Overwrite_Mode then
         Off := Get_Offset (Iter) - Start_Offset;
         Copy (Source => Iter, Dest => Iter2);
         Forward_Chars (Iter2, Count => Off, Result => Success);

         Copy (Source => Iter, Dest => Eol);
         if not Ends_Line (Eol) then
            Forward_To_Line_End (Eol, Success);
         end if;

         if Get_Offset (Eol) <= Get_Offset (Iter2) then
            Delete (Term, Iter, Eol);
         else
            Delete (Term, Iter, Iter2);
         end if;
      end if;

      Place_Cursor (Term, Iter);
   end Default_Insert;

   --------------------
   -- Default_Insert --
   --------------------

   procedure Default_Insert
     (Term  : access Gtkada_Terminal_Record'Class;
      Iter  : in out Gtk.Text_Iter.Gtk_Text_Iter;
      S     : String;
      Overwrite_Mode : Boolean := True) is
   begin
      Default_Insert (Term, Iter, S'Address, S'Length, Overwrite_Mode);
   end Default_Insert;

   --------------------
   -- On_Insert_Text --
   --------------------

   procedure On_Insert_Text
     (Widget : System.Address;
      Iter   : access Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : System.Address;
      Length : Gint)
   is
      Stub : Gtkada_Terminal_Record;
      pragma Warnings (Off, Stub);
      Term : constant Gtkada_Terminal :=
        Gtkada_Terminal (Get_User_Data (Widget, Stub));
      Txt  : constant c_char_array_access := UC (Text);

      Str_Arg_First, Str_Arg_Last : size_t;
      --  Integer arguments

      C                 : size_t := 0;
      Stopper           : char;

      procedure Insert_Substr (Frm, To : size_t);
      --  Write a specific substring to the buffer

      function To_String (Frm, To : size_t) return String;
      --  Return the specified substring of Text

      procedure Perform (Func : Capability);
      --  Perform an action requested by the user, and reset FSM

      procedure Send_Current_Sequence;
      --  Send the current sequence of characters to the screen, up to but not
      --  including the character pointing by C. Reset the state machine

      procedure Debug_Trace (Str : String);
      --  Print Str on the debug log, quoting special characters

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

      -----------------
      -- Debug_Trace --
      -----------------

      procedure Debug_Trace (Str : String) is
         Last : Integer := Str'First;
      begin
         for S in Str'Range loop
            if Str (S) < ' ' then
               if Last < S then
                  Trace (Me, Str (Last .. S - 1));
               end if;
               Trace (Me, "ASCII." & Character'Image (Str (S)));
               Last := S + 1;
            end if;
         end loop;

         if Last <= Str'Last then
            Trace (Me, Str (Last .. Str'Last));
         end if;
      end Debug_Trace;

      -------------------
      -- Insert_Substr --
      -------------------

      procedure Insert_Substr (Frm, To : size_t) is
      begin
         --  Insertion must be done in overwrite mode for a terminal, so we
         --  need to delete chars before inserting some (or the opposite)

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
                     if Active (Me) then
                        Debug_Trace (S (S'First .. Index - 1));
                     end if;
                     Default_Insert (Term, Iter.all, S (S'First .. Index - 1));
                     Index := S'First;
                  end if;
               end loop;

               if Index > S'First then
                  if Active (Me) then
                     Debug_Trace (S (S'First .. Index - 1));
                  end if;
                  Default_Insert (Term, Iter.all, S (S'First .. Index - 1));
               end if;
            end;

         else
            if Active (Me) then
               Debug_Trace (To_String (Frm, To));
            end if;
            Default_Insert
              (Term, Iter.all, Txt (Frm .. To)'Address, Gint (To - Frm + 1));
         end if;
      end Insert_Substr;

      -------------
      -- Perform --
      -------------

      procedure Perform (Func : Capability) is
      begin
         if Term.State.Current = Term.FSM then
            Send_Current_Sequence;
         end if;

         if Active (Me) then
            if Term.State.Current_Arg = 1 then
               Trace (Me, Func'Img);
            elsif Term.State.Current_Arg = 2 then
               Trace (Me, Func'Img
                      & Term.State.Arg (Numerical_Arguments'First)'Img);
            elsif Term.State.Current_Arg = 3 then
               Trace (Me, Func'Img
                      & Term.State.Arg (Term.State.Arg'First)'Img
                      & Term.State.Arg (Term.State.Arg'First + 1)'Img);
            elsif Term.State.Current_Arg = 4 then
               Trace (Me, Func'Img
                      & Term.State.Arg (Term.State.Arg'First)'Img
                      & Term.State.Arg (Term.State.Arg'First + 1)'Img
                      & Term.State.Arg (Term.State.Arg'First + 2)'Img);
            elsif Term.State.Current_Arg = 5 then
               Trace (Me, Func'Img
                      & Term.State.Arg (Term.State.Arg'First)'Img
                      & Term.State.Arg (Term.State.Arg'First + 1)'Img
                      & Term.State.Arg (Term.State.Arg'First + 2)'Img
                      & Term.State.Arg (Term.State.Arg'First + 3)'Img);
            else
               Trace (Me, Func'Img & " ...");
            end if;
         end if;

         --  The previous sequence of characters has already been emitted, no
         --  need to redo that (for instance when changing the charset to make
         --  sure the previous chars are displayed with the right set)

         case Func is
            when Do_Nothing =>
               null;
            when Display_In_Status_Line =>
               On_Set_Title (Term, To_String (Str_Arg_First, Str_Arg_Last));

            when Start_Alternative_Charset =>
               Term.Alternate_Charset := True;
            when End_Alternative_Charset =>
               Term.Alternate_Charset := False;
               End_All_Modes (Term);  --  ??? Is this correct
            when End_All_Modes =>
               End_All_Modes (Term);

            when Beginning_Of_Line =>
               Set_Line_Offset (Iter.all, 0);
               Place_Cursor (Term, Iter.all);

            when Cursor_Left_Multiple =>
               On_Move_Cursor_Left (Term, Iter.all, Term.State.Arg (1));
            when Cursor_Left =>
               On_Move_Cursor_Left (Term, Iter.all, 1);

            when Cursor_Right_Multiple =>
               On_Move_Cursor_Right (Term, Iter.all, Term.State.Arg (1));
            when Cursor_Right =>
               On_Move_Cursor_Right (Term, Iter.all, 1);

            when Cursor_Up_Multiple =>
               On_Move_Cursor_Up
                 (Term, Iter.all, Term.State.Arg (1), Preserve_Column => True);
            when Cursor_Up =>
               On_Move_Cursor_Up
                 (Term, Iter.all, 1, Preserve_Column => True);

            when Cursor_Down_Multiple =>
               On_Move_Cursor_Down
                 (Term, Iter.all, Term.State.Arg (1), Preserve_Column => True);
            when Cursor_Down =>
               On_Move_Cursor_Down
                 (Term, Iter.all, 1, Preserve_Column => True);

            when Clear_To_End_Of_Screen =>
               On_Clear_To_End_Of_Screen (Term, Iter.all);

            when Clear_To_End_Of_Line =>
               On_Clear_To_End_Of_Line (Term, Iter.all);

            when Delete_Chars =>
               declare
                  To      : Gtk_Text_Iter;
                  Success : Boolean;
               begin
                  Copy (Source => Iter.all, Dest => To);
                  Forward_Chars (To, Gint (Term.State.Arg (1)), Success);
                  Delete (Term, Iter.all, To);
               end;

            when Newline =>
               On_Newline (Term, Iter.all);
            when Memory_Unlock | Memory_Lock =>
               null;

            when Clear_Screen_And_Home =>
               On_Clear_Screen_And_Home (Term, Iter.all);

            when Move_Cursor =>
               On_Move_Cursor
                 (Term, Iter.all, Term.State.Arg (1), Term.State.Arg (2));
            when Cursor_Home =>
               On_Move_Cursor (Term, Iter.all, 1, 1);

            when Cursor_Horizontal_Absolute =>
               if Term.State.Current_Arg = 1 then
                  Set_Line_Offset (Iter.all, 0);
               else
                  Set_Line_Offset (Iter.all, Gint (Term.State.Arg (1) - 1));
               end if;
               Place_Cursor (Term, Iter.all);

            when Set_Char_Attribute =>
               for A in 1 .. Term.State.Current_Arg - 1 loop
                  On_Set_Attribute (Term, Term.State.Arg (A));
               end loop;

            when Reset_Char_Attribute =>
               On_Set_Attribute (Term, 0);

            when Cursor_Invisible | Normal_Cursor_Visible =>
               null;

            when Turn_Keypad_On | Turn_Keypad_Off =>
               null;

            when Audio_Bell =>
               null;

            when Insert_One_Line =>
               declare
                  It, It2 : Gtk_Text_Iter;
                  Success : Boolean;
               begin
                  Copy (Source => Iter.all, Dest => It);
                  Default_Insert
                     (Term, It, ASCII.LF & "", Overwrite_Mode => False);

                  --  Remove the last line in the current scrolling region
                  if Term.Region.Max_Line /= Gint'Last then
                     Term.Get_Start_Iter (It);
                     Forward_Lines (It, Term.Region.Max_Line, Success);
                     Copy (Source => It, Dest => It2);
                     Forward_To_Line_End (It2, Success);
                     Term.Delete (It, It2);
                  end if;
               end;

            --  http://www.sweger.com/ansiplus/EscSeqScroll.html
            when Scroll_Region =>
               if Term.State.Current_Arg = 1 then
                  Term.Region.Min_Line := 0;
               else
                  Term.Region.Min_Line :=
                     Gint (Term.State.Arg (Term.State.Arg'First)) - 1;
               end if;

               if Term.State.Current_Arg <= 2 then
                  Term.Region.Max_Line := Term.Get_Line_Count - 1;
               else
                  Term.Region.Max_Line :=
                     Gint (Term.State.Arg (Term.State.Arg'First + 1)) - 1;
               end if;

               if Term.State.Current_Arg <= 3 then
                  Term.Region.Min_Col := 0;
               else
                  Term.Region.Min_Col :=
                     Gint (Term.State.Arg (Term.State.Arg'First + 2)) - 1;
               end if;

               if Term.State.Current_Arg <= 4 then
                  Term.Region.Max_Col := Gint'Last;  --   ??? Unsupported
               else
                  Term.Region.Max_Col :=
                     Gint (Term.State.Arg (Term.State.Arg'First + 3)) - 1;
               end if;

            when others =>
               Trace (Me, "Unhandled capability: " & Func'Img);
         end case;

         Term.State.Current := Term.FSM;
         Term.State.Start_Of_Sequence := C + 1;
      end Perform;

      ---------------------------
      -- Send_Current_Sequence --
      ---------------------------

      procedure Send_Current_Sequence is
      begin
         if C > 0 and then Term.State.Start_Of_Sequence <= C - 1 then
            Insert_Substr (Term.State.Start_Of_Sequence, C - 1);
         end if;

         Term.State.Start_Of_Sequence := C;
         Term.State.Current_Arg       := Numerical_Arguments'First;
         Str_Arg_First                := size_t (Length) - 1;
      end Send_Current_Sequence;

      Cursor : Gtk_Text_Iter;

   begin
      if Term.FSM = null then
         --  No special terminal setup ? Send the string as is
         Insert_Substr (Txt'First, Txt'First + size_t (Length) - 1);
         return;
      end if;

      if Term.State.Current = null then
         Term.State.Current := Term.FSM;
      end if;

      --  If we are not inserting at the cursor, all bets are off, and the
      --  terminal might be corrupted. However, that's probably better than
      --  forbidding a modification altogether

      Get_Iter_At_Mark (Term, Cursor, Get_Insert (Term));
      if Get_Offset (Cursor) /= Get_Offset (Iter.all) then
         Default_Insert (Term, Iter.all, Text, Length);
         return;

      else
         --  Make sure we move the cursor to its old position, if appropriate
         if Term.Cursor_Mark /= null  then
            Get_Iter_At_Mark (Term, Cursor, Term.Cursor_Mark);
            if Get_Offset (Cursor) /= Get_Offset (Iter.all) then
               Place_Cursor (Term, Cursor);
               Copy (Source => Cursor, Dest => Iter.all);
            end if;
         end if;
      end if;

      while C < size_t (Length) loop
         if Txt (C) in Escape_Chars then
            if Term.State.Parsing_Number then
               if Txt (C) in '0' .. '9' then
                  Term.State.Arg (Term.State.Current_Arg) :=
                    Term.State.Arg (Term.State.Current_Arg) * 10
                    + char'Pos (Txt (C)) - char'Pos ('0');

                  if Term.State.Tmp /= null then
                     if Term.State.Tmp (Txt (C)) /= Null_State then
                        --  The following test is only needed if we assume that
                        --  the special command could end on a numeric
                        --  character, which I don't believe is possible yet
                        --  (and therefore we test again after the loop, ie
                        --  when we have encountered the first non-numerical
                        --  character
                        if Term.State.Tmp (Txt (C)).Callback /=
                          Self_Insert
                        then
                           Perform (Term.State.Tmp (Txt (C)).Callback);
                           Term.State.Current := Term.FSM;
                           Term.State.Start_Of_Sequence := C + 1;

                        else
                           Term.State.Tmp :=
                             Term.State.Tmp (Txt (C)).Transitions;
                        end if;

                     else
                        --  No need to check anymore, there are no transition
                        Term.State.Tmp := null;
                     end if;
                  end if;

               else
                  --  No more digits to complete the argument
                  Term.State.Parsing_Number := False;

                  --  We look at Tmp only if we have transitioned, otherwise we
                  --  would be ignoring the numbers altogether
                  if Term.State.Tmp /= null
                    and then Term.State.Tmp (Txt (C)) /= Null_State
                  then
                     if Term.State.Tmp (Txt (C)).Callback /= Self_Insert then
                        Perform (Term.State.Tmp (Txt (C)).Callback);

                     elsif Term.State.Tmp (Txt (C)).Transitions /= null then
                        --  Seems like the state machine is still trying to
                        --  match, so this is the new current state and we
                        --  forget about the %d argument. Seems an unlikely
                        --  case though
                        Term.State.Current :=
                          Term.State.Tmp (Txt (C)).Transitions;
                     end if;
                  else
                     Term.State.Current_Arg := Term.State.Current_Arg + 1;
                     C := C - 1;  --  So that we test the character after %d
                  end if;
               end if;

            elsif Term.State.Current (Txt (C)).String_Stopper /=
              char'Val (127)
            then
               --  Read a string parameter. This is a relatively rare case, for
               --  now we assume the string and the terminator come in the same
               --  batch.

               Str_Arg_First := C + 1;
               Stopper := Term.State.Current (Txt (C)).String_Stopper;
               while C < size_t (Length)
                 and then Txt (C) /= Stopper
               loop
                  C := C + 1;
               end loop;

               --  A command with a string parameter
               Str_Arg_Last := C - 1;
               Perform (Term.State.Current (Txt (Str_Arg_First - 1)).Callback);

            elsif Term.State.Current (Txt (C)).Callback /= Self_Insert then
               --  A special command was found
               Perform (Term.State.Current (Txt (C)).Callback);

            elsif Term.State.Current (Txt (C)).Any_Number /= null
              and then C < size_t (Length) - 1
              and then Txt (C + 1) in '0' .. '9'
            then
               Term.State.Parsing_Number := True;

               --  We have a command with one numerical parameter. The problem
               --  is that this could also be part of a shorter command, as in:
               --     ^[[%dm  => set attribute
               --     ^[[2J   => Clear screen and home cursor
               --  So after seeing "^[[" we need to test both the %d and the
               --  standard transitions. So we use Tmp to follow the
               --  potential states from here on.

               Term.State.Tmp     := Term.State.Current (Txt (C)).Transitions;
               Term.State.Current := Term.State.Current (Txt (C)).Any_Number;
               Term.State.Arg (Term.State.Current_Arg) := 0;

            elsif Term.State.Current (Txt (C)).Transitions /= null then
               --  Either starting a new special sequence (then emit current
               --  chars) or in the middle of one

               if Term.State.Current = Term.FSM then
                  Send_Current_Sequence;
               end if;

               Term.State.Current := Term.State.Current (Txt (C)).Transitions;

            else
               --  A self-insert character
               Term.State.Current := Term.FSM;
            end if;

         else
            --  Txt (C) not in Escape_Chars, ie > 127
            --  We have some UTF8 encoding, and we need to send all the char
            --  at once

            if char'Pos (Txt (C)) < 16#800# then
               C := C + 1;  --  utf8 char encoded on 2 bytes
            elsif char'Pos (Txt (C)) < 16#10000# then
               C := C + 2;  --  utf8 char encoded on 3 bytes
            elsif char'Pos (Txt (C)) < 16#200000# then
               C := C + 3;  --  utf8 char encoded on 4 bytes
            elsif char'Pos (Txt (C)) < 16#4000000# then
               C := C + 4;  --  utf8 char encoded on 5 bytes
            else
               C := C + 5;  --  utf8 char encoded on 6 bytes
            end if;
         end if;

         C := C + 1;
      end loop;

      if Term.State.Current = Term.FSM then
         Send_Current_Sequence;
      end if;

      --  On exit, we might be in the middle of parsing an escape sequence,
      --  in which case the start_of_sequence does not matter, or we have just
      --  send a sequence, in which case Start_Of_Sequence is already null. But
      --  we need to reset it since the next string will use a different
      --  sequence anyway
      Term.State.Start_Of_Sequence := 0;
   end On_Insert_Text;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Data : System.Address; Self : System.Address) is
      pragma Unreferenced (Data);
      Stub : Gtkada_Terminal_Record;
      pragma Warnings (Off, Stub);
      Term : constant Gtkada_Terminal :=
        Gtkada_Terminal (Get_User_Data (Self, Stub));

   begin
      Free (Term.FSM);
      if Term.Cursor_Mark /= null then
         Delete_Mark (Term, Term.Cursor_Mark);
      end if;
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self                             : out Gtkada_Terminal;
      Prevent_Cursor_Motion_With_Mouse : Boolean := False)
   is
   begin
      Self := new Gtkada_Terminal_Record;
      Gtkada.Terminal.Initialize (Self, Prevent_Cursor_Motion_With_Mouse);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self                             : access Gtkada_Terminal_Record'Class;
      Prevent_Cursor_Motion_With_Mouse : Boolean := False)
   is
      function Replace_Insert_Text
        (Class : GObject_Class; Func : Insert_Callback)
         return Insert_Callback;
      pragma Import (C, Replace_Insert_Text, "replace_insert_text");

      Iter  : Gtk_Text_Iter;
      Table : Gtk_Text_Tag_Table;
      F, B  : Gtk_Text_Tag;
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Text_Buffer.Get_Type,
         Class_Record => Class.C_Class,
         Type_Name    => "GtkAdaTerminal",
         Parameters   => Null_Parameter_Types);
      G_New (Self, Class.C_Class);
      Table := Self.Get_Tag_Table;

      Self.Prevent_Cursor_Motion := Prevent_Cursor_Motion_With_Mouse;
      if Self.Prevent_Cursor_Motion then
         Get_Start_Iter (Self, Iter);
         Self.Cursor_Mark := Create_Mark (Self, Where => Iter);
      end if;

      if Class.Default_Insert_Callback = null then
         Class.Default_Insert_Callback := Replace_Insert_Text
           (Glib.Types.Class_Peek (Class.C_Class.The_Type),
            On_Insert_Text'Access);

         --  Initialize charset tables
         for C in Alternate_Charset'Range loop
            if Alternate_Charset (C) = 0 then
               Alternate_Charset (C) := Character'Pos (C);
            end if;
         end loop;
      end if;

      --  ??? We could cache the finite state machine for all terminals, but
      --  we need to make this is properly freed on exit, and that also means
      --  that all terminals have the same FSM (not true if we are emulating
      --  various terminals)
      Self.FSM := Parse_Xterm_Termcap;

      Weak_Ref (Self, On_Destroy'Access);

      Gtk_New (Self.Bold_Tag);
      Set_Property
        (Self.Bold_Tag, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);
      Table.Add (Self.Bold_Tag);
      Unref (Self.Bold_Tag);

      Gtk_New (Self.Standout_Tag);
      Set_Property
        (Self.Standout_Tag, Gtk.Text_Tag.Background_Property, "black");
      Set_Property
        (Self.Standout_Tag, Gtk.Text_Tag.Foreground_Property, "white");
      Table.Add (Self.Standout_Tag);
      Unref (Self.Standout_Tag);

      for T in Tag_Array'Range loop
         Gtk_New (F);
         Self.Foreground_Tags (T) := F;

         Gtk_New (B);
         Self.Background_Tags (T) := B;

         case T is
            when 1 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "black");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "black");
            when 2 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "red");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "red");
            when 3 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "green");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "green");
            when 4 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "yellow");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "yellow");
            when 5 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "blue");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "blue");
            when 6 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "magenta");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "magenta");
            when 7 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "cyan");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "cyan");
            when 8 =>
               Set_Property (F, Gtk.Text_Tag.Foreground_Property, "white");
               Set_Property (B, Gtk.Text_Tag.Background_Property, "white");
         end case;

         Table.Add (F);
         Table.Add (B);
         Unref (F);
         Unref (B);
      end loop;

      Self.Region := (0, Gint'Last, 0, Gint'Last);
   end Initialize;

end Gtkada.Terminal;

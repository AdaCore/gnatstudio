------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Regpat;             use GNAT.Regpat;
with String_Utils;            use String_Utils;

package body Codefix.Text_Manager.Commands is

   use type Basic_Types.Visible_Column_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This              : in out Remove_Word_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Word              : Word_Cursor'Class;
      Search_Forward    : Boolean := False;
      All_Occurrences   : Boolean := False;
      Remove_Empty_Line : Boolean := False) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Word);
      This.Search_Forward    := Search_Forward;
      This.All_Occurrences   := All_Occurrences;
      This.Remove_Empty_Line := Remove_Empty_Line;
   end Initialize;

   overriding procedure Free (This : in out Remove_Word_Cmd) is
   begin
      Free (This.Word);
      Free (Text_Command (This));
   end Free;

   overriding procedure Execute
     (This         : Remove_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Word : Word_Cursor;

   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);

      declare
         Match      : constant String := Word.Get_Matching_Word (Current_Text);
         Str_Parsed : constant String :=
                        Do_Tab_Expansion
                          (Current_Text.Get_Line (Word, Start_Col => 1),
                           Tab_Width);
         Column     : Natural := Natural (Get_Column (Word));

      begin
         pragma Assert (Match /= "");

         --  Displace the cursor forward to the first occurrence of the
         --  undesired text

         if This.Search_Forward then
            for J in Column .. Str_Parsed'Last - Match'Length + 1 loop
               if Str_Parsed (J .. J + Match'Length - 1) = Match then
                  Column := J;
                  exit;
               end if;
            end loop;
         end if;

         pragma Assert
           (Column + Match'Length - 1 <= Str_Parsed'Last
              and then
            Str_Parsed (Column .. Column + Match'Length - 1) = Match);

         --  Move the cursor back to preceding (consecutive) occurrences of the
         --  text

         if This.All_Occurrences then
            declare
               Prev_Column : Natural;

            begin
               loop
                  Prev_Column := Column - Match'Length;
                  exit when Prev_Column <= 0
                              or else
                            Str_Parsed
                              (Prev_Column .. Prev_Column + Match'Length - 1)
                                /= Match;

                  Column := Prev_Column;
               end loop;
            end;
         end if;

         Word.Set_Column (Visible_Column_Type (Column));

         loop
            --  Remove one occurrence of the undesired text

            Current_Text.Replace
              (Word,
               Word.Get_Matching_Word
                 (Current_Text, Check => True)'Length, "");

            if Current_Text.Get_Line (Word, 1) = "" then
               if This.Remove_Empty_Line then
                  Current_Text.Delete_Line (Word);
               end if;

               exit;
            end if;

            --  Check if we must remove another occurrence found immediately
            --  after the removed text

            exit when not This.All_Occurrences;

            declare
               Str_Parsed : constant String := Current_Text.Get_Line (Word);

            begin
               exit when Str_Parsed'Length < Match'Length
                 or else
                   Str_Parsed
                     (Str_Parsed'First ..
                            Str_Parsed'First + Match'Length - 1)
                       /= Match;
            end;
         end loop;
      end;

      Free (Word);
   end Execute;

   overriding
   function Is_Writable (This : Remove_Word_Cmd) return Boolean is
   begin
      return This.Word.Mark_Id.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This            : in out Insert_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      After_Pattern   : String := "";
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified;
      Insert_New_Line : Boolean := False)
   is
      New_Word : Word_Cursor;
   begin
      This.Add_Spaces := Add_Spaces;
      This.Position := Position;
      Make_Word_Mark (Word, Current_Text, This.Word);
      This.After_Pattern := To_Unbounded_String (After_Pattern);

      Set_File (New_Word, Get_File (New_Position));
      Set_Location
        (New_Word, Get_Line (New_Position), Get_Column (New_Position));
      Set_Word (New_Word, Null_Unbounded_String, Text_Ascii);
      Make_Word_Mark (New_Word, Current_Text, This.New_Position);
      This.Insert_New_Line := Insert_New_Line;
   end Initialize;

   overriding procedure Free (This : in out Insert_Word_Cmd) is
   begin
      Free (This.Word);
      Free (This.New_Position);
      Free (Text_Command (This));
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      New_Str         : Unbounded_String;
      Line_Cursor     : File_Cursor;
      Space_Cursor    : File_Cursor;
      Word            : Word_Cursor;
      New_Pos         : Word_Cursor;
      Word_Char_Index : String_Index_Type;
      Modified_Text   : Ptr_Text;
   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);
      Make_Word_Cursor (This.New_Position, Current_Text, New_Pos);

      Modified_Text := Current_Text.Get_File (New_Pos.File);

      Line_Cursor := Clone (File_Cursor (New_Pos));
      Line_Cursor.Col := 1;

      New_Str := To_Unbounded_String (Word.Get_Matching_Word (Current_Text));

      if This.After_Pattern /= "" then
         declare
            Matches : Match_Array (0 .. 1);
         begin
            Match
              (To_String (This.After_Pattern),
               Get_Line (Current_Text, New_Pos),
               Matches);

            New_Pos.Col := To_Column_Index
              (String_Index_Type (Matches (1).Last) + 1,
               Get_Line (Current_Text, New_Pos, 1));
         end;
      end if;

      Word_Char_Index :=
        To_Char_Index (New_Pos.Col, Get_Line (Current_Text, Line_Cursor));

      if This.Position = Specified then
         if This.Add_Spaces then
            Space_Cursor := Clone (File_Cursor (New_Pos));

            if Space_Cursor.Col /= 0 then
               Space_Cursor.Col := Space_Cursor.Col - 1;

               if Word_Char_Index > 1
                 and then not Is_Separator (Get (Current_Text, Space_Cursor))
               then
                  New_Str := " " & New_Str;
               end if;

               Space_Cursor.Col := Space_Cursor.Col + 1;

               if Natural (Word_Char_Index) <
                 Line_Length (Current_Text, Line_Cursor)
                 and then not Is_Separator (Get (Current_Text, Space_Cursor))
               then
                  Append (New_Str, " ");
               end if;
            end if;
         end if;

         if This.Insert_New_Line then
            Modified_Text.Add_Line (New_Pos, To_String (New_Str), True);

         else
            Modified_Text.Replace (New_Pos, 0, To_String (New_Str));
         end if;

      elsif This.Position = After then
         Modified_Text.Add_Line (New_Pos, To_String (New_Str), True);

      elsif This.Position = Before then
         New_Pos.Line := New_Pos.Line - 1;
         Modified_Text.Add_Line (New_Pos, To_String (New_Str), True);
      end if;

      Free (Word);
   end Execute;

   overriding
   function Is_Writable (This : Insert_Word_Cmd) return Boolean is
   begin
      return This.Word.Mark_Id.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This            : in out Move_Word_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Word            : Word_Cursor'Class;
      New_Position    : File_Cursor'Class;
      Insert_New_Line : Boolean := False) is
   begin
      Initialize
        (This            => This.Step_Insert,
         Current_Text    => Current_Text,
         Word            => Word,
         New_Position    => New_Position,
         Insert_New_Line => Insert_New_Line);

      Initialize (This.Step_Remove, Current_Text, Word,
                  Remove_Empty_Line => True);
   end Initialize;

   overriding procedure Free (This : in out Move_Word_Cmd) is
   begin
      Free (This.Step_Remove);
      Free (This.Step_Insert);
      Free (Text_Command (This));
   end Free;

   overriding procedure Execute
     (This         : Move_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
   begin
      This.Step_Insert.Execute (Current_Text);
      This.Step_Remove.Execute (Current_Text);
   end Execute;

   overriding
   function Is_Writable (This : Move_Word_Cmd) return Boolean is
   begin
      return This.Step_Remove.Is_Writable
        and then This.Step_Insert.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This           : in out Replace_Word_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Word           : Word_Cursor'Class;
      New_Word       : Unbounded_String;
      Do_Indentation : Boolean := False) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Mark);
      This.Str_Expected := New_Word;
      This.Do_Indentation := Do_Indentation;
   end Initialize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Replace_Word_Cmd) is
   begin
      Free (This.Mark);
      Free (Text_Command (This));
   end Free;

   overriding procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Current_Word : Word_Cursor;
      Text         : Ptr_Text;

   begin
      Make_Word_Cursor (This.Mark, Current_Text, Current_Word);

      declare
         Match : constant String :=
           Current_Word.Get_Matching_Word (Current_Text);
      begin
         Text := Current_Text.Get_File (Current_Word.File);

         declare
            Lower_Expected : constant String :=
              To_Lower (To_String (This.Str_Expected));
         begin
            --   ??? We might be interrested by other cases here...
            if Lower_Expected = "is" then
               Current_Text.Replace
                 (Position      => Current_Word,
                  Len           => Match'Length,
                  New_Text      => To_String (This.Str_Expected),
                  Blanks_Before => One,
                  Blanks_After  => Keep);
            else
               Text.Replace
                 (Cursor    => Current_Word,
                  Len       => Match'Length,
                  New_Value => To_String (This.Str_Expected));
            end if;
         end;

         if This.Do_Indentation then
            Text.Indent_Line (Current_Word);
         end if;

         Free (Current_Word);
      end;
   end Execute;

   overriding
   function Is_Writable (This : Replace_Word_Cmd) return Boolean is
   begin
      return This.Mark.Mark_Id.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Message_Loc  : File_Cursor'Class;
      First_Word   : Unbounded_String;
      Second_Word  : Unbounded_String)
   is
   begin
      This.Location :=
        new Mark_Abstr'Class'(Current_Text.Get_New_Mark (Message_Loc));
      This.First_Word := First_Word;
      This.Second_Word := Second_Word;
   end Initialize;

   overriding procedure Free (This : in out Invert_Words_Cmd) is
   begin
      Free (This.Location);
      Free (Text_Command (This));
   end Free;

   overriding procedure Execute
     (This         : Invert_Words_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Matches       : Match_Array (1 .. 1);
      Matcher       : constant Pattern_Matcher :=
        Compile ("(" & To_String (This.Second_Word) & ") ", Case_Insensitive);
      First_Cursor  : constant File_Cursor := File_Cursor
        (Current_Text.Get_Current_Cursor (This.Location.all));
      Second_Cursor : File_Cursor := First_Cursor;
      Line          : Integer := Get_Line (Second_Cursor);

      Text : constant Ptr_Text := Current_Text.Get_File (Second_Cursor.File);
   begin
      loop
         Match (Matcher, Text.Get_Line (Second_Cursor, 1), Matches);

         exit when Matches (1) /= No_Match;
         Line := Line - 1;

         if Line = 0 then
            return;
         end if;

         Set_Location (Second_Cursor, Line, 1);
      end loop;

      Set_Location
        (Second_Cursor, Line, Visible_Column_Type (Matches (1).First));

      Text.Replace
        (First_Cursor, Length (This.First_Word), To_String (This.Second_Word));

      Text.Replace
        (Second_Cursor,
         Length (This.Second_Word),
         To_String (This.First_Word));
   end Execute;

   overriding
   function Is_Writable (This : Invert_Words_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Add_Line_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Line         : Unbounded_String;
      Indent       : Boolean) is
   begin
      This.Line := Line;
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
      This.Indent := Indent;
   end Initialize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Add_Line_Cmd) is
   begin
      Free (This.Position);
   end Free;

   overriding procedure Execute
     (This         : Add_Line_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Position.all);

      End_Of_Line : constant String := Current_Text.Get_Line (Cursor);
   begin
      if End_Of_Line /= "" then
         Current_Text.Replace (Cursor, End_Of_Line'Length, "");
      end if;

      Add_Line
        (Get_File (Current_Text, This.Position.File_Name).all,
         Cursor, End_Of_Line & To_String (This.Line), This.Indent);
   end Execute;

   overriding
   function Is_Writable (This : Add_Line_Cmd) return Boolean is
   begin
      return This.Position.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This                     : in out Replace_Slice_Cmd;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Cursor, End_Cursor : File_Cursor'Class;
      New_Text                 : Unbounded_String) is
   begin
      This.Start_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Start_Cursor));
      This.End_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, End_Cursor));
      This.New_Text := New_Text;
   end Initialize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Replace_Slice_Cmd) is
   begin
      Free (This.Start_Mark);
      Free (This.End_Mark);
   end Free;

   overriding procedure Execute
     (This         : Replace_Slice_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Start_Cursor, End_Cursor : File_Cursor;
      Modified_Text            : Ptr_Text;
   begin
      Start_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Start_Mark.all));
      End_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.End_Mark.all));
      Modified_Text := Current_Text.Get_File (Start_Cursor.File);

      Modified_Text.Replace
        (Start_Cursor, End_Cursor, To_String (This.New_Text));
   end Execute;

   overriding
   function Is_Writable (This : Replace_Slice_Cmd) return Boolean is
   begin
      return This.Start_Mark.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Remove_Blank_Lines_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Cursor : File_Cursor'Class)
   is
   begin
      This.Start_Mark := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Start_Cursor));
   end Initialize;

   overriding procedure Free (This : in out Remove_Blank_Lines_Cmd) is
   begin
      Free (This.Start_Mark);
   end Free;

   overriding procedure Execute
     (This         : Remove_Blank_Lines_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor := File_Cursor
        (Current_Text.Get_Current_Cursor (This.Start_Mark.all));
   begin
      Remove_Blank_Lines (Current_Text, Cursor);
   end Execute;

   procedure Remove_Blank_Lines
     (Current_Text : in out Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
   is
      Line_Cursor : File_Cursor'Class := Clone (Cursor);
      Text   : constant Ptr_Text := Current_Text.Get_File (Cursor.File);
   begin
      Line_Cursor.Col := 1;

      while Line_Cursor.Line < Text.Line_Max
        and then Is_Blank (Text.Get_Line (Line_Cursor, 1))
      loop
         Text.Delete_Line (Line_Cursor);
      end loop;

      Free (Line_Cursor);
   end Remove_Blank_Lines;

   overriding
   function Is_Writable (This : Remove_Blank_Lines_Cmd) return Boolean is
   begin
      return This.Start_Mark.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This   : in out Tab_Expansion_Cmd;
      Cursor : File_Cursor) is
   begin
      This.Cursor := Cursor;
   end Initialize;

   overriding procedure Free (This : in out Tab_Expansion_Cmd) is
   begin
      Free (Text_Command (This));
   end Free;

   overriding procedure Execute
     (This         : Tab_Expansion_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : File_Cursor renames This.Cursor;
   begin
      Current_Text.Add_Line
        (Cursor   => Cursor,
         New_Line => Do_Tab_Expansion
                       (Current_Text.Get_Line (Cursor, 1), Tab_Width),
         Indent   => False);

      Current_Text.Delete_Line (Cursor);
   end Execute;

   overriding
   function Is_Writable (This : Tab_Expansion_Cmd) return Boolean is
   begin
      return This.Cursor.Get_File.Is_Writable;
   end Is_Writable;

end Codefix.Text_Manager.Commands;

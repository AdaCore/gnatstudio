-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2006                    --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

with String_Utils;            use String_Utils;

with Codefix.Ada_Tools;       use Codefix.Ada_Tools;

package body Codefix.Text_Manager.Ada_Extracts is

   ----------------------------------------------------------------------------
   --  Internal subprograms
   ----------------------------------------------------------------------------

   ----------------
   -- Is_Comment --
   ----------------

   function Is_Comment (Line : String) return Boolean is
   begin
      for J in Line'Range loop
         if not Is_Blank (Line (J)) then
            if Line (J) /= '-'
              or else J = Line'Last
            then
               return False;
            end if;

            if Line (J + 1) = '-' then
               return True;
            else
               return False;
            end if;
         end if;
      end loop;

      return True;

   end Is_Comment;

   ----------------------------------------------------------------------------
   --  type Ada_Instruction
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ada_Instruction) is
   begin
      Free (Extract (This));
      Free (This.Start);
      Free (This.Stop);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Ada_Instruction) return Ada_Instruction is
      New_Extract : Ada_Instruction;
   begin
      New_Extract :=
        (Clone (Extract (This)) with
         Start => Clone (This.Start),
         Stop  => Clone (This.Stop));

      return New_Extract;
   end Clone;

   ----------------
   -- Is_Closest --
   ----------------

   function Is_Closest (Obj, Test1, Test2, Test3 : File_Cursor)
     return Boolean is
   begin
      if Obj = Null_File_Cursor then
         return False;
      end if;

      if Test1 /= Null_File_Cursor and then Obj < Test1 then
         return False;
      end if;

      if Test2 /= Null_File_Cursor and then Obj < Test2 then
         return False;
      end if;

      if Test3 /= Null_File_Cursor and then Obj < Test3 then
         return False;
      end if;

      return True;
   end Is_Closest;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_Instruction;
      Delimiters   : Delimiters_Array := Default_Delimiters) is

      Line_Cursor  : File_Cursor;
      Matched_Word : Word_Cursor;

   begin
      Matched_Word := Word_Cursor (Search_Strings
        (Current_Text,
           Position,
           Delimiters,
           Std_Ada_Escape,
           Reverse_Step));

      if Matched_Word = Null_Word_Cursor then
         Destination.Start := Clone (File_Cursor (Position));
         Destination.Start.Col := 1;
         Destination.Start.Line := 1;
      else
         Destination.Start := Clone (File_Cursor (Matched_Word));
         Destination.Start.Col := Destination.Start.Col +
           Get_Word (Matched_Word)'Length;
         Free (Matched_Word);
      end if;

      while Is_Blank (Get_Line (Current_Text, Destination.Start))
        or else Is_Comment (Get_Line (Current_Text, Destination.Start))
      loop
         Destination.Start.Col := 1;
         Destination.Start.Line := Destination.Start.Line + 1;
      end loop;

      Destination.Stop := File_Cursor
        (Search_Strings
           (Current_Text, Destination.Start, Delimiters, Std_Ada_Escape));

      Set_File (Line_Cursor, Get_File (Destination.Start));

      for J in Destination.Start.Line .. Destination.Stop.Line loop
         Set_Location (Line_Cursor, Line => J, Column => 1);
         Get_Line (Current_Text, Line_Cursor, Destination);
      end loop;
   end Get_Unit;

   ------------------------
   -- Remove_Instruction --
   ------------------------

   procedure Remove_Instruction (This : in out Ada_Instruction) is
   begin
      Erase (This, This.Start, This.Stop);
   end Remove_Instruction;

   ------------------------
   -- Comment_Instruction --
   ------------------------

   procedure Comment_Instruction (This : in out Ada_Instruction) is
   begin
      Comment (This, This.Start, This.Stop);
   end Comment_Instruction;

   --------------
   -- Get_Stop --
   --------------

   function Get_Start (This : Ada_Instruction) return File_Cursor is
   begin
      return This.Start;
   end Get_Start;

   --------------
   -- Get_Stop --
   --------------

   function Get_Stop (This : Ada_Instruction) return File_Cursor is
   begin
      return This.Stop;
   end Get_Stop;

   ----------------------------------------------------------------------------
   --  type Ada_List
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ada_List) is
   begin
      Free (Ada_Instruction (This));
      Free (This.Elements_List);
      Free (This.Back);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Ada_List) return Ada_List is
      New_Extract : Ada_List;
      Token_It    : Tokens_List.List_Node;
   begin
      New_Extract :=
        (Clone (Ada_Instruction (This)) with
         Elements_List => Tokens_List.Null_List,
         Back          => Clone (This.Back));

      Token_It := First (This.Elements_List);

      while Token_It /= Tokens_List.Null_Node loop
         declare
            New_Token : Token_Record := Clone (Data (Token_It));
         begin
            New_Token.Line := Get_Line
              (New_Extract, Data (Token_It).Line.Cursor);
            Append (New_Extract.Elements_List, New_Token);
            Token_It := Next (Token_It);
         end;
      end loop;

      return New_Extract;
   end Clone;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Line  : Ptr_Extract_Line;
      Index : in out Char_Index;
      Token : out Token_Record)
   is
      Content     : constant String := Get_String (Line.all);
      Buffer      : constant String :=
        Content (Natural (Index) .. Content'Last);
      Start, Stop : Char_Index := Index;

   begin
      Token.Content := null;

      Skip_Blanks (Buffer, Natural (Start));

      if Natural (Start) > Buffer'Last then
         Index := 1;
         return;
      end if;

      Stop := Start;

      loop
         if Buffer (Natural (Stop)) =  ';'
           or else Buffer (Natural (Stop)) = ','
           or else Buffer (Natural (Stop)) = ':'
         then
            if Stop /= Start then
               Stop := Stop - 1;
            elsif Natural (Stop) < Buffer'Last
              and then Is_Blank (Buffer (Natural (Stop) + 1)) then
               Stop := Stop + 1;
            end if;

            exit;
         elsif Is_Blank (Buffer (Natural (Stop))) then
            exit;
         end if;

         exit when Natural (Stop) >= Buffer'Last;
         Stop := Stop + 1;
      end loop;

      Token.Content := new String (1 .. Natural (Stop - Start) + 1);
      Token.Content.all := Buffer (Natural (Start) .. Natural (Stop));

      Token.First_Char := Start;
      Token.Last_Char := Stop;
      Token.First_Col := To_Column_Index (Start, Content);
      Token.Last_Col := To_Column_Index (Stop, Content);
      Token.Is_Separator :=
        Token.Content (1) = ','
        or else Token.Content (1) = ':'
        or else Token.Content (1) = ';';
      Token.Line := Line;
      Index := Token.Last_Char + 1;
   end Get_Token;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Token_Record) is
   begin
      Free (This.Content);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Token_Record) return Token_Record is
   begin
      return
        (Content      => Clone (This.Content),
         First_Col    => This.First_Col,
         Last_Col     => This.Last_Col,
         First_Char   => This.First_Char,
         Last_Char    => This.Last_Char,
         Line         => This.Line,
         Is_Separator => This.Is_Separator);
      --  Be careful !!! The line doesn't have to be cloned, because it is the
      --  real line contained into the Extract and not a work copy.
   end Clone;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (This : Ada_List; Num : Natural)
     return Tokens_List.List_Node is

      Current_Node : Tokens_List.List_Node := First (This.Elements_List);

   begin
      for I in 1 .. Num - 1 loop
         Current_Node := Next (Current_Node);
      end loop;

      return Current_Node;
   end Get_Element;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_List;
      Delimiters   : Delimiters_Array := Default_Delimiters)
   is
      Current_Token  : Token_Record;
      Current_Line   : Ptr_Extract_Line;
      Current_Index  : Char_Index;
      Minus          : Natural;
      Line_Cursor    : File_Cursor;

   begin
      Get_Unit
        (Current_Text, Position, Ada_Instruction (Destination), Delimiters);

      Current_Line := Get_First_Line (Destination);

      Current_Index := To_Char_Index
        (Destination.Start.Col, Get_String (Current_Line.all));

      Get_Token
        (Current_Line, Current_Index, Current_Token);

      loop
         while Current_Index = 1 loop
            Free (Current_Token);
            Current_Line := Next (Current_Line.all);

            if Current_Line = null then
               return;
            end if;

            Get_Token
              (Current_Line, Current_Index, Current_Token);
         end loop;

         declare
            Token_Without_Last_Blanks : constant String :=
              Without_Last_Blanks (Current_Token.Content.all);
         begin
            exit when Token_Without_Last_Blanks = ":="
              or else Token_Without_Last_Blanks = "=>"
              or else Token_Without_Last_Blanks = ";";

            if To_Lower
              (Token_Without_Last_Blanks) /= "with"
              and then To_Lower
                (Token_Without_Last_Blanks) /= "use"
            then
               Append (Destination.Elements_List, Current_Token);
            else
               Free (Current_Token);
            end if;
         end;

         Get_Token
           (Current_Line, Current_Index, Current_Token);

      end loop;

      Assign
        (Destination.Back,
         Get_String (Current_Line.all)
         (Natural (Current_Token.First_Char)
            .. Get_String (Current_Line.all)'Last));

      Current_Line := Next (Current_Line.all);

      loop
         exit when Current_Line = null;
         Assign
           (Destination.Back,
            Destination.Back.all & Get_String (Current_Line.all));
         Current_Line := Next (Current_Line.all);
      end loop;

      Line_Cursor := File_Cursor (Destination.Stop);
      Line_Cursor.Col := 1;

      declare
         Current_Line : constant String :=
           Get_Line (Current_Text, Line_Cursor);
      begin
         Minus := Current_Line'Last -
           Natural (To_Char_Index (Destination.Stop.Col, Current_Line));
      end;

      Assign
        (Destination.Back,
         Destination.Back.all
           (Destination.Back'First ..
              Destination.Back'Last - Minus));

      Free (Current_Token);

   end Get_Unit;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      New_Instr   : out GNAT.OS_Lib.String_Access;
      First       : Natural;
      Last        : Natural := 0) is

      function Elements_Select
        (Current_Element : Tokens_List.List_Node;
         Number_Left     : Natural;
         Append_Type     : Boolean := False) return String;
      --  This function return a string containing n elements from the token
      --  given in parameter. It should be called on the first token, with
      --  Append_Type set to false. When Append_Type goes to true, when the
      --  type part will be appened.

      Last_Used  : Natural;

      function Elements_Select
        (Current_Element : Tokens_List.List_Node;
         Number_Left     : Natural;
         Append_Type     : Boolean := False) return String
      is

      begin

         if Append_Type then
            if Current_Element = Tokens_List.Null_Node then
               return This.Back.all;
            else
               return Data (Current_Element).Content.all
                 & Elements_Select
                 (Next (Current_Element), 0, True);
            end if;
         else
            if Number_Left = 0 then
               declare
                  Seek : Tokens_List.List_Node := Current_Element;
               begin
                  while Seek /= Tokens_List.Null_Node
                    and then Without_Last_Blanks
                      (Data (Seek).Content.all) /= ":"
                  loop
                     Seek := Next (Seek);
                  end loop;

                  if Seek = Tokens_List.Null_Node then
                     return This.Back.all;
                  else
                     return Data (Seek).Content.all
                       & Elements_Select
                       (Next (Seek), 0, True);
                  end if;
               end;
            else
               return Data (Current_Element).Content.all & Elements_Select
                 (Next (Current_Element), Number_Left - 1, False);
            end if;
         end if;
      end Elements_Select;

   begin
      if Last = 0 then
         Last_Used := First;
      else
         Last_Used := Last;
      end if;

      New_Instr := new String'
        (Elements_Select
           (Get_Element (This, First),
            Last_Used - First + 1));

      Remove_Elements (This, Erase, First, Last_Used);

   end Cut_Off_Elements;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      New_Instr   : out GNAT.OS_Lib.String_Access;
      First       : String;
      Last        : String := "") is
   begin
      if Last = "" then
         Cut_Off_Elements
           (This, New_Instr, Get_Nth_Element (This, First), 0);
      else
         Cut_Off_Elements
           (This,
            New_Instr,
            Get_Nth_Element (This, First),
            Get_Nth_Element (This, Last));
      end if;
   end Cut_Off_Elements;

   -------------------------
   -- Get_Number_Elements --
   -------------------------

   function Get_Number_Of_Elements (This : Ada_List) return Natural is
   begin
      return Length (This.Elements_List);
   end Get_Number_Of_Elements;

   --------------------------------
   -- Get_Number_Of_Declarations --
   --------------------------------

   function Get_Number_Of_Declarations (This : Ada_List) return Natural is
      Node  : Tokens_List.List_Node := First (This.Elements_List);
      Total : Natural := 0;
   begin
      while Node /= Tokens_List.Null_Node loop
         declare
            String_Token : constant String :=
              Without_Last_Blanks (Data (Node).Content.all);
         begin
            exit when String_Token = ":";
         end;

         Total := Total + 1;
         Node := Next (Node);
      end loop;

      return Total;
   end Get_Number_Of_Declarations;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This  : in out Ada_List;
      Mode  : Remove_Code_Mode;
      First : Natural;
      Last  : Natural := 0)
   is
      Last_Used  : Natural;
      First_Used : Natural;
   begin
      if Last = 0 then
         Last_Used := First;
      else
         Last_Used := Last;
      end if;

      First_Used := First;

      if First_Used > 1 then
         if Data
           (Get_Element (This, First_Used - 1)).Content.all (1) = ','
         then
            First_Used := First_Used - 1;
            --  -1 comments the previous character, the ','.
         elsif To_Lower (Without_Last_Blanks
           (Data (Get_Element (This, First_Used - 1)).Content.all)) = "when"
         then
            Last_Used := Last_Used + 1;
         end if;
      elsif Length (This.Elements_List) > 1
        and then Data (Get_Element (This, Last_Used + 1)).Content.all (1) = ','
      then
         Last_Used := Last_Used + 1;
         --  In this case, there is no previous character, so the next ','
         --  is deleted.
      end if;

      --  If we are not in the case of an exception and the whole declaration
      --  has to be removed, then just remove it.

      if To_Lower (Without_Last_Blanks
        (Data (Get_Element (This, 1)).Content.all)) /= "when"
        and then First_Used = 1
        and then (Last_Used = Length (This.Elements_List)
                  or else Data
                    (Get_Element (This, Last_Used + 1)).Content.all (1) = ':')
      then
         case Mode is
            when Erase =>
               Remove_Instruction (This);
            when Comment =>
               Comment_Instruction (This);
         end case;

         return;
      end if;

      declare
         First_Token : constant Token_Record :=
           Data (Get_Element (This, First_Used));
         Last_Token  : constant Token_Record :=
           Data (Get_Element (This, Last_Used));

         Cursor_Begin : File_Cursor'Class :=
           Get_Cursor (First_Token.Line.all);
         Cursor_End : File_Cursor'Class :=
           Get_Cursor (Last_Token.Line.all);
      begin
         Set_Location
           (Cursor_Begin,
            Get_Line (Cursor_Begin),
            First_Token.First_Col);
         Set_Location
           (Cursor_End,
            Get_Line (Cursor_End),
            Last_Token.Last_Col);

         case Mode is
            when Erase =>
               Erase (This, Cursor_Begin, Cursor_End);
            when Comment =>
               Comment (This, Cursor_Begin, Cursor_End);
         end case;
      end;

      Remove_Nodes (This.Elements_List, Tokens_List.Null_Node);
   end Remove_Elements;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This  : in out Ada_List;
      Mode  : Remove_Code_Mode;
      First : String;
      Last  : String := "") is
   begin
      if Last = "" then
         Remove_Elements (This, Mode, Get_Nth_Element (This, First), 0);
      else
         Remove_Elements
           (This,
            Mode,
            Get_Nth_Element (This, First),
            Get_Nth_Element (This, Last));
      end if;
   end Remove_Elements;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (This : Ada_List; Num : Natural) return Word_Cursor is
      Node   : Tokens_List.List_Node := First (This.Elements_List);
      Word   : Word_Cursor;
   begin
      for J in 1 .. Num - 1 loop
         Node := Next (Node);
      end loop;

      Set_File (Word, Get_File (Data (Node).Line.Cursor));
      Set_Location (Word,
                    Line   => Data (Node).Line.Cursor.Line,
                    Column => Data (Node).First_Col);
      Set_Word (Word, Data (Node).Content.all, Text_Ascii);
      return Word;
   end Get_Element;

   ---------------------
   -- Get_Nth_Element --
   ---------------------

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural is
      Current_Num  : Natural := 1;
      Current_Node : Tokens_List.List_Node := First (This.Elements_List);

   begin
      loop
         if Compare_Last
           (Without_Last_Blanks (Data (Current_Node).Content.all),
            Without_Last_Blanks (Name))
         then
            return Current_Num;
         end if;

         Current_Num := Current_Num + 1;

         Current_Node := Next (Current_Node);
         exit when Current_Node = Tokens_List.Null_Node;
      end loop;

      raise Codefix_Panic;
   end Get_Nth_Element;

   --------------
   -- Is_Alone --
   --------------

   function Is_Alone (This : Token_Record)
     return Boolean is
      Content : constant String := Get_String (This.Line.all);
   begin
      return Is_Blank
        (Content (Content'First .. Natural (This.First_Char) - 1))
        and then Is_Blank
          (Content (Content'First + Natural (This.Last_Char) .. Content'Last));
   end Is_Alone;

end Codefix.Text_Manager.Ada_Extracts;

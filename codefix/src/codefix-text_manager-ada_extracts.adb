-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with String_Utils; use String_Utils;

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
      Destination  : in out Ada_Instruction) is

      C_Declare, C_Begin, C_Is, C_Semicolon : File_Cursor;
      Line_Cursor                           : File_Cursor;
      Str_Declare                           : constant String := "declare";
      Str_Begin                             : constant String := "begin";
      Str_Is                                : constant String := "is";
      Str_Semicolon                         : constant String := ";";

   begin

      C_Declare := File_Cursor
        (Search_String (Current_Text, Position, Str_Declare, Reverse_Step));
      C_Begin := File_Cursor
        (Search_String (Current_Text, Position, Str_Begin, Reverse_Step));
      C_Is := File_Cursor
        (Search_String (Current_Text, Position, Str_Is, Reverse_Step));
      C_Semicolon := File_Cursor
        (Search_String (Current_Text, Position, Str_Semicolon, Reverse_Step));

      if Is_Closest (C_Declare, C_Begin, C_Is, C_Semicolon) then
         Destination.Start := Clone (C_Declare);
         Destination.Start.Col := Destination.Start.Col + Str_Declare'Length;
      elsif Is_Closest (C_Begin, C_Is, C_Semicolon, C_Declare) then
         Destination.Start := Clone (C_Begin);
         Destination.Start.Col := Destination.Start.Col + Str_Begin'Length;
      elsif Is_Closest (C_Is, C_Semicolon, C_Declare, C_Begin) then
         Destination.Start := Clone (C_Is);
         Destination.Start.Col := Destination.Start.Col + Str_Is'Length;
      elsif Is_Closest (C_Semicolon, C_Declare, C_Begin, C_Is) then
         Destination.Start := Clone (C_Semicolon);
         Destination.Start.Col := Destination.Start.Col + Str_Semicolon'Length;
      else
         Destination.Start := Clone (File_Cursor (Position));
         Destination.Start.Col := 1;
         Destination.Start.Line := 1;
      end if;

      while Is_Blank (Get_Line (Current_Text, Destination.Start))
        or else Is_Comment (Get_Line (Current_Text, Destination.Start))
      loop
         Destination.Start.Col := 1;
         Destination.Start.Line := Destination.Start.Line + 1;
      end loop;

      Destination.Stop := File_Cursor
        (Search_String (Current_Text, Destination.Start, ";"));

      Line_Cursor.File_Name := Destination.Start.File_Name;
      Line_Cursor.Col := 1;

      for J in Destination.Start.Line .. Destination.Stop.Line loop
         Line_Cursor.Line := J;
         Get_Line (Current_Text, Line_Cursor, Destination);
      end loop;

      Free (C_Declare);
      Free (C_Begin);
      Free (C_Is);
      Free (C_Semicolon);

   end Get_Unit;

   ------------------------
   -- Remove_Instruction --
   ------------------------

   procedure Remove_Instruction (This : in out Ada_Instruction) is
   begin
      Erase (This, This.Start, This.Stop);
   end Remove_Instruction;

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

      while Token_It /= Tokens_List.Null_Node loop
         Append (New_Extract.Elements_List, Clone (Data (Token_It)));
         Token_It := Next (Token_It);
      end loop;

      return New_Extract;
   end Clone;

   ---------------
   -- Get_Token --
   ---------------

   --  Maybe without Buffer ???
   procedure Get_Token
     (Line      : Ptr_Extract_Line;
      Col       : in out Integer;
      Token     : out Token_Record) is

      Buffer      : constant String := Line.Content (Col .. Line.Content'Last);
      Start, Stop : Integer := Col;

   begin

      Token.Content := null; --  ??? Why, without this line, get I a SEGV ?

      Skip_Blanks (Buffer, Start);

      if Start > Buffer'Last then
         Col := 1;
         return;
      end if;

      Stop := Start;

      loop
         if Buffer (Stop) =  ';'
           or else Buffer (Stop) = ','
           or else Buffer (Stop) = ':'
           or else Is_Blank (Buffer (Stop))
         then
            if Stop /= Start then
               Stop := Stop - 1;
            end if;

            exit;
         end if;

         exit when Stop >= Buffer'Last;
         Stop := Stop + 1;
      end loop;

      Token.Content := new String (1 .. Stop - Start + 1);
      Token.Content.all := Buffer (Start .. Stop);

      Token.First_Col := Start;
      Token.Last_Col := Stop;
      Token.Is_Separator :=
        Token.Content.all = ","
        or else Token.Content.all = ":"
        or else Token.Content.all = ";";
      Token.Line := Line;
      Col := Token.Last_Col + 1;

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
         Line         => This.Line,
         Is_Separator => This.Is_Separator);
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
      Destination  : in out Ada_List) is

      Current_Token  : Token_Record;
      Current_Line   : Ptr_Extract_Line;
      Current_Col    : Natural;
      Minus          : Natural;
      Line_Cursor    : File_Cursor;

   begin
      Get_Unit (Current_Text, Position, Ada_Instruction (Destination));

      Current_Line := Get_First_Line (Destination);

      Current_Col := Destination.Start.Col;

      Get_Token
        (Current_Line, Current_Col, Current_Token);

      loop
         while Current_Col = 1 loop
            Free (Current_Token);
            Current_Line := Next (Current_Line.all);
            Get_Token
              (Current_Line, Current_Col, Current_Token);
         end loop;

         exit when Current_Token.Content.all = ":"
           or else Current_Token.Content.all = ";";

         --  ??? Make the same tests without case problems
         if Current_Token.Content.all /= "with"
           and then Current_Token.Content.all /= "use"
         then
            Append (Destination.Elements_List, Current_Token);
         else
            Free (Current_Token);
         end if;

         Get_Token
           (Current_Line, Current_Col, Current_Token);

      end loop;

      Assign
        (Destination.Back,
         Current_Line.Content
           (Current_Token.First_Col .. Current_Line.Content'Last));

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
      Minus := Get_Line (Current_Text, Line_Cursor)'Last -
        Destination.Stop.Col;

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
      New_Instr   : out Dynamic_String;
      First       : Natural;
      Last        : Natural := 0) is

      function Elements_Select
        (Current_Element : Tokens_List.List_Node;
         Number_Left     : Natural) return String;

      Last_Used  : Natural;

      function Elements_Select
        (Current_Element : Tokens_List.List_Node;
         Number_Left     : Natural) return String is
      begin

         if Number_Left = 0 then
            return This.Back.all;
         end if;

         return Data (Current_Element).Content.all &
           Elements_Select (Next (Current_Element), Number_Left - 1);

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

      Remove_Elements (This, First, Last_Used);

   end Cut_Off_Elements;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      New_Instr   : out Dynamic_String;
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

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This  : in out Ada_List; First : Natural; Last : Natural := 0) is

      Current_Element : Tokens_List.List_Node;
      Garbage_Node    : Tokens_List.List_Node;
      Last_Used       : Natural;
      First_Used      : Natural;

      Offset_Char     : Integer := 0;
      Previous_Line   : Ptr_Extract_Line;

   begin

      if Last = 0 then
         Last_Used := First;
      else
         Last_Used := Last;
      end if;

      First_Used := First;

      if First_Used > 1 then
         First_Used := First_Used - 1;
         --  -1 deletes the precedent character, the ','.
      else
         Last_Used := Last_Used + 1;
         --  In this case, there is no precedent character, so the next ','
         --  is deleted.
      end if;

      if Last_Used - First_Used + 1 >=  Length (This.Elements_List) then
         Remove_Instruction (This);
         return;
      end if;

      Current_Element := Get_Element (This, First_Used);

      for J in First_Used .. Last_Used loop
         if Data (Current_Element).Line /= Previous_Line then
            Offset_Char := 0;
            Previous_Line := Data (Current_Element).Line;
         end if;

         if Is_Alone (Data (Current_Element), -Offset_Char) then
            Data (Current_Element).Line.Context := Line_Deleted;
         else
            Set_String
              (Data (Current_Element).Line.all,
               "",
               Data (Current_Element).First_Col - Offset_Char,
               Data (Current_Element).Last_Col - Offset_Char);

            if Data (Current_Element).First_Col /= 1 then
               Offset_Char := Offset_Char + Data (Current_Element).Last_Col -
                 Data (Current_Element).First_Col + 1;
               --  If First_Col = 1, then the columns in the string are not
               --  corrupted.
            end if;
         end if;

         Garbage_Node := Current_Element;
         Current_Element := Next (Current_Element);

         Remove_Nodes
           (This.Elements_List,
            Prev (This.Elements_List, Garbage_Node),
            Garbage_Node);
      end loop;

   end Remove_Elements;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This  : in out Ada_List; First : String; Last : String := "") is
   begin
      if Last = "" then
         Remove_Elements (This, Get_Nth_Element (This, First), 0);
      else
         Remove_Elements
           (This, Get_Nth_Element (This, First), Get_Nth_Element (This, Last));
      end if;
   end Remove_Elements;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (This : Ada_List; Num : Natural) return String is
      pragma Unreferenced (This, Num);
   begin
      return "";
   end Get_Element;

   ---------------------
   -- Get_Nth_Element --
   ---------------------

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural is

      Current_Num  : Natural := 1;
      Current_Node : Tokens_List.List_Node := First (This.Elements_List);

   begin
      loop
         if Compare_Last (Data (Current_Node).Content.all, Name) then
            return Current_Num;
         end if;

         Current_Num := Current_Num + 1;
         Current_Node := Next (Current_Node);
         exit when Current_Node = Tokens_List.Null_Node;
      end loop;

      return 0;
   end Get_Nth_Element;

   --------------
   -- Is_Alone --
   --------------

   function Is_Alone (This : Token_Record; Offset_Col : Integer)
     return Boolean is
   begin
      return Is_Blank
        (This.Line.Content (This.Line.Content'First ..
                              This.First_Col - 1 + Offset_Col))
        and then Is_Blank
          (This.Line.Content (This.Last_Col + 1 + Offset_Col ..
                                This.Line.Content'Last));
   end Is_Alone;

end Codefix.Text_Manager.Ada_Extracts;

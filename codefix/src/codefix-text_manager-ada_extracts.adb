-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2008, AdaCore                 --
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
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with Language.Ada;            use Language.Ada;
with Codefix.Ada_Tools;       use Codefix.Ada_Tools;

package body Codefix.Text_Manager.Ada_Extracts is

   ----------------------------
   --  Internal subprograms  --
   ----------------------------

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
        (Start => new Mark_Abstr'Class'(This.Start.all),
         Stop  => new Mark_Abstr'Class'(This.Stop.all));

      return New_Extract;
   end Clone;

   --------------
   -- Get_Unit --
   --------------

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_Instruction;
      Delimiters   : Delimiters_Array := Default_Delimiters)
   is
      Matched_Word : Word_Cursor;
      Start_Cur, Stop_Cur : File_Cursor;
   begin
      Matched_Word := Word_Cursor (Search_Tokens
        (Current_Text,
           Position,
           Delimiters,
           Reverse_Step));

      if Matched_Word = Null_Word_Cursor then
         Start_Cur := Clone (File_Cursor (Position));
         Start_Cur.Col := 1;
         Start_Cur.Line := 1;
      else
         Start_Cur := Clone (File_Cursor (Matched_Word));
         Start_Cur.Col := Start_Cur.Col +
           Get_Word (Matched_Word)'Length;
         Free (Matched_Word);
      end if;

      while Is_Blank (Get_Line (Current_Text, Start_Cur))
        or else Is_Comment (Get_Line (Current_Text, Start_Cur))
      loop
         Start_Cur.Col := 1;
         Start_Cur.Line := Start_Cur.Line + 1;
      end loop;

      Stop_Cur := File_Cursor
        (Search_Tokens
           (Current_Text, Start_Cur, Delimiters));

      Destination.Start := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Start_Cur));
      Destination.Stop := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Stop_Cur));
   end Get_Unit;

   ------------------------
   -- Remove_Instruction --
   ------------------------

   procedure Remove_Instruction
     (This : in out Ada_Instruction;
      Text_Nav : in out Text_Navigator_Abstr'Class)
   is
      Text : constant Ptr_Text :=
        Text_Nav.Get_File (This.Get_Start (Text_Nav).File);
   begin
      Text.Erase (This.Get_Start (Text_Nav), This.Get_Stop (Text_Nav));
   end Remove_Instruction;

   ------------------------
   -- Comment_Instruction --
   ------------------------

   procedure Comment_Instruction
     (This : in out Ada_Instruction;
      Text_Nav : in out Text_Navigator_Abstr'Class)
   is
      Text : constant Ptr_Text :=
        Text_Nav.Get_File (This.Get_Start (Text_Nav).File);
   begin
      Text.Comment (This.Get_Start (Text_Nav), This.Get_Stop (Text_Nav));
   end Comment_Instruction;

   --------------
   -- Get_Stop --
   --------------

   function Get_Start
     (This     : Ada_Instruction;
      Text_Nav : Text_Navigator_Abstr'Class) return File_Cursor
   is
      Cursor : constant File_Cursor'Class :=
        Text_Nav.Get_Current_Cursor (This.Start.all);
   begin
      return File_Cursor (Cursor);
   end Get_Start;

   --------------
   -- Get_Stop --
   --------------

   function Get_Stop
     (This : Ada_Instruction;
      Text_Nav : Text_Navigator_Abstr'Class) return File_Cursor
   is
      Cursor : constant File_Cursor'Class :=
        Text_Nav.Get_Current_Cursor (This.Stop.all);
   begin
      return File_Cursor (Cursor);
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
         Elements_List => Tokens_List.Null_List);

      Token_It := First (This.Elements_List);

      while Token_It /= Tokens_List.Null_Node loop
         declare
            New_Token : constant Token_Record := Clone (Data (Token_It));
         begin
            Append (New_Extract.Elements_List, New_Token);
            Token_It := Next (Token_It);
         end;
      end loop;

      return New_Extract;
   end Clone;

   --------------------
   -- Get_Text_Slice --
   --------------------

   procedure Get_Text_Slice
     (This                     : Ada_List;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Index, End_Index   : Integer;
      Start_Cursor, End_Cursor : out File_Cursor)
   is
      First_Token : constant Token_Record :=
        Data (Get_Element (This, Start_Index));
      Last_Token  : constant Token_Record :=
        Data (Get_Element (This, End_Index));

      Begin_Char_Index, End_Char_Index : Char_Index;

   begin
      Start_Cursor := First_Token.Line;
      End_Cursor := Last_Token.Line;

      Begin_Char_Index := First_Token.First_Char;
      End_Char_Index := Last_Token.Last_Char;

      declare
         First_Line : constant String := Current_Text.Get_Line
           (Start_Cursor, 1);
         Last_Line : constant String := Current_Text.Get_Line
           (End_Cursor, 1);
      begin

         if Start_Index > 0
           and then First_Token.Content (First_Token.Content'First) = ':'
         then
            while Integer (Begin_Char_Index - 1) in First_Line'Range
              and then Is_Blank (First_Line (Integer (Begin_Char_Index - 1)))
            loop
               Begin_Char_Index := Begin_Char_Index - 1;
            end loop;
         end if;

         if End_Index < Length (This.Elements_List) - 1 and then
           (Last_Token.Content (Last_Token.Content'Last) = ':'
            or else Last_Token.Content (Last_Token.Content'Last) = ',')
         then
            while Integer (End_Char_Index + 1) in Last_Line'Range
              and then Is_Blank (Last_Line (Integer (End_Char_Index + 1)))
            loop
               End_Char_Index := End_Char_Index + 1;
            end loop;
         end if;

         Set_Location
           (Start_Cursor,
            Get_Line (Start_Cursor),
            To_Column_Index (Begin_Char_Index, First_Line));
         Set_Location
           (End_Cursor,
            Get_Line (End_Cursor),
            To_Column_Index (End_Char_Index, Last_Line));
      end;
   end Get_Text_Slice;

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
         First_Char   => This.First_Char,
         Last_Char    => This.Last_Char,
         Line         => This.Line);
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
      Start_Cur, Stop_Cur : File_Cursor;
   begin
      Get_Unit
        (Current_Text, Position, Ada_Instruction (Destination), Delimiters);

      Start_Cur := Destination.Get_Start (Current_Text);
      Stop_Cur := Destination.Get_Stop (Current_Text);

      declare
         Line_Offset   : constant Integer := Start_Cur.Line;
         Column_Offset : constant Integer := Integer (Start_Cur.Col);
         Instruction   : constant String :=
           Get (Current_Text, Start_Cur, Stop_Cur);

         function Entity_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;

         function Entity_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            Name : constant String := Instruction
              (Sloc_Start.Index .. Sloc_End.Index);

            Real_Start_Col : Integer := Sloc_Start.Column;
            Real_End_Col   : Integer := Sloc_End.Column;
         begin
            if Sloc_Start.Line = 1 then
               Real_Start_Col := Real_Start_Col + Column_Offset - 1;
               Real_End_Col := Real_End_Col + Column_Offset - 1;
            end if;

            if Entity = Operator_Text
              and then (Name = ":=" or else Name = "=>")
            then
               declare
                  New_Token   : Token_Record;
                  Back_Cursor : File_Cursor'Class := Position;
               begin
                  Back_Cursor.Line := Sloc_End.Line + Line_Offset - 1;
                  Back_Cursor.Col := To_Column_Index
                    (Char_Index (Real_Start_Col),
                     Get_Line (Current_Text, Back_Cursor, 1));

                  New_Token.First_Char := Char_Index (Real_Start_Col);
                  New_Token.Last_Char := To_Char_Index
                    (Stop_Cur.Col,
                     Get_Line (Current_Text, Back_Cursor, 1));
                  New_Token.Content := new String'
                    (Get (Current_Text, Back_Cursor, Stop_Cur));
                  New_Token.Line := File_Cursor (Back_Cursor);

                  Append (Destination.Elements_List, New_Token);

                  return True;
               end;
            elsif not Equal (Name, "use", False)
              and then not Equal (Name, "with", False)
              and then not Equal (Name, "type", False)
              and then Name /= ";"
            then
               declare
                  New_Token   : Token_Record;
                  Line_Cursor : File_Cursor'Class := Position;
               begin
                  Line_Cursor.Line := Sloc_Start.Line + Line_Offset - 1;
                  Line_Cursor.Col := 1;

                  New_Token.First_Char := Char_Index (Real_Start_Col);
                  New_Token.Last_Char := Char_Index (Real_End_Col);
                  New_Token.Content := new String'(Name);
                  New_Token.Line := File_Cursor (Line_Cursor);

                  Append (Destination.Elements_List, New_Token);
               end;
            end if;

            return False;
         end Entity_Callback;
      begin
         Parse_Entities
           (Ada_Lang,
            Instruction,
            Entity_Callback'Unrestricted_Access);
      end;
   end Get_Unit;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This         : in out Ada_List;
      Text_Nav     : in out Text_Navigator_Abstr'Class;
      New_Instr    : out GNAT.Strings.String_Access;
      Current_Text : Text_Navigator_Abstr'Class;
      First        : Natural;
      Last         : Natural := 0)
   is
      Last_Used  : Natural;
      Semicolon_Index : Natural;

      Cursor_Begin_Data, Cursor_End_Data : File_Cursor;
      Cursor_Begin_Type, Cursor_End_Type : File_Cursor;

   begin
      if Last = 0 then
         Last_Used := First;
      else
         Last_Used := Last;
      end if;

      Get_Text_Slice
        (This, Text_Nav, First, Last_Used, Cursor_Begin_Data, Cursor_End_Data);

      declare
         Seek : Tokens_List.List_Node := Get_Element (This, Last_Used);
      begin
         Semicolon_Index := Last_Used;

         while Seek /= Tokens_List.Null_Node
           and then Data (Seek).Content.all /= ":"
         loop
            Seek := Next (Seek);
            Semicolon_Index := Semicolon_Index + 1;
         end loop;

         Get_Text_Slice
           (This,
            Text_Nav,
            Semicolon_Index,
            Length (This.Elements_List),
            Cursor_Begin_Type, Cursor_End_Type);
      end;

      New_Instr := new String'
        (Get (Current_Text, Cursor_Begin_Data, Cursor_End_Data)
         & Get (Current_Text, Cursor_Begin_Type, Cursor_End_Type));

      Remove_Elements (This, Text_Nav, Erase, First, Last_Used);
   end Cut_Off_Elements;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This         : in out Ada_List;
      Text_Nav     : in out Text_Navigator_Abstr'Class;
      New_Instr    : out GNAT.Strings.String_Access;
      Current_Text : Text_Navigator_Abstr'Class;
      First        : String;
      Last         : String := "") is
   begin
      if Last = "" then
         Cut_Off_Elements
           (This, Text_Nav, New_Instr, Current_Text,
            Get_Nth_Element (This, First), 0);
      else
         Cut_Off_Elements
           (This,
            Text_Nav,
            New_Instr,
            Current_Text,
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
         exit when Data (Node).Content.all = ":";

         Total := Total + 1;
         Node := Next (Node);
      end loop;

      return Total;
   end Get_Number_Of_Declarations;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This     : in out Ada_List;
      Text_Nav : in out Text_Navigator_Abstr'Class;
      Mode     : Remove_Code_Mode;
      First    : Natural;
      Last     : Natural := 0)
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
           (Get_Element (This, First_Used - 1)).Content.all = ","
         then
            First_Used := First_Used - 1;
            --  -1 comments the previous character, the ','.
         elsif To_Lower
           (Data (Get_Element (This, First_Used - 1)).Content.all) = "when"
         then
            Last_Used := Last_Used + 1;
         end if;
      elsif Length (This.Elements_List) > 1
        and then Data (Get_Element (This, Last_Used + 1)).Content.all = ","
      then
         Last_Used := Last_Used + 1;
         --  In this case, there is no previous character, so the next ','
         --  is deleted.
      end if;

      --  If we are not in the case of an exception and the whole declaration
      --  has to be removed, then just remove it.

      if To_Lower (Data (Get_Element (This, 1)).Content.all) /= "when"
        and then First_Used = 1
        and then (Last_Used = Length (This.Elements_List)
                  or else Data
                    (Get_Element (This, Last_Used + 1)).Content.all = ":")
      then
         case Mode is
            when Erase =>
               Remove_Instruction (This, Text_Nav);
            when Comment =>
               Comment_Instruction (This, Text_Nav);
         end case;

         return;
      end if;

      declare
         Cursor_Begin, Cursor_End : File_Cursor;
         Text                     : Ptr_Text;
      begin
         Get_Text_Slice
           (This, Text_Nav, First_Used, Last_Used, Cursor_Begin, Cursor_End);

         Text := Text_Nav.Get_File (Cursor_Begin.File);

         case Mode is
            when Erase =>
               Text.Erase (Cursor_Begin, Cursor_End);
            when Comment =>
               Text.Comment (Cursor_Begin, Cursor_End);
         end case;
      end;

      Remove_Nodes (This.Elements_List, Tokens_List.Null_Node);
   end Remove_Elements;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This     : in out Ada_List;
      Text_Nav : in out Text_Navigator_Abstr'Class;
      Mode     : Remove_Code_Mode;
      First    : String;
      Last     : String := "") is
   begin
      if Last = "" then
         Remove_Elements
           (This, Text_Nav, Mode, Get_Nth_Element (This, First), 0);
      else
         Remove_Elements
           (This,
            Text_Nav,
            Mode,
            Get_Nth_Element (This, First),
            Get_Nth_Element (This, Last));
      end if;
   end Remove_Elements;

   ---------------------
   -- Get_Nth_Element --
   ---------------------

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural is
      Current_Num  : Natural := 1;
      Current_Node : Tokens_List.List_Node := First (This.Elements_List);

   begin
      loop
         if Compare_Last
           (Data (Current_Node).Content.all,
            Without_Last_Blanks (Name))
         then
            return Current_Num;
         end if;

         Current_Num := Current_Num + 1;

         Current_Node := Next (Current_Node);
         exit when Current_Node = Tokens_List.Null_Node;
      end loop;

      raise Obscolescent_Fix;
   end Get_Nth_Element;

end Codefix.Text_Manager.Ada_Extracts;

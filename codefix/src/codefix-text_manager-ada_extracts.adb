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

package body Codefix.Text_Manager.Ada_Extracts is

   ----------------------------------------------------------------------------
   --  Internal subprograms
   ----------------------------------------------------------------------------

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Str : String) return Boolean is
      Blank_Str : constant String (Str'First .. Str'Last) := (others => ' ');
   begin
      return Str = Blank_Str;
   end Is_Blank;

   ----------------
   -- Is_Comment --
   ----------------

   function Is_Comment (Line : String) return Boolean is
   begin
      for J in Line'Range loop
         if Line (J) /= ' ' then
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

   -----------
   -- Clone --
   -----------

   function Clone (This : Ada_Instruction) return Ada_Instruction is
   begin
      return This;
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
      Str_Is                                : constant String := "declare";
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

         while Is_Comment (Get_Line (Current_Text, Destination.Start)) loop
            Destination.Start.Line := Destination.Start.Line + 1;
         end loop;
      end if;

      while Is_Blank (Get_Line (Current_Text, Destination.Start)) loop
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
      Current_Line : Ptr_Extract_Line := Get_First_Line (This);
      Next_Line    : Ptr_Extract_Line;
   begin

      if Get_Number_Lines (This) = 1 then
         Assign
           (Current_Line.Content,
            Current_Line.Content (1 .. This.Start.Col - 1) &
              Current_Line.Content
                (This.Stop.Col + 1 .. Current_Line.Content'Last));
         if Is_Blank (Current_Line.Content.all) then
            Current_Line.Context := Line_Deleted;
         else
            Current_Line.Context := Line_Modified;
         end if;
         return;
      end if;

      Assign
        (Current_Line.Content,
         Current_Line.Content (1 .. This.Start.Col - 1));

      if Is_Blank (Current_Line.Content.all) then
         Current_Line.Context := Line_Deleted;
      else
         Current_Line.Context := Line_Modified;
      end if;

      loop
         Next_Line := Next (Current_Line.all);
         exit when Next_Line = null;
         Current_Line := Next_Line;
         Current_Line.Context := Line_Deleted;
      end loop;

      Assign
        (Current_Line.Content,
         Current_Line.Content
           (This.Stop.Col + 1 .. Current_Line.Content'Last));

      if Is_Blank (Current_Line.Content.all) then
         Current_Line.Context := Line_Deleted;
      else
         Current_Line.Context := Line_Modified;
      end if;

   end Remove_Instruction;

   ----------------------------------------------------------------------------
   --  type Ada_List
   ----------------------------------------------------------------------------

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Str : String) return Boolean is
   begin
      return Str = "," or else Str = ":" or else Str = ";";
   end Is_Separator;

   ---------------
   -- Get_Token --
   ---------------

   --  Maybe without Buffer ???
   procedure Get_Token
     (Line      : Extract_Line;
      Col       : in out Integer;
      Token     : out Token_Record;
      Str_Token : in out Dynamic_String) is

      Buffer      : constant String := Line.Content (Col .. Line.Content'Last);
      Start, Stop : Integer := Col;

   begin
      while Start <= Buffer'Last and then Buffer (Start) = ' ' loop
         Start := Start + 1;
      end loop;

      if Start > Buffer'Last then
         Col := 1;
         return;
      end if;

      Stop := Start;

      loop
         if Buffer (Stop) =  ';'
           or else Buffer (Stop) = ','
           or else Buffer (Stop) = ':'
           or else Buffer (Stop) = ' '
         then
            if Stop /= Start then
               Stop := Stop - 1;
            end if;
            exit;
         end if;
         exit when Stop >= Buffer'Last;
         Stop := Stop + 1;
      end loop;

      Assign (Str_Token, Buffer (Start .. Stop));
      Token.First_Col := Start;
      Token.Last_Col := Stop;
      Col := Token.Last_Col + 1;

   end Get_Token;

   ---------
   -- Add --
   ---------

   procedure Add
     (File_Name      : String;
      Destination    : in out Ada_List;
      First_Token    : Token_Record;
      Last_Token     : Token_Record;
      Str_Last_Token : String) is

      New_Element : Element;
      Line_Cursor : File_Cursor;
      Name_Cursor : File_Cursor;

   begin

      Assign (Line_Cursor.File_Name, File_Name);
      Assign (Name_Cursor.File_Name, File_Name);

      Name_Cursor.Col := First_Token.First_Col;
      Name_Cursor.Line := First_Token.Line;

      Assign (New_Element.Name,
              Get_String (Get_Line (Destination, Name_Cursor).all)
                (First_Token.First_Col .. First_Token.Last_Col));

      Line_Cursor.Col := 1;
      New_Element.Lines :=
        new Lines_Array (1 .. Last_Token.Line - First_Token.Line + 1);

      for J in New_Element.Lines'Range loop
         Line_Cursor.Line := J + First_Token.Line - 1;
         New_Element.Lines (J) := Get_Line (Destination, Line_Cursor);
      end loop;

      New_Element.First_Col := First_Token.First_Col;

      if Str_Last_Token = "," then
         New_Element.Last_Col := Last_Token.Last_Col;
      else
         New_Element.Last_Col := Last_Token.Last_Col - 1;
      end if;

      Append (Destination.Elements_List, New_Element);

      Free (Line_Cursor);
   end Add;

   -----------
   -- Clone --
   -----------

   function Clone (This : Ada_List) return Ada_List is
   begin
      return This;
   end Clone;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Element) is
   begin
      null;
   end Free;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (This : Ada_List; Num : Natural)
     return Elements_Lists.List_Node is

      Current_Node : Elements_Lists.List_Node := First (This.Elements_List);

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

      Current_Token, Old_Token : Token_Record;
      Current_Line             : Ptr_Extract_Line;
      Str_Token                : Dynamic_String;
      Current_Col              : Natural;

   begin
      Get_Unit (Current_Text, Position, Ada_Instruction (Destination));

      Current_Line := Get_First_Line (Destination);

      Current_Col := 1;

      Old_Token.Line := Get_First_Line (Destination).Cursor.Line;

      Get_Token
        (Current_Line.all, Current_Col, Current_Token, Str_Token);

      loop
         while Current_Col = 1 loop
            Current_Line := Next (Current_Line.all);
            Get_Token
              (Current_Line.all, Current_Col, Current_Token, Str_Token);
         end loop;
         Current_Token.Line := Get_Cursor (Current_Line.all).Line;

         if Is_Separator (Str_Token.all) then
            Add
              (Position.File_Name.all,
               Destination,
               Old_Token,
               Current_Token,
               Str_Token.all);

            exit when Str_Token.all /= ",";
         else
            Old_Token := Current_Token;
         end if;

         Get_Token
           (Current_Line.all, Current_Col, Current_Token, Str_Token);
      end loop;

   end Get_Unit;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      Destination : out Ada_List;
      First       : Natural;
      Last        : Natural := 0) is
   begin
      null;
   end Cut_Off_Elements;

   ----------------------
   -- Cut_Off_Elements --
   ----------------------

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      Destination : out Ada_List;
      First       : String;
      Last        : String := "") is
   begin
      null;
   end Cut_Off_Elements;

   -------------------------
   -- Get_Number_Elements --
   -------------------------

   function Get_Number_Elements (This : Ada_List) return Natural is
   begin
      return 0;
   end Get_Number_Elements;

   ---------------------
   -- Remove_Elements --
   ---------------------

   procedure Remove_Elements
     (This  : in out Ada_List; First : Natural; Last : Natural := 0) is

      Current_Element : Elements_Lists.List_Node;
      Garbage_Node    : Elements_Lists.List_Node;
      Last_Used       : Natural;

   begin
      Current_Element := Get_Element (This, First);

      if Last = 0 then
         Last_Used := First;
      else
         Last_Used := Last;
      end if;

      if First - Last_Used + 1 =  Length (This.Elements_List) then
         Remove_Instruction (This);
         return;
      end if;

      for J in First .. Last_Used loop
         if Is_Alone (Data (Current_Element)) then
            Delete_All_Lines (Data (Current_Element));
         else
            Erase (Data (Current_Element));
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
   begin
      return "";
   end Get_Element;

   ---------------------
   -- Get_Nth_Element --
   ---------------------

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural is

      Current_Num  : Natural := 1;
      Current_Node : Elements_Lists.List_Node := First (This.Elements_List);

   begin
      loop
         if Compare_Pkg (Data (Current_Node).Name.all, Name) then
            return Current_Num;
         end if;

         Current_Num := Current_Num + 1;
         Current_Node := Next (Current_Node);
         exit when Current_Node = Elements_Lists.Null_Node;
      end loop;

      return 0;
   end Get_Nth_Element;

   --------------
   -- Is_Alone --
   --------------

   function Is_Alone (This : Element) return Boolean is
   begin

      return Is_Blank (This.Lines (1).Content (1 .. This.First_Col - 1))
        and then Is_Blank
          (This.Lines (This.Lines'Last).Content
            (This.Last_Col  + 1 .. This.Lines (This.Lines'Last).Content'Last));

   end Is_Alone;

   ----------------------
   -- Delete_All_Lines --
   ----------------------

   procedure Delete_All_Lines (This : Element) is
   begin
      for J in This.Lines'Range loop
         This.Lines (J).Context := Line_Deleted;
      end loop;
   end Delete_All_Lines;

   -----------
   -- Erase --
   -----------

   procedure Erase (This : Element) is
   begin
      if This.Lines'Length = 1 then
         Set_String (This.Lines (1).all, "", This.First_Col, This.Last_Col);
         return;
      end if;

      Set_String (This.Lines (1).all, "", This.First_Col);

      for J in This.Lines'First + 1 .. This.Lines'Last - 1 loop
         This.Lines (J).Context := Line_Deleted;
      end loop;

      Set_String (This.Lines (This.Lines'Last).all, "", 1, This.Last_Col);
   end Erase;

end Codefix.Text_Manager.Ada_Extracts;

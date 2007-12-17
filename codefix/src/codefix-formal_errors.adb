-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2007, AdaCore               --
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

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with GNAT.Regpat;                       use GNAT.Regpat;

with Codefix.Ada_Tools;                 use Codefix.Ada_Tools;
with Codefix.Text_Manager.Ada_Commands; use Codefix.Text_Manager.Ada_Commands;
with Codefix.Text_Manager.Ada_Extracts; use Codefix.Text_Manager.Ada_Extracts;

with Language.Ada;                      use Language.Ada;
with Projects.Registry;                 use Projects.Registry;
with Traces;                            use Traces;
with VFS;                               use VFS;

package body Codefix.Formal_Errors is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This       : in out Error_Message;
      Registry   : Project_Registry_Access;
      Error_Line : String;
      Regexp     : GNAT.Regpat.Pattern_Matcher;
      File_Index, Line_Index, Col_Index, Msg_Index : Integer;
      Style_Index, Warning_Index : Integer)
   is
      Max_Index : Integer := File_Index;
   begin
      Max_Index := Integer'Max (Max_Index, Line_Index);
      Max_Index := Integer'Max (Max_Index, Col_Index);
      Max_Index := Integer'Max (Max_Index, Msg_Index);
      Max_Index := Integer'Max (Max_Index, Style_Index);
      Max_Index := Integer'Max (Max_Index, Warning_Index);

      declare
         Matches : Match_Array (0 .. Max_Index);
         Line    : Integer;
         Col     : Column_Index;
      begin
         Match (Regexp, Error_Line, Matches);

         if Matches (0) = No_Match then
            Free (This);
            This := Invalid_Error_Message;
            return;
         end if;

         Set_File
           (This, Create
              (Error_Line
                 (Matches (File_Index).First .. Matches (File_Index).Last),
               Registry.all));

         if Matches (Line_Index) /= No_Match then
            Line := Integer'Value
              (Error_Line
                 (Matches (Line_Index).First .. Matches (Line_Index).Last));
         else
            Line := 1;
         end if;

         if Matches (Col_Index) /= No_Match then
            Col := Column_Index'Value
              (Error_Line
                 (Matches (Col_Index).First .. Matches (Col_Index).Last));
         else
            Col := 1;
         end if;

         This.Is_Style   := Matches (Style_Index) /= No_Match;
         This.Is_Warning := Matches (Warning_Index) /= No_Match;

         Set_Location (This, Line => Line, Column => Col);

         if Matches (Msg_Index) /= No_Match then
            Assign (This.Message,
                    Error_Line
                      (Matches (Msg_Index).First .. Matches (Msg_Index).Last));
         else
            Assign (This.Message, "");
         end if;
      end;

   exception
      when E : Constraint_Error =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E)
               & " on message '" & Error_Line & "'");
         Free (This);
         This := Invalid_Error_Message;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : in out Error_Message;
      File    : VFS.Virtual_File;
      Line    : Positive;
      Col     : Column_Index;
      Message : String) is
   begin
      Assign (This.Message, Message);
      Set_File (This, File);
      Set_Location (This, Line, Col);
   end Initialize;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (This : Error_Message) return String is
   begin
      if This.Message = null then
         return "";
      else
         return This.Message.all;
      end if;
   end Get_Message;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Error_Message) is
   begin
      Free (File_Cursor (This));
      Free (This.Message);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Error_Message) return Error_Message is
      New_Message : Error_Message;
   begin
      New_Message := (Clone (File_Cursor (This)) with
                      Message    => new String'(This.Message.all),
                      Is_Style   => This.Is_Style,
                      Is_Warning => This.Is_Warning);
      return New_Message;
   end Clone;

   ------------
   -- Concat --
   ------------

   procedure Concat (Dest : in out Solution_List; Source : Solution_List) is
   begin
      Concat (Command_List.List (Dest), Command_List.List (Source));
   end Concat;

   ------------
   -- Length --
   ------------

   function Length (List : Solution_List) return Integer is
   begin
      return Length (Command_List.List (List));
   end Length;

   -----------
   -- First --
   -----------

   function First (List : Solution_List) return Solution_List_Iterator is
   begin
      return Solution_List_Iterator
        (Command_List.First (Command_List.List (List)));
   end First;

   ----------
   -- Next --
   ----------

   function Next (It : Solution_List_Iterator) return Solution_List_Iterator is
   begin
      return Solution_List_Iterator (Next (Command_List.List_Node (It)));
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Solution_List_Iterator) return Boolean is
   begin
      return Command_List.List_Node (It) = Command_List.Null_Node;
   end At_End;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (It : Solution_List_Iterator) return Text_Command'Class is
   begin
      return Data (It);
   end Get_Command;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (This     : Solution_List;
      Position : Positive) return Text_Command'Class
   is
      Current_Node : Command_List.List_Node;
   begin
      Current_Node := First (This);

      for J in 1 .. Position - 1 loop
         Current_Node := Next (Current_Node);
      end loop;

      return Data (Current_Node);
   end Get_Command;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (This : in out Solution_List) is
   begin
      Free (This, True);
   end Free_List;

   ---------------
   -- Should_Be --
   ---------------

   function Should_Be
     (Current_Text : Text_Navigator_Abstr'Class;
      Message      : File_Cursor'Class;
      Str_Expected : String;
      Str_Red      : String := "";
      Format_Red   : String_Mode := Text_Ascii;
      Caption      : String := "") return Solution_List
   is
      Result      : Solution_List;
      New_Command : Replace_Word_Cmd;
      Old_Word    : Word_Cursor;
   begin
      Set_File (Old_Word, Get_File (Message));
      Set_Location (Old_Word, Get_Line (Message), Get_Column (Message));

      if Str_Red /= "" then
         Set_Word (Old_Word, String_Match => Str_Red, Mode => Format_Red);

         if Caption = "" then
            if Format_Red = Text_Ascii then
               Set_Caption
                 (New_Command,
                  "Replace """ & Str_Red & """ by """ & Str_Expected & """");
            else
               Set_Caption
                 (New_Command,
                  "Replace misspelled word by """ & Str_Expected & """");
            end if;
         else
            Set_Caption (New_Command, Caption);
         end if;

      else
         Set_Word (Old_Word, "(^[\w]+)", Regular_Expression);

         if Caption = "" then
            Set_Caption
              (New_Command,
               "Replace misspelled word by """ & Str_Expected & """");
         else
            Set_Caption (New_Command, Caption);
         end if;
      end if;

      Initialize (New_Command, Current_Text, Old_Word, Str_Expected);

      Append (Result, New_Command);

      Free (Old_Word);

      return Result;
   end Should_Be;

   -----------------
   -- Wrong_Order --
   -----------------

   function Wrong_Order
     (Current_Text  : Text_Navigator_Abstr'Class;
      Message       : Error_Message;
      First_String  : String;
      Second_String : String) return Solution_List
   is
      New_Command   : Invert_Words_Cmd;
      Word1, Word2  : Word_Cursor;
      Matches       : Match_Array (1 .. 1);
      Matcher       : constant Pattern_Matcher :=
        Compile ("(" & Second_String & ") ", Case_Insensitive);
      Second_Cursor : File_Cursor := File_Cursor (Message);
      Result        : Solution_List;
      Line          : Integer := Get_Line (Second_Cursor);

   begin
      --  ??? This code is incomplete and does not work properly for
      --  e.g. "test.ads:2:18: "abstract" must come before "new", not after"

      loop
         Match (Matcher, Get_Line (Current_Text, Second_Cursor), Matches);
         exit when Matches (1) /= No_Match;
         Line := Line - 1;

         if Line = 0 then
            return Result;
         end if;

         Set_Location (Second_Cursor, Line, 1);
      end loop;

      Set_Location (Second_Cursor, Line, Column_Index (Matches (1).First));

      Set_File     (Word1, Get_File (Message));
      Set_Location (Word1, Get_Line (Message), Get_Column (Message));
      Set_Word     (Word1, First_String, Text_Ascii);

      Set_File     (Word2, Get_File (Message));
      Set_Location (Word2, Get_Line (Message), Get_Column (Message));
      Set_Word     (Word2, Second_String, Text_Ascii);

      Initialize (New_Command, Current_Text, Word1, Word2);

      Set_Caption
        (New_Command,
         "Invert """ & First_String & """ and """ & Second_String & """");

      Append (Result, New_Command);

      Free (Word1);
      Free (Word2);

      return Result;
   end Wrong_Order;

   --------------
   -- Expected --
   --------------

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      String_Expected : String;
      Add_Spaces      : Boolean := True;
      Position        : Relative_Position := Specified) return Solution_List
   is
      New_Command : Insert_Word_Cmd;
      Word        : Word_Cursor;
      Result      : Solution_List;

   begin
      Set_File (Word, Get_File (Message));
      Set_Location (Word, Get_Line (Message), Get_Column (Message));
      Set_Word (Word, String_Expected, Text_Ascii);

      Initialize (New_Command, Current_Text, Word,
                  File_Cursor (Word), Add_Spaces, Position);
      Set_Caption
        (New_Command,
         "Add expected string """ & String_Expected & """");
      Append (Result, New_Command);
      Free (Word);

      return Result;
   end Expected;

   ----------------
   -- Unexpected --
   ----------------

   function Unexpected
     (Current_Text      : Text_Navigator_Abstr'Class;
      Message           : File_Cursor'Class;
      String_Unexpected : String;
      Mode              : String_Mode := Text_Ascii) return Solution_List
   is
      New_Command : Remove_Word_Cmd;
      Word        : Word_Cursor;
      Result      : Solution_List;
   begin
      Set_File     (Word, Get_File (Message));
      Set_Location (Word, Get_Line (Message), Get_Column (Message));
      Set_Word     (Word, String_Unexpected, Mode);

      Initialize (New_Command, Current_Text, Word);
      Set_Caption
        (New_Command,
         "Remove unexpected word """ & String_Unexpected & """");
      Append (Result, New_Command);
      Free (Word);

      return Result;
   end Unexpected;

   ------------------
   -- Wrong_Column --
   ------------------

   function Wrong_Column
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      Column_Expected : Column_Index := 0) return Solution_List
   is
      New_Command : Replace_Word_Cmd;
      Result      : Solution_List;
      Word        : Word_Cursor;

      Indent_Size : Integer := -1;
      Char_Ind : Char_Index;
   begin
      if Column_Expected = 0 then
         --  Here, we force the creation of at least one space character, which
         --  will do a modification on the line and activate the requested
         --  auto-indentation.

         Char_Ind := To_Char_Index
           (Get_Column (Message), Get_Line (Current_Text, Word));

         Indent_Size := Integer (Char_Ind) + 1;
      else
         Char_Ind := To_Char_Index
           (Column_Expected, Get_Line (Current_Text, Word));
         Indent_Size := Natural (Char_Ind) - 1;
      end if;

      Set_File (Word, Get_File (Message));
      Set_Location (Word, Get_Line (Message), 1);
      Set_Word (Word, "(^[\s]*)", Regular_Expression);

      declare
         White_String : constant String (1 .. Indent_Size) := (others => ' ');
      begin
         Initialize
           (New_Command,
            Current_Text,
            Word,
            White_String,
            Column_Expected = 0);
      end;

      if Column_Expected = 0 then
         Set_Caption (New_Command, "Indent line");
      else
         Set_Caption
           (New_Command,
            "Move begin of instruction to column " &
            Column_Index'Image (Column_Expected));
      end if;

      Append (Result, New_Command);
      Free (Word);

      return Result;
   end Wrong_Column;

   ---------------------
   -- Clauses_Missing --
   ---------------------

   function Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String;
      Add_With       : Boolean;
      Add_Use        : Boolean) return Solution_List
   is
      New_Command : Add_Line_Cmd;
      Result      : Solution_List;

      With_Text : constant String := "with " & Missing_Clause & ";";
      Use_Text  : constant String := "use " & Missing_Clause & ";";
      Full_Text : constant String := With_Text & " " & Use_Text;

      Text_Start, Text_Stop : Integer;
   begin
      if Add_With and Add_Use then
         Text_Start := Full_Text'First;
         Text_Stop := Full_Text'Last;
      elsif Add_Use then
         Text_Start := Full_Text'First + With_Text'Length + 1;
         Text_Stop := Full_Text'Last;
      else
         Text_Start := Full_Text'First;
         Text_Stop := Full_Text'Last - Use_Text'Length - 1;
      end if;

      Initialize
        (New_Command,
         Current_Text,
         Get_Next_With_Position (Current_Text, Get_File (Cursor)),
         Full_Text (Text_Start .. Text_Stop));

      Set_Caption
        (New_Command,
         "Add missing clauses for package """ & Missing_Clause & ".");
      Append (Result, New_Command);

      return Result;
   end Clause_Missing;

   ----------------
   -- Bad_Casing --
   ----------------

   function Bad_Casing
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed) return Solution_List
   is
      Result      : Solution_List;
      New_Command : Recase_Word_Cmd;
   begin
      Initialize (New_Command, Current_Text, Cursor, Correct_Word, Word_Case);
      Set_Caption
        (New_Command,
         "Recase bad-cased word");
      Append (Result, New_Command);

      return Result;
   end Bad_Casing;

   --------------------
   -- Not_Referenced --
   --------------------

   function Not_Referenced
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Language_Category;
      Name         : String;
      Operations   : Useless_Entity_Operations) return Solution_List
   is
      function Add_Pragma return Add_Pragma_Cmd;
      --  Add a pragma after the declaration or, if there is no declaration,
      --  after the body.

      function Add_Literal_Pragma return Add_Pragma_Cmd;
      --  Add a pragma after the declaration of the current type.

      function Add_Parameter_Pragma return Add_Pragma_Cmd;
      --  Add a pragma after the 'is' of the subprogram

      ----------------
      -- Add_Pragma --
      ----------------

      function Add_Pragma return Add_Pragma_Cmd is
         New_Command  : Add_Pragma_Cmd;
         New_Position : File_Cursor;
         Declaration  : Construct_Tree_Iterator;
         Char_Ind     : Char_Index;
      begin
         Declaration := Get_Iterator_At (Current_Text, Cursor);

         Char_Ind := Char_Index (Get_Construct (Declaration).Sloc_End.Column);

         --  ??? This test is only here because the parser returns sometimes
         --  a Sloc_End.Col equal to 0.

         if Char_Ind = 0 then
            Char_Ind := 1;
         end if;

         Set_File (New_Position, Get_File (Cursor));
         Set_Location
           (New_Position, Get_Construct (Declaration).Sloc_End.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, New_Position);
         begin
            Set_Location
              (New_Position,
               Get_Construct (Declaration).Sloc_End.Line,
               To_Column_Index (Char_Ind, Line));
         end;

         Initialize
           (New_Command, Current_Text, New_Position, "Unreferenced", Name);
         Free (New_Position);
         return New_Command;
      end Add_Pragma;

      ------------------------
      -- Add_Literal_Pragma --
      ------------------------

      function Add_Literal_Pragma return Add_Pragma_Cmd is
         New_Command  : Add_Pragma_Cmd;
         New_Position : File_Cursor;
         Declaration : Construct_Tree_Iterator;
         Char_Ind    : Char_Index;
      begin
         Declaration := Get_Iterator_At
           (Current_Text, Cursor, Before, (1 => Cat_Type));
         Char_Ind := Char_Index (Get_Construct (Declaration).Sloc_End.Column);

         if Char_Ind = 0 then
            Char_Ind := 1;
         end if;

         Set_File (New_Position, Get_File (Cursor));
         Set_Location
           (New_Position, Get_Construct (Declaration).Sloc_End.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, New_Position);
         begin
            Set_Location
              (New_Position,
               Get_Construct (Declaration).Sloc_End.Line,
               To_Column_Index (Char_Ind, Line));
         end;

         Initialize
           (New_Command, Current_Text, New_Position, "Unreferenced", Name);
         Free (New_Position);
         return New_Command;
      end Add_Literal_Pragma;

      --------------------------
      -- Add_Parameter_Pragma --
      --------------------------

      function Add_Parameter_Pragma return Add_Pragma_Cmd is
         New_Command           : Add_Pragma_Cmd;
         New_Position, Garbage : File_Cursor;
         Declaration : Construct_Tree_Iterator;
      begin
         Declaration := Get_Iterator_At
           (Current_Text, Cursor, Before, (Cat_Procedure, Cat_Function));
         Set_File (New_Position, Get_File (Cursor));
         Set_Location
           (New_Position, Get_Construct (Declaration).Sloc_Entity.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, New_Position);
         begin
            Set_Location
              (New_Position, Get_Construct (Declaration).Sloc_Entity.Line,
               To_Column_Index
                 (Char_Index
                    (Get_Construct (Declaration).Sloc_Entity.Column), Line));
         end;

         Garbage := New_Position;
         New_Position := File_Cursor
           (Search_Token
              (Current_Text, New_Position, Close_Paren_Tok));
         Free (Garbage);

         Garbage := New_Position;
         New_Position := File_Cursor
           (Search_Token
              (Current_Text, New_Position, Is_Tok));
         Free (Garbage);

         Initialize
           (New_Command, Current_Text, New_Position, "Unreferenced", Name);

         Free (New_Position);

         return New_Command;
      end Add_Parameter_Pragma;

      --  begin of Not_Referenced

      Result : Solution_List;
      Actual_Category : Language_Category;

   begin
      if Category = Cat_Unknown then
         declare
            It : constant Construct_Tree_Iterator :=
              Get_Iterator_At (Current_Text, Cursor);
         begin
            Actual_Category := Get_Construct (It).Category;
         end;
      else
         Actual_Category := Category;
      end if;

      case Actual_Category is
         when Cat_Variable | Cat_Local_Variable =>
            declare
               Delete_Command  : Remove_Elements_Cmd;
               Pragma_Command  : Add_Pragma_Cmd;
               Comment_Command : Remove_Elements_Cmd;
               Var_Cursor      : Word_Cursor;
            begin
               Set_File (Var_Cursor, Get_File (Cursor));
               Set_Location
                 (Var_Cursor, Get_Line (Cursor), Get_Column (Cursor));
               Set_Word (Var_Cursor, Name, Text_Ascii);

               if Is_Set (Operations, Remove_Entity) then
                  Set_Remove_Mode (Delete_Command, Erase);
                  Add_To_Remove (Delete_Command, Current_Text, Var_Cursor);
                  Set_Caption (Delete_Command, "Delete """ & Name & """");
                  Append (Result, Delete_Command);
               end if;

               if Is_Set (Operations, Comment_Entity) then
                  Set_Remove_Mode (Comment_Command, Comment);
                  Add_To_Remove (Comment_Command, Current_Text, Var_Cursor);
                  Set_Caption
                    (Comment_Command,
                     "Comment """ & Name & """");
                  Append (Result, Comment_Command);
               end if;

               if Is_Set (Operations, Add_Pragma_Unreferenced) then
                  Pragma_Command := Add_Pragma;
                  Set_Caption (Pragma_Command, "Add pragma for " & Name);
                  Append (Result, Pragma_Command);
               end if;

            end;

         when Cat_Function | Cat_Procedure | Cat_Entry =>
            declare
               Delete_Command  : Remove_Entity_Cmd;
               Comment_Command : Remove_Entity_Cmd;
               Pragma_Command  : Add_Pragma_Cmd;
            begin
               if Is_Set (Operations, Remove_Entity) then
                  Initialize (Delete_Command, Current_Text, Cursor, Erase);
                  Set_Caption
                    (Delete_Command,
                     "Delete subprogram """ & Name & """");
                  Append (Result, Delete_Command);
               end if;

               if Is_Set (Operations, Comment_Entity) then
                  Initialize (Comment_Command, Current_Text, Cursor, Comment);
                  Set_Caption
                    (Comment_Command,
                     "Comment subprogram """ & Name & """");
                  Append (Result, Comment_Command);
               end if;

               Pragma_Command := Add_Pragma;
               Set_Caption
                 (Pragma_Command,
                  "Add pragma Unreferenced to subprogram """ & Name & """");
               Append (Result, Pragma_Command);
            end;

         when Cat_Type =>
            declare
               Delete_Command  : Remove_Entity_Cmd;
               Comment_Command : Remove_Entity_Cmd;
               Pragma_Command  : Add_Pragma_Cmd;
            begin
               if Is_Set (Operations, Remove_Entity) then
                  Initialize (Delete_Command, Current_Text, Cursor, Erase);
                  Set_Caption
                    (Delete_Command,
                     "Delete type """ & Name & """");
                  Append (Result, Delete_Command);
               end if;

               if Is_Set (Operations, Comment_Entity) then
                  Initialize (Comment_Command, Current_Text, Cursor, Comment);
                  Set_Caption
                    (Comment_Command,
                     "Comment type """ & Name & """");
                  Append (Result, Comment_Command);
               end if;

               Pragma_Command := Add_Pragma;
               Set_Caption
                 (Pragma_Command,
                  "Add pragma Unreferenced to type """ & Name & """");
               Append (Result, Pragma_Command);
            end;

         when Cat_Literal =>
            declare
               Pragma_Command : Add_Pragma_Cmd;
            begin
               Pragma_Command := Add_Literal_Pragma;
               Set_Caption
                 (Pragma_Command,
                  "Add pragma Unreferenced to literal """ & Name & """");
               Append (Result, Pragma_Command);
            end;

         when Cat_Parameter =>
            declare
               New_Command : Add_Pragma_Cmd;
            begin
               New_Command := Add_Parameter_Pragma;
               Set_Caption
                 (New_Command,
                  "Add pragma Unreferenced to parameter """ & Name & """");
               Append (Result, New_Command);
            end;

         when Cat_With =>
            declare
               New_Command : Remove_Pkg_Clauses_Cmd;
               With_Cursor : Word_Cursor;
            begin
               Set_File (With_Cursor, Get_File (Cursor));
               Set_Location
                 (With_Cursor, Get_Line (Cursor), Get_Column (Cursor));
               Set_Word (With_Cursor, Name, Text_Ascii);

               if Is_Set (Operations, Remove_Entity) then
                  Initialize (New_Command, Current_Text, With_Cursor, Before);
                  Set_Caption
                    (New_Command,
                     "Remove all clauses for package " & Name);
                  Append (Result, New_Command);
               end if;

               if Is_Set (Operations, Comment_Entity) then
                  null;

                  --  ??? Take this into account
               end if;

               Free (With_Cursor);
            end;

         when Cat_Package =>
            declare
               Remove_Command  : Remove_Entity_Cmd;
               Comment_Command : Remove_Entity_Cmd;
               Pragma_Command  : Add_Pragma_Cmd;
            begin
               if Is_Set (Operations, Remove_Entity) then
                  Initialize (Remove_Command, Current_Text, Cursor, Erase);
                  Set_Caption
                    (Remove_Command,
                     "Delete package """ & Name & """");
                  Append (Result, Remove_Command);
               end if;

               if Is_Set (Operations, Comment_Entity) then
                  Initialize (Comment_Command, Current_Text, Cursor, Comment);
                  Set_Caption
                    (Comment_Command,
                     "Comment package """ & Name & """");
                  Append (Result, Comment_Command);
               end if;

               Pragma_Command := Add_Pragma;
               Set_Caption
                 (Pragma_Command,
                  "Add pragma Unreferenced to package """ & Name & """");
               Append (Result, Pragma_Command);
            end;

         when others =>
            Raise_Exception
              (Codefix_Panic'Identity,
               "Wrong category given : " &
               Language_Category'Image (Actual_Category));
      end case;

      return Result;
   end Not_Referenced;

   -----------------------
   -- First_Line_Pragma --
   -----------------------

   function First_Line_Pragma
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List
   is
      Begin_Cursor  : File_Cursor;
      New_Command   : Move_Word_Cmd;
      Result        : Solution_List;
      Pragma_Cursor : Word_Cursor;
   begin
      Set_File (Pragma_Cursor, Get_File (Cursor));
      Set_Location (Pragma_Cursor, Get_Line (Cursor), Get_Column (Cursor));
      Set_Word
        (Pragma_Cursor,
         "(pragma\s+[\w\d_]+\s*(\([^\)]*\))?\s*;)", Regular_Expression);

      Set_File (Begin_Cursor, Get_File (Cursor));
      Set_Location (Begin_Cursor, 1, 1);

      Initialize
        (New_Command, Current_Text, Pragma_Cursor, Begin_Cursor, True);
      Set_Caption
        (New_Command, "Move the pragma to the beginning of the file");
      Append (Result, New_Command);
      Free (Pragma_Cursor);

      return Result;
   end First_Line_Pragma;

   ------------------
   -- Not_Modified --
   ------------------

   function Not_Modified
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : String) return Solution_List
   is
      New_Command : Make_Constant_Cmd;
      Result      : Solution_List;
   begin
      Initialize (New_Command, Current_Text, Cursor, Name);
      Set_Caption
        (New_Command,
         "Add ""constant"" to the declaration of """ & Name & """");
      Append (Result, New_Command);

      return Result;
   end Not_Modified;

   -----------------------
   -- Resolve_Ambiguity --
   -----------------------

   function Resolve_Ambiguity
     (Current_Text     : Text_Navigator_Abstr'Class;
      Error_Cursor     : File_Cursor'Class;
      Solution_Cursors : Cursor_Lists.List;
      Name             : String) return Solution_List
   is
      Str_Array   : array (1 .. Length (Solution_Cursors)) of String_Access;
      Cursor_Node : Cursor_Lists.List_Node;
      Index_Str   : Positive := 1;
      Word        : Word_Cursor;
      Result      : Solution_List;
   begin
      Cursor_Node := First (Solution_Cursors);

      while Cursor_Node /= Cursor_Lists.Null_Node loop
         declare
            New_Command : Insert_Word_Cmd;
         begin
            Assign
              (Str_Array (Index_Str),
               Get_Full_Prefix
                 (Current_Text, Data (Cursor_Node)));

            for J in 1 ..  Index_Str - 1 loop
               if Str_Array (J).all = Str_Array (Index_Str).all then

                  for J in Str_Array'Range loop
                     Free (Str_Array (J));
                  end loop;

                  Codefix.Formal_Errors.Free_List (Result);

                  return Solution_List (Command_List.Null_List);
               end if;
            end loop;

            Set_File (Word, Get_File (Error_Cursor));
            Set_Location
              (Word, Get_Line (Error_Cursor), Get_Column (Error_Cursor));
            Set_Word (Word, Str_Array (Index_Str).all & ".", Text_Ascii);

            Initialize
              (New_Command, Current_Text, Word, File_Cursor (Word), False);
            Set_Caption
              (New_Command,
               "Prefix """ & Name & """ by """ &
                 Str_Array (Index_Str).all & """");
            Append (Result, New_Command);
            Free (Word);

            Index_Str := Index_Str + 1;
            Cursor_Node := Next (Cursor_Node);
         end;
      end loop;

      for J in Str_Array'Range loop
         Free (Str_Array (J));
      end loop;

      return Result;
   end Resolve_Ambiguity;

   -----------------------
   -- Remove_Conversion --
   -----------------------

   function Remove_Conversion
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Object_Name  : String) return Solution_List
   is
      New_Command : Remove_Parenthesis_Cmd;
      Result      : Solution_List;
   begin
      Initialize (New_Command, Current_Text, Cursor);
      Set_Caption
        (New_Command, "Remove useless conversion of """ & Object_Name & """");
      Append (Result, New_Command);

      return Result;
   end Remove_Conversion;

   -----------------------
   -- Move_With_To_Body --
   -----------------------

   function Move_With_To_Body
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List
   is
      Result      : Solution_List;
      New_Command : Remove_Pkg_Clauses_Cmd;
      With_Cursor : Word_Cursor;
      Body_Name   : Virtual_File;

   begin
      Body_Name := Get_Body_Or_Spec (Current_Text, Get_File (Cursor));
      Set_File (With_Cursor, Get_File (Cursor));
      Set_Location (With_Cursor, Get_Line (Cursor), Get_Column (Cursor));
      Set_Word (With_Cursor, "", Text_Ascii);

      Initialize
        (New_Command,
         Current_Text,
         With_Cursor,
         Before,
         Body_Name);
      Set_Caption
        (New_Command,
         "Move with clause from """ & Base_Name (Get_File (Cursor)) &
         """ to """ & Base_Name (Body_Name) & """");
      Append (Result, New_Command);
      Free (With_Cursor);

      return Result;
   end Move_With_To_Body;

   ---------------------
   -- Make_Conformant --
   ---------------------

   function Make_Conformant
     (Current_Text : Text_Navigator_Abstr'Class;
      Body_Cursor  : File_Cursor'Class;
      Spec_Cursor  : File_Cursor'Class) return Solution_List
   is
      Result               : Solution_List;
      Command1, Command2   : Paste_Profile_Cmd;
      Body_Info, Spec_Info : Construct_Tree_Iterator;
   begin
      Body_Info := Get_Iterator_At
        (Current_Text,
         Body_Cursor,
         Enclosing,
         (Cat_Procedure, Cat_Function, Cat_Entry));
      Spec_Info := Get_Iterator_At
        (Current_Text,
         Spec_Cursor,
         After,
         (Cat_Procedure, Cat_Function, Cat_Entry));

      Initialize
        (Command1,
         Current_Text,
         Body_Info,
         Spec_Info,
         Get_File (Body_Cursor),
         Get_File (Spec_Cursor));

      Initialize
        (Command2,
         Current_Text,
         Spec_Info,
         Body_Info,
         Get_File (Spec_Cursor),
         Get_File (Body_Cursor));

      Set_Caption (Command1, "Modify the implementation profile");
      Set_Caption (Command2, "Modify the spec profile");

      Append (Result, Command1);
      Append (Result, Command2);

      return Result;
   end Make_Conformant;

   ------------------------------
   -- Remove_Dependency_Clause --
   ------------------------------

   function Remove_Dependency_Clause
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Category     : Dependency_Category;
      Position     : Relative_Position) return Solution_List
   is
      Result      : Solution_List;
      New_Command : Remove_Pkg_Clauses_Cmd;
      Word        : Word_Cursor;
   begin
      Set_File (Word, Get_File (Cursor));
      Set_Location (Word, Get_Line (Cursor), Get_Column (Cursor));
      Set_Word (Word, "", Text_Ascii);

      Initialize
        (New_Command,
         Current_Text,
         Word,
         Position,
         Category => Category);

      if Category = Cat_With then
         Set_Caption (New_Command, "Remove with clause");
      elsif Category = Cat_Use then
         Set_Caption (New_Command, "Remove use clause");
      end if;

      Append (Result, New_Command);

      Free (Word);

      return Result;
   end Remove_Dependency_Clause;

   -----------------------------------
   -- Resolve_Unvisible_Declaration --
   -----------------------------------

   function Resolve_Unvisible_Declaration
     (Current_Text  : Text_Navigator_Abstr'Class;
      Object_Cursor : File_Cursor'Class;
      Pkg_Cursor    : File_Cursor'Class;
      Seek_With     : Boolean) return Solution_List
   is
      Result          : Solution_List;
      Use_Solution    : Get_Visible_Declaration_Cmd;
      Prefix_Solution : Get_Visible_Declaration_Cmd;
   begin
      Add_Use
        (Use_Solution,
         Current_Text,
         Pkg_Cursor,
         Get_File (Object_Cursor),
         Seek_With);
      Set_Caption
        (Use_Solution, "Update with and use's clauses to show the object");
      Prefix_Object
        (Prefix_Solution,
         Current_Text,
         Pkg_Cursor,
         Object_Cursor,
         Seek_With);
      Set_Caption
        (Prefix_Solution, "Update with clauses and prefix the object");
      Append (Result, Use_Solution);
      Append (Result, Prefix_Solution);

      return Result;
   end Resolve_Unvisible_Declaration;

   ---------------------
   -- Replace_Code_By --
   ---------------------

   function Replace_Code_By
     (Start_Cursor : File_Cursor'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      Replaced_Exp : String;
      New_String   : String) return Solution_List
   is
      Result   : Solution_List;
      Solution : Replace_Code_By_Cmd;
   begin
      Initialize
        (Solution, Current_Text, Start_Cursor, Replaced_Exp, New_String);
      Append (Result, Solution);

      return Result;
   end Replace_Code_By;

   -----------------------------
   -- Remove_Extra_Underlines --
   -----------------------------

   function Remove_Extra_Underlines
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List
   is
      Line         : constant String := Get_Line (Current_Text, Cursor, 1);
      New_Id       : String (1 .. Line'Length);
      Index        : Char_Index := To_Char_Index (Get_Column (Cursor), Line);
      New_Id_Index : Integer := 1;
      Start_Index  : Char_Index;
   begin
      while Index > Char_Index (Line'First)
        and then not Is_Separator (Line (Integer (Index)))
      loop
         Index := Index - 1;
      end loop;

      if Is_Blank (Line (Integer (Index))) then
         Index := Index + 1;
      end if;

      Start_Index := Index;

      while Index < Char_Index (Line'Last)
        and then not Is_Separator (Line (Integer (Index)))
      loop
         if Line (Integer (Index)) = '_' then
            New_Id (New_Id_Index) := Line (Integer (Index));
            Index := Index + 1;
            New_Id_Index := New_Id_Index + 1;

            while Index < Char_Index (Line'Last)
              and then Line (Integer (Index)) = '_'
            loop
               Index := Index + 1;
            end loop;
         else
            New_Id (New_Id_Index) := Line (Integer (Index));
            Index := Index + 1;
            New_Id_Index := New_Id_Index + 1;
         end if;
      end loop;

      declare
         Begin_Cursor : File_Cursor := File_Cursor (Clone (Cursor));
      begin
         Set_Location
           (Begin_Cursor,
            Get_Line (Begin_Cursor),
            To_Column_Index (Start_Index, Line));

         return Replace_Code_By
           (Begin_Cursor,
            Current_Text,
            "([\w]+)",
            New_Id (1 .. New_Id_Index - 1));
      end;
   end Remove_Extra_Underlines;

   --------------------------
   -- Change_To_Tick_Valid --
   --------------------------

   function Change_To_Tick_Valid
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List
   is
      Depth : Integer := 0;
      Begin_Cursor, End_Cursor : File_Cursor;

      Is_First : Boolean := True;

      No_Fix : Boolean := False;

      function Entity_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean;

      ---------------------
      -- Entity_Callback --
      ---------------------

      function Entity_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Name : constant String := Line (Sloc_Start.Column .. Sloc_End.Column);
      begin
         if Is_First and then To_Lower (Name) = "not" then
            No_Fix := True;
            return True;
         end if;

         Is_First := False;

         if Name = "(" then
            Depth := Depth + 1;
         elsif Name = ")" then
            if Depth = 0 then
               return True;
            end if;

            Depth := Depth - 1;
         elsif Depth = 0 then
            if (Entity = Keyword_Text and then To_Lower (Name) /= "in")
              or else
                (Entity = Operator_Text
                 and then Name /= "'"
                 and then Name /= ".")
            then
               return True;
            end if;
         end if;

         Set_Location
           (End_Cursor,
            Sloc_End.Line,
            To_Column_Index (Char_Index (Sloc_End.Column), Line));

         return False;
      end Entity_Callback;

      Command : Replace_Slice_Cmd;
      Result  : Solution_List;

   begin
      Begin_Cursor := File_Cursor (Cursor);
      End_Cursor := File_Cursor (Cursor);

      loop
         declare
            Line       : constant String :=
              Get_Line (Current_Text, Begin_Cursor, 1);
            Real_Begin : Char_Index :=
              To_Char_Index (Get_Column (Begin_Cursor), Line) - 1;
         begin
            while Real_Begin > 1
              and then Is_Blank (Line (Natural (Real_Begin)))
            loop
               Real_Begin := Real_Begin - 1;
            end loop;

            if Is_Blank (Line (Natural (Real_Begin))) then
               if Get_Line (Begin_Cursor) = 1 then
                  Set_Location
                    (Begin_Cursor,
                     Get_Line (Begin_Cursor),
                     To_Column_Index (Real_Begin, Line));

                  exit;
               else
                  Set_Location (Begin_Cursor, Get_Line (Begin_Cursor) - 1, 1);
                  Set_Location
                    (Begin_Cursor,
                     Get_Line (Begin_Cursor),
                     Column_Index
                       (Get_Line (Current_Text, Begin_Cursor)'Last + 1));
               end if;
            else
               Set_Location
                 (Begin_Cursor,
                  Get_Line (Begin_Cursor),
                  To_Column_Index (Real_Begin, Line) + 1);

               exit;
            end if;
         end;
      end loop;

      Parse_Entities
        (Ada_Lang,
         Current_Text,
         Entity_Callback'Unrestricted_Access,
         Begin_Cursor);

      if not No_Fix then
         Initialize
           (Command, Current_Text, Begin_Cursor, End_Cursor, "'Valid");
         Set_Caption (Command, "Change 'in' expression by 'Valid");
         Append (Result, Command);
      end if;

      return Result;
   end Change_To_Tick_Valid;

   -------------------------
   -- Is_Style_Or_Warning --
   -------------------------

   function Is_Style_Or_Warning (Error : Error_Message) return Boolean is
   begin
      return Error.Is_Style or Error.Is_Warning;
   end Is_Style_Or_Warning;

end Codefix.Formal_Errors;

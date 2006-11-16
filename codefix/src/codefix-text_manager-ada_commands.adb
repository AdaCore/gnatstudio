-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
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

with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with GNAT.Regpat;                       use GNAT.Regpat;
with GNAT.Strings;

with Case_Handling;                     use Case_Handling;
with Codefix.Ada_Tools;                 use Codefix.Ada_Tools;
with String_Utils;                      use String_Utils;
with Language.Ada;                      use Language.Ada;
with VFS;                               use VFS;

package body Codefix.Text_Manager.Ada_Commands is

   ---------------------
   -- Recase_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed) is
   begin
      This.Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Cursor));
      Assign (This.Correct_Word, Correct_Word);
      This.Word_Case := Word_Case;
   end Initialize;

   procedure Execute
     (This         : Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class) is

      function To_Correct_Case (Str : String) return String;
      --  Return the string after having re-cased it (with Word_Case).

      ---------------------
      -- To_Correct_Case --
      ---------------------

      function To_Correct_Case (Str : String) return String is
         New_String : String (Str'Range);
      begin
         case This.Word_Case is
            when Mixed =>
               New_String := Str;
               Mixed_Case (New_String);

            when Upper =>
               for J in Str'Range loop
                  New_String (J) := To_Upper (Str (J));
               end loop;

            when Lower =>
               for J in Str'Range loop
                  New_String (J) := To_Lower (Str (J));
               end loop;
         end case;

         return New_String;
      end To_Correct_Case;

      Cursor_Line  : File_Cursor;
      Word_Matcher : constant Pattern_Matcher := Compile ("([\w]+)");
      Cursor       : File_Cursor;
      Matches      : Match_Array (0 .. 1);
      Size         : Integer;
      Line         : GNAT.Strings.String_Access;
      Word_Chosen  : GNAT.Strings.String_Access;

   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Cursor.all));
      Cursor_Line := Cursor;

      Cursor_Line.Col := 1;
      Get_Line (Current_Text, Cursor_Line, New_Extract);
      Assign (Line, Get_String (New_Extract));
      Match
        (Word_Matcher,
         Line (Natural (To_Char_Index (Cursor.Col, Line.all)) .. Line'Length),
         Matches);

      Size := Matches (1).Last - Matches (1).First + 1;

      if This.Correct_Word.all /= "" then
         Word_Chosen := Clone (This.Correct_Word);
      else
         Word_Chosen := new String'
           (To_Correct_Case (Line (Matches (1).First .. Matches (1).Last)));
      end if;

      Replace_Word
        (New_Extract,
         Cursor,
         Word_Chosen (Word_Chosen'Last - Size + 1 .. Word_Chosen'Last),
         Size);

      Free (Word_Chosen);
      Free (Line);
      Free (Cursor);
   end Execute;

   procedure Free (This : in out Recase_Word_Cmd) is
   begin
      Free (This.Cursor);
      Free (This.Correct_Word);
      Free (Text_Command (This));
   end Free;

   ----------------------------
   -- Remove_Instruction_Cmd --
   ----------------------------

   procedure Initialize
     (This              : in out Remove_Instruction_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Start_Instruction : File_Cursor'Class) is
   begin
      This.Begin_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Start_Instruction));
   end Initialize;

   procedure Execute
     (This         : Remove_Instruction_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Start_Instruction : constant File_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Begin_Mark.all));
      Instruction       : Ada_Instruction;
   begin
      Get_Unit (Current_Text, Start_Instruction, Instruction);
      Remove_Instruction (Instruction);
      Assign (New_Extract, Instruction);
      Free (Instruction);
   end Execute;

   procedure Free (This : in out Remove_Instruction_Cmd) is
   begin
      Free (This.Begin_Mark);
      Free (Text_Command (This));
   end Free;

   -------------------------
   -- Remove_Elements_Cmd --
   -------------------------

   procedure Set_Remove_Mode
     (This : in out Remove_Elements_Cmd; Mode : Remove_Code_Mode)
   is
   begin
      This.Mode := Mode;
   end Set_Remove_Mode;

   procedure Add_To_Remove
     (This         : in out Remove_Elements_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor)
   is
      New_Word : Word_Mark;
   begin
      Make_Word_Mark (Word, Current_Text, New_Word);
      Append (This.Remove_List, New_Word);
   end Add_To_Remove;

   procedure Execute
     (This         : Remove_Elements_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Remove_Extracts : Ada_Lists.List;
      Current_Cursor  : Word_Cursor;
      Current_Word    : Mark_List.List_Node;
      Current_Extract : Ada_Lists.List_Node;
      Already_Loaded  : Boolean;
      Merge_Success   : Boolean;

   begin
      Current_Word := First (This.Remove_List);

      while Current_Word /= Mark_List.Null_Node loop
         Make_Word_Cursor (Data (Current_Word), Current_Text, Current_Cursor);
         Current_Extract := First (Remove_Extracts);
         Already_Loaded := False;

         while Current_Extract /= Ada_Lists.Null_Node loop
            declare
               Extract_Temp : Ada_List;
               Test_Cursor  : constant File_Cursor :=
                                File_Cursor (Current_Cursor);
            begin
               if Test_Cursor >=
                 Get_Start (Data (Current_Extract))
                 and then Test_Cursor <=
                   Get_Stop (Data (Current_Extract))
               then
                  Extract_Temp := Clone (Data (Current_Extract));
                  Remove_Elements
                    (Extract_Temp, This.Mode, Current_Cursor.String_Match.all);
                  Set_Data (Current_Extract, Extract_Temp);
                  Already_Loaded := True;
                  exit;
               end if;

               Current_Extract := Next (Current_Extract);
            end;
         end loop;

         if not Already_Loaded then
            declare
               Extract_Temp : Ada_List;
            begin
               Get_Unit (Current_Text, Current_Cursor, Extract_Temp);
               Remove_Elements
                 (Extract_Temp, This.Mode, Current_Cursor.String_Match.all);
               Append (Remove_Extracts, Extract_Temp);
            end;
         end if;

         Already_Loaded := False;

         Free (Current_Cursor);
         Current_Word := Next (Current_Word);
      end loop;

      Current_Extract := First (Remove_Extracts);

      while Current_Extract /= Ada_Lists.Null_Node loop
         declare
            pragma Warnings (Off, New_Extract);
            Extract_Temp : Extract := Extract (New_Extract);
            --  New_Extract has not been initializated on the first
            --  iteration, so what is this supposed to do ???
            --  Disable clever GNAT warning for now, to be revisited.

         begin
            Merge_Extracts
              (New_Extract,
               Extract_Temp,
               Data (Current_Extract),
               Merge_Success,
               False);
            Free (Extract_Temp);
            --  Is it really safe to call Free here ???
         end;

         if not Merge_Success then
            raise Codefix_Panic;
         end if;

         Current_Extract := Next (Current_Extract);
      end loop;

      Free (Remove_Extracts);
   end Execute;

   procedure Free (This : in out Remove_Elements_Cmd) is
   begin
      Free (This.Remove_List);
      Free (Text_Command (This));
   end Free;

   ----------------------------
   -- Remove_Pkg_Clauses_Cmd --
   ----------------------------

   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Position     : Relative_Position := Specified;
      Destination  : VFS.Virtual_File := VFS.No_File;
      Category     : Dependency_Category := Cat_With)
   is
      use Codefix.Ada_Tools.Words_Lists;

      Pkg_Info     : Simple_Construct_Information;
      Word_Used    : Word_Cursor := Clone (Word);
      Clauses_List : Words_Lists.List;
      Clause_Node  : Words_Lists.List_Node;
      Last_With    : File_Cursor;
   begin
      if Word.String_Match /= null then
         Pkg_Info := Search_Unit
           (Current_Text, Get_File (Word), Cat_With, Word.String_Match.all);
      else
         declare
            It : constant Construct_Tree_Iterator := Get_Iterator_At
              (Current_Text, Word, Position, (1 => Category));
         begin
            Pkg_Info := Get_Construct (It);
         end;
      end if;

      if Pkg_Info.Category /= Cat_Unknown then
         Assign (Word_Used.String_Match, Pkg_Info.Name.all);
      end if;

      if Pkg_Info.Category = Cat_Unknown then
         Pkg_Info := Search_Unit
           (Current_Text, Get_File (Word),
            Cat_Package,
            Word.String_Match.all);

         Initialize
           (This.Instantiation_Pkg,
            Current_Text,
            Word_Used);
         This.Is_Instantiation := True;
      else
         Add_To_Remove
           (This.Clauses_Pkg,
            Current_Text,
            Word_Used);

         if Destination /= VFS.No_File then
            Append
              (This.Obj_List,
               new String'("with " & Pkg_Info.Name.all & ";"));
         end if;

         This.Is_Instantiation := False;
      end if;

      if Category /= Cat_Use then
         Clauses_List := Get_Use_Clauses
           (Word_Used.String_Match.all,
            Get_File (Word_Used),
            Current_Text,
            True);

         Clause_Node := First (Clauses_List);

         if Destination /= VFS.No_File then
            while Clause_Node /= Words_Lists.Null_Node loop
               Add_To_Remove
                 (This.Clauses_Pkg, Current_Text, Data (Clause_Node));
               Append
                 (This.Obj_List,
                  new String'
                    ("use " & Data (Clause_Node).String_Match.all & ";"));
               Clause_Node := Next (Clause_Node);
            end loop;
         else
            while Clause_Node /= Words_Lists.Null_Node loop
               Add_To_Remove
                 (This.Clauses_Pkg, Current_Text, Data (Clause_Node));
               Clause_Node := Next (Clause_Node);
            end loop;
         end if;

         Free (Clauses_List);
      end if;

      if Destination /= VFS.No_File then
         Last_With := File_Cursor
           (Get_Next_With_Position (Current_Text, Destination));

         This.Last_With := new Mark_Abstr'Class'
           (Get_New_Mark (Current_Text, Last_With));

         Free (Last_With);
      end if;

      Free (Word_Used);
   end Initialize;

   procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Instr_Extract, Clauses_Extract : Extract;
      Success                        : Boolean;
      Node                           : String_List.List_Node;
      Last_With                      : File_Cursor;
   begin
      if This.Is_Instantiation then
         Execute (This.Instantiation_Pkg, Current_Text, Instr_Extract);
         Execute (This.Clauses_Pkg, Current_Text, Clauses_Extract);
         Merge_Extracts
           (New_Extract,
            Instr_Extract,
            Clauses_Extract,
            Success,
            False);

         if not Success then
            raise Codefix_Panic;
         end if;

         Free (Instr_Extract);
         Free (Clauses_Extract);
      else
         Execute (This.Clauses_Pkg, Current_Text, New_Extract);
      end if;

      Delete_Empty_Lines (New_Extract);

      if This.Last_With /= null then

         Node := First (This.Obj_List);

         Last_With := File_Cursor
           (Get_Current_Cursor (Current_Text, This.Last_With.all));

         while Node /= String_List.Null_Node loop
            Add_Line (New_Extract, Last_With, Data (Node).all);
            Node := Next (Node);
         end loop;

         Free (Last_With);
      end if;
   end Execute;

   procedure Free (This : in out Remove_Pkg_Clauses_Cmd) is
   begin
      Free (This.Instantiation_Pkg);
      Free (This.Clauses_Pkg);
      Free (This.Last_With);
      Free (This.Obj_List);
      Free (Text_Command (This));
   end Free;

   -----------------------
   -- Remove_Entity_Cmd --
   -----------------------

   procedure Initialize
     (This         : in out Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Entity : File_Cursor'Class;
      Mode         : Remove_Code_Mode := Erase)
   is
      Spec_Begin, Spec_End : File_Cursor;
      Body_Begin, Body_End : File_Cursor;

   begin
      Get_Entity
        (Current_Text,
         Start_Entity,
         Spec_Begin, Spec_End,
         Body_Begin, Body_End);

      if Spec_Begin /= Null_File_Cursor then
         This.Spec_Begin := new Mark_Abstr'Class'
           (Get_New_Mark (Current_Text, Spec_Begin));
         This.Spec_End := new Mark_Abstr'Class'
           (Get_New_Mark (Current_Text, Spec_End));
      end if;

      This.Body_Begin := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Body_Begin));
      This.Body_End := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Body_End));

      This.Mode := Mode;

      Free (Spec_Begin);
      Free (Spec_End);
      Free (Body_Begin);
      Free (Body_End);
   end Initialize;

   procedure Execute
     (This         : Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Spec_Begin, Spec_End       : File_Cursor;
      Body_Begin, Body_End       : File_Cursor;
      Line_Cursor                : File_Cursor;
      Spec_Extract, Body_Extract : Extract;
      Success_Merge              : Boolean;

   begin
      Body_Begin := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Body_Begin.all));
      Body_End := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Body_End.all));

      Line_Cursor := Body_Begin;
      Line_Cursor.Col := 1;

      while Line_Cursor.Line <= Body_End.Line loop
         Get_Line (Current_Text, Line_Cursor, Body_Extract);
         Line_Cursor.Line := Line_Cursor.Line + 1;
      end loop;

      case This.Mode is
         when Erase =>
            Erase (Body_Extract, Body_Begin, Body_End);
         when Comment =>
            Comment (Body_Extract, Body_Begin, Body_End);
      end case;

      if This.Spec_Begin /= null then
         Spec_Begin := File_Cursor
           (Get_Current_Cursor (Current_Text, This.Spec_Begin.all));
         Spec_End := File_Cursor
           (Get_Current_Cursor (Current_Text, This.Spec_End.all));

         Line_Cursor := Spec_Begin;
         Line_Cursor.Col := 1;

         while Line_Cursor.Line <= Spec_End.Line loop
            Get_Line (Current_Text, Line_Cursor, Spec_Extract);
            Line_Cursor.Line := Line_Cursor.Line + 1;
         end loop;

         case This.Mode is
            when Erase =>
               Erase (Spec_Extract, Spec_Begin, Spec_End);
            when Comment =>
               Comment (Spec_Extract, Spec_Begin, Spec_End);
         end case;

         Merge_Extracts
           (New_Extract,
            Spec_Extract,
            Body_Extract,
            Success_Merge,
            False);

         if not Success_Merge then
            raise Codefix_Panic;
         end if;

         Free (Spec_Extract);
         Free (Body_Extract);
      else
         Extract (New_Extract) := Body_Extract;
      end if;

      Free (Spec_Begin);
      Free (Spec_End);
      Free (Body_Begin);
      Free (Body_End);
   end Execute;

   procedure Free (This : in out Remove_Entity_Cmd) is
   begin
      Free (This.Spec_Begin);
      Free (This.Spec_End);
      Free (This.Body_Begin);
      Free (This.Body_End);
      Free (Text_Command (This));
   end Free;

   --------------------
   -- Add_Pragma_Cmd --
   --------------------

   procedure Initialize
     (This           : in out Add_Pragma_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Position       : File_Cursor'Class;
      Name, Argument : String) is
   begin
      Assign (This.Name, Name);
      Assign (This.Argument, Argument);
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
   end Initialize;

   procedure Execute
     (This         : Add_Pragma_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Position      : File_Cursor;
      Pragma_Cursor : File_Cursor;
      Line_Cursor   : File_Cursor;
      Next_Str      : GNAT.Strings.String_Access;
   begin
      Position := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));

      Pragma_Cursor := Position;
      Pragma_Cursor.Line := Pragma_Cursor.Line + 1;
      Pragma_Cursor.Col := 1;

      Next_Word (Current_Text, Pragma_Cursor, Next_Str);

      if To_Lower (Next_Str.all) = "pragma" then
         Free (Next_Str);
         Next_Word (Current_Text, Pragma_Cursor, Next_Str);

         if To_Lower (Next_Str.all) = To_Lower (This.Name.all) then
            Pragma_Cursor := File_Cursor
              (Search_String
                 (Current_Text, Pragma_Cursor, ")", Std_Ada_Escape));
            Line_Cursor := Pragma_Cursor;
            Line_Cursor.Col := 1;
            Get_Line (Current_Text, Line_Cursor, New_Extract);
            Replace
              (New_Extract, Pragma_Cursor, 1, ", " & This.Argument.all & ")");
            Free (Line_Cursor);
         end if;

      else
         Add_Indented_Line
           (New_Extract,
            Position,
            "pragma " & This.Name.all & " (" & This.Argument.all & ");",
            Current_Text);
      end if;

      Free (Next_Str);
      Free (Position);
   end Execute;

   procedure Free (This : in out Add_Pragma_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
      Free (This.Argument);
      Free (Text_Command (This));
   end Free;

   -----------------------
   -- Make_Constant_Cmd --
   -----------------------

   procedure Initialize
     (This         : in out Make_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Name         : String) is
   begin
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
      Assign (This.Name, Name);
   end Initialize;

   procedure Execute
     (This         : Make_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Cursor        : File_Cursor;
      Work_Extract  : Ada_List;
      New_Instr     : GNAT.Strings.String_Access;
      Col_Decl      : Natural;

   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));
      Get_Unit (Current_Text, Cursor, Work_Extract);

      if Get_Number_Of_Declarations (Work_Extract) = 1 then
         Replace_Word
           (Work_Extract,
            Search_String (Work_Extract, ":", Std_Ada_Escape),
            ": constant",
            ":");
      else
         Cut_Off_Elements
           (Work_Extract, New_Instr, Current_Text, This.Name.all);

         Col_Decl := New_Instr'First;
         Skip_To_Char (New_Instr.all, Col_Decl, ':');

         Assign
           (New_Instr,
            New_Instr (New_Instr'First .. Col_Decl) & " constant" &
              New_Instr (Col_Decl + 1 .. New_Instr'Last));

         Add_Indented_Line
           (Work_Extract,
            Get_Stop (Work_Extract),
            New_Instr.all,
            Current_Text);

         Free (New_Instr);
      end if;

      Assign (New_Extract, Work_Extract);

      Free (Work_Extract);
      Free (Cursor);
   end Execute;

   procedure Free (This : in out Make_Constant_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
      Free (Text_Command (This));
   end Free;

   ----------------------------
   -- Remove_Parenthesis_Cmd --
   ----------------------------

   procedure Initialize
     (This         : in out Remove_Parenthesis_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) is
   begin
      This.Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Cursor));
   end Initialize;

   procedure Execute
     (This         : Remove_Parenthesis_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      procedure Right_Paren (Current_Index : in out Char_Index);
      --  Put Current_Index extactly on the right paren correponding the last
      --  left paren.

      Work_Extract  : Ada_Instruction;
      Current_Line  : Ptr_Extract_Line;
      Cursor        : File_Cursor;
      Line_Cursor   : File_Cursor;
      Current_Index : Char_Index := 1;

      procedure Right_Paren (Current_Index : in out Char_Index) is
      begin
         loop
            if Natural (Current_Index)
              > Get_String (Current_Line.all)'Last
            then
               Current_Index := 1;
               Current_Line := Next (Current_Line.all);
            end if;

            case Get_String (Current_Line.all) (Natural (Current_Index)) is
               when '(' =>
                  Current_Index := Current_Index + 1;
                  Right_Paren (Current_Index);
               when ')' =>
                  return;
               when others =>
                  Current_Index := Current_Index + 1;
            end case;
         end loop;
      end Right_Paren;

   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Cursor.all));

      Line_Cursor := Cursor;
      Line_Cursor.Col := 1;
      Get_Unit (Current_Text, Cursor, Work_Extract);
      Current_Line := Get_Line (Work_Extract, Line_Cursor);

      Erase
        (Work_Extract,
         Cursor,
         Search_String (Work_Extract, "(", Std_Ada_Escape, Cursor));

      Current_Index := To_Char_Index
        (Cursor.Col, Get_String (Current_Line.all));

      Right_Paren (Current_Index);

      Cursor := Current_Line.Cursor;
      Cursor.Col := To_Column_Index
        (Current_Index, Get_String (Current_Line.all));

      Erase
        (Work_Extract,
         Cursor,
         Cursor);

      Assign (New_Extract, Work_Extract);

      Free (Work_Extract);
      Free (Cursor);
   end Execute;

   procedure Free (This : in out Remove_Parenthesis_Cmd) is
   begin
      Free (This.Cursor);
      Free (Text_Command (This));
   end Free;

   -----------------------
   -- Paste_Profile_Cmd --
   -----------------------

   procedure Initialize
     (This             : in out Paste_Profile_Cmd;
      Current_Text     : Text_Navigator_Abstr'Class;
      Destination_It   : Construct_Tree_Iterator;
      Source_It        : Construct_Tree_Iterator;
      Destination_File : VFS.Virtual_File;
      Source_File      : VFS.Virtual_File)
   is
      procedure Initialize_Profile
        (Position_It              : Construct_Tree_Iterator;
         Position_File            : VFS.Virtual_File;
         Begin_Cursor, End_Cursor : out File_Cursor);
      --  Set Begin_Cursor and End_Cursor at the begining and at the end of the
      --  profile's function starting at position.

      procedure Initialize_Profile
        (Position_It              : Construct_Tree_Iterator;
         Position_File            : VFS.Virtual_File;
         Begin_Cursor, End_Cursor : out File_Cursor)
      is
         Paren_Depth : Integer := 0;

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

            procedure Begin_Of_Profile;

            procedure End_Of_Profile;

            ----------------------
            -- Begin_Of_Profile --
            ----------------------

            procedure Begin_Of_Profile is
            begin
               Begin_Cursor.File := Position_File;
               Begin_Cursor.Line := Sloc_Start.Line;
               Begin_Cursor.Col := To_Column_Index
                 (Char_Index (Sloc_Start.Column),
                  Get_Line (Current_Text, Begin_Cursor, 1));
            end Begin_Of_Profile;

            --------------------
            -- End_Of_Profile --
            --------------------

            procedure End_Of_Profile is
            begin
               End_Cursor.File := Position_File;
               End_Cursor.Line := Sloc_Start.Line;
               End_Cursor.Col := To_Column_Index
                 (Char_Index (Sloc_Start.Column),
                  Get_Line (Current_Text, End_Cursor, 1));
               End_Cursor := File_Cursor
                 (Previous_Char (Current_Text, End_Cursor));

               if Begin_Cursor = Null_File_Cursor then
                  End_Cursor.Col := End_Cursor.Col + 1;
                  Begin_Cursor := Clone (End_Cursor);
               end if;
            end End_Of_Profile;

            Name : constant String := To_Lower
              (Line (Sloc_Start.Index .. Sloc_End.Index));

         begin
            if Paren_Depth = 0 then
               if Entity = Keyword_Text then
                  if Name = "is" then
                     End_Of_Profile;
                     return True;
                  elsif Name = "return" then
                     Begin_Of_Profile;
                  end if;
               elsif Entity = Operator_Text then
                  if Name = "(" then
                     Begin_Of_Profile;
                     Paren_Depth := Paren_Depth + 1;
                  elsif Name = ";" then
                     End_Of_Profile;
                     return True;
                  end if;
               end if;
            else
               if Entity = Operator_Text then
                  if Name = "(" then
                     Paren_Depth := Paren_Depth + 1;
                  elsif Name = ")" then
                     Paren_Depth := Paren_Depth - 1;
                  end if;
               end if;
            end if;

            return False;
         end Entity_Callback;

         Begin_Analyze_Cursor : File_Cursor;

      begin

         Begin_Cursor := Null_File_Cursor;
         End_Cursor := Null_File_Cursor;

         Set_File (Begin_Analyze_Cursor, Position_File);

         Set_Line
           (Begin_Analyze_Cursor, Get_Construct (Position_It).Sloc_Start.Line);

         Set_Column
           (Begin_Analyze_Cursor,
            To_Column_Index
              (Char_Index (Get_Construct (Position_It).Sloc_Start.Column),
               Get_Line (Current_Text, Begin_Analyze_Cursor, 1)));

         Parse_Entities
           (Ada_Lang,
            Current_Text,
            Entity_Callback'Unrestricted_Access,
            Begin_Analyze_Cursor);
      end Initialize_Profile;

      Destination_Begin, Destination_End : File_Cursor;
      Source_Begin, Source_End           : File_Cursor;

   begin
      Initialize_Profile
        (Destination_It,
         Destination_File,
         Destination_Begin,
         Destination_End);

      Initialize_Profile
        (Source_It,
         Source_File,
         Source_Begin,
         Source_End);

      if Destination_Begin = Destination_End then
         This.Add_Spaces := True;
      end if;

      This.Destination_Begin := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Destination_Begin));
      This.Destination_End := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Destination_End));
      This.Source_Begin := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Source_Begin));
      This.Source_End := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Source_End));

      Free (Destination_Begin);
      Free (Destination_End);
      Free (Source_Begin);
      Free (Source_End);
   end Initialize;

   procedure Execute
     (This         : Paste_Profile_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Destination_Begin, Destination_End : File_Cursor;
      Source_Begin, Source_End           : File_Cursor;
      Line_Cursor                        : File_Cursor;

   begin
      Destination_Begin := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Destination_Begin.all));
      Destination_End := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Destination_End.all));
      Source_Begin := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Source_Begin.all));
      Source_End := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Source_End.all));

      Line_Cursor := Destination_Begin;
      Line_Cursor.Col := 1;

      for J in Destination_Begin.Line .. Destination_End.Line loop
         Line_Cursor.Line := J;
         Get_Line (Current_Text, Line_Cursor, New_Extract);
      end loop;

      Replace
        (New_Extract,
         Destination_Begin,
         Destination_End,
         Source_Begin,
         Source_End,
         Current_Text);

      if This.Add_Spaces then
         Replace (New_Extract, Destination_Begin, 0, " ");
      end if;

      Free (Destination_Begin);
      Free (Destination_End);
      Free (Source_Begin);
      Free (Source_End);
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Paste_Profile_Cmd) is
   begin
      Free (This.Destination_Begin);
      Free (This.Destination_End);
      Free (This.Source_Begin);
      Free (This.Source_End);
      Free (Text_Command (This));
   end Free;

   --------------------------------
   -- Get_Package_To_Be_Imported --
   --------------------------------

   function Get_Package_To_Be_Withed
     (Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class) return String
   is
      Tree : constant Construct_Tree :=
        Get_Tree (Current_Text, Source_Position);
      It   : Construct_Tree_Iterator :=
        Get_Iterator_At (Current_Text, Source_Position, After);
   begin
      while Get_Parent_Scope (Tree, It)
        /= Null_Construct_Tree_Iterator
      loop
         It := Get_Parent_Scope (Tree, It);
      end loop;

      return Get_Full_Name (Tree, It);
   end Get_Package_To_Be_Withed;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use
     (This             : out Get_Visible_Declaration_Cmd;
      Current_Text     : Text_Navigator_Abstr'Class;
      Source_Position  : File_Cursor'Class;
      File_Destination : VFS.Virtual_File;
      With_Could_Miss  : Boolean)
   is
      Result      : Get_Visible_Declaration_Cmd;
      With_Cursor : File_Cursor;
      Clauses_Str : GNAT.Strings.String_Access := new String'("");
      Pkg_Name    : constant String := Get_Package_To_Be_Withed
        (Current_Text, Source_Position);

   begin
      if With_Could_Miss then
         With_Cursor := File_Cursor
           (Search_With
              (Current_Text, File_Destination, Pkg_Name));

         if With_Cursor = Null_File_Cursor then
            Assign (Clauses_Str, "with " & Pkg_Name & "; ");
         end if;
      end if;

      Initialize
        (Result.Insert_With,
         Current_Text,
         Get_Next_With_Position (Current_Text, File_Destination),
         Clauses_Str.all & "use " & Pkg_Name & ";");

      Result.Insert_With_Enabled := True;

      Free (Clauses_Str);

      This := Result;
   end Add_Use;

   -------------------
   -- Prefix_Object --
   -------------------

   procedure Prefix_Object
     (This            : out Get_Visible_Declaration_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class;
      Object_Position : File_Cursor'Class;
      With_Could_Miss : Boolean)
   is
      Result      : Get_Visible_Declaration_Cmd;
      Word        : Word_Cursor;
      With_Cursor : File_Cursor;
      Pkg_Name    : constant String := Get_Package_To_Be_Withed
        (Current_Text, Source_Position);
   begin
      if With_Could_Miss then
         With_Cursor := File_Cursor
           (Search_With
              (Current_Text, Get_File (Object_Position), Pkg_Name));

         if With_Cursor = Null_File_Cursor then
            Initialize
              (Result.Insert_With,
               Current_Text,
               Get_Next_With_Position
                 (Current_Text, Get_File (Object_Position)),
               "with " & Pkg_Name & ";");

            Result.Insert_With_Enabled := True;
         end if;
      end if;

      declare
         Prefix : constant String := Get_Full_Prefix
           (Current_Text, Source_Position);
      begin
         if Prefix /= "" then
            Word := (Clone (File_Cursor (Object_Position)) with
                     String_Match => new String'
                       (Prefix & "."),
                     Mode         => Text_Ascii);

            Initialize
              (Result.Prefix_Obj,
               Current_Text,
               Word,
               File_Cursor (Word),
               False);

            Result.Prefix_Obj_Enabled := True;

            Free (Word);
         end if;
      end;

      This := Result;
   end Prefix_Object;

   procedure Execute
     (This         : Get_Visible_Declaration_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class) is
   begin
      if This.Insert_With_Enabled and then not This.Prefix_Obj_Enabled then
         Execute (This.Insert_With, Current_Text, New_Extract);
      elsif This.Prefix_Obj_Enabled and then not This.Insert_With_Enabled then
         Execute (This.Prefix_Obj, Current_Text, New_Extract);
      else
         declare
            Extract_1, Extract_2 : Extract;
            Success              : Boolean;
         begin
            Execute (This.Insert_With, Current_Text, Extract_2);
            Execute (This.Prefix_Obj, Current_Text, Extract_1);

            Merge_Extracts
              (New_Extract,
               Extract_1,
               Extract_2,
               Success,
               True);

            if not Success then
               raise Codefix_Panic;
            end if;
         end;
      end if;
   end Execute;

   procedure Free (This : in out Get_Visible_Declaration_Cmd) is
   begin
      Free (This.Insert_With);
      Free (This.Prefix_Obj);
   end Free;

   -------------------------
   -- Replace_Code_By_Cmd --
   -------------------------

   procedure Initialize
     (This         : in out Replace_Code_By_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Cursor : File_Cursor'Class;
      Replaced_Exp : String;
      New_String   : String)
   is
   begin
      This.Start_Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Start_Cursor));
      This.Replaced_Exp := new String'(Replaced_Exp);
      This.New_String   := new String'(New_String);
   end Initialize;

   procedure Execute
     (This         : Replace_Code_By_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Start_Cursor : File_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Start_Cursor.all));
      Start_Index  : Char_Index;

      Line    : constant String := Get_Line (Current_Text, Start_Cursor, 1);
      Matcher : constant Pattern_Matcher := Compile (This.Replaced_Exp.all);
      Matches : Match_Array (0 .. Paren_Count (Matcher));
   begin
      Start_Index := To_Char_Index (Get_Column (Start_Cursor), Line);

      Match (Matcher, Line (Integer (Start_Index) .. Line'Last), Matches);

      Set_Location (Start_Cursor, Get_Line (Start_Cursor), 1);

      if Matches (0) /= No_Match then
         Get_Line
           (Current_Text,
            Start_Cursor,
            New_Extract);

         Replace
           (New_Extract,
            To_Column_Index (Char_Index (Matches (1).First), Line),
            Matches (1).Last - Matches (1).First + 1,
            This.New_String.all);
      else
         Raise_Exception
           (Codefix_Panic'Identity,
            "Impossible to match """ &  This.Replaced_Exp.all & """ on " &
            """" & Line & """.");
      end if;
   end Execute;

   procedure Free (This : in out Replace_Code_By_Cmd) is
   begin
      Free (This.Replaced_Exp);
      Free (This.New_String);
      Free (This.Start_Cursor);
   end Free;

end Codefix.Text_Manager.Ada_Commands;

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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Regpat; use GNAT.Regpat;

with String_Utils; use String_Utils;

with Codefix.Text_Manager.Ada_Extracts; use Codefix.Text_Manager.Ada_Extracts;

package body Codefix.Text_Manager.Ada_Commands is

   ---------------------
   -- Recase_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Word);
      Assign (This.Correct_Word, Correct_Word);
      This.Word_Case := Word_Case;
   end Initialize;

   procedure Execute
     (This         : Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class) is

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
      Word         : Word_Cursor;
      Matches      : Match_Array (0 .. 1);
      Size         : Integer;
      Line         : Dynamic_String;
      Word_Chosen  : Dynamic_String;

   begin

      Make_Word_Cursor (This.Word, Current_Text, Word);
      Cursor_Line := File_Cursor (Word);

      Cursor_Line.Col := 1;
      Get_Line (Current_Text, Cursor_Line, New_Extract);
      Assign (Line, Get_String (New_Extract));
      Match (Word_Matcher, Line (Word.Col .. Line'Length), Matches);

      Size := Matches (1).Last - Matches (1).First + 1;

      if This.Correct_Word.all /= "" then
         Word_Chosen := Clone (This.Correct_Word);
      else
         Word_Chosen := new String'
           (To_Correct_Case (Line (Matches (1).First .. Matches (1).Last)));
      end if;

      Replace_Word
        (New_Extract,
         Word,
         Word_Chosen (Word_Chosen'Last - Size + 1 .. Word_Chosen'Last),
         Size);

      Free (Word_Chosen);

   end Execute;


   procedure Free (This : in out Recase_Word_Cmd) is
   begin
      Free (This.Word);
      Free (This.Correct_Word);
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
      New_Extract  : in out Extract'Class)
   is
      Start_Instruction : File_Cursor := File_Cursor
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
   end Free;

   -------------------------
   -- Remove_Elements_Cmd --
   -------------------------

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
      New_Extract  : in out Extract'Class)
   is
      Remove_Extracts : Ada_Lists.List;
      Current_Cursor  : Word_Cursor;
      Current_Word    : Mark_List.List_Node;
      Current_Extract : Ada_Lists.List_Node;
      Extract_Temp    : Ada_List;
      Already_Loaded  : Boolean;
      Merge_Success   : Boolean;
   begin
      Current_Word := First (This.Remove_List);

      while Current_Word /= Mark_List.Null_Node loop
         Make_Word_Cursor (Data (Current_Word), Current_Text, Current_Cursor);
         Current_Extract := First (Remove_Extracts);
         Already_Loaded := False;

         while Current_Extract /= Ada_Lists.Null_Node loop
            if File_Cursor (Current_Cursor) >=
                Get_Start (Data (Current_Extract))
              and then File_Cursor (Current_Cursor) <=
                Get_Stop (Data (Current_Extract)) then
               Extract_Temp := Clone (Data (Current_Extract));
               Remove_Elements (Extract_Temp, Current_Cursor.String_Match.all);
               Set_Data (Current_Extract, Extract_Temp);
               Unchecked_Free (Extract_Temp);
               Already_Loaded := True;
               exit;
            end if;

            Current_Extract := Next (Current_Extract);
         end loop;

         if not Already_Loaded then
            Get_Unit (Current_Text, Current_Cursor, Extract_Temp);
            Remove_Elements (Extract_Temp, Current_Cursor.String_Match.all);
            Append (Remove_Extracts, Extract_Temp);
            Unchecked_Free (Extract_Temp);
         end if;

         Already_Loaded := False;

         Current_Word := Next (Current_Word);
      end loop;

      Current_Extract := First (Remove_Extracts);

      while Current_Extract /= Ada_Lists.Null_Node loop

         Unchecked_Assign (Extract_Temp, New_Extract);
         Unchecked_Free (New_Extract);

         Merge_Extracts
           (New_Extract,
            Extract_Temp,
            Data (Current_Extract),
            Merge_Success,
            False);

         if not Merge_Success then
            raise Text_Manager_Error;
         end if;

         Free (Extract_Temp);
         Current_Extract := Next (Current_Extract);
      end loop;

   end Execute;

   procedure Free (This : in out Remove_Elements_Cmd) is
   begin
      Free (This.Remove_List);
   end Free;

   ----------------------------
   -- Remove_Pkg_Clauses_Cmd --
   ----------------------------


   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor)
   is
      Use_Info, Pkg_Info     : Construct_Information;
      Word_Use               : Word_Cursor := Word;
      Index_Name, Prev_Index : Natural := 0;
   begin
      Pkg_Info := Search_Unit
        (Current_Text, Word.File_Name.all, Cat_With, Word.String_Match.all);

      if Pkg_Info.Category = Cat_Unknown then
         Pkg_Info := Search_Unit
           (Current_Text, Word.File_Name.all,
            Cat_Package,
            Word.String_Match.all);
         Initialize (This.Instantiation_Pkg, Current_Text, Word);
         This.Is_Instantiation := True;
      else
         Add_To_Remove (This.Clauses_Pkg, Current_Text, Word);
         This.Is_Instantiation := False;
      end if;

      loop
         Index_Name := Index_Name + 1;
         Skip_To_Char (Pkg_Info.Name.all, Index_Name, '.');
         exit when Index_Name > Pkg_Info.Name'Last + 1;

         Use_Info := Search_Unit
           (Current_Text,
            Word.File_Name.all,
            Cat_Use,
            Pkg_Info.Name.all (Prev_Index + 1 .. Index_Name - 1));

         if Use_Info.Category /= Cat_Unknown then
            Word_Use.Col := Use_Info.Sloc_Start.Column;
            Word_Use.Line := Use_Info.Sloc_Start.Line;
            Assign (Word_Use.String_Match, Use_Info.Name.all);
            Add_To_Remove (This.Clauses_Pkg, Current_Text, Word_Use);
         end if;

         Prev_Index := Index_Name;
      end loop;

   end Initialize;

   procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class) is
   begin
      if This.Is_Instantiation then
         Execute (This.Instantiation_Pkg, Current_Text, New_Extract);
      end if;

      Execute (This.Clauses_Pkg, Current_Text, New_Extract);
   end Execute;


   procedure Free (This : in out Remove_Pkg_Clauses_Cmd) is
   begin
      Free (This.Instantiation_Pkg);
      Free (This.Clauses_Pkg);
   end Free;

   -----------------------
   -- Remove_Entity_Cmd --
   -----------------------

   procedure Initialize
     (This         : in out Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Entity : File_Cursor'Class)
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
   end Initialize;

   procedure Execute
     (This         : Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class)
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

      Erase (Body_Extract, Body_Begin, Body_End);

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

         Erase (Spec_Extract, Spec_Begin, Spec_End);

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
         Unchecked_Assign (New_Extract, Body_Extract);
      end if;
   end Execute;

   procedure Free (This : in out Remove_Entity_Cmd) is
   begin
      Free (This.Spec_Begin);
      Free (This.Spec_End);
      Free (This.Body_Begin);
      Free (This.Body_End);
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
      New_Extract  : in out Extract'Class)
   is
      Position : File_Cursor;
   begin
      Position := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));

      --  ??? Later, this function could detect the presence of another pragma
      --  and not add one a second time

      Add_Line
        (New_Extract,
         Position,
         "pragma " & This.Name.all & "(" & This.Argument.all & ")");
   end Execute;

   procedure Free (This : in out Add_Pragma_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
      Free (This.Argument);
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
      New_Extract  : in out Extract'Class)
   is
      Cursor        : File_Cursor;
      Work_Extract  : Ada_List;
      Tmp_Extract   : Extract;
      New_Instr     : Dynamic_String;
      Col_Decl      : Natural;
      Success_Merge : Boolean;
   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));
      Get_Unit (Current_Text, Cursor, Work_Extract);

      if Get_Number_Of_Elements (Work_Extract) = 1 then
         Replace_Word
           (Work_Extract,
            Search_String (Work_Extract, ":"),
            ": constant",
            ":");
      else
         Cut_Off_Elements (Work_Extract, New_Instr, This.Name.all);

         Col_Decl := New_Instr'First;
         Skip_To_Char (New_Instr.all, Col_Decl, ':');

         Assign
           (New_Instr,
            New_Instr (New_Instr'First .. Col_Decl) & " constant" &
              New_Instr (Col_Decl + 1 .. New_Instr'Last));

         Add_Line (Work_Extract, Get_Stop (Work_Extract), New_Instr.all);
         Free (New_Instr);
      end if;

      Unchecked_Assign (Tmp_Extract, New_Extract);
      Unchecked_Free (New_Extract);

      Merge_Extracts
        (New_Extract,
         Tmp_Extract,
         Work_Extract,
         Success_Merge,
         False);

      if not Success_Merge then
         raise Codefix_Panic;
      end if;

      Free (Work_Extract);

   end Execute;

   procedure Free (This : in out Make_Constant_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
   end Free;

end Codefix.Text_Manager.Ada_Commands;

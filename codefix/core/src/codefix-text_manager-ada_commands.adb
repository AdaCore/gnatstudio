-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2002-2010, AdaCore                  --
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

with Ada.Characters.Handling;       use Ada.Characters.Handling;
with GNATCOLL.Symbols;              use GNATCOLL.Symbols;
with GNATCOLL.Utils;                use GNATCOLL.Utils;
with Case_Handling;                 use Case_Handling;
with Codefix.Ada_Tools;             use Codefix.Ada_Tools;
with String_Utils;                  use String_Utils;
with Language.Ada;                  use Language.Ada;
with Codefix.Text_Manager.Commands; use Codefix.Text_Manager.Commands;
with Entities; use Entities;
with GNATCOLL.Traces; use GNATCOLL.Traces;

package body Codefix.Text_Manager.Ada_Commands is

   Me : constant Trace_Handle := Create ("Codefix");

   function Get_Closing_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Open_Cursor  : File_Cursor'Class) return File_Cursor;
   --  Returns the Closing parenthesis corresponding to the open one given
   --  in parameter

   -----------------------
   -- Get_Closing_Paren --
   -----------------------

   function Get_Closing_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Open_Cursor  : File_Cursor'Class) return File_Cursor
   is
      Close_Cursor : File_Cursor;

      Depth : Integer := 0;

      function Entity_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean;

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
         if Entity = Operator_Text and then Name = "(" then
            Depth := Depth + 1;
         elsif Entity = Operator_Text and then Name = ")" then
            Depth := Depth - 1;

            if Depth = 0 then
               Close_Cursor.Set_File (Open_Cursor.File);
               Close_Cursor.Set_Line (Sloc_Start.Line);
               Close_Cursor.Set_Column
                 (To_Column_Index
                    (String_Index_Type (Sloc_Start.Column), Line));

               return True;
            end if;
         end if;

         return False;
      end Entity_Callback;

   begin
      Parse_Entities
        (Ada_Lang,
         Current_Text.Get_File (Open_Cursor.File).all,
         Entity_Callback'Unrestricted_Access,
         Open_Cursor);

      return Close_Cursor;
   end Get_Closing_Paren;

   --  Recase_Word_Cmd

   ----------------
   -- Initialize --
   ----------------

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Recase_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
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

      Word   : Word_Cursor;
      Cursor : File_Cursor;
   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Cursor.all));
      Word :=
        (Cursor with
         String_Match => new String'("([\w]+)"), Mode => Regular_Expression);

      declare
         Miscased_Word : constant String :=
           Word.Get_Matching_Word (Current_Text);
      begin
         if This.Correct_Word.all /= "" then
            Current_Text.Replace
              (Cursor,
               Miscased_Word'Length,
               This.Correct_Word.all
                 (This.Correct_Word.all'Last - Miscased_Word'Length + 1
                  .. This.Correct_Word.all'Last));
         else
            Current_Text.Replace
              (Cursor,
               Miscased_Word'Length,
               To_Correct_Case (Miscased_Word));
         end if;
      end;

      Free (Word);
      Free (Cursor);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Recase_Word_Cmd) is
   begin
      Free (This.Cursor);
      Free (This.Correct_Word);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Recase_Word_Cmd) return Boolean is
   begin
      return This.Cursor.Get_File.Is_Writable;
   end Is_Writable;

   --  Remove_Instruction_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This              : in out Remove_Instruction_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Start_Instruction : File_Cursor'Class) is
   begin
      This.Begin_Mark := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Start_Instruction));
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Remove_Instruction_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Start_Instruction : constant File_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Begin_Mark.all));
      Instruction       : Ada_Statement;

      Location : constant Universal_Location := To_Location
        (Get_Or_Create (Db   => Get_Context (Current_Text).Construct_Db,
                        File => Start_Instruction.File),
         Start_Instruction.Line,
         Start_Instruction.Col);
   begin
      Initialize (Instruction, Get_Context (Current_Text), Location);

      Remove (Instruction);
      Free (Instruction);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Instruction_Cmd) is
   begin
      Free (This.Begin_Mark);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Instruction_Cmd) return Boolean is
   begin
      return This.Begin_Mark.File_Name.Is_Writable;
   end Is_Writable;

   --  Remove_Elements_Cmd

   ---------------------
   -- Set_Remove_Mode --
   ---------------------

   procedure Set_Remove_Mode
     (This : in out Remove_Elements_Cmd; Mode : Remove_Code_Mode)
   is
   begin
      This.Mode := Mode;
   end Set_Remove_Mode;

   -------------------
   -- Add_To_Remove --
   -------------------

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Remove_Elements_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Current_Cursor : Word_Cursor;
      Current_Word   : Mark_List.List_Node;
   begin
      Current_Word := First (This.Remove_List);

      while Current_Word /= Mark_List.Null_Node loop
         Make_Word_Cursor (Data (Current_Word), Current_Text, Current_Cursor);

         declare
            Extract_Temp : Ada_Statement;
            Loc          : aliased Universal_Location;
         begin
            Loc := To_Location
              (Current_Text.Get_Structured_File (Current_Cursor.File),
               Current_Cursor.Line,
               Current_Cursor.Col);

            Refactoring.Services.Initialize
              (Self     => Extract_Temp,
               Context  => Get_Context (Current_Text),
               Location => Loc);

            Remove_Element
              (Extract_Temp,
               This.Mode,
               Find_Normalized
                 (Get_Symbols
                    (Get_Context (Current_Text).Entity_Db),
                  Current_Cursor.String_Match.all));
         end;

         Current_Word := Next (Current_Word);
      end loop;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Elements_Cmd) is
   begin
      Free (This.Remove_List);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Elements_Cmd) return Boolean is
      Cur : Mark_List.List_Node := First (This.Remove_List);
   begin
      while Cur /= Mark_List.Null_Node loop
         if not Mark_List.Data (Cur).Mark_Id.Get_File.Is_Writable then
            return False;
         end if;

         Cur := Next (Cur);
      end loop;

      return True;
   end Is_Writable;

   --  Remove_Pkg_Clauses_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Position     : Relative_Position := Specified;
      Destination  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Category     : Dependency_Category := Cat_With;
      Look_For_Use : Boolean := True)
   is
   begin
      This.Word := new Mark_Abstr'Class'(Current_Text.Get_New_Mark (Word));
      This.Word_Str := new String'(Word.Get_Word);
      This.Position := Position;
      This.Destination := Destination;
      This.Category := Category;
      This.Look_For_Use := Look_For_Use;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      use Codefix.Ada_Tools.Words_Lists;

      Word         : Word_Cursor;
      Pkg_Info     : Simple_Construct_Information;
      Clauses_List : Words_Lists.List;
      Clause_Node  : Words_Lists.List_Node;
      Last_With    : File_Cursor := Null_File_Cursor;

      Is_Instantiation : Boolean;
      Node      : String_List.List_Node;

      Instantiation_Pkg : Ada_Statement;
      Clauses_Pkg       : Ada_Statement;
      Obj_List          : String_List.List;
   begin
      Trace (Me, "Execute Remove_Pkg_Clauses_Cmd");

      File_Cursor (Word) :=
        File_Cursor (Current_Text.Get_Current_Cursor (This.Word.all));
      Word.String_Match := new String'(This.Word_Str.all);

      declare
         It : constant Construct_Tree_Iterator := Get_Iterator_At
           (Current_Text,
            Word,
            Position => This.Position,
            Categories_Seeked => (1 => This.Category));
      begin
         Pkg_Info := Get_Construct (It).all;
      end;

      if Pkg_Info.Category /= Cat_Unknown then
         Assign (Word.String_Match, Get (Pkg_Info.Name).all);
      end if;

      if Pkg_Info.Category = Cat_Unknown then
         Pkg_Info := Search_Unit
           (Current_Text, Get_File (Word),
            Cat_Package,
            Word.String_Match.all);

         Initialize
           (Instantiation_Pkg,
            Get_Context (Current_Text),
            To_Location
              (Get_Or_Create
                 (Get_Context (Current_Text).Construct_Db, Word.File),
               Word.Line,
               Word.Col));

         Is_Instantiation := True;
      else
         Initialize
           (Clauses_Pkg,
            Get_Context (Current_Text),
            To_Location
              (Get_Or_Create
                 (Get_Context (Current_Text).Construct_Db, Word.File),
               Word.Line,
               Word.Col));

         if Get_Kind (Clauses_Pkg) not in Clause_Kind then
            --  If the list is neither a use or a with list, then that means
            --  that something changed in the text (e.g. the clause has been
            --  removed from a previous message). The fix is obsolescent.

            raise Obsolescent_Fix;
         end if;

         if This.Destination /= GNATCOLL.VFS.No_File then
            Append
              (Obj_List,
               new String'("with " & Get (Pkg_Info.Name).all & ";"));
         end if;

         Is_Instantiation := False;
      end if;

      --  If the category is not already a use clause, see if there are use
      --  clauses for that unit and remove them as well.

      if This.Look_For_Use and then This.Category /= Cat_Use then
         Clauses_List := Get_Use_Clauses
           (Word.String_Match.all,
            Get_File (Word),
            Current_Text,
            True);

         Clause_Node := First (Clauses_List);

         while Clause_Node /= Words_Lists.Null_Node loop
            declare
               Use_Pck : Ada_Statement;
            begin
               Initialize
                 (Use_Pck,
                  Get_Context (Current_Text),
                  To_Location
                    (Get_Or_Create
                       (Get_Context (Current_Text).Construct_Db, Word.File),
                     Data (Clause_Node).Line,
                     Data (Clause_Node).Col));

               Remove_Element
                 (Self => Use_Pck,
                  Mode => Erase,
                  Name => Find_Normalized
                    (Symbols =>
                       Get_Symbols (Get_Context (Current_Text).Entity_Db),
                     Name    => Data (Clause_Node).Get_Word));

               Free (Use_Pck);
            end;

            if This.Destination /= GNATCOLL.VFS.No_File then
               Append
                 (Obj_List,
                  new String'
                    ("use " & Data (Clause_Node).String_Match.all & ";"));
            end if;

            Clause_Node := Next (Clause_Node);
         end loop;

         Free (Clauses_List);
      end if;

      if This.Destination /= GNATCOLL.VFS.No_File then
         Last_With := File_Cursor
           (Get_Next_With_Position (Current_Text, This.Destination));
      end if;

      if Is_Instantiation then
         Remove (Instantiation_Pkg);
      end if;

      Remove_Element
        (Self => Clauses_Pkg,
         Mode => Erase,
         Name => Find_Normalized
           (Symbols => Get_Symbols (Get_Context (Current_Text).Entity_Db),
            Name    => Word.Get_Word));

      if Last_With /= Null_File_Cursor then
         Node := First (Obj_List);

         while Node /= String_List.Null_Node loop
            Current_Text.Add_Line (Last_With, Data (Node).all);
            Node := Next (Node);
         end loop;

         Free (Last_With);
      end if;

      Free (Word);
      Free (Instantiation_Pkg);
      Free (Clauses_Pkg);
      Free (Obj_List);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Pkg_Clauses_Cmd) is
   begin
      Free (This.Word_Str);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Pkg_Clauses_Cmd) return Boolean is
   begin
      return This.Word.Get_File.Is_Writable
        and then
          (This.Destination = No_File
           or else This.Destination.Is_Writable);
   end Is_Writable;

   --  Remove_Entity_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Entity : File_Cursor'Class;
      Mode         : Remove_Code_Mode := Erase)
   is
   begin
      This.Start_Entity := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Start_Entity));
      This.Mode := Mode;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Remove_Entity_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Start_Entity          : constant File_Cursor := File_Cursor
        (Current_Text.Get_Current_Cursor (This.Start_Entity.all));
      Text                  : Ptr_Text;
      Spec_Begin, Spec_End  : File_Cursor;
      Body_Begin, Body_End  : File_Cursor;
      Line_Cursor           : File_Cursor;
   begin
      Get_Entity
        (Current_Text,
         Start_Entity,
         Spec_Begin, Spec_End,
         Body_Begin, Body_End);

      if Spec_Begin = Null_File_Cursor
        and then Body_Begin = Null_File_Cursor
      then
         --  In this case, we didn't manage to retrieve the entity. It may be
         --  due to previous fixes, or to manual edit of the file. There's
         --  nothing we can do. No need to raise an exception, since this
         --  kind of situation is completely expected in the fix process.

         return;
      end if;

      Text := Current_Text.Get_File (Body_Begin.File);

      Line_Cursor := Body_Begin;
      Line_Cursor.Col := 1;

      while Line_Cursor.Line <= Body_End.Line loop
         Line_Cursor.Line := Line_Cursor.Line + 1;
      end loop;

      case This.Mode is
         when Erase =>
            Text.Erase (Body_Begin, Body_End);
         when Comment =>
            Text.Comment (Body_Begin, Body_End);
      end case;

      if Spec_Begin /= Null_File_Cursor then
         Line_Cursor := Spec_Begin;
         Line_Cursor.Col := 1;

         Text := Current_Text.Get_File (Spec_Begin.File);

         while Line_Cursor.Line <= Spec_End.Line loop
            Line_Cursor.Line := Line_Cursor.Line + 1;
         end loop;

         case This.Mode is
            when Erase =>
               Text.Erase (Spec_Begin, Spec_End);
            when Comment =>
               Text.Comment (Spec_Begin, Spec_End);
         end case;
      end if;

      Free (Spec_Begin);
      Free (Spec_End);
      Free (Body_Begin);
      Free (Body_End);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Entity_Cmd) is
   begin
      Free (This.Start_Entity.all);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Entity_Cmd) return Boolean is
   begin
      return This.Start_Entity.File_Name.Is_Writable;
   end Is_Writable;

   --  Add_Pragma_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This           : in out Add_Pragma_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Position       : File_Cursor'Class;
      Category       : Language_Category;
      Name, Argument : String) is
   begin
      Assign (This.Name, Name);
      Assign (This.Argument, Argument);
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
      This.Category := Category;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Add_Pragma_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Position.all);
      Position : File_Cursor;

      procedure Add_Pragma;
      procedure Add_Literal_Pragma;
      procedure Add_Parameter_Pragma;

      ----------------
      -- Add_Pragma --
      ----------------

      procedure Add_Pragma is
         Declaration  : Construct_Tree_Iterator;
         Char_Ind     : String_Index_Type;
      begin
         Declaration := Get_Iterator_At (Current_Text, Cursor);

         Char_Ind := String_Index_Type
           (Get_Construct (Declaration).Sloc_End.Column);

         --  ??? This test is only here because the parser returns sometimes
         --  a Sloc_End.Col equal to 0.

         if Char_Ind = 0 then
            Char_Ind := 1;
         end if;

         Set_File (Position, Get_File (Cursor));
         Set_Location
           (Position, Get_Construct (Declaration).Sloc_End.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, Position);
         begin
            Set_Location
              (Position,
               Get_Construct (Declaration).Sloc_End.Line,
               To_Column_Index (Char_Ind, Line));
         end;
      end Add_Pragma;

      ------------------------
      -- Add_Literal_Pragma --
      ------------------------

      procedure Add_Literal_Pragma is
         Declaration : Construct_Tree_Iterator;
         Char_Ind    : String_Index_Type;
      begin
         Declaration := Get_Iterator_At
           (Current_Text,
            Cursor,
            Position => Before,
            Categories_Seeked => (1 => Cat_Type));
         Char_Ind := String_Index_Type
           (Get_Construct (Declaration).Sloc_End.Column);

         if Char_Ind = 0 then
            Char_Ind := 1;
         end if;

         Set_File (Position, Get_File (Cursor));
         Set_Location
           (Position, Get_Construct (Declaration).Sloc_End.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, Position);
         begin
            Set_Location
              (Position,
               Get_Construct (Declaration).Sloc_End.Line,
               To_Column_Index (Char_Ind, Line));
         end;
      end Add_Literal_Pragma;

      --------------------------
      -- Add_Parameter_Pragma --
      --------------------------

      procedure Add_Parameter_Pragma is
         Garbage : File_Cursor;
         Declaration : Construct_Tree_Iterator;
      begin
         Declaration := Get_Iterator_At
           (Current_Text,
            Cursor,
            Position => Before,
            Categories_Seeked => (Cat_Procedure, Cat_Function));
         Set_File (Position, Get_File (Cursor));
         Set_Location
           (Position, Get_Construct (Declaration).Sloc_Entity.Line, 1);

         declare
            Line : constant String := Get_Line (Current_Text, Position);
         begin
            Set_Location
              (Position, Get_Construct (Declaration).Sloc_Entity.Line,
               To_Column_Index
                 (String_Index_Type
                    (Get_Construct (Declaration).Sloc_Entity.Column), Line));
         end;

         Garbage := Position;
         Position := File_Cursor
           (Search_Token (Current_Text, Position, Close_Paren_Tok));
         Free (Garbage);

         Garbage := Position;
         Position := File_Cursor
           (Search_Token (Current_Text, Position, Is_Tok));
         Free (Garbage);
      end Add_Parameter_Pragma;

      Actual_Category : Language_Category;

   begin
      if This.Category = Cat_Unknown then
         declare
            It : constant Construct_Tree_Iterator :=
              Get_Iterator_At (Current_Text, Cursor);
         begin
            Actual_Category := Get_Construct (It).Category;
         end;
      else
         Actual_Category := This.Category;
      end if;

      case Actual_Category is
         when Cat_Literal =>
            Add_Literal_Pragma;

         when Cat_Parameter =>
            Add_Parameter_Pragma;

         when others =>
            Add_Pragma;

      end case;

      declare
         Pragma_Cursor : File_Cursor;
         Line_Cursor   : File_Cursor;
         Next_Str      : Word_Cursor;
      begin
         Pragma_Cursor := Position;
         Pragma_Cursor.Line := Pragma_Cursor.Line + 1;
         Pragma_Cursor.Col := 1;

         Next_Word (Current_Text, Pragma_Cursor, Next_Str);

         if To_Lower (Next_Str.Get_Word) = "pragma" then
            Free (Next_Str);
            Next_Word (Current_Text, Pragma_Cursor, Next_Str);

            if To_Lower (Next_Str.Get_Word) = To_Lower (This.Name.all) then
               Pragma_Cursor := File_Cursor
                 (Search_Token (Current_Text, Pragma_Cursor, Close_Paren_Tok));
               Line_Cursor := Pragma_Cursor;
               Line_Cursor.Col := 1;
               Current_Text.Replace
                 (Pragma_Cursor, 1, ", " & This.Argument.all & ")");
               Free (Line_Cursor);
            end if;

         else
            Current_Text.Add_Line
              (Position,
               "pragma " & This.Name.all & " (" & This.Argument.all & ");",
               True);
         end if;

         Free (Next_Str);
         Free (Position);
      end;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Add_Pragma_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
      Free (This.Argument);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Add_Pragma_Cmd) return Boolean is
   begin
      return This.Position.Get_File.Is_Writable;
   end Is_Writable;

   --  Make_Constant_Cmd

   ----------------
   -- Initialize --
   ----------------

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Make_Constant_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor        : File_Cursor;
      Work_Extract  : Ada_Statement;
      New_Instr     : GNAT.Strings.String_Access;
      Col_Decl      : Natural;
      End_Decl      : aliased Universal_Location;

   begin
      Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Position.all));

      Initialize
        (Self     => Work_Extract,
         Context  => Get_Context (Current_Text),
         Location => To_Location
           (Get_Structured_File (Current_Text, Cursor.File),
            Cursor.Line,
            Cursor.Col));

      if Number_Of_Declarations (Work_Extract) = 1 then
         Current_Text.Replace
           (Position      => File_Cursor'Class
              (Current_Text.Search_Token (Cursor, Semicolon_Tok)),
            Len           => 1,
            New_Text      => ": constant",
            Blanks_Before => Keep,
            Blanks_After  => One);
      else
         Extract_Element
           (Work_Extract,
            New_Instr,
            Find_Normalized
              (Get_Symbols (Get_Context (Current_Text).Entity_Db),
               This.Name.all));

         Col_Decl := New_Instr'First;
         Skip_To_Char (New_Instr.all, Col_Decl, ':');

         if New_Instr (Col_Decl + 1) /= ' ' then
            Assign
              (New_Instr,
               New_Instr (New_Instr'First .. Col_Decl) & " constant" & " "
               & New_Instr (Col_Decl + 1 .. New_Instr'Last));
         else
            Assign
              (New_Instr,
               New_Instr (New_Instr'First .. Col_Decl) & " constant"
               & New_Instr (Col_Decl + 1 .. New_Instr'Last));
         end if;

         End_Decl := Get_End (Work_Extract);
         Current_Text.Add_Line
           (File_Cursor'
              (Line => Get_Line (End_Decl'Access),
               Col  => Get_Column (End_Decl'Access),
               File => Get_File_Path (Get_File (End_Decl'Access))),
            New_Instr.all, True);

         Free (New_Instr);
      end if;

      Free (Work_Extract);
      Free (Cursor);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Make_Constant_Cmd) is
   begin
      Free (This.Position);
      Free (This.Name);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Make_Constant_Cmd) return Boolean is
   begin
      return This.Position.Get_File.Is_Writable;
   end Is_Writable;

   --  Remove_Parenthesis_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Remove_Conversion_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) is
   begin
      This.Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Cursor));
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Remove_Conversion_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor        : File_Cursor := File_Cursor
        (Get_Current_Cursor (Current_Text, This.Cursor.all));
      Text : Ptr_Text;

      Open_Paren : File_Cursor'Class :=
        Current_Text.Search_Token (Cursor, Open_Paren_Tok);
      Close_Paren : File_Cursor;
   begin
      Text := Current_Text.Get_File (Cursor.File);

      Close_Paren := Get_Closing_Paren (Current_Text, Open_Paren);

      Current_Text.Replace (Close_Paren, 1, "");

      Text.Erase (Cursor, Current_Text.Search_Token (Cursor, Open_Paren_Tok));

      Free (Open_Paren);
      Free (Close_Paren);
      Free (Cursor);
   end Execute;

   --------
   -- Free --
   --------

   overriding procedure Free (This : in out Remove_Conversion_Cmd) is
   begin
      Free (This.Cursor);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Conversion_Cmd) return Boolean is
   begin
      return This.Cursor.Get_File.Is_Writable;
   end Is_Writable;

   --  Paste_Profile_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This                              : in out Paste_Profile_Cmd;
      Current_Text                      : Text_Navigator_Abstr'Class;
      Source_Cursor, Destination_Cursor : File_Cursor'Class;
      Source_Loc, Destination_Loc       : Relative_Position)
   is
   begin
      This.Source_Mark := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Source_Cursor));
      This.Destination_Mark := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Destination_Cursor));
      This.Look_For_Source := Source_Loc;
      This.Look_For_Destination := Destination_Loc;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Paste_Profile_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      procedure Initialize_Profile
        (Position_It              : Construct_Tree_Iterator;
         Position_File            : GNATCOLL.VFS.Virtual_File;
         Begin_Cursor, End_Cursor : out File_Cursor;
         Is_Empty, Is_Spec        : out Boolean);
      --  Set Begin_Cursor and End_Cursor at the begining and at the end of the
      --  profile's function starting at position.

      ------------------------
      -- Initialize_Profile --
      ------------------------

      procedure Initialize_Profile
        (Position_It              : Construct_Tree_Iterator;
         Position_File            : GNATCOLL.VFS.Virtual_File;
         Begin_Cursor, End_Cursor : out File_Cursor;
         Is_Empty, Is_Spec        : out Boolean)
      is
         Paren_Depth : Integer := 0;
         Last_Entity_Column : Integer;
         Last_Entity_Line : Integer;
         Profile_Started : Boolean := False;

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
               if not Profile_Started then
                  Profile_Started := True;
                  Begin_Cursor.File := Position_File;
                  Begin_Cursor.Line := Sloc_Start.Line;
                  Begin_Cursor.Col := To_Column_Index
                    (String_Index_Type (Sloc_Start.Column),
                     Get_Line (Current_Text, Begin_Cursor, 1));
               end if;
            end Begin_Of_Profile;

            --------------------
            -- End_Of_Profile --
            --------------------

            procedure End_Of_Profile is
            begin
               End_Cursor.File := Position_File;
               End_Cursor.Line := Last_Entity_Line;
               End_Cursor.Col := To_Column_Index
                 (String_Index_Type (Last_Entity_Column),
                  Get_Line (Current_Text, End_Cursor, 1));

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
                  if Equal (Name, "is", False)
                    or else Equal (Name, "do", False)
                  then
                     End_Of_Profile;
                     Is_Spec := False;
                     return True;
                  elsif Equal (Name, "return", False) then
                     Begin_Of_Profile;
                     Is_Empty := False;
                  end if;
               elsif Entity = Operator_Text then
                  if Name = "(" then
                     Begin_Of_Profile;
                     Paren_Depth := Paren_Depth + 1;
                     Is_Empty := False;
                  elsif Name = ";" then
                     End_Of_Profile;
                     Is_Spec := True;
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

            case Entity is
               when Comment_Text
                  | Annotated_Comment_Text
                  | Annotated_Keyword_Text =>
                  --  Don't take into account leading comments or annotations
                  --  in the profile. We don't want to copy them, and we don't
                  --  want to change them either.
                  null;

               when others =>
                  Last_Entity_Column := Sloc_End.Column;
                  Last_Entity_Line := Sloc_End.Line;

            end case;

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
              (String_Index_Type
                 (Get_Construct (Position_It).Sloc_Start.Column),
               Get_Line (Current_Text, Begin_Analyze_Cursor, 1)));

         Last_Entity_Column := Integer (Begin_Analyze_Cursor.Col);
         Last_Entity_Line := Begin_Analyze_Cursor.Line;

         Is_Empty := True;
         Is_Spec := True;

         Parse_Entities
           (Ada_Lang,
            Current_Text,
            Entity_Callback'Unrestricted_Access,
            Begin_Analyze_Cursor);
      end Initialize_Profile;

      Destination_Begin, Destination_End : File_Cursor;
      Source_Begin, Source_End           : File_Cursor;

      Is_Empty, Is_Spec : Boolean;

      Destination_It   : Construct_Tree_Iterator;
      Source_It        : Construct_Tree_Iterator;

      Source_Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Source_Mark.all);
      Destination_Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Destination_Mark.all);
      Blank_Before, Blank_After         : Replace_Blanks_Policy := Keep;

      Lock_Source : Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (Source_Cursor.Get_File));
      Lock_Destination : Update_Lock := Lock_Updates
        (Current_Text.Get_Structured_File (Destination_Cursor.Get_File));

      Profile_Categories : constant Category_Array :=
        (Cat_Procedure,
         Cat_Function,
         Cat_Entry,
         Cat_Accept_Statement,
         Cat_Type,
         Cat_Structure,
         Cat_Class,
         Cat_Subtype);

   begin
      Source_It := Get_Iterator_At
        (Current_Text,
         Source_Cursor,
         Position          => This.Look_For_Source,
         Categories_Seeked => Profile_Categories);
      Destination_It := Get_Iterator_At
        (Current_Text,
         Destination_Cursor,
         Position          => This.Look_For_Destination,
         Categories_Seeked => Profile_Categories);

      Initialize_Profile
        (Source_It,
         Get_File (Source_Cursor),
         Source_Begin,
         Source_End,
         Is_Empty,
         Is_Spec);

      if Is_Empty then
         Blank_Before := None;
      else
         Blank_Before := One;
      end if;

      Initialize_Profile
        (Destination_It,
         Get_File (Destination_Cursor),
         Destination_Begin,
         Destination_End,
         Is_Empty,
         Is_Spec);

      if Is_Spec then
         --  We don't add any space before ";"
         Blank_After := None;
      else
         --  We add a space before "is"
         Blank_After := One;
      end if;

      if Is_Empty then
         Current_Text.Replace
           (Destination_Begin,
            0,
            Current_Text.Get (Source_Begin, Source_End),
            Blank_Before,
            Blank_After);
      else
         Current_Text.Replace
           (Destination_Begin,
            Destination_End,
            Current_Text.Get (Source_Begin, Source_End),
            Blank_Before,
            Blank_After);
      end if;

      Free (Destination_Begin);
      Free (Destination_End);
      Free (Source_Begin);
      Free (Source_End);

      Lock_Source.Unlock;
      Lock_Destination.Unlock;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Paste_Profile_Cmd) is
   begin
      Free (Text_Command (This));
      Free (This.Source_Mark);
      Free (This.Destination_Mark);
   end Free;

   -----------------------
   -- Paste_Profile_Cmd --
   -----------------------

   overriding
   function Is_Writable (This : Paste_Profile_Cmd) return Boolean is
   begin
      return This.Destination_Mark.Get_File.Is_Writable;
   end Is_Writable;

   ------------------------------
   -- Get_Package_To_Be_Withed --
   ------------------------------

   function Get_Package_To_Be_Withed
     (Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class) return String
   is
      Tree : constant Construct_Tree :=
        Get_Tree
          (Get_Structured_File (Current_Text, Get_File (Source_Position)));
      It   : Construct_Tree_Iterator :=
        Get_Iterator_At
          (Tree, (False, Get_Line (Source_Position), 1), Position => After);
   begin
      while not Is_Parent_Scope (Null_Construct_Tree_Iterator, It) loop
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
      File_Destination : GNATCOLL.VFS.Virtual_File;
      With_Could_Miss  : Boolean)
   is
   begin
      This.Mode := Add_Use;
      This.With_Could_Miss := With_Could_Miss;
      This.Source_Position := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Source_Position));
      This.File_Destination := File_Destination;
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
   begin
      This.Mode := Prefix;
      This.With_Could_Miss := With_Could_Miss;
      This.Source_Position := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Source_Position));
      This.Object_Position := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Object_Position));
      This.File_Destination := Get_File (Object_Position);
   end Prefix_Object;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Get_Visible_Declaration_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Source_Position : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Source_Position.all);
      Pkg_Name    : constant String := Get_Package_To_Be_Withed
        (Current_Text, Source_Position);
      With_Cursor : File_Cursor;
   begin
      With_Cursor := File_Cursor
        (Search_With
           (Current_Text, This.File_Destination, Pkg_Name));

      if This.Mode = Add_Use then
         if This.With_Could_Miss and then With_Cursor = Null_File_Cursor then
            Current_Text.Add_Line
              (Get_Next_With_Position (Current_Text, This.File_Destination),
               "with " & Pkg_Name & "; use " & Pkg_Name & ";");
         else
            Current_Text.Add_Line
              (With_Cursor,
               "use " & Pkg_Name & ";");
         end if;
      elsif This.Mode = Prefix then
         if This.With_Could_Miss and then With_Cursor = Null_File_Cursor then
            Current_Text.Add_Line
              (Get_Next_With_Position (Current_Text, This.File_Destination),
               "with " & Pkg_Name & ";");
         end if;

         declare
            Prefix : constant String := Get_Full_Prefix
              (Current_Text, Source_Position);
            Object_Position : constant File_Cursor'Class :=
              Current_Text.Get_Current_Cursor (This.Object_Position.all);
         begin
            if Prefix /= "" then
               Current_Text.Replace (Object_Position, 0, Prefix & ".");
            end if;
         end;

      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Get_Visible_Declaration_Cmd) is
   begin
      Free (This.Source_Position);
      Free (This.Object_Position);
      Free (This.Pkg_Name);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Get_Visible_Declaration_Cmd) return Boolean is
   begin
      return This.File_Destination.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Indent_Code_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Line_Cursor  : File_Cursor'Class;
      Force_Column : Visible_Column_Type)
   is
   begin
      This.Line := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Line_Cursor));
      This.Force_Column := Force_Column;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Indent_Code_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Char_Ind : String_Index_Type;
      Indent_Size : Integer := -1;
      Line_Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Line.all);
   begin
      if This.Force_Column = 0 then
         Current_Text.Get_File
           (Get_File (Line_Cursor)).Indent_Line (Line_Cursor);
      else
         Char_Ind := To_Char_Index
           (This.Force_Column, Get_Line (Current_Text, Line_Cursor, 1));
         Indent_Size := Natural (Char_Ind) - 1;

         declare
            Word : Word_Cursor;
            White_String : constant String (1 .. Indent_Size) :=
              (others => ' ');
         begin
            Set_File (Word, Get_File (Line_Cursor));
            Set_Location (Word, Get_Line (Line_Cursor), 1);
            Set_Word (Word, "(^[\s]*)", Regular_Expression);

            Current_Text.Replace
              (Word,
               Word.Get_Matching_Word (Current_Text)'Length,
               White_String);

            Free (Word);
         end;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Indent_Code_Cmd)
   is
   begin
      Free (Text_Command (This));
      Free (This.Line);
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Indent_Code_Cmd) return Boolean is
   begin
      return This.Line.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This           : in out Add_Clauses_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String;
      Add_With       : Boolean;
      Add_Use        : Boolean)
   is
      pragma Unreferenced (Current_Text);
   begin
      This.File := Cursor.File;
      This.Missing_Clause := new String'(Missing_Clause);
      This.Add_With := Add_With;
      This.Add_Use := Add_Use;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This         : Add_Clauses_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
   begin
      if This.Add_With and then This.Add_Use then
         Current_Text.Add_Line
           (Get_Next_With_Position (Current_Text, This.File),
            "with " & This.Missing_Clause.all & ";"
            & " use " & This.Missing_Clause.all & ";");
      elsif This.Add_Use then
         Current_Text.Add_Line
           (Get_Next_With_Position (Current_Text, This.File),
            "use " & This.Missing_Clause.all & ";");
      else
         Current_Text.Add_Line
           (Get_Next_With_Position (Current_Text, This.File),
            "with " & This.Missing_Clause.all & ";");
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding
   procedure Free (This : in out Add_Clauses_Cmd) is
   begin
      Free (This.Missing_Clause);
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Add_Clauses_Cmd) return Boolean is
   begin
      return This.File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This           : in out Change_To_Tick_Valid_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This         : Change_To_Tick_Valid_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        File_Cursor (Current_Text.Get_Current_Cursor (This.Location.all));
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
            To_Column_Index (String_Index_Type (Sloc_End.Column), Line));

         return False;
      end Entity_Callback;
   begin
      Begin_Cursor := File_Cursor (Cursor);
      End_Cursor := File_Cursor (Cursor);

      loop
         declare
            Line       : constant String :=
              Get_Line (Current_Text, Begin_Cursor, 1);
            Real_Begin : String_Index_Type :=
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
                     Visible_Column_Type
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
         Current_Text.Replace (Begin_Cursor, End_Cursor, "'Valid");
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding
   procedure Free (This : in out Change_To_Tick_Valid_Cmd) is
   begin
      Free (This.Location);
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Change_To_Tick_Valid_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This           : in out Remove_Extra_Underlines_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This         : Remove_Extra_Underlines_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor       : File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
      Line         : constant String := Get_Line (Current_Text, Cursor, 1);
      New_Id       : String (1 .. Line'Length);
      Index        : String_Index_Type :=
        To_Char_Index (Get_Column (Cursor), Line);
      New_Id_Index : Integer := 1;
      Start_Index  : String_Index_Type;
   begin
      while Index > String_Index_Type (Line'First)
        and then not Is_Separator (Line (Integer (Index)))
      loop
         Index := Index - 1;
      end loop;

      if Is_Blank (Line (Integer (Index))) then
         Index := Index + 1;
      end if;

      Start_Index := Index;

      while Index < String_Index_Type (Line'Last)
        and then not Is_Separator (Line (Integer (Index)))
      loop
         if Line (Integer (Index)) = '_' then
            New_Id (New_Id_Index) := Line (Integer (Index));
            Index := Index + 1;
            New_Id_Index := New_Id_Index + 1;

            while Index < String_Index_Type (Line'Last)
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

      Cursor.Col := To_Column_Index (Start_Index, Line);

      Current_Text.Replace
        (Cursor,
         Integer (Index - Start_Index),
         New_Id (1 .. New_Id_Index - 1));
   end Execute;

   ----------
   -- Free --
   ----------

   overriding
   procedure Free (This : in out Remove_Extra_Underlines_Cmd) is
   begin
      Free (This.Location);
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Extra_Underlines_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   -------------------------------
   -- Remove_Pragma_Element_Cmd --
   -------------------------------

   procedure Initialize
     (This         : in out Remove_Pragma_Element_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Element_Name : String;
      Pragma_Name  : String)
   is
   begin
      This.Element_Name := new String'(To_Lower (Element_Name));
      This.Pragma_Name := new String'(To_Lower (Pragma_Name));
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding
   procedure Execute
     (This         : Remove_Pragma_Element_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);

      Pragma_Cursor : File_Cursor;
      Tree : constant Construct_Tree :=
        Get_Tree (Current_Text.Get_Structured_File (Cursor.File));
      It   : Construct_Tree_Iterator := Current_Text.Get_Iterator_At
        (Cursor            => Cursor,
         Position          => Before);

      Name : Normalized_Symbol;
   begin
      Pragma_Cursor.Set_File (Cursor.Get_File);

      Name := Find_Normalized
        (Symbols => Get_Symbols (Get_Context (Current_Text).Entity_Db),
         Name    => This.Element_Name.all);

      while It /= Null_Construct_Tree_Iterator loop
         if Get_Construct (It).Category = Cat_Pragma
           and then To_Lower (Get (Get_Construct (It).Name).all)
           = This.Pragma_Name.all
         then
            --  We're on a pragma of the proper name - see if there's the
            --  element that we're looking for here and if we can delete it.

            Pragma_Cursor.Set_Location (Get_Construct (It).Sloc_Start.Line, 1);
            Pragma_Cursor.Set_Column
              (To_Column_Index
                 (String_Index_Type (Get_Construct (It).Sloc_Start.Column),
                  Current_Text.Get_Line (Pragma_Cursor, 0)));

            declare
               List : Ada_Statement;
               Loc  : aliased Universal_Location;
            begin
               Loc := To_Location
                 (File   => Current_Text.Get_Structured_File (Cursor.File),
                  Line   => Pragma_Cursor.Line,
                  Column => Pragma_Cursor.Col);
               Refactoring.Services.Initialize
                 (Self     => List,
                  Context  => Get_Context (Current_Text),
                  Location => Loc);

               if Contains_Element (List, Name) then
                  Remove_Element (List, Erase, Name);
                  Free (List);

                  return;
               end if;

               Free (List);
            end;
         end if;

         It := Prev (Tree, It, Jump_Over);
      end loop;
   end Execute;

   overriding
   procedure Free (This : in out Remove_Pragma_Element_Cmd) is
   begin
      Free (This.Location);
      Free (This.Element_Name);
      Free (This.Pragma_Name);
   end Free;

   overriding
   function Is_Writable (This : Remove_Pragma_Element_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------------------
   -- Remove_Parenthesis_Cmd --
   ----------------------------

   procedure Initialize
     (This            : in out Remove_Parenthesis_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Cursor          : File_Cursor'Class)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding procedure Execute
     (This         : Remove_Parenthesis_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Open_Cursor : File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
      Close_Cursor : File_Cursor;
   begin
      Close_Cursor := Get_Closing_Paren (Current_Text, Open_Cursor);
      Current_Text.Replace (Close_Cursor, 1, "");
      Current_Text.Replace (Open_Cursor, 1, "");

      Free (Open_Cursor);
      Free (Close_Cursor);
   end Execute;

   overriding procedure Free (This : in out Remove_Parenthesis_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Remove_Parenthesis_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   --------------------------
   -- Fix_Index_Number_Cmd --
   --------------------------

   procedure Initialize
     (This         : in out Fix_Index_Number_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Mode         : Fix_Index_Number_Cmd_Mode)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
      This.Mode := Mode;
   end Initialize;

   overriding procedure Execute
     (This         : Fix_Index_Number_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
   begin
      if This.Mode = Remove then
         declare
            Open_Paren : File_Cursor'Class :=
              Current_Text.Get_File (Cursor.File).Search_Token
              (Cursor, Open_Paren_Tok, Reverse_Step);
            Close_Paren : File_Cursor'Class :=
              Get_Closing_Paren (Current_Text, Open_Paren);
         begin
            Current_Text.Replace (Open_Paren, Close_Paren, "", One, One);
            Free (Close_Paren);
            Free (Open_Paren);
         end;
      else
         declare
            End_Word : File_Cursor'Class := Clone (Cursor);
            Word     : Word_Cursor;
         begin
            Current_Text.Next_Word (End_Word, Word);

            if Word.Get_Word = "'" then
               --  In this case, we just retreived the ', we need to jump over
               --  the attribute name as well.

               Free (Word);
               Current_Text.Next_Word (End_Word, Word);
            end if;

            declare
               Ins_Cursor : File_Cursor'Class := Clone (File_Cursor (Word));
            begin
               Ins_Cursor.Col := Ins_Cursor.Col
                 + Word.Get_Matching_Word (Current_Text)'Length;
               Current_Text.Replace (Ins_Cursor, 0, " (1)");
               Free (Ins_Cursor);
            end;

            Free (Word);
         end;
      end if;

      Free (Cursor);
   end Execute;

   overriding procedure Free (This : in out Fix_Index_Number_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Fix_Index_Number_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------------------
   -- Reorder_Subprogram_Cmd --
   ----------------------------

   procedure Initialize
     (This         : in out Reorder_Subprogram_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding procedure Execute
     (This         : Reorder_Subprogram_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);

      S_File : constant Structured_File_Access :=
        Current_Text.Get_Structured_File (Cursor.File);
      Tree   : constant Construct_Tree := Get_Tree (S_File);

      Sb_It : constant Construct_Tree_Iterator :=
        Current_Text.Get_Iterator_At (Cursor);
      Prev_Entity : Construct_Tree_Iterator := Null_Construct_Tree_Iterator;

      Prev_It : Construct_Tree_Iterator := Sb_It;

      Src_Begin_Cursor, Src_End_Cursor : File_Cursor;
      Dst_Cursor : File_Cursor;
   begin
      --  Look for the previous subprogram in alphabetical order, if none found
      --  then the missplace subprogram has to be placed at the beginning of
      --  the scope.

      while Prev_It /= Null_Construct_Tree_Iterator
        and then Is_Parent_Scope (Get_Parent_Scope (Tree, Sb_It), Prev_It)
      loop
         if Get_Construct (Prev_It).Category in Subprogram_Category then
            if To_Lower (Get (Get_Construct (Prev_It).Name).all) >
              To_Lower (Get (Get_Construct (Sb_It).Name).all)
            then
               Prev_Entity := Prev (Tree, Prev_It, Jump_Over);

               if not Is_Parent_Scope
                 (Get_Parent_Scope (Tree, Sb_It), Prev_Entity)
               then
                  --  If we're out of the parent scope, then this is not
                  --  the previous entity.

                  Prev_Entity := Null_Construct_Tree_Iterator;
               end if;
            end if;
         end if;

         Prev_It := Prev (Tree, Prev_It, Jump_Over);
      end loop;

      --  Set the cursors for the text move.

      Src_End_Cursor.File := Cursor.File;
      Src_Begin_Cursor.File := Cursor.File;
      Dst_Cursor.File := Cursor.File;

      Src_Begin_Cursor.Col := 1;
      Src_Begin_Cursor.Line := Get_Construct (Sb_It).Sloc_Start.Line;

      Src_End_Cursor.Line := Get_Construct (Sb_It).Sloc_End.Line;

      declare
         End_Line : constant String :=
           Current_Text.Get_Line (Src_End_Cursor, 0);
      begin
         Src_End_Cursor.Col := To_Column_Index (End_Line'Length, End_Line);
      end;

      if Prev_Entity = Null_Construct_Tree_Iterator then
         --  If the subprogram has to be first, then place it right after the
         --  start of the enclosing entity.

         Prev_Entity := Get_Parent_Scope (Tree, Sb_It);

         Dst_Cursor.Line := Get_Construct (Prev_Entity).Sloc_Start.Line;

         declare
            End_Line : constant String :=
              Current_Text.Get_Line (Dst_Cursor, 0);
         begin
            Dst_Cursor.Col := To_Column_Index (End_Line'Length, End_Line) + 1;
         end;
      else
         --  If there is a subprogram before, place it after the end of that
         --  subprogram.

         Dst_Cursor.Line := Get_Construct (Prev_Entity).Sloc_End.Line;

         declare
            End_Line : constant String :=
              Current_Text.Get_Line (Dst_Cursor, 0);
         begin
            Dst_Cursor.Col := To_Column_Index (End_Line'Length, End_Line) + 1;
         end;
      end if;

      declare
         Sb_Text : constant String :=
           EOL_Str & EOL_Str & Current_Text.Get
             (Src_Begin_Cursor, Src_End_Cursor);
      begin
         --  Remove the current subprogram

         Current_Text.Replace (Src_Begin_Cursor, Src_End_Cursor, "");

         --  Remove potential extra blank lines introduced by the removal

         Remove_Blank_Lines (Current_Text, Src_Begin_Cursor);

         --  Add the subprogram at the appropriate location

         Current_Text.Replace (Dst_Cursor, 0, Sb_Text);
      end;
   end Execute;

   overriding procedure Free (This : in out Reorder_Subprogram_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Reorder_Subprogram_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   --------------------------
   -- Remove_Attribute_Cmd --
   --------------------------

   procedure Initialize
     (This         : in out Remove_Attribute_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding procedure Execute
     (This         : Remove_Attribute_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
      Start  : File_Cursor'Class := Clone (Cursor);
      Word   : Word_Cursor;
   begin
      Current_Text.Next_Word (Cursor, Word);

      if Word.Get_Matching_Word (Current_Text) = "'" then
         Free (Word);
         Current_Text.Next_Word (Cursor, Word);
      end if;

      Current_Text.Replace (Start, Word, "");

      Free (Cursor);
      Free (Word);
   end Execute;

   overriding procedure Free (This : in out Remove_Attribute_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Remove_Attribute_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   -------------------------
   -- Renames_To_Constant --
   -------------------------

   procedure Initialize
     (This         : in out Renames_To_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding procedure Execute
     (This         : Renames_To_Constant_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
      Semicol_Cursor : File_Cursor'Class :=
        Current_Text.Search_Token
          (Cursor, Semicolon_Tok, Reverse_Step);
   begin
      Current_Text.Replace (Semicol_Cursor, Semicol_Cursor, ": constant");

      declare
         Renames_Cursor : File_Cursor'Class :=
           Current_Text.Search_Token (Semicol_Cursor, Renames_Tok);
      begin
         Current_Text.Replace (Renames_Cursor, Renames_Cursor, ":=");
         Free (Renames_Cursor);
      end;

      Free (Semicol_Cursor);
   end Execute;

   overriding procedure Free (This : in out Renames_To_Constant_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Renames_To_Constant_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ---------------------------
   -- Remove_Comparison_Cmd --
   ---------------------------

   procedure Initialize
     (This         : in out Remove_Comparison_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
   end Initialize;

   overriding procedure Execute
     (This         : Remove_Comparison_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : constant File_Cursor'Class :=
        Current_Text.Get_Current_Cursor (This.Location.all);
      Comp_Cursor : File_Cursor'Class :=
        Current_Text.Search_Tokens
          (Cursor, (Equals_Tok, Not_Equals_Tok), Reverse_Step);
      True_Cursor : File_Cursor'Class :=
           Current_Text.Search_Token (Comp_Cursor, True_Tok);
   begin
      Current_Text.Replace (Comp_Cursor, True_Cursor, "", One, One);

      Free (Comp_Cursor);
      Free (True_Cursor);
   end Execute;

   overriding procedure Free (This : in out Remove_Comparison_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Remove_Comparison_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

end Codefix.Text_Manager.Ada_Commands;

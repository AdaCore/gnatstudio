------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with Ada.Characters.Handling;       use Ada.Characters.Handling;
with GNATCOLL.Symbols;              use GNATCOLL.Symbols;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Utils;                use GNATCOLL.Utils;
with GNATCOLL.Xref;
with Case_Handling;                 use Case_Handling;
with Codefix.Ada_Tools;             use Codefix.Ada_Tools;
with Language.Ada;                  use Language.Ada;
with Codefix.Text_Manager.Commands; use Codefix.Text_Manager.Commands;

package body Codefix.Text_Manager.Ada_Commands is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("GPS.CODEFIX.CODEFIX");

   function Get_Beginning_Of_Name
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return File_Cursor;
   --  Cursor points to any character inside the single name or expanded name
   --  of an entity. For single names return the location of the beginning of
   --  the name located at Cursor. For expanded names return the location of
   --  the beginning of the full expanded name.

   function Get_Closing_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Open_Cursor  : File_Cursor'Class) return File_Cursor;
   --  Returns the Closing parenthesis corresponding to the open one given
   --  in parameter

   function Get_End_Of_Preceding_Token
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return File_Cursor;
   --  Displace Cursor to the first character after the preceding word

   function To_Case (Text : String; With_Casing : Casing_Type) return String;
   --  Return Text after re-cased according to the specified casing type

   function To_Reserved_Word_Case (Word : String) return String;
   --  Return the Word after re-cased according to the user-defined preferences
   --  (value set in Edit->Preferences->Editor->Ada->Reserved_Word_Casing)

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

   ---------------------------
   -- Get_Beginning_Of_Name --
   ---------------------------

   function Get_Beginning_Of_Name
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return File_Cursor
   is
      Begin_Cursor : File_Cursor := Null_File_Cursor;

      procedure Scan_Backward_Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean);
      --  Scan backward the expanded name. Updates Loc to reference the
      --  beginning of the expression.

      -----------------------------
      --  Scan_Backward_Callback --
      -----------------------------

      Last_Index : String_Index_Type := 0;

      procedure Scan_Backward_Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean)
      is
         pragma Unreferenced (Buffer);

      begin
         case Token.Tok_Type is
            when Tok_Blank =>
               null;

            when Tok_Dot | Tok_Identifier =>
               Last_Index := Token.Token_First;

            when others =>
               declare
                  Line   : Integer;
                  Column : Visible_Column_Type;
               begin
                  To_Line_Column
                    (File                 =>
                       Current_Text.Get_Structured_File (Cursor.File),
                     Absolute_Byte_Offset => Last_Index,
                     Line                 => Line,
                     Column               => Column);

                  Begin_Cursor.Set_File (Cursor.Get_File);
                  Begin_Cursor.Set_Line (Line);
                  Begin_Cursor.Set_Column (Column);

                  Stop := True;
               end;
         end case;
      end Scan_Backward_Callback;

   --  Start of Get_Beginning_Of_Name

   begin
      Parse_Entities_Backwards
        (Lang     => Ada_Lang,
         This     => Current_Text,
         Callback => Scan_Backward_Callback'Access,
         Start    => Cursor);

      return Begin_Cursor;
   end Get_Beginning_Of_Name;

   --------------------------------
   -- Get_End_Of_Preceding_Token --
   --------------------------------

   function Get_End_Of_Preceding_Token
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return File_Cursor
   is
      End_Cursor : File_Cursor := File_Cursor (Cursor);

   begin
      loop
         declare
            Full_Line  : constant String :=
                           Get_Line (Current_Text, End_Cursor, 1);
            J          : String_Index_Type :=
                           To_Char_Index
                             (Get_Column (End_Cursor), Full_Line) - 1;

         begin
            --  Skip previous blanks

            while J > 1
              and then Is_Blank (Full_Line (Natural (J)))
            loop
               J := J - 1;
            end loop;

            --  Found the end of the preceding word. Leave Begin_Cursor located
            --  in the first blank after the preceding word.

            if not Is_Blank (Full_Line (Natural (J))) then
               Set_Location
                 (End_Cursor,
                  Get_Line (End_Cursor),
                  To_Column_Index (J, Full_Line) + 1);

               exit;

            --  The beginning of this line is still a blank. Locate us at the
            --  end of the previous line and continue skiping blanks.

            elsif Get_Line (End_Cursor) /= 1 then
               Set_Location (End_Cursor, Get_Line (End_Cursor) - 1, 1);
               Set_Location
                 (End_Cursor,
                  Get_Line (End_Cursor),
                  Visible_Column_Type
                    (Get_Line (Current_Text, End_Cursor)'Last + 1));

            --  First line of this buffer. Stop searching for the preceding
            --  token. Set location to line 1, column 1.

            else
               Set_Location
                 (End_Cursor,
                  Get_Line (End_Cursor),
                  To_Column_Index (J, Full_Line));

               exit;
            end if;
         end;
      end loop;

      return End_Cursor;
   end Get_End_Of_Preceding_Token;

   -------------
   -- To_Case --
   -------------

   function To_Case
     (Text        : String;
      With_Casing : Casing_Type) return String
   is
      New_String  : String (Text'Range) := Text;

   begin
      case With_Casing is
         when Mixed | Smart_Mixed =>
            return Mixed_Case (Text);

         when Upper =>
            for J in Text'Range loop
               New_String (J) := To_Upper (Text (J));
            end loop;

         when Lower =>
            for J in Text'Range loop
               New_String (J) := To_Lower (Text (J));
            end loop;

         when Unchanged =>
            null;
      end case;

      return New_String;
   end To_Case;

   ---------------------------
   -- To_Reserved_Word_Case --
   ---------------------------

   function To_Reserved_Word_Case (Word : String) return String is
      Indent_Params : Indent_Parameters;
      Indent_Style  : Indentation_Kind;
      pragma Unreferenced (Indent_Style);

   begin
      Get_Indentation_Parameters (Ada_Lang, Indent_Params, Indent_Style);

      return To_Case (Word, Indent_Params.Reserved_Casing);
   end To_Reserved_Word_Case;

   --  Recase_Word_Cmd

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : Unbounded_String := Null_Unbounded_String;
      Word_Case    : Case_Type := Mixed) is
   begin
      This.Cursor := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Cursor));
      This.Correct_Word := Correct_Word;
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
               return Mixed_Case (Str);

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
           String_Match => To_Unbounded_String ("([\w]+)"),
           Mode => Regular_Expression);

      declare
         Miscased_Word : constant String :=
           Word.Get_Matching_Word (Current_Text);
      begin
         if This.Correct_Word /= Null_Unbounded_String then
            Current_Text.Replace
              (Cursor,
               Miscased_Word'Length,
               Slice
                 (This.Correct_Word,
                  Length (This.Correct_Word) - Miscased_Word'Length + 1,
                  Length (This.Correct_Word)));

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
        (Get_Or_Create (Db   => Get_Context (Current_Text).Db.Constructs,
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
   begin
      for Item of This.Remove_List loop
         Make_Word_Cursor (Item, Current_Text, Current_Cursor);

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
                 (Get_Context (Current_Text).Db.Symbols,
                  To_String (Current_Cursor.String_Match)));
         end;
      end loop;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Elements_Cmd) is
   begin
      This.Remove_List.Clear;
      Free (Text_Command (This));
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Remove_Elements_Cmd) return Boolean is
   begin
      for Item of This.Remove_List loop
         if not Item.Mark_Id.Get_File.Is_Writable then
            return False;
         end if;
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
      Look_For_Use : Boolean := True) is
   begin
      This.Word := new Mark_Abstr'Class'(Current_Text.Get_New_Mark (Word));
      This.Word_Str := To_Unbounded_String (Word.Get_Word);
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
      Clauses_List : Words_Lists.Vector;
      Last_With    : File_Cursor := Null_File_Cursor;

      Is_Instantiation  : Boolean;
      Instantiation_Pkg : Ada_Statement;
      Clauses_Pkg       : Ada_Statement;
      Obj_List          : String_List.Vector;
   begin
      Trace (Me, "Execute Remove_Pkg_Clauses_Cmd");

      File_Cursor (Word) :=
        File_Cursor (Current_Text.Get_Current_Cursor (This.Word.all));
      Word.String_Match := This.Word_Str;

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
         Word.String_Match := To_Unbounded_String (Get (Pkg_Info.Name).all);
      end if;

      if Pkg_Info.Category = Cat_Unknown then
         Pkg_Info := Search_Unit
           (Current_Text, Get_File (Word),
            Cat_Package,
            To_String (Word.String_Match));

         Initialize
           (Instantiation_Pkg,
            Get_Context (Current_Text),
            To_Location
              (Get_Or_Create
                 (Get_Context (Current_Text).Db.Constructs, Word.File),
               Word.Line,
               Word.Col));

         Is_Instantiation := True;
      else
         Initialize
           (Clauses_Pkg,
            Get_Context (Current_Text),
            To_Location
              (Get_Or_Create
                 (Get_Context (Current_Text).Db.Constructs, Word.File),
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
               To_Unbounded_String ("with " & Get (Pkg_Info.Name).all & ";"));
         end if;

         Is_Instantiation := False;
      end if;

      --  If the category is not already a use clause, see if there are use
      --  clauses for that unit and remove them as well.

      if This.Category /= Cat_Use then
         if This.Look_For_Use then
            Get_Use_Clauses
              (To_String (Word.String_Match),
               Get_File (Word),
               Current_Text,
               True,
               Clauses_List);
         else
            Get_Use_Duplicates_On_Line (Current_Text, Word, Clauses_List);
         end if;
      end if;

      for Item of Clauses_List loop
         declare
            Use_Pck : Ada_Statement;
         begin
            Initialize
              (Use_Pck,
               Get_Context (Current_Text),
               To_Location
                 (Get_Or_Create
                      (Get_Context (Current_Text).Db.Constructs, Word.File),
                  Item.Line,
                  Item.Col));

            Remove_Element
              (Self => Use_Pck,
               Mode => Erase,
               Name => Find_Normalized
                 (Symbols => Get_Context (Current_Text).Db.Symbols,
                  Name    => Item.Get_Word));

            Free (Use_Pck);
         end;

         if This.Destination /= GNATCOLL.VFS.No_File then
            Append  (Obj_List, "use " & Item.String_Match & ";");
         end if;
      end loop;

      Clauses_List.Clear;

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
           (Symbols => Get_Context (Current_Text).Db.Symbols,
            Name    => Word.Get_Word));

      if Last_With /= Null_File_Cursor then
         for Item of Obj_List loop
            Current_Text.Add_Line (Last_With, To_String (Item));
         end loop;

         Free (Last_With);
      end if;

      Free (Word);
      Free (Instantiation_Pkg);
      Free (Clauses_Pkg);
      Obj_List.Clear;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Remove_Pkg_Clauses_Cmd) is
   begin
      Free (This.Word);
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
      Name           : Unbounded_String;
      Argument       : Unbounded_String) is
   begin
      This.Name     := Name;
      This.Argument := Argument;
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
      procedure Add_Clause_Pragma;
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

      -----------------------
      -- Add_Clause_Pragma --
      -----------------------

      procedure Add_Clause_Pragma is
         Clause   : Construct_Tree_Iterator;
         Char_Ind : String_Index_Type;
      begin
         Clause := Get_Iterator_At
           (Current_Text,
            Cursor,
            Start_Construct,
            Categories_Seeked => (Cat_With, Cat_Use));

         Char_Ind := String_Index_Type
           (Get_Construct (Clause).Sloc_End.Column);

         if Char_Ind = 0 then
            Char_Ind := 1;
         end if;

         Set_File (Position, Get_File (Cursor));

         declare
            Line : constant String := Get_Line (Current_Text, Position);
         begin
            Set_Location
              (Position,
               Get_Construct (Clause).Sloc_End.Line,
               To_Column_Index (Char_Ind, Line));
         end;
      end Add_Clause_Pragma;

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

         when Cat_With | Cat_Use =>
            Add_Clause_Pragma;

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

            if To_Lower (Next_Str.Get_Word)
              = To_Lower (To_String (This.Name))
            then
               Pragma_Cursor := File_Cursor
                 (Search_Token (Current_Text, Pragma_Cursor, Close_Paren_Tok));
               Line_Cursor := Pragma_Cursor;
               Line_Cursor.Col := 1;
               Current_Text.Replace
                 (Pragma_Cursor, 1, ", " & To_String (This.Argument) & ")");
               Free (Line_Cursor);
            end if;

         else
            Current_Text.Add_Line
              (Position,
               "pragma " & To_String (This.Name)
               & " (" & To_String (This.Argument) & ");",
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
      Name         : Unbounded_String) is
   begin
      This.Position := new Mark_Abstr'Class'
        (Get_New_Mark (Current_Text, Position));
      This.Name := Name;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (This         : Make_Constant_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      function Skip_Colon_And_Aliased
        (Cursor : File_Cursor) return File_Cursor;
      --  Find ':' after Cursor. If next token is 'aliased' then skip it

      ----------------------------
      -- Skip_Colon_And_Aliased --
      ----------------------------

      function Skip_Colon_And_Aliased
        (Cursor : File_Cursor) return File_Cursor
      is
         Colon  : Word_Cursor'Class :=
           Current_Text.Search_Token (Cursor, Colon_Tok);
         Alias  : Word_Cursor'Class :=
           Current_Text.Search_Tokens (Cursor, (Semicolon_Tok, Aliased_Tok));
      begin
         if Alias.Get_Word in "" | ";" then
            Colon.Set_Column (Colon.Get_Column + 1);

            return File_Cursor (Colon);
         else
            Alias.Set_Column (Alias.Get_Column + 7);

            return File_Cursor (Alias);
         end if;
      end Skip_Colon_And_Aliased;

      New_Word      : constant String := To_Reserved_Word_Case ("constant");
      Cursor        : File_Cursor;
      Work_Extract  : Ada_Statement;
      New_Instr     : Unbounded_String;
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

      if Number_Of_Declarations (Work_Extract) > 1 then
         Extract_Element
           (Work_Extract,
            New_Instr,
            Find_Normalized
              (Get_Context (Current_Text).Db.Symbols, To_String (This.Name)));

         End_Decl := Get_End (Work_Extract);

         Cursor := File_Cursor'
           (Line => Get_Line (End_Decl'Access),
            Col  => Get_Column (End_Decl'Access),
            File => Get_File_Path (Get_File (End_Decl'Access)));

         Current_Text.Add_Line (Cursor, To_String (New_Instr), True);

         --  Skip to new added line
         Cursor.Set_Line (Cursor.Line + 1);
         Cursor.Set_Column (1);

         Free (New_Instr);
      end if;

      Current_Text.Replace
        (Position      => Skip_Colon_And_Aliased (Cursor),
         Len           => 0,
         New_Text      => " " & New_Word,
         Blanks_Before => Keep,
         Blanks_After  => One);

      Free (Work_Extract);
      Free (Cursor);
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Make_Constant_Cmd) is
   begin
      Free (This.Position);
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
      From_Cursor : File_Cursor;

   begin
      Text := Current_Text.Get_File (Cursor.File);

      Close_Paren := Get_Closing_Paren (Current_Text, Open_Paren);

      Current_Text.Replace (Close_Paren, 1, "");

      From_Cursor := Get_Beginning_Of_Name (Current_Text, Cursor);
      Text.Erase
        (From_Cursor, Current_Text.Search_Token (Cursor, Open_Paren_Tok));

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
                  elsif Equal (Name, "with", False) then
                     End_Of_Profile;
                     Is_Spec := True;
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

   -----------------
   -- Is_Writable --
   -----------------

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

         if It = Null_Construct_Tree_Iterator then
            return "";
         end if;
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

      case This.Mode is
         when Add_Use =>
            if This.With_Could_Miss
              and then With_Cursor = Null_File_Cursor
            then
               Current_Text.Add_Line
                 (Get_Next_With_Position (Current_Text, This.File_Destination),
                  "with " & Pkg_Name & "; use " & Pkg_Name & ";");
            else
               Current_Text.Add_Line
                 (With_Cursor,
                  "use " & Pkg_Name & ";");
            end if;

         when Prefix =>
            if This.With_Could_Miss
              and then With_Cursor = Null_File_Cursor
            then
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
      end case;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Get_Visible_Declaration_Cmd) is
   begin
      Free (This.Source_Position);
      Free (This.Object_Position);
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
            Set_Word
              (Word, To_Unbounded_String ("(^[\s]*)"), Regular_Expression);

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
      Missing_Clause : Unbounded_String;
      Add_With       : Boolean;
      Add_Use        : Boolean)
   is
      pragma Unreferenced (Current_Text);
   begin
      This.File := Cursor.File;
      This.Missing_Clause := Missing_Clause;
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
            "with " & To_String (This.Missing_Clause) & ";"
            & " use " & To_String (This.Missing_Clause) & ";");
      elsif This.Add_Use then
         Current_Text.Add_Line
           (Get_Next_With_Position (Current_Text, This.File),
            "use " & To_String (This.Missing_Clause) & ";");
      else
         Current_Text.Add_Line
           (Get_Next_With_Position (Current_Text, This.File),
            "with " & To_String (This.Missing_Clause) & ";");
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Add_Clauses_Cmd) is
   begin
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
     (This          : in out Add_Record_Rep_Clause_Cmd;
      Current_Text  : Text_Navigator_Abstr'Class;
      Cursor        : File_Cursor'Class;
      First_Clause  : Unbounded_String;
      Second_Clause : Unbounded_String := Null_Unbounded_String;
      With_Clause   : Unbounded_String := Null_Unbounded_String)
   is
   begin
      This.Location :=
        new Mark_Abstr'Class'(Current_Text.Get_New_Mark (Cursor));
      This.First_Clause := First_Clause;
      This.Second_Clause := Second_Clause;
      This.With_Clause := With_Clause;

      if With_Clause /= "" then
         This.File := Cursor.File;
      end if;
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This         : Add_Record_Rep_Clause_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Cursor : File_Cursor'Class :=
                 Current_Text.Get_Current_Cursor (This.Location.all);

      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean;
      --  Scan forward for the termination of the full record type declaration

      ---------------------------
      -- Scan_Forward_Callback --
      ---------------------------

      In_Header              : Boolean := True;
      In_Header_Word_Counter : Natural := 0;
      Record_Name            : String (1 .. 80);
      Record_Name_Len        : Natural := 0;

      In_Epilog              : Boolean := False;
      End_Record             : Boolean := False;

      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Name : constant String := Line (Sloc_Start.Column .. Sloc_End.Column);
         Stop_Scanning : Boolean := False;

      begin
         if In_Header then
            In_Header_Word_Counter := In_Header_Word_Counter + 1;

            --  Save the name of the record type

            if In_Header_Word_Counter = 2 then
               Record_Name (1 .. Name'Length) :=
                 Name (Name'First .. Name'Last);
               Record_Name_Len := Name'Length;

               In_Header := False;
            end if;
         end if;

         --  Scan searching for "end record"

         case Entity is
            when Keyword_Text =>
               if Name = "end" then
                  In_Epilog := True;

               elsif In_Epilog then
                  if Name = "record" then
                     End_Record := True;

                  --  We found the termination of a record variant

                  elsif Name = "case" then
                     In_Epilog := False;

                  else
                     pragma Assert (False);
                     null;
                  end if;
               end if;

            --  Stop on the location of the first semicolon after "end record"

            when Operator_Text =>
               Stop_Scanning := End_Record and then Name = ";";

            --  5. For any other token we continue scanning

            when others =>
               null;
         end case;

         --  Update cursor location

         if Stop_Scanning then
            Set_Location
              (This   => Cursor,
               Line   => Sloc_End.Line,
               Column => To_Column_Index
                           (String_Index_Type (Sloc_End.Column), Line));
         end if;

         return Stop_Scanning;
      end Scan_Forward_Callback;

   --  Start of processing for Add_Record_Rep_Clause_Cmd.Execute

   begin
      --  Scan forward searching for the end of the record type declaration. As
      --  a side effect we save locally the name of the record type to append
      --  it to the added record representation clause.

      Parse_Entities
        (Lang     => Ada_Lang,
         This     => Current_Text,
         Callback => Scan_Forward_Callback'Unrestricted_Access,
         Start    => Cursor);

      --  Append the record representation clauses

      if This.Second_Clause /= Null_Unbounded_String then
         Current_Text.Add_Line
           (Cursor   => Cursor,
            Indent   => True,
            New_Line => "for "
                          & Record_Name (1 .. Record_Name_Len)
                          & To_String (This.Second_Clause));
      end if;

      Current_Text.Add_Line
        (Cursor   => Cursor,
         Indent   => True,
         New_Line => "for "
                       & Record_Name (1 .. Record_Name_Len)
                       & To_String (This.First_Clause));

      if This.With_Clause /= Null_Unbounded_String then
         if File_Cursor (Search_With (Current_Text, This.File, "System"))
           = Null_File_Cursor
         then
            Current_Text.Add_Line
              (Get_Next_With_Position (Current_Text, This.File),
               "with " & To_String (This.With_Clause) & ";");
         end if;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Add_Record_Rep_Clause_Cmd) is
   begin
      Free (This.Location);
   end Free;

   -----------------
   -- Is_Writable --
   -----------------

   overriding
   function Is_Writable (This : Add_Record_Rep_Clause_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
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
                 Current_Text.Get_Current_Cursor (This.Location.all);

      Replace_From      : File_Cursor;
      Replace_To        : File_Cursor;
      Begin_Expr_Cursor : File_Cursor := Null_File_Cursor;

      Has_Negation : Boolean := False;

      procedure Scan_Backward_Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean);
      --  Scan backward the expression located before the keyword "not".
      --  Updates Not_Cursor to reference the beginning of the expression.

      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean;
      --  Scan forward the expression. If the expression starts with "not" then
      --  Has_Negation is set to True. Updates Replace_To to reference the end
      --  of the expression.

      -----------------------------
      --  Scan_Backward_Callback --
      -----------------------------

      Last_Index  : String_Index_Type := 0;
      Paren_Depth : Natural := 0;
      --   Level of nested parenthesis

      procedure Scan_Backward_Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean)
      is
         pragma Unreferenced (Buffer);

      begin
         --  Process tokens found between parenthesis. When we find a left
         --  parenthesis we skip all the enclosing tokens until we locate the
         --  corresponding right parenthesis; this is safe because we know that
         --  the expression is well formed (otherwise the warning suggesting
         --  the replacement would not have been reported by the compiler).

         if Paren_Depth > 0 then
            if Token.Tok_Type = Tok_Close_Parenthesis then
               Paren_Depth := Paren_Depth + 1;

            elsif Token.Tok_Type = Tok_Open_Parenthesis then
               Paren_Depth := Paren_Depth - 1;
               Last_Index := Token.Token_First;
            end if;

         --   Process tokens found without enclosing parenthesis

         else
            case Token.Tok_Type is
               when Tok_Blank =>
                  null;

               when Tok_Close_Parenthesis =>
                  Paren_Depth := Paren_Depth + 1;

               when Tok_Dot | Tok_Identifier | Tok_Tick =>
                  Last_Index := Token.Token_First;

               when others =>
                  declare
                     Line   : Integer;
                     Column : Visible_Column_Type;
                  begin
                     To_Line_Column
                       (File                 =>
                          Current_Text.Get_Structured_File (Cursor.File),
                        Absolute_Byte_Offset => Last_Index,
                        Line                 => Line,
                        Column               => Column);

                     Begin_Expr_Cursor.Set_File (Cursor.Get_File);
                     Begin_Expr_Cursor.Set_Line (Line);
                     Begin_Expr_Cursor.Set_Column (Column);

                     Stop := True;
                  end;
            end case;
         end if;
      end Scan_Backward_Callback;

      ---------------------------
      -- Scan_Forward_Callback --
      ---------------------------

      Depth      : Natural := 0;
      --  Parenthesis nesting level

      In_Header  : Boolean := True;
      --  True if processing the header of the expression: "[not] in"

      End_Line   : Natural;
      End_Column : Visible_Column_Type;
      --  Line and column of the last character of the scanned expression

      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean
      is
         pragma Unreferenced (Partial_Entity);

         Name : constant String := Line (Sloc_Start.Column .. Sloc_End.Column);
         Stop_Scanning : Boolean := False;

      begin
         --  Process the header of the expression

         if In_Header then
            pragma Assert (Entity = Keyword_Text);

            if To_Lower (Name) = "not" then
               Has_Negation := True;
               return False;  --  skip "not"

            elsif To_Lower (Name) = "in" then
               In_Header := False;
               return False;  --  skip "in"

            else pragma Assert (False);
               raise Program_Error;
            end if;
         end if;

         --  When we find a left parenthesis we skip all the enclosing tokens
         --  until we locate the corresponding right parenthesis; this is safe
         --  because we know that the expression is well formed (otherwise the
         --  warning suggesting the replacement would not have been reported by
         --  the compiler). Depth handles nested parenthesis.

         --  Process tokens found between parenthesis

         if Depth > 0 then
            if Entity = Operator_Text then
               if Name = "(" then
                  Depth := Depth + 1;

               elsif Name = ")" then
                  Depth := Depth - 1;
               end if;
            end if;

         --  Process tokens found without enclosing parenthesis

         else
            case Entity is

               --  1. Keywords stop the scanner

               when Keyword_Text =>
                  Stop_Scanning := True;

               when Operator_Text =>

                  --  2. Left parenthesis. Increase the parenthesis level to
                  --     start skipping all the tokens until we find its right
                  --     parenthesis

                  if Name = "(" then
                     Depth := Depth + 1;

                  --  3. An extra right parenthesis stops scanning since it
                  --     means that the expression that we are processing is
                  --     enclosed between parenthesis (and hence we are located
                  --     at the end of this expression)

                  elsif Name = ")" then
                     Stop_Scanning := True;

                  --  4. Tokens that cannot be part of the supported expression
                  --     stop the scanner

                  elsif Name /= "."
                    and then Name /= "'"
                    and then Name /= ".."
                  then
                     Stop_Scanning := True;
                  end if;

               --  5. For any other token we continue scanning

               when others =>
                  null;
            end case;

            if Stop_Scanning then
               Set_Location (Replace_To, End_Line, End_Column);
               return True;
            end if;
         end if;

         --  Update the known location of the end of the expression and
         --  continue scanning

         End_Line   := Sloc_End.Line;
         End_Column := To_Column_Index
                         (String_Index_Type (Sloc_End.Column), Line);

         return False;
      end Scan_Forward_Callback;

      --  Start of processing for Change_To_Tick_Valid_Cmd.Execute

   begin
      --  Context: When the expression found in the Ada sources has the form
      --  "expr in ..." the warning reported by the compiler points to the
      --  location of the keyword "in"; when the expression found in the
      --  Ada sources has the form "expr not in ..." the warning reported
      --  by the compiler points to the location of keyword "not" (see
      --  exp_ch4.Expand_N_In).

      --  Examples:
      --    V in T
      --    V in T'Range
      --    V in T'First .. T'Last
      --
      --  Unsupported case:
      --    V not in ...

      --  Step 1: Locate the beginning of the expression to be replaced

      Replace_From := Get_End_Of_Preceding_Token (Current_Text, Cursor);

      --  Step 2: Locate the end of the expression to be replaced. As a side
      --  effect of scanning we identify if the removed expression starts with
      --  "not".

      Parse_Entities
        (Lang     => Ada_Lang,
         This     => Current_Text,
         Callback => Scan_Forward_Callback'Unrestricted_Access,
         Start    => Replace_From);

      --  Step 3: Replace the selected text

      Current_Text.Replace (Replace_From, Replace_To, "'Valid");

      --  Step 4: If the removed expression started with a negation (ie. the
      --  expression starts with "not in") then scan the expression backward
      --  and insert "not" at the beginning of the expression. As a result we
      --  replace an expression like "x not in t'range" by "not x'valid"

      if Has_Negation then
         Parse_Entities_Backwards
           (Lang     => Ada_Lang,
            This     => Current_Text,
            Callback => Scan_Backward_Callback'Access,
            Start    => Replace_From);
         Current_Text.Replace (Begin_Expr_Cursor, 0, "not ");
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Remove_Pragma_Element_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Element_Name : String;
      Pragma_Name  : String)
   is
   begin
      This.Element_Name := To_Unbounded_String (To_Lower (Element_Name));
      This.Pragma_Name := To_Unbounded_String (To_Lower (Pragma_Name));
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
        (Symbols => Get_Context (Current_Text).Db.Symbols,
         Name    => To_String (This.Element_Name));

      while It /= Null_Construct_Tree_Iterator loop
         if Get_Construct (It).Category = Cat_Pragma
           and then To_Lower (Get (Get_Construct (It).Name).all)
           = This.Pragma_Name
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

   overriding procedure Free (This : in out Remove_Pragma_Element_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Remove_Pragma_Element_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

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

   ----------------
   -- Initialize --
   ----------------

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

   ----------------
   -- Initialize --
   ----------------

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Replace_Attribute_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Replace_By   : String) is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
      This.Replace_By := To_Unbounded_String (Replace_By);
   end Initialize;

   overriding procedure Execute
     (This         : Replace_Attribute_Cmd;
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

      Current_Text.Replace (Start, Word, To_String (This.Replace_By));

      Free (Cursor);
      Free (Word);
   end Execute;

   overriding procedure Free (This : in out Replace_Attribute_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Replace_Attribute_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

   ----------------
   -- Initialize --
   ----------------

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
          (Cursor, Colon_Tok, Reverse_Step);
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

   ----------------
   -- Initialize --
   ----------------

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
      Comp_Cursor : File_Cursor := Null_File_Cursor;

      Not_Cursor : File_Cursor := Null_File_Cursor;
      Paren_Depth : Integer := 0;
      Last_Index  : String_Index_Type := 0;

      procedure Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean);

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Buffer : Unbounded_String;
         Token  : Language.Token_Record;
         Stop   : in out Boolean)
      is
         Line   : Integer;
         Column : Visible_Column_Type;
      begin
         if Comp_Cursor = Null_File_Cursor then
            declare
               Str : constant String :=
                 Slice
                   (Buffer,
                    Integer (Token.Token_First),
                    Integer (Token.Token_Last));

            begin
               if Str = "=" or else Str = "/=" then
                  To_Line_Column
                    (File                 =>
                       Current_Text.Get_Structured_File (Cursor.File),
                     Absolute_Byte_Offset => Token.Token_First,
                     Line                 => Line,
                     Column               => Column);

                  Comp_Cursor.Set_File (Cursor.Get_File);
                  Comp_Cursor.Set_Line (Line);
                  Comp_Cursor.Set_Column (Column);

                  Stop := Str = "=";
               end if;
            end;
         else
            if Token.Tok_Type = Tok_Close_Parenthesis then
               Paren_Depth := Paren_Depth + 1;
            elsif Token.Tok_Type = Tok_Open_Parenthesis then
               Paren_Depth := Paren_Depth - 1;
               Last_Index := Token.Token_First;
            elsif Paren_Depth = 0 then
               case Token.Tok_Type is
                  when Tok_Dot | Tok_Blank | Tok_Identifier | Tok_Tick =>
                     Last_Index := Token.Token_First;

                  when others =>
                     To_Line_Column
                       (File                 =>
                        Current_Text.Get_Structured_File (Cursor.File),
                        Absolute_Byte_Offset => Last_Index,
                        Line                 => Line,
                        Column               => Column);

                     Not_Cursor.Set_File (Cursor.Get_File);
                     Not_Cursor.Set_Line (Line);
                     Not_Cursor.Set_Column (Column);

                     Stop := True;
               end case;
            end if;
         end if;
      end Callback;
   begin
      Parse_Entities_Backwards
        (Ada_Lang, Current_Text, Callback'Access, Cursor);

      declare
         True_Cursor : File_Cursor'Class :=
           Current_Text.Search_Token (Comp_Cursor, True_Tok);
      begin
         Current_Text.Replace
           (Comp_Cursor, True_Cursor, "", One, Keep);

         if Not_Cursor /= Null_File_Cursor then
            Current_Text.Replace
              (Not_Cursor,
               0,
               " not");
         end if;

         Free (Comp_Cursor);
         Free (True_Cursor);
         Free (Not_Cursor);
      end;
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : in out Named_Association_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : String)
   is
   begin
      This.Location := new Mark_Abstr'Class'
        (Current_Text.Get_New_Mark (Cursor));
      This.Name := To_Unbounded_String (Name);
   end Initialize;

   overriding procedure Execute
     (This         : Named_Association_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean;

      Name_Found  : Boolean := False;
      Name_Cursor : File_Cursor;
      Name_Length : Natural;

      function Scan_Forward_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean;
         Line           : String) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
      begin
         case Entity is
            when Identifier_Text =>
               Name_Cursor.Set_Location
                 (Sloc_Start.Line,
                  To_Column_Index
                    (String_Index_Type (Sloc_Start.Column), Line));

               Name_Length := Sloc_End.Column - Sloc_Start.Column + 1;

               return False;  --  Continue scanning
            when Operator_Text =>
               --  We found name in named_association when '=>' appear
               declare
                  Name : constant String :=
                    Line (Sloc_Start.Column .. Sloc_End.Column);
               begin
                  Name_Found := Name = "=>";
                  return True;  --  Stop scanning
               end;
            when others =>
               return True;  --  Stop scanning
         end case;
      end Scan_Forward_Callback;

      Cursor : constant File_Cursor'Class :=
                 Current_Text.Get_Current_Cursor (This.Location.all);
   begin
      --  Scan forward searching for 'name' and '=>' tokens. As a side effect
      --  we save the name location and length.

      Parse_Entities
        (Lang     => Ada_Lang,
         This     => Current_Text,
         Callback => Scan_Forward_Callback'Unrestricted_Access,
         Start    => Cursor);

      if Name_Found then
         Name_Cursor.Set_File (Cursor.Get_File);

         Current_Text.Replace
           (Name_Cursor,
            Name_Length,
            To_String (This.Name));
      else
         Current_Text.Replace
           (Position      => Cursor,
            Len           => 0,
            New_Text      => To_String (This.Name) & " =>",
            Blanks_Before => None,
            Blanks_After  => One);
      end if;
   end Execute;

   overriding procedure Free (This : in out Named_Association_Cmd) is
   begin
      Free (This.Location);
   end Free;

   overriding
   function Is_Writable (This : Named_Association_Cmd) return Boolean is
   begin
      return This.Location.Get_File.Is_Writable;
   end Is_Writable;

end Codefix.Text_Manager.Ada_Commands;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2003                      --
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

with Ada.Exceptions;                    use Ada.Exceptions;

with GNAT.Regpat;                       use GNAT.Regpat;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with String_Utils;                      use String_Utils;
with Language;                          use Language;
with Glide_Kernel;                      use Glide_Kernel;

with Codefix.Text_Manager.Ada_Commands; use Codefix.Text_Manager.Ada_Commands;
with Codefix.Ada_Tools;                 use Codefix.Ada_Tools;
with VFS;                               use VFS;

package body Codefix.Formal_Errors is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This       : in out Error_Message;
      Kernel     : access Kernel_Handle_Record'Class;
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
         Line, Col : Integer;
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
               Kernel));

         if Matches (Line_Index) /= No_Match then
            Line := Integer'Value
              (Error_Line
                 (Matches (Line_Index).First .. Matches (Line_Index).Last));
         else
            Line := 1;
         end if;

         if Matches (Col_Index) /= No_Match then
            Col := Integer'Value
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
      when Constraint_Error =>
         Free (This);
         This := Invalid_Error_Message;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This      : in out Error_Message;
      File      : VFS.Virtual_File;
      Line, Col : Positive;
      Message   : String) is
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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Solution_List) is
   begin
      Free (This, True);
   end Free;

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
      loop
         Match (Matcher, Get_Line (Current_Text, Second_Cursor), Matches);
         exit when Matches (1) /= No_Match;
         Line := Line - 1;
      end loop;

      Set_Location (Second_Cursor, Line, Matches (1).First);

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
      Column_Expected : Natural := 0) return Solution_List
   is
      function Closest (Size_Red : Positive) return Positive;
      --  Return the closest indentation modulo Indentation_Width.

      function Closest (Size_Red : Positive) return Positive is
      begin
         case (Size_Red - 1) mod Indentation_Width is
            when 0 =>
               return Size_Red + Indentation_Width;
               --  not - Identation_Width because of the case where
               --  Size_Red = 1
            when 1 =>
               return Size_Red - 1;
            when 2 =>
               return Size_Red + 1;
            when others =>
               Raise_Exception
                 (Codefix_Panic'Identity,
                  "Indentation_With changed, please update Wrong_Column.");
         end case;
      end Closest;

      New_Command   : Replace_Word_Cmd;
      Result        : Solution_List;
      Column_Chosen : Natural;
      Word          : Word_Cursor;

   begin
      --  ??? Should use "indent" shell command
      if Column_Expected = 0 then
         Column_Chosen := Closest (Get_Column (Message));
      else
         Column_Chosen := Column_Expected;
      end if;

      Set_File (Word, Get_File (Message));
      Set_Location (Word, Get_Line (Message), 1);
      Set_Word (Word, "(^[\s]*)", Regular_Expression);

      declare
         White_String : constant String (1 .. Column_Chosen - 1) :=
           (others => ' ');
      begin
         Initialize
           (New_Command,
            Current_Text,
            Word,
            White_String);
      end;

      Set_Caption
        (New_Command,
         "Move begin of instruction to column " & Image (Column_Chosen));
      Append (Result, New_Command);
      Free (Word);

      return Result;
   end Wrong_Column;

   -------------------------
   -- With_Clause_Missing --
   -------------------------

   function With_Clause_Missing
     (Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : String) return Solution_List
   is
      Word_With   : Word_Cursor;
      New_Command : Insert_Word_Cmd;
      Result      : Solution_List;
   begin
      Set_File (Word_With, Get_File (Cursor));
      Set_Location (Word_With, 0, 1);
      Set_Word (Word_With,
                "with " & Missing_Clause & "; use " & Missing_Clause & ";",
                Text_Ascii);

      Initialize
        (New_Command, Current_Text, Word_With, File_Cursor (Word_With));
      Set_Caption
        (New_Command,
         "Add with and use clause for package """ & Missing_Clause &
         """ at the begining of the file");
      Append (Result, New_Command);
      Free (Word_With);

      return Result;
   end With_Clause_Missing;

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
      Name         : String) return Solution_List
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
         Declaration  : Construct_Information;
         Col          : Natural;
      begin
         Declaration := Get_Unit (Current_Text, Cursor);
         Col := Declaration.Sloc_End.Column;

         --  ??? This test is only here because the parser returns sometimes
         --  a Sloc_End.Col equal to 0.

         if Col = 0 then
            Col := 1;
         end if;

         Set_File (New_Position, Get_File (Cursor));
         Set_Location (New_Position, Declaration.Sloc_End.Line, Col);

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
         Declaration  : Construct_Information;
         Col : Natural;
      begin
         Declaration := Get_Unit (Current_Text, Cursor, Before, Cat_Type);
         Col := Declaration.Sloc_End.Column;

         if Col = 0 then
            Col := 1;
         end if;

         Set_File (New_Position, Get_File (Cursor));
         Set_Location (New_Position, Declaration.Sloc_End.Line, Col);

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
         Declaration           : Construct_Information;
      begin
         Declaration := Get_Unit
           (Current_Text, Cursor, Before, Cat_Procedure, Cat_Function);

         Set_File (New_Position, Get_File (Cursor));
         Set_Location (New_Position, Declaration.Sloc_Entity.Line,
                       Declaration.Sloc_Entity.Column);

         Garbage := New_Position;
         New_Position := File_Cursor
           (Search_String
              (Current_Text, New_Position, ")", Std_Ada_Escape));
         Free (Garbage);

         Garbage := New_Position;
         New_Position := File_Cursor
           (Search_String
              (Current_Text, New_Position, "is", Std_Ada_Escape));
         Free (Garbage);

         Initialize
           (New_Command, Current_Text, New_Position, "Unreferenced", Name);

         Free (New_Position);

         return New_Command;
      end Add_Parameter_Pragma;

      --  begin of Not_Referenced

      Result : Solution_List;

   begin
      case Category is
         when Cat_Variable =>
            declare
               New_Command : Remove_Elements_Cmd;
               Var_Cursor  : Word_Cursor;
            begin
               Set_File (Var_Cursor, Get_File (Cursor));
               Set_Location
                 (Var_Cursor, Get_Line (Cursor), Get_Column (Cursor));
               Set_Word (Var_Cursor, Name, Text_Ascii);

               Add_To_Remove (New_Command, Current_Text, Var_Cursor);
               Set_Caption (New_Command, "Delete """ & Name & """");
               Append (Result, New_Command);
            end;

         when Cat_Function | Cat_Procedure =>
            declare
               Delete_Command : Remove_Entity_Cmd;
               Pragma_Command : Add_Pragma_Cmd;
            begin
               Initialize (Delete_Command, Current_Text, Cursor);
               Set_Caption
                 (Delete_Command,
                  "Delete subprogram """ & Name & """");
               Append (Result, Delete_Command);

               Pragma_Command := Add_Pragma;
               Set_Caption
                 (Pragma_Command,
                  "Add pragma Unreferenced to subprogram """ & Name & """");
               Append (Result, Pragma_Command);
            end;

         when Cat_Type =>
            declare
               Delete_Command : Remove_Entity_Cmd;
               Pragma_Command : Add_Pragma_Cmd;
            begin
               Initialize (Delete_Command, Current_Text, Cursor);
               Set_Caption
                 (Delete_Command,
                  "Delete type """ & Name & """");
               Append (Result, Delete_Command);

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

               Initialize (New_Command, Current_Text, With_Cursor);
               Set_Caption
                 (New_Command,
                  "Remove all clauses for package " & Name);
               Append (Result, New_Command);
               Free (With_Cursor);
            end;

         when others =>
            Raise_Exception
              (Codefix_Panic'Identity,
               "Wrong category given : " & Language_Category'Image (Category));
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
      Set_Location (Begin_Cursor, 0, 1);

      Initialize (New_Command, Current_Text, Pragma_Cursor, Begin_Cursor);
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
               Get_Extended_Unit_Name (Current_Text, Data (Cursor_Node)));

            for J in 1 ..  Index_Str - 1 loop
               if Str_Array (J).all = Str_Array (Index_Str).all then

                  for J in Str_Array'Range loop
                     Free (Str_Array (J));
                  end loop;

                  Codefix.Formal_Errors.Free (Result);

                  return Command_List.Null_List;
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
      Internal_Body_Cursor : File_Cursor := File_Cursor (Body_Cursor);
      Body_Info            : Construct_Information;
   begin
      Body_Info := Get_Unit (Current_Text, Body_Cursor, Before);

      Set_Location
        (Internal_Body_Cursor,
         Body_Info.Sloc_Start.Line, Body_Info.Sloc_Start.Column);

      Initialize (Command1, Current_Text, Internal_Body_Cursor, Spec_Cursor);
      Initialize (Command2, Current_Text, Spec_Cursor, Internal_Body_Cursor);

      Set_Caption (Command1, "Modify the implementation profile");
      Set_Caption (Command2, "Modify the spec profile");

      Append (Result, Command1);
      Append (Result, Command2);

      return Result;
   end Make_Conformant;

   -----------------------
   -- Remove_Use_Clause --
   -----------------------

   function Remove_Use_Clause
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Solution_List
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
         Category => Cat_Use);
      Set_Caption (New_Command, "Remove use clause");
      Append (Result, New_Command);

      Free (Word);

      return Result;
   end Remove_Use_Clause;

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

   -------------------------
   -- Is_Style_Or_Warning --
   -------------------------

   function Is_Style_Or_Warning (Error : Error_Message) return Boolean is
   begin
      return Error.Is_Style or Error.Is_Warning;
   end Is_Style_Or_Warning;

end Codefix.Formal_Errors;

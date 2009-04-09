-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2009, AdaCore               --
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

with Language.Tree.Database;            use Language.Tree.Database;
with Projects.Registry;                 use Projects.Registry;
with Traces;                            use Traces;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

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
      Style_Index, Warning_Index : Integer;
      Order   : Long_Long_Integer)
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
              (+Error_Line
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

         This.Order := Order;
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
      File    : GNATCOLL.VFS.Virtual_File;
      Line    : Positive;
      Col     : Column_Index;
      Message : String;
      Order   : Long_Long_Integer) is
   begin
      Assign (This.Message, Message);
      Set_File (This, File);
      Set_Location (This, Line, Col);
      This.Order := Order;
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

   ---------------
   -- Get_Order --
   ---------------

   function Get_Order (This : Error_Message) return Long_Long_Integer is
   begin
      return This.Order;
   end Get_Order;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Error_Message) is
   begin
      Free (File_Cursor (This));
      Free (This.Message);
   end Free;

   -----------
   -- Clone --
   -----------

   overriding function Clone (This : Error_Message) return Error_Message is
      New_Message : Error_Message;
   begin
      New_Message := (Clone (File_Cursor (This)) with
                      Message    => new String'(This.Message.all),
                      Is_Style   => This.Is_Style,
                      Is_Warning => This.Is_Warning,
                      Order      => This.Order);
      return New_Message;
   end Clone;

   ------------
   -- Concat --
   ------------

   overriding procedure Concat
     (Dest : in out Solution_List; Source : Solution_List) is
   begin
      Concat (Command_List.List (Dest), Command_List.List (Source));
   end Concat;

   ------------
   -- Length --
   ------------

   overriding function Length (List : Solution_List) return Integer is
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

   overriding function Next
     (It : Solution_List_Iterator) return Solution_List_Iterator is
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
      New_Command : Invert_Words_Cmd;
      Result      : Solution_List;
   begin
      New_Command.Initialize
        (Current_Text => Current_Text,
         Message_Loc  => Message,
         First_Word   => First_String,
         Second_Word  => Second_String);

      Set_Caption
        (New_Command,
         "Invert """ & First_String & """ and """ & Second_String & """");

      Append (Result, New_Command);

      return Result;
   end Wrong_Order;

   --------------
   -- Expected --
   --------------

   function Expected
     (Current_Text    : Text_Navigator_Abstr'Class;
      Message         : File_Cursor'Class;
      String_Expected : String;
      After_Pattern   : String := "";
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
                  File_Cursor (Word), After_Pattern, Add_Spaces, Position);
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
      New_Command : Indent_Code_Cmd;
      Result      : Solution_List;
   begin
      Initialize
        (New_Command,
         Current_Text,
         Message,
         Column_Expected);

      if Column_Expected = 0 then
         Set_Caption (New_Command, "Indent line");
      else
         Set_Caption
           (New_Command,
            "Move begin of instruction to column " &
            Column_Index'Image (Column_Expected));
      end if;

      Append (Result, New_Command);

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
      New_Command : Add_Clauses_Cmd;
      Result      : Solution_List;
   begin
      New_Command.Initialize
        (Current_Text, Cursor, Missing_Clause, Add_With, Add_Use);

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
      It : Construct_Tree_Iterator;
      Tree : Construct_Tree;
      Result : Solution_List;

      Id : constant Composite_Identifier := To_Composite_Identifier
        (To_Lower (Name));

      function Remove_Quotes (Name : String) return String;
      --  Removes the quotes around a string

      function Matches (Name : String) return Boolean;
      --  Return true if the last elements of the name given in parameter
      --  corresponds to the Id looked for (stored in the global variable Id).

      function Remove_Quotes (Name : String) return String is
         First, Last : Integer;
      begin
         First := Name'First;
         Last := Name'Last;

         for J in Name'Range loop
            if Name (J) = '"' then
               First := J + 1;

               exit;
            end if;
         end loop;

         for J in reverse Name'Range loop
            if Name (J) = '"' then
               Last := J - 1;

               exit;
            end if;
         end loop;

         return Name (First .. Last);
      end Remove_Quotes;

      function Matches (Name : String) return Boolean is
         Name_Id : constant Composite_Identifier :=
           To_Composite_Identifier (To_Lower (Name));
      begin
         if Length (Name_Id) < Length (Id) then
            return False;
         end if;

         for J in 0 .. Length (Id) - 1 loop
            if Get_Item (Id, Length (Id) - J)
              /= Get_Item (Name_Id, Length (Name_Id) - J)
            then
               return False;
            end if;
         end loop;

         return True;
      end Matches;

      Entity_Found : Boolean := True;

      Actual_Category : Language_Category;

      It_Location : constant Text_Location :=
        (Absolute_Offset => False,
         Line            => Get_Line (Cursor),
         Line_Offset     => 0);
   begin
      --  We don't want to open the buffer here, so we can't have access to
      --  the function that would convert a char index into a column (which
      --  relies on the line). But we still need to analyse the text around
      --  the message to see if that's an entity that we can parse and analyse.
      --  Therefore, we retreive all entities on the line of the message, since
      --  the construct tree do not need the buffer to be in, and we get the
      --  one which seems to be of the proper name, if any.
      --  ??? This heuristic could be improved if we stored both the column
      --  and the char offset in the construct tree.

      Tree := Get_Tree
        (Get_Structured_File (Current_Text, Get_File (Cursor)));

      if Category = Cat_Unknown then
         It := Get_Iterator_At
           (Tree, It_Location, Start_Name, After);
      elsif Category = Cat_Variable or else Category = Cat_Local_Variable then
         It := Get_Iterator_At
           (Tree,
            It_Location,
            Start_Name,
            After,
            (Cat_Variable, Cat_Local_Variable));
      elsif Category = Cat_Type or else Category = Cat_Subtype then
         It := Get_Iterator_At
           (Tree,
            It_Location,
            Start_Name,
            After,
            (Cat_Type, Cat_Subtype));
      else
         It := Get_Iterator_At
           (Tree, It_Location, Start_Name, After, (1 => Category));
      end if;

      while Get_Construct (It).Sloc_Entity.Line = Cursor.Get_Line loop
         exit when Matches (Remove_Quotes (Get_Construct (It).Name.all));

         It := Next (Tree, It);
      end loop;

      if It = Null_Construct_Tree_Iterator
        or else Get_Construct (It).Sloc_Entity.Line /= Cursor.Get_Line
        or else not Matches (Remove_Quotes (Get_Construct (It).Name.all))
      then
         --  There's no construct that we can analyse at that location, so we
         --  won't do operations relying on entity data.

         Entity_Found := False;
      end if;

      if Category = Cat_Unknown then
         Actual_Category := Get_Construct (It).Category;
      else
         Actual_Category := Category;
      end if;

      case Actual_Category is
         when Cat_Variable | Cat_Local_Variable =>
            --  In this case, we test Entity_Found only when adding the pragma,
            --  as we want to support "Except : others =>" exception handlers
            --  that are not currently retreived by the parser, at least for
            --  suggesting removal.
            --  ??? The ada parser should clearly be improved in order to
            --  retreived these.

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

               if Is_Set (Operations, Add_Pragma_Unreferenced)
                 and then Entity_Found
               then
                  Pragma_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

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
               if Entity_Found then
                  if Is_Set (Operations, Remove_Entity) then
                     Initialize
                       (Delete_Command, Current_Text, Cursor, Erase);
                     Set_Caption
                       (Delete_Command,
                        "Delete subprogram """ & Name & """");
                     Append (Result, Delete_Command);
                  end if;

                  if Is_Set (Operations, Comment_Entity) then
                     Initialize
                       (Comment_Command, Current_Text, Cursor, Comment);
                     Set_Caption
                       (Comment_Command,
                        "Comment subprogram """ & Name & """");
                     Append (Result, Comment_Command);
                  end if;

                  Pragma_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

                  Set_Caption
                    (Pragma_Command,
                     "Add pragma Unreferenced to subprogram """ & Name & """");
                  Append (Result, Pragma_Command);
               end if;
            end;

         when Cat_Type | Cat_Subtype =>
            if Entity_Found then
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
                     Initialize
                       (Comment_Command, Current_Text, Cursor, Comment);
                     Set_Caption
                       (Comment_Command,
                        "Comment type """ & Name & """");
                     Append (Result, Comment_Command);
                  end if;

                  Pragma_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

                  Set_Caption
                    (Pragma_Command,
                     "Add pragma Unreferenced to type """ & Name & """");
                  Append (Result, Pragma_Command);
               end;
            end if;

         when Cat_Literal =>
            if Entity_Found then
               declare
                  Pragma_Command : Add_Pragma_Cmd;
               begin
                  Pragma_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

                  Set_Caption
                    (Pragma_Command,
                     "Add pragma Unreferenced to literal """ & Name & """");
                  Append (Result, Pragma_Command);
               end;
            end if;

         when Cat_Parameter =>
            if Entity_Found then
               declare
                  New_Command : Add_Pragma_Cmd;
               begin
                  New_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

                  Set_Caption
                    (New_Command,
                     "Add pragma Unreferenced to parameter """ & Name & """");
                  Append (Result, New_Command);
               end;
            end if;

         when Cat_With =>
            if Entity_Found then
               declare
                  --  I114-034: This command does some buffer operations
                  New_Command : Remove_Pkg_Clauses_Cmd;
                  With_Cursor : Word_Cursor;
               begin
                  Set_File (With_Cursor, Get_File (Cursor));
                  Set_Location
                    (With_Cursor, Get_Line (Cursor), Get_Column (Cursor));
                  Set_Word (With_Cursor, Name, Text_Ascii);

                  if Is_Set (Operations, Remove_Entity) then
                     Initialize
                       (New_Command, Current_Text, With_Cursor, Before);
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
            end if;

         when Cat_Package =>
            if Entity_Found then
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
                     Initialize
                       (Comment_Command, Current_Text, Cursor, Comment);
                     Set_Caption
                       (Comment_Command,
                        "Comment package """ & Name & """");
                     Append (Result, Comment_Command);
                  end if;

                  Pragma_Command.Initialize
                    (Current_Text => Current_Text,
                     Position     => Cursor,
                     Category     => Actual_Category,
                     Name         => "Unreferenced",
                     Argument     => Name);

                  Set_Caption
                    (Pragma_Command,
                     "Add pragma Unreferenced to package """ & Name & """");
                  Append (Result, Pragma_Command);
               end;
            end if;

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
      Set_Location (Begin_Cursor, 0, 0);

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
              (New_Command,
               Current_Text,
               Word,
               File_Cursor (Word),
               Add_Spaces => False);
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
         "Move with clause from """ & Display_Base_Name (Get_File (Cursor)) &
         """ to """ & Display_Base_Name (Body_Name) & """");
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
   begin
      Initialize
        (Command1,
         Current_Text,
         Spec_Cursor,
         Body_Cursor,
         After,
         Enclosing);

      Initialize
        (Command2,
         Current_Text,
         Body_Cursor,
         Spec_Cursor,
         Enclosing,
         After);

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
      Command : Remove_Extra_Underlines_Cmd;
      Result  : Solution_List;
   begin
      Command.Initialize (Current_Text, Cursor);
      Set_Caption (Command, "Remove extra underlines");
      Append (Result, Command);

      return Result;
   end Remove_Extra_Underlines;

   --------------------------
   -- Change_To_Tick_Valid --
   --------------------------

   function Change_To_Tick_Valid
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List
   is
      Command : Change_To_Tick_Valid_Cmd;
      Result  : Solution_List;
   begin
      Initialize (Command, Current_Text, Cursor);
      Set_Caption (Command, "Change 'in' expression by 'Valid");
      Append (Result, Command);

      return Result;
   end Change_To_Tick_Valid;

   ------------------------
   -- Remove_Blank_Lines --
   ------------------------

   function Remove_Blank_Lines
     (Current_Text : Text_Navigator_Abstr'Class; Cursor : File_Cursor'Class)
      return Solution_List
   is
      Result  : Solution_List;
      Command : Remove_Blank_Lines_Cmd;
   begin
      Command.Initialize (Current_Text, Cursor);

      Append (Result, Command);

      return Result;
   end Remove_Blank_Lines;

   -------------------------
   -- Is_Style_Or_Warning --
   -------------------------

   function Is_Style_Or_Warning (Error : Error_Message) return Boolean is
   begin
      return Error.Is_Style or Error.Is_Warning;
   end Is_Style_Or_Warning;

end Codefix.Formal_Errors;

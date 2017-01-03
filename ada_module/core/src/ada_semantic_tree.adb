------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2017, AdaCore                     --
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

with Ada_Semantic_Tree.Parts;     use Ada_Semantic_Tree.Parts;
with Ada.Unchecked_Deallocation;
with Language.Ada;                use Language.Ada;

package body Ada_Semantic_Tree is

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (E : access Entity_View_Record) return String is
   begin
      pragma Unreferenced (E);
      return "";
   end Get_Documentation;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (E : access Entity_View_Record'Class)
      return access Simple_Construct_Information
   is
      Entity : Entity_Access;
   begin
      if E = null then
         return null;
      end if;

      Entity := E.Get_Entity;
      if Entity /= Null_Entity_Access then
         return Get_Construct (Entity);
      else
         return null;
      end if;
   end Get_Construct;

   -------------------
   -- Is_Accessible --
   -------------------

   function Is_Accessible
     (E : access Entity_View_Record)
      return Boolean
   is
      pragma Unreferenced (E);
   begin
      return True;
   end Is_Accessible;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (E : access Entity_View_Record) return Language_Category
   is
      Construct : access Simple_Construct_Information;
   begin
      if E = null then
         return Cat_Unknown;
      end if;

      Construct := E.Get_Construct;

      if Construct /= null then
         return Construct.Category;

      else
         return Cat_Unknown;
      end if;
   end Get_Category;

   --------------------------------
   -- To_Construct_Tree_Iterator --
   --------------------------------

   function To_Construct_Tree_Iterator
     (E : Entity_View) return Construct_Tree_Iterator is
   begin
      return To_Construct_Tree_Iterator (E.Get_Entity);
   end To_Construct_Tree_Iterator;

   --------------
   -- Get_File --
   --------------

   function Get_File (E : Entity_View) return Structured_File_Access is
   begin
      if E.Entity /= Null_Entity_Access then
         return Get_File (E.Entity);
      else
         return Get_File (E.Persistent);
      end if;
   end Get_File;

   ------------
   -- Is_All --
   ------------

   function Is_All (E : Entity_View) return Boolean is
   begin
      return E.Is_All;
   end Is_All;

   ----------------
   -- Set_Is_All --
   ----------------

   procedure Set_Is_All (E : Entity_View; Is_All : Boolean) is
   begin
      E.Is_All := Is_All;
   end Set_Is_All;

   ----------
   -- Free --
   ----------

   procedure Free (E : in out Entity_View) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_View_Record'Class, Entity_View);
   begin
      if E /= null then
         Free (E.all);
         if E.Persistent /= Null_Entity_Persistent_Access then
            Unref (E.Persistent);
         end if;
         Unchecked_Free (E);
      end if;
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (E : Entity_View) return Entity_View
   is
      Copy : Entity_View;
   begin
      if E = null then
         return null;
      else
         Copy := new Entity_View_Record'Class'(E.all);

         if Copy.Entity /= Null_Entity_Access then
            Copy.Persistent := To_Entity_Persistent_Access (Copy.Entity);
            Copy.Entity     := Null_Entity_Access;

         elsif Copy.Persistent /= Null_Entity_Persistent_Access then
            Ref (Copy.Persistent);
         end if;

         Deep_Copy (Copy.all);

         return Copy;
      end if;
   end Deep_Copy;

   -----------------
   -- Copy_On_Get --
   -----------------

   procedure Copy_On_Get (E : in out Entity_View) is
   begin
      E := Deep_Copy (E);
   end Copy_On_Get;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (E : access Entity_View_Record'Class) return Entity_Access
   is
   begin
      if E = null then
         return Null_Entity_Access;

      elsif E.Entity /= Null_Entity_Access then
         return E.Entity;

      elsif E.Persistent /= Null_Entity_Persistent_Access then
         return To_Entity_Access (E.Persistent);

      else
         return Null_Entity_Access;
      end if;
   end Get_Entity;

   ---------------
   -- Filter_In --
   ---------------

   function Filter_In
     (Filter : Entity_Filter; E : Entity_Access) return Boolean is
   begin
      case Filter.Kind is
         when Categories_Filter =>
            return Filter.Categories (Get_Construct (E).Category);

         when Exceptions_Only =>
            if Get_Construct (E).Category = Cat_Package then
               return True;
            elsif Get_Construct (E).Category = Cat_Local_Variable
              or else Get_Construct (E).Category = Cat_Variable
            then
               declare
                  List : constant Referenced_Identifiers_List :=
                    Get_Referenced_Identifiers
                      (To_Construct_Tree_Iterator (E));
               begin
                  if List /= Null_Referenced_Identifiers_List then
                     if Get_Identifier (List) =
                       Find_Normalized (Ada_Lang.Symbols, "exception")
                     then
                        return True;
                     end if;
                  end if;
               end;
            end if;

            return False;

         when others =>
            return True;

      end case;
   end Filter_In;

   ------------
   -- Create --
   ------------

   function Create
     (Categories : Category_Array) return Entity_Filter
   is
      Result : Entity_Filter (Categories_Filter);
   begin
      Result.Categories := (others => False);

      for J in Categories'Range loop
         Result.Categories (Categories (J)) := True;
      end loop;

      return Result;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Entity_List) is
   begin
      Free (List.Contents);
      Unref (List.Excluded_List);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Entity_Iterator) is
   begin
      --  Excluded_List is freed with the list, no need to free it here
      Free (It.It);
   end Free;

   -----------
   -- First --
   -----------

   function First (List : Entity_List) return Entity_Iterator is
      Result : Entity_Iterator;
   begin
      Result :=
        (It              => Entity_List_Pckg.First (List.Contents),
         Excluded_List   => List.Excluded_List,
         From_Visibility => List.From_Visibility);

      while not Is_Valid (Result) loop
         Next (Result);
      end loop;

      return Result;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Entity_Iterator) is
   begin
      Next (It.It);

      while not Is_Valid (It) loop
         Next (It.It);
      end loop;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Entity_Iterator) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Entity_Iterator) return Boolean is
      Result : Boolean;
   begin
      if At_End (It) then
         return True;
      else
         Result := not Is_Excluded (It.Excluded_List, Get_Entity (It));

         return Result;
      end if;
   end Is_Valid;

   --------------
   -- Get_View --
   --------------

   function Get_View (It : Entity_Iterator) return Entity_View is
      E : Entity_View;
   begin
      --  This assumes that a copy of the data is done below, by the sub
      --  iterator
      E := Get (It.It);

      if Is_Excluded (It.Excluded_List, E.Get_Entity) then
         return Null_Entity_View;
      end if;

      Configure_View (E.all, It);

      return E;
   end Get_View;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (It : Entity_Iterator) return Entity_Access is
      View   : Entity_View := Get (It.It);
      Result : Entity_Access;
   begin
      if View = Null_Entity_View then
         return Null_Entity_Access;
      end if;

      Result := View.Get_Entity;
      Free (View);
      --  ??? It's a bit annoying to have to create and free a temporary view,
      --  would be better to somehow get the entity directly.

      return Result;
   end Get_Entity;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity (Stack : in out Excluded_Stack_Type) is
   begin
      Stack.Entities.Delete_First;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity
     (Stack : in out Excluded_Stack_Type; Entity : Entity_Access) is
   begin
      Prepend (Stack.Entities, Entity);
   end Push_Entity;

   -----------------
   -- Is_Excluded --
   -----------------

   function Is_Excluded
     (Stack : Excluded_Stack_Type; Entity : Entity_Access) return Boolean is
   begin
      if Stack = null then
         return False;
      end if;

      for Item of Stack.Entities loop
         --  If the two entities are exactly on the same construct, or if
         --  they are parts of the same enitity, then we found an excluded
         --  construct.

         if (Get_File (Item) = Get_File (Entity)
             and then To_Construct_Tree_Iterator (Item) =
               To_Construct_Tree_Iterator (Entity))
           or else Are_Same_Entity (Item, Entity)
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Excluded;

   ---------
   -- Ref --
   ---------

   procedure Ref (Stack : in out Excluded_Stack_Type) is
   begin
      if Stack = null then
         Stack := new Excluded_Stack_Type_Record;
      end if;

      Stack.Refs := Stack.Refs + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Stack : in out Excluded_Stack_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Excluded_Stack_Type_Record, Excluded_Stack_Type);
   begin
      if Stack /= null then
         Stack.Refs := Stack.Refs - 1;

         if Stack.Refs = 0 then
            Stack.Entities.Clear;
            Free (Stack);
         end if;
      end if;
   end Unref;

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   function Parse_Expression_Backward
     (Buffer            : access constant UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0;
      Multiple_Operands : Boolean := False)
      return Parsed_Expression
   is
      use Token_List;

      Result    : Parsed_Expression;
      Expression_Depth : Natural := 0;
      Expression_Token : Token_Record;
      Last_Token : Token_Record := Null_Token;
      Last_Non_Blank_Token : Token_Record := Null_Token;
      In_Found : Boolean := False;
      All_Found : Boolean := False;
      With_Token_Found : Token_Record := Null_Token;

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean);

      ------------------
      -- Handle_Token --
      ------------------

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean) is
      begin
         Stop := False;

         if Expression_Depth = 0 then
            if Token.Tok_Type /= Tok_Blank then

               if With_Token_Found /= Null_Token then
                  --  If token before "with" not a semicolon then
                  --  "with" token is for an aspect or for a record definition
                  --  for new type. "type T_Child is new T with null record;"
                  --  Result contains Tok_Aspect
                  --  take into account limited private with clause.
                  case Token.Tok_Type is
                     when Tok_Limited | Tok_Private =>
                        return;
                     when Tok_Semicolon =>
                        null;
                     when others =>
                        With_Token_Found.Tok_Type := Tok_Aspect;
                  end case;
                  Prepend (Result.Tokens, With_Token_Found);
                  With_Token_Found := Null_Token;
                  Stop := True;
                  return;
               end if;

               if In_Found then
                  --  If the 'in' keyword is found, we need to make sure that
                  --  we're on a variable declaration, e.g.:
                  --     V : in Integer;
                  --  in such cases, we need to check that the current token is
                  --  a semicolon, add it and leave.
                  --  In all other situations, dismiss the in and leave.

                  if Token.Tok_Type = Tok_Colon then
                     Prepend (Result.Tokens, Token);
                     Stop := True;
                  else
                     Stop := not Multiple_Operands;
                  end if;

                  if Stop then
                     return;
                  end if;
               end if;

               if All_Found and then Token.Tok_Type /= Tok_Dot then
                  --  The only thing that we analyse so far is the
                  --  .all expression. In any other case, e.g. access all, we
                  --  just dismiss the all keyword and stop the analysis.

                  Result.Tokens.Delete_First;
                  Stop := True;

                  return;
               else
                  All_Found := False;
               end if;
            end if;

            case Token.Tok_Type is
               when Tok_Use
                  | Tok_Pragma
                  | Tok_Colon
                  | Tok_Accept
                  | Tok_Raise =>

                  Prepend (Result.Tokens, Token);
                  Stop := True;

               when Tok_Identifier =>
                  if Last_Non_Blank_Token.Tok_Type = Tok_Identifier then
                     Stop := not Multiple_Operands;
                  elsif Result.Tokens.Is_Empty
                    and then Last_Token.Tok_Type = Tok_Blank
                  then
                     Stop := not Multiple_Operands;
                  else
                     Prepend (Result.Tokens, Token);
                  end if;

               when Tok_With =>
                  With_Token_Found := Token;

               when Tok_Tick
                  | Tok_Arrow
                  | Tok_Dot =>

                  Prepend (Result.Tokens, Token);

               when Tok_All =>
                  All_Found := True;
                  Prepend (Result.Tokens, Token);

               when Tok_Close_Parenthesis =>
                  if Last_Non_Blank_Token.Tok_Type = Tok_In then
                     --  We're on e.g. for X in A, don't get more things

                     if not Multiple_Operands then
                        Stop := True;

                        return;
                     end if;
                  end if;

                  Prepend (Result.Tokens, Token);
                  Expression_Depth := 1;
                  Expression_Token.Tok_Type := Tok_Expression;

               when Tok_Comma =>
                  if Last_Non_Blank_Token = Null_Token then
                     Expression_Depth := 1;
                     Expression_Token.Tok_Type := Tok_Expression;
                  else
                     Stop := True;
                  end if;

               when Tok_Open_Parenthesis =>
                  if Last_Non_Blank_Token = Null_Token then
                     Prepend (Result.Tokens, Token);
                  else
                     Stop := True;
                  end if;

               when Tok_In =>
                  --  In declaration, e.g. A : in B. Ignore the
                  --  modifier keywords, but store it to see later if we're
                  --  actually in a case like 'for A in B'.

                  In_Found := True;

               when Tok_Aliased
                  | Tok_Access
                  | Tok_Constant
                  | Tok_Null
                  | Tok_Not
                  | Tok_Out =>

                  --  In declaration, e.g. A : constant B. Ignore the
                  --  modifier keywords

                  null;

               when Tok_Blank =>
                  null;

               when Tok_Operator
                  | Tok_And
                  | Tok_Mod
                  | Tok_Or
                  | Tok_Rem
                  | Tok_Xor =>

                  Stop := not Multiple_Operands;

               when Tok_Then
                  | Tok_Else =>

                  --  ??? We should probably be smarter here, and work the
                  --  "or else" and "and then" things, but doesn't seems to
                  --  be too much of a problem so far given the way things are
                  --  used.

                  Stop := not Multiple_Operands;

               when others =>
                  Stop := True;

            end case;

            if Token.Tok_Type /= Tok_Blank then
               Last_Non_Blank_Token := Token;
            end if;

            Last_Token := Token;
         else
            if Token.Tok_Type = Tok_Close_Parenthesis then
               Expression_Depth := Expression_Depth + 1;
            elsif Token.Tok_Type = Tok_Open_Parenthesis then
               Expression_Depth := Expression_Depth - 1;
            elsif Token.Tok_Type = Tok_Comma and then Expression_Depth = 1 then
               Prepend (Result.Tokens, Expression_Token);
               Expression_Token := (Tok_Expression, 0, 0);
            end if;

            if Expression_Depth = 0 then
               Prepend (Result.Tokens, Expression_Token);

               if Token.Tok_Type = Tok_Open_Parenthesis then
                  Prepend (Result.Tokens, Token);
               end if;
            elsif Token.Tok_Type /= Tok_Comma
              and then Token.Tok_Type /= Tok_Blank
            then
               if Expression_Token.Token_Last = 0 then
                  Expression_Token.Token_Last := Token.Token_Last;
               end if;

               Expression_Token.Token_First := Token.Token_First;
            end if;

            Last_Token := Expression_Token;
            Last_Non_Blank_Token := Token;
         end if;
      end Handle_Token;

   begin
      Result.Original_Buffer := Buffer;

      Ada_Lang.Parse_Tokens_Backwards
         (Buffer      => Buffer.all,
         Start_Offset => Start_Offset,
         End_Offset   => End_Offset,
         Callback     => Handle_Token'Access);

      if With_Token_Found /= Null_Token then
         Prepend (Result.Tokens, With_Token_Found);
         With_Token_Found := Null_Token;
      end if;

      return Result;
   end Parse_Expression_Backward;

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   function Parse_Expression_Backward
     (Buffer : access constant UTF8_String) return Parsed_Expression
   is
   begin
      if Buffer /= null then
         return Parse_Expression_Backward
           (Buffer, String_Index_Type (Buffer'Last), 0);
      else
         return Null_Parsed_Expression;
      end if;
   end Parse_Expression_Backward;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Expression : Parsed_Expression; Token : Token_Record) return String is
   begin
      case Ada_Token'(Token.Tok_Type) is
         when No_Token =>
            return "";

         when Tok_Dot =>
            return ".";

         when Tok_Open_Parenthesis =>
            return "(";

         when Tok_Close_Parenthesis =>
            return ")";

         when Tok_Colon =>
            return " : ";

         when Tok_Arrow =>
            return "=>";

         when Tok_Identifier
            | Tok_Expression
            | Tok_Operator
            | Tok_String
            | Ada_Reserved_Token =>

            if Token.Token_First /= 0 and then Token.Token_Last /= 0 then
               return Expression.Original_Buffer
              (Natural (Token.Token_First) .. Natural (Token.Token_Last));
            else
               return "";
            end if;

         when Tok_Tick =>
            return "'";

         when Tok_Comma =>
            return ", ";

         when Tok_Semicolon =>
            return ";";

         when Tok_Blank =>
            return "";

         when Tok_Dot_Dot =>
            return "..";

      end case;
   end Get_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Expression : Parsed_Expression) return String is
      use Token_List;

      Length : Natural := 0;
   begin
      for Item of Expression.Tokens loop
         Length := Length + Get_Name (Expression, Item)'Length;
      end loop;

      return Result : String (1 .. Length) do
         Length := Result'First;

         for Item of Expression.Tokens loop
            declare
               N : constant String := Get_Name (Expression, Item);
            begin
               Result (Length .. Length + N'Length - 1) := N;
               Length := Length + N'Length;
            end;
         end loop;
      end return;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Expression : in out Parsed_Expression) is
   begin
      Expression.Tokens.Clear;
   end Free;

end Ada_Semantic_Tree;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;
with Language.Ada; use Language.Ada;

package body Ada_Semantic_Tree is

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (E : access Entity_View_Record'Class)
      return access Simple_Construct_Information
   is
   begin
      if E /= null and then E.Entity /= Null_Entity_Access then
         return Get_Construct (E.Entity);
      else
         return null;
      end if;
   end Get_Construct;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (E : access Entity_View_Record) return Language_Category
   is
   begin
      if E /= null and then E.Get_Construct /= null then
         return E.Get_Construct.Category;
      else
         return Cat_Unknown;
      end if;
   end Get_Category;

   --------------------------------
   -- To_Construct_Tree_Iterator --
   --------------------------------

   function To_Construct_Tree_Iterator
     (E : Entity_View) return Construct_Tree_Iterator
   is
   begin
      return To_Construct_Tree_Iterator (E.Entity);
   end To_Construct_Tree_Iterator;

   --------------
   -- Get_File --
   --------------

   function Get_File (E : Entity_View) return Structured_File_Access is
   begin
      return Get_File (E.Entity);
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
         Unchecked_Free (E);
      end if;
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (E : Entity_View) return Entity_View is
      Copy : Entity_View;
   begin
      if E = null then
         return null;
      else
         Copy := new Entity_View_Record'Class'(E.all);

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
      if E /= null then
         return E.Entity;
      else
         return Null_Entity_Access;
      end if;
   end Get_Entity;

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

      Result := View.Entity;
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
      Pop (Stack.Entities);
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity
     (Stack : in out Excluded_Stack_Type; Entity : Entity_Access) is
   begin
      Push (Stack.Entities, Entity);
   end Push_Entity;

   -----------------
   -- Is_Excluded --
   -----------------

   function Is_Excluded
     (Stack : Excluded_Stack_Type; Entity : Entity_Access) return Boolean
   is
      Excluded : Excluded_Stack_Pckg.Simple_Stack;
   begin
      if Stack = null then
         return False;
      end if;

      Excluded := Stack.Entities;

      while Excluded /= null loop
         declare
            Excluded_Entity : constant Entity_Access := Excluded.Val;
         begin
            --  If the two entities are exactly on the same construct, or if
            --  they are parts of the same enitity, then we found an excluded
            --  construct.

            if (Get_File (Excluded_Entity) = Get_File (Entity)
              and then To_Construct_Tree_Iterator (Excluded_Entity)
                = To_Construct_Tree_Iterator (Entity))
              or else Are_Same_Entity (Excluded_Entity, Entity)
            then
               return True;
            end if;
         end;

         Excluded := Excluded.Next;
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
            Clear (Stack.Entities);
            Free (Stack);
         end if;
      end if;
   end Unref;

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   function Parse_Expression_Backward
     (Buffer            : access Glib.UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0)
      return Parsed_Expression
   is
      use Token_List;

      Result    : Parsed_Expression;
      Expression_Depth : Natural := 0;
      Expression_Token : Token_Record;
      Last_Token : Token_Record := Null_Token;
      Last_Non_Blank_Token : Token_Record := Null_Token;

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean);
      function To_String (Token : Token_Record) return String;

      ---------------
      -- To_String --
      ---------------

      function To_String (Token : Token_Record) return String is
      begin
         if Token.Tok_Type = No_Token then
            return "";
         elsif Natural (Token.Token_First) not in Buffer'Range
           or else Natural (Token.Token_Last) not in Buffer'Range
         then
            return "";
         else
            return To_Lower
              (Buffer
                 (Natural (Last_Non_Blank_Token.Token_First)
                  .. Natural (Last_Non_Blank_Token.Token_Last)));
         end if;
      end To_String;

      ------------------
      -- Handle_Token --
      ------------------

      procedure Handle_Token (Token : Token_Record; Stop : in out Boolean) is
      begin
         Stop := False;

         if Expression_Depth = 0 then
            case Token.Tok_Type is
               when Tok_With | Tok_Use | Tok_Pragma =>
                  Prepend (Result.Tokens, Token);
                  Stop := True;

               when Tok_Identifier =>
                  if Last_Non_Blank_Token.Tok_Type = Tok_Identifier then
                     Stop := True;
                  elsif Length (Result.Tokens) = 0
                    and then Last_Token.Tok_Type = Tok_Blank
                  then
                     Stop := True;
                  else
                     Prepend (Result.Tokens, Token);
                  end if;

               when Tok_All
                  | Tok_Tick
                  | Tok_Arrow
                  | Tok_Colon
                  | Tok_Dot =>

                  Prepend (Result.Tokens, Token);

               when Tok_Close_Parenthesis =>
                  if To_String (Last_Non_Blank_Token) = "in" then
                     --  We're on e.g. for X in A, don't get more things

                     Stop := True;

                     return;
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

               when Tok_Operator
                  | Tok_Range
                  | Tok_String
                  | Tok_Semicolon =>

                  Stop := True;

               when No_Token | Tok_Expression | Tok_Blank | Tok_Reserved =>
                  null;

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

      return Result;
   end Parse_Expression_Backward;

   -------------------------------
   -- Parse_Expression_Backward --
   -------------------------------

   function Parse_Expression_Backward (Buffer : access Glib.UTF8_String)
      return Parsed_Expression
   is
   begin
      return Parse_Expression_Backward
        (Buffer, String_Index_Type (Buffer'Last), 0);
   end Parse_Expression_Backward;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
   (Expression : Parsed_Expression; Token : Token_Record) return String is
   begin
      case Token.Tok_Type is
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
            | Tok_Reserved
            | Tok_String =>

            if Token.Token_First /= 0 and then Token.Token_Last /= 0 then
               return Expression.Original_Buffer
              (Natural (Token.Token_First) .. Natural (Token.Token_Last));
            else
               return "";
            end if;

         when Tok_All =>
            return "all";

         when Tok_Tick =>
            return "'";

         when Tok_With =>
            return "with ";

         when Tok_Use =>
            return "use ";

         when Tok_Pragma =>
            return "pragma ";

         when Tok_Comma =>
            return ", ";

         when Tok_Range =>
            return "..";

         when Tok_Semicolon =>
            return ";";

         when Tok_Blank =>
            return "";

      end case;
   end Get_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (Expression : Parsed_Expression) return String is
      use Token_List;

      Length : Natural := 0;
      Iter   : Token_List.List_Node := First (Expression.Tokens);
   begin
      while Iter /= Null_Node loop
         Length := Length + Get_Name (Expression, Data (Iter))'Length;
         Iter := Next (Iter);
      end loop;

      return Result : String (1 .. Length) do
         Iter := First (Expression.Tokens);
         Length := Result'First;

         while Iter /= Null_Node loop
            declare
               N : constant String := Get_Name (Expression, Data (Iter));
            begin
               Result (Length .. Length + N'Length - 1) := N;
               Length := Length + N'Length;
            end;
            Iter := Next (Iter);
         end loop;
      end return;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Expression : in out Parsed_Expression) is
   begin
      Token_List.Free (Expression.Tokens);
   end Free;

end Ada_Semantic_Tree;

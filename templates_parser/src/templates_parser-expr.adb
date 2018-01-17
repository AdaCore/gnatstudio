------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

separate (Templates_Parser)

package body Expr is

   use Ada.Strings.Maps;

   --  BNF definition of the expression language:
   --
   --  <expr>     ::= <relation> {<Logic_Op> <relation>}
   --  <relation> ::= <term> {<comp_op> <term>}
   --  <term>     ::= ["not"] <primary>
   --  <primary>  ::= <value> | <var> | "(" <expr> ")"
   --  <logic_op> ::= "and" | "or" | "xor"
   --  <comp_op>  ::= "<" | "<=" | "=" | ">=" | ">" | "/="

   subtype Comp_Op  is Ops range O_Sup .. O_In;
   subtype Logic_Op is Ops range O_And .. O_Xor;

   Separator : constant Character_Set := Blank or To_Set ("<>=/()");

   -------------
   -- Analyze --
   -------------

   function Analyze (E : Expr.Tree) return String is

      type Ops_Fct is access function (L, R : Expr.Tree) return String;

      function F_And  (L, R : Expr.Tree) return String;
      function F_Or   (L, R : Expr.Tree) return String;
      function F_Xor  (L, R : Expr.Tree) return String;
      function F_Sup  (L, R : Expr.Tree) return String;
      function F_Esup (L, R : Expr.Tree) return String;
      function F_Einf (L, R : Expr.Tree) return String;
      function F_Inf  (L, R : Expr.Tree) return String;
      function F_Equ  (L, R : Expr.Tree) return String;
      function F_Diff (L, R : Expr.Tree) return String;
      function F_In   (L, R : Expr.Tree) return String;

      type U_Ops_Fct is access function (N : Expr.Tree) return String;

      function F_Not (N : Expr.Tree) return String;

      -----------
      -- F_And --
      -----------

      function F_And (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;
         elsif Is_True (LV) and then Is_True (RV) then
            return "TRUE";
         else
            return "FALSE";
         end if;
      end F_And;

      ------------
      -- F_Diff --
      ------------

      function F_Diff (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;
         elsif Analyze (L) /= Analyze (R) then
            return "TRUE";
         else
            return "FALSE";
         end if;
      end F_Diff;

      ------------
      -- F_Einf --
      ------------

      function F_Einf (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;

         elsif Utils.Is_Number (LV) and then Utils.Is_Number (RV) then
            if Integer'Value (LV) <= Integer'Value (RV) then
               return "TRUE";
            else
               return "FALSE";
            end if;

         else
            if LV <= RV then
               return "TRUE";
            else
               return "FALSE";
            end if;
         end if;
      end F_Einf;

      -----------
      -- F_Equ --
      -----------

      function F_Equ (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;
         elsif LV = RV then
            return "TRUE";
         else
            return "FALSE";
         end if;
      end F_Equ;

      ------------
      -- F_Esup --
      ------------

      function F_Esup (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;

         elsif Utils.Is_Number (LV) and then Utils.Is_Number (RV) then
            if Integer'Value (LV) >= Integer'Value (RV) then
               return "TRUE";
            else
               return "FALSE";
            end if;

         else
            if LV >= RV then
               return "TRUE";
            else
               return "FALSE";
            end if;
         end if;
      end F_Esup;

      ----------
      -- F_In --
      ----------

      function F_In (L, R : Expr.Tree) return String is
         pragma Unreferenced (L, R);
      begin
         --  Always unknown as an in expression contains a variable
         return Unknown;
      end F_In;

      -----------
      -- F_Inf --
      -----------

      function F_Inf (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;

         elsif Utils.Is_Number (LV) and then Utils.Is_Number (RV) then
            if Integer'Value (LV) < Integer'Value (RV) then
               return "TRUE";
            else
               return "FALSE";
            end if;

         else
            if LV < RV then
               return "TRUE";
            else
               return "FALSE";
            end if;
         end if;
      end F_Inf;

      -----------
      -- F_Not --
      -----------

      function F_Not (N : Expr.Tree) return String is
         NV : constant String := Analyze (N);
      begin
         if NV = Unknown then
            return Unknown;
         elsif Is_True (NV) then
            return "FALSE";
         else
            return "TRUE";
         end if;
      end F_Not;

      ----------
      -- F_Or --
      ----------

      function F_Or (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;
         elsif Is_True (LV) or else Is_True (RV) then
            return "TRUE";
         else
            return "FALSE";
         end if;
      end F_Or;

      -----------
      -- F_Sup --
      -----------

      function F_Sup (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;

         elsif Utils.Is_Number (LV) and then Utils.Is_Number (RV) then
            if Integer'Value (LV) > Integer'Value (RV) then
               return "TRUE";
            else
               return "FALSE";
            end if;

         else
            if LV > RV then
               return "TRUE";
            else
               return "FALSE";
            end if;
         end if;
      end F_Sup;

      -----------
      -- F_Xor --
      -----------

      function F_Xor (L, R : Expr.Tree) return String is
         LV : constant String := Analyze (L);
         RV : constant String := Analyze (R);
      begin
         if LV = Unknown or else RV = Unknown then
            return Unknown;
         elsif Is_True (LV) xor Is_True (RV) then
            return "TRUE";
         else
            return "FALSE";
         end if;
      end F_Xor;

      Op_Table   : constant array (Expr.Ops) of Ops_Fct :=
                     (Expr.O_And   => F_And'Access,
                      Expr.O_Or    => F_Or'Access,
                      Expr.O_Xor   => F_Xor'Access,
                      Expr.O_Sup   => F_Sup'Access,
                      Expr.O_Inf   => F_Inf'Access,
                      Expr.O_Esup  => F_Esup'Access,
                      Expr.O_Einf  => F_Einf'Access,
                      Expr.O_Equal => F_Equ'Access,
                      Expr.O_Diff  => F_Diff'Access,
                      Expr.O_In    => F_In'Access);

      U_Op_Table : constant array (Expr.U_Ops) of U_Ops_Fct :=
                     (Expr.O_Not => F_Not'Access);

   begin
      case E.Kind is
         when Expr.Value =>
            return To_String (E.V);

         when Expr.Var =>
            return Unknown;

         when Expr.Op =>
            return Op_Table (E.O) (E.Left, E.Right);

         when Expr.U_Op =>
            return U_Op_Table (E.U_O) (E.Next);
      end case;
   end Analyze;

   -----------
   -- Clone --
   -----------

   function Clone (E : Tree) return Tree is
      N : Tree;
   begin
      if E = null then
         return null;
      else
         N := new Node'(E.all);
      end if;

      case E.Kind is
         when Value | Var =>
            null;
         when Op =>
            N.Left := Clone (N.Left);
            N.Right := Clone (N.Right);
         when U_Op =>
            N.Next := Clone (N.Next);
      end case;
      return N;
   end Clone;

   -----------
   -- Image --
   -----------

   function Image (O : Ops) return String is
   begin
      case O is
         when O_And   => return "and";
         when O_Or    => return "or";
         when O_Xor   => return "xor";
         when O_Sup   => return ">";
         when O_Inf   => return "<";
         when O_Esup  => return ">=";
         when O_Einf  => return "<=";
         when O_Equal => return "=";
         when O_Diff  => return "/=";
         when O_In    => return "in";
      end case;
   end Image;

   function Image (O : U_Ops) return String is
   begin
      case O is
         when O_Not => return "not";
      end case;
   end Image;

   -------------
   -- Is_True --
   -------------

   function Is_True (Str : String) return Boolean is
      L_Str : constant String := Characters.Handling.To_Upper (Str);
   begin
      return L_Str = "TRUE" or else L_Str = "T" or else L_Str = "1";
   end Is_True;

   -----------
   -- Parse --
   -----------

   function Parse (Expression : String) return Tree is

      Start_Index : Natural := Expression'First;
      Index       : Natural := Expression'First;

      type Token_Kind
        is (Open_Par, Close_Par, Binary_Op, Unary_Op, Value, Var, End_Expr);

      type Token (Kind : Token_Kind := Var) is record
         case Kind is
            when Open_Par | Close_Par | End_Expr =>
               null;
            when Binary_Op =>
               Bin_Op : Ops;
            when Unary_Op =>
               Un_Op  : U_Ops;
            when Value | Var =>
               Start  : Positive; -- range of the token
               Stop   : Positive; -- in Expression string
         end case;
      end record;

      Current_Token : Token;

      procedure Error (Mess : String);
      pragma No_Return (Error);
      --  Raises Internal_Error with the column of the condition

      function Expr return Tree;
      --  Parse a logical operator

      function Term return Tree;
      --  Parse a term (unary operator)

      function Relation return Tree;
      --  Parse a relational operator

      function Primary return Tree;
      --  ???

      -----------
      -- Error --
      -----------

      procedure Error (Mess : String) is
      begin
         raise Internal_Error
           with "col" & Integer'Image (Start_Index) & " condition, " & Mess;
      end Error;

      procedure Next_Token;
      --  Moves Current_Token to next token. Set Index after the last analysed
      --  consumed from expression.

      ----------
      -- Expr --
      ----------

      function Expr return Tree is
         N : Tree;
         O : Ops;
      begin
         N := Relation;

         while Current_Token.Kind = Binary_Op
           and then Current_Token.Bin_Op in Logic_Op
         loop
            O := Current_Token.Bin_Op;
            Next_Token;
            N := new Node'(Op, O, N, Relation);
         end loop;

         return N;
      end Expr;

      ----------------
      -- Next_Token --
      ----------------

      procedure Next_Token is
         use Ada.Characters.Handling;
         I : Natural;
      begin
         --  Skip blanks

         while Index <= Expression'Last
           and then Is_In (Expression (Index), Blank)
         loop
            Index := Index + 1;
         end loop;

         Start_Index := Index;

         if Index > Expression'Last then
            --  No more data to read
            Current_Token := (Kind => End_Expr);

         --  Check symbolic operators
         elsif Expression (Index) = '(' then
            Current_Token := (Kind => Open_Par);
            Index := Index + 1;

         elsif Expression (Index) = ')' then
            Current_Token := (Kind => Close_Par);
            Index := Index + 1;

         elsif Expression (Index) = '=' then
            Current_Token := (Kind => Binary_Op, Bin_Op => O_Equal);
            Index := Index + 1;

         elsif Expression (Index) = '/'
           and then Index < Expression'Last
           and then Expression (Index + 1) = '='
         then
            Current_Token := (Kind => Binary_Op, Bin_Op => O_Diff);
            Index := Index + 2;

         elsif Expression (Index) = '<' then
            Index := Index + 1;
            if Expression (Index) = '=' then
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Einf);
               Index := Index + 1;
            else
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Inf);
            end if;

         elsif Expression (Index) = '>' then
            Index := Index + 1;
            if Expression (Index) = '=' then
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Esup);
               Index := Index + 1;
            else
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Sup);
            end if;

         elsif Expression (Index) = '"' then
            --  This is a string, return it
            Current_Token
              := (Kind => Value, Start => Index + 1, Stop => Index);

            loop
               if Current_Token.Stop = Expression'Last then
                  Error ("condition, no matching closing quote string");
               elsif Expression (Current_Token.Stop + 1) = '"' then
                  exit;
               else
                  Current_Token.Stop := Current_Token.Stop + 1;
               end if;
            end loop;
            Index := Current_Token.Stop + 2;

         else
            --  We have found the start of a string token, look for end of it

            I := Index;

            loop
               Index := Fixed.Index
                 (Expression (Index .. Expression'Last), Separator);

               if Index = 0 then
                  --  Token end is the end of Expression
                  Index := Expression'Last + 1;
                  exit;
               end if;

               --  Special case for '/': it is a separator only if appearing
               --  in '/='. Without this test, the "/" filter is not recognized
               --  Moreover, this allows comparisons of file paths (with '/')

               exit when Expression (Index) /= '/'
                 or else Expression (Index + 1) = '=';

               Index := Index + 1;
            end loop;

            declare
               Token_Image : constant String :=
                               To_Lower (Expression (I .. Index - 1));
            begin
               if Token_Image = "not" then
                  Current_Token := (Kind => Unary_Op, Un_Op => O_Not);

               elsif Token_Image = "and" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_And);

               elsif Token_Image = "or" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_Or);

               elsif Token_Image = "xor" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_Xor);

               elsif Token_Image = "in" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_In);

               elsif Token_Image'Length > Length (Begin_Tag)
                 and then
                   Token_Image (Token_Image'First
                                .. Token_Image'First + Length (Begin_Tag) - 1)
                   = Begin_Tag
               then
                  --  This is a variable, we have the start of it, now look
                  --  for the end of the variable.

                  if Index <= Expression'Last
                    and then Expression (Index) = '('
                  then
                     --  This is not the end of the tag variable but the
                     --  start of the tag parameters. Look for tag variable
                     --  end.
                     Index := Fixed.Index
                       (Expression (Index .. Expression'Last),
                        To_String (End_Tag));
                     Index := Index + Length (End_Tag);
                  end if;

                  if Index = 0 then
                     Error ("variable end not found");

                  else
                     Current_Token
                       := (Kind  => Var, Start => I, Stop  => Index - 1);
                  end if;

               else
                  Current_Token
                    := (Kind => Value, Start => I, Stop => Index - 1);
               end if;
            end;
         end if;
      end Next_Token;

      -------------
      -- Primary --
      -------------

      function Primary return Tree is
         Result      : Tree;
         Start, Stop : Natural;
      begin
         case Current_Token.Kind is
            --  Normal cases
            when Open_Par =>
               Next_Token;
               Result := Expr;
               if Current_Token.Kind = Close_Par then
                  Next_Token;
                  return Result;
               else
                  Error ("missing closing parenthesis");
               end if;

            when Value =>
               Start := Current_Token.Start;
               Stop  := Current_Token.Stop;
               Next_Token;
               return new Node'
                 (Value,
                  V => To_Unbounded_String (Expression (Start .. Stop)));

            when Var =>
               Start := Current_Token.Start;
               Stop  := Current_Token.Stop;
               Next_Token;
               return new Node'
                 (Var, Var => Data.Build (Expression (Start .. Stop)));

            --  Errors

            when Unary_Op =>
               Error ("misplaced operator """
                      & Image (Current_Token.Un_Op) & '"');

            when Binary_Op =>
               Error ("misplaced operator """
                      & Image (Current_Token.Bin_Op) & '"');

            when Close_Par =>
               Error ("unexpected right parenthesis");

            when End_Expr =>
               Error ("missing operand");
         end case;
      end Primary;

      --------------
      -- Relation --
      --------------

      function Relation return Tree is
         N : Tree;
         O : Ops;
      begin
         N := Term;

         while Current_Token.Kind = Binary_Op
           and then Current_Token.Bin_Op in Comp_Op
         loop
            O := Current_Token.Bin_Op;
            Next_Token;
            N := new Node'(Op, O, N, Term);
         end loop;

         return N;
      end Relation;

      ----------
      -- Term --
      ----------

      function Term return Tree is
         O : U_Ops;
      begin
         if Current_Token.Kind = Unary_Op then
            O := Current_Token.Un_Op;
            Next_Token;
            return new Node'(U_Op, U_O => O, Next => Primary);
         else
            return Primary;
         end if;
      end Term;

      Result : Tree;

   begin
      Next_Token;
      Result := Expr;

      case Current_Token.Kind is
         when End_Expr =>
            null;

         when Open_Par | Close_Par | Value | Var =>
            Error ("Missing operator");

         when Binary_Op | Unary_Op =>
            Error ("Missing operand");
      end case;

      return Result;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (E : Tree) is
   begin
      case E.Kind is
         when Value =>
            Text_IO.Put (Quote (To_String (E.V)));

         when Var =>
            Text_IO.Put (Data.Image (E.Var));

         when Op =>
            Text_IO.Put ('(');
            Print_Tree (E.Left);
            Text_IO.Put (' ' & Image (E.O) & ' ');
            Print_Tree (E.Right);
            Text_IO.Put (')');

         when U_Op =>
            Text_IO.Put ('(');
            Text_IO.Put (Image (E.U_O) & ' ');
            Print_Tree (E.Next);
            Text_IO.Put (')');
      end case;
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (E : in out Tree; Single : Boolean := False) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Node, Tree);
   begin
      case E.Kind is
         when Value =>
            null;

         when Var =>
            Data.Release (E.Var);

         when Op =>
            if not Single then
               Release (E.Left);
               Release (E.Right);
            end if;

         when U_Op =>
            if not Single then
               Release (E.Next);
            end if;
      end case;

      Unchecked_Free (E);
   end Release;

end Expr;

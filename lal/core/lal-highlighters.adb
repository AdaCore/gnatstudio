------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Ada.Containers.Vectors;

with GNAT.Strings;
with Langkit_Support.Slocs;              use Langkit_Support.Slocs;
with Langkit_Support.Text;

package body LAL.Highlighters is

   type Style_Enum_Type is
     (None,
      Keyword, String_Style, Number, Comment, Block, Type_Style,
      Aspect, Aspect_Keyword, Aspect_String, Aspect_Number,
      Aspect_Comment, Aspect_Block, Aspect_Type);

   type Known_Styles_Array is
     array (Keyword .. Aspect_Type) of GNAT.Strings.String_Access;

   Known_Styles : constant Known_Styles_Array :=
     (Keyword        => new String'("keyword"),
      String_Style   => new String'("string"),
      Number         => new String'("number"),
      Comment        => new String'("comment"),
      Block          => new String'("block"),
      Type_Style     => new String'("type"),
      Aspect         => new String'("aspect"),
      Aspect_Keyword => new String'("aspect_keyword"),
      Aspect_String  => new String'("aspect_string"),
      Aspect_Number  => new String'("aspect_number"),
      Aspect_Comment => new String'("aspect_comment"),
      Aspect_Block   => new String'("aspect_block"),
      Aspect_Type    => new String'("aspect_type"));

   package Highlights_Holders is
      type Highlights_Holder is tagged limited private;
      --  Highlights_Holder stores style for each token in the range given
      --  on initialization.

      procedure Initialize
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Empty : out Boolean);
      --  Initialize holder by providing token range. If From or To is a trivia
      --  holder uses corresponding non-trivia token instead.

      subtype Syntax_Style_Enum_Type
        is Style_Enum_Type range Block .. Type_Style;
      subtype Style_Subset is Style_Enum_Type
        with Static_Predicate =>
          Style_Subset in None | Aspect
                        | Block | Type_Style
                        | Aspect_Block | Aspect_Type;

      procedure Set
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Style : Syntax_Style_Enum_Type)
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      function Get
        (Self  : Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference) return Style_Subset
           with Pre => not Libadalang.Common.Is_Trivia (Token);

      procedure Set_Aspect
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference)
           with Pre => not Libadalang.Common.Is_Trivia (From) and then
                       not Libadalang.Common.Is_Trivia (To);
      --  Mark each token in the range From .. To as part of aspect

   private

      package Style_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Libadalang.Common.Token_Data_Handlers.Token_Index,
         Element_Type => Style_Subset);

      type Highlights_Holder is tagged limited record
         First  : Libadalang.Common.Token_Data_Handlers.Token_Index;
         Vector : Style_Vectors.Vector;
      end record;
   end Highlights_Holders;

   package body Highlights_Holders is

      Map : constant array (Syntax_Style_Enum_Type) of Style_Enum_Type :=
        (Block => Aspect_Block, Type_Style => Aspect_Type);

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference;
         Empty : out Boolean)
      is
         use type Libadalang.Common.Token_Data_Handlers.Token_Index;
         use type Libadalang.Common.Token_Reference;

         First : Libadalang.Common.Token_Reference := From;
         Last  : Libadalang.Common.Token_Reference := To;

         Count : Libadalang.Common.Token_Data_Handlers.Token_Index;
      begin
         if Libadalang.Common.Is_Trivia (First) then
            First := Libadalang.Common.Next (First, Exclude_Trivia => True);
         end if;

         if Libadalang.Common.Is_Trivia (Last) then
            Last := Libadalang.Common.Previous (Last, Exclude_Trivia => True);
         end if;

         if Libadalang.Common.No_Token in First | Last
           or else Last < First
         then
            Self.First := 0;
            Self.Vector.Clear;
            Empty := True;
            return;
         end if;

         Self.First := Libadalang.Common.Index (First);
         Count := Libadalang.Common.Index (Last) - Self.First + 1;

         Self.Vector.Clear;
         Self.Vector.Append (None, Ada.Containers.Count_Type (Count));
         Empty := False;
      end Initialize;

      ---------
      -- Get --
      ---------

      function Get
        (Self  : Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference) return Style_Subset
      is
         use type Libadalang.Common.Token_Data_Handlers.Token_Index;

         Index : constant Libadalang.Common.Token_Data_Handlers.Token_Index :=
           Libadalang.Common.Index (Token) - Self.First;
      begin
         return Self.Vector (Index);
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self  : in out Highlights_Holder'Class;
         Token : Libadalang.Common.Token_Reference;
         Style : Syntax_Style_Enum_Type)
      is
         use type Libadalang.Common.Token_Data_Handlers.Token_Index;
         Index : constant Libadalang.Common.Token_Data_Handlers.Token_Index :=
           Libadalang.Common.Index (Token) - Self.First;
      begin
         if Self.Vector (Index) in None | Block | Type_Style then
            Self.Vector (Index) := Style;
         else
            Self.Vector (Index) := Map (Style);
         end if;
      end Set;

      ----------------
      -- Set_Aspect --
      ----------------

      procedure Set_Aspect
        (Self  : in out Highlights_Holder'Class;
         From  : Libadalang.Common.Token_Reference;
         To    : Libadalang.Common.Token_Reference)
      is
         use type Libadalang.Common.Token_Reference;
         use type Libadalang.Common.Token_Data_Handlers.Token_Index;

         Token : Libadalang.Common.Token_Reference := From;
         Index : Libadalang.Common.Token_Data_Handlers.Token_Index;
      begin
         loop
            --  Re-highlight only the modified tokens
            if Libadalang.Common.Index (Token) >= Self.First then
               Index := Libadalang.Common.Index (Token) - Self.First;

               if Index > Self.Vector.Last_Index then
                  null;
               elsif Self.Vector (Index) = None then
                  Self.Vector (Index) := Aspect;
               elsif Self.Vector (Index) in Syntax_Style_Enum_Type then
                  Self.Vector (Index) := Map (Self.Vector (Index));
               end if;
            end if;

            exit when Token = To;

            Token := Libadalang.Common.Next (Token, Exclude_Trivia => True);
         end loop;
      end Set_Aspect;

   end Highlights_Holders;

   procedure Remove_Highlighting_on_Lines
     (Self   : in out Highlightable_Interface'Class;
      From   : Integer;
      To     : Integer);
   --  Remove any highlight related styles from between the specified lines

   procedure Apply_Style
     (Self  : in out Highlightable_Interface'Class;
      Token : Libadalang.Common.Token_Reference;
      Style : Style_Enum_Type);
   --  Apply Style to the given token.
   --  Do nothing if there is no style (i.e: when Style = None),

   function To_Style
     (Token      : Libadalang.Common.Token_Kind;
      Is_Keyword : Boolean;
      In_Aspect  : Boolean;
      In_String  : Boolean) return Style_Enum_Type;
   --  Get the style from a language token.
   --  It takes into account if the token is located in some aspect or Ghost
   --  code (In_Aspect) or in some unfinished string literal (In_String).

   function Get_Known_Style (Style : Style_Enum_Type) return String;
   pragma Inline (Get_Known_Style);
   --  Return the corresponding known style as a string.
   --  An empty string is returned when there is no corresponding known style.

   function Is_Ghost_Root_Node
     (Node  : Libadalang.Analysis.Ada_Node'Class) return Boolean;
   --  Check if given node is a declaration and has ghost aspect

   procedure Update_Wrong_Literal_State
     (State      : in out Boolean;
      Is_Error   : Boolean;
      Error_Text : Wide_Wide_String;
      Loc        : Source_Location_Range);
   --  Set error State to True when unfinished string literal found (Is_Error).
   --  Set it to False if new line found. Error_Text contains token text if
   --  Is_Error.

   --------------
   -- To_Style --
   --------------

   function To_Style
     (Token      : Libadalang.Common.Token_Kind;
      Is_Keyword : Boolean;
      In_Aspect  : Boolean;
      In_String  : Boolean) return Style_Enum_Type
   is
      use Libadalang.Common;

      --  Append corresponding style prefix if In_Aspect = True
      Aspect_Prefix      : constant array (None .. Type_Style, Boolean)
        of Style_Enum_Type :=
          (None         => (None, Aspect),
           Keyword      => (Keyword, Aspect_Keyword),
           String_Style => (String_Style, Aspect_String),
           Number       => (Number, Aspect_Number),
           Comment      => (Comment, Aspect_Comment),
           Block        => (Block, Aspect_Block),
           Type_Style   => (Type_Style, Aspect_Type));

   begin
      if In_String then
         return Aspect_Prefix (String_Style, In_Aspect);
      elsif Is_Keyword then
         return Aspect_Prefix (Keyword, In_Aspect);
      end if;

      case Token is
         when Ada_Identifier =>
            return Aspect_Prefix (None, In_Aspect);
         when Ada_Termination |
              Ada_Lexing_Failure |
              Ada_Whitespace =>
            return Aspect_Prefix (None, In_Aspect);
         when
              Ada_Abort |
              Ada_Abs |
              Ada_Accept |
              Ada_Access |
              Ada_All |
              Ada_And |
              Ada_Array |
              Ada_At |
              Ada_Begin |
              Ada_Body |
              Ada_Case |
              Ada_Constant |
              Ada_Declare |
              Ada_Delay |
              Ada_Delta |
              Ada_Digits |
              Ada_Do |
              Ada_Else |
              Ada_Elsif |
              Ada_End |
              Ada_Entry |
              Ada_Exception |
              Ada_Exit |
              Ada_For |
              Ada_Function |
              Ada_Generic |
              Ada_Goto |
              Ada_If |
              Ada_In |
              Ada_Is |
              Ada_Limited |
              Ada_Loop |
              Ada_Mod |
              Ada_New |
              Ada_Not |
              Ada_Null |
              Ada_Of |
              Ada_Or |
              Ada_Others |
              Ada_Out |
              Ada_Package |
              Ada_Pragma |
              Ada_Private |
              Ada_Procedure |
              Ada_Raise |
              Ada_Range |
              Ada_Record |
              Ada_Rem |
              Ada_Renames |
              Ada_Return |
              Ada_Reverse |
              Ada_Select |
              Ada_Separate |
              Ada_Subtype |
              Ada_Task |
              Ada_Terminate |
              Ada_Then |
              Ada_Type |
              Ada_Use |
              Ada_When |
              Ada_While |
              Ada_With |
              Ada_Xor =>
            return Aspect_Prefix (Keyword, In_Aspect);
         when
           Ada_Amp |
           Ada_Arrow |
           Ada_Assign |
           Ada_Colon |
           Ada_Comma |
           Ada_Diamond |
           Ada_Divide |
           Ada_Dot |
           Ada_Doubledot |
           Ada_Equal |
           Ada_Gt |
           Ada_Gte |
           Ada_Label_End |
           Ada_Label_Start |
           Ada_Lt |
           Ada_Lte |
           Ada_Minus |
           Ada_Mult |
           Ada_Notequal |
           Ada_Par_Close |
           Ada_Par_Open |
           Ada_Pipe |
           Ada_Plus |
           Ada_Power |
           Ada_Semicolon |
           Ada_Tick |
           Ada_Target =>
            return Aspect_Prefix (None, In_Aspect);
         when Ada_String | Ada_Char =>
            return Aspect_Prefix (String_Style, In_Aspect);
         when Ada_Decimal |
              Ada_Integer =>
            return Aspect_Prefix (Number, In_Aspect);

         when Ada_Comment =>
            return Aspect_Prefix (Comment, In_Aspect);
         when Ada_Prep_Line =>
            return Aspect_Prefix (None, In_Aspect);
      end case;
   end To_Style;

   ---------------------
   -- Get_Known_Style --
   ---------------------

   function Get_Known_Style (Style : Style_Enum_Type) return String is
   begin
      if Style = None then
         return "";
      end if;

      return Known_Styles (Style).all;
   end Get_Known_Style;

   --------------------------------
   -- Update_Wrong_Literal_State --
   --------------------------------

   procedure Update_Wrong_Literal_State
     (State      : in out Boolean;
      Is_Error   : Boolean;
      Error_Text : Wide_Wide_String;
      Loc        : Source_Location_Range) is
   begin
      if Is_Error
        and then Error_Text'Length > 0
        and then Error_Text (Error_Text'First) = '"'
      then
         State := True;
      elsif Loc.Start_Line /= Loc.End_Line then
         State := False;
      end if;
   end Update_Wrong_Literal_State;

   --------------------------
   -- Highlight_Using_Tree --
   --------------------------

   procedure Highlight_Using_Tree
     (Self   : in out Highlightable_Interface'Class;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      From   : Integer := -1;
      To     : Integer := -1)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;

      function In_Ghost_Or_Aspect (Node : Ada_Node) return Boolean;
      --  Check if given Node somwhere in the Ghost or Aspect subtree

      function Highlight_Node (Node : Ada_Node'Class) return Visit_Status;
      --  Highlight given node

      procedure Highlight_Name (Node : Name'Class; Style : Style_Enum_Type);
      --  Highlight given name with Style style

      procedure Highlight_Name_List
        (List  : Alternatives_List;
         Style : Style_Enum_Type);
      --  Highlight each name found in the List with Style style

      procedure Highlight_Token
        (Token : Token_Reference;
         Style : Highlights_Holders.Syntax_Style_Enum_Type);
      --  Highlight given Token with Style style

      procedure Highlight_Tokens (Start_In_Aspect : Boolean);
      --  Iterate over all tokens in range From .. To and apply style
      --  according to syntax style in Highlights_Holder

      procedure Highlight_Trivia
        (Token         : in out Token_Reference;
         Wrong_Literal : in out Boolean;
         In_Aspect     : Boolean);
      --  While Index is a trivia highlight it and go to the next token.
      --  Track state of unfinished string literal in Wrong_Literal.

      From_Token : constant Token_Reference :=
                     (if From /= -1 then Unit.Lookup_Token
                        ((Line_Number (From), 1))
                      else
                         Unit.First_Token);

      To_Token : constant Token_Reference :=
                   (if To /= -1 then Unit.Lookup_Token
                      ((Line_Number (To + 1), 1))
                    else
                       Unit.Last_Token);

      Holder : Highlights_Holders.Highlights_Holder;

      --------------------
      -- Highlight_Name --
      --------------------

      procedure Highlight_Name (Node : Name'Class; Style : Style_Enum_Type) is
      begin
         if Node.Is_Null then
            --  This happens on ada_anonymous_type_decl for example
            return;
         end if;

         case Node.Kind is
            when Ada_Identifier | Ada_String_Literal =>
               declare
                  Token : constant Token_Reference := Node.Token_Start;
               begin
                  Highlight_Token (Token, Style);
               end;

            when Ada_Attribute_Ref =>
               --  Highlighting for Type'Class
               Highlight_Name (Node.As_Attribute_Ref.F_Prefix, Style);

            when Ada_Dotted_Name =>
               declare
                  Dotted : constant Dotted_Name := Node.As_Dotted_Name;
                  Prefix : constant Name := Dotted.F_Prefix;
                  Dot    : constant Token_Reference :=
                    Next (Prefix.Token_End, True);
               begin
                  Highlight_Name (Prefix, Style);
                  Highlight_Token (Dot, Style);
                  Highlight_Name (Dotted.F_Suffix, Style);
               end;

            when Ada_Defining_Name =>
                  Highlight_Name (Node.As_Defining_Name.F_Name, Style);

            when others =>
               null;
         end case;
      end Highlight_Name;

      -------------------------
      -- Highlight_Name_List --
      -------------------------

      procedure Highlight_Name_List
        (List  : Alternatives_List;
         Style : Style_Enum_Type) is
      begin
         for Node of List loop
            case Node.Kind is
               when Ada_Name =>
                  Highlight_Name (Node.As_Name, Style);
               when others =>
                  null;
            end case;
         end loop;
      end Highlight_Name_List;

      --------------------
      -- Highlight_Node --
      --------------------

      function Highlight_Node (Node : Ada_Node'Class) return Visit_Status is
      begin
         if Node.Token_End < From_Token or To_Token < Node.Token_Start then
            --  Skip uninteresting nodes to speedup traversal
            return Over;
         end if;

         case Node.Kind is
            when Ada_Subunit =>
               Highlight_Name (Node.As_Subunit.F_Name, Block);
            when Ada_Accept_Stmt_Range =>
               Highlight_Name (Node.As_Accept_Stmt.F_Name, Block);
            when Ada_Base_Type_Decl =>
               Highlight_Name (Node.As_Base_Type_Decl.F_Name, Block);
            when Ada_Single_Protected_Decl =>
               Highlight_Name (Node.As_Single_Protected_Decl.F_Name, Block);
            when Ada_Label_Decl =>
               Highlight_Name (Node.As_Label_Decl.F_Name, Block);
            when Ada_Named_Stmt_Decl =>
               Highlight_Name (Node.As_Named_Stmt_Decl.F_Name, Block);
            when Ada_Entry_Body =>
               Highlight_Name (Node.As_Entry_Body.F_Entry_Name, Block);
            when Ada_Entry_Spec =>
               Highlight_Name (Node.As_Entry_Spec.F_Entry_Name, Block);
            when Ada_Exception_Handler =>
               Highlight_Name
                 (Node.As_Exception_Handler.F_Exception_Name, Block);

               Highlight_Name_List
                 (Node.As_Exception_Handler.F_Handled_Exceptions, Type_Style);
            when Ada_Task_Body =>
               Highlight_Name (Node.As_Task_Body.F_Name, Block);
            when Ada_Task_Body_Stub =>
               Highlight_Name (Node.As_Task_Body_Stub.F_Name, Block);
            when Ada_Generic_Package_Renaming_Decl =>
               Highlight_Name
                 (Node.As_Generic_Package_Renaming_Decl.F_Name, Block);
            when Ada_Generic_Package_Instantiation =>
               Highlight_Name
                 (Node.As_Generic_Package_Instantiation.F_Name, Block);
            when Ada_Generic_Subp_Renaming_Decl =>
               Highlight_Name
                 (Node.As_Generic_Subp_Renaming_Decl.F_Name, Block);
            when Ada_Package_Body_Stub =>
               Highlight_Name (Node.As_Package_Body_Stub.F_Name, Block);
            when Ada_Package_Renaming_Decl =>
               Highlight_Name (Node.As_Package_Renaming_Decl.F_Name, Block);
            when Ada_Protected_Body =>
               Highlight_Name (Node.As_Protected_Body.F_Name, Block);
            when Ada_Protected_Body_Stub =>
               Highlight_Name (Node.As_Protected_Body_Stub.F_Name, Block);
            when Ada_Base_Package_Decl =>
               Highlight_Name
                 (Node.As_Base_Package_Decl.F_Package_Name, Block);
            when Ada_Subp_Spec =>
               Highlight_Name (Node.As_Subp_Spec.F_Subp_Name, Block);
            when Ada_Generic_Subp_Instantiation =>
               Highlight_Name
                 (Node.As_Generic_Subp_Instantiation.F_Subp_Name, Block);
            when Ada_Package_Body =>
               Highlight_Name (Node.As_Package_Body.F_Package_Name, Block);
            when Ada_End_Name =>
               Highlight_Name (Node.As_End_Name.F_Name, Block);

            when Ada_Subtype_Indication =>
               Highlight_Name (Node.As_Subtype_Indication.F_Name, Type_Style);

            when Ada_Parent_List =>
               --  Highlight the interface/multiheritance
               for Name of Node.As_Parent_List loop
                  Highlight_Name (Name, Type_Style);
               end loop;
            when Ada_Qual_Expr =>
               Highlight_Name (Node.As_Qual_Expr.F_Prefix, Type_Style);

            when Ada_Aspect_Assoc =>
               Holder.Set_Aspect (Node.Token_Start, Node.Token_End);
            when others =>
               null;
         end case;

         if Is_Ghost_Root_Node (Node) then
            Holder.Set_Aspect (Node.Token_Start, Node.Token_End);
         end if;

         return Into;
      end Highlight_Node;

      ---------------------
      -- Highlight_Token --
      ---------------------

      procedure Highlight_Token
        (Token : Token_Reference;
         Style : Highlights_Holders.Syntax_Style_Enum_Type) is
      begin
         if Token < From_Token or To_Token < Token then
            --  Skip uninteresting tokens
            return;
         end if;

         Holder.Set (Token, Style);
      end Highlight_Token;

      ----------------------
      -- Highlight_Tokens --
      ----------------------

      procedure Highlight_Tokens (Start_In_Aspect : Boolean) is

         In_Aspect     : Boolean := Start_In_Aspect;
         Token         : Token_Reference := From_Token;
         Wrong_Literal : Boolean := False;
      begin
         Highlight_Trivia (Token, Wrong_Literal, In_Aspect);

         while Token < To_Token loop

            declare
               Is_Keyword : constant Boolean := Libadalang.Analysis.Is_Keyword
                 (Token   => Token,
                  Version => Libadalang.Common.Ada_2012);

               Style : constant Highlights_Holders.Style_Subset :=
                 Holder.Get (Token);

               Token_Data : constant Token_Data_Type := Data (Token);
            begin
               In_Aspect := Style in Aspect | Aspect_Block | Aspect_Type;

               if Wrong_Literal or Style in None | Aspect then
                  --  No syntax style here, so apply lexical style
                  Self.Apply_Style
                    (Token => Token,
                     Style => To_Style
                       (Token      => Kind (Token_Data),
                        Is_Keyword => Is_Keyword,
                        In_Aspect  => In_Aspect,
                        In_String  => Wrong_Literal));
               else
                  --  Apply syntax style
                  Self.Apply_Style (Token => Token, Style => Style);
               end if;

               Token := Next (Token, Exclude_Trivia => False);

               exit when not (Token < To_Token);

               Highlight_Trivia (Token, Wrong_Literal, In_Aspect);
            end;
         end loop;
      end Highlight_Tokens;

      ----------------------
      -- Highlight_Trivia --
      ----------------------

      procedure Highlight_Trivia
        (Token         : in out Token_Reference;
         Wrong_Literal : in out Boolean;
         In_Aspect     : Boolean) is
      begin
         while Is_Trivia (Token) loop

            declare
               Token_Data : constant Token_Data_Type := Data (Token);
               Loc        : constant Source_Location_Range := Sloc_Range
                 (Token_Data);
               Error      : constant Boolean :=
                              Kind (Token_Data) in Ada_Lexing_Failure;
               Image      : constant Wide_Wide_String :=
                              (if Error then Text (Token) else "");
            begin
               Update_Wrong_Literal_State (Wrong_Literal, Error, Image, Loc);

               Self.Apply_Style
                 (Token => Token,
                  Style => To_Style
                    (Kind (Data (Token)),
                     False,
                     In_Aspect,
                     Wrong_Literal));

               Token := Next (Token, Exclude_Trivia => False);
            end;
         end loop;
      end Highlight_Trivia;

      ------------------------
      -- In_Ghost_Or_Aspect --
      ------------------------

      function In_Ghost_Or_Aspect (Node : Ada_Node) return Boolean is
      begin
         for J of Node.Parents loop
            if Is_Ghost_Root_Node (J) or else
              Node.Kind = Ada_Aspect_Assoc
            then
               return True;
            end if;
         end loop;

         return False;
      end In_Ghost_Or_Aspect;

      Root   : constant Ada_Node := Libadalang.Analysis.Root (Unit);
      Empty  : Boolean;
   begin
      if Root.Is_Null or else  No_Token in From_Token | To_Token then
         --  No tokens to highlight
         return;
      end if;

      Remove_Highlighting_on_Lines (Self, From, To);
      Holder.Initialize (From_Token, To_Token, Empty);

      if not Empty then
         --  Traverse whole tree, look for intresting nodes and mark their
         --  tokens in Holder for further processing
         Root.Traverse (Highlight_Node'Access);
      end if;

      declare
         First_Node : constant Ada_Node :=
                        (if From /= -1 then Root.Lookup
                           ((Line_Number (From), 1))
                         else
                            Root);
         In_Aspect : constant Boolean := In_Ghost_Or_Aspect (First_Node);
      begin
         Highlight_Tokens (In_Aspect);
      end;
   end Highlight_Using_Tree;

   ------------------------
   -- Is_Ghost_Root_Node --
   ------------------------

   function Is_Ghost_Root_Node
     (Node  : Libadalang.Analysis.Ada_Node'Class) return Boolean
   is
      function Has_Ghost
        (Aspect : Libadalang.Analysis.Aspect_Spec) return Boolean;
      --  Check if Aspect has Ghost identifier

      ---------------
      -- Has_Ghost --
      ---------------

      function Has_Ghost
        (Aspect : Libadalang.Analysis.Aspect_Spec) return Boolean
      is
         Ghost : constant Langkit_Support.Text.Text_Type := "Ghost";
      begin
         if Aspect.Is_Null then
            return False;
         end if;

         for X of Aspect.F_Aspect_Assocs.Children loop
            if X.As_Aspect_Assoc.F_Id.Text = Ghost then
               return True;
            end if;
         end loop;

         return False;
      end Has_Ghost;

   begin
      case Node.Kind is
         when Libadalang.Common.Ada_Basic_Decl =>
            return Has_Ghost (Node.As_Basic_Decl.F_Aspects);
         when others =>
            return False;
      end case;
   end Is_Ghost_Root_Node;

   ----------------------------------
   -- Remove_Highlighting_on_Lines --
   ----------------------------------

   procedure Remove_Highlighting_on_Lines
     (Self   : in out Highlightable_Interface'Class;
      From   : Integer;
      To     : Integer) is
   begin
      for J of Known_Styles loop
         Self.Remove_Highlighting
           (J.all, From, To);
      end loop;
   end Remove_Highlighting_on_Lines;

   -----------------
   -- Apply_Style --
   -----------------

   procedure Apply_Style
     (Self  : in out Highlightable_Interface'Class;
      Token : Libadalang.Common.Token_Reference;
      Style : Style_Enum_Type) is
   begin
      Self.Highlight_Token
        (Token => Token,
         Style => Get_Known_Style (Style));
   end Apply_Style;

end LAL.Highlighters;

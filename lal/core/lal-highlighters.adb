------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with Basic_Types;                      use Basic_Types;
with GNATCOLL.VFS;
with LAL.Core_Module;
with Langkit_Support.Slocs;
with Language;
with Libadalang.Analysis;

package body LAL.Highlighters is

   procedure Remove_Style
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Positive;
      To     : Positive);
   --  Remove any highlight related styles from text span in the Buffer

   function To_Style
     (Token     : Libadalang.Lexer.Token_Kind;
      In_Aspect : Boolean) return String;
   --  Get the name of a style name from a language token

   function Aspect_Prefix
     (Style     : String;
      In_Aspect : Boolean) return String;
   --  Append corresponding style prefix if In_Aspect = True

   function Kind_Of
     (Node  : Libadalang.Analysis.Ada_Node;
      Value : Libadalang.Analysis.Ada_Node_Kind_Type) return Boolean is
        (Node.Kind in Value);
   --  Check if given node has given kind

   generic
      type Node_Type is new Libadalang.Analysis.Ada_Node with private;
      --  Type of enclosing node, e.g. Accept_Stmt
      type Id_Type is new Libadalang.Analysis.Ada_Node with private;
      --  Type of the field in the enclosing node, e.g. Identifier or Name

      with function To_Node
        (Node : Libadalang.Analysis.Ada_Node'Class) return Node_Type;
      --  Cast from Ada_Node to Node_Type, e.g. As_Accept_Stmt
      with function Field (Node : Node_Type'Class) return Id_Type;
      --  Field getter function, e.g. F_Name

      --  Node kind range for given Node_Type, e.g. Ada_Accept_Stmt_Range'Range
      From : Libadalang.Analysis.Ada_Node_Kind_Type;
      To   : Libadalang.Analysis.Ada_Node_Kind_Type;
   function Generic_Match_Field
     (Node : Libadalang.Analysis.Ada_Node) return Boolean;
   --  Check if Node is stored in given Field of its parent.

   -------------------------
   -- Generic_Match_Field --
   -------------------------

   function Generic_Match_Field
     (Node : Libadalang.Analysis.Ada_Node) return Boolean
   is
      use type Libadalang.Analysis.Ada_Node;
      Parent : constant Libadalang.Analysis.Ada_Node := Node.Parent;
   begin
      if Parent.Kind in From .. To then
         declare
            Value : constant Id_Type := Field (To_Node (Parent));
         begin
            return Libadalang.Analysis.As_Ada_Node (Value) = Node;
         end;
      else
         return False;
      end if;
   end Generic_Match_Field;

   function Base_Type_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Base_Type_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Base_Type_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Base_Type_Decl'First,
      To        => Libadalang.Analysis.Ada_Base_Type_Decl'Last);

   function Single_Protected_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Single_Protected_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Single_Protected_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Single_Protected_Decl,
      To        => Libadalang.Analysis.Ada_Single_Protected_Decl);

   function Accept_Stmt_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Accept_Stmt,
      Id_Type   => Libadalang.Analysis.Identifier,
      To_Node   => Libadalang.Analysis.As_Accept_Stmt,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Accept_Stmt_Range'First,
      To        => Libadalang.Analysis.Ada_Accept_Stmt_Range'Last);

   function Label_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Label_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Label_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Label_Decl,
      To        => Libadalang.Analysis.Ada_Label_Decl);

   function Named_Stmt_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Named_Stmt_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Named_Stmt_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Named_Stmt_Decl,
      To        => Libadalang.Analysis.Ada_Named_Stmt_Decl);

   function Generic_Package_Instantiation_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Generic_Package_Instantiation,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Generic_Package_Instantiation,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Generic_Package_Instantiation,
      To        => Libadalang.Analysis.Ada_Generic_Package_Instantiation);

   function Generic_Subp_Renaming_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Generic_Subp_Renaming_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Generic_Subp_Renaming_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Generic_Subp_Renaming_Decl,
      To        => Libadalang.Analysis.Ada_Generic_Subp_Renaming_Decl);

   function Package_Body_Stub_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Package_Body_Stub,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Package_Body_Stub,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Package_Body_Stub,
      To        => Libadalang.Analysis.Ada_Package_Body_Stub);

   function Package_Renaming_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Package_Renaming_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Package_Renaming_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Package_Renaming_Decl,
      To        => Libadalang.Analysis.Ada_Package_Renaming_Decl);

   function Protected_Body_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Protected_Body,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Protected_Body,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Protected_Body,
      To        => Libadalang.Analysis.Ada_Protected_Body);

   function Protected_Body_Stub_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Protected_Body_Stub,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Protected_Body_Stub,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Protected_Body_Stub,
      To        => Libadalang.Analysis.Ada_Protected_Body_Stub);

   function Subunit_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Subunit,
      Id_Type   => Libadalang.Analysis.Name,
      To_Node   => Libadalang.Analysis.As_Subunit,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Subunit,
      To        => Libadalang.Analysis.Ada_Subunit);

   function Task_Body_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Task_Body,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Task_Body,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Task_Body,
      To        => Libadalang.Analysis.Ada_Task_Body);

   function Task_Body_Stub_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Task_Body_Stub,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Task_Body_Stub,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Task_Body_Stub,
      To        => Libadalang.Analysis.Ada_Task_Body_Stub);

   function Generic_Package_Renaming_Decl_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Generic_Package_Renaming_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Generic_Package_Renaming_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Generic_Package_Renaming_Decl,
      To        => Libadalang.Analysis.Ada_Generic_Package_Renaming_Decl);

   function Entry_Body_Entry_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Entry_Body,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Entry_Body,
      Field     => Libadalang.Analysis.F_Entry_Name,
      From      => Libadalang.Analysis.Ada_Entry_Body,
      To        => Libadalang.Analysis.Ada_Entry_Body);

   function Entry_Spec_Entry_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Entry_Spec,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Entry_Spec,
      Field     => Libadalang.Analysis.F_Entry_Name,
      From      => Libadalang.Analysis.Ada_Entry_Spec,
      To        => Libadalang.Analysis.Ada_Entry_Spec);

   function Base_Package_Decl_Package_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Base_Package_Decl,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Base_Package_Decl,
      Field     => Libadalang.Analysis.F_Package_Name,
      From      => Libadalang.Analysis.Ada_Base_Package_Decl'First,
      To        => Libadalang.Analysis.Ada_Base_Package_Decl'Last);

   function Subp_Spec_Subp_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Subp_Spec,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Subp_Spec,
      Field     => Libadalang.Analysis.F_Subp_Name,
      From      => Libadalang.Analysis.Ada_Subp_Spec,
      To        => Libadalang.Analysis.Ada_Subp_Spec);

   function Generic_Subp_Instantiation_Subp_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Generic_Subp_Instantiation,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Generic_Subp_Instantiation,
      Field     => Libadalang.Analysis.F_Subp_Name,
      From      => Libadalang.Analysis.Ada_Generic_Subp_Instantiation,
      To        => Libadalang.Analysis.Ada_Generic_Subp_Instantiation);

   function Package_Body_Package_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Package_Body,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Package_Body,
      Field     => Libadalang.Analysis.F_Package_Name,
      From      => Libadalang.Analysis.Ada_Package_Body,
      To        => Libadalang.Analysis.Ada_Package_Body);

   function Exception_Handler_Exception_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Exception_Handler,
      Id_Type   => Libadalang.Analysis.Defining_Name,
      To_Node   => Libadalang.Analysis.As_Exception_Handler,
      Field     => Libadalang.Analysis.F_Exception_Name,
      From      => Libadalang.Analysis.Ada_Exception_Handler,
      To        => Libadalang.Analysis.Ada_Exception_Handler);

   function Subtype_Indication_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Subtype_Indication,
      Id_Type   => Libadalang.Analysis.Name,
      To_Node   => Libadalang.Analysis.As_Subtype_Indication,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Subtype_Indication,
      To        => Libadalang.Analysis.Ada_Subtype_Indication);

   type Check_List is array (Positive range <>) of access
     function (Node : Libadalang.Analysis.Ada_Node) return Boolean;

   function Check
     (List : Check_List;
      Node : Libadalang.Analysis.Ada_Node) return Boolean is
        (for some Item of List => Item (Node));
   --  Find if there is at least one item in the List that matches Node

   --  List of places in LAL tree where a dotted_name should be highlithed
   --  with 'block' style
   Dotted_Name_List : constant Check_List :=
     (1 => Subunit_Name'Access);

   --  List of places in LAL tree where a defining_name should be highlithed
   --  with 'block' style
   Defining_Name_List : constant Check_List :=
      (Base_Type_Decl_Name'Access,
       Single_Protected_Decl_Name'Access,
       Label_Decl_Name'Access,
       Named_Stmt_Decl_Name'Access,
       Entry_Body_Entry_Name'Access,
       Entry_Spec_Entry_Name'Access,
       Exception_Handler_Exception_Name'Access,
       Task_Body_Name'Access,
       Task_Body_Stub_Name'Access,
       Generic_Package_Renaming_Decl_Name'Access,
       Generic_Package_Instantiation_Name'Access,
       Generic_Subp_Renaming_Decl_Name'Access,
       Package_Body_Stub_Name'Access,
       Package_Renaming_Decl_Name'Access,
       Protected_Body_Name'Access,
       Protected_Body_Stub_Name'Access,
       Base_Package_Decl_Package_Name'Access,
       Subp_Spec_Subp_Name'Access,
       Generic_Subp_Instantiation_Subp_Name'Access,
       Package_Body_Package_Name'Access
      );

   --  List of places in LAL tree where an identifier should be highlithed
   --  with 'block' style
   Id_List : constant Check_List := Dotted_Name_List &
      (1 => Accept_Stmt_Name'Access);

   -------------------
   -- Aspect_Prefix --
   -------------------

   function Aspect_Prefix
     (Style     : String;
      In_Aspect : Boolean) return String is
   begin
      if not In_Aspect then
         return Style;  --  Keep style unprefixed
      elsif Style = "" then
         return "aspect";
      else
         return "aspect_" & Style;
      end if;
   end Aspect_Prefix;

   --------------
   -- To_Style --
   --------------

   function To_Style
     (Token     : Libadalang.Lexer.Token_Kind;
      In_Aspect : Boolean) return String
   is
      use Libadalang.Lexer;
   begin
      case Token is
         when Ada_Termination |
              Ada_Lexing_Failure |
              Ada_Identifier =>
            return Aspect_Prefix ("", In_Aspect);
         when
              Ada_Abort |
              Ada_Abs |
              Ada_Abstract |
              Ada_Accept |
              Ada_Access |
              Ada_Aliased |
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
              Ada_Requeue |
              Ada_Return |
              Ada_Reverse |
              Ada_Select |
              Ada_Separate |
              Ada_Some |
              Ada_Subtype |
              Ada_Tagged |
              Ada_Task |
              Ada_Terminate |
              Ada_Then |
              Ada_Type |
              Ada_Until |
              Ada_Use |
              Ada_When |
              Ada_While |
              Ada_With |
              Ada_Xor =>
            return Aspect_Prefix ("keyword", In_Aspect);
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
            return Aspect_Prefix ("", In_Aspect);
         when Ada_String | Ada_Char =>
            return Aspect_Prefix ("string", In_Aspect);
         when Ada_Decimal |
              Ada_Integer =>
            return Aspect_Prefix ("number", In_Aspect);

         when Ada_Comment =>
            return Aspect_Prefix ("comment", In_Aspect);
         when Ada_Prep_Line =>
            return Aspect_Prefix ("", In_Aspect);
      end case;
   end To_Style;

   --------------------
   -- Highlight_Fast --
   --------------------

   not overriding procedure Highlight_Fast
     (Self   : in out Highlighter;
      Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer)
   is
      use Libadalang.Lexer.Token_Data_Handlers;
      First : constant GPS.Editors.Editor_Location'Class :=
        Buffer.New_Location_At_Line (From);
      Last : constant GPS.Editors.Editor_Location'Class :=
        Buffer.New_Location_At_Line (To).End_Of_Line;
      Index : Libadalang.Lexer.Token_Data_Handlers.Token_Or_Trivia_Index;
      Text : constant String := Buffer.Get_Chars (First, Last);
   begin
      Remove_Style (Buffer, From, To);

      Libadalang.Lexer.Lex_From_Buffer
        (Buffer      => Text,
         Charset     => "utf-8",
         Read_BOM    => False,
         TDH         => Self.TDH,
         Diagnostics => Self.Diags,
         With_Trivia => True);

      Index := First_Token_Or_Trivia (Self.TDH);

      while Index /= No_Token_Or_Trivia_Index loop
         declare
            Token : constant Libadalang.Lexer.Token_Data_Type :=
              Data (Index, Self.TDH);
            Style : constant String := To_Style (Token.Kind, False);
            Line  : constant Positive :=
              From + Natural (Token.Sloc_Range.Start_Line) - 1;
            Start : constant Visible_Column_Type :=
              Visible_Column_Type (Token.Sloc_Range.Start_Column);
            Stop : constant Visible_Column_Type :=
              Visible_Column_Type (Token.Sloc_Range.End_Column);
         begin
            if Style /= "" then
               Buffer.Apply_Style (Style, Line, Start, Stop);
            end if;
         end;

         Index := Next (Index, Self.TDH);
      end loop;

      Self.Diags.Clear;
   end Highlight_Fast;

   --------------------------
   -- Highlight_Using_Tree --
   --------------------------

   not overriding procedure Highlight_Using_Tree
     (Self   : in out Highlighter;
      Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer)
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      package L renames Libadalang.Lexer;

      type Context is record
         In_Aspect       : Boolean := False;
         In_Block_Name   : Boolean := False;
         In_Subtype_Mark : Boolean := False;
      end record;

      package Context_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Context);

      procedure Fill_Stack
        (Stack : in out Context_Lists.List;
         Node  : Ada_Node);
      --  Walk from tree root to the Node and collect contexts along the path

      procedure Adjust_Context
        (Value : in out Context;
         Node  : Ada_Node);
      --  Modify enclosing context according to the node

      procedure Next_Token
        (Index : in out Token_Type;
         Stack : in out Context_Lists.List;
         Node  : in out Ada_Node);
      --  Step to next token, find corresponding enclosing node and correct
      --  stack of contexts according to the new node.

      --------------------
      -- Adjust_Context --
      --------------------

      procedure Adjust_Context
        (Value : in out Context;
         Node  : Ada_Node) is
      begin
         --  Calculate In_Block_Name

         --  Ignore root node
         if Node.Parent.Is_Null then
            null;
         --  check if node is an aspect
         elsif Kind_Of (Node, Libadalang.Analysis.Ada_Aspect_Assoc) then
            Value.In_Aspect := True;

         --  Check if identifier itself should be highlighted
         elsif (Kind_Of (Node, Libadalang.Analysis.Ada_Identifier)
             and then Check (Id_List, Node))
           --  check if node is a dotted_name to be highlighted
           or else
             (Kind_Of (Node, Libadalang.Analysis.Ada_Dotted_Name)
              and then Check (Dotted_Name_List, Node))
           --  check if node is a defining_name to be highlighted
           or else
             (Kind_Of (Node,
                       Libadalang.Analysis.Ada_Defining_Name)
              and then Check (Defining_Name_List, Node))
           --  check if node is part of any end_name
           or else
             Kind_Of (Node.Parent,
                      Libadalang.Analysis.Ada_End_Name)
         then
            Value.In_Block_Name := True;

         --  Check if node is subtype mark in subtype indication
         elsif Subtype_Indication_Name (Node) then
            Value.In_Subtype_Mark := True;

         end if;
      end Adjust_Context;

      ----------------
      -- Fill_Stack --
      ----------------

      procedure Fill_Stack
        (Stack : in out Context_Lists.List;
         Node  : Ada_Node)
      is
      begin
         Stack.Clear;
         for Parent of reverse Node.Parents loop
            declare
               Value : Context;
            begin
               Adjust_Context (Value, Parent);
               Stack.Append (Value);
            end;
         end loop;
      end Fill_Stack;

      ----------------
      -- Next_Token --
      ----------------

      procedure Next_Token
        (Index : in out Token_Type;
         Stack : in out Context_Lists.List;
         Node  : in out Ada_Node) is
      begin
         Index := Next (Index);

         if Index /= No_Token then
            declare
               Again : Boolean := True;
               Token : constant Token_Data_Type := Data (Index);
               Loc   : constant Source_Location :=
                 Start_Sloc (Sloc_Range (Token));
            begin
               --  Pop stack and node until node enclosing the token is found
               while Node.Compare (Loc) /= Inside loop
                  if Node.Parent.Is_Null then
                     return;
                  end if;

                  Node := Node.Parent;
                  Stack.Delete_Last;
               end loop;

               --  Find any child of the node that encloses the token
               while Again loop
                  Again := False;

                  for Child of Node.Children loop
                     if not Child.Is_Null then
                        declare
                           Span : constant Source_Location_Range :=
                             Child.Sloc_Range;
                        begin
                           case Compare (Span, Loc) is
                              when After =>
                                 null;  --  token after child, go to next
                              when Inside =>
                                 declare
                                    Value : Context := Stack.Last_Element;
                                 begin
                                    --  descent into child enclosing token
                                    Node := Child;
                                    Adjust_Context (Value, Node);
                                    Stack.Append (Value);
                                    Again := True;
                                    exit;
                                 end;
                              when Before =>
                                 --  if not empty span
                                 exit when
                                   Start_Sloc (Span) /= End_Sloc (Span);
                           end case;
                        end;
                     end if;
                  end loop;
               end loop;
            end;
         end if;
      end Next_Token;

      Stack     : Context_Lists.List;
      From_Line : constant Line_Number := Line_Number (From);

      File : constant GNATCOLL.VFS.Virtual_File := Buffer.File;
      Unit : constant Analysis_Unit := Get_From_File
        (Context     => Self.Module.Context,
         Filename    => File.Display_Full_Name);

      Root  : constant Ada_Node := Libadalang.Analysis.Root (Unit);
      Index : Token_Type := Lookup_Token (Unit, (From_Line, 1));
      Node  : Ada_Node := Root.Lookup (Start_Sloc (Sloc_Range (Data (Index))));
   begin
      Remove_Style (Buffer, From, To);

      if Node.Is_Null then
         --  Keep node not null even if we look at token outside the tree
         Node := Root;
      end if;

      Fill_Stack (Stack, Node);

      while Index /= No_Token loop
         declare
            Done  : Boolean := False;
            Top   : constant Context := Stack.Last_Element;
            Token : constant Token_Data_Type := Data (Index);
            Style : constant String := To_Style (Kind (Token), Top.In_Aspect);
            Loc   : constant Source_Location_Range := Sloc_Range (Token);
            Line  : constant Positive := Positive (Loc.Start_Line);
            Start : constant Visible_Column_Type :=
              Visible_Column_Type (Loc.Start_Column);
            Stop  : constant Visible_Column_Type :=
              Visible_Column_Type (Loc.End_Column);
         begin
            exit when Line > To;

            if Kind (Token) in L.Ada_Identifier | L.Ada_Dot then
               if Top.In_Block_Name then
                  Buffer.Apply_Style
                    (Aspect_Prefix ("block", Top.In_Aspect),
                     Line,
                     Start,
                     Stop);
                  Done := True;
               elsif Top.In_Subtype_Mark then
                  Buffer.Apply_Style
                    (Aspect_Prefix ("type", Top.In_Aspect),
                     Line,
                     Start,
                     Stop);
                  Done := True;
               end if;
            end if;

            if not Done and then Style /= "" then
               Buffer.Apply_Style (Style, Line, Start, Stop);
            end if;

            Next_Token (Index, Stack, Node);
         end;
      end loop;
   end Highlight_Using_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Highlighter'Class;
      Module  : LAL.Core_Module.LAL_Module_Id) is
   begin
      Self.Module := Module.all'Access;
   end Initialize;

   ------------------
   -- Remove_Style --
   ------------------

   procedure Remove_Style
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Positive;
      To     : Positive) is
   begin
      for Line in From .. To loop
         Buffer.Remove_Style ("keyword", Line, 1);
         Buffer.Remove_Style ("string", Line, 1);
         Buffer.Remove_Style ("character", Line, 1);
         Buffer.Remove_Style ("number", Line, 1);
         Buffer.Remove_Style ("comment", Line, 1);
         Buffer.Remove_Style ("block", Line, 1);
         Buffer.Remove_Style ("type", Line, 1);
      end loop;
   end Remove_Style;

end LAL.Highlighters;

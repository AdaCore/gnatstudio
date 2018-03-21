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

with Basic_Types;                      use Basic_Types;
with GNATCOLL.VFS;
with LAL.Core_Module;
with Langkit_Support.Slocs;
with Language;
with Libadalang.Analysis;

package body LAL.Highlighters is

   procedure Remove_Style
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      Line   : Positive;
      Start  : Visible_Column_Type;
      Stop   : Visible_Column_Type);
   --  Remove any highlight related styles from text span in the Buffer

   function To_Style (E : Libadalang.Lexer.Token_Kind) return String;
   --  Get the name of a style name from a language token

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
   --  Check if Node stored in given Field of its parent.

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
      Id_Type   => Libadalang.Analysis.Identifier,
      To_Node   => Libadalang.Analysis.As_Base_Type_Decl,
      Field     => Libadalang.Analysis.F_Name,
      From      => Libadalang.Analysis.Ada_Base_Type_Decl'First,
      To        => Libadalang.Analysis.Ada_Base_Type_Decl'Last);

   function Exception_Handler_Exception_Name is new Generic_Match_Field
     (Node_Type => Libadalang.Analysis.Exception_Handler,
      Id_Type   => Libadalang.Analysis.Identifier,
      To_Node   => Libadalang.Analysis.As_Exception_Handler,
      Field     => Libadalang.Analysis.F_Exception_Name,
      From      => Libadalang.Analysis.Ada_Exception_Handler,
      To        => Libadalang.Analysis.Ada_Exception_Handler);

   Id_List : constant array (Positive range <>) of access
     function (Node : Libadalang.Analysis.Ada_Node) return Boolean :=
       (Base_Type_Decl_Name'Access,
        Exception_Handler_Exception_Name'Access);

   --------------
   -- To_Style --
   --------------

   function To_Style (E : Libadalang.Lexer.Token_Kind) return String is
      use Libadalang.Lexer;
   begin
      case E is
         when Ada_Termination |
              Ada_Lexing_Failure |
              Ada_Identifier =>
            return "";
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
            return "keyword";
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
            return "";
         when Ada_String =>
            return "string";
         when Ada_Char =>
            return "character";
         when Ada_Decimal |
              Ada_Integer =>
            return "number";

         when Ada_Comment =>
            return "comment";
         when Ada_Prep_Line =>
            return "";

            --  Other Standout_Language_Entity styles:
            --  "block";
            --  "type";
            --  "annotated_keyword";
            --  "annotated_comment";
            --  "aspect_keyword";
            --  "aspect_comment";
            --  "aspect";
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
            Style : constant String := To_Style (Token.Kind);
            Line  : constant Positive :=
              From + Natural (Token.Sloc_Range.Start_Line) - 1;
            Start : constant Visible_Column_Type :=
              Visible_Column_Type (Token.Sloc_Range.Start_Column);
            Stop : constant Visible_Column_Type :=
              Visible_Column_Type (Token.Sloc_Range.End_Column);
         begin
            if Style = "" then
               Remove_Style (Buffer, Line, Start, Stop);
            else
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

      From_Line : constant Line_Number := Line_Number (From);

      File : constant GNATCOLL.VFS.Virtual_File := Buffer.File;
      Unit : constant Analysis_Unit := Get_From_File
        (Context     => Self.Module.Context,
         Filename    => File.Display_Full_Name);

      Root  : constant Ada_Node := Libadalang.Analysis.Root (Unit);
      Index : Token_Type := Lookup_Token (Unit, (From_Line, 1));
   begin
      while Index /= No_Token loop
         declare
            Token : constant Token_Data_Type := Data (Index);
            Style : constant String := To_Style (Kind (Token));
            Loc   : constant Source_Location_Range := Sloc_Range (Token);
            Line  : constant Positive := Positive (Loc.Start_Line);
            Start : constant Visible_Column_Type :=
              Visible_Column_Type (Loc.Start_Column);
            Stop  : constant Visible_Column_Type :=
              Visible_Column_Type (Loc.End_Column);
         begin
            exit when Line > To;

            if Style /= "" then
               Buffer.Apply_Style (Style, Line, Start, Stop);
            elsif Kind (Token) in L.Ada_Identifier then
               declare
                  Node : constant Ada_Node := Root.Lookup
                    ((Loc.Start_Line, Loc.Start_Column));
               begin
                  if (for some Check of Id_List => Check (Node)) then
                     Buffer.Apply_Style ("block", Line, Start, Stop);
                  else
                     Remove_Style (Buffer, Line, Start, Stop);
                  end if;
               end;
            else
               Remove_Style (Buffer, Line, Start, Stop);
            end if;

            Index := Next (Index);
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
      Line   : Positive;
      Start  : Visible_Column_Type;
      Stop   : Visible_Column_Type) is
   begin
      Buffer.Remove_Style ("keyword", Line, Start, Stop);
      Buffer.Remove_Style ("string", Line, Start, Stop);
      Buffer.Remove_Style ("character", Line, Start, Stop);
      Buffer.Remove_Style ("number", Line, Start, Stop);
      Buffer.Remove_Style ("comment", Line, Start, Stop);
      Buffer.Remove_Style ("block", Line, Start, Stop);
   end Remove_Style;

end LAL.Highlighters;

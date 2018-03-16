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
with LAL.Core_Module;
with Language;

package body LAL.Highlighters is

   procedure Remove_Style
     (Buffer : GPS.Editors.Editor_Buffer'Class;
      Line   : Positive;
      Start  : Visible_Column_Type;
      Stop   : Visible_Column_Type);
   --  Remove any highlight related styles from text span in the Buffer

   function To_Style (E : Libadalang.Lexer.Token_Kind) return String;
   --  Get the name of a style name from a language token

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

   not overriding procedure Highlight_Using_Tree
     (Self   : in out Highlighter;
      Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer) renames Highlight_Fast;

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

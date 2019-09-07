------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with Ada.Exceptions;
with GNAT.Strings;

with Langkit_Support.Diagnostics;

with Pp.Actions;
with Pp.Command_Lines;               use Pp.Command_Lines;
with Pp.Scanner;                     use Pp.Scanner;

with GPS.Messages_Windows;
with String_Utils;
with Utils.Char_Vectors;             use Utils.Char_Vectors;
with Utils.Command_Lines.Common;     use Utils.Command_Lines.Common;

package body LAL.Ada_Languages is

   -------------------
   -- Format_Buffer --
   -------------------

   overriding procedure Format_Buffer
     (Lang                : access Ada_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null)
   is
      pragma Unreferenced
        (Is_Optional_Keyword, Case_Exceptions, Indent_Params);
      use Char_Vectors;
      use type Libadalang.Analysis.Ada_Node;

      function To_Char_Subrange
        (Buffer   : String;
         From, To : Natural) return Char_Subrange;
      --  Convert line range to character range

      ----------------------
      -- To_Char_Subrange --
      ----------------------

      function To_Char_Subrange
        (Buffer   : String;
         From, To : Natural) return Char_Subrange
      is
         Ok     : Boolean;
         Result : Utils.Char_Vectors.Char_Subrange := (Buffer'First, 0);
      begin
         for J in 2 .. From loop
            String_Utils.Next_Line
              (Buffer  => Buffer,
               P       => Result.First,
               Next    => Result.First,
               Success => Ok);
         end loop;

         if To > 0 then
            Result.Last := Result.First;

            for J in From .. To loop
               String_Utils.Next_Line
                 (Buffer  => Buffer,
                  P       => Result.Last,
                  Next    => Result.Last,
                  Success => Ok);
            end loop;
         else
            Result.Last := Buffer'Length;
         end if;

         return Result;
      end To_Char_Subrange;

      Unit       : Libadalang.Analysis.Analysis_Unit;
      Root       : Libadalang.Analysis.Ada_Node;
      Ok         : Boolean;
      Input      : Char_Vector;
      Output     : Char_Vector;
      Errors     : Pp.Scanner.Source_Message_Vector;
      To_Range   : Utils.Char_Vectors.Char_Subrange := (1, 0);
      From_Range : constant Utils.Char_Vectors.Char_Subrange :=
        To_Char_Subrange (Buffer, From, To);

   begin
      Append (Input, Buffer);

      Unit := Libadalang.Analysis.Get_From_Buffer
        (Context     => Lang.Context,
         Filename    => "aaa",  --  ??
         Buffer      => Buffer);

      Root := Libadalang.Analysis.Root (Unit);

      if Root = Libadalang.Analysis.No_Ada_Node then
         Lang.Kernel.Messages_Window.Insert_UTF8
           ("Error during parsing:",
            Mode => GPS.Messages_Windows.Error);

         for Error of Libadalang.Analysis.Diagnostics (Unit) loop
            Lang.Kernel.Messages_Window.Insert_UTF8
              (Langkit_Support.Diagnostics.To_Pretty_String (Error));
         end loop;

         return;
      end if;

      begin
         Pp.Actions.Format_Vector
           (Cmd       => Lang.Pp_Command_Line,
            Input     => Input,
            Node      => Root,
            In_Range  => From_Range,
            Output    => Output,
            Out_Range => To_Range,
            Messages  => Errors);
      exception
         when E : others =>
            Lang.Kernel.Messages_Window.Insert_UTF8
              ("PP raised exception:",
               Mode => GPS.Messages_Windows.Error);

            Lang.Kernel.Messages_Window.Insert_UTF8
              (Ada.Exceptions.Exception_Information (E));

            return;
      end;

      for Error of Errors loop
         Lang.Kernel.Messages_Window.Insert_UTF8
           (UTF8 => Sloc_Image (Error.Sloc) & " " & To_Array (Error.Text));
      end loop;

      declare
         Text : String renames Elems (Output) (1 .. Last_Index (Output));
      begin
         if To_Range.Last = 0 then
            --  Calculate To_Range if it was not set by Pp
            To_Range := To_Char_Subrange (Text, From, To);
         end if;

         declare
            Text_Position   : Positive := To_Range.First;
            Buffer_Position : Positive := From_Range.First;
            Text_Next       : Positive := To_Range.First;
            Buffer_Next     : Positive := From_Range.First;
            Line            : Positive := Positive'Max (From, 1);
         begin
            loop
               String_Utils.Next_Line
                 (Buffer  => Text,
                  P       => Text_Position,
                  Next    => Text_Next,
                  Success => Ok);

               String_Utils.Next_Line
                 (Buffer  => Buffer,
                  P       => Buffer_Position,
                  Next    => Buffer_Next,
                  Success => Ok);

               Replace
                 (Line => Line,
                  First => 1,
                  Last  => Buffer_Next - Buffer_Position,
                  Replace => Text (Text_Position .. Text_Next - 2));

               Line := Line + 1;
               Text_Position := Text_Next;
               Buffer_Position := Buffer_Next;

               exit when Text_Position >= To_Range.Last
                 or Buffer_Position >= From_Range.Last;
            end loop;
         end;
      end;
   end Format_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Ada_Language'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel;
      Context : Libadalang.Analysis.Analysis_Context)
   is
      Empty : aliased GNAT.Strings.String_List := (1 .. 0 => <>);
   begin
      Self.Kernel := Kernel;
      Self.Context := Context;
      Utils.Command_Lines.Parse
        (Empty'Unchecked_Access,
         Self.Pp_Command_Line,
         Utils.Command_Lines.Cmd_Line_1,
         Callback           => null,
         Collect_File_Names => False);

      --  Use the "--syntax-only" mode when using lalpp from GPS
      Common_Boolean_Switches.Set_Arg
        (Self.Pp_Command_Line, Syntax_Only, True);

      --  Use the "--source-line-breaks" which tells gnatpp to keep line breaks
      --  as they are in the input.
      Pp_Boolean_Switches.Set_Arg
        (Self.Pp_Command_Line, Source_Line_Breaks, True);

      --  Use the "--spaces-only" which avoids changing anything but
      --  the spaces. For example, "end;" does not get turned into
      --  "end Something;".

      Pp_Boolean_Switches.Set_Arg
        (Self.Pp_Command_Line, Spaces_Only, True);
   end Initialize;

end LAL.Ada_Languages;

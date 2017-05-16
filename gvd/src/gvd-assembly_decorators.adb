------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Regpat;           use GNAT.Regpat;

with Glib.Convert;          use Glib.Convert;
with String_Utils;          use String_Utils;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

package body GVD.Assembly_Decorators is

   Me : constant Trace_Handle := Create ("Assembly_Decorators", On);

   Location_Pattern : constant Pattern_Matcher := Compile
     ("^(0x[0-9a-zA-Z]+)\s+<.+>");

   --------------
   -- Decorate --
   --------------

   function Decorate
     (Self        : Decorator;
      Instruction : String;
      Registers   : Registers_Set.Set)
      return String
   is
      pragma Unreferenced (Self);

      Index   : Natural := Instruction'First;
      Start   : Natural;
      Result  : Ada.Strings.Unbounded.Unbounded_String;

      procedure Parse_Argument (Arg : String);

      --------------------
      -- Parse_Argument --
      --------------------

      procedure Parse_Argument (Arg : String) is
         Matched : Match_Array (0 .. 2);
      begin
         Match (Location_Pattern, Arg, Matched);
         if Matched (0) /= No_Match then
            Append (Result, "<span foreground=" & '"' & "blue" & '"' & ">" &
                      Escape_Text (Arg) & "</span>");

         elsif Arg (Arg'First) = '%'
           and then Registers.Contains (Arg (Arg'First + 1 .. Arg'Last))
         then
            Append (Result, "<span foreground=" & '"' & "red" & '"' & ">" &
                      Escape_Text (Arg) & "</span>");

         else
            Append (Result, Escape_Text (Arg));
         end if;

      end Parse_Argument;

   begin
      Skip_To_Blank (Instruction, Index);

      if Index > Instruction'First then
         Append (Result, "<b>" & Escape_Text
                 (Instruction (Instruction'First .. Index - 1)) & "</b>");

         while Index <= Instruction'Last
           and then
             (Instruction (Index) = ' '
              or else Instruction (Index) = ASCII.HT)
         loop
            Append (Result, " ");
            Index := Index + 1;
         end loop;

         if Index > Instruction'Last then
            return To_String (Result);
         end if;

         Start := Index;
         Skip_To_Char (Instruction, Index, ',');

         if Index < Instruction'Last then
            Parse_Argument (Instruction (Start .. Index - 1));
            Append (Result, Escape_Text (","));
            Parse_Argument (Instruction (Index + 1 .. Instruction'Last));

         else
            Parse_Argument (Instruction (Start .. Instruction'Last));
         end if;

         return To_String (Result);

      else
         return "<b>" & Escape_Text (Instruction) & "</b>";
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return "<parsing error>";
   end Decorate;

end GVD.Assembly_Decorators;

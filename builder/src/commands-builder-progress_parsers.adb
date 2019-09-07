------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

package body Commands.Builder.Progress_Parsers is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
   begin
      return new Progress_Parser'
        (Child   => Child,
         Matcher => Self.Matcher);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Progress_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Start    : Integer := Item'First;
      Matched  : Match_Array (0 .. 3);
      Buffer   : Unbounded_String;
      Progress : Commands.Progress_Record;

   begin
      while Start <= Item'Last loop
         Match (Self.Matcher.all, Item (Start .. Item'Last), Matched);
         exit when Matched (0) = No_Match;

         if Command /= null then
            Progress.Current := Natural'Value
              (Item (Matched (1).First .. Matched (1).Last));
            Progress.Total := Natural'Value
              (Item (Matched (2).First .. Matched (2).Last));
            Command.Set_Progress (Progress);
         end if;

         Append (Buffer, Item (Start .. Matched (0).First - 1));
         Start := Matched (0).Last + 1;
      end loop;

      Append (Buffer, Item (Start .. Item'Last));

      if Length (Buffer) /= 0 and Self.Child /= null then
         Self.Child.Parse_Standard_Output (To_String (Buffer), Command);
      end if;
   end Parse_Standard_Output;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Self    : access Output_Parser_Fabric;
      Pattern : String) is
   begin
      Self.Matcher := new Pattern_Matcher'(Compile (Pattern, Single_Line));
   end Set_Pattern;

end Commands.Builder.Progress_Parsers;

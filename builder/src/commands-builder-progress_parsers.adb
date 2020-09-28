------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
        (Child            => Child,
         Phase_Matcher    => Self.Phase_Matcher,
         Progress_Matcher => Self.Progress_Matcher);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Progress_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Start             : Integer := Item'First;
      Phase_Matched     : Match_Array (0 .. 1);
      Progress_Matched  : Match_Array (0 .. 3);
      Buffer            : Unbounded_String;
      Progress          : Commands.Progress_Record;

   begin
      while Start <= Item'Last loop
         Match
           (Self.Phase_Matcher.all,
            Item (Start .. Item'Last),
            Phase_Matched);
         Match
           (Self.Progress_Matcher.all,
            Item (Start .. Item'Last),
            Progress_Matched);

         exit when Phase_Matched (0) = No_Match
           and Progress_Matched (0) = No_Match;

         if Progress_Matched (0) = No_Match
           or else (Phase_Matched (0) /= No_Match
                      and then Phase_Matched (0).First
                                 < Progress_Matched (0).First)
         then
            if Command /= null then
               Command.Set_Label
                 (Item (Phase_Matched (1).First .. Phase_Matched (1).Last));
            end if;

            Append (Buffer, Item (Start .. Phase_Matched (0).First - 1));
            Start := Phase_Matched (0).Last + 1;

         else
            if Command /= null then
               Progress.Current := Natural'Value
                 (Item
                    (Progress_Matched (1).First .. Progress_Matched (1).Last));
               Progress.Total := Natural'Value
                 (Item
                    (Progress_Matched (2).First .. Progress_Matched (2).Last));
               Command.Set_Progress (Progress);
            end if;

            Append (Buffer, Item (Start .. Progress_Matched (0).First - 1));
            Start := Progress_Matched (0).Last + 1;
         end if;
      end loop;

      Append (Buffer, Item (Start .. Item'Last));

      if Length (Buffer) /= 0 and Self.Child /= null then
         Self.Child.Parse_Standard_Output (To_String (Buffer), Command);
      end if;
   end Parse_Standard_Output;

   -----------------------
   -- Set_Phase_Pattern --
   -----------------------

   procedure Set_Phase_Pattern
     (Self    : access Output_Parser_Fabric;
      Pattern : String) is
   begin
      Self.Phase_Matcher :=
        new Pattern_Matcher'(Compile (Pattern, Single_Line));
   end Set_Phase_Pattern;

   --------------------------
   -- Set_Progress_Pattern --
   --------------------------

   procedure Set_Progress_Pattern
     (Self    : access Output_Parser_Fabric;
      Pattern : String) is
   begin
      Self.Progress_Matcher :=
        new Pattern_Matcher'(Compile (Pattern, Single_Line));
   end Set_Progress_Pattern;

end Commands.Builder.Progress_Parsers;

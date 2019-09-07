------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

--  This package describes the base types used for the various replace actions.

with GNAT.Expect;

package GPS.Search.Replaces is

   type Replacement_Pattern is tagged private;
   --  Parsed representation of Replace_String

   procedure Initialize
     (Self            : in out Replacement_Pattern;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Is_Regexp       : Boolean);
   --  Parse Replace_String in given Case_Preserving/Is_Regexp mode.
   --  If Case_Preserving is True, then case of Replacement_Text will be
   --  changed to the case of matched text.
   --  If Is_Regexp is True, then substring of Replace_String will be
   --  interpreted is follow way:
   --  a) \0 - replaced by whole matched text
   --  b) \1 .. \9 - replaced by matched regexp group 1 .. 9

   function Replacement_Text
     (Pattern      : Replacement_Pattern;
      Result       : GPS.Search.Search_Context;
      Matched_Text : String;
      Keywords     : GNAT.Expect.Pattern_Matcher_Access)
      return String;
   --  Return replacement text based on replacement pattern

   procedure Reset (Self : in out Replacement_Pattern);
   --  Called whenever a new search/replace will start (as opposed to
   --  continuing the current one through the Next button).

   procedure Free (Result : in out Replacement_Pattern);
   --  Free cached Replacement_Pattern

private

   type Casing_Type is (Lower, Upper, Smart_Mixed, Unchanged);

   function Guess_Casing
     (S        : String;
      Keywords : GNAT.Expect.Pattern_Matcher_Access)
      return Casing_Type;
   --  Guess the casing which is used in S, keywords are not takken in account.
   --  S is encoded in UTF-8.

   function To_Casing
     (S        : String;
      Casing   : Casing_Type;
      Keywords : GNAT.Expect.Pattern_Matcher_Access)
      return String;
   --  Return S transformed to match Casing.
   --  If S is not all lower-case, return S unchanged.
   --  S is encoded in UTF-8, and so is the result.
   --  Lower case is used for keywords.

   type Subexpression is abstract tagged null record;
   type Subexpression_Access is access all Subexpression'Class;
   --  Abstract type to provide text of replace subexpression such as "\1"

   function Origin_Length (Self : Subexpression) return Positive is abstract;
   --  Length of replace subexpression in origin text. For example 2 for "\1"

   function Replace
     (Self    : access Subexpression;
      Context : GPS.Search.Search_Context;
      Matched : String) return String is abstract;
   --  Provide text of given replace subexpression

   procedure Reset (Self : access Subexpression) is null;
   --  Reset subexpression to original state

   type Regexp_Reference is record
      Offset : Positive;
      Object : Subexpression_Access;
   end record;
   --  Reference to one Regexp subexpression in Replace_String

   type Regexp_Reference_Array is
     array (Positive range <>) of Regexp_Reference;
   type Regexp_Reference_Array_Access is access all Regexp_Reference_Array;
   --  Array elements ordered by Offset field

   type Replacement_Pattern is tagged record
      Replace_String   : GNAT.Strings.String_Access := null;
      --  Cached Replace_String
      Is_Replace_Lower : Boolean := False;
      --  replace string has lower case
      Case_Preserving  : Boolean;
      --  Current Case_Preserving option
      Is_Regexp        : Boolean;
      --  Current regexp mode
      References       : Regexp_Reference_Array_Access;
      --  References to regexp subexpressions in Replace_String
      Last             : Natural := 0;
      --  Last valid element in References;
   end record;

end GPS.Search.Replaces;

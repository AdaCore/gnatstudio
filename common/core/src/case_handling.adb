------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Wide_Wide_Latin_1;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

package body Case_Handling is

   procedure Add_Exception
     (HTable    : Exceptions_Table;
      Str       : String;
      Read_Only : Boolean);
   --  Add Str exception in HTable

   procedure Remove_Exception (HTable : Exceptions_Table; Str : String);
   --  Remove str exception from the HTable

   function Mixed_Case
     (Image : Wide_Wide_String;
      Smart : Boolean := False) return Wide_Wide_String;
   --  The same as one in package specification, but on Wide_Wide_String.

   ----------------
   -- Mixed_Case --
   ----------------

   function Mixed_Case
     (Image : Wide_Wide_String;
      Smart : Boolean := False) return Wide_Wide_String
   is
      package Latin_1 renames Ada.Characters.Wide_Wide_Latin_1;

      Do_Upper : Boolean;
      C        : Wide_Wide_Character;
      Result : Unbounded_Wide_Wide_String;
   begin
      if Image'Length = 0 then
         return Image;
      end if;

      Do_Upper := True;

      for Index in Image'Range loop
         C := Image (Index);

         if C = '.'
           or else C = '_'
           or else C = ' '
           or else C = '('
           or else C = ')'
           or else C = '+'
           or else C = '-'
           or else C = '*'
           or else C = '/'
           or else C = ','
           or else C = ';'
           or else C = '''
           or else C = Latin_1.HT
           or else C = Latin_1.LF
           or else C = Latin_1.CR
         then
            Do_Upper := True;
            Append (Result, C);
         else
            if Do_Upper then
               Append (Result, Ada.Wide_Wide_Characters.Handling.To_Upper (C));
            elsif not Smart then
               Append (Result, To_Lower (C));
            else
               Append (Result, C);
            end if;

            Do_Upper := False;
         end if;
      end loop;

      return To_Wide_Wide_String (Result);
   end Mixed_Case;

   ----------------
   -- Mixed_Case --
   ----------------

   function Mixed_Case
     (S : UTF8_String; Smart : Boolean := False) return UTF8_String
   is
      Image  : constant Wide_Wide_String := Decode (S);
   begin
      return Encode (Mixed_Case (Image, Smart));
   end Mixed_Case;

   ---------------
   --  Set_Case --
   ---------------

   function Set_Case
     (C      : Casing_Exceptions;
      Word   : UTF8_String;
      Casing : Casing_Type) return UTF8_String
   is
      function Set_Substring_Exception
        (Word : Wide_Wide_String) return UTF8_String;
      --  Apply substring exception to word if possible.

      -----------------------------
      -- Set_Substring_Exception --
      -----------------------------

      function Set_Substring_Exception
        (Word : Wide_Wide_String) return UTF8_String
      is
         procedure Apply (Substring : Wide_Wide_String);
         --  Check if a substring exception exists for this substring and
         --  apply it.

         Result : Unbounded_Wide_Wide_String;

         -----------
         -- Apply --
         -----------

         procedure Apply (Substring : Wide_Wide_String) is
            --  Set L_Str with the key for Str in the exception hash table
            L_Word  : constant Wide_Wide_String := To_Lower (Substring);

            Pos : Cursor;
         begin
            Pos := C.S.Find (L_Word);

            if Has_Element (Pos) then
               Append (Result, Element (Pos).Word);
            else
               Append (Result, Substring);
            end if;
         end Apply;

         First : Natural := Word'First - 1;
      begin
         if C.S = null then
            return Encode (Word);
         end if;

         --  Look for all substring in this word

         for K in Word'Range loop
            if Word (K) = '_' then
               Apply (Word (First + 1 .. K - 1));
               First := K;
               Append (Result, '_');
            end if;
         end loop;

         --  Apply to the last one

         Apply (Word (First + 1 .. Word'Last));

         return Encode (To_Wide_Wide_String (Result));
      end Set_Substring_Exception;

      Image  : constant Wide_Wide_String := Decode (Word);
      L_Str  : constant Wide_Wide_String := To_Lower (Image);
      N      : Cursor;
   begin
      if Casing = Unchanged then
         --  Nothing to do in this case
         return Word;
      end if;

      --  Now we check for the case exception for this word. If found we
      --  just return the record casing, if not set we set the word casing
      --  according to the rule set in Casing.

      if C.E /= null then
         N := C.E.Find (L_Str);
      end if;

      if not Has_Element (N) then
         --  No case exception for this word, apply standard rules

         case Casing is
            when Unchanged =>
               return Set_Substring_Exception (Image);

            when Upper =>
               return Set_Substring_Exception (To_Upper (Image));

            when Lower =>
               return Set_Substring_Exception (L_Str);

            when Mixed =>
               return Set_Substring_Exception (Mixed_Case (Image));

            when Smart_Mixed =>
               return Set_Substring_Exception
                 (Mixed_Case (Image, Smart => True));
         end case;

      else
         --  We have found a case exception
         return Encode (Element (N).Word);
      end if;
   end Set_Case;

   -------------------
   -- Add_Exception --
   -------------------

   procedure Add_Exception
     (HTable    : Exceptions_Table;
      Str       : String;
      Read_Only : Boolean)
   is
      Image : constant Wide_Wide_String := Decode (Str);
   begin
      HTable.Include (To_Lower (Image), (Image'Length, Read_Only, Image));
   end Add_Exception;

   procedure Add_Exception
     (C         : in out Casing_Exceptions;
      Word      : String;
      Read_Only : Boolean) is
   begin
      Add_Exception (C.E, Word, Read_Only);
   end Add_Exception;

   -----------------------------
   -- Add_Substring_Exception --
   -----------------------------

   procedure Add_Substring_Exception
     (C         : in out Casing_Exceptions;
      Substring : String;
      Read_Only : Boolean) is
   begin
      Add_Exception (C.S, Substring, Read_Only);
   end Add_Substring_Exception;

   ----------------------
   -- Remove_Exception --
   ----------------------

   procedure Remove_Exception
     (HTable : Exceptions_Table;
      Str    : String)
   is
      Image : constant Wide_Wide_String := Decode (Str);
      L_Str : constant Wide_Wide_String := To_Lower (Image);
      Pos   : Cursor := HTable.Find (L_Str);
   begin
      if Has_Element (Pos) and then not Element (Pos).Read_Only then
         HTable.Delete (Pos);
      end if;
   end Remove_Exception;

   procedure Remove_Exception (C : in out Casing_Exceptions; Word : String) is
   begin
      Remove_Exception (C.E, Word);
   end Remove_Exception;

   --------------------------------
   -- Remove_Substring_Exception --
   --------------------------------

   procedure Remove_Substring_Exception
     (C         : in out Casing_Exceptions;
      Substring : String) is
   begin
      Remove_Exception (C.S, Substring);
   end Remove_Substring_Exception;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (C : in out Casing_Exceptions) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Map, Exceptions_Table);
   begin
      --  Word exceptions

      if C.E /= null then
         C.E.Clear;
         Unchecked_Free (C.E);
      end if;

      --  Substring exceptions

      if C.S /= null then
         C.S.Clear;
         Unchecked_Free (C.S);
      end if;
   end Destroy;

end Case_Handling;

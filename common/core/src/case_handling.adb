------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Glib.Unicode;               use Glib.Unicode;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

package body Case_Handling is

   procedure Add_Exception
     (HTable    : Exceptions_Table;
      Str       : String;
      Read_Only : Boolean);
   --  Add Str exception in HTable

   procedure Remove_Exception (HTable : Exceptions_Table; Str : String);
   --  Remove str exception from the HTable

   ----------
   -- Free --
   ----------

   procedure Free (N : in out W_Node) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (String, Word_Access);
   begin
      Unchecked_Free (N.Word);
   end Free;

   ----------------
   -- Mixed_Case --
   ----------------

   function Mixed_Case
     (S : UTF8_String; Smart : Boolean := False) return UTF8_String
   is
      Do_Upper : Boolean;
      Index    : Natural := S'First;
      Last     : Natural;
      C        : Gunichar;
      X        : UTF8_String := S;
   begin
      if S'Length = 0 then
         return S;
      end if;

      Do_Upper := True;

      while Index <= S'Last loop
         C := UTF8_Get_Char (S (Index .. S'Last));

         if C = Character'Pos ('.')
           or else C = Character'Pos ('_')
           or else C = Character'Pos (' ')
           or else C = Character'Pos ('(')
           or else C = Character'Pos (')')
           or else C = Character'Pos ('+')
           or else C = Character'Pos ('-')
           or else C = Character'Pos ('*')
           or else C = Character'Pos ('/')
           or else C = Character'Pos (',')
           or else C = Character'Pos (';')
           or else C = Character'Pos (''')
           or else C = Character'Pos (ASCII.HT)
           or else C = Character'Pos (ASCII.LF)
           or else C = Character'Pos (ASCII.CR)
         then
            Do_Upper := True;

         else
            if Do_Upper then
               Unichar_To_UTF8 (To_Upper (C), X (Index .. S'Last), Last);
            elsif not Smart then
               Unichar_To_UTF8 (To_Lower (C), X (Index .. S'Last), Last);
            end if;

            Do_Upper := False;
         end if;

         Index := UTF8_Next_Char (S, Index);
      end loop;

      return X;
   end Mixed_Case;

   ---------------
   --  Set_Case --
   ---------------

   function Set_Case
     (C      : Casing_Exceptions;
      Word   : UTF8_String;
      Casing : Casing_Type) return UTF8_String
   is
      function Set_Substring_Exception (Word : UTF8_String) return UTF8_String;
      --  Apply substring exception to word if possible.

      -----------------------------
      -- Set_Substring_Exception --
      -----------------------------

      function Set_Substring_Exception
        (Word : UTF8_String) return UTF8_String
      is
         procedure Apply (Substring : String);
         --  Check if a substring exception exists for this substring and
         --  apply it.

         Result : Unbounded_String;

         -----------
         -- Apply --
         -----------

         procedure Apply (Substring : String) is
            --  Set L_Str with the key for Str in the exception hash table
            L_Word : constant String := UTF8_Strdown (Substring);
            N : W_Node;
         begin
            N := String_Hash_Table.Get (C.S.all, L_Word);

            if N.Word = null then
               Append (Result, Substring);
            else
               Append (Result, N.Word.all);
            end if;
         end Apply;

         First : Natural := Word'First - 1;
      begin
         if C.S = null then
            return Word;
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

         return To_String (Result);
      end Set_Substring_Exception;

      N      : W_Node;
   begin
      if Casing = Unchanged then
         --  Nothing to do in this case
         return Word;
      end if;

      --  Now we check for the case exception for this word. If found we
      --  just return the record casing, if not set we set the word casing
      --  according to the rule set in Casing.

      if C.E /= null then
         N := String_Hash_Table.Get (C.E.all, UTF8_Strdown (Word));
      end if;

      if N.Word = null then
         --  No case exception for this word, apply standard rules

         case Casing is
            when Unchanged =>
               return Set_Substring_Exception (Word);

            when Upper =>
               return Set_Substring_Exception (UTF8_Strup (Word));

            when Lower =>
               return Set_Substring_Exception (UTF8_Strdown (Word));

            when Mixed =>
               return Set_Substring_Exception (Mixed_Case (Word));

            when Smart_Mixed =>
               return Set_Substring_Exception
                 (Mixed_Case (Word, Smart => True));
         end case;

      else
         --  We have found a case exception
         return N.Word.all;
      end if;
   end Set_Case;

   -------------------
   -- Add_Exception --
   -------------------

   procedure Add_Exception
     (HTable    : Exceptions_Table;
      Str       : String;
      Read_Only : Boolean) is
   begin
      String_Hash_Table.Set
        (HTable.all, To_Lower (Str), (Read_Only, new String'(Str)));
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
      L_Str : constant String := To_Lower (Str);
      N     : W_Node;
   begin
      N := String_Hash_Table.Get (HTable.all, L_Str);

      if not N.Read_Only then
         String_Hash_Table.Remove (HTable.all, L_Str);
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
        (String_Hash_Table.Instance, Exceptions_Table);
   begin
      --  Word exceptions

      if C.E /= null then
         String_Hash_Table.Reset (C.E.all);
         Unchecked_Free (C.E);
      end if;

      --  Substring exceptions

      if C.S /= null then
         String_Hash_Table.Reset (C.S.all);
         Unchecked_Free (C.S);
      end if;
   end Destroy;

end Case_Handling;

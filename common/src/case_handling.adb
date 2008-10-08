-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2004-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

with Glib.Unicode;               use Glib.Unicode;

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

   procedure Mixed_Case (S : in out UTF8_String; Smart : Boolean := False) is
      Do_Upper : Boolean;
      Index    : Natural := S'First;
      Last     : Natural;
   begin
      if S'Length = 0 then
         return;
      end if;

      Do_Upper := True;

      while Index <= S'Last loop
         if S (Index) = '.'
           or S (Index) = '_'
           or S (Index) = ' '
           or S (Index) = ASCII.HT
           or S (Index) = ASCII.LF
           or S (Index) = ASCII.CR
         then
            Do_Upper := True;

         else
            if Do_Upper then
               Unichar_To_UTF8
                 (To_Upper (UTF8_Get_Char (S (Index .. S'Last))),
                  S (Index .. S'Last), Last);
            elsif not Smart then
               Unichar_To_UTF8
                 (To_Lower (UTF8_Get_Char (S (Index .. S'Last))),
                  S (Index .. S'Last), Last);
            end if;

            Do_Upper := False;
         end if;

         Index := UTF8_Next_Char (S, Index);
      end loop;
   end Mixed_Case;

   ---------------
   --  Set_Case --
   ---------------

   procedure Set_Case
     (C      : Casing_Exceptions;
      Word   : in out UTF8_String;
      Casing : Casing_Type)
   is
      procedure Set_Substring_Exception
        (Word   : in out String;
         L_Word : String);
      --  Apply substring exception to word if possible and set Found to true
      --  in this case.

      -----------------------------
      -- Set_Substring_Exception --
      -----------------------------

      procedure Set_Substring_Exception
        (Word   : in out String;
         L_Word : String)
      is
         procedure Apply (Substring : String);
         --  Check if a substring exception exists for this substring and
         --  apply it.

         -----------
         -- Apply --
         -----------

         procedure Apply (Substring : String) is
            N : W_Node;
         begin
            N := String_Hash_Table.Get (C.S.all, Substring);

            if N.Word /= null then
               Word (Substring'Range) := N.Word.all;
            end if;
         end Apply;

         First : Natural;
      begin
         First := L_Word'First - 1;

         --  Look for all substring in this word

         for K in L_Word'Range loop
            if L_Word (K) = '_' then
               Apply (L_Word (First + 1 .. K - 1));
               First := K;
            end if;
         end loop;

         --  Apply to the last one

         Apply (L_Word (First + 1 .. L_Word'Last));
      end Set_Substring_Exception;

      L_Word : String (Word'Range);
      N      : W_Node;
   begin
      if Casing = Unchanged then
         --  Nothing to do in this case
         return;
      end if;

      --  Set L_Str with the key for Str in the exception hash table

      L_Word := UTF8_Strdown (Word);

      --  Now we check for the case exception for this word. If found we
      --  just return the record casing, if not set we set the word casing
      --  according to the rule set in Casing.

      if C.E /= null then
         N := String_Hash_Table.Get (C.E.all, L_Word);
      end if;

      if N.Word = null then
         --  No case exception for this word, apply standard rules

         case Casing is
            when Unchanged =>
               null;

            when Upper =>
               Word := UTF8_Strup (L_Word);

            when Lower =>
               Word := L_Word;

            when Mixed =>
               Mixed_Case (Word);

            when Smart_Mixed =>
               Mixed_Case (Word, Smart => True);
         end case;

         --  Check now for substring exceptions

         if C.S /= null then
            Set_Substring_Exception (Word, L_Word);
         end if;

      else
         --  We have found a case exception
         Word := N.Word.all;
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
        (String_Hash_Table.HTable, Exceptions_Table);
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

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2004                         --
--                            ACT-Europe                             --
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

--  The case exception file is an XML file with the following structure.
--
--  <?xml version="1.0"?>
--  <custom_section>
--     <case_exceptions>
--        <word>OS_Lib</word>
--        <word>GNAT</word>
--        <substring>IO</substring>
--     </case_exceptions>
--  </custom_section>
--
--  <word>       : A full case exception word.
--  <substring>  : A substring exception. A substring is defined as a part
--                 of the word separated by underscores.

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Traces;                     use Traces;
with Glib.Xml_Int;               use Glib.Xml_Int;
with XML_Parsers;

package body Case_Handling is

   Me : constant Debug_Handle := Create ("Case_Handling");

   procedure Add_Exception
     (HTable    : in out Exceptions_Table;
      Str       : String;
      Read_Only : Boolean);
   --  Add Str exception in HTable

   procedure Remove_Exception (HTable : in out Exceptions_Table; Str : String);
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

   procedure Mixed_Case (S : in out String) is
      Dot : Boolean := False;
   begin
      S (S'First) := To_Upper (S (S'First));

      for J in S'First + 1 .. S'Last loop
         if Dot or else S (J - 1) = '_' then
            S (J) := To_Upper (S (J));
         else
            S (J) := To_Lower (S (J));
         end if;

         if S (J) = '.' then
            Dot := True;
         elsif S (J) /= ' '
           and then S (J) /= ASCII.HT
           and then S (J) /= ASCII.LF
           and then S (J) /= ASCII.CR
         then
            Dot := False;
         end if;
      end loop;
   end Mixed_Case;

   ----------------------
   -- Smart_Mixed_Case --
   ----------------------

   procedure Smart_Mixed_Case (S : in out String) is
      Dot : Boolean := False;
   begin
      S (S'First) := To_Upper (S (S'First));

      for J in S'First + 1 .. S'Last loop
         if Dot or else S (J - 1) = '_' then
            S (J) := To_Upper (S (J));
         end if;

         if S (J) = '.' then
            Dot := True;
         elsif S (J) /= ' '
           and then S (J) /= ASCII.HT
           and then S (J) /= ASCII.LF
           and then S (J) /= ASCII.CR
         then
            Dot := False;
         end if;
      end loop;
   end Smart_Mixed_Case;

   ---------------
   --  Set_Case --
   ---------------

   procedure Set_Case
     (C      : Casing_Exceptions;
      Word   : in out String;
      Casing : Casing_Type)
   is
      procedure Set_Substring_Exception
        (Word   : in out String;
         L_Word : String;
         Found  : out Boolean);
      --  Apply substring exception to word if possible and set Found to true
      --  in this case.

      -----------------------------
      -- Set_Substring_Exception --
      -----------------------------

      procedure Set_Substring_Exception
        (Word   : in out String;
         L_Word : String;
         Found  : out Boolean)
      is
         procedure Apply (Substring : String);
         --  Check if a substring exception exists for this substring and
         --  apply it.

         procedure Apply (Substring : String) is
            N : W_Node;
         begin
            N := String_Hash_Table.Get (C.S.all, Substring);

            if N.Word /= null then
               Word (Substring'Range) := N.Word.all;
               Found := True;
            end if;
         end Apply;

         First : Natural;
      begin
         Found := False;

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

      for J in Word'Range loop
         L_Word (J) := To_Lower (Word (J));
      end loop;

      --  Now we check for the case exception for this word. If found we
      --  just return the record casing, if not set we set the word casing
      --  according to the rule set in Casing.

      if C.E /= null then
         N := String_Hash_Table.Get (C.E.all, L_Word);
      end if;

      if N.Word = null then
         --  No case exception for this word, or case exceptions feature
         --  not activated. Check now for substring exceptions.

         if C.S /= null then
            declare
               Found : Boolean;
            begin
               Set_Substring_Exception (Word, L_Word, Found);
               if Found then
                  --  Some substring exceptions have been found, return now
                  return;
               end if;
            end;
         end if;

         --  No casing exceptions found, apply standard rules

         case Casing is
            when Unchanged =>
               null;

            when Upper =>
               for J in Word'Range loop
                  Word (J) := To_Upper (Word (J));
               end loop;

            when Lower =>
               Word := L_Word;

            when Mixed =>
               Mixed_Case (Word);

            when Smart_Mixed =>
               Smart_Mixed_Case (Word);
         end case;

      else
         --  We have found a case exception
         Word := N.Word.all;
      end if;
   end Set_Case;

   -------------------
   -- Add_Exception --
   -------------------

   procedure Add_Exception
     (HTable    : in out Exceptions_Table;
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
     (HTable : in out Exceptions_Table;
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

   ---------------------
   -- Load_Exceptions --
   ---------------------

   procedure Load_Exceptions
     (C         : in out Casing_Exceptions;
      Filename  : String;
      Read_Only : Boolean)
   is
      File, Child : Node_Ptr;
      Err         : String_Access;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);

         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Trace (Me, Err.all);
            Free (Err);

         else
            --  Get node exceptions

            Child := File.Child;

            --  Get node exception

            Child := Child.Child;

            while Child /= null loop
               if Child.Tag.all = "word" then
                  Add_Exception (C, Child.Value.all, Read_Only);
               elsif Child.Tag.all = "substring" then
                  Add_Substring_Exception (C, Child.Value.all, Read_Only);
               else
                  Trace (Exception_Handle,
                         "Unknown casing exceptions node " & Child.Tag.all);
               end if;
               Child := Child.Next;
            end loop;

            Free (File);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Load_Exceptions;

   ---------------------
   -- Save_Exceptions --
   ---------------------

   procedure Save_Exceptions (C : in Casing_Exceptions; Filename : String) is
      File, Ada_Child : Node_Ptr;
      Child           : Node_Ptr;
      Iter            : String_Hash_Table.Iterator;
      N               : W_Node;
   begin
      if C.E /= null or else C.S /= null then
         File     := new Node;
         File.Tag := new String'("custom_section");

         Ada_Child     := new Node;
         Ada_Child.Tag := new String'("case_exceptions");
         Add_Child (File, Ada_Child);

         --  Word exceptions

         if C.E /= null then
            String_Hash_Table.Get_First (C.E.all, Iter);

            loop
               N := String_Hash_Table.Get_Element (Iter);
               exit when N = Null_Node;

               if not N.Read_Only then
                  Child       := new Node;
                  Child.Tag   := new String'("word");
                  Child.Value := new String'(N.Word.all);
                  Add_Child (Ada_Child, Child);
               end if;

               String_Hash_Table.Get_Next (C.E.all, Iter);
            end loop;
         end if;

         --  Substring exceptions

         if C.S /= null then
            String_Hash_Table.Get_First (C.S.all, Iter);

            loop
               N := String_Hash_Table.Get_Element (Iter);
               exit when N = Null_Node;

               if not N.Read_Only then
                  Child       := new Node;
                  Child.Tag   := new String'("substring");
                  Child.Value := new String'(N.Word.all);
                  Add_Child (Ada_Child, Child);
               end if;

               String_Hash_Table.Get_Next (C.S.all, Iter);
            end loop;
         end if;

         Trace (Me, "Saving " & Filename);
         Print (File, Filename);
         Free (File);
      end if;
   end Save_Exceptions;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (C : in out Casing_Exceptions) is
      procedure Free is new Ada.Unchecked_Deallocation
        (String_Hash_Table.HTable, Exceptions_Table);
   begin
      --  Word exceptions

      if C.E /= null then
         String_Hash_Table.Reset (C.E.all);
         Free (C.E);
      end if;

      --  Substring exceptions

      if C.S /= null then
         String_Hash_Table.Reset (C.S.all);
         Free (C.E);
      end if;
   end Destroy;

end Case_Handling;

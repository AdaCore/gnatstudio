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
--     </case_exceptions>
--  </custom_section>

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Traces;                     use Traces;
with Glib.Xml_Int;               use Glib.Xml_Int;

package body Case_Handling is

   Me : constant Debug_Handle := Create ("Case_Handling");

   ----------
   -- Free --
   ----------

   procedure Free (N : in out W_Node) is
      procedure Free is new Ada.Unchecked_Deallocation (String, Word_Access);
   begin
      Free (N.Word);
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
     (C      : in Casing_Exceptions;
      Word   : in out String;
      Casing : Casing_Type)
   is
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
         --  not activated.

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
     (C         : in out Casing_Exceptions;
      Word      : String;
      Read_Only : Boolean) is
   begin
      String_Hash_Table.Set
        (C.E.all, To_Lower (Word), (Read_Only, new String'(Word)));
   end Add_Exception;

   ----------------------
   -- Remove_Exception --
   ----------------------

   procedure Remove_Exception (C : in out Casing_Exceptions; Word : String) is
      L_Word : constant String := To_Lower (Word);
      N      : W_Node;
   begin
      N := String_Hash_Table.Get (C.E.all, L_Word);

      if not N.Read_Only then
         String_Hash_Table.Remove (C.E.all, To_Lower (Word));
      end if;
   end Remove_Exception;

   ---------------------
   -- Load_Exceptions --
   ---------------------

   procedure Load_Exceptions
     (C         : in out Casing_Exceptions;
      Filename  : String;
      Read_Only : Boolean)
   is
      File, Child : Node_Ptr;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);

         File := Parse (Filename);

         --  Get node exceptions

         Child := File.Child;

         --  Get node exception

         Child := Child.Child;

         while Child /= null loop
            Add_Exception (C, Child.Value.all, Read_Only);
            Child := Child.Next;
         end loop;

         Free (File);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      File     := new Node;
      File.Tag := new String'("custom_section");

      Ada_Child     := new Node;
      Ada_Child.Tag := new String'("case_exceptions");
      Add_Child (File, Ada_Child);

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

      Trace (Me, "Saving " & Filename);
      Print (File, Filename);
      Free (File);
   end Save_Exceptions;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (C : in out Casing_Exceptions) is
      procedure Free is new Ada.Unchecked_Deallocation
        (String_Hash_Table.HTable, Exceptions_Table);
   begin
      if C.E /= null then
         String_Hash_Table.Reset (C.E.all);
         Free (C.E);
      end if;
   end Destroy;

end Case_Handling;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Characters.Handling;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body String_List_Utils is

   use String_List;

   ----------------------
   -- Copy_String_List --
   ----------------------

   function Copy_String_List
     (S : String_List.Vector) return String_List.Vector is
   begin
      return S;
   end Copy_String_List;

   --------------------------------
   -- Less_Than_Case_Insensitive --
   --------------------------------

   function Less_Than_Case_Insensitive (Item1, Item2 : String)
      return Boolean is
   begin
      return Ada.Characters.Handling.To_Lower (Item1)
        < Ada.Characters.Handling.To_Lower (Item2);
   end Less_Than_Case_Insensitive;

   ----------------------
   -- Remove_From_List --
   ----------------------

   procedure Remove_From_List
     (L               : in out String_List.Vector;
      S               : String;
      All_Occurrences : Boolean := True)
   is
      Node : Cursor := L.Find (S);
   begin
      while Has_Element (Node) loop
         L.Delete (Node);

         if not All_Occurrences then
            return;
         end if;

         Node := L.Find (S);
      end loop;
   end Remove_From_List;

   ----------------
   -- Is_In_List --
   ----------------

   function Is_In_List (L : String_List.Vector; S : String) return Boolean is
   begin
      return L.Contains (S);
   end Is_In_List;

   -----------------------
   -- Add_Unique_Sorted --
   -----------------------

   procedure Add_Unique_Sorted
     (L : in out String_List.Vector;
      S : String)
   is
      N : Cursor;
      P : Cursor;
   begin
      if L.Is_Empty then
         L.Append (S);

         return;
      end if;

      N := L.First;

      while Has_Element (N)
        and then Element (N) < S
      loop
         P := N;
         Next (N);
      end loop;

      if Has_Element (N)
        and then Element (N) = S
      then
         return;
      else
         if not Has_Element (P) then
            L.Prepend (S);
         else
            L.Insert (P, S);
         end if;
      end if;
   end Add_Unique_Sorted;

   ---------------------------
   -- List_To_Argument_List --
   ---------------------------

   function List_To_Argument_List
     (L : String_List.Vector) return GNAT.OS_Lib.Argument_List
   is
      Args   : Argument_List (1 .. Natural (L.Length));
      Index  : Natural := Args'First;
   begin
      for Item of L loop
         Args (Index) := new String'(Item);
         Index := Index + 1;
      end loop;

      return Args;
   end List_To_Argument_List;

   --------------------
   -- Longest_Prefix --
   --------------------

   function Longest_Prefix (L : String_List.Vector) return String is
   begin
      if L.Is_Empty then
         return "";
      end if;

      declare
         Node    : Cursor          := L.First;
         First_S : constant String := Element (Node);
         Length  : Natural         := First_S'Length;
      begin
         Next (Node);
         while Has_Element (Node) loop
            declare
               Data_S : constant String := Element (Node);
            begin
               Length := Natural'Min (Length, Data_S'Length);
               while Length > 0
                 and then First_S
                   (First_S'First .. First_S'First + Length - 1) /=
                   Data_S (Data_S'First .. Data_S'First + Length - 1)
               loop
                  Length := Length - 1;
               end loop;

               --  No need to continue further if there is no prefix
               exit when Length = 0;
            end;

            Next (Node);
         end loop;

         return First_S (First_S'First .. First_S'First + Length - 1);
      end;
   end Longest_Prefix;

   function Longest_Prefix
     (L : GNAT.Strings.String_List_Access) return String is
   begin
      if L = null or else L'Length = 0 then
         return "";
      end if;

      declare
         First_S : constant String := L (L'First).all;
         Length  : Natural         := First_S'Length;
      begin
         for K in L'Range loop
            Length := Natural'Min (Length, L (K)'Length);
            while Length > 0
              and then First_S
                (First_S'First .. First_S'First + Length - 1) /=
                L (K) (L (K)'First .. L (K)'First + Length - 1)
            loop
               Length := Length - 1;
            end loop;

            --  No need to continue further if there is no prefix
            exit when Length = 0;
         end loop;

         return First_S (First_S'First .. First_S'First + Length - 1);
      end;
   end Longest_Prefix;

end String_List_Utils;

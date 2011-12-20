------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
     (S : String_List.List) return String_List.List
   is
      Result : String_List.List;
      Temp   : List_Node := First (S);

   begin
      while Temp /= Null_Node loop
         Append (Result, Data (Temp));
         Temp := Next (Temp);
      end loop;

      return Result;
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

   -----------------
   -- String_Free --
   -----------------

   procedure String_Free (S : in out String) is
      pragma Unreferenced (S);
   begin
      null;
   end String_Free;

   ----------------------
   -- Remove_From_List --
   ----------------------

   procedure Remove_From_List
     (L               : in out String_List.List;
      S               : String;
      All_Occurrences : Boolean := True)
   is
      Node      : List_Node;
      Prev_Node : List_Node := Null_Node;
   begin
      Node := First (L);

      while Node /= Null_Node loop
         if Data (Node) = S then
            Remove_Nodes (L, Prev_Node, Node);

            if not All_Occurrences then
               return;
            end if;

            if Prev_Node = Null_Node then
               Node := First (L);
            else
               Node := Prev_Node;
            end if;

            if Node = Null_Node then
               return;
            end if;
         end if;

         Prev_Node := Node;
         Node := Next (Node);
      end loop;
   end Remove_From_List;

   ----------------
   -- Is_In_List --
   ----------------

   function Is_In_List (L : String_List.List; S : String) return Boolean is
      Node : List_Node := First (L);
   begin
      while Node /= Null_Node loop
         if S = Data (Node) then
            return True;
         end if;

         Node := Next (Node);
      end loop;

      return False;
   end Is_In_List;

   ------------------------
   --  Add_Unique_Sorted --
   ------------------------

   procedure Add_Unique_Sorted
     (L : in out String_List.List;
      S : String)
   is
      N : List_Node;
      P : List_Node;
   begin
      if Is_Empty (L) then
         Append (L, S);

         return;
      end if;

      N := First (L);

      while N /= Null_Node
        and then Data (N) < S
      loop
         P := N;
         N := Next (N);
      end loop;

      if N /= Null_Node
        and then Data (N) = S
      then
         return;
      else
         if P = Null_Node then
            Prepend (L, S);
         else
            Append (L, P, S);
         end if;
      end if;
   end Add_Unique_Sorted;

   ---------------------------
   -- List_To_Argument_List --
   ---------------------------

   function List_To_Argument_List
     (L : String_List.List) return GNAT.OS_Lib.Argument_List
   is
      Length : constant Natural := String_List.Length (L);
      Args   : Argument_List (1 .. Length);
      Index  : Natural := Args'First;
      Node   : List_Node := First (L);
   begin
      while Node /= Null_Node loop
         Args (Index) := new String'(Data (Node));
         Index := Index + 1;
         Node := Next (Node);
      end loop;

      return Args;
   end List_To_Argument_List;

   --------------------
   -- Longest_Prefix --
   --------------------

   function Longest_Prefix (L : String_List.List) return String is
   begin
      if L = Null_List then
         return "";
      end if;

      declare
         Node    : List_Node       := First (L);
         First_S : constant String := Data (Node);
         Length  : Natural         := First_S'Length;
      begin
         Node := Next (Node);
         while Node /= Null_Node loop
            declare
               Data_S : constant String := Data (Node);
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

            Node := Next (Node);
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

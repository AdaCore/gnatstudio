-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package body String_List_Utils is

   use String_List;

   procedure Split (L : in out String_List.List;
                    L1, L2 : out String_List.List);
   function Fuse (L_1, L_2 : String_List.List) return String_List.List;
   --  Utilities for the Sort procedure.

   ----------------------
   -- Copy_String_List --
   ----------------------

   function Copy_String_List
     (S : in String_List.List) return String_List.List
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

   -----------------
   -- String_Free --
   -----------------

   procedure String_Free (S : in out String) is
      pragma Unreferenced (S);
   begin
      null;
   end String_Free;

   ----------
   -- Fuse --
   ----------

   function Fuse (L_1, L_2 : String_List.List) return String_List.List is
      L  : List;
      L1 : List := L_1;
      L2 : List := L_2;

   begin
      while not Is_Empty (L1)
        and then not Is_Empty (L2)
      loop
         if Head (L1) < Head (L2) then
            Append (L, Head (L1));
            Next (L1);
         else
            Append (L, Head (L2));
            Next (L2);
         end if;
      end loop;

      if Is_Empty (L1) then
         Concat (L, L2);
      else
         Concat (L, L1);
      end if;

      return L;
   end Fuse;

   -----------
   -- Split --
   -----------

   procedure Split (L : in out String_List.List;
                    L1, L2 : out String_List.List)
   is
      Target : Natural := 1;
   begin
      while not Is_Empty (L) loop
         if Target = 1 then
            Append (L1, Head (L));
            Target := 2;
         else
            Append (L2, Head (L));
            Target := 1;
         end if;
         Next (L);
      end loop;
   end Split;

   ----------
   -- Sort --
   ----------

   procedure Sort (L : in out String_List.List)
   is
      L1, L2 : List;
   begin
      if Is_Empty (L) then
         return;
      end if;

      if Length (L) = 1 then
         return;
      end if;

      Split (L, L1, L2);
      Sort (L1);
      Sort (L2);
      L := Fuse (L1, L2);
   end Sort;

end String_List_Utils;

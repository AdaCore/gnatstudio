------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

package body List_Utils is

   ----------
   -- Sort --
   ----------

   procedure Sort (L : in out List_Pkg.List) is
      use List_Pkg;

      procedure Split
        (L : in out List_Pkg.List; L1, L2 : out List_Pkg.List);
      --  Split L into two sublists of the same size.

      function Fuse (L_1, L_2 : List_Pkg.List) return List_Pkg.List;
      --  Merge the two lists, so that the result is sorted.

      -----------
      -- Split --
      -----------

      procedure Split
        (L : in out List_Pkg.List; L1, L2 : out List_Pkg.List)
      is
         Ln           : List_Node := First (L);
         Target_Is_L1 : Boolean := True;
      begin
         while Ln /= Null_Node loop
            if Target_Is_L1 then
               Append (L1, Data (Ln));
            else
               Append (L2, Data (Ln));
            end if;

            Target_Is_L1 := not Target_Is_L1;
            Ln := Next (Ln);
         end loop;
         Free (L, Free_Data => False);
      end Split;

      ----------
      -- Fuse --
      ----------

      function Fuse (L_1, L_2 : List_Pkg.List) return List_Pkg.List is
         L  : List;
         L1 : List_Node := First (L_1);
         L2 : List_Node := First (L_2);
      begin
         loop
            if L1 = Null_Node then
               exit when L2 = Null_Node;

               Append (L, Data (L2));
               L2 := Next (L2);

            elsif L2 = Null_Node then
               Append (L, Data (L1));
               L1 := Next (L1);

            elsif Data (L1) < Data (L2) then
               Append (L, Data (L1));
               L1 := Next (L1);

            else
               Append (L, Data (L2));
               L2 := Next (L2);
            end if;
         end loop;
         return L;
      end Fuse;

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
      Free (L1, Free_Data => False);
      Free (L2, Free_Data => False);
   end Sort;

end List_Utils;

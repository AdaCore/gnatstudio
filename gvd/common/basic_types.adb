-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Basic_Types is

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_Array) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out Argument_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   procedure Free (Ar : in out String_Array_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation (String_Array, String_Array_Access);
   begin
      if Ar /= null then
         Free (Ar.all);
         Free (Ar);
      end if;
   end Free;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (List1, List2   : Argument_List;
      Case_Sensitive : Boolean := True) return Boolean is
   begin
      if List1'Length /= List2'Length then
         return False;

      else
         declare
            L1 : Argument_List := List1;
            L2 : Argument_List := List2;
         begin
            for A in L1'Range loop
               for B in L2'Range loop
                  if L2 (B) /= null and then
                    ((Case_Sensitive and then L1 (A).all = L2 (B).all)
                     or else
                     (not Case_Sensitive
                      and then To_Lower (L1 (A).all) =
                               To_Lower (L2 (B).all)))
                  then
                     L1 (A) := null;
                     L2 (B) := null;
                     exit;
                  end if;
               end loop;
            end loop;

            return L1 = (L1'Range => null)
              and then L2 = (L2'Range => null);
         end;
      end if;
   end Is_Equal;

end Basic_Types;

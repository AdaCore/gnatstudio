-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with String_Utils;   use String_Utils;
with GNAT.Case_Util; use GNAT.Case_Util;

package body String_Hash is

   -----------
   -- Equal --
   -----------

   function Equal (Key1, Key2 : String) return Boolean is
   begin
      if Case_Sensitive then
         return Key1 = Key2;
      else
         return Case_Insensitive_Equal (Key1, Key2);
      end if;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Name_Htable_Num is
      function Internal is new HTables.Hash (Name_Htable_Num);
   begin
      if Case_Sensitive then
         return Internal (Key);
      else
         declare
            K : String := Key;
         begin
            To_Lower (K);
            return Internal (K);
         end;
      end if;
   end Hash;

end String_Hash;

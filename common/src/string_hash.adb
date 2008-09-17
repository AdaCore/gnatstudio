-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2002-2008, AdaCore                  --
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

with GNAT.Case_Util; use GNAT.Case_Util;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package body String_Hash is

   -----------
   -- Equal --
   -----------

   function Equal (Key1, Key2 : String) return Boolean is
   begin
      return Equal (Key1, Key2, Case_Sensitive => Case_Sensitive);
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

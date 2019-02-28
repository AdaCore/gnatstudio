------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with GNAT.Case_Util; use GNAT.Case_Util;
with GNATCOLL.Utils; use GNATCOLL.Utils;
with String_Utils;

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
      function Internal is new String_Utils.Hash (Name_Htable_Num);
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

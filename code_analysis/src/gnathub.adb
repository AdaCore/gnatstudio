------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Ada.Strings.Hash;
with Ada.Characters.Handling;

package body GNAThub is

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : Severity_Record)
                      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Characters.Handling.To_Lower
             (Message_Importance_Type'Image (Item.Ranking)));
   end Get_Name;

   ----------
   -- Hash --
   ----------

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Message_Importance_Type'Image (Item.Ranking));
   end Hash;

   ----------
   -- Less --
   ----------

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      if Left.Name /= Right.Name then
         return Left.Name < Right.Name;
      else
         return Left.Identifier < Right.Identifier;
      end if;
   end Less;

   ----------
   -- Less --
   ----------

   function Less (L, R : GNAThub.Severity_Access) return Boolean is
   begin
      return L.Ranking > R.Ranking;
   end Less;

end GNAThub;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with Ada.Strings.Unbounded.Hash;

package body GNAThub is

   ----------
   -- Hash --
   ----------

   function Hash (Item : Severity_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash (Item.Name);
   end Hash;

   ----------
   -- Less --
   ----------

   function Less (Left : Rule_Access; Right : Rule_Access) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      return Left.Name < Right.Name;
   end Less;

end GNAThub;

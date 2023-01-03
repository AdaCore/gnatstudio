------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with VSS.String_Vectors;
with VSS.Strings;

generic
   Variants : VSS.String_Vectors.Virtual_String_Vector;

package Minimal_Perfect_Hash is
   --  pragma Preelaborate;

   function Get_Index (Text : VSS.Strings.Virtual_String) return Natural;
   --  Return index of Text in Variants or zero if Variants doesn't containt
   --  Text.

   procedure Initialize (Seed : Natural := 0);
   --  Performe internal initialization.

end Minimal_Perfect_Hash;

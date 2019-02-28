------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

--  Items used for string types.

package GVD.Variables.Types.Simples.Strings is

   type GVD_String_Type is new GVD_Simple_Type with private;
   type GVD_String_Type_Access is access all GVD_String_Type'Class;

   function New_String_Type return GVD_Type_Holder;
   --  Create a new string value.

private

   type GVD_String_Type is new GVD_Simple_Type with null record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_String_Type) return String is ("String");

end GVD.Variables.Types.Simples.Strings;

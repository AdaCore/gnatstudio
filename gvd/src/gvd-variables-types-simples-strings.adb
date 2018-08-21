------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

package body GVD.Variables.Types.Simples.Strings is

   ---------------------
   -- New_String_Type --
   ---------------------

   function New_String_Type return GVD_Type_Holder is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_String_Type);
   begin
      return GVD_Type_Holder'(Ada.Finalization.Controlled with Data);
   end New_String_Type;

end GVD.Variables.Types.Simples.Strings;

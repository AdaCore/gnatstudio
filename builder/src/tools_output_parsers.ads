------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

--  Root of filters hierarchy to parse output of tools running by GPS.
--  Filters organized in the chains.

package Tools_Output_Parsers is

   type Tools_Output_Parser is abstract tagged private;
   type Tools_Output_Parser_Access is access all Tools_Output_Parser'Class;

   procedure Parse_Standard_Output
     (Self : not null access Tools_Output_Parser;
      Item : String);

   procedure Parse_Standard_Error
     (Self : not null access Tools_Output_Parser;
      Item : String);

   procedure End_Of_Stream (Self : not null access Tools_Output_Parser);

   function Child
     (Self : not null access Tools_Output_Parser'Class)
      return access Tools_Output_Parser'Class;

   procedure Destroy (Self : not null access Tools_Output_Parser);
   --  Free internal allocated data

   procedure Free (Self : in out Tools_Output_Parser_Access);
   --  Deallocate Self object

private

   type Tools_Output_Parser is abstract tagged record
      Child : Tools_Output_Parser_Access;
   end record;

end Tools_Output_Parsers;

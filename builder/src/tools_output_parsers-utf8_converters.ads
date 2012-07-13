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

--  Declare parser to convert output to UTF-8 encoding.

with GPS.Kernel;

package Tools_Output_Parsers.UTF8_Converters is

   type UTF8_Converter is new Tools_Output_Parser with private;
   --  This parser converts output to UTF-8 encoding

   function Create_UTF8_Converter
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Child  : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to convert to UTF-8 encoding

   overriding procedure Parse_Standard_Output
     (Self : not null access UTF8_Converter;
      Item : String);

private

   type UTF8_Converter is new Tools_Output_Parser with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

end Tools_Output_Parsers.UTF8_Converters;

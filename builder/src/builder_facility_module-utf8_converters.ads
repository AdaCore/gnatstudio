------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Commands; use Commands;

with GPS.Kernel;

with GPS.Tools_Output;                 use GPS.Tools_Output;

package Builder_Facility_Module.UTF8_Converters is

   type UTF8_Converter is new Tools_Output_Parser with private;
   --  This parser converts output to UTF-8 encoding

   overriding procedure Parse_Standard_Output
     (Self    : not null access UTF8_Converter;
      Item    : String;
      Command : access Root_Command'Class);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   procedure Set
     (Self   : access Output_Parser_Fabric;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to convert to UTF-8 encoding

private

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with record
      Kernel     : GPS.Kernel.Kernel_Handle;
   end record;

   type UTF8_Converter is new Tools_Output_Parser with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

end Builder_Facility_Module.UTF8_Converters;

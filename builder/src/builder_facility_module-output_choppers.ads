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

--  Declare parser to round output by line bounds.

with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with GPS.Kernel.Tools_Output;          use GPS.Kernel.Tools_Output;

package Builder_Facility_Module.Output_Choppers is

   type Output_Chopper is new Tools_Output_Parser with private;
   --  This parser rounds output by line bounds

   overriding procedure Parse_Standard_Output
     (Self : not null access Output_Chopper;
      Item : String);

   overriding
   procedure End_Of_Stream (Self : not null access Output_Chopper);

   type Output_Parser_Fabric is
     new GPS.Kernel.Tools_Output.Output_Parser_Fabric with private;

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to round output by line bounds.

private

   type Output_Parser_Fabric is
     new GPS.Kernel.Tools_Output.Output_Parser_Fabric with null record;

   type Output_Chopper is new Tools_Output_Parser with record
      Buffer : Unbounded_String;
   end record;

end Builder_Facility_Module.Output_Choppers;

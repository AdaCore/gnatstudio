------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

--  Declare parser to split text to lines and pass to child line by line.

with Commands; use Commands;
with GPS.Tools_Output;                 use GPS.Tools_Output;

package Builder_Facility_Module.Text_Splitters is

   type Text_Splitter is new Tools_Output_Parser with private;
   --  This parser splits text to lines

   overriding procedure Parse_Standard_Output
     (Self    : not null access Text_Splitter;
      Item    : String;
      Command : access Root_Command'Class);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to split text to lines

private

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with null record;

   type Text_Splitter is new Tools_Output_Parser with null record;

end Builder_Facility_Module.Text_Splitters;

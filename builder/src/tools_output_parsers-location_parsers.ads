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

--  Declare parser to fill Locations view and highlight locations in editors.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPS.Kernel;
with GPS.Styles.UI;

package Tools_Output_Parsers.Location_Parsers is

   type Location_Parser is new Tools_Output_Parser with private;

   function Create_Location_Parser
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Category          : String;
      Styles            : GPS.Styles.UI.Builder_Message_Styles;
      Show_In_Locations : Boolean;
      Child             : Tools_Output_Parser_Access := null)
      return Tools_Output_Parser_Access;
   --  Create new parser to fill Locations view under given Category, if
   --  Show_In_Locations = True, otherwise show messages only in the editors.
   --  Use Styles to highlight locations in editor.

   overriding procedure Parse_Standard_Output
     (Self : not null access Location_Parser;
      Item : String);

private

   type Location_Parser is new Tools_Output_Parser with record
      Kernel            : GPS.Kernel.Kernel_Handle;
      Category          : Unbounded_String;
      Styles            : GPS.Styles.UI.Builder_Message_Styles;
      Show_In_Locations : Boolean;
   end record;

end Tools_Output_Parsers.Location_Parsers;

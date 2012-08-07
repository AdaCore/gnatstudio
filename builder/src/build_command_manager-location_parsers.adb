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

with GPS.Kernel.Messages.Tools_Output;

package body Build_Command_Manager.Location_Parsers is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is
   begin
      return new Location_Parser'
        (Child             => Child,
         Kernel            => Self.Kernel,
         Category          => Self.Category,
         Styles            => Self.Styles,
         Show_In_Locations => Self.Show_In_Locations);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self : not null access Location_Parser;
      Item : String) is
   begin
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Self.Kernel,
         Item,
         Category          => To_String (Self.Category),
         Highlight         => True,
         Styles            => Self.Styles,
         Show_In_Locations => Self.Show_In_Locations);

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self              : access Output_Parser_Fabric;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Category          : String;
      Styles            : GPS.Styles.UI.Builder_Message_Styles;
      Show_In_Locations : Boolean) is
   begin
      Self.Kernel := GPS.Kernel.Kernel_Handle (Kernel);
      Self.Category := To_Unbounded_String (Category);
      Self.Styles := Styles;
      Self.Show_In_Locations := Show_In_Locations;
   end Set;

end Build_Command_Manager.Location_Parsers;

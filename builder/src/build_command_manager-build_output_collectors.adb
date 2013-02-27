------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Builder_Facility_Module;

package body Build_Command_Manager.Build_Output_Collectors is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is
   begin
      return new Build_Output_Collector'
        (Child      => Child,
         Kernel     => Self.Kernel,
         Shadow     => Self.Shadow,
         Target     => Self.Target,
         Background => Self.Background);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Build_Output_Collector;
      Item    : String;
      Command : Command_Access)
   is
      Last     : Natural := Item'Last;
   begin
      --  Strip trailing new line character
      if Last >= Item'First and then Item (Last) = ASCII.LF then
         Last := Last - 1;
      end if;

      Builder_Facility_Module.Append_To_Build_Output
        (Self.Kernel,
         Item (Item'First .. Last),
         To_String (Self.Target),
         Self.Shadow,
         Self.Background);

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self       : access Output_Parser_Fabric;
      Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean) is
   begin
      Self.Kernel := GPS.Kernel.Kernel_Handle (Kernel);
      Self.Target := To_Unbounded_String (Target);
      Self.Shadow := Shadow;
      Self.Background := Background;
   end Set;

end Build_Command_Manager.Build_Output_Collectors;

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

with Build_Configurations;             use Build_Configurations;

package body Commands.Builder.Build_Output_Collectors is

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
         Builder    => Self.Builder,
         Build      => Self.Builder.Get_Last_Build);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Build_Output_Collector;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Last     : Natural := Item'Last;
   begin
      --  Strip trailing new line character
      if Last >= Item'First and then Item (Last) = ASCII.LF then
         Last := Last - 1;
      end if;

      Self.Builder.Append_To_Build_Output
        (Line       => Item (Item'First .. Last),
         Target     => Get_Name (Self.Build.Target),
         Shadow     => Self.Build.Shadow,
         Background => Self.Build.Background);

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self       : access Output_Parser_Fabric;
      Builder    : Builder_Context) is
   begin
      Self.Builder := Builder;
   end Set;

end Commands.Builder.Build_Output_Collectors;

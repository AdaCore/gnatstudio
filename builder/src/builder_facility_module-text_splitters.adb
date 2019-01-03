------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Strings.Fixed;

package body Builder_Facility_Module.Text_Splitters is

   New_Line : constant String := (1 => ASCII.LF);

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      pragma Unreferenced (Self);
   begin
      return new Text_Splitter'(Child => Child);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Text_Splitter;
      Item    : String;
      Command : access Root_Command'Class)
   is
      use type Tools_Output_Parser_Access;
      From : Positive := Item'First;
      To   : Natural;
   begin
      if Self.Child = null then
         return;
      end if;

      while From <= Item'Last loop
         To := Ada.Strings.Fixed.Index (Item, New_Line, From => From);

         if To = 0 then
            Self.Child.Parse_Standard_Output
              (Item (From .. Item'Last), Command);
            exit;
         else
            Self.Child.Parse_Standard_Output (Item (From .. To), Command);
            From := To + 1;
         end if;
      end loop;
   end Parse_Standard_Output;

end Builder_Facility_Module.Text_Splitters;

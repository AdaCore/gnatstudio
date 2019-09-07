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

with Ada.Strings.Fixed;

package body Builder_Facility_Module.Output_Choppers is
   use type Tools_Output_Parser_Access;

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
      return new Output_Chopper'
        (Child  => Child,
         Buffer => Null_Unbounded_String);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Output_Chopper;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Last_EOL : Natural;
   begin
      if Self.Child = null then
         return;
      end if;

      Last_EOL := Ada.Strings.Fixed.Index
        (Item, New_Line, Ada.Strings.Backward);

      if Last_EOL = 0 then
         Append (Self.Buffer, Item);
      else
         Self.Child.Parse_Standard_Output
           (To_String (Self.Buffer) & Item (Item'First .. Last_EOL),
            Command);
         Self.Buffer := To_Unbounded_String (Item (Last_EOL + 1 .. Item'Last));
      end if;
   end Parse_Standard_Output;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Output_Chopper;
      Status  : Integer;
      Command : access Root_Command'Class) is
   begin
      if Self.Child = null then
         return;
      end if;

      if Self.Buffer /= Null_Unbounded_String then
         Self.Child.Parse_Standard_Output
           (To_String (Self.Buffer) & New_Line, Command);
      end if;

      --  Call parent procedure
      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);
   end End_Of_Stream;

end Builder_Facility_Module.Output_Choppers;

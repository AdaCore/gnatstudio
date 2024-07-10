------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2023, AdaCore                     --
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

with Ada.Characters.Latin_1;

package body Builder_Facility_Module.Output_Choppers is

   LF : constant Character := Ada.Characters.Latin_1.LF;
   CR : constant Character := Ada.Characters.Latin_1.CR;

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
      Command : access Root_Command'Class) is
   begin
      if Self.Child = null or Item = "" then
         return;
      end if;

      for Char of Item loop
         case Char is
            when CR =>
               null;  --  skip CR

            when LF =>
               Append (Self.Buffer, Char);

               Self.Child.Parse_Standard_Output
                 (To_String (Self.Buffer), Command);

               Self.Buffer := Null_Unbounded_String;

            when others =>
               Append (Self.Buffer, Char);
         end case;
      end loop;
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
           (To_String (Self.Buffer) & LF, Command);
      end if;

      --  Call parent procedure
      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);
   end End_Of_Stream;

end Builder_Facility_Module.Output_Choppers;

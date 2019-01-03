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

with GPS.Intl;                         use GPS.Intl;
with GNAT.Strings;                     use GNAT.Strings;
with UTF8_Utils;

package body Builder_Facility_Module.UTF8_Converters is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is
   begin
      return new UTF8_Converter'
        (Child  => Child,
         Kernel => Self.Kernel);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access UTF8_Converter;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Output : GNAT.Strings.String_Access;
      Valid  : Boolean;

   begin
      --  If we reach this point, this means we have collected some output to
      --  parse. In this case, verify that it is proper UTF-8 before
      --  transmitting it to the rest of GPS.

      --  It is hard to determine which encoding the compiler result is,
      --  especially given that we are supporting third-party compilers, build
      --  scripts, etc. Therefore, we call Unknown_To_UTF8.

      UTF8_Utils.Unknown_To_UTF8 (Item, Output, Valid);

      if Valid then
         if Output = null then
            Tools_Output_Parser (Self.all).Parse_Standard_Output
              (Item, Command);
         else
            Tools_Output_Parser (Self.all).Parse_Standard_Output
              (Output.all, Command);
         end if;
      else
         Self.Kernel.Insert
           (-"Could not convert compiler output to UTF8",
            Mode => GPS.Kernel.Error);
      end if;

      Free (Output);
   end Parse_Standard_Output;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : access Output_Parser_Fabric;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Self.Kernel := GPS.Kernel.Kernel_Handle (Kernel);
   end Set;

end Builder_Facility_Module.UTF8_Converters;

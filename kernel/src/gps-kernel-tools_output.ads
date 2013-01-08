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

--  Root of filters hierarchy to parse output of tools running by GPS.

package GPS.Kernel.Tools_Output is

   type Tools_Output_Parser is tagged;
   type Tools_Output_Parser_Access is access all Tools_Output_Parser'Class;

   type Tools_Output_Parser (Child : Tools_Output_Parser_Access) is
     abstract tagged null record;
   --  Output parsers organized in the chains.
   --  Child is pointer to next item in the chain.

   procedure Parse_Standard_Output
     (Self : not null access Tools_Output_Parser;
      Item : String);
   --  Parse a piece of an output passed as Item.
   --  Default implementation just pass Item to the Child if any.

   procedure Parse_Standard_Error
     (Self : not null access Tools_Output_Parser;
      Item : String);
   --  Parse a piece of an error output passed as Item.
   --  Default implementation just pass Item to the Child if any.

   procedure End_Of_Stream (Self : not null access Tools_Output_Parser);
   --  Process end of streams (both output and error).
   --  Default implementation just call the Child if any.

   procedure Destroy (Self : not null access Tools_Output_Parser);
   --  Free internal allocated data

   procedure Free (Self : in out Tools_Output_Parser_Access);
   --  Deallocate Self object and it's children

   type Output_Parser_Fabric is abstract tagged limited null record;
   --  Abstract fabric to create output parser objects

   function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access is abstract;
   --  Create output parser object. Set Child as next item in the chain

   type Parser_Priority is range 0 .. 1023;
   --  Priority is used to sort chain of output parser on creation.
   --  Parser with lower priority is executed first

   Reserved     : constant Parser_Priority := 20;
   --  System parsers such as UTF-8 converter registered at Reserved level

   Line_By_Line : constant Parser_Priority := 500;
   --  Parsers registered after Line_By_Line level will get input line by line

   procedure Register_Output_Parser
     (Fabric   : access Output_Parser_Fabric'Class;
      Priority : Parser_Priority);
   --  Register new output parser fabric at given Priority

   function New_Parser_Chain return Tools_Output_Parser_Access;
   --  Create new chain of Tools_Output_Parsers.
   --  Result should be deallocated after use

end GPS.Kernel.Tools_Output;

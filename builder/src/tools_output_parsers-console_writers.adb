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

with Interactive_Consoles;      use Interactive_Consoles;

package body Tools_Output_Parsers.Console_Writers is

   ---------------------------
   -- Create_Console_Writer --
   ---------------------------

   function Create_Console_Writer
     (Console : Interactive_Consoles.Interactive_Console;
      Child   : Tools_Output_Parser_Access := null)
      return Tools_Output_Parser_Access is
   begin
      if Console =  null then
         return Child;
      else
         return new Console_Writer'(Child => Child, Console => Console);
      end if;
   end Create_Console_Writer;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self : not null access Console_Writer;
      Item : String) is
   begin
      Self.Console.Insert (Item, Add_LF => False);
      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item);
   end Parse_Standard_Output;

end Tools_Output_Parsers.Console_Writers;

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

--  Declare parser to exclude progress strings from output.

with Commands;
private with GNAT.Regpat;

package Commands.Builder.Progress_Parsers is

   type Progress_Parser is new Tools_Output_Parser with private;
   --  This parser excludes progress strings from output

   type Progress_Parser_Access is access all Progress_Parser'Class;

   overriding procedure Parse_Standard_Output
     (Self : not null access Progress_Parser;
      Item : String);
   --  Process the builder output: update the progress bar in Command as
   --  necessary, strip the progress output, and pass the other outputs to
   --  Child.

   type Output_Parser_Fabric is
     new GPS.Kernel.Tools_Output.Output_Parser_Fabric with private;

   procedure Set_Pattern
     (Self    : access Output_Parser_Fabric;
      Pattern : String);

   procedure Set_Command
     (Self    : access Output_Parser_Fabric;
      Command : Commands.Command_Access);

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to exclude progress strings from output.
   --  Parser will use Data to access Command and set progress on it.

private

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;

   type Output_Parser_Fabric is
     new GPS.Kernel.Tools_Output.Output_Parser_Fabric with record
      Matcher : Pattern_Matcher_Access;
      Command : Commands.Command_Access;
   end record;

   type Progress_Parser is new Tools_Output_Parser with record
      Command : Commands.Command_Access;
      Matcher : Pattern_Matcher_Access;
   end record;

end Commands.Builder.Progress_Parsers;

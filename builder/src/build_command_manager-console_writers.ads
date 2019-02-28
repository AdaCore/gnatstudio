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

--  Declare parser to write each output item to console.

with Ada.Calendar;
with Commands;                  use Commands;
with GPS.Tools_Output;          use GPS.Tools_Output;
with Interactive_Consoles;

package Build_Command_Manager.Console_Writers is

   type Console_Writer is new Tools_Output_Parser with private;

   overriding procedure Parse_Standard_Output
     (Self    : not null access Console_Writer;
      Item    : String;
      Command : access Root_Command'Class);
   overriding procedure End_Of_Stream
     (Self    : not null access Console_Writer;
      Status  : Integer;
      Command : access Root_Command'Class);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   procedure Set
     (Self    : access Output_Parser_Fabric;
      Builder : Builder_Context);

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to write on given Console.

private

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with record
      Builder : Builder_Context;
   end record;

   type Console_Writer is new Tools_Output_Parser with record
      Builder        : Builder_Context;
      Build          : Build_Information;
      Console        : Interactive_Consoles.Interactive_Console;
      Raise_On_Error : Boolean;
      Show_Status    : Boolean;
      Start_Time     : Ada.Calendar.Time;
   end record;

end Build_Command_Manager.Console_Writers;

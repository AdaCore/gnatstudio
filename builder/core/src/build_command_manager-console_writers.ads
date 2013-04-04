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

--  Declare parser to write each output item to console.

with Ada.Calendar;
with Commands;                  use Commands;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Tools_Output;          use GPS.Tools_Output;
with Interactive_Consoles;

package Build_Command_Manager.Console_Writers is

   type Console_Writer is new Tools_Output_Parser with private;

   overriding procedure Parse_Standard_Output
     (Self    : not null access Console_Writer;
      Item    : String;
      Command : Command_Access);

   overriding procedure End_Of_Stream
     (Self    : not null access Console_Writer;
      Status  : Integer;
      Command : Command_Access);

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with private;

   procedure Set_Console
     (Self    : access Output_Parser_Fabric;
      Console : Interactive_Consoles.Interactive_Console);

   procedure Raise_Console_On_Error
     (Self     : access Output_Parser_Fabric;
      Kernel   : Kernel_Handle;
      Category : Unbounded_String);
   --  Request console raising on exit with error if no messages generated
   --  at given Category in location view

   procedure Show_Status_On_Exit (Self : access Output_Parser_Fabric);
   --  Request showing exit status

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to write on given Console.

private

   type Properties is record
      Console        : Interactive_Consoles.Interactive_Console;
      Raise_On_Error : Boolean;
      Show_Status    : Boolean;
      Kernel         : Kernel_Handle;
      Category       : Unbounded_String;
      Start_Time     : Ada.Calendar.Time;
   end record;

   type Output_Parser_Fabric is
     new GPS.Tools_Output.Output_Parser_Fabric with record
      Data : Properties;
   end record;

   type Console_Writer is new Tools_Output_Parser with record
      Data : Properties;
   end record;

end Build_Command_Manager.Console_Writers;

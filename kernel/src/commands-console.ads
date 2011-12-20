------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Console; use GPS.Kernel.Console;
with GNAT.Strings;

package Commands.Console is

   type Console_Command is new Root_Command with private;
   type Console_Command_Access is access all Console_Command;

   procedure Create
     (Item           : out Console_Command_Access;
      Kernel         : Kernel_Handle;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info);
   --  Create a new console command

   function Copy (Item : Console_Command_Access) return Console_Command_Access;
   --  Create a new Console_Command_Access as a copy of Item

   overriding function Execute
     (Command : access Console_Command) return Command_Return_Type;
   --  Execute Command

   overriding function Undo (Command : access Console_Command) return Boolean;

   overriding procedure Free (Command : in out Console_Command);
   --  Free memory associated to Command

private

   type Console_Command is new Root_Command with record
      Kernel         : Kernel_Handle;
      Text           : GNAT.Strings.String_Access;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Insert_Mode    : Message_Type := Info;
   end record;

end Commands.Console;

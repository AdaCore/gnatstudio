------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Commands;                    use Commands;
with Commands.Interactive;        use Commands.Interactive;
with GPS.Kernel;                  use GPS.Kernel;

package Docgen2_Module is

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the Docgen2 module in GPS.

private

   type Generate_Project_Command is new Interactive_Command with record
      Recursive : Boolean := False;
   end record;

   overriding function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Generate_File_Command is new Interactive_Command with null record;

   overriding function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

end Docgen2_Module;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2007, AdaCore                       --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Commands;                    use Commands;
with Commands.Interactive;        use Commands.Interactive;
with GPS.Kernel;                  use GPS.Kernel;

package Docgen2_Module is

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the Docgen2 module in GPS.

   type Generate_Project_Command is new Interactive_Command with record
      Recursive : Boolean := False;
   end record;

   function Execute
     (Command : access Generate_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Generate_File_Command is new Interactive_Command with null record;

   function Execute
     (Command : access Generate_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

end Docgen2_Module;

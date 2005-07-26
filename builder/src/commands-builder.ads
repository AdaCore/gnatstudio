-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
--                              AdaCore                              --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package handles build commands.

with GPS.Kernel;

package Commands.Builder is

   Error_Category   : constant String := "Builder results";
   --  -"Builder results"
   Warning_Category : constant String := "Builder warnings";
   --  -"Builder warnings"
   Style_Category   : constant String := "Style errors";
   --  -"Style errors"
   Shadow_Category  : constant String := "Syntax check";
   --  -"Syntax check"

   procedure Process_Builder_Output
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Command : Commands.Command_Access;
      Output  : String;
      Quiet   : Boolean);
   --  Process the builder output: update the progress bar in Command as
   --  necessary, hide the progress output, and display the other outputs in
   --  the console. Error messages are displayed in the locations window.
   --
   --  Output can contain multiple lines.

end Commands.Builder;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glide_Kernel;           use Glide_Kernel;

with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Text_Manager;   use Codefix.Text_Manager;

package Commands.Codefix is

   type Codefix_Command is new Root_Command with record
      Error        : Error_Id;
      Current_Text : Ptr_Text_Navigator;
      Corrector    : Ptr_Correction_Manager;
      Kernel       : Kernel_Handle;
   end record;

   function Execute (Command : access Codefix_Command) return Boolean;
   --  Fix the error recorded in the Codefix_Command.

   procedure Free (Command : in out Codefix_Command);
   --  Do not do anyting, as far as nothing has to be freed in Codefix_Command.

end Commands.Codefix;

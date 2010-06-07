-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2010, AdaCore             --
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

with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with Refactoring.Rename;      use Refactoring.Rename;
with Refactoring.Parameters;  use Refactoring.Parameters;
with Refactoring.Subprograms; use Refactoring.Subprograms;

package body Refactoring_Module is

   type Refactoring_Module_Record is new Module_ID_Record with null record;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Refactoring_Module_Id : Module_ID;
   begin
      Refactoring_Module_Id := new Refactoring_Module_Record;
      Register_Module (Refactoring_Module_Id, Kernel, "refactoring");
      Refactoring.Rename.Register_Refactoring (Kernel);
      Refactoring.Parameters.Register_Refactoring (Kernel);
      Refactoring.Subprograms.Register_Refactoring (Kernel);
   end Register_Module;

end Refactoring_Module;

------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2022, AdaCore                     --
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

with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with Refactoring.Rename;
with Refactoring.Parameters;
with Refactoring.Subprograms;
with Refactoring.Code_Actions;

package body Refactoring_Module is

   type Refactoring_Module_Record is new Module_ID_Record with null record;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      LSP_Ada_Support : Boolean)
   is
      Refactoring_Module_Id : Module_ID;
   begin
      --  Register all the refactoring modules
      Refactoring_Module_Id := new Refactoring_Module_Record;
      Register_Module (Refactoring_Module_Id, Kernel, "refactoring");

      --  This supports redirection between the new and old engine
      Refactoring.Rename.Register_Refactoring (Kernel);

      if not LSP_Ada_Support then
         --  This is implemented in the old and new engine separately
         Refactoring.Parameters.Register_Refactoring (Kernel);

         --  This works only when LSP is enabled
         Refactoring.Subprograms.Register_Refactoring (Kernel);
      end if;

      Refactoring.Code_Actions.Register_Actions (Kernel);
   end Register_Module;

end Refactoring_Module;

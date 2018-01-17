------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014-2018, AdaCore                   --
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

with GPS.Kernel.Actions; use GPS.Kernel.Actions;

package body GNAThub.Actions is

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

   begin
      Self.Module.Display_Data;

      return Success;
   end Execute;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Module : not null GNAThub_Module_Id) is
   begin
      Register_Action
        (Module.Get_Kernel,
         "gnathub display analysis",
         new Display_Command (Module));
   end Register_Actions;

end GNAThub.Actions;

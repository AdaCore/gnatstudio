------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;

with GPS.Kernel;
with Commands.Interactive;

package Refactoring.Rename is

   procedure Register_Refactoring
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the shell commands and menus

   --  Temporary until we switch to LSP and remove old implementation  --

   procedure Rename
     (Kernel        : GPS.Kernel.Kernel_Handle;
      Context       : Commands.Interactive.Interactive_Command_Context;
      Old_Name      : Ada.Strings.Unbounded.Unbounded_String;
      New_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Auto_Save     : Boolean;
      Overridden    : Boolean;
      Make_Writable : Boolean);

end Refactoring.Rename;

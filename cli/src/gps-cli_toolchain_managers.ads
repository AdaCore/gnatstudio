------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

--  This package defines toolchain manager for command line interface tool.

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;

with Toolchains;
with Commands;                        use Commands;

with GPS.Core_Kernels;
with GPS.Tools_Output;                use GPS.Tools_Output;

package GPS.CLI_Toolchain_Managers is

   type Toolchain_Manager_Record
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
       is new Toolchains.Toolchain_Manager_Record with null record;

   overriding function Execute
     (Self              : Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String;

private

   type Output_To_String is new Tools_Output_Parser with record
      Result : not null access Unbounded_String;
   end record;

   overriding procedure Parse_Standard_Output
     (Self    : not null access Output_To_String;
      Item    : String;
      Command : Command_Access);

end GPS.CLI_Toolchain_Managers;

------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with GNATCOLL.Arg_Lists;

with Spawn.Environments;

with VSS.Strings;

with GPS.Tools_Output;
with Interactive_Consoles;

package GPS.Kernel.Spawns is

   function Is_Enabled return Boolean;
   --  Return True is GPS.KERNEL.Spawn trace is ON.

   procedure Launch_Process
     (Command_Name  : VSS.Strings.Virtual_String;
      Console       : Interactive_Consoles.Interactive_Console;
      Exec          : String;
      Arg_List      : GNATCOLL.Arg_Lists.Arg_List;
      Env           : Spawn.Environments.Process_Environment;
      Directory     : String;
      Use_Pipes     : Boolean;
      Output_Parser : GPS.Tools_Output.Tools_Output_Parser_Access;
      Command       : out Commands.Command_Access);

end GPS.Kernel.Spawns;

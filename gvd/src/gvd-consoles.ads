------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2005-2023, AdaCore                     --
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

--  Handling of the debugger and debuggee consoles

with GPS.Debuggers;
with GVD.Process;          use GVD.Process;
with GPS.Kernel;           use GPS.Kernel;
with Interactive_Consoles; use Interactive_Consoles;

package GVD.Consoles is

   procedure Attach_To_Debugger_Console
     (Debugger            : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach debugger to a console
   --  If an unattached console exists in the desktop, it is reused.
   --  If none exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a console.

   function Get_Debugger_Interactive_Console
     (Process : not null access GPS.Debuggers.Base_Visual_Debugger'Class)
      return access Interactive_Console_Record'Class;
   --  Return the interactive console associated with the given debugger.
   --  If no interactive console is associated with this debugger, return null.

   function Get_Debuggee_Interactive_Console
     (Process : not null access GPS.Debuggers.Base_Visual_Debugger'Class)
      return access Interactive_Console_Record'Class;
   --  Return the interactive console associated with the debuggee.
   --  If no interactive console is associated with the debuggee, return null.

   procedure Attach_To_Debuggee_Console
     (Debugger            : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach to the console for the program that is debugged.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the consoles

   function Debugger_Console_Has_Focus
     (Process : not null access GVD.Process.Visual_Debugger_Record'Class)
      return Boolean;
   --  Whether the debugger console has the keyboard focus

   procedure Display_In_Debugger_Console
     (Process       : not null access Visual_Debugger_Record'Class;
      Text           : String;
      Mode           : GPS.Kernel.Message_Type := Info;
      Add_To_History : Boolean := False);
   --  Display some text in the debugger console
   --  See Interactive_Consoles.Insert for the meaning of parameters.

   procedure Display_In_Debuggee_Console
     (Process   : not null access Visual_Debugger_Record'Class;
      Text      : String;
      Mode      : GPS.Kernel.Message_Type := Info);
   --  Display the given text in the debuggee console.
   --  The text will be displayed in the debugger console instead if there is
   --  no debuggee console attached to this process .
   --  See Interactive_Consoles.Insert for the meaning of the other
   --  Parameters.

end GVD.Consoles;

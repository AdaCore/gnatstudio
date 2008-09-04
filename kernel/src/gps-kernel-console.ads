-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                              AdaCore                              --
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

with Interactive_Consoles; use Interactive_Consoles;

package GPS.Kernel.Console is

   type Message_Type is (Info, Error, Verbose);
   --  We are dealing with 3 types of messages :
   --   - Info for general information
   --   - Error for signaling errors
   --   - Verbose for detailed information

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the console module into the list

   procedure Initialize_Console
     (Kernel : access Kernel_Handle_Record'Class);
   --  Initializes the Kernel's console. Note that the main window must have
   --  been created first.

   procedure Insert
     (Kernel : access Kernel_Handle_Record'Class;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);
   --  Insert Text in the GPS's console.
   --  Highlight parts of Text that match a source location (the color is set
   --  using the preferences) if Highlight_Sloc is True.
   --  If Add_LF is True, automatically add a line separator.

   procedure Raise_Console (Kernel : access Kernel_Handle_Record'Class);
   --  If the message window is present in the MDI, raise it

   procedure Clear (Kernel : access Kernel_Handle_Record'Class);
   --  Clear all the text in the Console

   function Create_Interactive_Console
     (Kernel              : access Kernel_Handle_Record'Class;
      Title               : String := "";
      History             : Histories.History_Key := "interactive";
      Create_If_Not_Exist : Boolean := True;
      Module              : GPS.Kernel.Abstract_Module_ID := null;
      Force_Create        : Boolean := False;
      Accept_Input        : Boolean := True) return Interactive_Console;
   --  Return a new interactive console (or an existing one if there is already
   --  one with the given Title). Any existing console is not cleared.
   --  If the title is the empty stirng, the GPS console is returned.
   --  If Force_Create, a new console is created even if one with the same
   --  name already exists.
   --  Module is used to associate the console with a specific module.
   --  If Accept_Input is True, the console will be editable. This is ignored
   --  if the console already exists.

   function Get_Console
     (Kernel : access Kernel_Handle_Record'Class) return Interactive_Console;
   --  Return the interactive console associated with the kernel

end GPS.Kernel.Console;

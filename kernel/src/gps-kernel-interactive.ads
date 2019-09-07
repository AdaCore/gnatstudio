------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Interactive_Consoles; use Interactive_Consoles;

package GPS.Kernel.Interactive is

   function Create_Interactive_Console
     (Kernel               : access Kernel_Handle_Record'Class;
      Title                : String := "";
      History              : Histories.History_Key := "interactive";
      Create_If_Not_Exist  : Boolean := True;
      Module               : GPS.Kernel.Abstract_Module_ID := null;
      Force_Create         : Boolean := False;
      Accept_Input         : Boolean := True;
      ANSI_Support         : Boolean := False;
      Manage_Prompt        : Boolean := True;
      Toolbar_Name         : String := "";
      Give_Focus_On_Create : Boolean := True) return Interactive_Console;
   --  Create or retrieve an interactive console
   --
   --  If Title is the empty string, the GPS console is returned.
   --
   --  If Create_If_Not_Exist is True, a new console is created if no existing
   --  console with the same Title has been found
   --
   --  Module is used to associate the console with a specific module.
   --
   --  If Force_Create, a new console is created even if one with the same
   --  name already exists.
   --
   --  If Accept_Input is True, the console will be editable. This is ignored
   --  if the console already exists.
   --
   --  If Give_Focus_On_Create is True and if a new console has been created,
   --  the focus will be given to this new console.
   --  See Interactive_Consoles.Gtk_New for more info on the parameters

end GPS.Kernel.Interactive;

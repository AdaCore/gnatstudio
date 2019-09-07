------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

--  This package implements GPS shell commands to manipulate messages.

package GPS.Kernel.Messages.Shell is

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the shell commands used to handle messages

   function Create_Message_Instance
     (Script  :
        not null access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Message : not null Message_Access)
      return GNATCOLL.Scripts.Class_Instance;
   --  Create new instance of specified message.

   function Get_Data
     (Instance : GNATCOLL.Scripts.Class_Instance)
      return GPS.Kernel.Messages.Message_Access;
   --  Returns message associated with instance.

   procedure Set_Data
     (Instance : GNATCOLL.Scripts.Class_Instance;
      Message  : Message_Access);
   --  Set data in Instance to Message

end GPS.Kernel.Messages.Shell;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

private

   package Instance_Linked_List is new Ada.Containers.Doubly_Linked_Lists
     (GNATCOLL.Scripts.Class_Instance,
      GNATCOLL.Scripts."=");
   use Instance_Linked_List;

   type Shell_Note_Record is new Abstract_Note with record
      Instances : Instance_Linked_List.List;
   end record;
   type Shell_Note is access all Shell_Note_Record'Class;

   overriding procedure Finalize (Self : not null access Shell_Note_Record);

end GPS.Kernel.Messages.Shell;

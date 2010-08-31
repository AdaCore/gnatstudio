-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009-2010, AdaCore              --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  Executes gps_codepeer_bridge utility and call module's
--  subprograms for handling of the output.

package Code_Peer.Module.Bridge is

   procedure Inspection (Module : Code_Peer.Module.Code_Peer_Module_Id);
   --  Runs gps_codepeer_bridge to retrive inspection's information.
   --  Reuses existent XML file if it is exists and up-to-date.

   procedure Remove_Inspection_Cache_File
      (Module : Code_Peer.Module.Code_Peer_Module_Id);
   --  Removes auxiliary inspection's information file, used as cache.

   procedure Review_Message
     (Module  : Code_Peer.Module.Code_Peer_Module_Id;
      Message : Code_Peer.Message_Access);
   --  ???

   procedure Add_Audit_Record
     (Module  : Code_Peer.Module.Code_Peer_Module_Id;
      Message : Code_Peer.Message_Access);
   --  ???

end Code_Peer.Module.Bridge;

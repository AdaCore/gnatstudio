------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

--  Executes gps_codepeer_bridge utility and call module's
--  subprograms for handling of the output.

package CodePeer.Module.Bridge is

   procedure Inspection (Module : CodePeer.Module.CodePeer_Module_Id);
   --  Runs gps_codepeer_bridge to retrive inspection's information.
   --  Reuses existent XML file if it is exists and up-to-date.

   procedure Remove_Inspection_Cache_File
      (Module : CodePeer.Module.CodePeer_Module_Id);
   --  Removes auxiliary inspection's information file, used as cache.

   procedure Review_Message
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Message : CodePeer.Message_Access);
   --  ???

   procedure Add_Audit_Record
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Message : CodePeer.Message_Access);
   --  ???

end CodePeer.Module.Bridge;

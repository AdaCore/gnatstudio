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

--  Generates command files for gps_codepeer_bridge commands

with GNATCOLL.VFS;     use GNATCOLL.VFS;

package Code_Peer.Bridge.Commands is

   procedure Inspection
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File);
   --  Generates command file for export inspection information from the
   --  database.

   procedure Audit_Trail
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File;
      Message_Id        : Positive);
   --  Generates command file for export audit trail information from the
   --  database.

   procedure Add_Audit_Record
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      Ids                 : Natural_Sets.Set;
      Probability_Changed : Boolean;
      New_Ranking         : Code_Peer.Message_Ranking_Level;
      Comment             : String);
   --  Generates command file for add audit record to the database.

end Code_Peer.Bridge.Commands;

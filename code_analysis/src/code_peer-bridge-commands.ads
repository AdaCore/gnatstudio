-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2011, AdaCore                 --
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

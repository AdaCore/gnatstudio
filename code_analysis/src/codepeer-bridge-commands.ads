------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;          use GNATCOLL.VFS;

package CodePeer.Bridge.Commands is

   procedure Inspection
     (Command_File_Name    : Virtual_File;
      Server_URL           : String;
      Output_Directory     : Virtual_File;
      DB_Directory         : Virtual_File;
      Message_Patterns     : Virtual_File;
      Additional_Patterns  : Virtual_File;
      Inspection_File_Name : Virtual_File;
      Status_File_Name     : Virtual_File;
      Import_Annotations   : Boolean;
      Maximum_Version      : Format_Version);
   --  Generates command file for export inspection information from the
   --  database.

   procedure Audit_Trail
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Export_File_Name    : Virtual_File;
      Messages            : CodePeer.Message_Vectors.Vector;
      Version             : Supported_Format_Version);
   --  Generates command file for export audit trail information from the
   --  database.

   procedure Add_Audit_Record_V4_5_6
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Messages            : Message_Vectors.Vector;
      Version             : Supported_Format_Version);
   --  Generates command file for add audit record to the database using
   --  version 4 of interchange format.

end CodePeer.Bridge.Commands;

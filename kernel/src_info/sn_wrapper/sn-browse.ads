-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
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

pragma Warnings (Off);
with GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.OS_Lib;

package SN.Browse is
   Unlink_Failure    : exception;
   Spawn_Failure     : exception;
   Temp_File_Failure : exception;

   DB_Dir_Name          : constant String := ".gpssnprj";
   --  Name of directory where all SN files reside

   DB_File_Name         : constant String := "data";
   --  Name of the SN database files

   Xref_Pool_Filename   : constant String := "xrefs";
   --  Name of file for persistent xref pool

   procedure Browse
     (File_Name     : String;
      DB_Directory  : String;
      DBIMP_Path    : String;
      Cbrowser_Path : String;
      PD            : out GNAT.Expect.TTY.TTY_Process_Descriptor);
   --  Start the language browser on the files lists in File_Name (one file per
   --  line, and lines can start with @ to specify the name of the xref file to
   --  use from then on).
   --  All database files are placed in the directory
   --  DB_Directory. DB_Directory must end with a directory separator.
   --  A number of exceptions may be thrown to signal error during
   --  process spawning, file unlinking...

   procedure Generate_Xrefs
     (DB_Directories : GNAT.OS_Lib.String_List_Access;
      DBIMP_Path     : String;
      Temp_Name      : out GNAT.OS_Lib.Temp_File_Name;
      PD             : out GNAT.Expect.TTY.TTY_Process_Descriptor);
   --  Removes .by and .to tables in the DB_Directories (1) and
   --  spawns dbimp process with DB_Directories as input arguments
   --  specifying all loaded SN databases. The cross reference
   --  files are taken only from the first DB_Directories (1)
   --  directory which is supposed to be working and corresponding
   --  to the current project.
   --  on error an exception is thrown

   procedure Delete_Database (DB_Directory : in String);
   --  Removes all files from SN DB directory except xref pool

   procedure Is_Alive
     (PD     : in out GNAT.Expect.TTY.TTY_Process_Descriptor;
      Status : out Boolean);
   --  checks if the process is still running. If the process exited, its
   --  descriptor is closed and False returned in Status.

end SN.Browse;

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

with GPS.Kernel;        use GPS.Kernel;
with VCS;               use VCS;
with VCS_Activities;    use VCS_Activities;
with GNATCOLL.VFS;

package Log_Utils is

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class);
   --  This subprogram must be called before calling any subprogram
   --  from this package.

   function Get_ChangeLog_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File;
   --  Returns the global ChangeLog file given a File_Name. A global ChangeLog
   --  is named ChangeLog and is placed into the same directory as File_Name.

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Create    : Boolean;
      Suffix    : String := "$log") return GNATCOLL.VFS.Virtual_File;
   --  Return the name for the log file corresponding to File_Name.
   --  If the log file doesn't exist and Create is True, then
   --  the function will create the file, return an empty string
   --  otherwise.
   --  Suffix is the suffix to append at the end of the log file to identify
   --  it as log.

   function Get_File_From_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Log_Name : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File;
   --  Return the name for the file corresponding to Log_Name

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File) return String;
   --   Return the log content for the given file

   procedure Get_Log_From_ChangeLog
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Suffix    : String := "$log");
   --  Read the global ChangeLog entry for File_Name and write it into the
   --  log file corresponding to File_Name. If a current log file exists it
   --  does nothing.
   --  Suffix is the default suffix to use for the log when creating it.

   procedure Remove_File_From_Mapping
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : GNATCOLL.VFS.Virtual_File);
   --  Remove the entry File_Name from the logs mapping,
   --  if such an entry exists.

   function Action_To_Log_Suffix (Action : VCS_Action) return String;
   --  Return the suffix to be used in log files that correspond to Action

   procedure Log_Action_Files
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Ref      : VCS_Access;
      Action   : VCS_Action;
      Files    : GNATCOLL.VFS.File_Array;
      Activity : Activity_Id);
   --  Perform Action on the list of files, assuming that they all belong to
   --  the VCS system identified by Ref.
   --  This subprogram will do all the necessary file/log checks before
   --  performing Action.

end Log_Utils;

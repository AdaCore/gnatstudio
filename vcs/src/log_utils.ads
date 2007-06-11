-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with String_List_Utils; use String_List_Utils;
with GPS.Kernel;        use GPS.Kernel;
with VCS;               use VCS;
with VCS_Activities;    use VCS_Activities;
with VFS;

package Log_Utils is

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class);
   --  This subprogram must be called before calling any subprogram
   --  from this package.

   function Get_ChangeLog_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File) return VFS.Virtual_File;
   --  Returns the global ChangeLog file given a File_Name. A global ChangeLog
   --  is named ChangeLog and is placed into the same directory as File_Name.

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File;
      Create    : Boolean;
      Suffix    : String := "$log") return VFS.Virtual_File;
   --  Return the name for the log file corresponding to File_Name.
   --  If the log file doesn't exist and Create is True, then
   --  the function will create the file, return an empty string
   --  otherwise.
   --  Suffix is the suffix to append at the end of the log file to identify
   --  it as log.

   function Get_File_From_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Log_Name : VFS.Virtual_File) return VFS.Virtual_File;
   --  Return the name for the file corresponding to Log_Name

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File) return String;
   --   Return the log content for the given file

   procedure Get_Log_From_ChangeLog
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File;
      Suffix    : String := "$log");
   --  Read the global ChangeLog entry for File_Name and write it into the
   --  log file corresponding to File_Name. If a current log file exists it
   --  does nothing.
   --  Suffix is the default suffix to use for the log when creating it.

   procedure Remove_File_From_Mapping
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File);
   --  Remove the entry File_Name from the logs mapping,
   --  if such an entry exists.

   function Action_To_Log_Suffix (Action : VCS_Action) return String;
   --  Return the suffix to be used in log files that correspond to Action.

   procedure Log_Action_Files
     (Kernel   : Kernel_Handle;
      Ref      : VCS_Access;
      Action   : VCS_Action;
      Files    : String_List.List;
      Activity : Activity_Id);
   --  Perform Action on the list of files, assuming that they all belong to
   --  the VCS system identified by Ref.
   --  This subprogram will do all the necessary file/log checks before
   --  performing Action.

end Log_Utils;

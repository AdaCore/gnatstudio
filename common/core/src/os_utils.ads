------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  This package provides a set of subprograms that give high level access
--  to OS functionnalities.

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNATCOLL.VFS;

package OS_Utils is

   function Max_Path_Len return Natural;
   --  Return the maximum length of a pathname for the current host

   procedure Put (File : File_Descriptor; Str : String);
   --  Write Str to File

   procedure Put_Line (File : File_Descriptor; Str : String);
   --  Write Str to File, and appends an end-of-line sequence

   procedure New_Line (File : File_Descriptor; Count : Natural := 1);
   --  Write an end-of-line sequence to File

   type Ctrl_C_Handler is access procedure;
   pragma Convention (C, Ctrl_C_Handler);
   --  Any parameterless library level procedure can be used as a handler.
   --  Ctrl_C_Handler should not propagate exceptions.

   procedure Install_Ctrl_C_Handler (Handler : Ctrl_C_Handler);
   --  Set up Handler to be called if the operator hits Ctrl-C

   procedure Uninstall_Ctrl_C_Handler;
   --  Reinstall the standard Control-C handler.
   --  If Install_Handler has never been called, this procedure has no effect.

   function Create_Tmp_File return GNATCOLL.VFS.Virtual_File;
   --  Create a temporary file in the temporary directoy.
   --  Return the full name of the file.

   procedure Make_Dir_Recursive (Name : GNATCOLL.VFS.Virtual_File);
   --  Create the directory Name, and its parents if necessary (for instance,
   --  if Name is /tmp/foo/bar, the directories /tmp, /tmp/foo and /tmp/foo/bar
   --  are created if they don't exist yet).
   --  Directory_Error is raised if the directory or one of its parents
   --  couldn't be created.

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process

   type Path_Style is (UNIX, DOS, System_Default, Cygwin);
   function Format_Pathname
     (Path  : GNATCOLL.VFS.Filesystem_String;
      Style : Path_Style := System_Default)
      return GNATCOLL.VFS.Filesystem_String;
   --  This routines call GNAT.Directory_Operations.Format_Pathname. The only
   --  difference is for the Cygwin mode. In this case the drive prefix is
   --  converted to the cygwin equivalent (/cygdrive/<drive>).

   function Is_Cygwin_Path
     (Path : GNATCOLL.VFS.Filesystem_String) return Boolean;
   pragma Inline (Is_Cygwin_Path);
   --  Return true if Path start with /cygdrive/<drive>/

   function Normalize_To_OS_Case
     (Full_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Filesystem_String;
   --  On non case-sensitive OS returns the casing as recorded by the OS. This
   --  routine can be time consuming as for example on Windows it parses all
   --  directories and sub-directories to get the OS recorded casing.
   --  On case-sensitive OS it just returns Full_Name.
   --  At the moment, the only non case-sensitive OS supported is Windows.
   --  This routine should be called for any pathname comming from the command
   --  line or from dialogs filled by users.

   function Strip_Exe (Name : String) return String;
   --  Strips the .exe extension if needed

private
   pragma Import (C, Install_Ctrl_C_Handler, "__gnat_install_int_handler");
   pragma Import (C, Uninstall_Ctrl_C_Handler, "__gnat_uninstall_int_handler");
   pragma Import (C, Get_Process_Id, "getpid");
end OS_Utils;

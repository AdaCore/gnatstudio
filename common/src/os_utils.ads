-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a set of subprograms that give high level access
--  to OS functionnalities.

with GNAT.OS_Lib; use GNAT.OS_Lib;

package OS_Utils is

   function Executable_Location return String;
   --  Return the name of the parent directory where the executable is stored.
   --  The executable must be located in a directory called "bin". Thus, if
   --  the executable is stored in directory "/foo/bar/bin", this routine
   --  returns "/foo/bar/". If the executable is not stored in a directory
   --  "bin" (casing is unimportant) then a null string is returned.

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   procedure Set_OpenVMS_Host (Setting : Boolean := True);
   --  Set whether the host is an OpenVMS host.

   function Max_Path_Len return Natural;
   --  Return the maximum length of a pathname for the current host.

   procedure Put (File : File_Descriptor; Str : String);
   --  Write Str to File.

   procedure Put_Line (File : File_Descriptor; Str : String);
   --  Write Str to File, and appends an end-of-line sequence.

   procedure New_Line (File : File_Descriptor; Count : Natural := 1);
   --  Write an end-of-line sequence to File.

   type Ctrl_C_Handler is access procedure;
   --  Any parameterless library level procedure can be used as a handler.
   --  Ctrl_C_Handler should not propagate exceptions.

   procedure Install_Ctrl_C_Handler (Handler : Ctrl_C_Handler);
   --  Set up Handler to be called if the operator hits Ctrl-C.

   procedure Uninstall_Ctrl_C_Handler;
   --  Reinstall the standard Control-C handler.
   --  If Install_Handler has never been called, this procedure has no effect.

   function Get_Tmp_Dir return String;
   --  Return a string representing a valid directory that can be used to
   --  create temporary files. If needed, a trailing directory separator will
   --  be added.

   function Create_Tmp_File return String;
   --  Create a temporary file in the temporary directoy.
   --  Return the full name of the file.

   procedure Make_Dir_Recursive (Name : String);
   --  Create the directory Name, and its parents if necessary (for instance,
   --  if Name is /tmp/foo/bar, the directories /tmp, /tmp/foo and /tmp/foo/bar
   --  are created if they don't exist yet).
   --  Directory_Error is raised if the directory or one of its parents
   --  couldn't be created.

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process

   type Path_Style is (UNIX, DOS, System_Default, Cygwin);
   function Format_Pathname
     (Path  : String;
      Style : Path_Style := System_Default) return String;
   --  This routines call GNAT.Directory_Operations.Format_Pathname. The only
   --  difference is for the Cygwin mode. In this case the drive prefix is
   --  converted to the cygwin equivalent (/cygdrive/<drive>).

   function Is_Cygwin_Path (Path : String) return Boolean;
   pragma Inline (Is_Cygwin_Path);
   --  Return true if Path start with /cygdrive/<drive>/

private
   pragma Import (C, Install_Ctrl_C_Handler, "__gnat_install_int_handler");
   pragma Import (C, Uninstall_Ctrl_C_Handler, "__gnat_uninstall_int_handler");
   pragma Import (C, Get_Process_Id, "getpid");
end OS_Utils;

-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2001-2003                      --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

   type Idle_Callback is access procedure;
   --  Callback procedure called by Spawn while waiting the inferior.

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Idle         : Idle_Callback;
      Success      : out Boolean);
   --  Similar to GNAT.OS_Lib.Spawn, but call Idle periodically while waiting.

   function Read_File (File : String) return String_Access;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  File is a UTF8-encoded string

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

   procedure Make_Dir_Recursive (Name : String);
   --  Create the directory Name, and its parents if necessary (for instance,
   --  if Name is /tmp/foo/bar, the directories /tmp, /tmp/foo and /tmp/foo/bar
   --  are created if they don't exist yet).
   --  Directory_Error is raised if the directory or one of its parents
   --  couldn't be created.

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process.

private
   pragma Import (C, Install_Ctrl_C_Handler, "__gnat_install_int_handler");
   pragma Import (C, Uninstall_Ctrl_C_Handler, "__gnat_uninstall_int_handler");
   pragma Import (C, Get_Process_Id, "getpid");
end OS_Utils;

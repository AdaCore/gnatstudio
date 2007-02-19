-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

--  This package contains remote shell descriptors.

with GNAT.Strings;       use GNAT.Strings;
with Filesystem;         use Filesystem;

with GNAT.Expect;        use GNAT.Expect;

package Shell_Descriptors is

   Invalid_Nickname : exception;

   type Shell_Descriptor_Record;
   type Shell_Descriptor_Access is access all Shell_Descriptor_Record;

   type Shell_Descriptor_Record is record
      Name             : String_Access                := null;
      Filesystem       : Filesystem_Access            := null;
      Start_Cmd        : String_Access                := null;
      Init_Cmds        : String_List_Access           := null;
      Exit_Cmds        : String_List_Access           := null;
      Cd_Cmd           : String_Access                := null;
      Get_Status_Cmd   : String_Access                := null;
      Get_Status_Ptrn  : Pattern_Matcher_Access       := null;
      Generic_Prompt   : Pattern_Matcher_Access       := null;
      Prompt           : Pattern_Matcher_Access       := null;
      Next             : Shell_Descriptor_Access      := null;
   end record;

   Null_String_List : constant String_List (1 .. 0) := (others => null);
   --  Null string list

   procedure Add_Shell_Descriptor
     (Name                : String;
      Start_Command       : String             := "";
      Generic_Prompt      : String             := "";
      Configured_Prompt   : String             := "";
      FS                  : Filesystem_Record'Class;
      Init_Commands       : String_List        := Null_String_List;
      Exit_Commands       : String_List        := Null_String_List;
      Cd_Command          : String             := "";
      Get_Status_Command  : String             := "";
      Get_Status_Ptrn     : String             := "");
   --  This function is used to add a new shell descriptor
   --  - Name                : name in the program descriptor table
   --  - Start_Command       : name of the program to be launched
   --  - Start_Command_Args  : arguments passed to the program (note %h
   --                         is a special argument and is replaced by the
   --                         target network name...)
   --  - Start_Timeout       : timeout used during launch
   --  - Init_Commands       : list of commands sent just after program is
   --                         launched
   --  - Exit_Commands       : idem but for Quit
   --  - Cd_Command          : change working directory (%d is replaced by dir)
   --  - Get_Status_Command  : command used to retrieve the terminated
   --                          program's status
   --  - Get_Status_Ptrn     : regular expression used to retrieve the result
   --                          of Get_Status_Command
   --  - General_Prompt      : prompt expected during init phase
   --  - Prompt              : regexp that match the prompt
   --  - Buffer_Size         : size of the expect buffer
   --  - Use_TTY             : if set to true use TTY version of GNAT.Expect
   --  - Output_Processor    : processing of the output ... (to get for example
   --                          the exit status)
   --
   --  For all Commands, %h is replaced by target host, %d is replaced by
   --  working directory

   function Get_Nb_Shell_Descriptor return Natural;
   --  Get the total number of shell descriptor configured

   function Get_Shell_Descriptor_Name (N : Natural) return String;
   --  Get the Nth shell descriptor name

   function Get_Descriptor_From_Name
     (Name : String) return Shell_Descriptor_Access;
   --  Return a Shell_Descriptor from its name.
   --  Return null if no matching descriptor was found.
   --  User must not free the result.

   function Get_Filesystem_From_Shell
     (Shell : String) return Filesystem_Record'Class;
   --  Get the filesystem corresponding to shell

   function Get_Shell_Descriptor
     (Nickname : String) return Shell_Descriptor_Access;
   --  Get shell descriptor from nickname

end Shell_Descriptors;

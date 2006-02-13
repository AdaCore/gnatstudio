------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with System;          use System;

package GNAT.Expect.TTY.Remote is

   Invalid_Config_Selector : exception;

   Null_String_List : constant String_List (1 .. 0) := (others => null);
   --  Null string list

   -----------------------------
   -- Configuration functions --
   -----------------------------

   type Config_Selector_Function is access
     function (Identifier    : String) return String;

   type Cmd_Wrapper_Function is access
     function (Cmd : String) return String;
   --  Used to format the commands to host format if needed.

   type Output_Processor_Procedure is access
     procedure (Str : in out String);

   procedure Set_Config_Selector
     (Remote_Access_Selector : Config_Selector_Function;
      Shell_Selector         : Config_Selector_Function);
   --  Set the functions that will be used as Config_Selector. The
   --  Config_Selector is a function that given the identifier returns a
   --  shell or Remote access descriptor name.

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      User_Prompt_Ptrn          : String;
      Password_Prompt_Ptrn      : String);
   --  Adds a new Remote Access Descriptor
   --  Name : identifier of this descriptor
   --  Start_Command : command used to launch the remote access utility
   --  Start_Command_Common_Args : arguments always provided to this utility
   --  Start_Command_User_Arg    : if user is specified, this argument will
   --   be used (%u replaced by actual user)
   --  User_Prompt_Ptrn          : regular expression for user prompt
   --  Password_Prompt_Ptrn      : regular expression for password prompt

   procedure Add_Shell_Descriptor
     (Name                : String;
      Start_Command       : String             := "";
      Generic_Prompt      : String             := "";
      Configured_Prompt   : String             := "";
      Init_Commands       : String_List        := Null_String_List;
      Exit_Commands       : String_List        := Null_String_List;
      Cd_Command          : String             := "";
      Get_Status_Command  : String             := "";
      Get_Status_Ptrn     : String             := "";
      Echoing             : Boolean            := False;
      Wrapper             : Cmd_Wrapper_Function := null;
      Output_Processor    : Output_Processor_Procedure := null;
      Interrupt_Command   : String             := "");
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
   --  - Echoing             : if set to true, it means that the shell is
   --                         echoing so when the output of a command is
   --                         retrieved the first line just be ignored
   --  - Wrapper             : when sending a command Cmd the real string that
   --                          is sent to the process is Wrapper (Cmd)
   --  - Output_Processor    : processing of the output ... (to get for example
   --                          the exit status)
   --  - Interrupt_Command   : string that can be sent to interrupt a command
   --
   --  For all Commands, %h is replaced by target host, %d is replaced by
   --  working directory

   ----------------------
   -- Expect interface --
   ----------------------

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with private;

   procedure Add_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function;
      Filter_On  : Filter_Type := Output;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False);
   --  Add a new filter for one of the filter type. This filter will be
   --  run before all the existing filters, unless After is set True,
   --  in which case it will be run after existing filters. User_Data
   --  is passed as is to the filter procedure.

   procedure Remove_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function);
   --  Remove a filter from the list of filters (whatever the type of the
   --  filter).

   procedure Lock_Filters (Descriptor : in out Remote_Process_Descriptor);
   --  Temporarily disables all output and input filters. They will be
   --  reactivated only when Unlock_Filters has been called as many times as
   --  Lock_Filters;

   procedure Unlock_Filters (Descriptor : in out Remote_Process_Descriptor);
   --  Unlocks the filters. They are reactivated only if Unlock_Filters
   --  has been called as many times as Lock_Filters.

   procedure Send
     (Descriptor   : in out Remote_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False);
   --  Send a string to the file descriptor.
   --
   --  The string is not formatted in any way, except if Add_LF is True,
   --  in which case an ASCII.LF is added at the end, so that Str is
   --  recognized as a command by the external process.
   --
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent. The output
   --  filters are of course called as usual.

   procedure Remote_Spawn
     (Descriptor          : out Remote_Process_Descriptor;
      Target_Name         : String;
      Target_Identifier   : String;
      Local_Args          : GNAT.OS_Lib.Argument_List;
      Args                : GNAT.OS_Lib.Argument_List;
      Execution_Directory : String := "";
      User_Name           : String := "";
      Launch_Timeout      : Natural := 5000;
      Err_To_Out          : Boolean := False);
   --  Spawns a process on a remote machine
   --  Target_Name designs the machine on which the process is spawned
   --  Target_Identifier is used to retrieve the descriptors
   --  Args are the arguments used to launch the program
   --  Execution_Directory is the working directory on the remote machine
   --  Buffer_Size is the maximum buffer size for process output reception
   --   (set Buffer_Size to 0 for dynamic memory allocation)
   --  Err_To_Out tells if the stderr shall be redirected to stdout

private

   --  The following methods are inherited. See parent for explanations.

   procedure Close
     (Descriptor : in out Remote_Process_Descriptor;
      Status     : out Integer);

   procedure Close
     (Descriptor : in out Remote_Process_Descriptor);

   procedure Interrupt (Descriptor : in out Remote_Process_Descriptor);

   type Shell_Descriptor;

   type Shell_Descriptor_Access is access all Shell_Descriptor;

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with record
      Busy                 : Boolean                   := False;
      --  tells if the remote shell is busy processing a command
      Current_Echo_Skipped : Boolean := False;
      --  tells if the command we've sent has been echoed or not.
      Shell                : Shell_Descriptor_Access   := null;
      --  what shell is on the remote server
      Last_Index           : Integer := -1;
      --  if remote execution finished, points to the last command output
      Getting_Status       : Boolean := False;
      --  tells if we are retrieving the status of the finished command
      Status               : Integer := 0;
      --  records the status of the finished command
      R_Filters_Lock       : Integer := 0;
      --  Tells if the filters are locked
      R_Filters            : Filter_List := null;
      --  List of filters. Filter_List is defined in GNAT.Expect
   end record;

end GNAT.Expect.TTY.Remote;

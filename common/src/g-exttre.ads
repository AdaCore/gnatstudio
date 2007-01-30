------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2006-2007 AdaCore                     --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.Regpat;     use GNAT.Regpat;
with GNAT.Strings;    use GNAT.Strings;

with Filesystem;      use Filesystem;
with System;          use System;

with Ada.Unchecked_Deallocation;

package GNAT.Expect.TTY.Remote is

   Invalid_Nickname : exception;
   No_Session_Available : exception;

   Null_String_List : constant String_List (1 .. 0) := (others => null);
   --  Null string list

   -------------------------
   -- Connection Debugger --
   -------------------------

   type Connection_Debugger_Record is abstract tagged null record;
   type Connection_Debugger is access all Connection_Debugger_Record'Class;

   procedure Create (Dbg   : access Connection_Debugger_Record'Class;
                     Title : String) is abstract;
   --  Create a new Connection Debugger with furnished title

   type Mode_Type is (Input, Output);

   procedure Print (Dbg  : access Connection_Debugger_Record;
                    Str  : String;
                    Mode : Mode_Type) is abstract;
   --  Display Str in the connection debugger

   -----------------------------
   -- Configuration functions --
   -----------------------------

   type Extra_Prompt (Auto_Answer : Boolean := False) is record
      Ptrn : Pattern_Matcher_Access;

      case Auto_Answer is
         when True =>
            Answer : String_Access;
         when False =>
            Question : String_Access;
      end case;
   end record;
   --  Used to handle extra prompts received from a remote access tool
   --  Auto_Answer: Tells if we shall automatically send an answer to this
   --   prompt.
   --  Ptrn: The pattern to expect
   --  Answer: The automatic answer to send to the remote access tool.
   --  Question: The question GPS will ask to the user. The user's response
   --   will then be sent to the remote access tool.

   Null_Extra_Prompt : constant Extra_Prompt :=
     (Auto_Answer => True,
      Ptrn        => null,
      Answer      => null);

   --  Tool specific prompt. If Auto_Answer is set, then the GPS user will not
   --  be asked for anything, and Answer will be sent to the tool.
   --  Else, the User will be asked the User_Question.
   type Extra_Prompts is array (Natural range <>) of Extra_Prompt;
   Null_Extra_Prompts : constant Extra_Prompts (1 .. 0)
     := (others => Null_Extra_Prompt);

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      User_Prompt_Ptrn          : String_Access;
      Password_Prompt_Ptrn      : String_Access;
      Passphrase_Prompt_Ptrn    : String_Access;
      Extra_Prompt_Array        : Extra_Prompts := Null_Extra_Prompts;
      Use_Cr_Lf                 : Boolean := False;
      Use_Pipes                 : Boolean := False);
   --  Adds a new Remote Access Descriptor
   --  Name : identifier of this descriptor
   --  Start_Command : command used to launch the remote access utility
   --  Start_Command_Common_Args : arguments always provided to this utility
   --  Start_Command_User_Arg    : if user is specified, this argument will
   --   be used (%u replaced by actual user)
   --  User_Prompt_Ptrn          : regular expression for user prompt
   --                              if null, the default user prompt is used
   --  Password_Prompt_Ptrn      : regular expression for password prompt
   --                              if null, the default password prompt is used
   --  Passphrase_Prompt_Ptrn    : regular expression for passphrases. This
   --                              expression shall isolate the key_id with
   --                              parenthesis
   --  Extra_Prompt_Array        : extra specific prompts.
   --  Use_Cr_Lf                 : tell if CR character needs to be added when
   --                               sending commands to the tool.
   --  Use_Pipes                 : tell if the tool is launched in pipe or tty
   --                               mode. Only applicable on Windows (no effect
   --                               on other machines)

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

   type Machine_Descriptor_Record is tagged record
      Nickname            : String_Access;
      --  Identifier of the machine
      Network_Name        : String_Access;
      --  Used to access the server using the network
      Access_Name         : String_Access;
      --  Tool used to remotely access the server
      Shell_Name          : String_Access;
      --  Shell used on the remote server
      Extra_Init_Commands : GNAT.OS_Lib.Argument_List_Access := null;
      --  User specific init commands
      User_Name           : String_Access;
      --  User name used for connection
      Timeout             : Natural := 5000;
      --  Timeout value used when connecting to the machine (in ms)
      Max_Nb_Connections  : Natural := 3;
      --  Maximum number of simultaneous connections on the machine
      Ref                 : Natural := 0;
      --  Ref counter
      Dbg                 : Connection_Debugger := null;
      --  Connection debug console.
   end record;
   type Machine_Descriptor is access all Machine_Descriptor_Record'Class;

   procedure Unref (Desc : in out Machine_Descriptor);
   --  Does Ref - 1. Free allocated memory for the descriptor if ref=0

   procedure Add_Machine_Descriptor (Desc : Machine_Descriptor);
   --  Adds a new machine descriptor.

   procedure Remove_Machine_Descriptor (Desc : in out Machine_Descriptor);
   --  Removes a machine descriptor.

   procedure Remove_All_Machine_Descriptors;
   --  Removes all machine descriptors.

   function Get_Nb_Shell_Descriptor return Natural;
   --  Get the total number of shell descriptor configured

   function Get_Shell_Descriptor_Name (N : Natural) return String;
   --  Get the Nth shell descriptor name

   function Get_Filesystem_From_Shell
     (Shell : String) return Filesystem_Record'Class;
   --  Get the filesystem corresponding to shell

   function Get_Nb_Remote_Access_Descriptor return Natural;
   --  Get the total number of remote access descriptor configured

   function Get_Remote_Access_Name (N : Natural) return String;
   --  Get the Nth remote access descriptor name

   function Get_Nb_Machine_Descriptor return Natural;
   --  Get the total number of Machine Descriptor configured

   function Get_Machine_Descriptor (N : Natural) return Machine_Descriptor;
   --  Retrieve the descriptor of the Nth configured machine

   function Get_Machine_Descriptor
     (Nickname : String) return Machine_Descriptor;
   --  Get machine descriptor from nickname

   function Get_Nickname (N : Natural) return String;
   --  Retrieve the nickname of the Nth configured machine
   --  Raise Invalid_Nickname if N does not correspond to a server

   function Is_Configured (Nickname : String) return Boolean;
   --  Tells if server Nickname exists

   function Is_Ready_Session (Nickname : String) return Boolean;
   --  Tell if a ready session is available for specified server

   function Get_Network_Name (Nickname : String) return String;
   --  Retrieve the network name of the specified server.
   --  Raise Invalid_Nickname if Nickname does not correspond to a server

   function Get_Filesystem (Nickname : String) return Filesystem_Record'Class;
   --  Retrieve the filesystem of the specified server
   --  Raise Invalid_Nickname if Nickname does not correspond to a server

   function Get_Local_Filesystem return Filesystem_Record'Class;
   --  Retrieve the local filesystem type

   ----------------------
   -- Expect interface --
   ----------------------

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with private;
   type Remote_Process_Descriptor_Access is
     access all Remote_Process_Descriptor'Class;

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
     (Descriptor          : out Process_Descriptor_Access;
      Target_Nickname     : String;
      Args                : GNAT.OS_Lib.Argument_List;
      Execution_Directory : String := "";
      Err_To_Out          : Boolean := False;
      On_New_Connection   : access procedure (Target_Name : String) := null);
   --  Spawns a process on a remote machine
   --  Target_Name designs the machine on which the process is spawned
   --  Target_Identifier is used to retrieve the descriptors
   --  Args are the arguments used to launch the program
   --  Execution_Directory is the working directory on the remote machine
   --  Buffer_Size is the maximum buffer size for process output reception
   --   (set Buffer_Size to 0 for dynamic memory allocation)
   --  Err_To_Out tells if the stderr shall be redirected to stdout
   --  If a request from user is needed, main_window is used as parent for the
   --  dialog presented to the user.

   procedure Sync_Execute
     (Host                : String;
      Args                : GNAT.OS_Lib.Argument_List;
      Status              : out Boolean;
      Execution_Directory : String  := "");
   --  Spawns synchronously a remote program and returns its status
   --  Host : Host on which the program is launched
   --  Args : The program to launch
   --  Status : The return status
   --  Execution_Directory: Where the program is launched.

   procedure Sync_Execute
     (Host                : String;
      Args                : GNAT.OS_Lib.Argument_List;
      Out_Value           : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : String  := "");
   --  Same as above, except that the program output is also returned

   procedure Close_All;
   --  Closes all opened connection.

   --  Note on the following expect procedures. Contrary to its ancestor,
   --  this package does not handle nicely the timeout value of -1 (infinite
   --  timeout), that can really lead to infinite wait. So this value is
   --  automatically replaced with a timeout of 1s.

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

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

   type Machine_Descriptor_Item;
   type Machine_Descriptor_Access is access all Machine_Descriptor_Item;

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with record
      Busy                 : Boolean                   := False;
      --  Tells if the remote shell is busy processing a command
      Current_Echo_Skipped : Boolean := False;
      --  Tells if the command we've sent has been echoed or not.
      Shell                : Shell_Descriptor_Access   := null;
      --  What shell is on the remote server
      Machine              : Machine_Descriptor_Access := null;
      --  What machine this descriptor is connected to
      Use_Cr_Lf            : Boolean := False;
      --  Tell if CR shall be sent along with LF
      Session_Nb           : Natural := 0;
      --  Session number on this machine
      Terminated           : Boolean := False;
      --  Tells if the command has finished
      Session_Died         : Boolean := False;
      --  Tells if the shell died unexpectedly
      Status               : Integer := 0;
      --  Records the status of the finished command
      R_Filters_Lock       : Integer := 0;
      --  Tells if the filters are locked
      R_Filters            : Filter_List := null;
      --  List of filters. Filter_List is defined in GNAT.Expect
      Nb_Password_Prompt   : Natural := 0;
      --  Number of password prompts currently encountered
   end record;

end GNAT.Expect.TTY.Remote;

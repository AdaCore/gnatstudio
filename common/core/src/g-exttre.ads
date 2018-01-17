------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2006-2018, AdaCore                    --
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

with Ada.Unchecked_Deallocation;
with System;          use System;

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.Regpat;     use GNAT.Regpat;
with GNAT.Strings;    use GNAT.Strings;

with GNATCOLL.VFS;    use GNATCOLL.VFS;

with Gexpect;         use Gexpect;

package GNAT.Expect.TTY.Remote is

   No_Session_Available : exception;

   ----------------------
   -- Expect interface --
   ----------------------

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with private;
   type Remote_Process_Descriptor_Access is
     access all Remote_Process_Descriptor'Class;

   overriding procedure Add_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function;
      Filter_On  : Filter_Type := Output;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False);
   --  Add a new filter for one of the filter type. This filter will be
   --  run before all the existing filters, unless After is set True,
   --  in which case it will be run after existing filters. User_Data
   --  is passed as is to the filter procedure.

   overriding procedure Remove_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function);
   --  Remove a filter from the list of filters (whatever the type of the
   --  filter).

   overriding procedure Lock_Filters
     (Descriptor : in out Remote_Process_Descriptor);
   --  Temporarily disables all output and input filters. They will be
   --  reactivated only when Unlock_Filters has been called as many times as
   --  Lock_Filters;

   overriding procedure Unlock_Filters
     (Descriptor : in out Remote_Process_Descriptor);
   --  Unlocks the filters. They are reactivated only if Unlock_Filters
   --  has been called as many times as Lock_Filters.

   overriding procedure Send
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
      Execution_Directory : Filesystem_String := "";
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
      Execution_Directory : Filesystem_String  := "");
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
      Execution_Directory : Filesystem_String  := "");
   --  Same as above, except that the program output is also returned

   function Is_Ready_Session (Nickname : String) return Boolean;
   --  Tell if a ready session is available for specified server

   procedure Close_All (Host : String);
   procedure Close_All;
   --  Closes all opened connections.

   --  Note on the following expect procedures. Contrary to its ancestor,
   --  this package does not handle nicely the timeout value of -1 (infinite
   --  timeout), that can really lead to infinite wait. So this value is
   --  automatically replaced with a timeout of 1s.

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  See parent for description

private

   --  The following methods are inherited. See parent for explanations.

   overriding procedure Close
     (Descriptor : in out Remote_Process_Descriptor;
      Status     : out Integer);

   overriding procedure Close
     (Descriptor : in out Remote_Process_Descriptor);

   overriding procedure Interrupt
     (Descriptor : in out Remote_Process_Descriptor);

   type Shell_State_Type is (OFF, BUSY, READY);
   --  The state of a session.
   --  OFF: the session has not been launched
   --  BUSY: the session is busy processing a remote program
   --  READY: the session has been launched, and is waiting on a shell prompt

   type Session is record
      Pd    : TTY_Process_Descriptor;
      Cr_Lf : Cr_Lf_Handling;
      State : Shell_State_Type := OFF;
   end record;
   --  This record represents a machine's session. A session is an opened
   --  connection that can be reused to launch successive remote programs.

   type Session_Array is
     array (Natural range <>) of Session;

   type TTY_Data_Record (Max_Nb_Connections : Natural)
     is new Machine_User_Data_Type
   with record
      Sessions          : Session_Array (1 .. Max_Nb_Connections);
      Echoing           : Boolean := False;
      Determine_Echoing : Boolean := True;
      Ref_Counter       : Natural := 1;
   end record;
   type TTY_Data_Access is access all TTY_Data_Record;

   type Remote_Process_Descriptor is new TTY_Process_Descriptor with record
      Busy                 : Boolean                   := False;
      --  Tells if the remote shell is busy processing a command
      Current_Echo_Skipped : Boolean := False;
      --  Tells if the command we've sent has been echoed or not.
      Machine              : Machine_Access := null;
      --  What machine this descriptor is connected to
      Use_Cr_Lf            : Cr_Lf_Handling := Auto;
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

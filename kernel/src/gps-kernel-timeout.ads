------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Commands;                use Commands;
with GNAT.Expect;             use GNAT.Expect;
with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Interactive_Consoles;    use Interactive_Consoles;
with Remote;                  use Remote;

package GPS.Kernel.Timeout is

   type External_Process_Data is tagged private;
   type External_Process_Data_Access is access all External_Process_Data'Class;
   --  Data that is passed to the callbacks

   procedure Launch_Process
     (Scheduled            : out Scheduled_Command_Access;
      Success              : out Boolean;
      Data                 : access External_Process_Data'Class := null;
      Kernel               : not null access Kernel_Handle_Record'Class;
      CL                   : Arg_List;
      Server               : Server_Type := GPS_Server;
      Console              : Interactive_Consoles.Interactive_Console := null;
      Use_Ext_Terminal     : Boolean := False;
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Line_By_Line         : Boolean := False;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True;
      Block_Exit           : Boolean := True;
      Start_Immediately    : Boolean := False;
      Active               : Boolean := False);
   --  Launch a given command with arguments on server 'Server'.
   --  Arguments must be freed by the user.
   --
   --  Data is freed automatically.
   --
   --  Output is sent to Console, if not null and Show_Output is True, or
   --  discarded otherwise.
   --
   --  Check GPS.Kernel.Console.Create_Interactive_Console and
   --  GPS.Kernel.Console.Get_Console.
   --  If Show_Command is True and the output is displayed, the command
   --  itself is displayed in the console.
   --  If the console is specified and is closed while the process is
   --  running, a dialog will be displayed asking the user whether the
   --  process should be killed as well.
   --
   --  The output is processed by large chunks when possible, except when
   --  Line_By_Line is set to True, In this case, each line output by the
   --  external process is handled independently (and passed to On_Output).
   --
   --  If Directory is not empty, move to Dir before launching the command,
   --  and change back to the current directory once the command is spawned.
   --  This applies to the local host, not the remote host.
   --
   --  If Use_Ext_Terminal is true, then the command is launched on an external
   --  terminal instead of an interactive_console
   --
   --  Show_In_Task_Manager tells whether the underlying command should be
   --  visible in the task manager. If Name_In_Task_Manager is different from
   --  the empty string, then this command will have this name, else the
   --  command will be used.

   --  Synchronous is used to know whether Launch_Process should
   --  return immediately or wait for the process to complete. In addition, if
   --  Active is True and Synchronous is False, GPS will more frequently check
   --  the output of the external process, assuming this is "urgent".
   --
   --  If Start_Immediately is True, the external process is spawn right away.
   --  Otherwise, it will be spawned as soon as the background loop (idle or
   --  timeout) runs for the first time, or Expect is called.
   --
   --  If Show_Exit_Status is true, the exit status of the process will be
   --  displayed in the Console.
   --
   --  If Timeout is not -1, then the launched process will be interrupted
   --  after Timeout ms if no output is received.
   --
   --  If Strip_CR is set, then output from the process is stripped from CR
   --  characters.
   --
   --  Use_Pipes tells if process communication shall be performed with pipes
   --  or through the console on Windows
   --
   --  Block_Exit indicates whether the fact that this process is running
   --  should prevent GPS from closing.

   procedure Set_Progress_Regexp
     (Self              : in out External_Process_Data;
      Regexp            : String;
      Group_For_Current : Natural := 1;
      Group_For_Total   : Natural := 2);
   --  A special regular expression to match on the output of the external
   --  process to detect its progress. This assumes this process is able to
   --  display that progress somehow.
   --  Group_For_* indicate which parenthesis group matches resp. the current
   --  and the total progress.
   --  ??? Could we use the output parsers instead ?

   procedure On_Output
     (Self     : not null access External_Process_Data;
      External : not null access Root_Command'Class;
      Output   : String) is null;
   --  Called when some new data is available from the process.
   --  Override to perform your own processing, but you should in general
   --  call the inherited On_Output.
   --  Command is the one that executes the external process, not the one
   --  manipulated by the task manager.

   procedure On_Before_Kill
     (Self     : not null access External_Process_Data;
      External : not null access Root_Command'Class) is null;
   --  Called just before the external process is killed by a call to
   --  Interrupt (or when GPS terminates).
   --  Not called when the process terminates normally.
   --  When this is called, it is still possible to send commands to the
   --  process (and thus for instance ask it to save some data).

   procedure On_Exit
     (Self     : not null access External_Process_Data;
      External : not null access Root_Command'Class) is null;
   --  Called when the process terminates

   procedure Free (Self : in out External_Process_Data) is null;
   --  Free memory used by Self

   function Kernel
     (Self : not null access External_Process_Data)
     return not null access Kernel_Handle_Record'Class
     with Inline;
   --  Return the kernel

   function Descriptor
     (Self : not null access External_Process_Data)
      return access GNAT.Expect.Process_Descriptor'Class
     with Inline;
   --  Return a handle on the external process

   function Process_Died
     (Self : not null access External_Process_Data) return Boolean
     with Inline;
   --  Whether the external process has finished executing

   function Exit_Status
     (Self : not null access External_Process_Data) return Integer
     with Inline;
   --  Return the exit status of the command.
   --  This is only meaningful once the external process has finished running

   type Expect_Status is (Matched, Timed_Out, Died);
   function Expect
     (Self                : not null access External_Process_Data'Class;
      Regexp              : GNAT.Regpat.Pattern_Matcher;
      Timeout             : Integer;
      Output              : out Ada.Strings.Unbounded.Unbounded_String;
      Stop_At_First_Match : Boolean := True)
      return Expect_Status;
   --  Check whether the external process's output buffer contains data that
   --  matches Regexp. Wait for such data for up to Timeout milliseconds.
   --  Calls On_Output and On_Exit as appropriate.
   --  Output contains the full output since the last call to On_Output.
   --  The external process is spawned if not done yet.
   --  If Stop_At_First_Match is False, this function will process all
   --  available data from the external process. This is in general more
   --  efficient when the regexp is not relevant and we just want to get to
   --  the end of the process as fast as possible.

   procedure Suspend_Monitoring
     (Self    : not null access External_Process_Data'Class;
      Suspend : Boolean);
   --  If Suspend is True, stop monitoring the external process (although the
   --  process itself still runs). This is useful if you are doing your own
   --  monitoring loop (which calls Expect above)

private

   type Progress_Support is record
      Regexp  : Pattern_Matcher_Access;
      Current : Natural := 1;
      Final   : Natural := 2;
   end record;

   type External_Process_Data is tagged record
      Kernel       : access Kernel_Handle_Record'Class;
      Command      : Command_Access;   --  a Monitor command
      Descriptor   : GNAT.Expect.Process_Descriptor_Access;
      Console      : Interactive_Consoles.Interactive_Console;
      Progress     : Progress_Support;
      On_Exit_Run  : Boolean := False;
      Process_Died : Boolean := False;
      Exit_Status  : Integer := 0;
      Strip_CR     : Boolean;
      Show_Output  : Boolean;

      Monitoring_Stopped : Boolean := False;
   end record;

   function Kernel
     (Self : not null access External_Process_Data)
      return not null access Kernel_Handle_Record'Class is (Self.Kernel);

   function Descriptor
     (Self : not null access External_Process_Data)
      return access GNAT.Expect.Process_Descriptor'Class is (Self.Descriptor);

   function Process_Died
     (Self : not null access External_Process_Data) return Boolean
     is (Self.Process_Died);

   function Exit_Status
     (Self : not null access External_Process_Data) return Integer
     is (Self.Exit_Status);

end GPS.Kernel.Timeout;

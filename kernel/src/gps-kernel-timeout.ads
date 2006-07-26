-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
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

with Ada.Unchecked_Deallocation;
with GNAT.Expect;
with GNAT.OS_Lib;

with Gtk.Main;
with Interactive_Consoles;
with Commands;                use Commands;

with Remote;                  use Remote;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;

package GPS.Kernel.Timeout is

   type Process_Data;

   type Output_Callback is
     access procedure (Data : Process_Data; Output : String);
   --  This callback is called whenever some output is read from the file
   --  descriptor.

   type Exit_Callback is
     access procedure (Data : Process_Data; Status : Integer);
   --  Callback called when an underlying process launched by Launch_Process
   --  terminates.

   type Callback_Data_Record is abstract tagged null record;
   type Callback_Data_Access is access all Callback_Data_Record'Class;
   procedure Destroy (Data : in out Callback_Data_Record);
   --  Destroy the memory allocated for Data

   type Process_Data is record
      Kernel        : Kernel_Handle;
      Descriptor    : GNAT.Expect.Process_Descriptor_Access;
      Callback      : Output_Callback;
      Exit_Cb       : Exit_Callback;
      Callback_Data : Callback_Data_Access;

      Command       : Commands.Command_Access;
      --  The command in which the process is wrapped. There might be no such
      --  command if Launch_Process was called with Show_In_Task_Manager set to
      --  False.
      --  This command can be used to report the progress of the action, so
      --  that the task manager is usefully updated.

      Process_Died : Boolean := False;
      --  This flag is true when the process is died, false otherwise
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package Process_Timeout is new Gtk.Main.Timeout (Process_Data);

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      Command              : String;
      Arguments            : GNAT.OS_Lib.Argument_List;
      Server               : Server_Type := GPS_Server;
      Console              : Interactive_Consoles.Interactive_Console := null;
      Callback             : Output_Callback := null;
      Exit_Cb              : Exit_Callback := null;
      Success              : out Boolean;
      Use_Ext_Terminal     : Boolean := False;
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : String := "";
      Show_In_Task_Manager : Boolean := True;
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True);
   --  Launch a given command with arguments on server 'Server'.
   --  Arguments must be freed by the user.
   --
   --  Set Success to True if the command could be spawned.
   --  Callback will be called asynchronousely when some new data is
   --  available from the process.
   --  Exit_Callback will be called when the underlying process dies.
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
   --  If Line_By_Line is True, then the output of the command is processed
   --  line by line, instead of being processed with as big chunks as possible.
   --  If it is false, there is no garantee where the chunks will be splitted.
   --
   --  If Directory is not empty, move to Dir before launching the command,
   --  and change back to the current directory once the command is spawned.
   --  This applies to the local host, not the remote host.
   --
   --  If Use_Ext_Terminal is true, then the command is launched on an external
   --  terminal instead of an interactive_console
   --
   --  If Show_In_Task_Manager is true, then a Command wrapper will be set up
   --  so that the process appears in the task manager and can be interrupted
   --  by the user. In this case, Synchronous is taken into account to know
   --  whether Launch_Process should return immediately or wait for the process
   --  to complete.
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
   --  Callback_Data is freed automatically when the process terminates.
   --
   --  Use_Pipes tells if process communication shall be performed with pipes
   --   or through the console on Windows

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      Command              : String;
      Arguments            : GNAT.OS_Lib.Argument_List;
      Server               : Server_Type := GPS_Server;
      Console              : Interactive_Consoles.Interactive_Console := null;
      Callback             : Output_Callback := null;
      Exit_Cb              : Exit_Callback := null;
      Success              : out Boolean;
      Use_Ext_Terminal     : Boolean := False;
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : String := "";
      Show_In_Task_Manager : Boolean := True;
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True;
      Created_Command      : out Scheduled_Command_Access);
   --  Same as above, and returns the created Command_Access.
   --  Cmd is allocated by this procedure, and will be cleaned automatically,
   --  and should not be freed by the caller of Launch_Process (although it is
   --  of course authorized to Interrupt the Cmd).

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      Command              : String;
      Arguments            : GNAT.OS_Lib.Argument_List;
      Console              : Interactive_Consoles.Interactive_Console := null;
      Callback             : Output_Callback := null;
      Exit_Cb              : Exit_Callback := null;
      Success              : out Boolean;
      Use_Ext_Terminal     : Boolean := False;
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : String := "";
      Show_In_Task_Manager : Boolean := True;
      Queue_Id             : String := "";
      Show_Exit_Status     : Boolean := False;
      Use_Pipes            : Boolean := True;
      Fd                   : out GNAT.Expect.Process_Descriptor_Access;
      Created_Command      : out Scheduled_Command_Access);
   --  Same as above, and returns the created File_Descriptor.
   --  This process is necessarily launched locally.

private

end GPS.Kernel.Timeout;

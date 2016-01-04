------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.Expect;
with Glib.Main;
with Commands;                use Commands;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Interactive_Consoles;
with Remote;                  use Remote;
with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;

package GPS.Kernel.Timeout is

   type Process_Data;

   type Output_Callback is access procedure
     (Data    : Process_Data;
      Command : not null access Root_Command'Class;
      Output  : String);
   --  This callback is called whenever some output is read from the file
   --  descriptor.
   --  Command is the command that executes the external process, not the
   --  command manipulated by the task manager. As such, it cannot be passed
   --  to the task manager.

   type Exit_Callback is access procedure
     (Data    : Process_Data;
      Command : not null access Root_Command'Class;
      Status  : Integer);
   --  Callback called when an underlying process launched by Launch_Process
   --  terminates.
   --  Status is the exit status of the process. If the process could not
   --  be launched, status is set to -1.

   type Callback_Data_Record is abstract tagged null record;
   type Callback_Data_Access is access all Callback_Data_Record'Class;
   procedure Destroy (Data : in out Callback_Data_Record) is null;
   --  Destroy the memory allocated for Data

   type Process_Data is record
      Kernel        : Kernel_Handle;
      Descriptor    : GNAT.Expect.Process_Descriptor_Access;
      Callback      : Output_Callback;
      Exit_Cb       : Exit_Callback;
      Callback_Data : Callback_Data_Access;

      Process_Died : Boolean := False;
      --  This flag is true when the process has died, false otherwise
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package Process_Timeout is new Glib.Main.Generic_Sources (Process_Data);

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      CL                   : Arg_List;
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
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True;
      Block_Exit           : Boolean := True);
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
   --  Show_In_Task_Manager tells whether the underlying command should be
   --  visible in the task manager. If Name_In_Task_Manager is different from
   --  the empty string, then this command will have this name, else the
   --  command will be used.

   --  Synchronous is used to know whether Launch_Process should
   --  return immediately or wait for the process to complete.
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
   --
   --  Block_Exit indicates whether the fact that this process is running
   --  should prevent GPS from closing.

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      CL                   : Arg_List;
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
      Created_Command      : out Scheduled_Command_Access);
   --  Same as above, and returns the created Command_Access.
   --  Cmd is allocated by this procedure, and will be cleaned automatically,
   --  and should not be freed by the caller of Launch_Process (although it is
   --  of course authorized to Interrupt the Cmd).

   procedure Launch_Process
     (Kernel               : Kernel_Handle;
      CL                   : Arg_List;
      Console              : Interactive_Consoles.Interactive_Console := null;
      Callback             : Output_Callback := null;
      Exit_Cb              : Exit_Callback := null;
      Success              : out Boolean;
      Use_Ext_Terminal     : Boolean := False;
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Show_Exit_Status     : Boolean := False;
      Use_Pipes            : Boolean := True;
      Fd                   : out GNAT.Expect.Process_Descriptor_Access;
      Created_Command      : out Scheduled_Command_Access);
   --  Same as above, and returns the created File_Descriptor.
   --  This process is necessarily launched locally.

end GPS.Kernel.Timeout;

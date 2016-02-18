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

with GNAT.Expect;
with Commands;                use Commands;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Interactive_Consoles;
with Remote;                  use Remote;
with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;

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
      Block_Exit           : Boolean := True);
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
   --  Use_Pipes tells if process communication shall be performed with pipes
   --   or through the console on Windows
   --
   --  Block_Exit indicates whether the fact that this process is running
   --  should prevent GPS from closing.

   procedure On_Output
     (Self     : not null access External_Process_Data;
      External : not null access Root_Command'Class;
      Output   : String) is null;
   --  Called when some new data is available from the process.
   --  Override to perform your own processing, but you should in general
   --  call the inherited On_Output.
   --  Command is the one that executes the external process, not the one
   --  manipulated by the task manager.

   procedure On_Exit
     (Self     : not null access External_Process_Data;
      External : not null access Root_Command'Class;
      Status   : Integer) is null;
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

private

   type External_Process_Data is tagged record
      Kernel       : access Kernel_Handle_Record'Class;
      Descriptor   : GNAT.Expect.Process_Descriptor_Access;
      On_Exit_Run  : Boolean := False;
      Process_Died : Boolean := False;
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

end GPS.Kernel.Timeout;

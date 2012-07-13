------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Calendar;               use Ada, Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Unchecked_Conversion;
with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;                use GNAT.Regpat;
with System;                     use System;

with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib.Object;                use Glib.Object;
with Glib.Values;
with Glib.Main;                  use Glib.Main;

with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Dialogs;             use Gtkada.Dialogs;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Remote;          use GPS.Kernel.Remote;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;

package body GPS.Kernel.Timeout is

   Me : constant Debug_Handle := Create ("Timeout");

   type Console_Process_Data is new GObject_Record with record
      CL                   : Arg_List;
      Server               : Server_Type;
      Console              : Interactive_Console;
      Delete_Id            : Gtk.Handlers.Handler_Id;
      Strip_CR             : Boolean;
      Use_Pipes            : Boolean;
      Show_Output          : Boolean;
      Show_Command         : Boolean;
      Show_Exit_Status     : Boolean;
      Synchronous          : Boolean;
      Use_Ext_Terminal     : Boolean;
      Directory            : Virtual_File;

      Expect_Regexp        : GNAT.Expect.Pattern_Matcher_Access;

      D                    : Process_Data;
      Died                 : Boolean := False;
      --  Indicates that the process has died

      Interrupted          : Boolean := False;
      --  Whether the process was interrupted by the user

      Started              : Boolean := False;
      --  Whether the process has been started

      Timeout              : Integer;
      --  How many time do we wait for first output

      Start_Time           : Ada.Calendar.Time;
      --  Start time of the process

      Id                   : G_Source_Id := No_Source_Id;
   end record;
   type Console_Process is access all Console_Process_Data'Class;

   package Console_Process_Timeout is new
     Glib.Main.Generic_Sources (Console_Process);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Callback_Data_Record'Class, Callback_Data_Access);

   Id : Natural := 0;
   function Get_New_Queue_Id (QId : String) return String;
   --  Returns a new unique queue id

   function Process_Cb (Data : Console_Process) return Boolean;
   --  Generic callback for async spawn of processes

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" event

   procedure Cleanup (Data : Console_Process);
   --  Close the process descriptor and mark the process as terminated in the
   --  task manager.

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String;
   --  Handler for user input on the console

   type Monitor_Command is new Root_Command with record
      Name : String_Access;
      Data : Console_Process;
   end record;
   type Monitor_Command_Access is access all Monitor_Command'Class;
   --  Command that can be used to monitor an external process through the task
   --  manager, and make it interruptible by users. No special handling of
   --  the output is done, since this is assumed to be done through the call
   --  to Launch_Process already. Closing the console terminates the process.

   overriding procedure Interrupt (Command : in out Monitor_Command);
   --  Interrupts the command

   overriding procedure Free (D : in out Monitor_Command);
   overriding function Execute
     (Command : access Monitor_Command) return Command_Return_Type;
   overriding function Name (Command : access Monitor_Command) return String;
   --  See inherited documentation

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Monitor_Command) is
   begin
      if Command.Data.D.Descriptor /= null then
         Interrupt (Command.Data.D.Descriptor.all);
         Close (Command.Data.D.Descriptor.all);
         Command.Data.Interrupted := True;
      end if;
   end Interrupt;

   ----------
   -- Free --
   ----------

   overriding procedure Free (D : in out Monitor_Command) is
      PID : GNAT.Expect.Process_Id;
   begin
      if not D.Data.Died and then D.Data.D.Descriptor /= null then
         PID := Get_Pid (D.Data.D.Descriptor.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (D.Data.D.Descriptor.all);
            Close (D.Data.D.Descriptor.all);
            D.Data.Interrupted := True;
         end if;
      end if;

      Cleanup (D.Data);

      --  Free memory

      Free (D.Name);
      D.Data.D.Command := null;

      Pop_State (D.Data.D.Kernel);

      Unchecked_Free (D.Data.Expect_Regexp);

      if D.Data.D.Callback_Data /= null then
         Destroy (D.Data.D.Callback_Data.all);
         Unchecked_Free (D.Data.D.Callback_Data);
      end if;

      Unref (D.Data);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Monitor_Command) return Command_Return_Type
   is
      Timeout : constant Guint := 50;
      Success : Boolean;
   begin
      if not Command.Data.Started then
         Trace (Me, "Starting the program " & Command.Name.all);

         if Command.Data.Console /= null then
            Trace (Me, "Connect the command_handler to the console");
            Set_Command_Handler
              (Command.Data.Console, Data_Handler'Access,
               Command.Data.all'Address);

            Command.Data.Delete_Id := Object_Return_Callback.Object_Connect
              (Command.Data.Console, Gtk.Widget.Signal_Delete_Event,
               Delete_Handler'Access, GObject (Command.Data));
         end if;

         Command.Data.Start_Time := Ada.Calendar.Clock;

         Trace (Me, "Spawn the process");

         Spawn (Command.Data.D.Kernel,
                Command.Data.CL,
                Command.Data.Server,
                Command.Data.D.Descriptor,
                Success,
                Command.Data.Use_Ext_Terminal,
                Command.Data.Console,
                Command.Data.Show_Command,
                Command.Data.Directory,
                Command.Data.Use_Pipes);
         --  Set Started here so that even if spawn fails we don't pass twice
         --  here
         Command.Data.Started := True;

         if Success then
            if not Command.Data.Synchronous then
               Command.Data.Id := Console_Process_Timeout.Timeout_Add
                 (Timeout, Process_Cb'Access, Command.Data);
            end if;

            return Execute_Again;

         else
            Trace (Me, "Failure when spawning the process");

            --  We could not launch the process: call the Exit_Cb nonetheless,
            --  as it may be used to keep count of executions, or to free
            --  memory, for instance.
            if Command.Data.D.Exit_Cb /= null then
               begin
                  Command.Data.D.Exit_Cb (Command.Data.D, -1);

               exception
                  when E : others =>
                     Trace (Exception_Handle, E);
               end;
            end if;

            Free (Command.Data.D.Descriptor);
            Command.Data.Died := True;
            return Failure;
         end if;

      elsif Command.Data.D.Descriptor = null
        or else Command.Data.Died
      then
         Trace (Me, "Process is finished");
         return Failure;

      else
         if Command.Data.Synchronous then
            Success := Process_Cb (Command.Data);
         end if;

         return Execute_Again;
      end if;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name (Command : access Monitor_Command) return String is
   begin
      if Command.Name /= null then
         return Command.Name.all;
      else
         return "Command";
      end if;
   end Name;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Data : Console_Process) is
      Status  : Integer;
      Console : Interactive_Console := Data.Console;
   begin
      if Data.Id /= No_Source_Id then
         Remove (Data.Id);
         Data.Id := No_Source_Id;
      end if;

      if Data.D.Descriptor = null then
         return;
      end if;

      Close (Data.D.Descriptor.all, Status);

      --  So that next call to Cleanup does nothing
      Free (Data.D.Descriptor);

      if Data.Console = null then
         Console := Get_Console (Data.D.Kernel);
      end if;

      declare
         End_Time      : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Time_Stamp    : constant String :=
                           "[" & Image (End_Time, ISO_Date & " %T") & "] ";
         Elapsed       : constant String :=
                           Calendar.Formatting.Image
                             (End_Time - Data.Start_Time,
                              Include_Time_Fraction => True);
         Elapsed_Start : Natural := Elapsed'First;

      begin
         --  The console might no longer exists if we are exiting GPS
         if Console /= null then
            --  Do not show hours and minutes if they are 0. The output is
            --  thus similar to the one of the Unix command time

            if Elapsed (Elapsed_Start .. Elapsed_Start + 1) = "00" then
               Elapsed_Start := Elapsed_Start + 3;
            end if;

            if Elapsed (Elapsed_Start .. Elapsed_Start + 1) = "00" then
               Elapsed_Start := Elapsed_Start + 3;
            end if;

            if Data.Interrupted then
               Insert (Console, Time_Stamp &
                       (-"<^C> process interrupted (elapsed time: ")
                       & Elapsed (Elapsed_Start .. Elapsed'Last) & "s)");
               --  ??? elsif Data.Show_Output or else Data.Show_Command then
            elsif Data.Show_Exit_Status then
               if Status = 0 then
                  Insert (Console, Time_Stamp &
                          (-"process terminated successfully (elapsed time: ")
                          & Elapsed (Elapsed_Start .. Elapsed'Last) & "s)");
               else
                  Insert (Console, Time_Stamp
                          & (-"process exited with status ")
                          & Image (Status) & " (elapsed time: "
                          & Elapsed (Elapsed_Start .. Elapsed'Last) & "s)");
               end if;
            end if;
         end if;
      end;

      if Data.D.Exit_Cb /= null then
         begin
            Data.D.Exit_Cb (Data.D, Status);

         exception
            when E : others =>
               Trace (Exception_Handle, (E));
         end;
      end if;
   end Cleanup;

   ----------------
   -- Process_Cb --
   ----------------

   function Process_Cb (Data : Console_Process) return Boolean is
      Fd     : Process_Descriptor_Access;
      Result : Expect_Match;

      function Conditional_Strip_CR (S : String) return String;
      --  Return a stripped from CR string if Data tells so

      --------------------------
      -- Conditional_Strip_CR --
      --------------------------

      function Conditional_Strip_CR (S : String) return String is
      begin
         if Data.Strip_CR then
            return Strip_CR (S);
         else
            return S;
         end if;
      end Conditional_Strip_CR;

   begin
      if Data = null or else Data.Died then
         return False;
      end if;

      Fd := Data.D.Descriptor;
      if Fd /= null then
         loop
            Expect (Fd.all, Result, Data.Expect_Regexp.all, Timeout => 1);

            if Result /= Expect_Timeout then
               --  Received something. Cancel timeout
               Data.Timeout := -1;

               declare
                  Output : constant String :=
                             Conditional_Strip_CR (Expect_Out (Fd.all));
               begin
                  if Data.Console /= null
                    and then Data.Show_Output
                  then
                     Insert (Data.Console, Output, Add_LF => False);

                     --  ??? This might be costly, we could cache this MDI
                     --  Child.
                     Highlight_Child
                       (Find_MDI_Child
                          (Get_MDI (Data.D.Kernel), Data.Console));
                  end if;

                  if Data.D.Callback /= null then
                     Data.D.Callback (Data.D, Output);
                  end if;
               end;

            else
               if Data.Timeout /= -1
                 and then Ada.Calendar.Clock - Data.Start_Time >
                   Duration (Data.Timeout) /  1000.0
               then
                  --  Make sure the process is killed. Just interrupting it is
                  --  sometimes not enough.
                  Close (Fd.all);
               end if;

               exit;
            end if;
         end loop;
      else
         raise Process_Died;
      end if;

      return True;

   exception
      when Process_Died =>
         if Fd /= null then
            declare
               Output : constant String :=
                          Conditional_Strip_CR (Expect_Out (Fd.all));
            begin
               if Data.D.Callback /= null then
                  Data.D.Process_Died := True;
                  Data.D.Callback (Data.D, Output);
               end if;

               if Data.Console /= null then
                  --  Display all remaining output

                  Insert (Data.Console, Output, Add_LF => False);
                  Highlight_Child
                    (Find_MDI_Child (Get_MDI (Data.D.Kernel), Data.Console));
               end if;
            end;
         end if;

         if Data.Console /= null
           and then Data.Console /= Get_Console (Data.D.Kernel)
         then
            Enable_Prompt_Display (Data.Console, False);
         end if;

         if Data.Delete_Id.Id /= Null_Handler_Id
           and Data.Console /= null
         then
            Gtk.Handlers.Disconnect (Data.Console, Data.Delete_Id);
         end if;

         Data.Died := True;
         Cleanup (Data);
         return False;

      when E : others =>
         Trace (Exception_Handle, E);
         Cleanup (Data);
         return False;
   end Process_Cb;

   ------------------
   -- Data_Handler --
   ------------------

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String
   is
      pragma Unreferenced (Console);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Console_Process);
      Process : constant Console_Process := Convert (User_Data);
   begin
      if not Process.Died then
         --  ??? If Process.D.Descriptor is null then Process.Died should be
         --  True. This is being investigated under EC06-004.
         Send (Process.D.Descriptor.all, Input);
      end if;

      return "";

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Cleanup (Process);
         return "";
   end Data_Handler;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Callback_Data_Record) is
      pragma Unreferenced (Data);
   begin
      null;
   end Destroy;

   ----------------------
   -- Get_New_Queue_Id --
   ----------------------

   function Get_New_Queue_Id (QId : String) return String is
      Str_Id : constant String := Natural'Image (Id);
   begin
      if QId = "" then
         Id := Id + 1;
         return "gps-kernel-timeout" & Str_Id;
      else
         return QId;
      end if;
   end Get_New_Queue_Id;

   --------------------
   -- Launch_Process --
   --------------------

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
      Directory            : Virtual_File := No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True;
      Created_Command      : out Scheduled_Command_Access)
   is
      Q_Id          : constant String := Get_New_Queue_Id (Queue_Id);
      C             : Monitor_Command_Access;
      No_Handler    : Handler_Id;
      Expect_Regexp : GNAT.Expect.Pattern_Matcher_Access;
   begin
      Push_State (Kernel, Processing);

      if not Is_Local (Server) then
         Synchronize (Kernel, GPS_Server, Server,
                      Blocking      => False,
                      Print_Command => True,
                      Print_Output  => False,
                      Force         => False,
                      Queue_Id      => Q_Id);
      end if;

      C := new Monitor_Command;

      No_Handler.Id := Null_Handler_Id;

      if Line_By_Line then
         Expect_Regexp := new Pattern_Matcher'(Compile ("^.*?\n"));
      else
         Expect_Regexp := new Pattern_Matcher'(Compile (".*$", Single_Line));
      end if;

      if Name_In_Task_Manager /= "" then
         C.Name := new String'(Name_In_Task_Manager);
      else
         C.Name := new String'(Get_Command (CL));
      end if;

      C.Data := new Console_Process_Data'
        (GObject_Record with
         CL                   => CL,
         Server               => Server,
         Console              => Console,
         Use_Ext_Terminal     => Use_Ext_Terminal,
         Directory            => Directory,
         Delete_Id            => No_Handler,
         Show_Output          => Show_Output,
         Show_Command         => Show_Command,
         Show_Exit_Status     => Show_Exit_Status,
         Strip_CR             => Strip_CR,
         Use_Pipes            => Use_Pipes,
         Synchronous          => Synchronous,
         Expect_Regexp        => Expect_Regexp,
         D                    => (Kernel        => Kernel,
                                  Descriptor    => null,
                                  Callback      => Callback,
                                  Exit_Cb       => Exit_Cb,
                                  Callback_Data => Callback_Data,
                                  Command       => Command_Access (C),
                                  Process_Died  => False),
         Died                 => False,
         Interrupted          => False,
         Started              => False,
         Start_Time           =>
           Time_Of (Year_Number'First, Month_Number'First, Day_Number'First),
         Id                   => 0,
         Timeout              => Timeout);
      Initialize (C.Data);

      if Synchronous then
         Launch_Synchronous (Command_Access (C), 0.1);
         Unref (Command_Access (C));

      else
         --  ??? Add_Alternate_Action: sync even if main action fails
--           Add_Alternate_Action
--             (Item   => C,
--              Action =>
         Created_Command := Launch_Background_Command
           (Kernel,
            Command_Access (C),
            Active   => False,
            Show_Bar => Show_In_Task_Manager,
            Queue_Id => Q_Id);
      end if;

      if not Is_Local (Server) then
         Synchronize (Kernel, Server, GPS_Server,
                      Blocking      => False,
                      Print_Command => True,
                      Print_Output  => False,
                      Force         => False,
                      Queue_Id      => Q_Id);
      end if;

      Success := True;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Kernel);
         Success := False;
   end Launch_Process;

   --------------------
   -- Launch_Process --
   --------------------

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
      Directory            : Virtual_File := No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Show_Exit_Status     : Boolean := False;
      Use_Pipes            : Boolean := True;
      Fd                   : out GNAT.Expect.Process_Descriptor_Access;
      Created_Command      : out Scheduled_Command_Access)
   is
   begin
      Launch_Process
        (Kernel               => Kernel,
         CL                   => CL,
         Server               => GPS_Server,
         Console              => Console,
         Callback             => Callback,
         Exit_Cb              => Exit_Cb,
         Success              => Success,
         Use_Ext_Terminal     => Use_Ext_Terminal,
         Show_Command         => Show_Command,
         Show_Output          => Show_Output,
         Callback_Data        => Callback_Data,
         Line_By_Line         => Line_By_Line,
         Directory            => Directory,
         Show_In_Task_Manager => Show_In_Task_Manager,
         Name_In_Task_Manager => Name_In_Task_Manager,
         Queue_Id             => Queue_Id,
         Synchronous          => False,
         Show_Exit_Status     => Show_Exit_Status,
         Use_Pipes            => Use_Pipes,
         Created_Command      => Created_Command);

      if Success
        and then Execute (Monitor_Command_Access
                            (Get_Command (Created_Command)))
        = Execute_Again
      then
         Fd := Monitor_Command_Access
           (Get_Command (Created_Command)).Data.D.Descriptor;
      else
         Fd := null;
         Success := False;
         --  Interrupt just launched command because program did not start
         Interrupt_Queue (Kernel, Get_Command (Created_Command));
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Success := False;
   end Launch_Process;

   --------------------
   -- Launch_Process --
   --------------------

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
      Directory            : Virtual_File := No_File;
      Show_In_Task_Manager : Boolean := True;
      Name_In_Task_Manager : String := "";
      Queue_Id             : String := "";
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Timeout              : Integer := -1;
      Strip_CR             : Boolean := True;
      Use_Pipes            : Boolean := True)
   is
      Created_Command : Scheduled_Command_Access;
   begin
      Launch_Process
        (Kernel               => Kernel,
         CL                   => CL,
         Server               => Server,
         Console              => Console,
         Callback             => Callback,
         Exit_Cb              => Exit_Cb,
         Success              => Success,
         Use_Ext_Terminal     => Use_Ext_Terminal,
         Show_Command         => Show_Command,
         Show_Output          => Show_Output,
         Callback_Data        => Callback_Data,
         Line_By_Line         => Line_By_Line,
         Directory            => Directory,
         Show_In_Task_Manager => Show_In_Task_Manager,
         Name_In_Task_Manager => Name_In_Task_Manager,
         Queue_Id             => Queue_Id,
         Synchronous          => Synchronous,
         Show_Exit_Status     => Show_Exit_Status,
         Timeout              => Timeout,
         Strip_CR             => Strip_CR,
         Use_Pipes            => Use_Pipes,
         Created_Command      => Created_Command);
   end Launch_Process;

   --------------------
   -- Delete_Handler --
   --------------------

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);

      Console : constant Console_Process := Console_Process (Object);
      Button  : Message_Dialog_Buttons;
   begin
      if Console.Died then
         Cleanup (Console);
         return False;
      end if;

      Button := Message_Dialog
        (-"The process attached to this window" & ASCII.LF
          & (-"is still active, do you want to kill it ?"),
         Confirmation,
         Button_Yes or Button_No,
         Button_Yes);

      if Button = Button_Yes then
         --  The console is about to be destroyed: avoid dangling pointer.
         Console.Console := null;

         if Console.D.Descriptor /= null then
            Close (Console.D.Descriptor.all);
         end if;

         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Delete_Handler;

end GPS.Kernel.Timeout;

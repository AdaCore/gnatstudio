-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007                      --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;                use GNAT.Regpat;
with System;                     use System;

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
      Args                 : String_List_Access := null;
      Server               : Server_Type;
      Console              : Interactive_Console;
      Delete_Id            : Gtk.Handlers.Handler_Id;
      Show_In_Task_Manager : Boolean;
      Strip_CR             : Boolean;
      Use_Pipes            : Boolean;
      Show_Output          : Boolean;
      Show_Command         : Boolean;
      Show_Exit_Status     : Boolean;
      Synchronous          : Boolean;
      Use_Ext_Terminal     : Boolean;
      Directory            : String_Ptr;

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

      Id                   : G_Source_Id;
   end record;
   type Console_Process is access all Console_Process_Data'Class;

   package Console_Process_Timeout is new
     Glib.Main.Generic_Sources (Console_Process);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Callback_Data_Record'Class, Callback_Data_Access);

   function Process_Cb (Data : Console_Process) return Boolean;
   --  Generic callback for async spawn of processes

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" event

   procedure Cleanup (Data : Console_Process);
   --  Close the process descriptor and free its associated memory.
   --  Free memory used by Data itself

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : in String;
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

   procedure Interrupt (Command : in out Monitor_Command);
   --  Interrupts the command

   procedure Free (D : in out Monitor_Command);
   function Execute
     (Command : access Monitor_Command) return Command_Return_Type;
   function Name (Command : access Monitor_Command) return String;
   --  See inherited documentation

   function Execute_Monitor
     (Command : Command_Access) return Command_Return_Type;
   --  Execute the monitor command Command once, then process its current
   --  output and check whether it should be executed again.

   ---------------------
   -- Execute_Monitor --
   ---------------------

   function Execute_Monitor
     (Command : Command_Access) return Command_Return_Type
   is
      C        : constant Monitor_Command_Access :=
                   Monitor_Command_Access (Command);
      Result   : Command_Return_Type;
      Continue : Boolean;
      pragma Unreferenced (Continue);
   begin
      Result := Execute (Command);
      Continue := Process_Cb (C.Data);
      return Result;
   end Execute_Monitor;

   procedure Launch_Monitor_Command_Synchronous is new
     Launch_Synchronous_Generic (Execute_Monitor);

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Command : in out Monitor_Command) is
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

   procedure Free (D : in out Monitor_Command) is
      PID    : GNAT.Expect.Process_Id;
   begin
      if not D.Data.Died and then D.Data.D.Descriptor /= null then
         PID := Get_Pid (D.Data.D.Descriptor.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (D.Data.D.Descriptor.all);
            Close (D.Data.D.Descriptor.all);
         end if;
      end if;

      D.Data.D.Command := null;
      D.Data.Interrupted := True;

      Free (D.Name);
      Cleanup (D.Data);
      Unref (D.Data);
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Monitor_Command) return Command_Return_Type
   is
      Timeout : constant Guint := 50;
      Success : Boolean;
   begin
      if not Command.Data.Started then
         Trace (Me, "Starting the program " & Command.Name.all);

         if Command.Data.Console /= null then
            Trace (Me, "Connect the command_handler to the console");
            Set_Command_Handler (Command.Data.Console, Data_Handler'Access,
                                 Command.Data.all'Address);
            Command.Data.Delete_Id := Object_Return_Callback.Object_Connect
              (Command.Data.Console, Gtk.Widget.Signal_Delete_Event,
               Delete_Handler'Access, GObject (Command.Data));
         end if;

         if Command.Data.Timeout /= -1 then
            Command.Data.Start_Time := Ada.Calendar.Clock;
         end if;

         Trace (Me, "Spawn the process");
         Spawn (Command.Data.D.Kernel,
                Command.Data.Args.all,
                Command.Data.Server,
                Command.Data.D.Descriptor,
                Success,
                Command.Data.Use_Ext_Terminal,
                Command.Data.Console,
                Command.Data.Show_Command,
                Command.Data.Directory.all,
                Command.Data.Use_Pipes);
         Free (Command.Data.Args);
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
            Free (Command.Data.D.Descriptor);
            Command.Data.Died := True;
            return Failure;
         end if;

      elsif Command.Data.D.Descriptor = null
        or else Command.Data.Died
      then
         return Failure;
      else
         return Execute_Again;
      end if;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access Monitor_Command) return String is
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
      if Data.D.Descriptor = null then
         return;
      end if;

      Close (Data.D.Descriptor.all, Status);

      if Data.Console = null then
         Console := Get_Console (Data.D.Kernel);
      end if;

      --  The console might no longer exists if we are exiting GPS
      if Console /= null then
         if Data.Interrupted then
            Insert (Console, -"<^C> process interrupted");
            --  ??? elsif Data.Show_Output or else Data.Show_Command then
         elsif Data.Show_Exit_Status then
            if Status = 0 then
               Insert (Console, -"process terminated successfully");
            else
               Insert
                 (Console, -"process exited with status " & Image (Status));
            end if;
         end if;
      end if;

      if Data.D.Exit_Cb /= null then
         Data.D.Exit_Cb (Data.D, Status);
      end if;

      Free (Data.D.Descriptor);
      Pop_State (Data.D.Kernel);

      if Data.D.Callback_Data /= null then
         Destroy (Data.D.Callback_Data.all);
         Unchecked_Free (Data.D.Callback_Data);
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
               --  Received something. Cancel timeout.
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
                     --  Child
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
                  --  sometimes not enough
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
               Output : constant String := Strip_CR (Expect_Out (Fd.all));
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

         if Data.Delete_Id.Id /= Null_Handler_Id then
            Gtk.Handlers.Disconnect (Data.Console, Data.Delete_Id);
         end if;

         Data.Died := True;
         Unchecked_Free (Data.Expect_Regexp);
         Cleanup (Data);
         Unref (Data);

         return False;

      when E : others =>
         Trace (Exception_Handle, E);
         Unchecked_Free (Data.Expect_Regexp);
         Cleanup (Data);
         Unref (Data);
         return False;
   end Process_Cb;

   ------------------
   -- Data_Handler --
   ------------------

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : in String;
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
         Remove (Process.Id);
         Unchecked_Free (Process.Expect_Regexp);
         Cleanup (Process);
         Unref (Process);
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

   Id : Natural := 0;
   function Get_New_Queue_Id (QId : String) return String;
   --  Returns a new unique queue id

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
                      Blocking       => False,
                      Print_Output   => False,
                      Sync_Once_Dirs => False,
                      Queue_Id       => Q_Id);
      end if;

      C := new Monitor_Command;
      C.Data := new Console_Process_Data;
      Initialize (C.Data);

      No_Handler.Id := Null_Handler_Id;

      if Line_By_Line then
         Expect_Regexp := new Pattern_Matcher'(Compile ("^.*?\n"));
      else
         Expect_Regexp := new Pattern_Matcher'(Compile (".*$", Single_Line));
      end if;

      C.Name := new String'(Command);
      C.Data.Args                 := new String_List'
        ((1 => new String'(Command)) & Clone (Arguments));
      C.Data.Server               := Server;
      C.Data.Console              := Console;
      C.Data.Use_Ext_Terminal     := Use_Ext_Terminal;
      C.Data.Directory            := new String'(Directory);
      C.Data.Delete_Id            := No_Handler;
      C.Data.Show_In_Task_Manager := Show_In_Task_Manager;
      C.Data.Show_Output          := Show_Output;
      C.Data.Show_Command         := Show_Command;
      C.Data.Show_Exit_Status     := Show_Exit_Status;
      C.Data.Strip_CR             := Strip_CR;
      C.Data.Use_Pipes            := Use_Pipes;
      C.Data.Synchronous          := Synchronous;
      C.Data.Expect_Regexp        := Expect_Regexp;
      C.Data.D                    := (Kernel        => Kernel,
                                      Descriptor    => null,
                                      Callback      => Callback,
                                      Exit_Cb       => Exit_Cb,
                                      Callback_Data => Callback_Data,
                                      Command       => Command_Access (C),
                                      Process_Died  => False);
      C.Data.Died                 := False;
      C.Data.Interrupted          := False;
      C.Data.Started              := False;
      C.Data.Id                   := 0;
      C.Data.Timeout              := Timeout;
      Ref (C.Data);

      if Synchronous then
         Launch_Monitor_Command_Synchronous (Command_Access (C), 0.1);
         Destroy (Command_Access (C));

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
                      Blocking       => False,
                      Print_Output   => False,
                      Sync_Once_Dirs => False,
                      Queue_Id       => Q_Id);
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
      Created_Command      : out Scheduled_Command_Access)
   is
   begin
      Launch_Process
        (Kernel               => Kernel,
         Command              => Command,
         Arguments            => Arguments,
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
      Use_Pipes            : Boolean := True)
   is
      Created_Command : Scheduled_Command_Access;
   begin
      Launch_Process
        (Kernel               => Kernel,
         Command              => Command,
         Arguments            => Arguments,
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
         Remove (Console.Id);
         Unref (Console);
         return False;
      end if;

      Button := Message_Dialog
        (-"The process attached to this window" & ASCII.LF
          & (-"is still active, do you want to kill it ?"),
         Confirmation,
         Button_Yes or Button_No,
         Button_Yes);

      if Button = Button_Yes then
         Remove (Console.Id);
         Cleanup (Console);
         Unchecked_Free (Console.Expect_Regexp);
         Unref (Console);
         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Remove (Console.Id);
         Unchecked_Free (Console.Expect_Regexp);
         Cleanup (Console);
         Unref (Console);
         return False;
   end Delete_Handler;

end GPS.Kernel.Timeout;

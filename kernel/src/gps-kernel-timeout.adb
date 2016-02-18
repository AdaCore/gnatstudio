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

with Ada.Calendar;               use Ada, Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;                use GNAT.Regpat;
with System;                     use System;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib.Object;                use Glib.Object;

with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Dialogs;             use Gtkada.Dialogs;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Remote;          use GPS.Kernel.Remote;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with Time_Utils;                 use Time_Utils;

package body GPS.Kernel.Timeout is

   Me : constant Trace_Handle := Create ("Timeout");

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package System_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Interactive_Console_Record, Boolean, System.Address);

   Id : Natural := 0;
   function Get_New_Queue_Id (QId : String) return String;
   --  Returns a new unique queue id

   procedure Run_On_Exit
     (Self     : not null access External_Process_Data'Class;
      External : not null access Root_Command'Class;
      Status   : Integer);
   --  Run the On_Exit callback if not done yet.

   function Delete_Handler
     (Console : access Interactive_Console_Record'Class;
      Data    : System.Address) return Boolean;
   --  Callback for the "delete_event" event

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String;
   --  Handler for user input on the console

   type Monitor_Command is new Root_Command with record
      Name                 : GNAT.Strings.String_Access;
      CL                   : Arg_List;
      Server               : Server_Type;
      Console              : Interactive_Console;

      Delete_Id            : Gtk.Handlers.Handler_Id;
      --  Signals connecting the gtk widget to the underlying process

      Strip_CR             : Boolean;
      Use_Pipes            : Boolean;
      Show_Output          : Boolean;
      Show_Command         : Boolean;
      Show_Exit_Status     : Boolean;
      Use_Ext_Terminal     : Boolean;
      Directory            : Virtual_File;

      Expect_Regexp        : GNAT.Expect.Pattern_Matcher_Access;

      D                    : External_Process_Data_Access;
      Died                 : Boolean := False;
      --  Indicates that the process has died

      Interrupted          : Boolean := False;
      --  Whether the process was interrupted by the user

      Started              : Boolean := False;
      --  Whether the process has been started

      Finished             : Boolean := False;
      --  Whether the process has been died and Exit_Callback called

      Timeout              : Integer;
      --  How many time do we wait for first output

      Start_Time           : Ada.Calendar.Time;
      --  Start time of the process
   end record;
   type Monitor_Command_Access is access all Monitor_Command'Class;
   --  Command that can be used to monitor an external process through the task
   --  manager, and make it interruptible by users. No special handling of
   --  the output is done, since this is assumed to be done through the call
   --  to Launch_Process already. Closing the console terminates the process.

   overriding procedure Interrupt (Command : in out Monitor_Command);
   overriding procedure Primitive_Free (Self : in out Monitor_Command);
   overriding function Execute
     (Command : access Monitor_Command) return Command_Return_Type;
   overriding function Name (Command : access Monitor_Command) return String
     is (Command.Name.all);
   --  See inherited documentation

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (External_Process_Data'Class, External_Process_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Monitor_Command_Access);

   function Process_Cb
     (Command : not null access Monitor_Command'Class) return Boolean;
   --  Generic callback for async spawn of processes

   procedure Cleanup (Command : not null access Monitor_Command'Class);
   --  Close the process descriptor and mark the process as terminated in the
   --  task manager.

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Monitor_Command) is
   begin
      if Command.D.Descriptor /= null then
         Interrupt (Command.D.Descriptor.all);
         Close (Command.D.Descriptor.all);
         Command.Interrupted := True;
      end if;
   end Interrupt;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Self : in out Monitor_Command) is
      PID : GNAT.Expect.Process_Id;
   begin
      if not Self.Died and then Self.D.Descriptor /= null then
         PID := Get_Pid (Self.D.Descriptor.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (Self.D.Descriptor.all);
            Close (Self.D.Descriptor.all);
            Self.Interrupted := True;
         end if;
      end if;

      --  ??? This seems complex behavior while we free the process. Might
      --  be better to expect this to be run from Process_Cb already.
      Cleanup (Self'Unchecked_Access);

      Free (Self.Name);
      Unchecked_Free (Self.Expect_Regexp);

      if Self.D /= null then
         Free (Self.D.all);
         Unchecked_Free (Self.D);
      end if;
   end Primitive_Free;

   -----------------
   -- Run_On_Exit --
   -----------------

   procedure Run_On_Exit
     (Self     : not null access External_Process_Data'Class;
      External : not null access Root_Command'Class;
      Status   : Integer) is
   begin
      if not Self.On_Exit_Run then
         Self.On_Exit_Run := True;
         Self.On_Exit (External => External, Status => Status);
      end if;
   exception
      when E : others =>
         Trace (Me, E);
   end Run_On_Exit;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Monitor_Command) return Command_Return_Type
   is
      Success : Boolean;
   begin
      if not Command.Started then
         Trace (Me, "Starting the program " & Command.Name.all);

         if Command.Console /= null then
            Trace (Me, "Connect the command_handler to the console");
            Set_Command_Handler
              (Command.Console, Data_Handler'Access,
               Command.all'Address);

            Command.Delete_Id := System_Callbacks.Connect
              (Command.Console, Gtk.Widget.Signal_Delete_Event,
               System_Callbacks.To_Marshaller (Delete_Handler'Access),
               Command.all'Address);
         end if;

         Command.Start_Time := Ada.Calendar.Clock;

         Trace (Me, "Spawn the process " & Get_Command (Command.CL));

         Spawn (Kernel_Handle (Command.D.Kernel),
                Command.CL,
                Command.Server,
                Command.D.Descriptor,
                Success,
                Command.Use_Ext_Terminal,
                Command.Console,
                Command.Show_Command,
                Command.Directory,
                Command.Use_Pipes);

         --  Set Started here so that even if spawn fails we don't pass twice
         --  here
         Command.Started := True;

         if Success then
            return Execute_Again;

         else
            Trace (Me, "Failure when spawning the process "
                   & Get_Command (Command.CL));

            --  We could not launch the process: call the Exit_Cb nonetheless,
            --  as it may be used to keep count of executions, or to free
            --  memory, for instance.
            Run_On_Exit (Command.D, Command, -1);

            Free (Command.D.Descriptor);
            Command.Died := True;
            Cleanup (Command);
            return Failure;
         end if;

      elsif Command.Finished then
         Trace (Me, "Process finished: "  & Get_Command (Command.CL));
         return Commands.Success;

      else
         Success := Process_Cb (Command);
         return Execute_Again;
      end if;
   end Execute;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Command : not null access Monitor_Command'Class) is
      procedure Insert (Msg : String);
      procedure Insert (Msg : String) is
      begin
         if Command.Console /= null then
            Command.Console.Insert (Msg);
         else
            Command.D.Kernel.Insert (Msg);
         end if;
      end Insert;

      Status  : Integer := 0;
   begin
      if Command.D.Descriptor /= null then
         Close (Command.D.Descriptor.all, Status);
         if Command.Interrupted then
            Status := -1;
         end if;

         --  So that next call to Cleanup does nothing
         Free (Command.D.Descriptor);
      end if;

      declare
         End_Time      : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Time_Stamp    : constant String := Timestamp (End_Time);
      begin
         if Command.Interrupted then
            Insert (Time_Stamp &
                    (-"<^C> process interrupted (elapsed time: ")
                    & Elapsed (Command.Start_Time, End_Time) & "s)");
            --  ??? elsif Data.Show_Output or else Data.Show_Command then
         elsif Command.Show_Exit_Status then
            if Status = 0 then
               Insert (Time_Stamp &
                       (-"process terminated successfully (elapsed time: ")
                       & Elapsed (Command.Start_Time, End_Time) & "s)");
            else
               Insert (Time_Stamp
                       & (-"process exited with status ")
                       & Image (Status) & " (elapsed time: "
                       & Elapsed (Command.Start_Time, End_Time) & "s)");
            end if;
         end if;
      end;

      Run_On_Exit (Command.D, Command, Status);
      Command.Finished := True;
   end Cleanup;

   ----------------
   -- Process_Cb --
   ----------------

   function Process_Cb
     (Command : not null access Monitor_Command'Class) return Boolean
   is
      Fd     : Process_Descriptor_Access;
      Result : Expect_Match;

      function Conditional_Strip_CR (S : String) return String;
      --  Return a stripped from CR string if Data tells so

      --------------------------
      -- Conditional_Strip_CR --
      --------------------------

      function Conditional_Strip_CR (S : String) return String is
      begin
         if Command.Strip_CR then
            return Strip_CR (S);
         else
            return S;
         end if;
      end Conditional_Strip_CR;

   begin
      if Command = null or else Command.Died then
         return False;
      end if;

      Fd := Command.D.Descriptor;
      if Fd /= null then
         loop
            Expect (Fd.all, Result, Command.Expect_Regexp.all, Timeout => 1);

            if Result /= Expect_Timeout then
               --  Received something. Cancel timeout
               Command.Timeout := -1;

               declare
                  Output : constant String :=
                             Conditional_Strip_CR (Expect_Out (Fd.all));
                  Child  : MDI_Child;
               begin
                  if Command.Console /= null
                    and then Command.Show_Output
                  then
                     Insert (Command.Console, Output, Add_LF => False);

                     --  ??? This might be costly, we could cache this MDI
                     --  Child.
                     Child := Find_MDI_Child
                       (Get_MDI (Command.D.Kernel), Command.Console);

                     if Child /= null then
                        Highlight_Child (Child);
                     end if;
                  end if;

                  Command.D.On_Output (Command, Output);
               end;

            else
               if Command.Timeout /= -1
                 and then Ada.Calendar.Clock - Command.Start_Time >
                   Duration (Command.Timeout) /  1000.0
               then
                  --  Make sure the process is killed. Just interrupting it is
                  --  sometimes not enough.
                  Close (Fd.all);
               end if;

               exit;
            end if;
         end loop;
      else
         raise GNAT.Expect.Process_Died;
      end if;

      return True;

   exception
      when GNAT.Expect.Process_Died =>
         if Fd /= null then
            declare
               Output : constant String :=
                          Conditional_Strip_CR (Expect_Out (Fd.all));
            begin
               Command.D.Process_Died := True;
               Command.D.On_Output (Command, Output);

               if Command.Console /= null then
                  --  Display all remaining output

                  Insert (Command.Console, Output, Add_LF => False);
               end if;
            end;
         end if;

         if Command.Console /= null then
            Enable_Prompt_Display (Command.Console, False);
            Gtk.Handlers.Disconnect (Command.Console, Command.Delete_Id);
            Command.Console.Set_Command_Handler (null, System.Null_Address);
         end if;

         Command.Died := True;
         Cleanup (Command);
         return False;

      when E : others =>
         Trace (Me, E);
         Cleanup (Command);
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
      Process : constant Monitor_Command_Access := Convert (User_Data);
   begin
      if not Process.Died then
         --  ??? If Process.D.Descriptor is null then Process.Died should be
         --  True. This is being investigated under EC06-004.
         Send (Process.D.Descriptor.all, Input);
      end if;

      return "";

   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end Data_Handler;

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
      Block_Exit           : Boolean := True)
   is
      Q_Id          : constant String := Get_New_Queue_Id (Queue_Id);
      C             : Monitor_Command_Access;
      Wrapper       : Scheduled_Command_Access;
   begin
      if not Is_Local (Server) then
         Synchronize (Kernel_Handle (Kernel), GPS_Server, Server,
                      Blocking      => False,
                      Print_Command => True,
                      Print_Output  => False,
                      Force         => False,
                      Queue_Id      => Q_Id);
      end if;

      C := new Monitor_Command'
        (Root_Command with
         Name                 => null,
         CL                   => CL,
         Server               => Server,
         Console              => Console,
         Use_Ext_Terminal     => Use_Ext_Terminal,
         Directory            => Directory,
         Delete_Id            => (Id => Null_Handler_Id, Closure => null),
         Show_Output          => Show_Output,
         Show_Command         => Show_Command,
         Show_Exit_Status     => Show_Exit_Status,
         Strip_CR             => Strip_CR,
         Use_Pipes            => Use_Pipes,
         Expect_Regexp        => null,
         D                    => null,
         Died                 => False,
         Interrupted          => False,
         Started              => False,
         Finished             => False,
         Start_Time           =>
           Time_Of (Year_Number'First, Month_Number'First, Day_Number'First),
         Timeout              => Timeout);

      if Line_By_Line then
         C.Expect_Regexp := new Pattern_Matcher'(Compile ("^.*?\n"));
      else
         C.Expect_Regexp := new Pattern_Matcher'(Compile (".*$", Single_Line));
      end if;

      if Name_In_Task_Manager /= "" then
         C.Name := new String'(Name_In_Task_Manager);
      else
         C.Name := new String'(Get_Command (CL));
      end if;

      if Data = null then
         C.D := new External_Process_Data;
      else
         C.D := External_Process_Data_Access (Data);
      end if;

      C.D.Kernel := Kernel;

      Wrapper := Create_Wrapper (Command => C, Destroy_On_Exit => True);

      if Synchronous then
         Launch_Synchronous (Command_Access (Wrapper), 0.1);
         Unref (Command_Access (Wrapper));
         Scheduled := null;

      else
         --   ??? A scheduled command that wraps a scheduled command
         Scheduled := Launch_Background_Command
           (Kernel,
            Command_Access (Wrapper),
            Active     => False,
            Show_Bar   => Show_In_Task_Manager,
            Queue_Id   => Q_Id,
            Block_Exit => Block_Exit);
      end if;

      if not Is_Local (Server) then
         Synchronize (Kernel_Handle (Kernel), Server, GPS_Server,
                      Blocking      => False,
                      Print_Command => True,
                      Print_Output  => False,
                      Force         => False,
                      Queue_Id      => Q_Id);
      end if;

      Success := True;

   exception
      when E : others =>
         Trace (Me, E);
         Success := False;
         Scheduled := null;
   end Launch_Process;

   --------------------
   -- Delete_Handler --
   --------------------

   function Delete_Handler
     (Console : access Interactive_Console_Record'Class;
      Data    : System.Address) return Boolean
   is
      pragma Unreferenced (Console);
      Self : constant Monitor_Command_Access := Convert (Data);
      Button  : Message_Dialog_Buttons;
   begin
      if Self.Died then
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
         Self.Console := null;

         if Self.D.Descriptor /= null then
            Close (Self.D.Descriptor.all);
         end if;

         return False;
      end if;

      return True;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Delete_Handler;

end GPS.Kernel.Timeout;

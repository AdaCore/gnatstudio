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

with Ada.Calendar;               use Ada, Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
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
with String_Utils;               use String_Utils;
with Time_Utils;                 use Time_Utils;

package body GPS.Kernel.Timeout is

   Me : constant Trace_Handle := Create ("Timeout");
   Me_Expect : constant Trace_Handle := Create ("EXPECT", Off);

   package System_Callbacks is new Gtk.Handlers.User_Return_Callback
     (Interactive_Console_Record, Boolean, System.Address);

   Id : Natural := 0;
   function Get_New_Queue_Id (QId : String) return String;
   --  Returns a new unique queue id (or use Qid if specified)

   procedure Run_On_Exit
     (Self : not null access External_Process_Data'Class);
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

      Delete_Id            : Gtk.Handlers.Handler_Id;
      --  Signals connecting the gtk widget to the underlying process

      Use_Pipes            : Boolean;
      Show_Command         : Boolean;
      Show_Exit_Status     : Boolean;
      Use_Ext_Terminal     : Boolean;
      Directory            : Virtual_File;

      Expect_Regexp        : GNAT.Expect.Pattern_Matcher_Access;

      D                    : External_Process_Data_Access;

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

   procedure Remove_Progress_Info
     (Self   : not null access Monitor_Command'Class;
      Input  : String;
      Output : out Unbounded_String);
   --  Parse and remove progress information from Input.
   --  Returns the remaining string in Output.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (External_Process_Data'Class, External_Process_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Monitor_Command_Access);

   procedure Cleanup (Command : not null access Monitor_Command'Class);
   --  Close the process descriptor and mark the process as terminated in the
   --  task manager.

   procedure Spawn_External_Process
     (Self : not null access External_Process_Data'Class);
   --  Spawn the external process if not done yet.

   procedure Get_And_Process_Output
     (Self : not null access External_Process_Data'Class;
      Str  : out Unbounded_String);
   --  Retrieve the output of the external process since the last call to
   --  On_Output. This output is cleaned up to detect progress regexp, remove
   --  CR if needed, and On_Output is called as appropriate.

   procedure In_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   procedure Out_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   procedure Died_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   --  Various filters to trace the external process executing when traces are
   --  activated

   ---------------------
   -- In_Trace_Filter --
   ---------------------

   procedure In_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
      Start : Integer := Str'First;
   begin
      for S in Str'Range loop
         if not Ada.Characters.Handling.Is_Graphic (Str (S)) then
            if S /= Start then
               Trace (Me_Expect, "Sending: " & Str (Start .. S - 1));
            end if;
            Trace (Me_Expect, "Sending< ASCII." & Character'Image (Str (S)));
            Start := S + 1;
         end if;
      end loop;
      if Str'Last + 1 /= Start then
         Trace (Me_Expect, "Sending: " & Str (Start .. Str'Last));
      end if;
   end In_Trace_Filter;

   ----------------------
   -- Out_Trace_Filter --
   ----------------------

   procedure Out_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
      Last : Integer := Str'First;
   begin
      for S in Str'Range loop
         if Str (S) < ' ' then
            if Last < S then
               Trace (Me_Expect, "Receiving: " & Str (Last .. S - 1));
            end if;
            Trace (Me_Expect, "Receiving< ASCII." & Character'Image (Str (S)));
            Last := S + 1;
         end if;
      end loop;

      if Last <= Str'Last then
         Trace (Me_Expect, "Receiving: " & Str (Last .. Str'Last));
      end if;
   end Out_Trace_Filter;

   -----------------------
   -- Died_Trace_Filter --
   -----------------------

   procedure Died_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
   begin
      Trace (Me_Expect, "Died: " & Str);
   end Died_Trace_Filter;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Monitor_Command) is
   begin
      if Command.D.Descriptor /= null then
         Command.D.On_Before_Kill (Command'Access);
         Interrupt (Command.D.Descriptor.all);
         Close (Command.D.Descriptor.all, Command.D.Exit_Status);
         Command.Interrupted := True;
      end if;
   end Interrupt;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Self : in out Monitor_Command) is
      PID : GNAT.Expect.Process_Id;
   begin
      if not Self.D.Process_Died and then Self.D.Descriptor /= null then
         PID := Get_Pid (Self.D.Descriptor.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (Self.D.Descriptor.all);
            Close (Self.D.Descriptor.all, Self.D.Exit_Status);
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
     (Self : not null access External_Process_Data'Class) is
   begin
      if not Self.On_Exit_Run then
         Self.On_Exit_Run := True;
         Self.On_Exit (External => Self.Command);
      end if;
   exception
      when E : others =>
         Trace (Me, E);
   end Run_On_Exit;

   ----------------------------
   -- Spawn_External_Process --
   ----------------------------

   procedure Spawn_External_Process
     (Self : not null access External_Process_Data'Class)
   is
      Monitor : constant Monitor_Command_Access :=
        Monitor_Command_Access (Self.Command);
      Success : Boolean;
   begin
      if not Monitor.Started then
         Trace (Me, "Starting the program " & Monitor.Name.all);

         if Self.Console /= null then
            Trace (Me, "Connect the command_handler to the console");
            Set_Command_Handler
              (Self.Console, Data_Handler'Access,
               Monitor.all'Address);

            Monitor.Delete_Id := System_Callbacks.Connect
              (Self.Console, Gtk.Widget.Signal_Delete_Event,
               System_Callbacks.To_Marshaller (Delete_Handler'Access),
               Monitor.all'Address);
         end if;

         Monitor.Start_Time := Ada.Calendar.Clock;

         Trace (Me, "Spawn the process " & Get_Command (Monitor.CL));

         Spawn (Kernel           => Kernel_Handle (Self.Kernel),
                Arguments        => Monitor.CL,
                Server           => Monitor.Server,
                Pd               => Self.Descriptor,
                Success          => Success,
                Use_Ext_Terminal => Monitor.Use_Ext_Terminal,
                Console          => Self.Console,
                Show_Command     => Monitor.Show_Command,
                Directory        => Monitor.Directory,
                Use_Pipes        => Monitor.Use_Pipes);

         if GNATCOLL.Traces.Active (Me_Expect) then
            Add_Filter
              (Self.Descriptor.all,
               Filter    => In_Trace_Filter'Access,
               Filter_On => Input);
            Add_Filter
              (Self.Descriptor.all,
               Filter    => Out_Trace_Filter'Access,
               Filter_On => Output);
            Add_Filter
              (Self.Descriptor.all,
               Filter    => Died_Trace_Filter'Access,
               Filter_On => Died);
         end if;

         --  Set Started here so that even if spawn fails we don't pass twice
         --  here
         Monitor.Started := True;

         if not Success then
            Trace (Me, "Failure when spawning the process "
                   & To_Display_String (Monitor.CL));

            --  We could not launch the process: call the Exit_Cb nonetheless,
            --  as it may be used to keep count of executions, or to free
            --  memory, for instance.
            Self.Exit_Status := -1;

            Unchecked_Free (Self.Descriptor);
            Cleanup (Monitor);
         end if;
      end if;
   end Spawn_External_Process;

   ------------------------
   -- Suspend_Monitoring --
   ------------------------

   procedure Suspend_Monitoring
     (Self    : not null access External_Process_Data'Class;
      Suspend : Boolean)
   is
   begin
      Self.Monitoring_Stopped := Suspend;
   end Suspend_Monitoring;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Monitor_Command) return Command_Return_Type
   is
   begin
      if Command.D.Monitoring_Stopped then
         return Execute_Again;

      elsif not Command.Started then
         Spawn_External_Process (Command.D);
         if Command.D.Process_Died then
            return Failure;
         else
            return Execute_Again;
         end if;

      elsif Command.Finished then
         Trace (Me, "Process finished: "  & To_Display_String (Command.CL));
         return Commands.Success;

      else
         declare
            Str    : Unbounded_String;
            Status : Expect_Status;
         begin
            Status := Command.D.Expect
              (Regexp  => Command.Expect_Regexp.all,
               Timeout => 1,
               Output  => Str,
               Stop_At_First_Match => False);

            if Status = Died then
               Trace
                 (Me,
                  "Process died: "  & To_Display_String (Command.CL)
                  & " (exit status:" & Integer'Image (Command.D.Exit_Status)
                  & ")");

               return Commands.Success;

            else
               return Execute_Again;
            end if;
         end;
      end if;
   end Execute;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Command : not null access Monitor_Command'Class) is
      procedure Insert (Msg : String);
      procedure Insert (Msg : String) is
      begin
         if Command.D.Console /= null then
            Command.D.Console.Insert (Msg);
         else
            Command.D.Kernel.Insert (Msg);
         end if;
      end Insert;

   begin
      if Command.D.Descriptor /= null then
         Close (Command.D.Descriptor.all, Command.D.Exit_Status);
         if Command.Interrupted then
            Command.D.Exit_Status := -1;
         end if;

         --  So that next call to Cleanup does nothing
         Unchecked_Free (Command.D.Descriptor);
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
            if Command.D.Exit_Status = 0 then
               Insert (Time_Stamp &
                       (-"process terminated successfully (elapsed time: ")
                       & Elapsed (Command.Start_Time, End_Time) & "s)");
            else
               Insert (Time_Stamp
                       & (-"process exited with status ")
                       & Image (Command.D.Exit_Status) & " (elapsed time: "
                       & Elapsed (Command.Start_Time, End_Time) & "s)");
            end if;
         end if;
      end;

      Run_On_Exit (Command.D);
      Command.Finished := True;
      Command.D.Process_Died := True;
   end Cleanup;

   ----------------------------
   -- Get_And_Process_Output --
   ----------------------------

   procedure Get_And_Process_Output
     (Self : not null access External_Process_Data'Class;
      Str  : out Unbounded_String)
   is
      Child : MDI_Child;
      Monitor : constant Monitor_Command_Access :=
        Monitor_Command_Access (Self.Command);
   begin
      if Self.Descriptor = null then
         Str := Null_Unbounded_String;
      else
         Remove_Progress_Info
           (Monitor,
            (if Self.Strip_CR
             then Strip_CR (Expect_Out (Self.Descriptor.all))
             else Expect_Out (Self.Descriptor.all)),
            Str);

         if Self.Console /= null
           and then Self.Show_Output
         then
            Insert (Self.Console, To_String (Str), Add_LF => False);

            --  ??? This might be costly, we could cache this MDI
            --  Child.
            Child := Find_MDI_Child
              (Get_MDI (Self.Kernel), Self.Console);

            if Child /= null then
               Child.Highlight_Child;
            end if;
         end if;

         Self.On_Output (Monitor, To_String (Str));
      end if;
   end Get_And_Process_Output;

   ------------
   -- Expect --
   ------------

   function Expect
     (Self    : not null access External_Process_Data'Class;
      Regexp  : GNAT.Regpat.Pattern_Matcher;
      Timeout : Integer;
      Output  : out Ada.Strings.Unbounded.Unbounded_String;
      Stop_At_First_Match : Boolean := True)
      return Expect_Status
   is
      Monitor  : constant Monitor_Command_Access :=
        Monitor_Command_Access (Self.Command);
      Result   : Expect_Match;
      Str      : Unbounded_String;
      Status   : Expect_Status := Timed_Out;
   begin
      Output := Null_Unbounded_String;

      if Self.Command = null
        or else Self.Process_Died
      then
         return Died;
      end if;

      if not Monitor.Started then
         Spawn_External_Process (Self);
      end if;

      if Self.Descriptor = null then
         return Died;
      end if;

      --  Process all the buffered output of the process (and exit as soon
      --  as that buffer is empty).
      loop
         Expect (Self.Descriptor.all, Result, Regexp, Timeout => Timeout);

         if Result /= Expect_Timeout then
            --  Received something. Cancel global timeout
            Monitor.Timeout := -1;
            Get_And_Process_Output (Self, Str);
            Append (Output, Str);
            Status  := Matched;

            exit when Stop_At_First_Match;

         else
            --  Got a timeout: nothing available in the buffer.
            --  If we have been waiting too long already (more than the
            --  global timeout), we simply close the process and give up.

            if Monitor.Timeout /= -1
              and then Ada.Calendar.Clock - Monitor.Start_Time >
                Duration (Monitor.Timeout) /  1000.0
            then
               --  Make sure the process is killed. Just interrupting it is
               --  sometimes not enough.
               Close (Self.Descriptor.all, Self.Exit_Status);
            end if;

            --  In any case, give up waiting for now
            exit;
         end if;
      end loop;

      return Status;

   exception
      when GNAT.Expect.Process_Died =>
         Self.Process_Died := True;
         Get_And_Process_Output (Self, Str);
         Append (Output, Str);

         if Self.Console /= null then
            Enable_Prompt_Display (Self.Console, False);
            Gtk.Handlers.Disconnect (Self.Console, Monitor.Delete_Id);
            Self.Console.Set_Command_Handler (null, System.Null_Address);
         end if;

         Cleanup (Monitor);
         return Died;

      when E : others =>
         Trace (Me, E);
         Cleanup (Monitor);
         return Died;
   end Expect;

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
      if not Process.D.Process_Died then
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
      Block_Exit           : Boolean := True;
      Start_Immediately    : Boolean := False;
      Active               : Boolean := False)
   is
      Q_Id          : constant String := Get_New_Queue_Id (Queue_Id);
      C             : Monitor_Command_Access;
      Wrapper       : Scheduled_Command_Access;
   begin
      if GNATCOLL.Traces.Active (Me) then
         Trace (Me, "Launch_Process " & To_Display_String (CL)
                & " synchronous=" & Synchronous'Img);
      end if;

      if not Is_Local (Server) then
         Synchronize (Kernel_Handle (Kernel), GPS_Server, Server,
                      Blocking      => False,
                      Print_Command => Show_Command,
                      Print_Output  => False,
                      Force         => False,
                      Queue_Id      => Q_Id);
      end if;

      C := new Monitor_Command'
        (Root_Command with
         Name                 => null,
         CL                   => CL,
         Server               => Server,
         Use_Ext_Terminal     => Use_Ext_Terminal,
         Directory            => Directory,
         Delete_Id            => (Id => Null_Handler_Id, Closure => null),
         Show_Command         => Show_Command,
         Show_Exit_Status     => Show_Exit_Status,
         Use_Pipes            => Use_Pipes,
         Expect_Regexp        =>
           (if Line_By_Line
            then new Pattern_Matcher'(Compile ("^.*?\n"))
            else new Pattern_Matcher'(Compile (".*$", Single_Line))),
         D                    => null,
         Interrupted          => False,
         Started              => False,
         Finished             => False,
         Start_Time           =>
           Time_Of (Year_Number'First, Month_Number'First, Day_Number'First),
         Timeout              => Timeout);

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

      C.D.Kernel      := Kernel;
      C.D.Command     := Command_Access (C);
      C.D.Console     := Console;
      C.D.Show_Output := Show_Output;
      C.D.Strip_CR    := Strip_CR;

      Wrapper := Create_Wrapper (Command => C);

      if Synchronous then
         Launch_Synchronous (Command_Access (Wrapper), 0.1);
         Unref (Command_Access (Wrapper));
         Scheduled := null;
      else
         --   ??? A scheduled command that wraps a scheduled command
         Scheduled := Launch_Background_Command
           (Kernel,
            Command_Access (Wrapper),
            Active            => Active,
            Start_Immediately => Start_Immediately,
            Show_Bar          => Show_In_Task_Manager,
            Queue_Id          => Q_Id,
            Block_Exit        => Block_Exit);
      end if;

      if not Is_Local (Server) then
         Synchronize (Kernel_Handle (Kernel), Server, GPS_Server,
                      Blocking      => False,
                      Print_Command => Show_Command,
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
      Self : constant Monitor_Command_Access := Convert (Data);
      Button  : Message_Dialog_Buttons;
   begin
      if Self.D = null or else Self.D.Process_Died then
         return False;
      end if;

      Button := Message_Dialog
        (-"The process attached to this window" & ASCII.LF
          & (-"is still active, do you want to kill it ?"),
         Confirmation,
         Button_Yes or Button_No,
         Button_Yes,
         Parent => Console.Kernel.Get_Main_Window);

      if Button = Button_Yes then
         --  The console is about to be destroyed: avoid dangling pointer.
         Self.D.Console := null;

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

   -------------------------
   -- Set_Progress_Regexp --
   -------------------------

   procedure Set_Progress_Regexp
     (Self              : in out External_Process_Data;
      Regexp            : String;
      Group_For_Current : Natural := 1;
      Group_For_Total   : Natural := 2) is
   begin
      Unchecked_Free (Self.Progress.Regexp);

      if Regexp /= "" then
         Self.Progress.Regexp :=
           new Pattern_Matcher'(Compile (Regexp, Multiple_Lines));
         Self.Progress.Current := Group_For_Current;
         Self.Progress.Final   := Group_For_Total;
      end if;
   end Set_Progress_Regexp;

   --------------------------
   -- Remove_Progress_Info --
   --------------------------

   procedure Remove_Progress_Info
     (Self   : not null access Monitor_Command'Class;
      Input  : String;
      Output : out Unbounded_String)
   is
      Matches : Match_Array (0 .. Max_Paren_Count);
      Index   : Natural := Input'First;
      Current, Final : Natural;
   begin
      if Self.D.Progress.Regexp = null then
         Output := To_Unbounded_String (Input);
         return;
      end if;

      Output := Null_Unbounded_String;

      while Index <= Input'Last loop
         Match
           (Self.D.Progress.Regexp.all,
            Input,
            Data_First => Index,
            Matches    => Matches);

         if Matches (0) = No_Match then
            Append (Output, Input (Index .. Input'Last));
            exit;
         end if;

         Append (Output, Input (Index .. Matches (0).First - 1));

         Current := Safe_Value
           (Input (Matches (Self.D.Progress.Current).First ..
                   Matches (Self.D.Progress.Current).Last));
         Final := Safe_Value
           (Input (Matches (Self.D.Progress.Final).First ..
                   Matches (Self.D.Progress.Final).Last));

         Set_Progress
           (Self,
            Progress_Record'
              (Activity => Running,
               Current  => Current,
               Total    => Final));

         --  Avoid infinite loop when matching empty strings
         Index := Natural'Max (Index + 1, Matches (0).Last + 1);
      end loop;
   end Remove_Progress_Info;

end GPS.Kernel.Timeout;

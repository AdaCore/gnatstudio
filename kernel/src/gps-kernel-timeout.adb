-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with System;                     use System;

with Glib.Object;                use Glib.Object;
with Glib.Values;

with Gtk.Main;                   use Gtk.Main;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Dialogs;             use Gtkada.Dialogs;

with Commands;                   use Commands;
with Config;                     use Config;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel;                 use GPS.Kernel;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;

package body GPS.Kernel.Timeout is
   Me : constant Debug_Handle := Create ("Timeout");

   type Console_Process_Data is new GObject_Record with record
      Console          : Interactive_Console;
      Delete_Id        : Gtk.Handlers.Handler_Id;
      Show_Output      : Boolean;
      Show_Command     : Boolean;
      Show_Exit_Status : Boolean;

      Expect_Regexp    : GNAT.Expect.Pattern_Matcher_Access;

      D                : Process_Data;
      Died             : Boolean := False;
      --  Indicates that the process has died.

      Interrupted      : Boolean := False;
      --  Whether the process was interrupted by the user

      Id               : Timeout_Handler_Id;
   end record;
   type Console_Process is access all Console_Process_Data'Class;

   package Console_Process_Timeout is new Gtk.Main.Timeout (Console_Process);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Callback_Data_Record'Class, Callback_Data_Access);

   function Process_Cb (Data : Console_Process) return Boolean;
   --  Generic callback for async spawn of processes.

   function Delete_Handler
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Callback for the "delete_event" event.

   procedure Cleanup (Data : Console_Process);
   --  Close the process descriptor and free its associated memory.
   --  Free memory used by Data itself

   function Data_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : in String;
      User_Data : System.Address) return String;
   --  Handler for user input on the console.

   type Monitor_Command is new Root_Command with record
      Name    : String_Access;
      Data    : Console_Process;
   end record;
   type Monitor_Command_Access is access all Monitor_Command'Class;
   --  Command that can be used to monitor an external process through the task
   --  manager, and make it interruptible by users. No special handling of
   --  the output is done, since this is assumed to be done through the call
   --  to Launch_Process already. Closing the console terminates the process.

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
      C      : constant Monitor_Command_Access :=
        Monitor_Command_Access (Command);
      Result : Command_Return_Type;
      Continue : Boolean;
      pragma Unreferenced (Continue);
   begin
      Result := Execute (Command);
      Continue := Process_Cb (C.Data);
      return Result;
   end Execute_Monitor;

   procedure Launch_Monitor_Command_Synchronous is new
     Launch_Synchronous_Generic (Execute_Monitor);

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Monitor_Command) is
      PID     : GNAT.Expect.Process_Id;
      Status  : Integer;
   begin
      if not D.Data.Died and then D.Data.D.Descriptor /= null then
         PID := Get_Pid (D.Data.D.Descriptor.all);

         if PID /= Null_Pid and then PID /= GNAT.Expect.Invalid_Pid then
            Interrupt (D.Data.D.Descriptor.all);
            Close (D.Data.D.Descriptor.all, Status);
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
     (Command : access Monitor_Command) return Command_Return_Type is
   begin
      if Command.Data.D.Descriptor = null
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
      Status : Integer;
      Console : Interactive_Console := Data.Console;
   begin
      if Data.D.Descriptor = null then
         return;
      end if;

      Close (Data.D.Descriptor.all, Status);

      if Data.Console = null then
         Console := Get_Console (Data.D.Kernel);
      end if;

      if Data.Interrupted then
         Insert (Console, -"<^C> process interrupted");
      --  ??? elsif Data.Show_Output or else Data.Show_Command then
      elsif Data.Show_Exit_Status then
         if Status = 0 then
            Insert (Console, -"process terminated successfully");
         else
            Insert (Console, -"process exited with status " & Image (Status));
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

   begin
      if Data = null or else Data.Died then
         return False;
      end if;

      Fd := Data.D.Descriptor;
      if Fd /= null then
         Expect (Fd.all, Result, Data.Expect_Regexp.all, Timeout => 1);

         if Result /= Expect_Timeout then
            declare
               Output : constant String := Strip_CR (Expect_Out (Fd.all));
            begin
               if Data.Console /= null then
                  if Data.Show_Output then
                     Insert (Data.Console, Output, Add_LF => False);

                     --  ??? This might be costly, we could cache this MDI
                     --  Child
                     Highlight_Child
                       (Find_MDI_Child
                          (Get_MDI (Data.D.Kernel), Data.Console));
                  end if;
               end if;

               if Data.D.Callback /= null then
                  Data.D.Callback (Data.D, Output);
               end if;
            end;
         end if;
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

         if Data.Delete_Id.Signal /= Null_Signal_Id then
            Gtk.Handlers.Disconnect (Data.Console, Data.Delete_Id);
         end if;

         Data.Died := True;
         Unchecked_Free (Data.Expect_Regexp);
         Cleanup (Data);
         Unref (Data);

         return False;

      when E : others =>
         Unchecked_Free (Data.Expect_Regexp);
         Cleanup (Data);
         Unref (Data);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
         Timeout_Remove (Process.Id);
         Unchecked_Free (Process.Expect_Regexp);
         Cleanup (Process);
         Unref (Process);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : String := "";
      Remote_Host          : String := "";
      Remote_Protocol      : String := "";
      Show_In_Task_Manager : Boolean := False;
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False;
      Fd                   : out GNAT.Expect.Process_Descriptor_Access)
   is
      Timeout : constant Guint32 := 50;
      Data    : Console_Process;
      C       : Monitor_Command_Access;

      procedure Spawn
        (Command         : String;
         Arguments       : Argument_List;
         Success         : out Boolean;
         Remote_Host     : String := "";
         Remote_Protocol : String := "");
      --  Launch given command.

      -----------
      -- Spawn --
      -----------

      procedure Spawn
        (Command         : String;
         Arguments       : Argument_List;
         Success         : out Boolean;
         Remote_Host     : String := "";
         Remote_Protocol : String := "")
      is
         Exec         : String_Access;
         Old_Dir      : String_Access;
         Args         : Argument_List_Access;
         Remote_Args  : Argument_List_Access;
      begin
         --  Take into account possible switches specified in Remote_Host
         --  and Remote_Protocol.

         if Remote_Host /= "" and then Remote_Protocol /= "" then
            Remote_Args := Argument_String_To_List
              (Remote_Protocol & ' ' & Remote_Host);
         end if;

         --  Find the command to execute on the local host

         if Remote_Protocol = "" or else Remote_Host = "" then
            if Host = Windows then
               --  Execute  "cmd /c command arg1 arg2"
               Exec := new String'("cmd");
               Args := new Argument_List'
                 ((new String'("/c"), new String'(Command))
                  & Clone (Arguments));

            else
               --  Execute "command arg1 arg2"
               Exec := Locate_Exec_On_Path (Command);
               if Exec = null then
                  Success := False;
                  GPS.Kernel.Console.Insert
                    (Kernel, -"Executable not found on PATH: " & Command,
                     Mode => Error);
                  return;
               end if;

               Args := new Argument_List'(Clone (Arguments));
            end if;
         else
            if Host = Windows then
               --  Execute "cmd /c protocol host command arg1 arg2"
               Exec := new String'("cmd");
               Args := new Argument_List'
                 ((1 => new String'("/c"))
                  & Remote_Args.all & new String'(Command)
                  & Clone (Arguments));
            else
               --  Execute "protocol host command arg1 arg2"
               Exec := Remote_Args (Remote_Args'First);
               Args := new Argument_List'
                 (Remote_Args (Remote_Args'First + 1 .. Remote_Args'Last)
                  & new String'(Command) & Clone (Arguments));
            end if;
         end if;

         if Data.Console /= null and then Show_Command then
            Insert (Data.Console,
                    Command & ' ' & Argument_List_To_String (Arguments),
                    Add_LF => True);
         end if;

         Fd := new TTY_Process_Descriptor;

         if Directory /= "" then
            Old_Dir := new String'(Get_Current_Dir);
            Change_Dir (Directory);
         end if;

         if Active (Me) then
            Trace (Me, "Spawning " & Exec.all
                   & ' ' & Argument_List_To_String (Args.all));
         end if;

         Non_Blocking_Spawn (Fd.all, Exec.all, Args.all, Err_To_Out => True);

         if Directory /= "" then
            Change_Dir (Old_Dir.all);
            Free (Old_Dir);
         end if;

         Success := True;

         Free (Exec);
         Free (Args);

      exception
         when Invalid_Process =>
            Success := False;
            GPS.Kernel.Console.Insert
              (Kernel, -"Invalid command", Mode => Error);
      end Spawn;

   begin
      Push_State (Kernel, Processing);

      --  Data is freed in Process_Cb
      Data := new Console_Process_Data;
      Initialize (Data);

      if Console /= null then
         Set_Command_Handler (Console, Data_Handler'Access, Data.all'Address);
         Data.Delete_Id := Object_Return_Callback.Object_Connect
           (Console, "delete_event",
            Delete_Handler'Access,
            GObject (Data),
            After => False);
      else
         Data.Delete_Id.Signal := Null_Signal_Id;
      end if;

      Data.Console := Console;
      Data.Show_Output := Show_Output;
      Data.Show_Command := Show_Command;
      Data.Show_Exit_Status := Show_Exit_Status;

      Spawn (Command, Arguments, Success, Remote_Host, Remote_Protocol);

      if Success then
         --  Precompile the regular expression for more efficiency
         if Line_By_Line then
            Data.Expect_Regexp := new Pattern_Matcher'(Compile ("^.*?\n"));
         else
            Data.Expect_Regexp := new Pattern_Matcher'
              (Compile (".*$", Single_Line));
         end if;

         Data.D := (Kernel, Fd, Callback, Exit_Cb, Callback_Data, null, False);
         if not Synchronous then
            Data.Id :=
              Console_Process_Timeout.Add (Timeout, Process_Cb'Access, Data);
         end if;

         if Show_In_Task_Manager then
            C      := new Monitor_Command;
            C.Data := Data;
            Ref (Data);
            C.Name := new String'(Command);

            Data.D.Command := Command_Access (C);

            if Synchronous then
               Launch_Monitor_Command_Synchronous (Command_Access (C), 0.1);
               Destroy (Command_Access (C));
            else
               Launch_Background_Command
                 (Kernel,
                  Command_Access (C),
                  Active   => False,
                  Show_Bar => True,
                  Queue_Id => "");
            end if;
         end if;

      else
         Unref (Data);
         Pop_State (Kernel);
         Fd := null;
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      Show_Command         : Boolean := True;
      Show_Output          : Boolean := True;
      Callback_Data        : Callback_Data_Access := null;
      Line_By_Line         : Boolean := False;
      Directory            : String := "";
      Remote_Host          : String := "";
      Remote_Protocol      : String := "";
      Show_In_Task_Manager : Boolean := False;
      Synchronous          : Boolean := False;
      Show_Exit_Status     : Boolean := False)
   is
      Fd : Process_Descriptor_Access;
   begin
      Launch_Process
        (Kernel, Command, Arguments, Console, Callback, Exit_Cb, Success,
         Show_Command, Show_Output, Callback_Data, Line_By_Line, Directory,
         Remote_Host, Remote_Protocol, Show_In_Task_Manager,
         Synchronous, Show_Exit_Status, Fd);
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
         Timeout_Remove (Console.Id);
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
         Timeout_Remove (Console.Id);
         Cleanup (Console);
         Unchecked_Free (Console.Expect_Regexp);
         Unref (Console);
         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         Timeout_Remove (Console.Id);
         Unchecked_Free (Console.Expect_Regexp);
         Cleanup (Console);
         Unref (Console);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Delete_Handler;

end GPS.Kernel.Timeout;

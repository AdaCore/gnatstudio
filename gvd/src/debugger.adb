------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regpat;                use GNAT.Regpat;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Glib.Convert;
with Glib.Object;                use Glib.Object;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Types;               use Gtkada.Types;

with Commands;                   use Commands;
with Config;                     use Config;
with GVD;                        use GVD;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Process;                use GVD.Process;
with GVD.Types;                  use GVD.Types;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Remote;          use GPS.Kernel.Remote;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Intl;                   use GPS.Intl;
with Items;                      use Items;
with Language;                   use Language;
with Language.Debugger;          use Language.Debugger;
with Process_Proxies;            use Process_Proxies;
with Remote;                     use Remote;
with String_Utils;               use String_Utils;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

package body Debugger is

   use String_History, Language_Lists;

   Me : constant Trace_Handle := Create ("Debugger");

   procedure Free is new
     Ada.Unchecked_Deallocation (Command_Record, Command_Access);

   type Output_Monitor_Command is new Root_Command with record
      Process : Visual_Debugger;
   end record;
   type Output_Monitor_Command_Access is access Output_Monitor_Command;
   overriding function Execute (Command : access Output_Monitor_Command)
     return Command_Return_Type;
   --  Called when waiting output from the debugger.
   --  This procedure is activated to handle asynchronous commands.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger, until a prompt is found.

   ---------------------
   -- Local Functions --
   ---------------------

   procedure Send_Internal_Pre
     (Debugger         : access Debugger_Root'Class;
      Cmd              : String;
      Empty_Buffer     : Boolean := True;
      Mode             : Command_Type);
   --  Internal procedure used by Send. This takes care of sending the
   --  command to the debugger, but doesn't parse or even read the output.
   --  The command is displayed in the command window and added to the
   --  history if necessary

   procedure Send_Internal_Post
     (Debugger          : access Debugger_Root'Class;
      Mode              : Command_Type;
      Always_Emit_Hooks : Boolean);
   --  Various updates and cleanups to be done when the current command has
   --  finished executing. This will also run other commands in the queue, if
   --  there are any, before running the appropriate hooks.
   --  The hooks reporting the change of state of the debugger are only emited
   --  when the mode is not Internal. But if Always_Emit_Hooks is true, they
   --  are always emitted.

   procedure Internal_Send
     (Debugger        : not null access Debugger_Root;
      Cmd             : String;
      Synchronous     : Boolean;
      Output          : out GNAT.OS_Lib.String_Access;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : Command_Type := Hidden)
     with Pre => (not Synchronous or else Wait_For_Prompt);
   --  Internal version of Send
   --  This version sends a command to the debugger, and will possibly wait for
   --  its output (if Synchronous is True, otherwise Output is always set to
   --  null).
   --  See Send for the explanation for the other parameters.

   procedure On_Debugger_Died
     (Debugger : not null access Debugger_Root'Class);
   --  This is called when the debugger process has died.
   --  Diaplay a message dialog to prevent the user that the undelying debugger
   --  is dead and close the debugging session.

   ---------------------
   -- Command Queuing --
   ---------------------

   procedure Queue_Command
     (Debugger        : access Debugger_Root'Class;
      Cmd             : String;
      Empty_Buffer    : Boolean;
      Mode            : Command_Type);
   --  Queue a given command to be executed after the next call to Wait.

   function Process_Command
     (Debugger : access Debugger_Root'Class) return Boolean;
   --  Call the first command queued for Debugger.
   --  Return False if no command are in the queue, True otherwise.

   ----------
   -- Free --
   ----------

   procedure Free (Bt : in out Backtrace_Vector) is
   begin
      for J of Bt loop
         Free (J.Subprogram);
         J.File := No_File;
         for P of J.Parameters loop
            Free (P.Value);
         end loop;
         J.Parameters.Clear;
      end loop;
      Bt.Clear;
   end Free;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Items.Generic_Type_Access
   is
      Result   : Generic_Type_Access;
      Type_Str : constant String  := Debugger.Type_Of (Entity);
      Index    : Natural := Type_Str'First;

   begin
      if Type_Str'Length /= 0 then
         Language_Debugger_Access (Debugger.Get_Language).Parse_Type
           (Type_Str, Entity, Index, Result);
      end if;

      return Result;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Debugger    : access Debugger_Root'Class;
      Entity      : String;
      Value       : in out Items.Generic_Type_Access;
      Format      : Value_Format := Default_Format;
      Value_Found : out Boolean)
   is
      Type_Str   : constant String := Debugger.Value_Of (Entity, Format);
      Index      : Natural := Type_Str'First;
      Repeat_Num : Positive;

   begin
      Reset_Recursive (Value);
      Value_Found := Type_Str'Length /= 0;

      if Value_Found then
         Language_Debugger_Access (Debugger.Get_Language).Parse_Value
           (Type_Str, Index, Value, Repeat_Num);
      end if;
   end Parse_Value;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Debugger     : access Debugger_Root;
      The_Language : access Language.Language_Root'Class)
   is
      C : Language_Lists.Cursor := First (Debugger.Languages);
   begin
      while Has_Element (C) loop
         if Element (C) = The_Language then
            Debugger.The_Language := C;
            return;
         end if;
         Next (C);
      end loop;

      Append (Debugger.Languages, Language_Access (The_Language));
      Debugger.The_Language := Last (Debugger.Languages);
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Debugger : access Debugger_Root;
      Lang     : String := "") return Language.Language_Access
   is
      C : Language_Lists.Cursor;
   begin
      if Lang = "" then
         if Has_Element (Debugger.The_Language) then
            return Element (Debugger.The_Language);
         else
            return null;
         end if;
      end if;

      C := First (Debugger.Languages);
      while Has_Element (C) loop
         if Equal (Element (C).Get_Name, Lang, Case_Sensitive => False) then
            return Element (C);
         end if;
         Next (C);
      end loop;
      return null;
   end Get_Language;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Debugger : access Debugger_Root) return Process_Proxy_Access is
   begin
      return Debugger.Process;
   end Get_Process;

   -------------------
   -- General_Spawn --
   -------------------

   procedure General_Spawn
     (Debugger       : access Debugger_Root'Class;
      Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Arguments      : GNAT.OS_Lib.Argument_List;
      Debugger_Name  : String;
      Proxy          : Process_Proxies.Process_Proxy_Access)
   is
      Process    : Visual_Debugger;
      Descriptor : Process_Descriptor_Access;
      Success    : Boolean;
      CL         : Arg_List := Create (Debugger_Name);

   begin
      for J in Arguments'Range loop
         Append_Argument (CL, Arguments (J).all, One_Arg);
      end loop;

      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process := Proxy;
      Debugger.Kernel  := Kernel_Handle (Kernel);

      --   ??? Should use GPS.Kernel.Timeout instead
      GPS.Kernel.Remote.Spawn
        (Kernel            => Debugger.Kernel,
         Arguments         => CL,
         Server            => Debug_Server,
         Pd                => Descriptor,
         Success           => Success);

      if not Success
        or else Descriptor.Get_Pid = GNAT.Expect.Invalid_Pid
      then
         raise Spawn_Error;
      end if;

      Debugger.Process.Set_Descriptor (Descriptor);
      Debugger.Set_Is_Started (False);

      --  Install a callback on the debugger's output file descriptor

      Process := GVD.Process.Convert (Debugger);

      if Process /= null then
         declare
            C : constant Output_Monitor_Command_Access :=
              new Output_Monitor_Command;
         begin
            C.Process := Process;
            Launch_Background_Command
              (Kernel            => Kernel,
               Command           => C,
               Active            => False,
               Show_Bar          => False,
               Queue_Id          => Debug_Queue_Name,
               Block_Exit        => False,
               Start_Immediately => False);

            --  We have an issue with the monitoring of the lifecycle of the
            --  Process above: we want to stop processing this command as
            --  soon as we free the data structures. Right now there is no
            --  way to do this as part of Process itself, so we rely on
            --  GVD.Process.Close_Debugger to interrupt the queue.
         end;
      end if;
   end General_Spawn;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Debugger    : access Debugger_Root;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      pragma Unreferenced (Debugger, Str);
   begin
      Name := Null_Unbounded_String;
      Line := 0;
      Addr := Invalid_Address;
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   procedure Found_Frame_Info
     (Debugger    : access Debugger_Root;
      Str         : String;
      Frame       : out Unbounded_String;
      Message     : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger, Str);
   begin
      Frame   := Null_Unbounded_String;
      Message := Location_Not_Found;
   end Found_Frame_Info;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   function Get_Uniq_Id
     (Debugger : access Debugger_Root;
      Entity   : String) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return Entity;
   end Get_Uniq_Id;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Debugger_Root) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Debugger);
      A : GNAT.Strings.String_List (1 .. 0);
   begin
      return A;
   end Source_Files_List;

   ----------------------
   -- Output_Available --
   ----------------------

   Line_Regexp : constant Pattern_Matcher := Compile ("^.*?\n");
   --  Matches a complete line

   overriding function Execute
     (Command : access Output_Monitor_Command)
      return Command_Return_Type
   is
      Process  : Visual_Debugger renames Command.Process;
      Debugger : constant Debugger_Access := Process.Debugger;
      Mode     : Command_Type;
      Match    : Expect_Match;

      procedure Flush_Async_Output;
      --  Check whether we got an asynchronous output from the debugger
      --  and flush all lines found.

      ------------------------
      -- Flush_Async_Output --
      ------------------------

      procedure Flush_Async_Output is
      begin
         loop
            Debugger.Get_Process.Wait (Match, Line_Regexp, Timeout => 1);
            exit when Match <= 0;

            if Active (Me) then
               declare
                  S : constant String :=
                    Strip_CR (Debugger.Get_Process.Expect_Out);
               begin
                  --  Reduce noise in output
                  if S /= (1 => ASCII.LF) then
                     Trace (Me, "async output: " & S);
                  end if;
               end;
            end if;

            --  Should we do anything more here??? Note that filters
            --  have already been called automatically by GNAT.Expect
         end loop;
      end Flush_Async_Output;

   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if Debugger = null then
         return Success;
      end if;

      case Debugger.State is
         when Async_Wait =>
            --  Continue processing below
            null;

         when Idle =>
            Flush_Async_Output;
            return Execute_Again;

         when Sync_Wait =>
            --  In therory this shouldn't happen since calls are blocking
            --  and this subprogram will never get a chance to be called
            return Execute_Again;
      end case;

      pragma Assert (Debugger.State = Async_Wait);

      if Debugger.Wait_Prompt (Timeout => 1) then
         Debugger.Continuation_Line := False;
         Mode := Debugger.Get_Process.Get_Command_Mode;

         Debugger.State := Idle;
         Flush_Async_Output;

         Send_Internal_Post (Debugger, Mode, Always_Emit_Hooks => True);
      end if;

      return Execute_Again;

   exception
      when E : Process_Died =>
         Trace (Me, E);

         On_Debugger_Died (Debugger);
         return Failure;

      when E : others =>
         --  Will close the debugger in GVD.Process when getting this
         --  exception the next time.

         Trace (Me, E);

         if Debugger /= null and then Debugger.Get_Process /= null then
            Debugger.Get_Process.Set_Command_In_Process (False);
         end if;

         Free (Process.Current_Command);
         Process.Unregister_Dialog;
         return Failure;
   end Execute;

   -----------------------
   -- Send_Internal_Pre --
   -----------------------

   procedure Send_Internal_Pre
     (Debugger     : access Debugger_Root'Class;
      Cmd          : String;
      Empty_Buffer : Boolean := True;
      Mode         : Command_Type)
   is
      Data    : History_Data;
      Process : Visual_Debugger;
      Kind    : Command_Category;

   begin
      Debugger.Get_Process.Set_Command_Mode (Mode);
      Kind := Debugger.Command_Kind (Cmd);

      if not Debugger.Is_Started
        and then Kind = Execution_Command
      then
         Debugger.Set_Is_Started (True);
      end if;

      Process := GVD.Process.Convert (Debugger);
      if Process /= null then
         if not Debugger.Get_Process.Command_In_Process then
            --  If we are already processing a command, this means we set a
            --  Force_Send parameter to True in the call to Send. Most notably,
            --  this is used when sending additional input to the debugger (for
            --  instance answering a gdb question), and we do not want to
            --  change the current command in this case.

            if Process.Current_Command /= null then
               Assert (Me, Process.Current_Command = null,
                       "Memory leak, still has cmd="
                       & Process.Current_Command.all
                       & " while sending " & Cmd,
                       Raise_Exception => True);
            end if;

            Process.Current_Command := new String'(Cmd);
         end if;

         Debugger.Get_Process.Set_Command_In_Process;

         if Mode /= Internal
           and then Kind = Execution_Command
         then
            Unhighlight_Current_Line (Debugger.Kernel);
         end if;

         --  Display the command in the output window if necessary

         if Mode = Visible then
            Output_Text (Process, Cmd & ASCII.LF, True);
         end if;

      else
         Debugger.Get_Process.Set_Command_In_Process;
      end if;

      --  Append the command to the history if necessary

      if Index_Non_Blank (Cmd) /= 0
        and then Mode /= Internal
        and then Process /= null
      then
         Data.Mode    := Mode;
         Data.Command := new String'
           (Cmd (Index_Non_Blank (Cmd) .. Index_Non_Blank (Cmd, Backward)));
         Append (Process.Command_History, Data);
      end if;

      --  Send the command to the debugger

      Debugger.Get_Process.Send (Cmd, Empty_Buffer);
   end Send_Internal_Pre;

   ------------------------
   -- Send_Internal_Post --
   ------------------------

   procedure Send_Internal_Post
     (Debugger          : access Debugger_Root'Class;
      Mode              : Command_Type;
      Always_Emit_Hooks : Boolean)
   is
      Process : constant Visual_Debugger := GVD.Process.Convert (Debugger);
      Bp_Might_Have_Changed : Boolean;
      Kind                  : Command_Category;
      Dummy                 : Boolean;

   begin
      Debugger.Get_Process.Set_Command_In_Process (False);

      --  ??? Process might be null in the testsuite.

      if Process /= null then
         --  Compute whether breakpoints might have changed before running
         --  hooks and e.g. running other debugger commands as a side effect.

         Bp_Might_Have_Changed := Debugger.Breakpoints_Changed
           (Process.Current_Command.all);
         Kind := Debugger.Command_Kind (Process.Current_Command.all);

         Free (Process.Current_Command);
         Process.Unregister_Dialog;

         --  Are there still commands to run in the queue ? If yes, execute
         --  them and let the last one do the post-processing.
         if Mode /= Internal and then Process_Command (Debugger) then
            return;
         end if;

         Process.Final_Post_Process
           (Mode,
            Always_Emit_Hooks              => Always_Emit_Hooks,
            Category                       => Kind,
            Breakpoints_Might_Have_Changed => Bp_Might_Have_Changed);

         --  In case a command has been queued while handling the signals and
         --  breakpoints above.

         if Mode /= Internal then
            Dummy := Process_Command (Debugger);
         end if;
      end if;
   end Send_Internal_Post;

   -------------------
   -- Internal_Send --
   -------------------

   procedure Internal_Send
     (Debugger        : not null access Debugger_Root;
      Cmd             : String;
      Synchronous     : Boolean;
      Output          : out GNAT.OS_Lib.String_Access;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : Command_Type := Hidden)
   is
      Process  : constant Visual_Debugger := GVD.Process.Convert (Debugger);
      Last     : Positive := Cmd'First;
      Cmd_Last : Natural;
      First    : Positive;

      procedure Wait_For_Prompt_And_Get_Output;
      --  Wait for the prompt synchronously, then get the full debugger
      --  output. Finally, terminate do all the post-processing.

      procedure Wait_For_Prompt_And_Get_Output is
         Dummy   : Boolean;
      begin
         Debugger_Access (Debugger).Wait_Prompt;
         Debugger.Continuation_Line := False;
         Debugger.State := Idle;

         if Synchronous then
            Free (Output);

            declare
               S : String := Glib.Convert.Locale_To_UTF8
                 (Debugger.Get_Process.Expect_Out);
               L : Natural;
            begin
               --  Strip CRs in remote mode, as we can't know in advance if the
               --  debug server outputs CR/LF or just LF, and the consequences
               --  or removing CRs in the latter case are better than not
               --  removing them in the first case
               if Need_To_Strip_CR or else not Is_Local (Debug_Server) then
                  Strip_CR (S, L, CR_Found => Dummy);
                  Output := new String'(S (S'First .. L));
               else
                  Output := new String'(S);
               end if;
            end;
         end if;

         Send_Internal_Post (Debugger, Mode, Always_Emit_Hooks => False);
      end Wait_For_Prompt_And_Get_Output;

   begin
      Free (Output);

      if Cmd = "" then
         return;
      end if;

      --  When there are multiple commands separated by ASCII.LF, Force_Send
      --  applies to the command set as a whole. If the debugger is processing
      --  a command, we send none of them, otherwise we send them all without
      --  queuing any of them. Chaining of commands through ASCII.LF seems to
      --  only occur in a few limited cases anyway (Set_Breakpoint_Command for
      --  instance).

      if Debugger.Get_Process.Command_In_Process then
         if Synchronous then
            Trace (Me, "Cannot send command " & Cmd & " since debugger is"
                   & " already processing"
                   & (if Process = null or else  Process.Current_Command = null
                     then "" else Process.Current_Command.all));
            return;

         elsif not Force_Send then
            --  Will be processed by the same Send later on
            Queue_Command (Debugger, Cmd, Empty_Buffer, Mode);
            return;
         end if;
      end if;

      --  Cleanup the command to remove special characters that would make
      --  the debugger and GPS hang
      --  ??? Should forbid commands that modify the configuration of the
      --  debugger, like "set annotate" for gdb, otherwise we can't be sure
      --  what to expect from the debugger.

      Cmd_Last := Cmd'Last;
      while Cmd_Last >= Cmd'First
        and then (Cmd (Cmd_Last) = '\' or else Cmd (Cmd_Last) = ASCII.HT)
      loop
         Cmd_Last := Cmd_Last - 1;
      end loop;

      --  Each command is separated with a ASCII.LF and is handled separately

      Skip_Blanks (Cmd (Cmd'First .. Cmd_Last), Last);
      loop
         First := Last;
         Skip_To_Char (Cmd (Cmd'First .. Cmd_Last), Last, ASCII.LF);

         --  Test custom commands defined by the views and user plugins. This
         --  might execute or queue commands (by ultimately calling this same
         --  Internal_Send procedure).

         if Debugger.Kernel /= null then   --  not in the testsuite
            declare
               Tmp : constant String := Debugger_Command_Action_Hook.Run
                  (Kernel   => Debugger.Kernel,
                   Debugger => Process,
                   Str      => Cmd (First .. Last - 1));
            begin
               if Tmp = Command_Intercepted then
                  if Mode >= Visible then
                     Process.Debugger.Display_Prompt;
                  end if;
                  return;

               elsif Tmp /= "" then
                  if Mode in Visible_Command then
                     Process.Output_Text
                       (Tmp,
                        Is_Command   => False,
                        Set_Position => True);

                     Debugger_Root'Class (Debugger.all).Display_Prompt;
                  end if;
                  return;
               end if;
            end;
         end if;

         --  Used to have the following text:
         --    if Mode not in Invisible_Command
         --       and then Wait_For_Prompt
         --       and then Command_In_Process (Get_Process (Debugger))
         --  However, this fails sometimes with gdb: when "cont" terminates the
         --  program, gdb is emitting a "tty" command while cont is still
         --  being processed, and therefore the if was changed to the below to
         --  queue the tty command.
         --  ??? In fact, I (Manu) am not sure how this was working before: if
         --  we are waiting for a prompt, we should not be queuing the command
         --  still this would return to the caller before we saw the prompt.

         Send_Internal_Pre
           (Debugger, Cmd (First .. Last - 1), Empty_Buffer, Mode);

         case Mode is
            when Invisible_Command =>
               Debugger.State := Sync_Wait;

               if Last > Cmd_Last and then Wait_For_Prompt then
                  Wait_For_Prompt_And_Get_Output;
               end if;

            when Visible_Command =>
               if Wait_For_Prompt then
                  if Synchronous then
                     Wait_For_Prompt_And_Get_Output;
                  else
                     --  Asynchronous handling of commands, done in
                     --  Output_Available.

                     Debugger.State := Async_Wait;
                  end if;

               else   --  always asynchronous
                  if Mode >= Visible then
                     --  Clear the current output received from the debugger
                     --  to avoid confusing the prompt detection, since
                     --  we're sending input in the middle of a command,
                     --  which is delicate.

                     Process_Proxies.Empty_Buffer (Debugger.Get_Process);
                  end if;
               end if;
         end case;

         exit when Last > Cmd_Last;
         Last := Last + 1;
      end loop;

   exception
      when E : Process_Died =>
         Trace (Me, E);

         On_Debugger_Died (Debugger);
   end Internal_Send;

   ----------------------
   -- On_Debugger_Died --
   ----------------------

   procedure On_Debugger_Died
     (Debugger : not null access Debugger_Root'Class)
   is
      Process : constant Visual_Debugger := GVD.Process.Convert (Debugger);
      Dummy   : Message_Dialog_Buttons;
   begin
      if Process /= null then
         Free (Process.Current_Command);

         if Process.Exiting then
            return;
         end if;

         Dummy :=
           Message_Dialog
             (Debugger.Get_Process.Expect_Out & ASCII.LF &
              (-"The underlying debugger died unexpectedly. Closing it"),
              Error, Button_OK, Button_OK,
              Parent => Debugger.Kernel.Get_Main_Window);
         Process.Close_Debugger;
      end if;
   end On_Debugger_Died;

   ----------
   -- Send --
   ----------

   procedure Send
     (Debugger        : access Debugger_Root;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : Command_Type := Hidden)
   is
      Output : GNAT.OS_Lib.String_Access;
   begin
      Internal_Send
        (Debugger,
         Cmd             => Cmd,
         Synchronous     => False,
         Output          => Output,   --  will always be null
         Empty_Buffer    => Empty_Buffer,
         Wait_For_Prompt => Wait_For_Prompt,
         Force_Send      => Force_Send,
         Mode            => Mode);
      Free (Output);  --  Just in case
   end Send;

   -------------------------
   -- Send_And_Get_Output --
   -------------------------

   function Send_And_Get_Output
     (Debugger : access Debugger_Root;
      Cmd      : String;
      Mode     : Command_Type := Hidden) return String
   is
      Output : GNAT.OS_Lib.String_Access;
   begin
      Internal_Send
        (Debugger,
         Cmd             => Cmd,
         Synchronous     => True,
         Output          => Output,
         Empty_Buffer    => True,
         Wait_For_Prompt => True,
         Force_Send      => False,
         Mode            => Mode);

      if Output = null then
         return "";
      else
         return S : constant String := Output.all do
            Free (Output);
         end return;
      end if;
   end Send_And_Get_Output;

   ---------------------
   -- List_Exceptions --
   ---------------------

   function List_Exceptions
     (Debugger : access Debugger_Root) return Exception_Array
   is
      pragma Unreferenced (Debugger);

      Arr : Exception_Array (1 .. 0);
   begin
      return Arr;
   end List_Exceptions;

   -------------------
   -- Get_Type_Info --
   -------------------

   function Get_Type_Info
     (Debugger  : access Debugger_Root;
      Entity    : String;
      Default   : String) return String
   is
      pragma Unreferenced (Debugger, Entity);
   begin
      return Default;
   end Get_Type_Info;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Debugger : access Debugger_Root; File_Name : String) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return File_Name;
   end Find_File;

   ----------------
   -- Is_Started --
   ----------------

   function Is_Started (Debugger : access Debugger_Root) return Boolean is
   begin
      return Debugger.Is_Started;
   end Is_Started;

   --------------------
   -- Set_Is_Started --
   --------------------

   procedure Set_Is_Started
     (Debugger   : access Debugger_Root;
      Is_Started : Boolean)
   is
      Process : constant Visual_Debugger := GVD.Process.Convert (Debugger);
   begin
      Debugger.Is_Started := Is_Started;
      if Process /= null then   --  null in testsuite
         Debugger_Process_Terminated_Hook.Run (Process.Kernel, Process);
      end if;
   end Set_Is_Started;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Debugger : access Debugger_Root;
      Var_Name : String;
      Value    : String)
   is
      S : constant String :=
        Language_Debugger_Access (Debugger.Get_Language).Set_Variable
        (Var_Name, Value);
   begin
      if S /= "" then
         --  We need to send the command in hidden mode (synchronously)
         --  because right after this call, Set_Value will typically request
         --  the new value of the variable, before we got the debugger's
         --  prompt asynchronously.
         --  ??? On the other hand, we want the command to be visible in the
         --  console (and its output, since we get an error when casting to an
         --  invalid value for instance), and we also want the Variables view
         --  to refresh after the command.

         Debugger.Send (S, Mode => Hidden);
      end if;
   end Set_Variable;

   -----------------------
   -- Wait_User_Command --
   -----------------------

   procedure Wait_User_Command (Debugger : access Debugger_Root) is
      Current_Process : Process_Proxy_Access;
      Tmp             : Boolean;
      pragma Unreferenced (Tmp);

      Num_Events      : Positive;
      Max_Events      : constant := 30;
      --  Limit the number of events to process in one iteration

   begin
      --  Wait until the command has been processed

      Current_Process := Debugger.Get_Process;

      --  Make sure that Current_Process is not null before calling
      --  Command_In_Process : this can happen when GVD is exiting.

      while Current_Process /= null
        and then Current_Process.Command_In_Process
      loop
         Num_Events := 1;

         while Gtk.Main.Events_Pending
           and then Num_Events <= Max_Events
         loop
            Tmp := Gtk.Main.Main_Iteration;
            Num_Events := Num_Events + 1;
         end loop;

         Current_Process := Debugger.Get_Process;
      end loop;
   end Wait_User_Command;

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Thread_Information_Array) is
   begin
      for J in Info'Range loop
         Free (Info (J).Information);
      end loop;
   end Free;

   -------------------
   -- Queue_Command --
   -------------------

   procedure Queue_Command
     (Debugger        : access Debugger_Root'Class;
      Cmd             : String;
      Empty_Buffer    : Boolean;
      Mode            : Command_Type)
   is
      Tmp     : Command_Access := Debugger.Command_Queue;
      Command : Command_Access;

   begin
      Command := new Command_Record'
        (Cmd             => new String'(Cmd),
         Empty_Buffer    => Empty_Buffer,
         Mode            => Mode,
         Next            => null);

      if Tmp = null then
         Debugger.Command_Queue := Command;
      else
         while Tmp.Next /= null loop
            Tmp := Tmp.Next;
         end loop;

         Tmp.Next := Command;
      end if;
   end Queue_Command;

   ---------------------
   -- Process_Command --
   ---------------------

   function Process_Command
     (Debugger : access Debugger_Root'Class) return Boolean
   is
      Command : Command_Access := Debugger.Command_Queue;
   begin
      if Command = null then
         return False;
      end if;

      Debugger.Command_Queue := Command.Next;
      Debugger.Send
        (Command.Cmd.all,
         Command.Empty_Buffer,
         Mode => Command.Mode);

      Free (Command.Cmd);
      Free (Command);
      return True;
   end Process_Command;

   -----------------
   -- Clear_Queue --
   -----------------

   procedure Clear_Queue (Debugger : access Debugger_Root'Class) is
      Command : Command_Access := Debugger.Command_Queue;
   begin
      while Command /= null loop
         Debugger.Command_Queue := Command.Next;
         Free (Command.Cmd);
         Free (Command);
      end loop;
   end Clear_Queue;

   use GVD.Proc_Utils;

   --------------------
   -- Open_Processes --
   --------------------

   procedure Open_Processes (Debugger : access Debugger_Root) is
   begin
      Open_Processes (Debugger.Handle, Debugger.Kernel);
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
     (Debugger : access Debugger_Root;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean) is
   begin
      Next_Process (Debugger.Handle, Info, Success);
   end Next_Process;

   ---------------------
   -- Close_Processes --
   ---------------------

   procedure Close_Processes (Debugger : access Debugger_Root) is
   begin
      Close_Processes (Debugger.Handle);
   end Close_Processes;

   -----------------
   -- Support_TTY --
   -----------------

   function Support_TTY (Debugger : access Debugger_Root) return Boolean is
      pragma Unreferenced (Debugger);
   begin
      return False;
   end Support_TTY;

   -----------------------
   -- Continuation_Line --
   -----------------------

   function Continuation_Line
     (Debugger : access Debugger_Root) return Boolean is
   begin
      return Debugger.Continuation_Line;
   end Continuation_Line;

   -------------------------------
   -- Separate_Execution_Window --
   -------------------------------

   function Separate_Execution_Window
     (Debugger : access Debugger_Root) return Boolean is
   begin
      return Debugger.Execution_Window;
   end Separate_Execution_Window;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Debugger_Root) is
      Result : Expect_Match;
      C      : Language_Lists.Cursor := First (Debugger.Languages);
      Lang   : Language.Language_Access;
   begin
      while Has_Element (C) loop
         Lang := Element (C);
         Language.Free (Lang);
         Next (C);
      end loop;
      Clear (Debugger.Languages);

      if Debugger.Get_Process /= null
        and then Debugger.Get_Process.Get_Descriptor /= null
      then
         begin
            --  Ensure that the debugger is terminated before closing the pipes
            --  and trying to kill it abruptly.

            begin
               Debugger.Get_Process.Wait (Result, ".+", Timeout => 200);
            exception
               when Process_Died =>
                  --  This is somewhat expected... RIP.
                  null;
            end;
            Debugger.Get_Process.Get_Descriptor.Close;
         exception
            when Process_Died =>
               null;
         end;
      end if;

      Free (Debugger.Process);
      Free (Debugger.Remote_Target);
      Free (Debugger.Remote_Protocol);

      --  ??? Shouldn't we free Command_Queue
   end Close;

   -----------------------
   -- Get_Remote_Target --
   -----------------------

   function Get_Remote_Target
     (Debugger : access Debugger_Root) return String
   is
     (if Debugger.Remote_Target /= null then
         Debugger.Remote_Target.all
      else
         "");

   -------------------------
   -- Get_Remote_Protocol --
   -------------------------

   function Get_Remote_Protocol
     (Debugger : access Debugger_Root) return String
   is
     (if Debugger.Remote_Protocol /= null then
         Debugger.Remote_Protocol.all
      else
         "");

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Debugger : access Debugger_Root'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Debugger.Kernel;
   end Get_Kernel;

   ----------------
   -- Info_Tasks --
   ----------------

   procedure Info_Tasks
     (Debugger : access Debugger_Root;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Info, Debugger);
   begin
      Len := 0;
   end Info_Tasks;

   ------------------
   -- Info_Threads --
   ------------------

   procedure Info_Threads
     (Debugger : access Debugger_Root;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Info, Debugger);
   begin
      Len := 0;
   end Info_Threads;

   -------------
   -- Info_PD --
   -------------

   procedure Info_PD
     (Debugger : access Debugger_Root;
      Info     : out PD_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Info, Debugger);
   begin
      Len := 0;
   end Info_PD;

   ---------------------
   -- VxWorks_Version --
   ---------------------

   function VxWorks_Version
     (Debugger : access Debugger_Root)
      return GVD.Types.VxWorks_Version_Type
   is
      pragma Unreferenced (Debugger);
   begin
      return Vx_None;
   end VxWorks_Version;

   -------------------
   -- Filter_Output --
   -------------------

   procedure Filter_Output
     (Debugger : access Debugger_Root;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String)
   is
      pragma Unreferenced (Debugger, Mode);
   begin
      Set_Unbounded_String (Result, Str);
   end Filter_Output;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   function Is_Quit_Command
     (Debugger : access Debugger_Root;
      Command : String) return Boolean is
      pragma Unreferenced (Debugger, Command);
   begin
      return False;
   end Is_Quit_Command;

   -------------
   -- To_File --
   -------------

   function To_File
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
     return GNATCOLL.VFS.Virtual_File
   is
      F : Virtual_File;
   begin
      --  Translate filename into local file if needed
      F := To_Local (Create (+Name, Get_Nickname (Debug_Server)));

      --  Convert from a patch returned by the debugger to the actual
      --  path in the project, in case sources have changed
      if not F.Is_Absolute_Path or else not F.Is_Regular_File then
         F := Kernel.Create_From_Base (F.Full_Name);
      end if;

      return F;
   end To_File;

end Debugger;

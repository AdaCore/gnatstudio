-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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

with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with Glib;                       use Glib;
with Glib.Convert;
with Glib.Object;                use Glib.Object;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Types;               use Gtkada.Types;

with Config;                     use Config;
with GVD;                        use GVD;
with GVD.Canvas;                 use GVD.Canvas;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Process;                use GVD.Process;
with GVD.Source_Editor;          use GVD.Source_Editor;
with GVD.Scripts;                use GVD.Scripts;
with GVD.Types;                  use GVD.Types;
with GPS.Kernel.Remote;          use GPS.Kernel.Remote;
with GPS.Intl;                   use GPS.Intl;
with Items;                      use Items;
with Language;                   use Language;
with Language.Debugger;          use Language.Debugger;
with Process_Proxies;            use Process_Proxies;
with Remote;                     use Remote;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;

package body Debugger is

   use String_History, Language_Lists;

   Me : constant Debug_Handle := Create ("Debugger");

   Debug_Timeout : constant Guint32 := 100;
   --  Timeout in millisecond to check input from the underlying debugger
   --  when handling asynchronous commands.

   package Debugger_Timeout is new Gtk.Main.Timeout (Visual_Debugger);

   ---------------------
   -- Local Functions --
   ---------------------

   function Output_Available (Process : Visual_Debugger) return Boolean;
   --  Called when waiting output from the debugger.
   --  This procedure is activated to handle asynchronous commands.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger, until a prompt is found.

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
     (Debugger         : access Debugger_Root'Class;
      Mode             : Command_Type);
   --  Internal procedure used by Send. This takes care of processing the
   --  output of the debugger, but it doesn't read it.
   --  This should be called only if we are currently waiting for the next
   --  prompt, ie processing the output
   --  Note that this function will do nothing if Mode is Internal.

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

   -----------------------
   -- Connect_To_Target --
   -----------------------

   procedure Connect_To_Target
     (Debugger : access Debugger_Root;
      Target   : String;
      Protocol : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Target, Protocol, Mode);
   begin
      null;
   end Connect_To_Target;

   ----------
   -- Free --
   ----------

   procedure Free (Bt : in out Backtrace_Array) is
   begin
      for J in Bt'Range loop
         Free (Bt (J).Program_Counter);
         Free (Bt (J).Subprogram);
         Free (Bt (J).Source_Location);
      end loop;
   end Free;

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Items.Generic_Type_Access
   is
      Result   : Generic_Type_Access;
      Type_Str : constant String  := Type_Of (Debugger, Entity);
      Index    : Natural := Type_Str'First;

   begin
      if Type_Str'Length /= 0 then
         Parse_Type
           (Language_Debugger_Access (Get_Language (Debugger)),
            Type_Str, Entity, Index, Result);
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
      Type_Str   : constant String := Value_Of (Debugger, Entity, Format);
      Index      : Natural := Type_Str'First;
      Repeat_Num : Positive;

   begin
      Reset_Recursive (Value);
      Value_Found := Type_Str'Length /= 0;

      if Value_Found then
         Parse_Value
           (Language_Debugger_Access (Get_Language (Debugger)),
            Type_Str, Index, Value, Repeat_Num);
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
         return Element (Debugger.The_Language);
      end if;

      C := First (Debugger.Languages);
      while Has_Element (C) loop
         if Equal (Get_Name (Element (C)), Lang, Case_Sensitive => False) then
            return Element (C);
         end if;
         Next (C);
      end loop;
      return null;
   end Get_Language;

   ---------------------
   -- Detect_Language --
   ---------------------

   procedure Detect_Language (Debugger : access Debugger_Root) is
      pragma Unreferenced (Debugger);
   begin
      null;
   end Detect_Language;

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
      Descriptor : Process_Descriptor_Access;
      Success    : Boolean;
      The_Args   : Argument_List := (new String'(Debugger_Name) &
                                     Arguments);

   begin
      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process := Proxy;
      Debugger.Kernel  := Kernel_Handle (Kernel);

      GPS.Kernel.Remote.Spawn
        (Kernel            => Debugger.Kernel,
         Arguments         => The_Args,
         Server            => Debug_Server,
         Pd                => Descriptor,
         Success           => Success);
      Free (The_Args (The_Args'First));

      if not Success
        or else Get_Pid (Descriptor.all) = GNAT.Expect.Invalid_Pid
      then
         raise Spawn_Error;
      end if;

      Set_Descriptor (Debugger.Process, Descriptor);
      Set_Is_Started (Debugger, False);
   end General_Spawn;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Debugger    : access Debugger_Root;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural)
   is
      pragma Unreferenced (Debugger, Str);
   begin
      First      := 0;
      Last       := 0;
      Name_First := 0;
      Name_Last  := 1;
      Line       := 0;
      Addr_First := 0;
      Addr_Last  := 1;
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   procedure Found_Frame_Info
     (Debugger    : access Debugger_Root;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger, Str);
   begin
      First := 0;
      Last  := 0;
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

   ---------------------
   -- Lines_With_Code --
   ---------------------

   procedure Lines_With_Code
     (Debugger : access Debugger_Root;
      File     : GNATCOLL.VFS.Virtual_File;
      Result   : out Boolean;
      Lines    : out Line_Array)
   is
      pragma Unreferenced (Debugger, File, Lines);
   begin
      Result := False;
   end Lines_With_Code;

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

   function Output_Available (Process : Visual_Debugger) return Boolean is
      Debugger : constant Debugger_Access := Process.Debugger;
      Mode     : Command_Type;

   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if Debugger = null then
         Timeout_Remove (Process.Timeout_Id);
         Process.Timeout_Id := 0;
         return False;
      end if;

      if Wait_Prompt (Debugger, Timeout => 1) then
         Debugger.Continuation_Line := False;
         Timeout_Remove (Process.Timeout_Id);
         Process.Timeout_Id := 0;
         Mode := Get_Command_Mode (Get_Process (Debugger));

         --  Put back the standard cursor

         if Mode >= Visible then
            Set_Busy (Process, False);
         end if;

         Set_Command_In_Process (Get_Process (Debugger), False);
         Unregister_Dialog (Process);

         --  Do the postprocessing here instead of calling Send_Internal_Post
         --  since we need to handle post processing slightly differently

         declare
            Current_Command : constant String := Get_Command (Process);
            Result          : Boolean;
            pragma Unreferenced (Result);

         begin
            Free (Process.Current_Command);

            if Process_Command (Debugger) then
               --  ??? register if needed for some of the hooks
               --  before returning
               return False;
            end if;

            Final_Post_Process (Process, Mode);

            if Is_Load_Command (Debugger, Current_Command) then
--                 Detect_Language (Debugger);
--                 Pos := Current_Command'First;
--                 Skip_To_Blank (Current_Command, Pos);
--                 Skip_Blanks (Current_Command, Pos);
--            Executable_Name= Current_Command (Pos .. Current_Command'Last);
               Run_Debugger_Hook (Process, Debugger_Executable_Changed_Hook);

            elsif Is_Context_Command (Debugger, Current_Command) then
               Run_Debugger_Hook (Process, Debugger_Context_Changed_Hook);
            elsif Is_Execution_Command (Debugger, Current_Command) then
               Run_Debugger_Hook (Process, Debugger_Process_Stopped_Hook);
            end if;

            Update_Breakpoints
              (Process,
               Force => Is_Break_Command (Debugger, Current_Command));

            --  In case a command has been queued while handling the signals
            --  and breakpoints above.

            Result := Process_Command (Debugger);
         end;

         return False;

      else
         return True;
      end if;

   exception
      when E : others =>
         --  Will close the debugger in GVD.Process when getting this
         --  exception the next time.

         Traces.Trace (Exception_Handle, E);

         if Process.Timeout_Id > 0 then
            Timeout_Remove (Process.Timeout_Id);
         end if;
         Process.Timeout_Id := 0;

         if Debugger /= null and then Get_Process (Debugger) /= null then
            Set_Command_In_Process (Get_Process (Debugger), False);
         end if;

         Set_Busy (Process, False);
         Free (Process.Current_Command);
         Unregister_Dialog (Process);
         return False;
   end Output_Available;

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

   begin
      Set_Command_Mode (Get_Process (Debugger), Mode);

      if not Is_Started (Debugger)
        and then Is_Execution_Command (Debugger, Cmd)
      then
         Set_Is_Started (Debugger, True);
      end if;

      Process := GVD.Process.Convert (Debugger);
      if Process /= null then
         if not Command_In_Process (Get_Process (Debugger)) then
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

         Set_Command_In_Process (Get_Process (Debugger));

         if Mode /= Internal
           and then Is_Execution_Command (Debugger, Cmd)
         then
            Unhighlight_Current_Line
              (Get_Source (Process.Editor_Text), GObject (Process));
         end if;

         if Mode >= Visible then
            Set_Busy (Process);
         end if;

         --  Display the command in the output window if necessary

         if Mode = Visible then
            Output_Text (Process, Cmd & ASCII.LF, True);
         end if;

      else
         Set_Command_In_Process (Get_Process (Debugger));
      end if;

      --  Append the command to the history if necessary

      if Index_Non_Blank (Cmd) /= 0
        and then Mode /= Internal
        and then Process /= null
      then
         Data.Mode := Mode;
         Data.Command := new String'
           (Cmd (Index_Non_Blank (Cmd) .. Index_Non_Blank (Cmd, Backward)));
         Append (Process.Command_History, Data);
      end if;

      --  Send the command to the debugger

      Send (Get_Process (Debugger), Cmd, Empty_Buffer);
   end Send_Internal_Pre;

   ------------------------
   -- Send_Internal_Post --
   ------------------------

   procedure Send_Internal_Post
     (Debugger : access Debugger_Root'Class;
      Mode     : Command_Type)
   is
      Process : Visual_Debugger;
      Result  : Boolean;
      pragma Unreferenced (Result);
      Is_Context, Is_Exec, Is_Break : Boolean;

   begin
      --  See also Output_Available for similar handling.
      Set_Command_In_Process (Get_Process (Debugger), False);

      Process := GVD.Process.Convert (Debugger);

      if Process /= null then
         Is_Context := Is_Context_Command
           (Debugger, Process.Current_Command.all);
         Is_Exec    := Is_Execution_Command
           (Debugger, Process.Current_Command.all);
         Is_Break   := Is_Break_Command
           (Debugger, Process.Current_Command.all);

         Free (Process.Current_Command);
      end if;

      if Mode /= Internal and then Process_Command (Debugger) then
         --  ??? register if needed for hooks before returning
         return;
      end if;

      if Process /= null then
         Final_Post_Process (Process, Mode);

         if Mode /= Internal then
            --  Postprocessing (e.g handling of auto-update).

            if Is_Context then
               Run_Debugger_Hook (Process, Debugger_Context_Changed_Hook);
            elsif Is_Exec then
               Run_Debugger_Hook (Process, Debugger_Process_Stopped_Hook);
            end if;

            Update_Breakpoints (Process, Force => Is_Break);
         end if;

         if Mode >= Visible then
            Set_Busy (Process, False);
         end if;

         Unregister_Dialog (Process);

         --  In case a command has been queued while handling the signals
         --  and breakpoints above.

         if Mode /= Internal then
            Result := Process_Command (Debugger);
         end if;
      end if;
   end Send_Internal_Post;

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
      Process : Visual_Debugger;
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
      Last    : Positive := Cmd'First;
      First   : Positive;

   begin
      loop
         --  Each command is separated with a ASCII.LF and is handled
         --  separately.

         First := Last;
         Skip_To_Char (Cmd, Last, ASCII.LF);

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

         if not Force_Send
           and then Command_In_Process (Get_Process (Debugger))
         then
            Queue_Command
              (Debugger, Cmd (First .. Last - 1), Empty_Buffer, Mode);
         else
            Send_Internal_Pre
              (Debugger, Cmd (First .. Last - 1), Empty_Buffer, Mode);

            case Mode is
               when Invisible_Command =>
                  if Last > Cmd'Last and then Wait_For_Prompt then
                     Wait_Prompt (Debugger_Access (Debugger));
                     Debugger.Continuation_Line := False;
                     Send_Internal_Post (Debugger, Mode);
                  end if;

               when Visible_Command =>
                  if Wait_For_Prompt then
                     if not Async_Commands then
                        --  Synchronous handling of commands, simple case

                        Wait_Prompt (Debugger_Access (Debugger));
                        Debugger.Continuation_Line := False;
                        Send_Internal_Post (Debugger, Mode);

                     else
                        --  Asynchronous handling of commands, install a
                        --  callback on the debugger's output file descriptor.

                        Process := GVD.Process.Convert (Debugger);
                        pragma Assert (Process.Timeout_Id = 0);

                        Process.Timeout_Id := Debugger_Timeout.Add
                          (Debug_Timeout, Output_Available'Access, Process);
                     end if;

                  else
                     if Mode >= Visible then
                        --  Clear the current output received from the debugger
                        --  to avoid confusing the prompt detection, since
                        --  we're sending input in the middle of a command,
                        --  which is delicate.

                        Process_Proxies.Empty_Buffer (Get_Process (Debugger));
                        Process := GVD.Process.Convert (Debugger);
                        Set_Busy (Process, False);
                     end if;
                  end if;
            end case;
         end if;

         exit when Last > Cmd'Last;

         Last := Last + 1;
      end loop;

   exception
      when Process_Died =>
         Process := GVD.Process.Convert (Debugger);
         Free (Process.Current_Command);

         if Process.Exiting then
            return;
         end if;

         Trace (Me, "underlying debugger died unexpectedly in 'send'");
         Button :=
           Message_Dialog
             (Expect_Out (Get_Process (Debugger)) & ASCII.LF &
              (-"The underlying debugger died unexpectedly. Closing it"),
              Error, Button_OK, Button_OK);
         Set_Command_In_Process (Get_Process (Debugger), False);
         Set_Busy (Process, False);
         Unregister_Dialog (Process);
         Close_Debugger (Process);
   end Send;

   ---------------
   -- Send_Full --
   ---------------

   function Send_Full
     (Debugger : access Debugger_Root;
      Cmd      : String;
      Mode     : Invisible_Command := Hidden) return String
   is
      Process  : Visual_Debugger;
      Last     : Natural;
      CR_Found : Boolean;

   begin
      if Command_In_Process (Get_Process (Debugger)) then
         --  Should never happen, but it's safer to return immediately in case
         --  we're compiling without assertions, rather than hanging.

         pragma Assert (False);
         return "";
      end if;

      Send_Internal_Pre (Debugger, Cmd, Mode => Mode);
      Wait_Prompt (Debugger_Access (Debugger));
      Debugger.Continuation_Line := False;

      declare
         S : String :=
           Glib.Convert.Locale_To_UTF8 (Expect_Out (Get_Process (Debugger)));
      begin
         Send_Internal_Post (Debugger, Mode);

         --  Strip CRs in remote mode, as we can't know in advance if the debug
         --  server outputs CR/LF or just LF, and the consequences or removing
         --  CRs in the latter case are better than not removing them in the
         --  first case
         if Need_To_Strip_CR or else not Is_Local (Debug_Server) then
            Strip_CR (S, Last, CR_Found);
            return S (S'First .. Last);
         else
            return S;
         end if;
      end;

   exception
      when Process_Died =>
         Trace (Me, "underlying debugger died unexpectedly in 'send_full'");
         Set_Command_In_Process (Get_Process (Debugger), False);

         Process := GVD.Process.Convert (Debugger);
         if Process /= null then
            Free (Process.Current_Command);
            Set_Busy (Process, False);
            Unregister_Dialog (Process);
            Close_Debugger (Process);
         end if;

         return "";
   end Send_Full;

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
      Is_Started : Boolean) is
   begin
      Debugger.Is_Started := Is_Started;
      Run_Debugger_Hook
        (GVD.Process.Convert (Debugger), Debugger_Process_Terminated_Hook);
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
        Set_Variable (Language_Debugger_Access (Get_Language (Debugger)),
                      Var_Name, Value);
   begin
      if S /= "" then
         Send (Debugger, S, Mode => Visible);
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

      Current_Process := Get_Process (Debugger);

      --  Make sure that Current_Process is not null before calling
      --  Command_In_Process : this can happen when GVD is exiting.

      while Current_Process /= null
        and then Command_In_Process (Current_Process)
      loop
         Num_Events := 1;

         while Gtk.Main.Events_Pending
           and then Num_Events <= Max_Events
         loop
            Tmp := Gtk.Main.Main_Iteration;
            Num_Events := Num_Events + 1;
         end loop;

         Current_Process := Get_Process (Debugger);
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

   procedure Free is new
     Ada.Unchecked_Deallocation (Command_Record, Command_Access);

   function Process_Command
     (Debugger : access Debugger_Root'Class) return Boolean
   is
      Command : Command_Access := Debugger.Command_Queue;
      Process : Visual_Debugger;
      First   : Natural;

   begin
      if Command = null then
         return False;
      end if;

      Debugger.Command_Queue := Command.Next;

      First := Command.Cmd'First;
      Skip_Blanks (Command.Cmd.all, First);

      if Looking_At (Command.Cmd.all, First, "graph") then
         Process := GVD.Process.Convert (Debugger);
         if Process /= null then
            Process_Graph_Cmd (Process, Command.Cmd.all);
         end if;

      else
         Send
           (Debugger, Command.Cmd.all, Command.Empty_Buffer,
            Mode => Command.Mode);
      end if;

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

   -------------
   -- Set_TTY --
   -------------

   procedure Set_TTY (Debugger : access Debugger_Root; TTY : String) is
      pragma Unreferenced (Debugger, TTY);
   begin
      raise Unknown_Command;
   end Set_TTY;

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
      C : Language_Lists.Cursor := First (Debugger.Languages);
      Lang : Language.Language_Access;
   begin
      while Has_Element (C) loop
         Lang := Element (C);
         Language.Free (Lang);
         Next (C);
      end loop;
      Clear (Debugger.Languages);

      if Get_Process (Debugger) /= null
        and then Get_Descriptor (Get_Process (Debugger)) /= null
      then
         begin
            --  Ensure that the debugger is terminated before closing the pipes
            --  and trying to kill it abruptly.

            begin
               Wait (Get_Process (Debugger), Result, ".+", Timeout => 200);
            exception
               when Process_Died =>
                  --  This is somewhat expected... RIP.
                  null;
            end;
            Close (Get_Descriptor (Get_Process (Debugger)).all);
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

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Debugger : access Debugger_Root'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Debugger.Kernel;
   end Get_Kernel;

end Debugger;

-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

pragma Warnings (Off);
with GNAT.Expect;       use GNAT.Expect;
with GNAT.Expect.TTY;   use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Glib;              use Glib;
with Gtk.Main;          use Gtk.Main;
with Gtk.Window;        use Gtk.Window;
with Gdk.Input;
with Gdk.Types;
with Gtkada.Types;      use Gtkada.Types;

with Odd_Intl;          use Odd_Intl;
with Items;             use Items;
with Process_Proxies;   use Process_Proxies;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with GVD;               use GVD;
with GVD.Preferences;   use GVD.Preferences;
with GVD.Process;       use GVD.Process;
with String_Utils;      use String_Utils;
with GVD.Trace;         use GVD.Trace;
with GVD.Types;         use GVD.Types;
with Basic_Types;       use Basic_Types;
with GVD.Main_Window;   use GVD.Main_Window;

with Gtkada.Dialogs;    use Gtkada.Dialogs;

package body Debugger is

   use String_History;

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   ---------------------
   -- Local Functions --
   ---------------------

   function To_Gint is new Ada.Unchecked_Conversion (File_Descriptor, Gint);

   procedure Output_Available
     (Process   : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Called whenever some output becomes available from the debugger.
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
      Cmd              : String;
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
           (Language_Debugger_Access (Debugger.The_Language),
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
      Value_Found : out Boolean)
   is
      Type_Str   : String := Value_Of (Debugger, Entity);
      Index      : Natural := Type_Str'First;
      Repeat_Num : Positive;

   begin
      Reset_Recursive (Value);
      Value_Found := Type_Str'Length /= 0;
      if Value_Found then
         Parse_Value
           (Language_Debugger_Access (Debugger.The_Language),
            Type_Str, Index, Value, Repeat_Num);
      end if;
   end Parse_Value;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Debugger     : access Debugger_Root;
      The_Language : Language.Language_Access) is
   begin
      Language.Free (Debugger.The_Language);
      Debugger.The_Language := The_Language;
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Debugger : access Debugger_Root) return Language.Language_Access is
   begin
      return Debugger.The_Language;
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
      Arguments      : GNAT.OS_Lib.Argument_List;
      Debugger_Name  : String;
      Proxy          : Process_Proxies.Process_Proxy_Access;
      Remote_Machine : String := "")
   is
      Descriptor : Process_Descriptor_Access;
   begin
      if Use_Ptys then
         Descriptor := new TTY_Process_Descriptor;
      else
         Descriptor := new Process_Descriptor;
      end if;

      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process := Proxy;

      if Remote_Machine = "" then
         if Locate_Exec_On_Path (Debugger_Name) /= null then
            Non_Blocking_Spawn
              (Descriptor.all, Debugger_Name, Arguments,
               Buffer_Size => 0,
               Err_To_Out => True);
         else
            declare
               Buttons : Message_Dialog_Buttons;
            begin
               Buttons :=
                 Message_Dialog
                   (-("Could not find executable ") &
                      '"' & Debugger_Name & '"' & (-" in path."),
                    Error,
                    Button_OK,
                    Button_OK);
               OS_Exit (1);
            end;
         end if;
      else
         declare
            Real_Arguments : Argument_List (1 .. Arguments'Length + 2);
         begin
            Real_Arguments (1) := new String'(Remote_Machine);
            Real_Arguments (2) := new String'(Debugger_Name);
            Real_Arguments (3 .. Real_Arguments'Last) := Arguments;

            Non_Blocking_Spawn
              (Descriptor.all,
               Get_Pref (Remote_Protocol),
               Real_Arguments,
               Buffer_Size => 0,
               Err_To_Out => True);
            Free (Real_Arguments (1));
            Free (Real_Arguments (2));
         exception
            when Invalid_Process =>
               declare
                  Buttons : Message_Dialog_Buttons;
               begin
                  Buttons := Message_Dialog
                    ((-"Could not spawn the remote process: ") & ASCII.LF
                     & (-"  debugger: ") & Debugger_Name & ASCII.LF
                     & (-"  machine: ") & Remote_Machine & ASCII.LF
                     & (-"  using protocol: ")
                     & Get_Pref (Remote_Protocol),
                     Error,
                     Button_OK,
                     Button_OK);
                  OS_Exit (1);
               end;
         end;
      end if;

      if Get_Pid (Descriptor.all) = GNAT.Expect.Invalid_Pid then
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
      Addr_Last   : out Natural) is
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
      First, Last : out Natural) is
   begin
      First := 0;
      Last  := 0;
   end Found_Frame_Info;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   function Get_Uniq_Id
     (Debugger : access Debugger_Root;
      Entity   : String) return String is
   begin
      return Entity;
   end Get_Uniq_Id;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Debugger_Root) return String_Array
   is
      A : String_Array (1 .. 0);
   begin
      return A;
   end Source_Files_List;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Process   : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Debugger : constant Debugger_Access := Process.Debugger;
   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if Wait_Prompt (Debugger, Timeout => 0) then
         Gdk.Input.Remove (Process.Input_Id);
         Process.Input_Id := 0;

         --  Put back the standard cursor

         if Get_Command_Mode (Get_Process (Debugger)) >= Visible then
            Set_Busy_Cursor (Process, False);
         end if;

         Set_Command_In_Process (Get_Process (Debugger), False);
         Debugger.Processing_User_Command := False;
         Unregister_Dialog (Process);

         --  Do the postprocessing here instead of calling Send_Internal_Post
         --  since we need to handle post processing slightly differently

         declare
            Current_Command : constant String := Process.Current_Command.all;
            Result          : Boolean;
            Pos             : Positive;
         begin
            Free (Process.Current_Command);

            if Process_Command (Debugger) then
               --  ??? register if needed for
               --  executable_changed/context_changed/process_stopped
               --  before returning
               return;
            end if;

            Set_Command_In_Process (Get_Process (Debugger));
            Final_Post_Process (Process);

            if Is_Load_Command (Debugger, Current_Command) then
               Pos := Current_Command'First;
               Skip_To_Blank (Current_Command, Pos);
               Skip_Blanks (Current_Command, Pos);

               declare
                  pragma Suppress (Index_Check);
                  pragma Suppress (Range_Check);
                  --  ??? work around a bug in GNAT 3.14
               begin
                  Executable_Changed
                    (Process, Current_Command (Pos .. Current_Command'Last));
               end;

            elsif Is_Context_Command (Debugger, Current_Command) then
               Context_Changed (Process);
            elsif Is_Execution_Command (Debugger, Current_Command) then
               Process_Stopped (Process);
            end if;

            Update_Breakpoints
              (Process,
               Force => Is_Break_Command (Debugger, Current_Command));
            Set_Command_In_Process (Get_Process (Debugger), False);

            --  In case a command has been queued while handling the signals
            --  and breakpoints above.

            Result := Process_Command (Debugger);
         end;
      end if;

   exception
      when Process_Died =>
         --  Will close the debugger in GVD.Process when getting this
         --  exception the next time.

         Gdk.Input.Remove (Process.Input_Id);
         Process.Input_Id := 0;
         Debugger.Processing_User_Command := False;
         Set_Command_In_Process (Get_Process (Debugger), False);
         Set_Busy_Cursor (Process, False);
         Free (Process.Current_Command);
         Unregister_Dialog (Process);
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
      use type Gtk.Window.Gtk_Window;
      Data    : History_Data;
      Process : Debugger_Process_Tab;

   begin
      Debugger.Processing_User_Command := True;
      Set_Command_In_Process (Get_Process (Debugger));
      Set_Command_Mode (Get_Process (Debugger), Mode);

      if Debugger.Window /= null then
         Process := Convert (Debugger.Window, Debugger);

         if Mode >= Visible then
            Set_Busy_Cursor (Process);
         end if;

         --  Display the command in the output window if necessary

         if Mode = Visible then
            Output_Text (Process, Cmd & ASCII.LF, True);
         end if;
      end if;

      --  Append the command to the history if necessary
      --  ??? Will only work if Debugger is the current page in the notebook

      if Index_Non_Blank (Cmd) /= 0
        and then Debugger.Window /= null
        and then Mode /= Internal
      then
         Data.Mode := Mode;
         Data.Debugger_Num := Integer (Get_Num (Process));
         Data.Command := new String'
           (Cmd (Index_Non_Blank (Cmd) .. Index_Non_Blank (Cmd, Backward)));
         Append (Process.Window.Command_History, Data);
      end if;

      --  Send the command to the debugger

      Send (Get_Process (Debugger), Cmd, Empty_Buffer);
   end Send_Internal_Pre;

   ------------------------
   -- Send_Internal_Post --
   ------------------------

   procedure Send_Internal_Post
     (Debugger : access Debugger_Root'Class;
      Cmd      : String;
      Mode     : Command_Type)
   is
      Process : Debugger_Process_Tab;
      Result  : Boolean;

   begin
      --  See also Output_Available for similar handling.

      Debugger.Processing_User_Command := False;
      Set_Command_In_Process (Get_Process (Debugger), False);

      if Mode /= Internal and then Process_Command (Debugger) then
         --  ??? register if needed for
         --  executable_changed/context_changed/process_stopped
         --  before returning
         return;
      end if;

      if Debugger.Window /= null then
         Set_Command_In_Process (Get_Process (Debugger));
         Process := Convert (Debugger.Window, Debugger);
         Final_Post_Process (Process);

         if Mode /= Internal then
            --  Postprocessing (e.g handling of auto-update).

            if Is_Context_Command (Debugger, Cmd) then
               Context_Changed (Process);
            elsif Is_Execution_Command (Debugger, Cmd) then
               Process_Stopped (Process);
            end if;

            Update_Breakpoints
              (Process, Force => Is_Break_Command (Debugger, Cmd));
         end if;

         Set_Command_In_Process (Get_Process (Debugger), False);

         if Mode >= Visible then
            Set_Busy_Cursor (Process, False);
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
     (Debugger        : access Debugger_Root'Class;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : Command_Type := Hidden)
   is
      Process : Debugger_Process_Tab;
      Button  : Message_Dialog_Buttons;
      Last    : Positive := Cmd'First;
      First   : Positive;

   begin
      loop
         --  Each command is separated with a ASCII.LF and is handled
         --  separately.

         First := Last;
         Skip_To_Char (Cmd, Last, ASCII.LF);

         if Mode not in Invisible_Command
           and then Wait_For_Prompt
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
                     Wait_Prompt (Debugger);
                     Send_Internal_Post
                       (Debugger, Cmd (First .. Last - 1), Mode);
                  end if;

               when Visible_Command =>
                  if Wait_For_Prompt then
                     if not Async_Commands then
                        --  Synchronous handling of commands, simple case

                        Wait_Prompt (Debugger);
                        Send_Internal_Post
                          (Debugger, Cmd (First .. Last - 1), Mode);

                     else
                        --  Asynchronous handling of commands, install a
                        --  callback on the debugger's output file descriptor.

                        Process := Convert (Debugger.Window, Debugger);
                        Process.Current_Command :=
                          new String' (Cmd (First .. Last - 1));

                        pragma Assert (Process.Input_Id = 0);

                        Process.Input_Id := My_Input.Add
                          (To_Gint
                           (Get_Output_Fd
                            (Get_Descriptor (Get_Process (Debugger)).all)),
                           Gdk.Types.Input_Read,
                           Output_Available'Access,
                           My_Input.Data_Access (Process));
                     end if;
                  end if;
            end case;
         end if;

         exit when Last > Cmd'Last;

         Last := Last + 1;
      end loop;

   exception
      when Process_Died =>
         Process := Convert (Debugger.Window, Debugger);

         if Process.Exiting then
            return;
         end if;

         Button := Message_Dialog
           (-"The underlying debugger died unexpectedly. Closing it",
            Error, Button_OK);
         Debugger.Processing_User_Command := False;
         Set_Command_In_Process (Get_Process (Debugger), False);
         Set_Busy_Cursor (Process, False);
         Unregister_Dialog (Process);
         Close_Debugger (Process);
   end Send;

   ---------------
   -- Send_Full --
   ---------------

   function Send_Full
     (Debugger     : access Debugger_Root'Class;
      Cmd          : String;
      Mode         : Invisible_Command := Hidden) return String
   is
      Process : Debugger_Process_Tab;
      Main    : constant GVD_Main_Window := GVD_Main_Window (Debugger.Window);

   begin
      if Debugger.Processing_User_Command then
         Wait_User_Command (Debugger);
      end if;

      if Main.Locked then
         Output_Error (Main, "Internal inconsistency: recursing in Send_Full");
         return "";
      end if;

      Main.Locked := True;

      Send_Internal_Pre (Debugger, Cmd, Mode => Mode);
      Wait_Prompt (Debugger);

      declare
         S : constant String := Expect_Out (Get_Process (Debugger));
      begin
         Main.Locked := False;
         Send_Internal_Post (Debugger, Cmd, Mode);

         if Need_To_Strip_CR then
            return Strip_CR (S);
         else
            return S;
         end if;
      end;

   exception
      when Process_Died =>
         Main.Locked := False;
         Process := Convert (Debugger.Window, Debugger);
         Debugger.Processing_User_Command := False;
         Set_Command_In_Process (Get_Process (Debugger), False);
         Set_Busy_Cursor (Process, False);
         Unregister_Dialog (Process);
         Close_Debugger (Process);
         return "";
   end Send_Full;

   ------------------------------
   -- Variable_Name_With_Frame --
   ------------------------------

   function Variable_Name_With_Frame
     (Debugger : access Debugger_Root;
      Var      : String) return String is
   begin
      return Var;
   end Variable_Name_With_Frame;

   ---------------------
   -- List_Exceptions --
   ---------------------

   function List_Exceptions
     (Debugger : access Debugger_Root) return Exception_Array
   is
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
      Default   : String) return String is
   begin
      return Default;
   end Get_Type_Info;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Debugger : access Debugger_Root; File_Name : String) return String is
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
      Tmp        : Boolean;
      Num_Events : Positive;
      Max_Events : constant := 30;
      --  Limit the number of events to process in one iteration

      Current_Process : Process_Proxy_Access;
   begin
      --  Wait until the command has been processed

      Current_Process := Get_Process (Debugger);
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
        (Cmd             => new String' (Cmd),
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
   begin
      if Command = null then
         return False;
      end if;

      Debugger.Command_Queue := Command.Next;
      Send
        (Debugger, Command.Cmd.all, Command.Empty_Buffer,
         Mode => Command.Mode);
      Free (Command.Cmd);
      Free (Command);
      return True;
   end Process_Command;

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
      if Debugger.Remote_Host = null then
         Open_Processes (Debugger.Handle);
      else
         Open_Processes (Debugger.Handle, Debugger.Remote_Host.all);
      end if;
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

end Debugger;

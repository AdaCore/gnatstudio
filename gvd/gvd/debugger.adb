-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Ada.Text_IO;       use Ada.Text_IO;
with Unchecked_Conversion;

with Glib;              use Glib;
with Gtk.Window;        use Gtk.Window;
with Gdk.Input;
with Gdk.Types;

with GVD;               use GVD;
with Items;             use Items;
with Process_Proxies;   use Process_Proxies;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with GVD.Strings;       use GVD.Strings;
with GVD.Types;         use GVD.Types;
with GVD.Process;       use GVD.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with GVD.Preferences;   use GVD.Preferences;

with Gtkada.Dialogs;     use Gtkada.Dialogs;

package body Debugger is

   use String_History;

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

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
      Type_Str : String  := Type_Of (Debugger, Entity);
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
               Buttons :  Message_Dialog_Buttons;
            begin
               Buttons := Message_Dialog ("GVD could not find executable "
                                          & '"' & Debugger_Name & '"'
                                          & " in path.",
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
               Remote_Protocol,
               Real_Arguments,
               Buffer_Size => 0,
               Err_To_Out => True);
            Free (Real_Arguments (1));
            Free (Real_Arguments (2));
         exception
            when Invalid_Process =>
               declare
                  Buttons :  Message_Dialog_Buttons;
               begin
                  Buttons := Message_Dialog
                    ("GVD could not spawn the remote process : " & ASCII.LF
                     & "  debugger : " & Debugger_Name & ASCII.LF
                     & "  machine : " & Remote_Machine & ASCII.LF
                     & "  using protocol : " & Remote_Protocol,
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

   -------------------
   -- Thread_Switch --
   -------------------

   procedure Thread_Switch
     (Debugger : access Debugger_Root'Class;
      Thread   : Natural;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, Thread_Switch (Get_Language (Debugger), Thread),
            Mode => Mode);
   end Thread_Switch;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Debugger_Root) return GVD.Types.String_Array
   is
      A : GVD.Types.String_Array (1 .. 0);
   begin
      return A;
   end Source_Files_List;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Process   : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition) is
   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if Wait_Prompt (Process.Debugger, Timeout => 0) then
         Gdk.Input.Remove (Process.Input_Id);
         Process.Input_Id := 0;

         --  Put back the standard cursor

         Set_Command_In_Process (Get_Process (Process.Debugger), False);
         Set_Busy_Cursor (Process, False);
         Unregister_Dialog (Process);

         --  Do the postprocessing here instead of calling Send_Internal_Post
         --  since we need to handle post processing slightly differently

         Process_Post_Processes (Get_Process (Process.Debugger));

         if Is_Context_Command
           (Process.Debugger, Process.Current_Command.all)
         then
            Context_Changed (Process);
         elsif Is_Execution_Command
           (Process.Debugger, Process.Current_Command.all)
         then
            Process_Stopped (Process);
         end if;

         Update_Breakpoints
           (Process,
            Force =>
              Is_Break_Command
                (Process.Debugger, Process.Current_Command.all));
         Free (Process.Current_Command);
      end if;
   end Output_Available;

   -----------------------
   -- Send_Internal_Pre --
   -----------------------

   procedure Send_Internal_Pre
     (Debugger         : access Debugger_Root'Class;
      Cmd              : String;
      Empty_Buffer     : Boolean := True;
      Mode             : Command_Type)
   is
      use type Gtk.Window.Gtk_Window;
      Data    : History_Data;
      Process : Debugger_Process_Tab;

   begin
      Set_Command_In_Process (Get_Process (Debugger));

      if Mode >= Visible then
         Set_Busy_Cursor (Process);
      end if;

      Set_Command_Mode (Get_Process (Debugger), Mode);

      if Debugger.Window /= null then
         Process := Convert (Debugger.Window, Debugger);
         Send_Init (Process);

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
      Send_Completed (Debugger, Cmd);
   end Send_Internal_Pre;

   ------------------------
   -- Send_Internal_Post --
   ------------------------

   procedure Send_Internal_Post
     (Debugger         : access Debugger_Root'Class;
      Cmd              : String;
      Mode             : Command_Type)
   is
      Process : Debugger_Process_Tab;
   begin
      Set_Command_In_Process (Get_Process (Debugger), False);
      Process_Post_Processes (Get_Process (Debugger));

      --  Not an internal command, not in text mode (for testing purposes...)

      if Debugger.Window /= null then
         Process := Convert (Debugger.Window, Debugger);

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

         Set_Busy_Cursor (Process, False);
         Unregister_Dialog (Process);
      end if;
   end Send_Internal_Post;

   ----------
   -- Send --
   ----------

   procedure Send
     (Debugger         : access Debugger_Root'Class;
      Cmd              : String;
      Empty_Buffer     : Boolean := True;
      Wait_For_Prompt  : Boolean := True;
      Mode             : Command_Type := Hidden)
   is
      Process : Debugger_Process_Tab;
      Last    : Positive := Cmd'First;
      First   : Positive;
   begin
      --  ???Note: Handling of several commands inside the same string,
      --  separated by ASCII.LF is just a temporary workaround for the
      --  "start" command in C mode. This should be removed once we have
      --  the command queue working.

      case Mode is
         when Invisible_Command =>
         --  Handle global lock
         --  if Command_In_Process (Convert (Debugger.Window, Debugger)) then
         --     return;
         --  end if;

            --  ??? Need to queue the command instead
            if Wait_For_Prompt
              and then Command_In_Process (Get_Process (Debugger))
            then
               return;
            end if;

            while Last <= Cmd'Last loop
               First := Last;
               Skip_To_Char (Cmd, Last, ASCII.LF);

               Send_Internal_Pre
                 (Debugger, Cmd (First .. Last - 1), Empty_Buffer, Mode);

               --  All commands, except possibly the last, must wait for the
               --  prompt

               if Wait_For_Prompt or else Last - 1 /= Cmd'Last then
                  Wait_Prompt (Debugger);
                  Send_Internal_Post (Debugger, Cmd (First .. Last - 1), Mode);
               end if;

               Last := Last + 1;
            end loop;

         when Visible_Command =>
            --  ??? Need to queue the command instead
            if Wait_For_Prompt
              and then Command_In_Process (Get_Process (Debugger))
            then
               return;
            end if;

            while Last <= Cmd'Last loop
               First := Last;
               Skip_To_Char (Cmd, Last, ASCII.LF);

               Send_Internal_Pre
                 (Debugger, Cmd (First .. Last - 1), Empty_Buffer, Mode);

               --  All commands, except the last, are synchronous, and must
               --  wait for the prompt

               if not Async_Commands or else Last - 1 /= Cmd'Last then
                  Wait_Prompt (Debugger);
                  Send_Internal_Post (Debugger, Cmd (First .. Last - 1), Mode);

               elsif Wait_For_Prompt then
                  Process := Convert (Debugger.Window, Debugger);
                  Process.Current_Command := new String'
                    (Cmd (First .. Last - 1));
                  Process.Input_Id := My_Input.Add
                    (To_Gint
                     (Get_Output_Fd
                      (Get_Descriptor (Get_Process (Debugger)).all)),
                     Gdk.Types.Input_Read,
                     Output_Available'Access,
                     My_Input.Data_Access (Process));
               end if;

               Last := Last + 1;
            end loop;
      end case;
   end Send;

   ---------------
   -- Send_Full --
   ---------------

   function Send_Full
     (Debugger        : access Debugger_Root'Class;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : Invisible_Command := Hidden) return String is
   begin
      --  ??? We should always avoid concurrent calls to Wait, or the exact
      --  behavior of the application will depend on specific timing, which is
      --  not reliable.

      if Command_In_Process (Get_Process (Debugger)) then
         Put_Line ("!!! already running a Wait command!!");
      end if;

      --  Block if the global lock is set
      --  if Command_In_Process (Convert (Debugger.Window, Debugger)) then
      --     return;
      --  end if;

      Send_Internal_Pre (Debugger, Cmd, Empty_Buffer, Mode);

      if Wait_For_Prompt then
         Wait_Prompt (Debugger);

         declare
            S : String := Expect_Out (Get_Process (Debugger));
         begin
            Send_Internal_Post (Debugger, Cmd, Mode);

            if Need_To_Strip_Control_M then
               return Strip_Control_M (S);
            else
               return S;
            end if;
         end;
      end if;

      return "";
   end Send_Full;

   --------------------
   -- Send_Completed --
   --------------------

   procedure Send_Completed
     (Debugger : access Debugger_Root;
      Cmd      : String) is
   begin
      null;
   end Send_Completed;

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
     (Debugger : access Debugger_Root) return GVD.Types.Exception_Array
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

end Debugger;

------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;                     use System;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Strings;
with GNAT.TTY;                   use GNAT.TTY;

with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;         use GNATCOLL.VFS_Utils;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;

with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtk;                        use Gtk;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.MDI;                 use Gtkada.MDI;

with Commands;                   use Commands;
with Config;                     use Config;
with Debugger.Base_Gdb.Gdb_CLI;  use Debugger.Base_Gdb.Gdb_CLI;
with Debugger.Base_Gdb.Gdb_MI;   use Debugger.Base_Gdb.Gdb_MI;
with Debugger.LLDB;              use Debugger.LLDB;
with Default_Preferences;        use Default_Preferences;
with GPS.Intl;                   use GPS.Intl;
with GPS.Core_Kernels;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;    use GPS.Kernel.Task_Manager;
with GPS.Kernel.Remote;
with GPS.Main_Window;            use GPS.Main_Window;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Consoles;               use GVD.Consoles;
with GVD.Preferences;            use GVD.Preferences;
with GVD.Types;                  use GVD.Types;
with GVD_Module;                 use GVD_Module;
with GVD.Registers_View;
with GUI_Utils;                  use GUI_Utils;
with Language_Handlers;          use Language_Handlers;
with Process_Proxies;            use Process_Proxies;
with Projects;                   use Projects;
with Remote;                     use Remote;
with Toolchains;                 use Toolchains;
with Toolchains_Old;             use Toolchains_Old;

package body GVD.Process is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GVD_PROCESS");

   type GPS_Proxy is new Process_Proxy with record
      Process : Visual_Debugger;
   end record;
   --  GNAT Studio specific proxy, used to redefine Set_Command_In_Process

   overriding procedure Set_Command_In_Process
     (Proxy : access GPS_Proxy; In_Process : Boolean := True);
   --  Set the appropriate debugger menu items to the corresponding state

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function To_Main_Debug_Window is new
     Ada.Unchecked_Conversion (System.Address, GPS_Window);
   pragma Warnings (On);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   Null_TTY : GNAT.TTY.TTY_Handle;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's output to the debugger window.
   --  Simply strip CR characters if needed and then call Text_Output_Filter

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Real handler called by First_Text_Output_Filter

   type On_Before_Exit is new Return_Boolean_Hooks_Function with record
      Process : access Visual_Debugger_Record'Class;
   end record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean;
   --  Called before exiting

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class);
   --  Internal initialize procedure

   procedure On_Console_Destroy
     (Process : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Called when the debugger console is destroyed, which also terminates the
   --  debugger itself

   type String_Access_Access is access all GNAT.Strings.String_Access;

   procedure Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type;
      Output         : String_Access_Access);
   --  Wrapper implementing common code for Process_User_Command routines

   ------------------------------
   -- Create_Execution_Console --
   ------------------------------

   procedure Create_Execution_Console
     (Process : access Visual_Debugger_Record'Class) is
   begin
      Attach_To_Debuggee_Console
        (Process,
         Process.Kernel,
         Create_If_Necessary =>
           Execution_Window.Get_Pref
         and then Is_Local (Debug_Server)
         and then Support_TTY (Process.Debugger)
         and then GNAT.TTY.TTY_Supported);
   end Create_Execution_Console;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Process : access Visual_Debugger_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Process.Kernel;
   end Get_Kernel;

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   overriding procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True) is
   begin
      Set_Command_In_Process (Process_Proxy (Proxy.all)'Access, In_Process);
      Proxy.Process.Kernel.Refresh_Context;

      if In_Process then
         Debugger_State_Changed_Hook.Run
           (Proxy.Process.Kernel, Proxy.Process, Debug_Busy);
      else
         Debugger_State_Changed_Hook.Run
           (Proxy.Process.Kernel, Proxy.Process, Debug_Available);
      end if;

      Update_Menus_And_Buttons (Get_Kernel (Proxy.Process));
   end Set_Command_In_Process;

   -----------------------
   -- Add_Regexp_Filter --
   -----------------------

   procedure Add_Regexp_Filter
     (Process : access Visual_Debugger_Record'Class;
      Filter  : Regexp_Filter_Function;
      Regexp  : Pattern_Matcher) is
   begin
      Process.Filters :=
        new Regexp_Filter_List_Elem'
          (Filter => Filter,
           Regexp => new Pattern_Matcher'(Regexp),
           Next   => Process.Filters);
   end Add_Regexp_Filter;

   -------------
   -- Convert --
   -------------

   function Convert
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger
   is
      procedure Callback
        (Object : not null access Base_Visual_Debugger'Class);

      Result : Visual_Debugger;

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Object : not null access Base_Visual_Debugger'Class)
      is
         Process : constant Visual_Debugger := Visual_Debugger (Object);
      begin
         if Process.Debugger /= null then
            --  Note: The process might have been already killed when this
            --  function is called.

            if Get_Descriptor
              (Get_Process (Process.Debugger)).all = Descriptor
            then
               Result := Process;
            end if;
         end if;
      end Callback;

   begin
      For_Each_Debugger (Kernel, Callback'Access);

      return Result;

   exception
      when Constraint_Error =>
         return null;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Debugger   : access Debugger_Root'Class)
      return Visual_Debugger is
   begin
      return Convert
        (Get_Kernel (Debugger), Get_Descriptor (Get_Process (Debugger)).all);
   end Convert;

   ------------------------
   -- Command_In_Process --
   ------------------------

   overriding function Command_In_Process
     (Self : not null access Visual_Debugger_Record) return Boolean is
   begin
      return Self.Debugger /= null
        and then Get_Process (Self.Debugger) /= null
        and then Command_In_Process (Get_Process (Self.Debugger));
   end Command_In_Process;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Process : access Visual_Debugger_Record'Class) return String is
   begin
      if Process.Current_Command = null then
         return "";
      else
         return Process.Current_Command.all;
      end if;
   end Get_Command;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   function Is_Execution_Command
     (Process : access Visual_Debugger_Record'Class) return Boolean is
   begin
      if Process.Current_Command = null then
         return False;
      else
         return Command_Kind
           (Process.Debugger, Process.Current_Command.all) = Execution_Command;
      end if;
   end Is_Execution_Command;

   -----------------
   -- Output_Text --
   -----------------

   procedure Output_Text
     (Process      : not null access Visual_Debugger_Record'Class;
      Str          : String;
      Is_Command   : Boolean := False;
      Set_Position : Boolean := False)
   is
      pragma Unreferenced (Set_Position);
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;
      Console_Child : MDI_Child;
   begin
      if Process.Debugger_Text /= null then
         if Is_Command then
            Display_In_Debugger_Console
              (Process        => Process,
               Text           => Str,
               Mode           => GPS.Kernel.Verbose,
               Add_To_History => True);
         else
            while Start <= Str'Last loop
               Match (Highlighting_Pattern (Process.Debugger),
                      Str (Start .. Str'Last),
                      Matched);

               if Matched (0) /= No_Match then
                  if Matched (0).First - 1 >= Start then
                     Display_In_Debugger_Console
                       (Process, Str (Start .. Matched (0).First - 1));
                  end if;

                  Display_In_Debugger_Console
                    (Process => Process,
                     Text    => Str (Matched (0).First .. Matched (0).Last),
                     Mode    => GPS.Kernel.Verbose);
                  Start := Matched (0).Last + 1;

               else
                  Display_In_Debugger_Console
                    (Process, Str (Start .. Str'Last));
                  Start := Str'Last + 1;
               end if;
            end loop;
         end if;

         Console_Child := Find_MDI_Child
           (Get_MDI (Process.Kernel), Process.Debugger_Text);

         if Console_Child /= null then
            Highlight_Child (Console_Child);
         end if;
      end if;
   end Output_Text;

   ------------------------
   -- Final_Post_Process --
   ------------------------

   procedure Final_Post_Process
     (Process           : not null access Visual_Debugger_Record'Class;
      Mode              : GVD.Types.Command_Type;
      Always_Emit_Hooks : Boolean;
      Category          : Command_Category;
      Breakpoints_Might_Have_Changed : Boolean;
      Register_Changed  : Boolean)
   is
      File : Unbounded_String;
      Line : Natural := 0;
      Addr : Address_Type;

   begin
      if Process.Post_Processing or else Process.Current_Output = null then
         return;
      end if;

      Process.Post_Processing := True;

      if Process.Debugger.Get_Process.Get_Parse_File_Name then
         Process.Debugger.Found_File_Name
           (Process.Current_Output
              (Process.Current_Output'First .. Process.Current_Output_Pos - 1),
            File, Line, Addr);

         if Addr /= Invalid_Address then
            Process.Pc := Addr;
         end if;
      end if;

      --  Do we have a file name or line number indication?
      --  This runs the Debugger_Location_Changed_Hook hook

      if Length (File) /= 0
        and then Line /= 0
      then
         Set_Current_File_And_Line
           (Kernel  => Process.Kernel,
            Process => Base_Visual_Debugger_Access (Process),
            File    =>
              GPS.Core_Kernels.To_File (Process.Kernel, To_String (File)),
            Line    => Line,
            Focus   => False);

      elsif Addr /= Invalid_Address then
         --  the address without debugging information
         Set_Current_File_And_Line
           (Kernel  => Process.Kernel,
            Process => Base_Visual_Debugger_Access (Process),
            File    => GNATCOLL.VFS.No_File,
            Line    => 0,
            Focus   => False);

         Process.Kernel.Messages_Window.Insert
           (-"There is no debug information for this frame.");
      end if;

      Process.Post_Processing := False;
      Free (Process.Current_Output);
      Process.Is_From_Dbg_Console := False;

      if Always_Emit_Hooks or else Mode /= Internal then
         if Breakpoints_Might_Have_Changed
           or else Process.Breakpoints.Has_Temporary_Breakpoint
         then
            Refresh_Breakpoints_List (Process.Kernel, Process);
         end if;

         case Category is
            when Load_Command =>
               Debugger_Executable_Changed_Hook.Run (Process.Kernel, Process);

            when Context_Command =>
               Debugger_Context_Changed_Hook.Run (Process.Kernel, Process);

            when Frame_Command =>
               Debugger_Frame_Changed_Hook.Run (Process.Kernel, Process);

            when Execution_Command =>
               Debugger_Process_Stopped_Hook.Run (Process.Kernel, Process);

            when Misc_Command =>
               null;
         end case;
      end if;

      if Register_Changed then
         GVD.Registers_View.Refresh_Registers (Process.Kernel, Process);
      end if;
   end Final_Post_Process;

   ------------------------------
   -- First_Text_Output_Filter --
   ------------------------------

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address) is
   begin
      --  Strip CRs in remote mode, as we can't know in advance if the debug
      --  server outputs CR/LF or just LF, and the consequences or removing
      --  CRs in the latter case are better than not removing them in the
      --  first case
      if Need_To_Strip_CR or else not Is_Local (Debug_Server) then
         Text_Output_Filter (Descriptor, Strip_CR (Str), Window);
      else
         Text_Output_Filter (Descriptor, Str, Window);
      end if;
   end First_Text_Output_Filter;

   ------------------------
   -- Text_Output_Filter --
   ------------------------

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address)
   is
      Process         : constant Visual_Debugger :=
        Convert
          (To_Main_Debug_Window (Window).Kernel, Descriptor);
      Tmp_Str         : GNAT.Strings.String_Access;
      Current_Filter  : Regexp_Filter_List;
      Matched         : Match_Array (0 .. Max_Paren_Count);
      Last_Match      : Natural := 0;
      Offset          : Natural := 0;
      --  Offset from the start of the buffer to start the matching
      New_Offset      : Natural := 0;
      Min_Size        : Natural;
      New_Size        : Natural;
      Str_Match       : Boolean;
      Console_Output  : Unbounded_String;
      Log_Output      : Unbounded_String;
      Debuggee_Output : Unbounded_String;
      Results_Output  : Unbounded_String;
      Mode            : GVD.Types.Command_Type;
   begin
      --  Replace current output

      if Process.Current_Output = null then
         Process.Current_Output := new String (1 .. 1024);
         Process.Current_Output_Pos := 1;
         Process.Last_Match := 0;
      end if;

      Min_Size := Process.Current_Output_Pos + Str'Length;

      if Process.Current_Output'Last < Min_Size then
         New_Size := Process.Current_Output'Length * 2;

         while New_Size < Min_Size loop
            New_Size := New_Size * 2;
         end loop;

         Tmp_Str := new String (1 .. New_Size);
         Tmp_Str (1 .. Process.Current_Output_Pos - 1) :=
           Process.Current_Output (1 .. Process.Current_Output_Pos - 1);
         Free (Process.Current_Output);
         Process.Current_Output := Tmp_Str;
      end if;

      Process.Current_Output
        (Process.Current_Output_Pos ..
         Process.Current_Output_Pos + Str'Length - 1) := Str;
      Process.Current_Output_Pos := Process.Current_Output_Pos + Str'Length;

      --  Process the filters. Each filter is tested until there is no more
      --  match at which point we move to the next one.

      Current_Filter := Process.Filters;
      Last_Match := Process.Last_Match;

      while Current_Filter /= null loop
         New_Offset := Offset;

         Match
           (Current_Filter.Regexp.all,
            Process.Current_Output
              (Process.Last_Match + 1 + Offset ..
                 Process.Current_Output_Pos - 1),
            Matched);
         Str_Match := False;

         if Matched (0) = No_Match then
            --  Try with just the input (Str)
            Str_Match := True;
            Match
              (Current_Filter.Regexp.all,
               Process.Current_Output
                 (Process.Current_Output_Pos - Str'Length + Offset ..
                    Process.Current_Output_Pos - 1),
               Matched);

            if Matched (0) /= No_Match then
               --  Match found, record offset for next one. We cannot change
               --  Offset here as we need it later when calling the filters.
               New_Offset := Matched (0).Last + 1 -
                 (Process.Current_Output_Pos - Str'Length);
            end if;

         else
            --  Match found (see comment above)
            New_Offset := Matched (0).Last + 1 - (Process.Last_Match + 1);
         end if;

         if Matched (0) = No_Match then
            --  No more match for this filter, move to next one
            Current_Filter := Current_Filter.Next;

            --  Reset New_Offset (and Offset in the next iteration) to the
            --  start of the buffer.
            New_Offset := 0;

         else
            if Matched (0).Last > Last_Match then
               Last_Match := Matched (0).Last;
            end if;

            if Str_Match then
               Current_Filter.Filter
                 (Process,
                  Process.Current_Output
                    (Process.Current_Output_Pos - Str'Length + Offset ..
                       Process.Current_Output_Pos - 1),
                  Matched);
            else
               Current_Filter.Filter
                 (Process,
                  Process.Current_Output
                    (Process.Last_Match + 1 + Offset ..
                       Process.Current_Output_Pos - 1),
                  Matched);
            end if;
         end if;

         --  Now we can set the offset for next iteration
         Offset := New_Offset;
      end loop;

      if Last_Match /= 0 then
         Process.Last_Match := Last_Match;
      end if;

      --  Do not show the output if we have an internal or hidden command

      Mode := Get_Command_Mode (Get_Process (Process.Debugger));

      if not Debugger_Console_All_Interactions.Get_Pref
        and then (Mode = User or else Mode = GVD.Types.Visible)
      then
         Filter_Output
           (Process.Debugger,
            Mode            => Mode,
            Str             => Str,
            Console_Output  => Console_Output,
            Log_Output      => Log_Output,
            Debuggee_Output => Debuggee_Output,
            Results_Output  => Results_Output);

         if Length (Console_Output) > 0 then
            Output_Text
              (Process, To_String (Console_Output), Set_Position => True);
         end if;

         if Length (Debuggee_Output) > 0 then
            Display_In_Debuggee_Console
              (Process => Process,
               Text    => To_String (Debuggee_Output));
         end if;
      end if;
   end Text_Output_Filter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class) is
   begin
      Initialize (Process);
      Ref (Process);
      Process.Kernel := Window.Kernel;

      Set_Current_Debugger
        (Window.Kernel, Base_Visual_Debugger_Access (Process));
      Add_Debugger (Window.Kernel, Process);
   end Initialize;

   ------------------------
   -- On_Console_Destroy --
   ------------------------

   procedure On_Console_Destroy
     (Process : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
      Proc : constant Visual_Debugger := Visual_Debugger (Process);
   begin
      Proc.Debugger_Text := null;
      Close_Debugger (Proc);
   end On_Console_Destroy;

   --------------------
   -- Close_Debugger --
   --------------------

   procedure Close_Debugger
     (Process  : access Visual_Debugger_Record;
      Has_Died : Boolean := False)
   is
      Kernel : constant Kernel_Handle := Process.Kernel;
      Count  : Natural;
   begin
      if Process.Exiting then
         return;
      end if;

      GNATCOLL.Traces.Trace (Me, "Closing Debugger");

      Process.Exiting := True;
      Count := Count_Running_Debuggers (Kernel);

      --  Load the default perspective before closing the debugger. This is
      --  necessary because the position of views is memorized at this time:
      --  if views are closed before the perspective change, their position
      --  is lost.

      if Count = 1 then
         Load_Perspective (Kernel, "Default");
      end if;

      --  Interrupt the monitoring of debugger output

      Get_Task_Manager (Kernel).Interrupt_Queue
        (Debug_Queue_Name & Process.Debugger_Num'Img);

      --  Let all views know that they should close

      Debugger_State_Changed_Hook.Run (Process.Kernel, Process, Debug_None);
      Debugger_Terminated_Hook.Run (Process.Kernel, Process);

      Unhighlight_Current_Line (Process.Kernel);

      Unregister_Dialog (Process);
      Free (Process.Command_History);

      --  Close the underlying debugger

      if not Has_Died
        and then Process.Debugger /= null
        and then Get_Process (Process.Debugger) /= null
      then
         begin
            Close (Process.Debugger);
         exception
            when E : others =>
               Trace (Me, E);
         end;
      end if;

      Process.Debugger := null;
      Remove_Debugger (Kernel, Process);
      if Process.Debuggee_TTY /= Null_TTY then
         Close_TTY (Process.Debuggee_TTY);
      end if;
      Unref (Process);

      if Count = 1 then
         Debug_Terminate (Kernel);
      end if;
   end Close_Debugger;

   ---------------
   -- Interrupt --
   ----------------

   procedure Interrupt
     (Process            : access Visual_Debugger_Record;
      Display_In_Console : Boolean := True) is
   begin
      --  Give some visual feedback to the user

      if Display_In_Console then
         Process.Output_Text ("<^C>" & ASCII.LF, Is_Command => True);
      end if;

      Process.Unregister_Dialog;

      --  Need to flush the queue of commands
      Process.Debugger.Clear_Queue;

      Process.Debugger.Interrupt;

      if not Process.Debugger.Get_Process.Command_In_Process then
         Process.Debugger.Display_Prompt;
      end if;

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.

      --  Note that doing anything at this point is very unsafe, since we got
      --  called while handling a command, and this command has not been fully
      --  handled yet, so we cannot reliably send new commands to the debugger
      --  without creating a synchronization problem. Also, we should be able
      --  to clean up properly the current command, which is particularly
      --  tricky when handling an internal command.
   end Interrupt;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type;
      Output         : String_Access_Access)
   is
      Lowered_Command : constant String := To_Lower (Command);
      Busy            : Boolean;

   begin
      if Output /= null then
         Output.all := null;
      end if;

      if Debugger.Debugger = null then
         return;
      end if;

      Busy := Debugger.Debugger.Get_Process.Command_In_Process;

      if Output /= null
        and then Busy
      then
         --  Catch interrupt command when gdb is busy
         if Is_Interrupt_Command (Debugger.Debugger, Lowered_Command) then
            Debugger.Interrupt (Display_In_Console => False);
            return;
         end if;

         GNATCOLL.Traces.Trace
           (Me, "Process_User_Command: Debugger is already busy");
         return;
      end if;

      if Output_Command
        and then not Debugger_Console_All_Interactions.Get_Pref
      then
         Debugger.Output_Text (Command & ASCII.LF, Is_Command => True);
      end if;

      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      if Is_Quit_Command (Debugger.Debugger, Lowered_Command) then
         if Busy
           and then not Separate_Execution_Window (Debugger.Debugger)
         then
            --  If the debugger does not have a separate execution window,
            --  send the command right away.

            Debugger.Debugger.Send
              (Command, Wait_For_Prompt => False, Mode => Mode);

         else
            Close_Debugger (Debugger);
         end if;
         return;
      end if;

      --  Regular debugger command, send it.
      --  If a dialog is currently displayed, do not wait for the debugger
      --  prompt, since the prompt won't be displayed before the user
      --  answers the question...

      if Output = null
        and then (Debugger.Debugger.Continuation_Line
                  or else Debugger.Registered_Dialog /= null)
      then
         --  For interactive command, we always send them immediately to
         --  the debugger, since this might be an answer to a gdb question
         --  ("restart process (y/n) ?")
         Debugger.Debugger.Send
           (Command,
            Wait_For_Prompt => False,
            Mode            => Mode,
            Force_Send      => Debugger.Is_From_Dbg_Console);

      elsif Output = null then
         --  Force_Send is always false so that commands are queued. We
         --  are not in a secondary prompt anyway (which should be when
         --  we have a Registered_Dialog).
         Debugger.Debugger.Send
           (Command,
            Mode            => Mode,
            Wait_For_Prompt => False,
            Force_Send      => False);

      else
         Output.all := new String'
           (Debugger.Debugger.Send_And_Get_Clean_Output
              (Command, Mode => Mode));

         if Output_Command
           and then Debugger.Debugger /= null
         then
            Debugger.Debugger.Display_Prompt;
         end if;
      end if;
   end Process_User_Command;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type := GVD.Types.Visible) is
   begin
      Process_User_Command (Debugger, Command, Output_Command, Mode, null);
   end Process_User_Command;

   --------------------------
   -- Process_User_Command --
   --------------------------

   function Process_User_Command
     (Debugger       : not null access Visual_Debugger_Record'Class;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String
   is
      Result : aliased GNAT.Strings.String_Access;
   begin
      Process_User_Command
        (Debugger, Command, Output_Command, Mode, Result'Unchecked_Access);

      if Result = null then
         return "";
      else
         declare
            S : constant String := Result.all;
         begin
            GNAT.Strings.Free (Result);
            return S;
         end;
      end if;
   end Process_User_Command;

   ---------------------
   -- Register_Dialog --
   ---------------------

   procedure Register_Dialog
     (Process : access Visual_Debugger_Record;
      Dialog  : access GPS_Dialog_Record'Class) is
   begin
      if Process.Registered_Dialog /= null then
         --  Typically happens when the filter used to create a dialog
         --  is called several times for the same dialog.

         Destroy (Process.Registered_Dialog);
      end if;

      Process.Registered_Dialog := Dialog;
   end Register_Dialog;

   -----------------------
   -- Unregister_Dialog --
   -----------------------

   procedure Unregister_Dialog
     (Process : access Visual_Debugger_Record) is
   begin
      if Process.Registered_Dialog /= null then
         Destroy (Process.Registered_Dialog);
         Process.Registered_Dialog := null;
      end if;
   end Unregister_Dialog;

   -------------
   -- Get_Num --
   -------------

   overriding function Get_Num
     (Self : not null access Visual_Debugger_Record) return Gint is
   begin
      return Gint (Self.Debugger_Num);
   end Get_Num;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Process.Debugger_Text);
   end Get_Console;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class) return Boolean is
   begin
      if Save_Desktop_On_Exit.Get_Pref then
         Save_Desktop (Kernel, "Default");
      end if;

      --  Close the debugger immediately when receiving the "before exit"
      --  action hook. This is needed so that the perspective can be saved
      --  and the default perspective can be reset before the main window
      --  is actually destroyed.
      Close_Debugger (Self.Process);
      return True;
   end Execute;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Prefered_Kind   : GVD.Types.Debugger_Type;
      File            : GNATCOLL.VFS.Virtual_File;
      Project         : Project_Type;
      Args            : String;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Load_Executable : Boolean := False) return Visual_Debugger
   is
      Top           : constant GPS_Window :=
                        GPS_Window (Get_Main_Window (Kernel));
      Process       : Visual_Debugger;
      Program_Args  : GNAT.Strings.String_Access;
      Blank_Pos     : Natural;
      Proxy         : Process_Proxy_Access;
      Console_Child : MDI_Child;

      function Get_Main return Virtual_File;
      --  Return the file to debug

      function Is_MI_Protocol_Allowed
        (Args : GNAT.OS_Lib.Argument_List) return Boolean;
      --  check whether GDB version is correct for using MI protocol

      function Get_Debugger_Executable return String;
      --  Returns name of debugger and parameters for it

      function Get_Debugger_Kind
        (CL : GNAT.OS_Lib.Argument_List)
          return GVD.Types.Debugger_Type;
      --  Get debugger kind according to executable and prefered kind

      --------------
      -- Get_Main --
      --------------

      function Get_Main return Virtual_File is
         type Extension_Array is array (Positive range <>) of
           Filesystem_String (1 .. 4);
         Extensions : constant Extension_Array := (".exe", ".out", ".vxe");
         Tmp        : Virtual_File;

         End_Of_Exec  : Natural;
         Exec         : Virtual_File;
      begin
         if File /= GNATCOLL.VFS.No_File then
            Exec := File;

         elsif Args /= "" then
            Blank_Pos := Ada.Strings.Fixed.Index (Args, " ");

            if Blank_Pos = 0 then
               End_Of_Exec := Args'Last;
            else
               End_Of_Exec := Blank_Pos - 1;
               Free (Program_Args);
               Program_Args := new String'(Args (Blank_Pos + 1 .. Args'Last));
            end if;

            --  The code below assumes that the name of the executable is in
            --  Args (Args'First .. End_Of_Exec).

            declare
               Exec_Name : constant Filesystem_String :=
                 +Args (Args'First .. End_Of_Exec);

            begin
               --  First check whether Exec_Name is an absolute path
               Exec := Create (Full_Filename => Exec_Name);

               if not Exec.Is_Absolute_Path then
                  --  If the Exec name is not an absolute path, check
                  --  whether it corresponds to a file found from the
                  --  current directory.

                  Exec := Create
                    (Full_Filename =>
                       Normalize_Pathname (Exec_Name, Get_Current_Dir));

                  if not Exec.Is_Regular_File then
                     --  If the Exec is not an absolute path and it is not
                     --  found from the current directory, try to locate it
                     --  on path.

                     Exec := Locate_Compiler_Executable (Exec_Name);

                     if Exec = No_File then
                        Exec := Create_From_Base (Exec_Name);
                     end if;
                  end if;
               end if;
            end;
         end if;

         --  Check for a missing extension in module, and add it if needed
         --  Extensions currently checked in order: .exe, .out, .vxe

         if Exec = GNATCOLL.VFS.No_File or else Exec.Is_Regular_File then
            return Exec;
         else
            for J in Extensions'Range loop
               Tmp := Create
                 (Full_Filename => Exec.Full_Name.all & Extensions (J));

               if Tmp.Is_Regular_File then
                  Exec := Tmp;
                  exit;
               end if;
            end loop;
         end if;

         return Exec;
      end Get_Main;

      -----------------------
      -- Get_Debugger_Kind --
      -----------------------

      function Get_Debugger_Kind
        (CL : GNAT.OS_Lib.Argument_List)
         return GVD.Types.Debugger_Type
      is
         Is_LLDB : constant Boolean :=
           (for some Arg of CL => Starts_With (Arg.all, "lldb"));
      begin
         if Is_LLDB then
            --  Executable is LLDB supports only LLDB debugger kind
            if Prefered_Kind /= GVD.Types.LLDB then
               Process.Kernel.Insert
                 ("Can't use GDB protocol with given LLDB debugger, switching"
                  & " to LLDB mode", True, Error);
            end if;

            return GVD.Types.LLDB;
         end if;

         case Prefered_Kind is
            when GVD.Types.Gdb =>
               return GVD.Types.Gdb;

            when GVD.Types.DAP =>
               return GVD.Types.DAP;

            when GVD.Types.Gdb_MI =>
               if Is_MI_Protocol_Allowed (CL) then
                  return GVD.Types.Gdb_MI;
               else
                  Process.Kernel.Insert
                    ("MI protocol is not supported by GDB, switching to"
                       & " CI mode", True, Error);

                  return GVD.Types.Gdb;
               end if;
            when GVD.Types.LLDB =>
               Process.Kernel.Insert
                 ("Can't use LLDB protocol with given GDB debugger, switching"
                  & " to GDB CI mode", True, Error);

               return GVD.Types.Gdb;
         end case;
      end Get_Debugger_Kind;

      Args2        : GNAT.OS_Lib.Argument_List_Access;
      Actual_Remote_Target   : constant String :=
        (if Remote_Target /= ""
         then Remote_Target
         else Project.Attribute_Value (Program_Host_Attribute));
      Actual_Remote_Protocol : constant String :=
        (if Remote_Protocol /= ""
         then Remote_Protocol
         else Project.Attribute_Value (Protocol_Attribute));

      Executable    : GNATCOLL.VFS.Virtual_File;

      ----------------------------
      -- Is_MI_Protocol_Allowed --
      ----------------------------

      function Is_MI_Protocol_Allowed
        (Args : GNAT.OS_Lib.Argument_List) return Boolean
      is
         use Debugger.Base_Gdb;

         CL      : Arg_List;
         Fd      : GNAT.Expect.Process_Descriptor_Access := null;
         Version : Version_Number := Unknown_Version;
         Success : Boolean := False;
      begin

         --  Force the use of CLI mode hen debugging a VxWorks6 native
         --  application since we don't support it correctly in MI mode.
         if Index (Args (Args'First).all, "wrs-vxworks") /= 0 then
            return False;
         end if;

         for Arg of Args loop
            Append_Argument (CL, Arg.all, One_Arg);
         end loop;

         Append_Argument (CL, "--version", One_Arg);

         GPS.Kernel.Remote.Spawn
           (Kernel            => Kernel_Handle (Kernel),
            Arguments         => CL,
            Server            => Debug_Server,
            Pd                => Fd,
            Success           => Success);

         if Success
           and then Fd /= null
         then
            declare
               S : constant String := GNATCOLL.Utils.Get_Command_Output (Fd);
            begin
               Trace (Me, "GDB version is: " & S);
               Version := Parse_GDB_Version (S);
            exception
               when others =>
                  Trace (Me, "Could not detect gdb version");
            end;
         end if;
         Unchecked_Free (Fd);

         return Version.Major > 7
           or else (Version.Major = 7
                    and then Version.Minor >= 10);

      exception
         when others =>
            Trace (Me, "Could not detect gdb version");
            Unchecked_Free (Fd);
            return False;
      end Is_MI_Protocol_Allowed;

      -----------------------------
      -- Get_Debugger_Executable --
      -----------------------------

      function Get_Debugger_Executable return String is
      begin
         if Project.Has_Attribute (Debugger_Command_Attribute) then
            --  return debuger from project
            declare
               Name : constant String := Project.Attribute_Value
                 (Debugger_Command_Attribute);
            begin

               return Name;
            end;
         end if;

         case Prefered_Kind  is
            when GVD.Types.LLDB =>
               return "lldb";

            when others =>
               declare
                  Tc      : constant Toolchain :=
                    Kernel.Get_Toolchains_Manager.Get_Toolchain
                      (Kernel.Get_Project_Tree.Root_Project);
                  Command : constant String := Get_Command
                    (Tc, Toolchains.Debugger);
               begin
                  --  return debugger from toolchain

                  return Command;
               end;
         end case;
      end Get_Debugger_Executable;

   begin
      Process := new Visual_Debugger_Record;
      GVD.Process.Initialize (Process, Top);

      Program_Args := new String'("");

      Executable := Get_Main;

      Proxy := new GPS_Proxy;
      GPS_Proxy (Proxy.all).Process := Process;

      Args2 := GNAT.OS_Lib.Argument_String_To_List (Get_Debugger_Executable);

      Process.Descriptor.Debugger      := Get_Debugger_Kind (Args2.all);
      Process.Descriptor.Program       := Executable;
      Process.Descriptor.Debugger_Name := new String'(Args2 (1).all);

      case Process.Descriptor.Debugger is
         when GVD.Types.Gdb =>
            Process.Debugger := new Gdb_Debugger;
         when GVD.Types.Gdb_MI | GVD.Types.DAP =>
            Process.Debugger := new Gdb_MI_Debugger;
         when GVD.Types.LLDB =>
            Process.Debugger := new LLDB_Debugger;
      end case;

      --  Switch to the "Debug" perspective if available
      Load_Perspective (Kernel, "Debug");

      --  Spawn the debugger

      Process.Debugger.Spawn
        (Kernel          => Kernel,
         Executable      => Executable,
         Debugger_Args   => Args2 (2 .. Args2'Last),
         Executable_Args => Program_Args.all,
         Proxy           => Proxy,
         Remote_Target   => Actual_Remote_Target,
         Remote_Protocol => Actual_Remote_Protocol,
         Debugger_Name   => Process.Descriptor.Debugger_Name.all,
         Debugger_Num    => Process.Debugger_Num);
      GNAT.OS_Lib.Free (Args2);

      --  Destroying the console should kill the debugger
      --  ??? Signal should be handled in GVD.Console directly
      Attach_To_Debugger_Console
        (Process, Process.Kernel, Create_If_Necessary => True);
      if Process.Debugger_Text /= null then
         Kernel_Callback.Object_Connect
           (Process.Debugger_Text, Signal_Destroy,
            On_Console_Destroy'Access,
            After       => True,
            User_Data   => null,
            Slot_Object => Process);
      end if;

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window.

      Add_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         First_Text_Output_Filter'Access, Output, Top.all'Address);

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.

      Initialize (Process.Debugger);

      --  If we have a debuggee console in the desktop, always use it.
      --  Otherwise, we only create one when the user has asked for it.

      Process.Create_Execution_Console;

      --  When True, Load the executable on the target, if any

      if Load_Executable then
         Process.Debugger.Load_Executable
           (Executable => Executable.To_Remote (Get_Nickname (Debug_Server)),
            Mode       => Visible);
      end if;

      --  Catch exceptions if the preference is set
      Process.Debugger.Catch_Exception;

      --  Force the creation of the project if needed
      Load_Project_From_Executable (Kernel, Process);

      Before_Exit_Action_Hook.Add
        (Obj   => new On_Before_Exit'(Hook_Function with Process => Process),
         Watch => Process);

      Debugger_State_Changed_Hook.Run
        (Process.Kernel, Process, Debug_Available);
      Debugger_Started_Hook.Run (Process.Kernel, Process);

      --  Give the focus to the Debugger Console
      Console_Child := Find_MDI_Child
        (Get_MDI (Kernel), Process.Debugger_Text);

      if Console_Child /= null then
         Raise_Child (Console_Child);
      end if;

      Process.Store_History := False;
      Process.Interactions_History.Clear;

      return Process;

   exception
      when Process_Died =>
         GNATCOLL.Traces.Trace (Me, "could not launch the debugger");

         for Str of Process.Interactions_History loop
            Kernel.Messages_Window.Insert (Str, Mode => Error);
         end loop;
         Process.Store_History := False;
         Process.Interactions_History.Clear;

         declare
            Dummy : constant Message_Dialog_Buttons :=
              GPS_Message_Dialog
                (Expect_Out (Get_Process (Process.Debugger)) & ASCII.LF &
                 (-"Could not launch the debugger"),
                 Error, Button_OK, Button_OK,
                 Parent => Gtk_Window (Top));
         begin
            Process.Exiting := True;

            Close_Debugger (Process);
            Process.Exiting := False;
            return null;
         end;

      when Spawn_Error =>
         for Str of Process.Interactions_History loop
            Kernel.Messages_Window.Insert (Str, Mode => Error);
         end loop;
         Process.Store_History := False;
         Process.Interactions_History.Clear;

         --  Do not display a dialog here since the Spawn procedure displays
         --  a dialog before raising Spawn_Error.

         --  This will close the debugger and switch back to the default
         --  perspective. However, some windows haven't had a size allocation
         --  yet, so this might corrupt the desktop.
         Close_Debugger (Process);
         return null;

      when E : others =>
         for Str of Process.Interactions_History loop
            Kernel.Messages_Window.Insert (Str, Mode => Error);
         end loop;
         Process.Store_History := False;
         Process.Interactions_History.Clear;

         GNATCOLL.Traces.Trace (Me, E);
         return Process;
   end Spawn;

   ----------------------------------
   -- Load_Project_From_Executable --
   ----------------------------------

   procedure Load_Project_From_Executable
     (Kernel   : access Kernel_Handle_Record'Class;
      Debugger : access Visual_Debugger_Record'Class)
   is
      Project : Project_Type := Get_Project (Kernel);
      Exec    : Virtual_File;

   begin
      --  Do nothing unless the current project was already generated from an
      --  executable.

      if Get_Registry (Kernel).Tree.Status /= From_Executable then
         return;
      end if;

      Exec := Get_Executable (Debugger.Debugger);

      if Exec /= No_File and then not Is_Regular_File (Exec) then
         declare
            Buttons : Message_Dialog_Buttons;
            pragma Unreferenced (Buttons);
         begin
            if Active (Testsuite_Handle) then
               GNATCOLL.Traces.Trace
                 (Testsuite_Handle, "executable passed to --debug not found");
            else
               Buttons := GPS_Message_Dialog
                 (Msg =>
                    "The following executable specified with"
                  & " --debug does not exist on disk: "
                  & ASCII.LF
                  & Exec.Display_Full_Name,
                  Dialog_Type => Error,
                  Buttons => Button_OK,
                  Title =>  -"Executable not found",
                  Parent => Get_Main_Window (Kernel));
            end if;
         end;
      end if;

      declare
         List : String_List_Access := Project.Attribute_Value (Main_Attribute);
      begin
         if List /= null then
            for L in List'Range loop
               if Equal (+List (L).all, Full_Name (Exec)) then
                  Free (List);
                  return;
               end if;
            end loop;
            Free (List);
         end if;
      end;

      --  No handling of desktop is done here, we want to leave all windows
      --  as-is.

      declare
         Debugger_Name : constant String :=
                           Project.Attribute_Value
                             (Debugger_Command_Attribute, Default => "");
         Target        : constant String :=
                           Project.Attribute_Value
                             (Program_Host_Attribute, Default => "");
         Protocol      : constant String :=
                           Project.Attribute_Value
                             (Protocol_Attribute, Default => "");
      begin
         Get_Registry (Kernel).Tree.Unload;

         --  Create an empty project, and we'll add properties to it

         if Exec /= GNATCOLL.VFS.No_File then
            Get_Registry (Kernel).Tree.Load_Empty_Project
              (Get_Registry (Kernel).Environment,
               Name           => "debugger_" & (+Base_Name (Exec)),
               Recompute_View => False);
         else
            Get_Registry (Kernel).Tree.Load_Empty_Project
              (Get_Registry (Kernel).Environment,
               Name           => "debugger_no_file",
               Recompute_View => False);
         end if;

         Project := Get_Registry (Kernel).Tree.Root_Project;

         if Debugger_Name /= "" then
            Project.Set_Attribute
              (Attribute          => Debugger_Command_Attribute,
               Value              => Debugger_Name);
         end if;

         if Target /= "" then
            Project.Set_Attribute
              (Attribute          => Program_Host_Attribute,
               Value              => Target);
         end if;

         if Protocol /= "" then
            Project.Set_Attribute
              (Attribute          => Protocol_Attribute,
               Value              => Protocol);
         end if;
      end;

      declare
         List        : GNAT.Strings.String_List :=
                         Source_Files_List (Debugger.Debugger);
         Bases       : GNAT.OS_Lib.Argument_List (List'Range);
         Bases_Index : Natural := Bases'First;
         Dirs        : GNAT.OS_Lib.Argument_List (List'Range);
         Dirs_Index  : Natural := Dirs'First;
         Main        : GNAT.OS_Lib.Argument_List (1 .. 1);
         Langs       : GNAT.OS_Lib.Argument_List (List'Range);
         Lang_Index  : Natural := Langs'First;

      begin
         --  Source_Files, Source_Dirs & Languages

         for L in List'Range loop
            declare
               Remote_File : constant Virtual_File :=
                               Create_From_Base
                                 (+List (L).all,
                                  Dir_Name (Exec),
                                  Get_Nickname (Debug_Server));
               Local_File  : constant Virtual_File := To_Local (Remote_File);
               Dir         : constant Virtual_File := Local_File.Dir;
               Base        : constant Filesystem_String :=
                               Base_Name (Local_File);
               Lang        : constant String :=
                               Get_Language_From_File
                                 (Get_Language_Handler (Kernel),
                                  Local_File);
               Found       : Boolean;

            begin
               Found := False;

               if Is_Directory (Dir) then
                  for D in Dirs'First .. Dirs_Index - 1 loop
                     if Equal (+Dirs (D).all, Dir.Full_Name) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Dirs (Dirs_Index) := new String'(+Dir.Full_Name);
                     Dirs_Index := Dirs_Index + 1;
                  end if;

                  Found := False;
                  for J in Bases'First .. Bases_Index - 1 loop
                     if Equal (+Bases (J).all, Base) then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Bases (Bases_Index) :=
                       new String'(Base_Name (List (L).all));
                     Bases_Index := Bases_Index + 1;
                  end if;

                  Found := False;
                  if Lang /= "" then
                     for La in Langs'First .. Lang_Index - 1 loop
                        if Langs (La).all = Lang then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Langs (Lang_Index) := new String'(Lang);
                        Lang_Index := Lang_Index + 1;
                     end if;
                  end if;
               end if;
            end;
         end loop;

         GNATCOLL.Traces.Trace (Me, "Setting Source_Dirs:");
         for D in Dirs'First .. Dirs_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Dirs (D).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Dirs_Attribute,
            Values             => Dirs (Dirs'First .. Dirs_Index - 1));
         Free (Dirs);

         GNATCOLL.Traces.Trace (Me, "Setting Source_Files:");
         for B in Bases'First .. Bases_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Bases (B).all);
         end loop;

         Project.Set_Attribute
           (Attribute          => Source_Files_Attribute,
            Values             => Bases (Bases'First .. Bases_Index - 1));
         Free (Bases);

         GNATCOLL.Traces.Trace (Me, "Setting Languages:");
         for L in Langs'First .. Lang_Index - 1 loop
            GNATCOLL.Traces.Trace (Me, "   " & Langs (L).all);
         end loop;

         if Lang_Index = Langs'First then
            Project.Set_Attribute
              (Scenario  => All_Scenarios,
               Attribute => Languages_Attribute,
               Values    =>
                 (new String'("ada"), new String'("c"), new String'("c++")));
         else
            Project.Set_Attribute
              (Attribute          => Languages_Attribute,
               Values             => Langs (Langs'First .. Lang_Index - 1));
         end if;

         Free (Langs);

         --  Object_Dir, Exec_Dir, Main

         if Exec /= GNATCOLL.VFS.No_File then
            Project.Set_Attribute
              (Attribute          => Obj_Dir_Attribute,
               Value              => +Dir_Name (Exec));
            Project.Set_Attribute
              (Attribute          => Exec_Dir_Attribute,
               Value              => +Dir_Name (Exec));

            Main (Main'First) := new String'(+Full_Name (Exec));
            Project.Set_Attribute
              (Attribute          => Main_Attribute,
               Values             => Main);
            Free (Main);
         end if;
         Free (List);
      end;

      --  Is the information for this executable already cached? If yes,
      --  we simply reuse it to avoid the need to interact with the debugger.

      Project.Set_Modified (False);
      Get_Registry (Kernel).Tree.Set_Status (From_Executable);
      Project_Changed_Hook.Run (Kernel);
      Recompute_View (Kernel);
   end Load_Project_From_Executable;

   -----------------
   -- Dbg_Command --
   -----------------

   package body Dbg_Command is
      overriding function Execute
        (Command : access Debugger_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
         Process : Visual_Debugger;
      begin
         Process := Visual_Debugger (Get_Current_Debugger (Kernel));

         if Process = null or else Process.Debugger = null then
            return Commands.Failure;
         end if;

         if Debugger_Console_Has_Focus (Process) then
            Process.Is_From_Dbg_Console := True;
         end if;

         return Debugger_Command_Access (Command).Execute_Dbg (Process);
      end Execute;
   end Dbg_Command;

end GVD.Process;

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

with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Config;                    use Config;
with Default_Preferences;       use Default_Preferences;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Process;               use GVD.Process;
with GVD.Trace;                 use GVD.Trace;
with GVD.Types;                 use GVD.Types;
with MI.Lexer;                  use MI.Lexer;
with Process_Proxies;           use Process_Proxies;
with Remote;                    use Remote;
with String_Utils;              use String_Utils;

package body Debugger.Gdb_MI is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp             : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_String             : constant String := "(gdb) ";
   --  The prompt used by the debugger

   Gdb_Command               : constant String := "gdb";
   --  Name of the command to launch gdb

   Gdb_Options               : constant String := "-nw -q --interpreter=mi";
   --  Options always passed to gdb

   Highlight_Pattern         : constant Pattern_Matcher :=
     Compile ("\(gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window

   Breakpoint_Pattern        : constant Pattern_Matcher := Compile
     ("^(=breakpoint-|\^done,bkpt=)", Multiple_Lines);
   --  Pattern used to detect when breakpoints are created/deleted

   Breakpoint_Num_Pattern    : constant Pattern_Matcher := Compile
     ("^=breakpoint-created,bkpt={number=""(\d+)");
   --  Pattern to match the breakpoint number after we just created one.

   Stopped_Pattern           : constant Pattern_Matcher := Compile
     ("^\*stopped", Multiple_Lines);
   --  Pattern used to detect when the debuggee stops

   Context_Pattern           : constant Pattern_Matcher := Compile
     ("^=thread-", Multiple_Lines);
   --  Pattern used to detect when the debuggee stops

   Terminate_Pattern         : constant Pattern_Matcher := Compile
     ("^=thread-group-exited", Multiple_Lines);
   --  Pattern used to detect when the debuggee terminates

   Running_Pattern           : constant Pattern_Matcher := Compile
     ("^\^error,msg=""The program is not being run.", Multiple_Lines);
   --  Pattern used to detect when the debuggee is not running

   Version_Pattern : constant Pattern_Matcher := Compile
     ("^GNU gdb( \(GDB\))? ([0-9]+)\.([0-9]+)(.[0-9]+)? .*");
   --  To detect the version of GDB

   procedure Breakpoint_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when a breakpoint is created/deleted

   procedure Stopped_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the debuggee stopped

   procedure Context_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the debuggee's context changed

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the program no longer runs

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_MI_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier;
   --  Send a command that sets a breakpoint, and retrieve the id of the newly
   --  created breakpoint

   procedure Set_Args
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden);
   --  Set the debuggee arguments to Arguments

   procedure Switch_Language
     (Debugger : access Gdb_MI_Debugger;
      Language : String);
   --  Switch gdb to another language. The possible values for Language are:
   --  "ada", "c", "c++", "asm", "chill", "fortran", "java", "modula-2",
   --  "scheme".
   --  When calling this function, the current language is stored internally
   --  and can be restored by calling Restore_Language.

   procedure Restore_Language
     (Debugger : access Gdb_MI_Debugger);
   --  Restore the language that was active before Switch_Language was called

   function Get_Module (Executable : GNATCOLL.VFS.Virtual_File) return String;
   --  Return the name of the module contained in Executable
   --  Assume that the name of the module is the executable file
   --  with no path information and no extension.

   procedure Detect_Debugger_Mode (Debugger : access Gdb_MI_Debugger);
   --  This detects the debugger mode depending on the remote protocol being
   --  used. Ideally, it should be called every time we change the remote
   --  protocol.

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_MI_Debugger);
   --  Connect to the target if not already connected

   function Get_GDB_Version
     (Debugger : access Gdb_MI_Debugger) return Version_Number;
   --  Return the GDB version number.

   procedure Reset_State (Debugger : access Gdb_MI_Debugger);
   --  Reset some itnernal states.
   --  Typically caleld before sending a new command to the debugger.

   function Find_Identifier
     (C : Token_Lists.Cursor; Value : String) return Token_Lists.Cursor;
   --  Helper function to find the first Udentifier token starting from C
   --  named Value.

   procedure Next (C : in out Token_Lists.Cursor; N : Natural);
   --  Execute Next (C) N times

   function Strip_Escape (S : String) return String;
   --  Strip escape characters, e.g. replace \\ by \, \" by "

   ---------------------
   -- Find_Identifier --
   ---------------------

   function Find_Identifier
     (C : Token_Lists.Cursor; Value : String) return Token_Lists.Cursor
   is
      use Token_Lists;

      Cur  : Token_Lists.Cursor := C;
      Elem : MI.Lexer.Token_Type;

   begin
      loop
         exit when Cur = No_Element;

         Elem := Element (Cur);

         if Elem.Code = Identifier
           and then Elem.Text /= null
           and then Elem.Text.all = Value
         then
            return Cur;
         end if;

         Next (Cur);
      end loop;

      return Cur;
   end Find_Identifier;

   ----------
   -- Next --
   ----------

   procedure Next (C : in out Token_Lists.Cursor; N : Natural) is
      use Token_Lists;
   begin
      for J in 1 .. N loop
         Next (C);
      end loop;
   end Next;

   ------------------
   -- Strip_Escape --
   ------------------

   function Strip_Escape (S : String) return String is
      Result    : String (1 .. S'Length);
      J, Index  : Natural := 0;
   begin
      J := S'First;
      while J <= S'Last loop
         if S (J) = '\'
           and then J < S'Last
           and then S (J + 1) in '\' | '"'
         then
            J := J + 1;
         end if;

         Index := Index + 1;
         Result (Index) := S (J);
         J := J + 1;
      end loop;

      return Result (1 .. Index);
   end Strip_Escape;

   ---------------------
   -- Get_GDB_Version --
   ---------------------

   function Get_GDB_Version
     (Debugger : access Gdb_MI_Debugger) return Version_Number
   is
   begin
      if Debugger.GDB_Version /= Unknown_Version then
         return Debugger.GDB_Version;
      end if;

      declare
         S       : constant String := Send_And_Get_Clean_Output
           (Debugger, "-gdb-version", Mode => Internal);
         Matched : Match_Array (0 .. 3);

      begin
         Match (Version_Pattern, S, Matched);

         if Matched (0) = No_Match then
            Debugger.GDB_Version := (0, 1);
         else
            Debugger.GDB_Version :=
              (Natural'Value (S (Matched (2).First .. Matched (2).Last)),
               Natural'Value (S (Matched (3).First .. Matched (3).Last)));
         end if;
      end;

      return Debugger.GDB_Version;
   end Get_GDB_Version;

   --------------------------
   -- Detect_Debugger_Mode --
   --------------------------

   procedure Detect_Debugger_Mode (Debugger : access Gdb_MI_Debugger) is
   begin
      if Debugger.Remote_Protocol = null
        or else Debugger.Remote_Protocol.all = ""
      then
         Debugger.Mode := Native;
      else
         Debugger.Mode := Cross;
      end if;
   end Detect_Debugger_Mode;

   -----------------------
   -- Breakpoint_Filter --
   -----------------------

   procedure Breakpoint_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Gdb_MI_Debugger (Process.Debugger.all).Breakpoints_Changed := True;
   end Breakpoint_Filter;

   --------------------
   -- Stopped_Filter --
   --------------------

   procedure Stopped_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Gdb_MI_Debugger (Process.Debugger.all).Category := Execution_Command;
   end Stopped_Filter;

   --------------------
   -- Context_Filter --
   --------------------

   procedure Context_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Gdb_MI_Debugger (Process.Debugger.all).Category := Context_Command;
   end Context_Filter;

   --------------------
   -- Running_Filter --
   --------------------

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Set_Is_Started (Process.Debugger, False);
   end Running_Filter;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Debugger : access Gdb_MI_Debugger) is
   begin
      Debugger.Breakpoints_Changed := False;
      Debugger.Category := Misc_Command;
   end Reset_State;

   -------------------------------
   -- Send_And_Get_Clean_Output --
   -------------------------------

   overriding function Send_And_Get_Clean_Output
     (Debugger : access Gdb_MI_Debugger;
      Cmd      : String;
      Mode     : Command_Type := Hidden) return String is
   begin
      Reset_State (Debugger);

      declare
         S   : constant String := Send_And_Get_Output (Debugger, Cmd, Mode);
         Pos : Integer;
      begin
         if Ends_With (S, Prompt_String) then
            Pos := S'Last - Prompt_String'Length;
            if S (Pos) = ASCII.LF then
               Pos := Pos - 1;
            end if;
            return S (S'First .. Pos);
         else
            return S;
         end if;
      end;
   end Send_And_Get_Clean_Output;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Debugger        : access Gdb_MI_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      J, K : Integer;
   begin
      Reset_State (Debugger);

      --  ???? consider deriving from Gdb_Debugger instead
      --  Override Send to recognize some commands such as "target"

      if Cmd'Length > 10
        and then Cmd (Cmd'First .. Cmd'First + 6) = "target "
      then
         J := Cmd'First + 7;
         Skip_Blanks (Cmd, J);
         K := J + 1;
         Skip_To_Blank (Cmd, K);

         if K < Cmd'Last then
            Free (Debugger.Remote_Protocol);
            Debugger.Remote_Protocol := new String'(Cmd (J .. K - 1));
            Detect_Debugger_Mode (Debugger);

            J := K + 1;
            Skip_Blanks (Cmd, J);
            Free (Debugger.Remote_Target);
            Debugger.Remote_Target := new String'(Cmd (J .. Cmd'Last));
         end if;
      end if;

      --  Call the Parent procedure

      Send
        (Debugger_Root (Debugger.all)'Access, Cmd,
         Empty_Buffer, Wait_For_Prompt, Force_Send, Mode);
   end Send;

   -------------
   -- Type_Of --
   -------------

   overriding function Type_Of
     (Debugger : access Gdb_MI_Debugger; Entity : String) return String is
   begin
      --  If Entity contains a LF, this is an invalid entity, so give up
      --  immediately.

      for J in reverse Entity'Range loop
         if Entity (J) = ASCII.LF then
            return "";
         end if;
      end loop;

      declare
         S   : constant String := Send_And_Get_Clean_Output
           (Debugger, "ptype " & Entity, Mode => Internal);
         Pos : constant Integer := Index (S, "type = ");
         End_Pos : constant Integer := Index (S, "\n");

      begin
         if Pos = 0 or End_Pos = 0 then
            return "";
         else
            return S (Pos + 7 .. End_Pos - 1);
         end if;
      end;
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   overriding function Info_Locals
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-stack-list-locals 1";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   overriding function Info_Args
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      --  ????
      return "info args";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   overriding function Info_Registers
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-data-list-register-values N";
   end Info_Registers;

   --------------
   -- Value_Of --
   --------------

   subtype String_2 is String (1 .. 2);
   Fmt_Array : constant array (Value_Format) of String_2 :=
     (Default_Format => "  ",
      Decimal        => "/d",
      Binary         => "/t",
      Hexadecimal    => "/x",
      Octal          => "/o");
   --  Array used by Value_Of to print values in various formats

   overriding function Value_Of
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String
   is
      S : constant String := Send_And_Get_Clean_Output
        (Debugger, "print" & Fmt_Array (Format) & ' ' & Entity,
         Mode => Internal);
      Index : Natural := S'First;

   begin
      --  The value is valid only if it starts with '$'

      if S = ""
        or else S (S'First) /= '$'
      then
         return "";
      end if;

      --  Skip the '$nn =' part
      Skip_To_Char (S, Index, '=');
      Index := Index + 1;

      return S (Index + 1 .. S'Last);
   end Value_Of;

   ---------------------
   -- Print_Value_Cmd --
   ---------------------

   overriding function Print_Value_Cmd
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "print " & Entity;
   end Print_Value_Cmd;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   overriding function Get_Uniq_Id
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String) return String
   is
      --  ??? Probably, this should be language-dependent.
      --  In particular, in C, &(*A) returns A, not an address, which causes
      --  unexpected wrong aliases to be detected.

      S       : constant String := Send_And_Get_Clean_Output
        (Debugger, "print &(" & Entity & ")", Mode => Internal);
      Matched : Match_Array (0 .. 1);

   begin
      Match (" (0x[0-9a-zA-Z]+)", S, Matched);

      if Matched (1) /= No_Match then
         return S (Matched (1).First .. Matched (1).Last);
      end if;

      return "";
   end Get_Uniq_Id;

   -----------
   -- Spawn --
   -----------

   overriding procedure Spawn
     (Debugger        : access Gdb_MI_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Debugger_Args   : GNAT.OS_Lib.Argument_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Gdb_Arguments   : Argument_List_Access :=
                          Argument_String_To_List (Gdb_Options);
      Num_Options     : constant Natural := Gdb_Arguments'Length;
      Local_Arguments : Argument_List
        (1 .. Debugger_Args'Length + Num_Options);
      Process         : Visual_Debugger;

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);

   begin
      Local_Arguments (1 .. Num_Options) := Gdb_Arguments.all;
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) :=
        Debugger_Args;
      Free (Gdb_Arguments);

      if Debugger_Name = "" then
         General_Spawn
           (Debugger, Kernel, Local_Arguments, Gdb_Command, Proxy);
      else
         General_Spawn
           (Debugger, Kernel, Local_Arguments, Debugger_Name, Proxy);
      end if;

      Free (Debugger.Executable_Args);
      Free (Debugger.Remote_Target);
      Free (Debugger.Remote_Protocol);

      for J in 1 .. Num_Options loop
         Free (Local_Arguments (J));
      end loop;

      Debugger.Executable := Executable;

      if Executable_Args /= "" then
         Debugger.Executable_Args := new String'(Executable_Args);
      end if;

      if Remote_Target /= "" then
         Debugger.Remote_Target := new String'(Remote_Target);
         Debugger.Remote_Protocol := new String'(Remote_Protocol);
         Detect_Debugger_Mode (Debugger);
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      Process := Convert (Debugger);

      if Process /= null then
         Add_Regexp_Filter (Process, Running_Filter'Access, Terminate_Pattern);
         Add_Regexp_Filter (Process, Running_Filter'Access, Running_Pattern);
         Add_Regexp_Filter
           (Process, Breakpoint_Filter'Access, Breakpoint_Pattern);
         Add_Regexp_Filter (Process, Stopped_Filter'Access, Stopped_Pattern);
         Add_Regexp_Filter (Process, Context_Filter'Access, Context_Pattern);

         Set_Input_Output_Filter (Process);
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Debugger : access Gdb_MI_Debugger) is
      Num     : Expect_Match;
      Process : Visual_Debugger;
      Ignored : Version_Number;
      pragma Unreferenced (Ignored);

      use GVD;

      pragma Unreferenced (Num);
   begin
      Debugger.Initializing := True;

      --  Wait for initial output and prompt (and display it in the window)
      Wait
        (Get_Process (Debugger), Num,
         Compile ("^\([^\s]+\).*$", Multiple_Lines),
         Timeout => -1);

      Send (Debugger, "-gdb-set width 0", Mode => Internal);
      Send (Debugger, "-gdb-set height 0", Mode => Internal);

      --  Cache result of 'show version', and also as a side effect, includes
      --  output of 'show version' in the log file.

      Ignored := Get_GDB_Version (Debugger);

      if not Is_Local (Debug_Server) then
         --  Workaround the following problem: when telneting to Windows,
         --  using the windows telnet service, then the TERM env variable is
         --  set to vt100, which produces undesirable results (input is
         --  duplicated, see HC08-007). A workaround is to send
         --  'set editing off' to gdb, as this removes this undesired result.
         Send (Debugger, "-gdb-set editing off", Mode => Internal);
      end if;

      if Get_Pref (Execution_Window) and then Is_Local (Debug_Server) then
         if Host = Windows then
            Send (Debugger, "-gdb-set new-console", Mode => Internal);
         end if;

         Debugger.Execution_Window := True;

      else
         Debugger.Execution_Window := False;
      end if;

      --  Make sure gdb will not ask too much interactive questions.
      --  Interactive questions are better left to the GUI itself.
      Send (Debugger, "-gdb-set confirm off", Mode => Internal);

      --  Load the module to debug, if any

      if Debugger.Executable /= GNATCOLL.VFS.No_File then
         Set_Executable (Debugger, Debugger.Executable, Mode => Visible);

      else
         --  Connect to the target, if needed. This is normally done by
         --  Set_Executable, but GPS should also connect immediately if
         --  the corresponding options were passed on the command line.
         Connect_To_Target_If_Needed (Debugger);

         --  Indicate that a new executable is present (even if there is none,
         --  we still need to reset some data).
         --  Do this before looking for the current file, since the explorer
         --  must also be initialized.
         --  No need to do anything in text-only mode

         Process := Convert (Debugger);
         if Process /= null then
            Debugger_Executable_Changed_Hook.Run (Process.Kernel, Process);
         end if;

         --  ??? missing support for Open_Main_Unit preference
      end if;

      if Debugger.Executable_Args /= null then
         Set_Args (Debugger, Debugger.Executable_Args.all, Mode => Internal);
      end if;

      if Debugger.Executable = GNATCOLL.VFS.No_File then
         Display_Prompt (Debugger);
      end if;

      --  ??? call Set_Language
      Debugger.Initializing := False;

   exception
      --  If the executable was not found, simply display the prompt before
      --  leaving, nothing else needs to be done.

      when Executable_Not_Found =>
         Display_Prompt (Debugger);
         Debugger.Initializing := False;
   end Initialize;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Debugger : access Gdb_MI_Debugger) is
   begin
      --  If the debugger process is dead, do not attempt to communicate
      --  with the underlying process.

      if Get_Process (Debugger) /= null
        and then Get_Descriptor (Get_Process (Debugger)) /= null
        and then Get_Pid (Get_Descriptor (Get_Process (Debugger)).all) /=
          GNAT.Expect.Invalid_Pid
      then
         --  In case the debugger is busy processing a command.

         if Command_In_Process (Debugger.Process) then
            Interrupt (Debugger);
         end if;

         --  Now exit the debugger
         Send (Debugger, "-gdb-exit", Mode => Internal);
      end if;

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_MI_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : Invisible_Command := Hidden)
   is
      pragma Unreferenced (Force);
   begin
      Send (Debugger, "target " & Protocol & " " & Target, Mode => Mode);
      Set_VxWorks_Version (Debugger, Force => True);
   end Connect_To_Target;

   ----------------------------
   -- Is_Connected_To_Target --
   ----------------------------

   overriding function Is_Connected_To_Target
     (Debugger : access Gdb_MI_Debugger) return Boolean
   is
      (Debugger.Target_Connected);

   --------------
   -- Set_Args --
   --------------

   procedure Set_Args
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-arguments " & Arguments, Mode => Mode);
   end Set_Args;

   --------------------
   -- Get_Executable --
   --------------------

   overriding function Get_Executable
     (Debugger : access Gdb_MI_Debugger) return GNATCOLL.VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   ---------------------------------
   -- Connect_To_Target_If_Needed --
   ---------------------------------

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_MI_Debugger) is
   begin
      if Debugger.Remote_Target /= null
        and then not Debugger.Target_Connected
      then
         declare
            Process : constant Visual_Debugger := Convert (Debugger);
            Cmd : constant String :=
                    "target " & Debugger.Remote_Protocol.all & " "
                      & Debugger.Remote_Target.all;
         begin
            if Process = null then
               Send (Debugger, Cmd, Mode => Internal);

            else
               Output_Text
                 (Process,
                  Send_And_Get_Clean_Output
                    (Debugger, Cmd, Mode => Internal) & ASCII.LF);
            end if;

            if Debugger.Remote_Protocol.all = "remote" then
               Set_Is_Started (Debugger, True);
            end if;
         end;

         Set_VxWorks_Version (Debugger);
         Debugger.Target_Connected := True;
      end if;
   end Connect_To_Target_If_Needed;

   --------------------
   -- Set_Executable --
   --------------------

   overriding procedure Set_Executable
     (Debugger   : access Gdb_MI_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : Command_Type := Hidden)
   is
      pragma Unreferenced (Mode);

      Remote_Exec         : constant Virtual_File :=
                              To_Remote
                                (Executable, Get_Nickname (Debug_Server));
      Exec_Has_Spaces     : constant Boolean :=
                              Index (Remote_Exec.Display_Full_Name, " ") /= 0;
      Full_Name           : constant String :=
                              +Remote_Exec.Unix_Style_Full_Name;
      No_Such_File_Regexp : constant Pattern_Matcher :=
                              Compile (Full_Name &
                                       ": No such file or directory.");
      --  Note that this pattern should work even when LANG isn't english
      --  because gdb does not seem to take into account this variable at all.

      Process : Visual_Debugger;

      procedure Launch_Command_And_Output (Command : String);
      --  Launch a "file" or "load" command and display it if relevant

      -------------------------------
      -- Launch_Command_And_Output --
      -------------------------------

      procedure Launch_Command_And_Output (Command : String) is
         Cmd : GNAT.Strings.String_Access;
      begin
         if Exec_Has_Spaces then
            Cmd := new String'
              (Command & " """ & Full_Name & '"');
         else
            Cmd := new String'
              (Command & " " & Full_Name);
         end if;

         if Process /= null then
            Output_Text (Process, Cmd.all & ASCII.LF, Set_Position => True);
         end if;

         declare
            S      : constant String := Send_And_Get_Clean_Output
              (Debugger, Cmd.all, Mode => Hidden);
            Result : Unbounded_String;
         begin
            Free (Cmd);

            if Match (No_Such_File_Regexp, S) /= 0 then
               raise Executable_Not_Found;
            end if;

            if Process /= null and then S /= "" then
               Filter_Output (Debugger, Hidden, S, Result);
               Output_Text
                 (Process,
                  To_String (Result) & ASCII.LF,
                  Set_Position => True);
            end if;
         end;
      end Launch_Command_And_Output;

   begin
      Process := Convert (Debugger);

      Debugger.Executable := Executable;
      Launch_Command_And_Output ("file");

      --  Connect to the remote target if needed

      Connect_To_Target_If_Needed (Debugger);

      --  Send the "load" command if needed

      if Debugger.Mode /= Native then
         Launch_Command_And_Output ("load");
      end if;

      Display_Prompt (Debugger);

      --  If we are in Cross mode (ie, with the "remote" protocol), the call
      --  to "target" has the side effect of starting the executable.
      --  In all other cases the executable is not started at this stage.

      if Debugger.Mode = Cross then
         Set_Is_Started (Debugger, True);
      else
         Set_Is_Started (Debugger, False);
      end if;

      --  Report a change in the executable. This has to be done before we
      --  look for the current file and line, so that the explorer can be
      --  correctly updated.
      --  No need to do anything in text-only mode

      if Process /= null then
         Debugger_Executable_Changed_Hook.Run (Process.Kernel, Process);
      end if;

      if Get_Pref (Break_On_Exception) then
         declare
            Cmd : constant String := "-catch-exception";
            S   : constant String := Send_And_Get_Clean_Output (Debugger, Cmd);

         begin
            if Process /= null then
               Output_Text (Process, Cmd & ASCII.LF, Set_Position => True);

               if S /= "" then
                  Output_Text (Process, S & ASCII.LF, Set_Position => True);
               end if;

               Display_Prompt (Debugger);
            end if;
         end;
      end if;

      --  ??? missing support for Open_Main_Unit
   end Set_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   overriding procedure Load_Core_File
     (Debugger : access Gdb_MI_Debugger;
      Core     : Virtual_File;
      Mode     : Command_Type := Hidden)
   is
      Core_File : constant Filesystem_String := Core.Unix_Style_Full_Name;
   begin
      Set_Is_Started (Debugger, False);
      Send (Debugger, "core " & (+Core_File), Mode => Mode);

      if Mode in Visible_Command then
         Wait_User_Command (Debugger);
      end if;
   end Load_Core_File;

   -----------------------------
   -- Load_Current_Executable --
   -----------------------------

   overriding procedure Load_Current_Executable
      (Debugger : access Gdb_MI_Debugger;
       Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Debugger.Is_Connected_To_Target then
         Send (Debugger, "-target-download", Mode => Mode);
      end if;
   end Load_Current_Executable;

   -----------------
   -- Add_Symbols --
   -----------------

   overriding procedure Add_Symbols
     (Debugger : access Gdb_MI_Debugger;
      Module   : Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Symbols : constant String := +Module.Unix_Style_Full_Name;
   begin
      --  ???
      --        Test_If_Has_Command
      --          (Debugger, Debugger.Has_Wtx_Add_Symbol_File,
      --           "wtx add-symbol-file");

      Send
        (Debugger,
         "add-symbol-file " & Symbols & " " & Address, Mode => Mode);

      if Mode in Visible_Command then
         Wait_User_Command (Debugger);
      end if;
   end Add_Symbols;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access Gdb_MI_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "attach " & Process, Mode => Mode);
      Set_Is_Started (Debugger, True);

      if Mode in Visible_Command then
         Wait_User_Command (Debugger);
      end if;

      --  Find the first frame containing source information to be as user
      --  friendly as possible, and also check whether attach was successful

      loop
         Set_Parse_File_Name (Get_Process (Debugger), False);

         declare
            Str  : constant String := Send_And_Get_Clean_Output
              (Debugger, "up", Mode => Internal);
            File : Unbounded_String;
            Line : Natural;
            Addr : Address_Type;
            pragma Unreferenced (Line, Addr);
         begin
            Set_Parse_File_Name (Get_Process (Debugger), True);

            --  If attach failed, "up" will return an error message

            if Str = "No stack." then
               Set_Is_Started (Debugger, False);
               return;
            end if;

            exit when Str = "Initial frame selected; you cannot go up.";

            Found_File_Name (Debugger, Str, File, Line, Addr);

            exit when Length (File) /= 0;
         end;
      end loop;

      Send (Debugger, "frame", Mode => Internal);

   exception
      when Constraint_Error =>
         --  Most likely the underlying process died
         null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   overriding procedure Detach_Process
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "detach", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access Gdb_MI_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "kill", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt (Debugger : access Gdb_MI_Debugger) is
      Num : Expect_Match;
      pragma Unreferenced (Num);
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   overriding function Wait_Prompt
     (Debugger : access Gdb_MI_Debugger;
      Timeout  : Integer) return Boolean
   is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => Timeout);
      return Num /= Expect_Timeout;
   end Wait_Prompt;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module (Executable : Virtual_File) return String is
      Exec      : constant String := +Base_Name (Executable);
      Dot_Index : Natural;
   begin
      --  Strip extensions (e.g .out)

      Dot_Index := Index (Exec, ".");

      if Dot_Index = 0 then
         Dot_Index := Exec'Last + 1;
      end if;

      return Exec (Exec'First .. Dot_Index - 1);
   end Get_Module;

   ---------
   -- Run --
   ---------

   procedure Run_Helper
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean);
   --  Shared code beteen Run/Start

   procedure Run_Helper
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean) is
   begin
      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= GNATCOLL.VFS.No_File
      then
         declare
            Module : constant String := Get_Module (Debugger.Executable);
         begin
            Send (Debugger, "-exec-arguments " & Module, Mode => Internal);
         end;

      else
         Send (Debugger, "-exec-arguments " & Arguments, Mode => Internal);
      end if;

      Send (Debugger, "-exec-run" & (if Start then " --start" else ""),
            Mode => Mode);
      Set_Is_Started (Debugger, True);
   end Run_Helper;

   overriding procedure Run
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => False);
   end Run;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => False);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   overriding procedure Step_Into
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   overriding procedure Step_Over
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-next", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   overriding procedure Step_Into_Instruction
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-step-instruction", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   overriding procedure Step_Over_Instruction
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-next-instruction", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-continue", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access Gdb_MI_Debugger) is
      Proxy      : constant Process_Proxy_Access := Get_Process (Debugger);
      Descriptor : constant Process_Descriptor_Access :=
                     Get_Descriptor (Proxy);
      use GVD;

   begin
      --  Should only do this when running under Windows, in native mode,
      --  with an external execution window, and the debuggee running.

      if Debugger.Debuggee_Pid /= 0           -- valid pid
        and then Is_Started (Debugger)        -- debuggee started
        and then Command_In_Process (Proxy)   -- and likely running
        and then Host = Windows               -- Windows host
        and then Is_Local (Debug_Server)      -- no remote debugging
        and then Debugger.Execution_Window    -- external window
      then
         GNAT.Expect.TTY.Interrupt (Debugger.Debuggee_Pid);
      else
         Interrupt (Descriptor.all);
         Set_Interrupted (Proxy);
      end if;
      --  Consider using -exec-interrupt when not Command_In_Process???
      --  Send (Debugger, "-exec-interrupt", Mode => Internal);
   end Interrupt;

   ------------------
   -- Command_Kind --
   ------------------

   overriding function Command_Kind
     (Debugger : access Gdb_MI_Debugger;
      Command  : String) return Command_Category
   is
      pragma Unreferenced (Command);
   begin
      return Debugger.Category;
   end Command_Kind;

   -------------------------
   -- Breakpoints_Changed --
   -------------------------

   overriding function Breakpoints_Changed
     (Debugger : access Gdb_MI_Debugger;
      Command  : String) return Boolean is
   begin
      return Debugger.Breakpoints_Changed
        or else Starts_With (Command, "-break");
   end Breakpoints_Changed;

   ----------------
   -- Stack_Down --
   ----------------

   overriding procedure Stack_Down
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      --  ??? Store current frame
      Send (Debugger, "down", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      --  ??? Store current frame
      Send (Debugger, "up", Mode => Mode);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access Gdb_MI_Debugger;
      Frame    : Positive;
      Mode     : Command_Type := Hidden)
   is
      Str : constant String := "frame" & Natural'Image (Frame - 1);
   begin
      --  ??? Store current frame
      Send (Debugger, Str, Mode => Mode);
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   overriding procedure Backtrace
     (Debugger : access Gdb_MI_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
      use Token_Lists;

      S          : constant String := Send_And_Get_Clean_Output
        (Debugger, "-stack-list-frames", Mode => Internal);
      List       : Token_List := Build_Tokens (S);
      C, C2, C3  : Token_Lists.Cursor;

   begin
      Len := 0;
      C := Find_Identifier (First (List), "stack");

      if C = No_Element then
         Clear_Token_List (List);
         return;
      end if;

      --  Skip stack=[
      Next (C, 3);

      loop
         C := Find_Identifier (C, "frame");

         exit when C = No_Element;

         Len := Len + 1;
         Next (C, 3);

         C2 := Find_Identifier (C, "level");
         Next (C2, 2);
         Value (Len).Frame_Id := Natural'Value (Element (C2).Text.all);

         C2 := Find_Identifier (C2, "addr");
         Next (C2, 2);
         Value (Len).Program_Counter := new String'(Element (C2).Text.all);

         C2 := Find_Identifier (C2, "func");
         Next (C2, 2);
         Value (Len).Subprogram := new String'(Element (C2).Text.all);

         C2 := Find_Identifier (C2, "fullname");
         Next (C2, 2);
         C3 := Find_Identifier (C2, "line");
         Next (C3, 2);
         Value (Len).Source_Location :=
           new String'
             (Strip_Escape
                (Element (C2).Text.all) & ':' & Element (C3).Text.all);
      end loop;

      Clear_Token_List (List);
   end Backtrace;

   -----------------------------
   -- Internal_Set_Breakpoint --
   -----------------------------

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_MI_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier
   is
      C : constant String := Debugger.Send_And_Get_Clean_Output
        (Cmd => Command, Mode => Mode);
      M : Match_Array (0 .. 1);
   begin
      Match (Breakpoint_Num_Pattern, C, Matches => M);
      if M (1) /= No_Match then
         return Breakpoint_Identifier'Value
           (C (M (1).First .. M (1).Last));
      else
         return No_Breakpoint;
      end if;
   end Internal_Set_Breakpoint;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   overriding function Break_Subprogram
     (Debugger  : access Gdb_MI_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert " & (if Temporary then "-t " else "")
         & Name, Mode => Mode);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   overriding function Break_Source
     (Debugger  : access Gdb_MI_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "-break-insert " & (if Temporary then "-t " else "")
         & (+Base_Name (File)) & ':' & Image (Integer (Line)),
         Mode => Mode);
   end Break_Source;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access Gdb_MI_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, File, Line, Mode);
   begin
      --  ??? Is there an equivalent to "clear" in gdb-mi ?
      null;
   end Remove_Breakpoint_At;

   ---------------------
   -- Break_Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access Gdb_MI_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "-catch-exception" & (if Temporary then " -t" else "") &
         (if Unhandled then " -u"
            elsif Name /= "" and then Name /= "all"
            then " -e " & Name else ""),
         Mode => Mode);
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   overriding function Break_Address
     (Debugger  : access Gdb_MI_Debugger;
      Address   : GVD.Types.Address_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert " & (if Temporary then "-t " else "")
         & "*" & Address_To_String (Address), Mode => Mode);
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   overriding function Break_Regexp
     (Debugger  : access Gdb_MI_Debugger;
      Regexp    : String;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      --  ????
      if Temporary then
         raise Unknown_Command;
         --  Error ("Temporary regexp breakpoints not supported");
      else
         return Internal_Set_Breakpoint
           (Debugger, "rbreak " & Regexp, Mode => Mode);
      end if;
   end Break_Regexp;

   ----------------------------
   -- Get_Last_Breakpoint_Id --
   ----------------------------

   overriding function Get_Last_Breakpoint_Id
     (Debugger : access Gdb_MI_Debugger) return Breakpoint_Identifier
   is
      S            : constant String := Send_And_Get_Clean_Output
         (Debugger, "print $bpnum", Mode => Internal);
      Error_String : constant String := "void";
      Index        : Integer := S'First;
   begin
      Skip_To_String (S, Index, Error_String);

      if Index <= S'Last - Error_String'Length + 1 then
         return 0;
      end if;

      Index := S'First;
      Skip_To_Char (S, Index, '=');

      return Breakpoint_Identifier'Value (S (Index + 1 .. S'Last));

   exception
      when Constraint_Error =>
         return 0;
   end Get_Last_Breakpoint_Id;

   ------------------------------
   -- Set_Breakpoint_Condition --
   ------------------------------

   overriding procedure Set_Breakpoint_Condition
     (Debugger  : access Gdb_MI_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "-break-condition" & Breakpoint_Identifier'Image (Num)
            & " " & Condition, Mode => Mode);
   end Set_Breakpoint_Condition;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   overriding procedure Set_Breakpoint_Command
     (Debugger : access Gdb_MI_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      --  ???? split commands in strings and quote them
      Send (Debugger, "-break-commands" & Breakpoint_Identifier'Image (Num)
            & Commands, Mode => Mode);
   end Set_Breakpoint_Command;

   ---------------------------------
   -- Set_Breakpoint_Ignore_Count --
   ---------------------------------

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Gdb_MI_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "-break-after" & Breakpoint_Identifier'Image (Num)
            & Integer'Image (Count), Mode => Mode);
   end Set_Breakpoint_Ignore_Count;

   -----------
   -- Watch --
   -----------

   overriding function Watch
     (Debugger  : access Gdb_MI_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      function Command return String;
      --  Returns the appropriate GDB watchpoint command based on the
      --  Trigger argument.

      -------------
      -- Command --
      -------------

      function Command return String is
      begin
         case Trigger is
            when GVD.Types.Read =>
               return "rwatch";
            when GVD.Types.Write =>
               return "watch";
            when GVD.Types.Read_Write =>
               return "awatch";
         end case;
      end Command;

   begin
      if Condition = "" then
         return Internal_Set_Breakpoint
           (Debugger, Command & " " & Name, Mode => Mode);
      else
         return Internal_Set_Breakpoint
           (Debugger,
            Command & " " & Name & " if " & Condition,
            Mode => Mode);
      end if;
   end Watch;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-exec-finish", Mode => Mode);
   end Finish;

   -----------------
   -- Task_Switch --
   -----------------

   overriding procedure Task_Switch
     (Debugger : access Gdb_MI_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "task" & Natural'Image (Task_Num), Mode => Mode);
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   overriding procedure Thread_Switch
     (Debugger : access Gdb_MI_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "thread" & Natural'Image (Thread), Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   overriding procedure PD_Switch
     (Debugger : access Gdb_MI_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "pd " & PD, Mode => Mode);
   end PD_Switch;

   ----------------
   -- Info_Tasks --
   ----------------

   overriding procedure Info_Tasks
     (Debugger : access Gdb_MI_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Info);
      Output  : constant String := Send_And_Get_Clean_Output
         (Debugger, "-ada-task-info", Mode => Internal);
      pragma Unreferenced (Output);
   begin
      Len := 0;
      --  ???? parse Output
   end Info_Tasks;

   ------------------
   -- Info_Threads --
   ------------------

   overriding procedure Info_Threads
     (Debugger : access Gdb_MI_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Info);
      Output : constant String := Send_And_Get_Clean_Output
        (Debugger, "-thread-info", Mode => Internal);
      pragma Unreferenced (Output);

   begin
      Len := 0;
      --  ???? implement
   end Info_Threads;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   overriding function Highlighting_Pattern
     (Debugger : access Gdb_MI_Debugger) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt (Debugger : access Gdb_MI_Debugger) is
      Proc : constant Visual_Debugger := Convert (Debugger);
   begin
      if Proc /= null then
         Output_Text
           (Proc,
            "(gdb) ",
            Is_Command => False,
            Set_Position => True);
      end if;
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   overriding procedure Change_Directory
     (Debugger : access Gdb_MI_Debugger;
      Dir      : Virtual_File;
      Mode     : Command_Type := Hidden)
   is
      Directory : constant String := +Dir.Unix_Style_Full_Name;
   begin
      Send (Debugger, "-environment-cd " & Directory, Mode => Mode);
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   Frame_Pattern : constant Pattern_Matcher :=
     Compile ("^\*stopped.*frame={(.*)}", Multiple_Lines);
   Subframe_Pattern : constant Pattern_Matcher :=
     Compile ("addr=""(0x[\da-hA-H]+)"",.*fullname=""(.*)"",line=""(\d+)""");

   overriding procedure Found_File_Name
     (Debugger    : access Gdb_MI_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      pragma Unreferenced (Debugger);
      Matched : Match_Array (0 .. 3);
   begin
      --  Default values if nothing better is found
      Name := Null_Unbounded_String;
      Line := 0;
      Addr := Invalid_Address;
      Match (Frame_Pattern, Str, Matched);

      if Matched (0) = No_Match then
         return;
      end if;

      Match
        (Subframe_Pattern,
         Str (Matched (1).First .. Matched (1).Last),
         Matched);

      if Matched (0) = No_Match then
         return;
      end if;

      Addr := String_To_Address (Str (Matched (1).First .. Matched (1).Last));
      Set_Unbounded_String
        (Name, Strip_Escape (Str (Matched (2).First .. Matched (2).Last)));
      Line := Integer'Value (Str (Matched (3).First .. Matched (3).Last));
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   overriding procedure Found_Frame_Info
     (Debugger : access Gdb_MI_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type)
   is
      pragma Unreferenced (Str);

   begin
      --  ??? Call Debugger.Gdb.Found_Frame_Info first and if no result
      --  then do the following:

      declare
         use Token_Lists;
         S : constant String := Send_And_Get_Clean_Output
           (Debugger, "-stack-info-frame", Mode => Internal);
         Tokens : Token_List := Build_Tokens (S);
         C      : Token_Lists.Cursor;

      begin
         C := Find_Identifier (First (Tokens), "level");

         if C = No_Element then
            Message := Location_Not_Found;
            Frame   := Null_Unbounded_String;
         else
            Next (C, 2);
            Set_Unbounded_String (Frame, Element (C).Text.all);
            C := Find_Identifier (C, "line");

            if C = No_Element then
               Message := No_Debug_Info;
            else
               Message := Location_Found;
            end if;
         end if;

         Clear_Token_List (Tokens);
      end;
   end Found_Frame_Info;

   -----------------------
   -- Source_Files_List --
   -----------------------

   overriding function Source_Files_List
     (Debugger : access Gdb_MI_Debugger) return GNAT.Strings.String_List
   is
      use Token_Lists;
      S        : constant String := Send_And_Get_Clean_Output
        (Debugger, "-file-list-exec-source-files", Mode => Internal);
      List     : Token_List := Build_Tokens (S);
      Start, C : Token_Lists.Cursor;
      Count    : Natural := 0;

   begin
      C := Find_Identifier (First (List), "files");

      if C = No_Element then
         Clear_Token_List (List);
         return (1 .. 0 => <>);
      end if;

      --  Skip files=[
      Next (C, 3);
      Start := C;

      --  Count number of files
      loop
         C := Find_Identifier (C, "fullname");
         exit when C = No_Element;
         Count := Count + 1;
         Next (C);
      end loop;

      declare
         Result : String_List (1 .. Count);
      begin
         C := Start;

         for J in 1 .. Count loop
            C := Find_Identifier (C, "fullname");
            Next (C, 2);
            Result (J) := new String'(Strip_Escape (Element (C).Text.all));
         end loop;

         Clear_Token_List (List);
         return Result;
      end;
   end Source_Files_List;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   overriding procedure List_Breakpoints
     (Debugger  : not null access Gdb_MI_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector)
   is
      use Token_Lists;
      S      : constant String := Send_And_Get_Clean_Output
        (Debugger, "-break-list", Mode => Internal);
      Tokens : Token_List := Build_Tokens (S);
      C, C2  : Token_Lists.Cursor;
      Start  : Token_Lists.Cursor;

   begin
      List.Clear;

      C := Find_Identifier (First (Tokens), "body");

      if C = No_Element then
         Clear_Token_List (Tokens);
         return;
      end if;

      --  Skip body=[
      Next (C, 3);
      Start := C;

      C2 := Start;

      loop
         C2 := Find_Identifier (C2, "number");
         exit when C2 = No_Element;
         Next (C2, 2);

         declare
            B    : Breakpoint_Data;
            F    : Virtual_File;
            Line : Editable_Line_Type;
         begin
            B.Num := Breakpoint_Identifier'Value (Element (C2).Text.all);

            C2 := Find_Identifier (C2, "type");
            Next (C2, 2);
            B.The_Type := Breakpoint_Type'Value (Element (C2).Text.all);

            C2 := Find_Identifier (C2, "disp");
            Next (C2, 2);
            B.Disposition :=
              Breakpoint_Disposition'Value (Element (C2).Text.all);

            C2 := Find_Identifier (C2, "enabled");
            Next (C2, 2);
            B.Enabled := Element (C2).Text.all = "y";

            C2 := Find_Identifier (C2, "addr");
            Next (C2, 2);
            B.Address := String_To_Address (Element (C2).Text.all);

            --  ??? missing Trigger & Expression for watchpoints
            --  ??? missing Except

            Next (C2, 2);

            if B.The_Type = Breakpoint then
               F    := No_File;
               Line := 0;

               loop
                  declare
                     Text : constant Ada.Strings.Unbounded.String_Access :=
                       Element (C2).Text;
                  begin
                     if Text.all = "func" then
                        Next (C2, 2);
                        B.Subprogram :=
                          To_Unbounded_String (Element (C2).Text.all);
                     elsif Text.all = "file" then
                        Next (C2, 2);
                        F := Debugger.Get_Kernel.Create_From_Base
                          (+Element (C2).Text.all);

                     elsif Text.all = "fullname" then
                        Next (C2, 2);
                        F := To_File
                           (Kernel, Strip_Escape (Element (C2).Text.all));

                     elsif Text.all = "line" then
                        Next (C2, 2);
                        Line :=
                          Editable_Line_Type'Value (Element (C2).Text.all);

                     elsif Text.all = "what" then
                        Next (C2, 2);
                        B.Expression :=
                          To_Unbounded_String (Element (C2).Text.all);
                     else
                        exit;
                     end if;
                  end;

                  Next (C2, 2);
               end loop;

               --  Convert from a path returned by the debugger to the actual
               --  path in the project, in case sources have changed.

               if not F.Is_Absolute_Path or else not F.Is_Regular_File then
                  F := Debugger.Kernel.Create_From_Base (F.Full_Name);
               end if;

               if Line /= 0 then
                  B.Location := Kernel.Get_Buffer_Factory.Create_Marker
                    (File   => F,
                     Line   => Line,
                     Column => 1);
               end if;

            else  -- Watchpoint
               C2 := Find_Identifier (C2, "what");
               Next (C2, 2);
               B.Expression := To_Unbounded_String (Element (C2).Text.all);
            end if;

            Next (C2, 2);

            loop
               exit when Element (C2).Code /= Identifier;

               declare
                  Text : constant Ada.Strings.Unbounded.String_Access :=
                    Element (C2).Text;
               begin
                  Next (C2, 2);

                  if Text.all = "cond" then
                     B.Condition :=
                       To_Unbounded_String (Element (C2).Text.all);
                  elsif Text.all = "ignore" then
                     B.Ignore := Integer'Value (Element (C2).Text.all);
                  end if;
                  --  ??? missing Commands
               end;

               Next (C2, 2);
            end loop;

            List.Append (B);
         end;
      end loop;

      Clear_Token_List (Tokens);

   exception
      when Constraint_Error =>
         Clear_Token_List (Tokens);
   end List_Breakpoints;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   overriding procedure Enable_Breakpoint
     (Debugger : access Gdb_MI_Debugger;
      Num      : Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden) is
   begin
      if Enable then
         Send (Debugger, "-break-enable" & Breakpoint_Identifier'Image (Num),
               Mode => Mode);
      else
         Send (Debugger, "-break-disable" & Breakpoint_Identifier'Image (Num),
               Mode => Mode);
      end if;
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   overriding procedure Remove_Breakpoint
     (Debugger : access Gdb_MI_Debugger;
      Num      : Breakpoint_Identifier;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "-break-delete" & Breakpoint_Identifier'Image (Num),
            Mode => Mode);
   end Remove_Breakpoint;

   ---------------------
   -- List_Exceptions --
   ---------------------

   overriding function List_Exceptions
     (Debugger : access Gdb_MI_Debugger)
     return GVD.Types.Exception_Array
   is
      S     : constant String := Send_And_Get_Clean_Output
        (Debugger, "info exceptions", Mode => Internal);
      --  ??? replace with -info-ada-exceptions:

      --  -info-ada-exceptions
      --  ^done,ada-exceptions=
      --   {nr_rows="2",nr_cols="2",
      --    hdr=[{width="1",alignment="-1",col_name="name",colhdr="Name"},
      --       {width="1",alignment="-1",col_name="address",colhdr="Address"}],
      --    body=[{name="global_exceptions.a_global_exception",
      --           address="0x0000000000613a80"},
      --          {name="global_exceptions.a_private_exception",
      --           address="0x0000000000613ac0"}]}

      Nums  : Natural := 0;
   begin
      --  Count the number of exceptions listed
      for J in S'Range loop
         if S (J) = ASCII.LF then
            Nums := Nums + 1;
         end if;
      end loop;

      --  Ignore the first line ("All defined exceptions")
      if Nums = 0 then
         declare
            Arr : Exception_Array (1 .. 0);
         begin
            return Arr;
         end;
      end if;
      Nums := Nums - 1;

      declare
         Arr   : Exception_Array (1 .. Nums);
         Index : Natural := S'First;
         Num   : Natural := 1;
         Start : Natural;
      begin
         if Nums <= 0 then
            return Arr;
         end if;

         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Index <= S'Last and then Num <= Nums loop
            Start := Index;
            Skip_To_Char (S, Index, ':');
            Arr (Num).Name := To_Unbounded_String (S (Start .. Index - 1));
            Skip_To_Char (S, Index, ASCII.LF);
            Index := Index + 1;
            Num := Num + 1;
         end loop;

         return Arr;
      end;
   end List_Exceptions;

   -------------------
   -- Get_Type_Info --
   -------------------

   overriding function Get_Type_Info
     (Debugger  : access Gdb_MI_Debugger;
      Entity    : String;
      Default   : String) return String
   is
      S : constant String := Send_And_Get_Clean_Output
        (Debugger, "whatis " & Entity, Mode => Internal);

   begin
      if S'Length > 6
        and then S (S'First .. S'First + 5) = "type ="
      then
         return S (S'First + 7 .. S'Last);
      else
         return Default;
      end if;
   end Get_Type_Info;

   ---------------
   -- Find_File --
   ---------------

   overriding function Find_File
     (Debugger : access Gdb_MI_Debugger; File_Name : String) return String is
   begin
      --  Given that we no longer process graphic events when sending
      --  commands to the debugger, Command_In_Process should never be true
      --  here, but in any case, let's have this additional protection:

      if Command_In_Process (Get_Process (Debugger)) then
         return File_Name;
      end if;

      return File_Name;
      --  ???? do we need to implement this function
   end Find_File;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_MI_Debugger;
      Range_Start   : out GVD.Types.Address_Type;
      Range_End     : out Address_Type;
      Code          : out GNAT.Strings.String_Access;
      Start_Address : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address)
   is
      function Get_Disassembled return String;
      --  Return the output of the appropriate "disassemble" command

      function Raw_Disassembled (Separator : String) return String;
      --  Return the output of "disassemble" command without postprocessing

      function Code_Address_To_String (Address : Address_Type) return String;
      --  return address with explicit convertion to code

      ----------------------------
      -- Code_Address_To_String --
      ----------------------------

      function Code_Address_To_String (Address : Address_Type) return String is
      begin
         return "(void (*)())" & Address_To_String (Address);
      end Code_Address_To_String;

      ----------------------
      -- Get_Disassembled --
      ----------------------

      function Get_Disassembled return String is
         Version : constant Version_Number := Get_GDB_Version (Debugger);

      begin
         if Version.Major > 7
           or else
             (Version.Major = 7
              and then Version.Minor >= 1)
         then
            declare
               S : constant String := Raw_Disassembled (", ");
               A : constant Unbounded_String_Array := Split (S, ASCII.LF);
               R : Unbounded_String;
            begin
               --  Process the output to strip the blanks or the "=>" leading
               --  to the addresses
               R := A (A'First) & ASCII.LF;
               for J in A'First + 1 .. A'Last - 1 loop
                  declare
                     T : constant String := To_String (A (J));
                     N : Natural := T'First;
                  begin
                     Skip_To_String (T, N, "0x");
                     R := R & T (N .. T'Last) & ASCII.LF;
                  end;
               end loop;
               R := R & A (A'Last);
               return To_String (R);
            end;

         else
            return Raw_Disassembled (" ");
         end if;
      end Get_Disassembled;

      ----------------------
      -- Raw_Disassembled --
      ----------------------

      function Raw_Disassembled (Separator : String) return String is
      begin
         Switch_Language (Debugger, "c");

         declare
            S : constant String := Send_And_Get_Clean_Output
              (Debugger,
               "disassemble " &
                 Code_Address_To_String (Start_Address) & Separator &
                 Code_Address_To_String (End_Address),
               Mode => Internal);
         begin
            Restore_Language (Debugger);
            return S;
         end;
      end Raw_Disassembled;

      Disassembled : constant String := Get_Disassembled;
      Tmp,
      Start_Index,
      End_Index    : Integer;

   begin
      Start_Index := Disassembled'First;
      Skip_To_Char (Disassembled, Start_Index, ASCII.LF);
      Start_Index := Start_Index + 1;

      End_Index := Disassembled'Last;
      Skip_To_Char (Disassembled, End_Index, ASCII.LF, Step => -1);
      End_Index := End_Index - 1;

      --  Gdb always return a leading and tailing line, which we don't want
      --  to return.

      Code := new String'(Disassembled (Start_Index .. End_Index));

      --  If there is nothing left, this means gdb couldn't disassemble that
      --  part.
      --  For instance: "No function contains specified address" is returned
      --  when the user program wasn't compiled with -g.

      if Code.all = "" then
         Range_Start := Invalid_Address;
         Range_End := Invalid_Address;
         return;
      end if;

      --  Always read the actual start and end address from the output of
      --  gdb, in case gdb didn't start disassembling at the exact location,

      Tmp := Start_Index;
      Skip_To_Char (Disassembled, Tmp, ' ');

      if Start_Index < Disassembled'Last then
         Range_Start :=
           String_To_Address (Disassembled (Start_Index .. Tmp - 1));
      else
         Range_Start := Invalid_Address;
      end if;

      --  Get the actual end address, in case the disassembled zone doesn't end
      --  exactly on End_Address.
      --  The output line from gdb can look like:
      --       0x379214 <_ada_main+260>:\tmr\tr1,r11
      --    or 0x379250:\tstfd\tf14,160(r1)

      Skip_To_Char (Disassembled, End_Index, ASCII.LF, Step => -1);
      End_Index := End_Index + 1;
      Tmp := End_Index;
      while Tmp <= Disassembled'Last
        and then Disassembled (Tmp) /= ' '
        and then Disassembled (Tmp) /= ':'
      loop
         Tmp := Tmp + 1;
      end loop;
      --  Skip_To_Char (Disassembled, Tmp, ' ');

      if End_Index < Disassembled'Last then
         Range_End := String_To_Address (Disassembled (End_Index .. Tmp - 1));
      else
         Range_End := Invalid_Address;
      end if;
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   overriding procedure Get_Line_Address
     (Debugger    : access Gdb_MI_Debugger;
      Line        : Natural;
      Range_Start : out Address_Type;
      Range_End   : out Address_Type)
   is
      pragma Unreferenced (Debugger, Line);
   begin
      Range_Start := Invalid_Address;
      Range_End   := Invalid_Address;
      --  ???? implement
   end Get_Line_Address;

   ----------------
   -- Get_Memory --
   ----------------

   overriding function Get_Memory
     (Debugger : access Gdb_MI_Debugger;
      Size     : Integer;
      Address  : String) return Memory_Dump_Access
   is
      procedure Get_Label
        (Text  : String;
         From  : in out Positive;
         Value : out Unbounded_String);
      --  Scan Text starting from From position and search for label.
      --  If found put label into Value.

      subtype Gigantic_Word is String (1 .. 16);

      function Swap (Dump : String) return Gigantic_Word;
      --  Swap bytes in Dump if little endian

      Endian : constant Endian_Type := Get_Endian_Type (Debugger);

      ---------------
      -- Get_Label --
      ---------------

      procedure Get_Label
        (Text  : String;
         From  : in out Positive;
         Value : out Unbounded_String)
      is
         --  We expect Text in the form: "address <label> : 0x...", for example
         --  0x1234567 <label+123>: 0xff
         Start : Positive := Text'First;
      begin
         while From <= Text'Last loop
            if Text (From) = '<' then
               Start := From + 1;
            elsif Text (From) = '>' then
               Value := To_Unbounded_String (Text (Start .. From - 1));
               return;
            elsif Text (From) = ':' then
               return;
            end if;

            From := From + 1;
         end loop;
      end Get_Label;

      ----------
      -- Swap --
      ----------

      function Swap (Dump : String) return Gigantic_Word is
      begin
         if Endian = Little_Endian then
            declare
               Result : Gigantic_Word;
            begin
               for J in 1 .. 8 loop
                  Result (J * 2 - 1 .. J * 2) :=
                    Dump (Dump'Last - J * 2 + 1 .. Dump'Last - J * 2 + 2);
               end loop;

               return Result;
            end;
         else
            return Dump;
         end if;
      end Swap;

      Dump_Item_Size : constant := 16;
      --  Expected size of an Memory_Dump_Item.Value

      Error_String : constant String := "Cannot access memory at";
      Image        : constant String := Integer'Image (Size / 8);
      S              : GNAT.OS_Lib.String_Access := new String'
        (Send_And_Get_Clean_Output
           (Debugger,
            "x/" & Image (Image'First + 1 .. Image'Last)
            & "gx " & Address, Mode => Internal));
      S_Index      : Integer := S'First + 2;
      Last_Index   : Integer := S'First + 2;
      Result       : constant Memory_Dump_Access :=
        new Memory_Dump (1 .. (Size + Dump_Item_Size - 1) / Dump_Item_Size);
      Result_Index : Integer := Result'First;
      Last         : Integer := S'Last;
      Total        : Integer := 0;
      Has_Error    : Boolean := False;
   begin
      --  Detect "Cannot access memory at ..."

      Skip_To_String (S.all, Last_Index, Error_String);

      if Last_Index <= S'Last - Error_String'Length then
         --  The error string was detected...
         Has_Error := True;
         Last := Last_Index;
      end if;

      if Has_Error then
         --  We may have missed some bytes here due to the fact that we were
         --  fetching gigantic words. Now try to fetch memory using byte units.

         Free (S);

         declare
            Image : constant String := Integer'Image (Size);
         begin
            S := new String'(Send_And_Get_Clean_Output
              (Debugger,
                 "x/" & Image (Image'First + 1 .. Image'Last)
                 & "b " & Address, Mode => Internal));

            S_Index := S'First + 2;
            Last_Index := S'First + 2;
            Last := S'Last;

            --  Detect "Cannot access memory at ..."

            Skip_To_String (S.all, Last_Index, Error_String);

            if Last_Index <= S'Last - Error_String'Length then
               --  The error string was detected...
               Last := Last_Index;
            end if;

            Get_Label (S.all, S_Index, Result (Result_Index).Label);

            while S_Index <= Last loop

               --  Detect actual data : 0xXX... right after an ASCII.HT
               if S (S_Index) = '0' and S (S_Index - 1) = ASCII.HT then
                  Append (Result (Result_Index).Value,
                          S (S_Index + 2 .. S_Index + 3));
                  Total := Total + 1;
               elsif S (S_Index) = ASCII.LF and then
                 Length (Result (Result_Index).Value) >= Dump_Item_Size * 2
               then
                  --  If new line and we have collected enought bytes
                  --  Read label in new string
                  Result_Index := Result_Index + 1;

                  if Result_Index in Result'Range then
                     Get_Label (S.all, S_Index, Result (Result_Index).Label);
                  end if;
               end if;

               S_Index := S_Index + 1;
            end loop;
         end;
      else
         --  Read label: 0x1234567 <label+123>: 0xff
         Get_Label (S.all, S_Index, Result (Result_Index).Label);

         while S_Index <= Last loop
            --  Detect actual data : 0xXX... right after an ASCII.HT

            if S (S_Index) = '0' and S (S_Index - 1) = ASCII.HT then
               Append (Result (Result_Index).Value,
                       Swap (S (S_Index + 2 .. S_Index + 17)));
               Total := Total + 8;
            elsif S (S_Index) = ASCII.LF then
               --  Read label in new string
               Result_Index := Result_Index + 1;

               if Result_Index in Result'Range then
                  Get_Label (S.all, S_Index, Result (Result_Index).Label);
               end if;
            end if;

            S_Index := S_Index + 1;
         end loop;
      end if;

      --  Fill the values that could not be accessed with "-"
      while Total < Size loop
         if Length (Result (Result_Index).Value) >= Dump_Item_Size * 2 then
            Result_Index := Result_Index + 1;
         end if;

         Append (Result (Result_Index).Value, "--");

         Total := Total + 1;
      end loop;

      Free (S);
      return Result;

   exception
      when Constraint_Error =>
         return null;
   end Get_Memory;

   ---------------------
   -- Put_Memory_Byte --
   ---------------------

   overriding procedure Put_Memory_Byte
     (Debugger : access Gdb_MI_Debugger;
      Address  : String;
      Byte     : String) is
   begin
      Switch_Language (Debugger, "c");
      Send (Debugger, "set {char}" & Address & " = 0x" & Byte,
            Mode => Internal);
      Restore_Language (Debugger);
   end Put_Memory_Byte;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   overriding function Get_Variable_Address
     (Debugger : access Gdb_MI_Debugger;
      Variable : String) return String
   is
      S         : constant String :=
        Send_And_Get_Clean_Output
          (Debugger, "print &(" & Variable & ")",
           Mode => Internal);
      Index     : Integer := S'Last;
      Error_Msg : constant String := "No ";
      --  Error messages can be "No definition..." or "No symbol..."

   begin
      if S (S'First .. S'First + Error_Msg'Length - 1) = Error_Msg then
         return "";
      end if;

      --  Find the last occurence of "0x" in the string
      loop
         Skip_To_Char (S, Index, 'x', Step => -1);

         --  No address found in the string ?
         if Index <= S'First then
            return "";
         end if;

         Index := Index - 1;
         exit when S (Index) = '0';
      end loop;

      return S (Index .. S'Last);
   end Get_Variable_Address;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   overriding function Get_Endian_Type
     (Debugger : access Gdb_MI_Debugger) return Endian_Type is
   begin
      if Debugger.Endian /= Unknown_Endian then
         --  Return the cached value, to avoid too much communication with
         --  the underlying debugger.
         return Debugger.Endian;
      end if;

      declare
         S      : constant String :=
           Send_And_Get_Clean_Output
             (Debugger, "show endian", Mode => Internal);
         Little : constant String := "little endian";

      begin
         if Index (S, Little) > 0 then
            Debugger.Endian := Little_Endian;
         else
            Debugger.Endian := Big_Endian;
         end if;

         return Debugger.Endian;
      end;
   end Get_Endian_Type;

   --------------
   -- Complete --
   --------------

   overriding function Complete
     (Debugger  : access Gdb_MI_Debugger;
      Beginning : String) return GNAT.Strings.String_List
   is
      S           : constant String :=
                      Send_And_Get_Clean_Output
                        (Debugger, "complete " & Beginning, Mode => Internal);
      First_Index : Integer := S'First;
      Last_Index  : Integer := S'First;
      Num         : Integer := 0;

   begin
      --  Find the number of words in the list

      for Index in S'Range loop
         if S (Index) = ASCII.LF then
            Num := Num + 1;
         end if;
      end loop;

      if S'Length /= 0 then
         Num := Num + 1;
      end if;

      --  Fill the string array with the proper values
      declare
         Result : GNAT.Strings.String_List (1 .. Num);
      begin
         for Index in 1 .. Num loop
            while S (Last_Index) /= ASCII.LF and then Last_Index < S'Last loop
               Last_Index := Last_Index + 1;
            end loop;

            if Last_Index = S'Last then
               Last_Index := Last_Index + 1;
            end if;

            Result (Index) := new String'(S (First_Index .. Last_Index - 1));
            Last_Index  := Last_Index + 1;
            First_Index := Last_Index;
         end loop;

         return Result;
      end;
   end Complete;

   use GVD.Proc_Utils;

   --------------------
   -- Open_Processes --
   --------------------

   overriding procedure Open_Processes (Debugger : access Gdb_MI_Debugger) is
   begin
      Open_Processes (Debugger.Handle, Debugger.Kernel);
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   overriding procedure Next_Process
     (Debugger : access Gdb_MI_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean) is
   begin
      Next_Process (Debugger.Handle, Info, Success);
   end Next_Process;

   ---------------------
   -- Close_Processes --
   ---------------------

   overriding procedure Close_Processes (Debugger : access Gdb_MI_Debugger) is
   begin
      Close_Processes (Debugger.Handle);
   end Close_Processes;

   ---------------------
   -- Detect_Language --
   ---------------------

   overriding procedure Detect_Language (Debugger : access Gdb_MI_Debugger) is
      S : constant String := Send_And_Get_Clean_Output
        (Debugger, "show lang", Mode => Internal);
      pragma Unreferenced (S);
   begin
      null;
   end Detect_Language;

   ---------------------
   -- Switch_Language --
   ---------------------

   procedure Switch_Language
     (Debugger : access Gdb_MI_Debugger;
      Language : String)
   is
      S           : constant String := Send_And_Get_Clean_Output
        (Debugger, "show lang", Mode => Internal);
      First_Index : Integer := S'First;
      End_Index   : Integer;

   begin
      Free (Debugger.Stored_Language);
      Skip_To_Char (S, First_Index, '"');
      End_Index := First_Index + 1;

      while S (End_Index) /= '"' and then S (End_Index) /= ';' loop
         End_Index := End_Index + 1;
      end loop;

      Debugger.Stored_Language :=
        new String'(S (First_Index + 1 .. End_Index - 1));

      Send (Debugger, "set lang " & Language, Mode => Internal);
   end Switch_Language;

   ----------------------
   -- Restore_Language --
   ----------------------

   procedure Restore_Language (Debugger : access Gdb_MI_Debugger) is
   begin
      if Debugger.Stored_Language /= null then
         Send (Debugger, "set lang " & Debugger.Stored_Language.all);
      end if;
   end Restore_Language;

   -----------------
   -- Support_TTY --
   -----------------

   overriding function Support_TTY
     (Debugger : access Gdb_MI_Debugger) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return True;
   end Support_TTY;

   -------------
   -- Set_TTY --
   -------------

   overriding procedure Set_TTY
     (Debugger : access Gdb_MI_Debugger; TTY : String) is
   begin
      if TTY /= "" then
         Send (Debugger, "-inferior-tty-set " & TTY, Mode => Hidden);
      end if;
   end Set_TTY;

   overriding procedure Filter_Output
     (Debugger : access Gdb_MI_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String)
   is
      pragma Unreferenced (Debugger);
      J         : Integer;
      First     : Integer;
      New_Line  : Boolean := True;

   begin
      J := Str'First;

      while J <= Str'Last loop
         if New_Line and then J + 2 < Str'Last then
            if Str (J) = '~'
              and then Str (J + 1) = '"'
            then
               --  Replace ~"...." by ....

               J := J + 2;
               First := J;

               while J <= Str'Last and then Str (J) /= '"' loop
                  if Str (J) /= '\' then
                     Append (Result, Str (J));
                  else
                     J := J + 1;
                     case Str (J) is
                        when '\' | '"' =>
                           Append (Result, Str (J));
                        when 'n' | 'r' =>
                           null;
                        when others =>
                           Append (Result, Str (J - 1 .. J));
                     end case;
                  end if;

                  J := J + 1;
               end loop;

            elsif Str (J) = '&'
              and then Str (J + 1) = '"'
            then
               --  Strip &"..."
               J := J + 2;

               while J <= Str'Last and then Str (J) /= ASCII.LF loop
                  J := J + 1;
               end loop;

            elsif Mode = User
              and then J + 4 <= Str'Last
              and then Str (J .. J + 4) = "^done"
            then
               --  Strip "^done[,]", keep the rest (result expected by user)
               if Str'Last > J + 4
                 and then Str (J + 5) = ','
               then
                  J := J + 5;
               else
                  J := J + 4;
               end if;

            elsif Str (J) in '^' | '*' | '=' then
               --  Strip [^*=]...
               J := J + 1;

               if J + 12 < Str'Last
                 and then Str (J .. J + 10) = "error,msg="""
               then
                  J := J + 11;
                  First := J;

                  while J <= Str'Last and then Str (J) /= '"' loop
                     J := J + 1;
                  end loop;

                  if J <= Str'Last then
                     Append (Result, Str (First .. J - 1));
                  else
                     Append (Result, Str (First .. Str'Last));
                  end if;
               else
                  if J + 6 < Str'Last
                    and then Str (J .. J + 6) = "stopped"
                  then
                     --  Display info to the user
                     Append (Result, "[program stopped");
                     J := J + 7;

                     if J + 8 < Str'Last
                       and then Str (J .. J + 8) = ",reason="""
                     then
                        Append (Result, ": ");
                        J := J + 9;

                        while J <= Str'Last and then Str (J) /= '"' loop
                           Append (Result, Str (J));
                           J := J + 1;
                        end loop;
                     end if;

                     Append (Result, "]" & ASCII.LF);
                  end if;

                  while J <= Str'Last and then Str (J) /= ASCII.LF loop
                     J := J + 1;
                  end loop;
               end if;
            else
               Append (Result, Str (J));
            end if;
         elsif J = Str'Last
           and then Str (J) = ASCII.LF
           and then J - 6 >= Str'First
           and then Str (J - 6 .. J - 1) = "(gdb) "
         then
            --  Strip last LF after prompt
            null;
         else
            Append (Result, Str (J));
         end if;

         New_Line := J <= Str'Last and then Str (J) = ASCII.LF;
         J := J + 1;
      end loop;
   end Filter_Output;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   overriding function Is_Quit_Command
     (Debugger : access Gdb_MI_Debugger;
      Command : String) return Boolean
   is
      pragma Unreferenced (Debugger);
      Quit     : constant String := "quit     ";
      Gdb_Exit : constant String := "-gdb-exit     ";
   begin
      if (Command'Length <= Quit'Length
          and then Command = Quit (1 .. Command'Length))
        or else (Command'Length <= Gdb_Exit'Length
                 and then Command = Gdb_Exit (1 .. Command'Length))
      then
         return True;
      else
         return False;
      end if;
   end Is_Quit_Command;

end Debugger.Gdb_MI;

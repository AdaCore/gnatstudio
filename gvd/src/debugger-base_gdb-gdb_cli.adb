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

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Config;                    use Config;
with Debugger.Base_Gdb.Ada;     use Debugger.Base_Gdb.Ada;
with Default_Preferences;       use Default_Preferences;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Trace;                 use GVD.Trace;
with GVD.Types;                 use GVD.Types;
with Language;                  use Language;
with Process_Proxies;           use Process_Proxies;
with Remote;                    use Remote;
with String_Utils;              use String_Utils;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;

package body Debugger.Base_Gdb.Gdb_CLI is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp             : constant Pattern_Matcher :=
     Compile ("^>*\(([^\s]*-)?gdb\) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_String             : constant String := "(gdb) ";
   --  The prompt used by the debugger

   Gdb_Command               : constant String := "gdb";
   --  Name of the command to launch gdb

   Gdb_Options_NW             : aliased String := "-nw";
   Gdb_Options_Q              : aliased String := "-q";
   Gdb_Options_Ex             : aliased String := "-ex";
   Gdb_Options_Set_Prompt     : aliased String := "set prompt (gdb) ";
   Gdb_Options_Set_Ext_Prompt : aliased String :=
      "set extended-prompt (gdb) ";
   Gdb_Options                : constant Argument_List :=
     (Gdb_Options_NW'Access,
      Gdb_Options_Q'Access,
      Gdb_Options_Ex'Access,
      Gdb_Options_Set_Prompt'Access,
      Gdb_Options_Ex'Access,
      Gdb_Options_Set_Ext_Prompt'Access);
   --  Options always passed to gdb: run GDB in terminal mode, keep it quiet
   --  and override any user-defined prompt so that we can match the first
   --  prompt.

   Highlight_Pattern         : constant Pattern_Matcher :=
     Compile ("^\(([^\s]*-)?gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window

   File_Name_Pattern         : constant Pattern_Matcher :=
     Compile (ASCII.SUB & ASCII.SUB
              & "(.+):(\d+):\d+:[^:]+:(0x[0-9a-f]+)$", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output

   File_Name_Pattern2        : constant Pattern_Matcher :=
     Compile ("^(\d+)" & ASCII.HT &
              "(in (.+)|(.+): No such file or directory.)",
              Multiple_Lines);
   --  Second regexp used to detect when the current frame can not be displayed
   --  Note that this pattern should work even when LANG isn't english because
   --  gdb does not seem to take into account this variable at all.

   Language_Pattern          : constant Pattern_Matcher := Compile
     ("^(The current source language is|Current language:) +" &
      """?(auto; currently )?([^""\s]+)("".)?\n", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger

   Terminate_Pattern         : constant Pattern_Matcher := Compile
     ("^Program exited (normally|with code)", Multiple_Lines);
   --  Pattern used to detect when the debuggee terminates

   Running_Pattern           : constant Pattern_Matcher := Compile
     ("^The program is not being run.", Multiple_Lines);
   --  Pattern used to detect when the debuggee is not running

   Frame_Pattern             : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))?$", Multiple_Lines);
   --  Regular expression used to detect and parse callstack frames

   Frame_Pattern_With_File   : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))$", Multiple_Lines);
   --  Regular expression used to detect and parse callstack frames
   --  with no file information

   Breakpoint_Num_Pattern    : constant Pattern_Matcher := Compile
     ("(?:Breakpoint|Watchpoint|Catchpoint) (\d+)");
   --  Extract breakpoint identifier after setting a breakpoint via "break"
   --  or "tbreak"

   Breakpoint_Pattern        : constant Pattern_Matcher := Compile
     ("^(\d+)\s+(breakpoint|\w+? watchpoint|catchpoint)\s+"
      & "(keep|dis|del)\s+([yn])"
      & "\s+(<MULTIPLE>\s+)*((0x0*)?(\S+))?\s+(.*)$",
      Multiple_Lines);
   --  Pattern to match a single line in "info breakpoint"

   Multiloc_Breakpoint_Instance_Pattern : constant Pattern_Matcher := Compile
     ("^\d+\.\d+\s+[yn]\s+(.*)$", Multiple_Lines);

   File_Name_In_Breakpoint   : constant Pattern_Matcher := Compile
     ("\bat (.+):(\d+)$", Multiple_Lines);
   --  How to find file names in the info given by "info breakpoint".
   --  Note that we have to allow for special characters in the directory
   --  or file name, since the user might be using some strange names. The only
   --  restriction is that the name can not contain newline characters.

   Exception_In_Breakpoint   : constant Pattern_Matcher := Compile
     ("\b(on (exception ([-\w_:]+)|all|unhandled))" &
      "|((`([-\w_:]+)'|all|unhandled) Ada exception)");
   --  How to detect exception names in the info given by "info breakpoint"

   Subprogram_In_Breakpoint  : constant Pattern_Matcher := Compile
     ("\bin (\S+)");
   --  How to detect subprogram names in the info given by "info breakpoint"

   Condition_In_Breakpoint   : constant Pattern_Matcher := Compile
     ("^\t?stop only if (.*)");
   --  How to detect breakpoint conditions in "info breakpoint"

   Ignore_In_Breakpoint      : constant Pattern_Matcher := Compile
     ("^\t?ignore next (\d+) hits");
   --  How to detect the ignore count in "info breakpoint"

   Breakpoint_Extra_Info     : constant Pattern_Matcher := Compile
     ("^(\d+)\s+(task|pd|any)\s+(task|pd|all)(.*)$", Multiple_Lines);
   --  Pattern to match a single line in "info
   --  breakpoints-extra-info"

   Continuation_Line_Pattern : constant Pattern_Matcher := Compile ("^ ?>");

   Address_Range_Pattern     : constant Pattern_Matcher := Compile
     ("starts at address (0x[0-9a-f]+) <[^>]+> and ends at (0x[0-9a-f]+)");
   --  How to get the range of addresses for a given line

   GNAT_Binder_File_Pattern  : constant Pattern_Matcher := Compile
     ("(b(~|_).+\.(adb|c))");

   Version_Pattern : constant Pattern_Matcher := Compile
     ("^GNU gdb( \(GDB\))? ([0-9]+)\.([0-9]+)(.[0-9]+)? .*");
   --  To detect the version of GDB

   Undefined_Info_Command    : constant String := "Undefined info command";
   --  Another string used to detect undefined info commands

   Info_Tasks_Pattern : constant Pattern_Matcher := Compile
     ("^ +(ID) +(TID) +(P-ID) +(Pri) +(State) +(Name).*$");
   --  Used to parse the output of "info tasks"

   Question_Filter_Pattern1  : constant Pattern_Matcher :=
     Compile ("^\[0\] .*> ", Multiple_Lines + Single_Line);

   Question_Filter_Pattern2  : constant Pattern_Matcher :=
     Compile ("^(.*\?) \(y or n\) ", Multiple_Lines);
   --  How to detect a question in gdb's output

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the program no longer runs

   procedure Continuation_Line_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect commands handled on multiple lines

   procedure Parse_Backtrace_Info
     (S     : String;
      Value : out Backtrace_Array;
      Len   : out Natural);
   --  Parse all the lines in S.
   --  These lines should contain the info returned either by "where"
   --  or "frame".
   --  Value'First will contain the value described in the first line, and
   --  so on.

   procedure Set_Args
     (Debugger  : access Gdb_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden);
   --  Set the debuggee arguments to Arguments

   procedure Switch_Language
     (Debugger : access Gdb_Debugger;
      Language : String);
   --  Switch gdb to another language. The possible values for Language are:
   --  "ada", "c", "c++", "asm", "chill", "fortran", "java", "modula-2",
   --  "scheme".
   --  When calling this function, the current language is stored internally
   --  and can be restored by calling Restore_Language.

   procedure Restore_Language
     (Debugger : access Gdb_Debugger);
   --  Restore the language that was active before Switch_Language was called

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier;
   --  Send a command that sets a breakpoint, and retrieve the id of the newly
   --  created breakpoint

   function Get_Module (Executable : GNATCOLL.VFS.Virtual_File) return String;
   --  Return the name of the module contained in Executable
   --  Assume that the name of the module is the executable file
   --  with no path information and no extension.

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_Debugger);
   --  Connect to the target if not already connected

   function Get_GDB_Version
     (Debugger : access Gdb_Debugger) return Version_Number;
   --  Return the GDB version number.

   function Break_Exception_Cmd
     (Debugger  : access Gdb_Debugger;
      Name      : String := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String;
   --  Return the GDB command corresponding to a break on exception, given
   --  Name/Temporary/Unhandled.

   -------------------------
   -- Break_Exception_Cmd --
   -------------------------

   function Break_Exception_Cmd
     (Debugger  : access Gdb_Debugger;
      Name      : String := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String
   is
      Break   : aliased constant String := "break";
      Catch   : aliased constant String := "catch";
      Command : access constant String := Break'Access;

      use GNATCOLL.Tribooleans;

   begin
      if Debugger.Use_Catch_For_Exceptions = Indeterminate then
         --  Check whether we should use "catch" or "break" to set a
         --  breakpoint on exceptions. The former is the newer syntax, but
         --  wasn't supported in older versions of the debugger.

         declare
            Help : constant String :=
              Send_And_Get_Clean_Output (Debugger, "help catch");
         begin
            Debugger.Use_Catch_For_Exceptions :=
              To_TriBoolean (Index (Help, "catch exception") >= 1);
         end;
      end if;

      if Debugger.Use_Catch_For_Exceptions = GNATCOLL.Tribooleans.True then
         Command := Catch'Access;
      end if;

      if Unhandled then
         return (if Temporary then "t" else "") &
           Command.all & " exception unhandled";
      elsif Name /= "" and then Name /= "all" then
         return (if Temporary then "t" else "") &
           Command.all & " exception " & Name;
      else
         return (if Temporary then "t" else "") &
           Command.all & " exception";
      end if;
   end Break_Exception_Cmd;

   ---------------------
   -- Get_GDB_Version --
   ---------------------

   function Get_GDB_Version
     (Debugger : access Gdb_Debugger) return Version_Number is
   begin
      if Debugger.GDB_Version /= Unknown_Version then
         return Debugger.GDB_Version;
      end if;

      declare
         S       : constant String := Send_And_Get_Clean_Output
           (Debugger, "show version", Mode => Internal);
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
      --  ??? Should generate a callback/hook informing other units
      --  that debugging has ended (so that e.g. the call stack can be cleared)
   end Running_Filter;

   ------------------------------
   -- Continuation_Line_Filter --
   ------------------------------

   procedure Continuation_Line_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Process.Debugger.Continuation_Line := True;
   end Continuation_Line_Filter;

   -------------------------------
   -- Send_And_Get_Clean_Output --
   -------------------------------

   overriding function Send_And_Get_Clean_Output
     (Debugger : access Gdb_Debugger;
      Cmd      : String;
      Mode     : Command_Type := Hidden) return String
   is
      S   : constant String := Send_And_Get_Output (Debugger, Cmd, Mode);
      Pos : Integer;
   begin
      if Ends_With (S, Prompt_String) then
         Pos := S'Last - Prompt_String'Length;
         if Pos in S'Range and then S (Pos) = ASCII.LF then
            Pos := Pos - 1;
         end if;
         return S (S'First .. Pos);
      else
         return S;
      end if;
   end Send_And_Get_Clean_Output;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      --  Override Send to recognize some commands such as "target"

      Debugger.Prepare_Target_For_Send (Cmd);

      --  Call the Parent procedure

      Send
        (Debugger_Root (Debugger.all)'Access, Cmd,
         Empty_Buffer, Wait_For_Prompt, Force_Send, Mode);
   end Send;

   -------------
   -- Type_Of --
   -------------

   overriding function Type_Of
     (Debugger : access Gdb_Debugger; Entity : String) return String is
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

      begin
         if Pos = 0 then
            return "";
         else
            return S (Pos + 7 .. S'Last);
         end if;
      end;
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   overriding function Info_Locals
     (Debugger : access Gdb_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "info locals";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   overriding function Info_Args
     (Debugger : access Gdb_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "info args";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   overriding function Info_Registers
     (Debugger : access Gdb_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "info registers";
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
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
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
     (Debugger        : access Gdb_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Debugger_Args   : GNAT.OS_Lib.Argument_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Local_Arguments : constant Argument_List := Gdb_Options & Debugger_Args;
      --  The string accesses in Gdb_Options are statically allocated and the
      --  caller owns the ones in Debugger_Args, so we have no memory
      --  management to do here for these.

      Process         : Visual_Debugger;

      function Contains (S : String; Substring : String) return Boolean;
      --  Return True if S contains Substring

      --------------
      -- Contains --
      --------------

      function Contains (S : String; Substring : String) return Boolean is
         N : Natural := S'First;
      begin
         if S'Length < Substring'Length then
            return False;
         end if;

         Skip_To_String (S, N, Substring);
         return Substring'Length <= S'Last + 1 - N;
      end Contains;

   begin
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

      Debugger.Executable := Executable;

      if Executable_Args /= "" then
         Debugger.Executable_Args := new String'(Executable_Args);
      end if;

      if Remote_Target /= "" then
         Debugger.Remote_Target := new String'(Remote_Target);
         Debugger.Remote_Protocol := new String'(Remote_Protocol);
         Detect_Debugger_Mode (Debugger);
      end if;

      --  Perform an additional guess of the mode of gdb from the name of the
      --  executable launched.
      if Contains (Debugger_Name, "vxworks") then
         Debugger.Remote_Mode := VxWorks;
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      Process := Convert (Debugger);

      if Process /= null then
         Add_Regexp_Filter (Process, Language_Filter'Access, Language_Pattern);
         Add_Regexp_Filter (Process, Running_Filter'Access, Terminate_Pattern);
         Add_Regexp_Filter (Process, Running_Filter'Access, Running_Pattern);

         --  Set another filter to detect the cases when gdb asks questions,
         --  so that we can display dialogs.

         Add_Regexp_Filter
           (Process, Question_Filter1'Access, Question_Filter_Pattern1);

         Add_Regexp_Filter
           (Process, Question_Filter2'Access, Question_Filter_Pattern2);

         Add_Regexp_Filter
           (Process,
            Continuation_Line_Filter'Access, Continuation_Line_Pattern);

         Set_Input_Output_Filter (Process);
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Debugger : access Gdb_Debugger) is
      Num     : Expect_Match;
      Lang    : Language_Access;
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

      --  Make sure that the prompt is what we are expecting

      Send (Debugger, Gdb_Options_Set_Prompt, Mode => Internal);
      Send (Debugger, "set width 0", Mode => Internal);
      Send (Debugger, "set height 0", Mode => Internal);
      Send (Debugger, "set annotate 1", Mode => Internal);

      --  Cache result of 'show version', and also as a side effect, includes
      --  output of 'show version' in the log file.

      Ignored := Get_GDB_Version (Debugger);

      if not Is_Local (Debug_Server) then
         --  Workaround the following problem: when telneting to Windows,
         --  using the windows telnet service, then the TERM env variable is
         --  set to vt100, which produces undesirable results (input is
         --  duplicated, see HC08-007). A workaround is to send
         --  'set editing off' to gdb, as this removes this undesired result.
         Send (Debugger, "set editing off", Mode => Internal);
      end if;

      if Get_Pref (Execution_Window) and then Is_Local (Debug_Server) then
         if Host = Windows then
            Send (Debugger, "set new-console", Mode => Internal);
         end if;

         Debugger.Execution_Window := True;

      else
         Debugger.Execution_Window := False;
      end if;

      --  Make sure gdb will not ask too much interactive questions.
      --  Interactive questions are better left to the GUI itself.
      Send (Debugger, "set confirm off", Mode => Internal);

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

         --  Get the initial file name, so that we can display the appropriate
         --  file in the code editor.
         --  This should be done only after we have detected the current
         --  language, or no color highlighting will be provided.
         --
         --  Note that we need to send the "list" command first, otherwise
         --  info line will not work.
         --
         --  Since "list" with no arguments has undefined behavior, take
         --  advantage of the adainit symbol in case of Ada executables.
         --  This is particularly important in case of cross environments.

         Lang := Get_Language (Debugger);

         if Lang /= null and then Lang.all in Gdb_Ada_Language'Class then
            --  Switch to C language to avoid possible ambiguities

            Switch_Language (Debugger, "c");
            Send (Debugger, "list adainit", Mode => Internal);
            Restore_Language (Debugger);

         else
            Send (Debugger, "list main,main", Mode => Internal);
         end if;

         if Get_Pref (Open_Main_Unit) then
            Send (Debugger, "info line", Mode => Internal);
         end if;
      end if;

      if Debugger.Executable_Args /= null then
         Set_Args (Debugger, Debugger.Executable_Args.all, Mode => Internal);
      end if;

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

   overriding procedure Close (Debugger : access Gdb_Debugger) is
   begin
      --  If the debugger process is dead, do not attempt to communicate
      --  with the underlying process.

      if Get_Process (Debugger) /= null
        and then Get_Descriptor (Get_Process (Debugger)) /= null
        and then Get_Pid (Get_Descriptor (Get_Process (Debugger)).all) /=
          GNAT.Expect.Invalid_Pid
      then
         --  In case the debugger was waiting for some input, or was busy
         --  processing a command.
         --  Try to handle case were gdb is waiting on a user question.

         if Command_In_Process (Debugger.Process) then
            Send (Debugger, "n", Wait_For_Prompt => False, Mode => Internal);
            Interrupt (Debugger);

            --  Force the waiting of a prompt between sending the Ctrl-C above
            --  and sending "quit".
            Send (Debugger, "echo",
                  Wait_For_Prompt => True,
                  Force_Send      => True,
                  Mode            => Internal);
         end if;

         --  Now exit the debugger
         Send (Debugger, "quit", Wait_For_Prompt => False, Mode => Internal);
      end if;

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : Invisible_Command := Hidden)
   is
      Process : constant Visual_Debugger := Convert (Debugger);
      Cmd     : constant String := "target " & Protocol & " " & Target;
      Timeout : constant := 2_000;
      Success : Boolean;
   begin
      --  If the debugger is already connected, kill the connection if Force
      --  is True or simply return otherwise.
      if Debugger.Target_Connected then
         if Force then
            Interrupt (Debugger);
            Wait_Prompt (Debugger);
         else
            return;
         end if;
      end if;

      --  Send the command to the debugger in a non-blocking way and check if
      --  it succeed.
      Send (Debugger,
            Cmd             => Cmd,
            Wait_For_Prompt => False,
            Mode            => Mode);
      Success := Wait_Prompt (Debugger, Timeout);

      --  Mark the command as processed, even if did not succeed in the
      --  specified timeout so that we can continue sending other commands.
      Set_Command_In_Process (Get_Process (Debugger), False);
      Free (Process.Current_Command);

      if Success then
         if Process /= null then
            Output_Text
              (Process, Protocol & " debugging using " & Target & ASCII.LF);
         end if;

         if Protocol = "remote" then
            Set_Is_Started (Debugger, True);
         end if;

         Set_VxWorks_Version (Debugger);
      else
         --  If it
         Interrupt (Debugger);
         Wait_Prompt (Debugger);

         if Process /= null then
            Output_Text (Process, "Can't connect to the target using "
                         & Protocol & " protocol on " & Target & ASCII.LF);
         end if;
      end if;

      Debugger.Target_Connected := Success;
      Display_Prompt (Debugger);
   end Connect_To_Target;

      ----------------------------
   -- Is_Connected_To_Target --
   ----------------------------

   overriding function Is_Connected_To_Target
     (Debugger : access Gdb_Debugger) return Boolean
   is
     (Debugger.Target_Connected);

   --------------
   -- Set_Args --
   --------------

   procedure Set_Args
     (Debugger  : access Gdb_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger, "set args " & Arguments, Mode => Mode);
      Set_VxWorks_Version (Debugger, Force => True);
   end Set_Args;

   --------------------
   -- Get_Executable --
   --------------------

   overriding function Get_Executable
     (Debugger : access Gdb_Debugger) return GNATCOLL.VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   ---------------------------------
   -- Connect_To_Target_If_Needed --
   ---------------------------------

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Remote_Target /= null
        and then Debugger.Remote_Protocol /= null
        and then not Debugger.Target_Connected
      then
         Debugger.Connect_To_Target (Target   => Debugger.Get_Remote_Target,
                                     Protocol => Debugger.Get_Remote_Protocol,
                                     Mode     => Internal);
      else
         Display_Prompt (Debugger);
      end if;
   end Connect_To_Target_If_Needed;

   --------------------
   -- Set_Executable --
   --------------------

   overriding procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
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

      Lang                : Language_Access;
      Process             : Visual_Debugger;

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
            S : constant String := Send_And_Get_Clean_Output
              (Debugger, Cmd.all, Mode => Hidden);
         begin
            Free (Cmd);

            if Match (No_Such_File_Regexp, S) /= 0 then
               raise Executable_Not_Found;
            end if;

            if Process /= null and then S /= "" then
               Output_Text (Process, S & ASCII.LF, Set_Position => True);
            end if;
         end;
      end Launch_Command_And_Output;

   begin
      Process := Convert (Debugger);

      Debugger.Executable := Executable;

      --  Send the "file" command if needed

      if Debugger.Remote_Mode /= VxWorks then
         Launch_Command_And_Output ("file");
      end if;

      --  Connect to the remote target if needed

      Connect_To_Target_If_Needed (Debugger);

      --  If we are in Cross mode (ie, with the "remote" protocol), the call
      --  to "target" has the side effect of starting the executable.
      --  In all other cases the executable is not started at this stage.

      if Debugger.Remote_Mode = Cross then
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

      --  Get the name and line of the initial file

      Lang := Get_Language (Debugger);

      if Lang /= null and then Lang.all in Gdb_Ada_Language'Class then
         Switch_Language (Debugger, "c");
         Send (Debugger, "list adainit", Mode => Internal);
         Restore_Language (Debugger);

      else
         Send (Debugger, "list main,main", Mode => Internal);
      end if;

      if Get_Pref (Break_On_Exception) then
         declare
            Cmd : constant String := Break_Exception_Cmd (Debugger);
            S   : constant String := Send_And_Get_Clean_Output
              (Debugger, Cmd);

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

      if Get_Pref (Open_Main_Unit) then
         Set_Parse_File_Name (Get_Process (Debugger), False);

         declare
            Str         : constant String := Send_And_Get_Clean_Output
              (Debugger, "info line", Mode => Internal);
            Matched     : Match_Array (0 .. 2);
            File        : Unbounded_String;
            Line        : Natural;
            Addr        : Address_Type;
            pragma Unreferenced (Line, Addr);

         begin
            Set_Parse_File_Name (Get_Process (Debugger), True);
            Found_File_Name (Debugger, Str, File, Line, Addr);

            if Length (File) /= 0 then
               declare
                  F     : constant String := To_String (File);
                  First : Natural;
               begin
                  Match (GNAT_Binder_File_Pattern, F, Matched);

                  --  If we find a file that looks like a GNAT binder file,
                  --  load the corresponding main file.

                  if Matched (0) /= No_Match then
                     First := Matched (0).First + 2;

                     if F (First) = '_' then
                        First := First + 1;
                     end if;

                     Send
                       (Debugger,
                        "info line " &
                          F (First .. Matched (0).Last) & ":1",
                        Mode => Internal);
                     return;
                  end if;
               end;
            end if;

            Send (Debugger, "info line", Mode => Internal);
         end;
      end if;
   end Set_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   overriding procedure Load_Core_File
     (Debugger : access Gdb_Debugger;
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

      --  Detect the current language, and get the name and line of the
      --  current file.

      Send (Debugger, "show lang", Mode => Internal);
      Send (Debugger, "info line", Mode => Internal);
   end Load_Core_File;

   -----------------
   -- Add_Symbols --
   -----------------

   overriding procedure Add_Symbols
     (Debugger : access Gdb_Debugger;
      Module   : Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Symbols : constant String := +Module.Unix_Style_Full_Name;
   begin
      Test_If_Has_Command
        (Debugger, Debugger.Has_Wtx_Add_Symbol_File,
         "wtx add-symbol-file");

      if Debugger.Has_Wtx_Add_Symbol_File = GNATCOLL.Tribooleans.True
        and then Address = ""
      then
         Send (Debugger, "wtx add-symbol-file " & Symbols, Mode => Mode);
      else
         Send
           (Debugger,
            "add-symbol-file " & Symbols & " " & Address, Mode => Mode);
      end if;

      if Mode in Visible_Command then
         Wait_User_Command (Debugger);
      end if;
   end Add_Symbols;

   ---------------------
   -- Load_Executable --
   ---------------------

   overriding procedure Load_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Debugger.Is_Connected_To_Target then

         if Executable /= GNATCOLL.VFS.No_File then
            Send
              (Debugger, "load """ & (+Executable.Unix_Style_Full_Name) & '"',
               Mode => Mode);
         else
            Send (Debugger, "load", Mode => Mode);
         end if;

         if Mode in Visible_Command then
            Wait_User_Command (Debugger);
         end if;
      end if;
   end Load_Executable;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "detach", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "kill", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt (Debugger : access Gdb_Debugger) is
      Num : Expect_Match;
      pragma Unreferenced (Num);
   begin
      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   overriding function Wait_Prompt
     (Debugger : access Gdb_Debugger;
      Timeout  : Integer) return Boolean
   is
      Num : Expect_Match;
   begin
      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => Timeout);
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

   overriding procedure Run
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= GNATCOLL.VFS.No_File
      then
         declare
            Module : constant String := Get_Module (Debugger.Executable);
         begin
            Send (Debugger, "run " & Module, Mode => Mode);
         end;

      else
         Send (Debugger, "run " & Arguments, Mode => Mode);
      end if;

      Set_Is_Started (Debugger, True);
   end Run;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Debugger.Test_If_Has_Command (Debugger.Has_Start_Cmd, "start");

      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= GNATCOLL.VFS.No_File
      then
         declare
            Module : constant String := Get_Module (Debugger.Executable);
         begin
            if Debugger.Has_Start_Cmd = GNATCOLL.Tribooleans.True then
               Send (Debugger, "start " & Module, Mode => Mode);
            else
               Send (Debugger, "begin " & Module, Mode => Mode);
            end if;
         end;

      else
         if Debugger.Has_Start_Cmd = GNATCOLL.Tribooleans.True then
            Send (Debugger, "start " & Arguments, Mode => Mode);
         else
            Send (Debugger, "begin " & Arguments, Mode => Mode);
         end if;
      end if;

      Set_Is_Started (Debugger, True);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   overriding procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   overriding procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "next", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   overriding procedure Step_Into_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "stepi", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   overriding procedure Step_Over_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "nexti", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("continue", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access Gdb_Debugger) is
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

      --  Interrupting the debugger closes any remote connection
      Debugger.Target_Connected := False;
   end Interrupt;

   ------------------
   -- Command_Kind --
   ------------------

   overriding function Command_Kind
     (Debugger : access Gdb_Debugger;
      Command  : String) return Command_Category
   is
      pragma Unreferenced (Debugger);
      Index : Natural;
   begin
      if Command = "" then
         return Misc_Command;
      end if;

      if Starts_With (Command, "file")
        or else Starts_With (Command, "add-symbol-file")
        or else Starts_With (Command, "wtx add-symbol-file")
        or else Starts_With (Command, "load")
      then
         return Load_Command;
      end if;

      if Starts_With (Command, "thread")
        or else Starts_With (Command, "task")
        or else Starts_With (Command, "core")
        or else Starts_With (Command, "attach")
      then
         return Context_Command;
      end if;

      Index := Command'First;
      Skip_Word (Command, Index);

      if        Command (Command'First .. Index - 1) = "step"
        or else Command (Command'First .. Index - 1) = "stepi"
        or else Command (Command'First .. Index - 1) = "s"
        or else Command (Command'First .. Index - 1) = "si"
        or else Command (Command'First .. Index - 1) = "next"
        or else Command (Command'First .. Index - 1) = "n"
        or else Command (Command'First .. Index - 1) = "nexti"
        or else Command (Command'First .. Index - 1) = "cont"
        or else Command (Command'First .. Index - 1) = "c"
        or else Command (Command'First .. Index - 1) = "continue"
        or else Command (Command'First .. Index - 1) = "run"
        or else Command (Command'First .. Index - 1) = "r"
        or else Starts_With (Command, "fin")  --  fin/fini/finish/...
        or else Starts_With (Command, "attach")
        or else Starts_With (Command, "target")
        or else Starts_With (Command, "run")
        or else Starts_With (Command, "r ")
        or else Starts_With (Command, "begin")
        or else Starts_With (Command, "start")
        or else Starts_With (Command, "set variable")
      then
         return Execution_Command;
      end if;

      return Misc_Command;
   end Command_Kind;

   ----------------------
   -- Is_Break_Command --
   ----------------------

   overriding function Breakpoints_Changed
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Looking_At (Command, Command'First + 1, "break")
        or else Starts_With (Command, "break")
        or else Starts_With (Command, "b ")
        or else Starts_With (Command, "watch")
        or else Starts_With (Command, "catch")
        or else Starts_With (Command, "awatch")
        or else Starts_With (Command, "rwatch")
        or else Starts_With (Command, "delete")
        or else Starts_With (Command, "clear")
        or else Starts_With (Command, "del ")
        or else Starts_With (Command, "d ")
        or else Starts_With (Command, "disable")
        or else Starts_With (Command, "enable")
        or else Starts_With (Command, "begin")
        or else Starts_With (Command, "start")
        or else Starts_With (Command, "ignore")
        or else Starts_With (Command, "command")
        or else Starts_With (Command, "run")
        or else Starts_With (Command, "r ")
        or else Command = "r"
        or else Starts_With (Command, "condition")
        or else Starts_With (Command, "set break-command")
        or else Starts_With (Command, "change-break");
   end Breakpoints_Changed;

   ----------------
   -- Stack_Down --
   ----------------

   overriding procedure Stack_Down
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "down", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "up", Mode => Mode);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access Gdb_Debugger;
      Frame    : Positive;
      Mode     : Command_Type := Hidden)
   is
      Str : constant String := "frame" & Natural'Image (Frame - 1);
   begin
      Send (Debugger, Str, Mode => Mode);
   end Stack_Frame;

   --------------------------
   -- Parse_Backtrace_Info --
   --------------------------

   procedure Parse_Backtrace_Info
     (S     : String;
      Value : out Backtrace_Array;
      Len   : out Natural)
   is
      Matched : Match_Array (0 .. 6);
      First   : Positive := S'First;
   begin
      Len := Value'First - 1;

      while Len /= Value'Last loop
         Match (Frame_Pattern, S (First .. S'Last), Matched);

         exit when Matched (0) = No_Match;

         Len := Len + 1;
         Value (Len).Frame_Id :=
           Natural'Value (S (Matched (1).First .. Matched (1).Last));

         if Matched (2) = No_Match then
            Value (Len).Program_Counter := new String'("");
         else
            Value (Len).Program_Counter :=
              new String'(S (Matched (3).First .. Matched (3).Last));
         end if;

         Value (Len).Subprogram :=
           new String'(S (Matched (4).First .. Matched (4).Last));

         if Matched (5) = No_Match then
            Value (Len).Source_Location := new String'("");
         else
            Value (Len).Source_Location :=
              new String'(S (Matched (6).First .. Matched (6).Last));
         end if;

         First := Matched (0).Last + 2;
      end loop;
   end Parse_Backtrace_Info;

   -------------------------
   -- Configure_Backtrace --
   -------------------------

   overriding procedure Configure_Backtrace
     (Self                 : not null access Gdb_Debugger;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True)
   is
      pragma Unreferenced
        (Show_Id, Show_PC, Show_Subprogram_Name, Show_Location);
   begin
      if Show_Parameters then
         Self.Send ("set print frame-arguments scalar");
      else
         Self.Send ("set print frame-arguments none");
      end if;
   end Configure_Backtrace;

   ---------------
   -- Backtrace --
   ---------------

   overriding procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural) is
   begin
      Parse_Backtrace_Info
        (Send_And_Get_Clean_Output
           (Debugger, "where", Mode => Internal), Value, Len);
   end Backtrace;

   -----------------------------
   -- Internal_Set_Breakpoint --
   -----------------------------

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_Debugger;
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
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      if Temporary then
         return Internal_Set_Breakpoint
           (Debugger, "tbreak " & Name, Mode => Mode);
      else
         return Internal_Set_Breakpoint
           (Debugger, "break " & Name, Mode => Mode);
      end if;
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   overriding function Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      if Temporary then
         return Internal_Set_Breakpoint
           (Debugger,
            "tbreak " & (+Base_Name (File)) & ":" & Image (Integer (Line)),
            Mode => Mode);

      else
         return Internal_Set_Breakpoint
           (Debugger,
            "break " & (+Base_Name (File)) & ':' & Image (Integer (Line)),
            Mode => Mode);
      end if;
   end Break_Source;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access Gdb_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      --  ??? We use the base name here, because older versions of gdb on
      --  Windows do not work well with a full name:
      --      clear Z:\foo\bar.adb:2
      --  fails, when
      --      clear bar.adb:2
      --  works.
      --  Since the risk of having two files with the same basename and a
      --  breakpoint on the same line is limited, we simply use base names.
      Debugger.Send
        ("clear " & File.Display_Base_Name & ":" & Image (Integer (Line)),
         Mode => Mode);
   end Remove_Breakpoint_At;

   ---------------------
   -- Break_Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         Break_Exception_Cmd (Debugger, Name, Temporary, Unhandled),
         Mode => Mode);
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   overriding function Break_Address
     (Debugger  : access Gdb_Debugger;
      Address   : GVD.Types.Address_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      if Temporary then
         return Internal_Set_Breakpoint
           (Debugger, "tbreak *" & Address_To_String (Address), Mode => Mode);
      else
         return Internal_Set_Breakpoint
           (Debugger, "break *" & Address_To_String (Address), Mode => Mode);
      end if;
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   overriding function Break_Regexp
     (Debugger  : access Gdb_Debugger;
      Regexp    : String;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
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
     (Debugger : access Gdb_Debugger) return Breakpoint_Identifier
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
     (Debugger  : access Gdb_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "condition" & Breakpoint_Identifier'Image (Num)
            & " " & Condition, Mode => Mode);
   end Set_Breakpoint_Condition;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   overriding procedure Set_Breakpoint_Command
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Commands = "" or else Commands (Commands'Last) = ASCII.LF then
         Send (Debugger, "command" & Breakpoint_Identifier'Image (Num)
               & ASCII.LF & Commands & "end", Mode => Mode);
      else
         Send (Debugger, "command" & Breakpoint_Identifier'Image (Num)
               & ASCII.LF & Commands & ASCII.LF & "end", Mode => Mode);
      end if;
   end Set_Breakpoint_Command;

   ---------------------------------
   -- Set_Breakpoint_Ignore_Count --
   ---------------------------------

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "ignore" & Breakpoint_Identifier'Image (Num)
            & Integer'Image (Count), Mode => Mode);
   end Set_Breakpoint_Ignore_Count;

   ----------------------
   -- Set_Scope_Action --
   ----------------------

   Task_String : aliased String := "task";
   PD_String   : aliased String := "pd";
   Any_String  : aliased String := "any";
   All_String  : aliased String := "all";

   overriding procedure Set_Scope_Action
     (Debugger : access Gdb_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Scope_String  : GNAT.Strings.String_Access;
      Action_String : GNAT.Strings.String_Access;
   begin
      case Scope is
         when Current_Task =>
            Scope_String := Task_String'Access;

         when Tasks_In_PD =>
            Scope_String := PD_String'Access;

         when Any_Task =>
            Scope_String := Any_String'Access;

         when No_Scope =>
            null;
      end case;

      if Scope /= No_Scope then
         --  If the breakpoint identifier is 0, then set the
         --  session-wide default scope, otherwise change only the
         --  scope of the breakpoint indicated by Num

         --  These commands are sent as Internal to avoid the call
         --  to Refresh_Breakpoints_List in Send_Internal_Post, which causes
         --  the Action radio button to jump to its previous setting

         if Num = 0 then
            Send (Debugger, "set break-command-scope "
                  & Scope_String.all, Mode => GVD.Types.Internal);
         else
            Send (Debugger, "change-breakpoint-scope" & Num'Img
                  & " " & Scope_String.all, Mode => GVD.Types.Internal);
         end if;
      end if;

      case Action is
         when Current_Task =>
            Action_String := Task_String'Access;

         when Tasks_In_PD =>
            Action_String := PD_String'Access;

         when All_Tasks =>
            Action_String := All_String'Access;

         when No_Action =>
            null;
      end case;

      if Action /= No_Action then
         --  If the breakpoint identifier is 0, then set the
         --  session-wide default action, otherwise change only the
         --  scope of breakpoint indicated by Num

         if Num = 0 then
            Send (Debugger, "set break-command-action "
                  & Action_String.all, Mode => Mode);
         else
            Send (Debugger, "change-breakpoint-action" & Num'Img
                  & " " & Action_String.all, Mode => Mode);
         end if;
      end if;
   end Set_Scope_Action;

   -----------
   -- Watch --
   -----------

   overriding function Watch
     (Debugger  : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "finish", Mode => Mode);
   end Finish;

   -----------------
   -- Task_Switch --
   -----------------

   overriding procedure Task_Switch
     (Debugger : access Gdb_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "task" & Natural'Image (Task_Num), Mode => Mode);
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   overriding procedure Thread_Switch
     (Debugger : access Gdb_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "thread" & Natural'Image (Thread), Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   overriding procedure PD_Switch
     (Debugger : access Gdb_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "pd " & PD, Mode => Mode);
   end PD_Switch;

   ----------------
   -- Info_Tasks --
   ----------------

   overriding procedure Info_Tasks
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      Output  : constant String :=
        Send_And_Get_Clean_Output (Debugger, "info tasks", Mode => Internal);
      Matched : Match_Array (0 .. 6);
   begin
      Len := 0;

      if Output = "The program is not being run."
        or else Output = "No stack"
      then
         return;
      end if;

      declare
         Lines : constant Unbounded_String_Array := Split (Output, ASCII.LF);

         Header_Found : Boolean := False;

         ID_End      : Natural;
         TID_End     : Natural;
         P_ID_End    : Natural;
         Pri_End     : Natural;
         State_Start : Natural;
         Name_Start  : Natural;

      begin
         for L in Lines'Range loop
            if not Header_Found then
               declare
                  S : constant String := To_String (Lines (L));
               begin
                  Match (Info_Tasks_Pattern, S, Matched);

                  if Matched (0) /= No_Match then
                     Header_Found := True;

                     ID_End      := Matched (1).Last - S'First;
                     TID_End     := Matched (2).Last - S'First;
                     P_ID_End    := Matched (3).Last - S'First;
                     Pri_End     := Matched (4).Last - S'First;
                     State_Start := Matched (5).First - S'First;
                     Name_Start  := Matched (6).First - S'First;

                     Len := Len + 1;
                     Info (Len) :=
                       (Num_Fields => 6,
                        Information =>
                          (New_String ("ID"),
                           New_String ("TID"),
                           New_String ("P-ID"),
                           New_String ("Pri"),
                           New_String ("State"),
                           New_String ("Name")));
                  end if;
               end;
            else
               declare
                  S : constant String := To_String (Lines (L));
                  F : constant Natural := S'First;
               begin
                  if S'Length > Name_Start then
                     Len := Len + 1;
                     Info (Len) :=
                       (Num_Fields => 6,
                        Information =>
                          (New_String (S (F .. ID_End + F)),
                           New_String (S (ID_End + F + 1 .. TID_End + F)),
                           New_String (S (TID_End + F + 1 .. P_ID_End + F)),
                           New_String (S (P_ID_End + F + 1 .. Pri_End + F)),
                           New_String
                             (S (State_Start + F .. Name_Start + F - 1)),
                           New_String (S (Name_Start + F .. S'Last))));
                  end if;
               end;
            end if;
         end loop;
      end;

      exception
         when Constraint_Error =>
            --  A parsing error occured when filling Info (Len)

            if Len > 0 then
               Len := Len - 1;
            end if;
   end Info_Tasks;

   ------------------
   -- Info_Threads --
   ------------------

   overriding procedure Info_Threads
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      Output : constant String := Send_And_Get_Clean_Output
        (Debugger, "info threads", Mode => Internal);
      EOL    : Natural;
      Index  : Integer := Output'Last;

   begin
      Len := Info'First;
      Info (Len) :=
        (Num_Fields => 1,
         Information => (1 => New_String ("Thread")));

      while Index > Output'First loop
         Len := Len + 1;
         EOL := Index;

         while EOL >= Output'First and then Output (EOL) /= ASCII.LF loop
            EOL := EOL - 1;
         end loop;

         if EOL = Output'First then
            EOL := EOL - 1;
         end if;

         Info (Len) :=
           (Num_Fields => 1,
            Information => (1 => New_String (Output (EOL + 1 .. Index))));
         Index := EOL - 1;
      end loop;
   end Info_Threads;

   --------------
   -- Info_PD --
   --------------

   overriding procedure Info_PD
     (Debugger : access Gdb_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural)
   is
      Output : constant String := Send_And_Get_Clean_Output
        (Debugger, "info pds", Mode => Internal);
      EOL    : Positive;
      Start  : Positive := Output'First;
      First  : Positive := Output'First;
      Second : Positive := Output'First;

      function Is_Delimitor (C : Character) return Boolean;
      --  Return True if C is a delimiter

      ------------------
      -- Is_Delimitor --
      ------------------

      function Is_Delimitor (C : Character) return Boolean is
      begin
         return C = ' ' or else C = ASCII.HT or else C = '*';
      end Is_Delimitor;

   begin
      Len := 0;

      if Output = "The program is not being run." then
         return;
      end if;

      while Start < Output'Last loop
         Len := Len + 1;
         EOL := Start;

         while EOL <= Output'Last and then Output (EOL) /= ASCII.LF loop
            EOL := EOL + 1;
         end loop;

         --  Find the end of the first token in the line...
         First := Start;
         while First < EOL and then Is_Delimitor (Output (First)) loop
            First := First + 1;
         end loop;
         while First < EOL and then not Is_Delimitor (Output (First)) loop
            First := First + 1;
         end loop;

         --  Find the start of the second token...
         Second := First + 1;
         while Second < EOL and then Is_Delimitor (Output (Second)) loop
            Second := Second + 1;
         end loop;

         Info (Len) :=
           (Num_Fields => 2,
            Information =>
              (New_String (Output (Start  .. First)),
               New_String (Output (Second .. EOL - 1))));
         Start := EOL + 1;
      end loop;

   exception
      when Constraint_Error =>
         --  A parsing error occured when filling Info (Len)

         if Len > 0 then
            Len := Len - 1;
         end if;
   end Info_PD;

   -------------------------
   -- Set_VxWorks_Version --
   -------------------------

   overriding procedure Set_VxWorks_Version
     (Debugger : access Gdb_Debugger; Force : Boolean := False) is
   begin
      if Force or else Debugger.VxWorks_Version = Vx_None then
         declare
            Output : constant String := Send_And_Get_Clean_Output
              (Debugger, "info wtx vxworks-version", Mode => Internal);
         begin
            if Output'Length >= 17 then
               if Output (1 .. 17) = "VxWorks version 5" then
                  Debugger.VxWorks_Version := Vx5;
               elsif Output (1 .. 17) = "VxWorks version 6" then
                  Debugger.VxWorks_Version := Vx6;
               elsif Output (1 .. 11) = "VxWorks 653" then
                  Debugger.VxWorks_Version := Vx653;
               else
                  Debugger.VxWorks_Version := Vx_Unknown;
               end if;
            else
               Debugger.VxWorks_Version := Vx_Unknown;
            end if;
         end;
      end if;
   end Set_VxWorks_Version;

   ---------------------
   -- VxWorks_Version --
   ---------------------

   overriding function VxWorks_Version
     (Debugger : access Gdb_Debugger) return VxWorks_Version_Type is
   begin
      return Debugger.VxWorks_Version;
   end VxWorks_Version;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   overriding function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt (Debugger : access Gdb_Debugger) is
      Proc : constant Visual_Debugger := Convert (Debugger);
   begin
      if Proc /= null then
         Output_Text
           (Proc,
            Send_And_Get_Output (Debugger, "echo", Mode => Internal),
            Is_Command => False,
            Set_Position => True);
      end if;
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   overriding procedure Change_Directory
     (Debugger : access Gdb_Debugger;
      Dir      : Virtual_File;
      Mode     : Command_Type := Hidden)
   is
      Directory : constant String := +Dir.Unix_Style_Full_Name;
   begin
      Send (Debugger, "cd " & Directory, Mode => Mode);
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   overriding procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      pragma Unreferenced (Debugger);

      Start      : Natural := Str'First;
      Matched    : Match_Array (0 .. 4);
      Matched2   : Match_Array (0 .. 4);
      Name_Index : Natural := 1;
      Line_Index : Natural := 2;
      Addr_Index : Natural := 3;

   begin
      --  Default values if nothing better is found
      Name := Null_Unbounded_String;
      Line := 0;
      Addr := Invalid_Address;

      --  Search for the last file reference in the output. There might be
      --  several of them, for instance when we hit a breakpoint with an
      --  associated 'up' command.

      Matched (0) := No_Match;

      loop
         Match (File_Name_Pattern, Str (Start .. Str'Last), Matched2);
         exit when Matched2 (0) = No_Match;
         Matched := Matched2;
         Start := Matched (0).Last + 1;
      end loop;

      if Matched (0) = No_Match then
         --  Try another regexp
         --  This regexp takes longer to execute when the output has a lot of
         --  lines. There wouldn't be any need to test that if we knew what
         --  is the debugger output and what is the user's program output???

         Match (File_Name_Pattern2, Str, Matched);

         if Matched (0) = No_Match then
            return;
         end if;

         if Matched (3) = No_Match then
            Name_Index := 4;
         else
            Name_Index := 3;
         end if;

         Line_Index := 1;
         Addr_Index := 0;
      end if;

      Set_Unbounded_String
        (Name, Str (Matched (Name_Index).First .. Matched (Name_Index).Last));

      if Addr_Index /= 0 then
         Addr :=
           String_To_Address
             (Str (Matched (Addr_Index).First .. Matched (Addr_Index).Last));
      end if;

      Line := Natural'Value
        (Str (Matched (Line_Index).First .. Matched (Line_Index).Last));
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   overriding procedure Found_Frame_Info
     (Debugger : access Gdb_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger);

      Matched : Match_Array (0 .. 1);
   begin
      Match (Frame_Pattern, Str, Matched);

      if Matched (1) /= No_Match then
         Set_Unbounded_String
           (Frame, Str (Matched (1).First .. Matched (1).Last));
         Match (Frame_Pattern_With_File, Str, Matched);

         if Matched (1) /= No_Match then
            Message := Location_Found;
         else
            Message := No_Debug_Info;
         end if;

      else
         Frame := Null_Unbounded_String;
         Message := Location_Not_Found;
      end if;
   end Found_Frame_Info;

   -----------------------
   -- Source_Files_List --
   -----------------------

   overriding function Source_Files_List
     (Debugger : access Gdb_Debugger) return GNAT.Strings.String_List
   is
      S         : constant String := Send_And_Get_Clean_Output
        (Debugger, "info sources", Mode => Internal);
      Max_Files : Natural := 0;

   begin
      --  ??? Will fail on
      --  "/home/user/Ada/gps/gps_window_pkg-callbacks: \
      --     No such file or directory."
      --  which can be emitted as the result of "info sources"

      --  Count the number of files

      for J in S'Range loop
         if S (J) = ',' then
            Max_Files := Max_Files + 1;
         end if;
      end loop;

      --  Add two, since there are in fact two lists of files (already
      --  read, and to be read), that do not end with ','

      Max_Files := Max_Files + 2;

      declare
         Result : GNAT.Strings.String_List (1 .. Max_Files);
         Num    : Natural := 1;
         Index  : Positive := S'First;
         Start  : Positive;

      begin
         while Index <= S'Last loop
            --  Parse each file

            while Looking_At (S, Index, "Source files for")
              or else Looking_At (S, Index, "No symbol table")
            loop
               Skip_To_Char (S, Index, ':');
               Index := Index + 1;
               Skip_Blanks (S, Index);
            end loop;

            exit when Index > S'Last;

            Start := Index;
            while Index <= S'Last
              and then S (Index) /= ','
              and then S (Index) /= ASCII.LF
            loop
               Index := Index + 1;
            end loop;

            if S (Start .. Index - 1) /= "<bad string table offset>" then
               Result (Num) := new String'(S (Start .. Index - 1));
               Num := Num + 1;
               Index := Index + 1;
               Skip_Blanks (S, Index);
            end if;
         end loop;

         return Result (1 .. Num - 1);
      end;
   end Source_Files_List;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   overriding procedure List_Breakpoints
     (Debugger  : not null access Gdb_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector)
   is
      S : constant String :=
        Send_And_Get_Output (Debugger, "info breakpoints", Mode => Internal);
      Index           : Natural := S'First;
      Tmp             : Natural;

      procedure Fill_Scope_Action (Debugger : access Gdb_Debugger);
      --  Assign values of Scope and Action for the breakpoints in
      --  List as reported by the debugger.

      procedure Match_Breakpoint_Info
        (First       : Integer;
         Last        : Integer;
         Current     : in out Breakpoint_Data;
         Has_Matched : out Boolean);
      --  Match in S (First .. Last - 2) file name, exception name and
      --  subprogram name associated with the current breakpoint.

      procedure Match_Extra_Breakpoint_Info
        (First   : Integer;
         Last    : Integer;
         Current : in out Breakpoint_Data);
      --  Match in S (First .. Last - 2) extra information like commands,
      --  condition,... The string represents a single line of the output, so
      --  should add any extra info to the breakpoint, not override

      -----------------------
      -- Fill_Scope_Action --
      -----------------------

      procedure Fill_Scope_Action (Debugger : access Gdb_Debugger) is
         S : constant String := Send_And_Get_Clean_Output
           (Debugger, "info breakpoints-extra-info",
            Mode => Internal);

         Index   : Natural := S'First;
         Matched : Match_Array (0 .. 4);
         Scope   : GVD.Types.Scope_Type;
         Action  : GVD.Types.Action_Type;
         J       : Integer := List.First_Index;
      begin
         --  skip the first line

         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Index <= S'Last loop
            Match (Breakpoint_Extra_Info, S (Index .. S'Last), Matched);

            if Matched (0) /= No_Match then
               --  get the scope value

               if S (Matched (2).First .. Matched (2).Last) = "task" then
                  Scope := Current_Task;
               elsif S (Matched (2).First .. Matched (2).Last) = "pd" then
                  Scope := Tasks_In_PD;
               elsif S (Matched (2).First .. Matched (2).Last) = "any" then
                  Scope := Any_Task;
               else
                  raise Program_Error;
               end if;

               --  get the action value

               if S (Matched (3).First .. Matched (3).Last) = "task" then
                  Action := Current_Task;
               elsif S (Matched (3).First .. Matched (3).Last) = "pd" then
                  Action := Tasks_In_PD;
               elsif S (Matched (3).First .. Matched (3).Last) = "all" then
                  Action := All_Tasks;
               else
                  raise Program_Error;
               end if;

               List (J).Scope := Scope;
               List (J).Action := Action;

               Skip_To_Char (S, Index, ASCII.LF);
               Index := Index + 1;
            else
               List (J).Scope := No_Scope;
               List (J).Action := No_Action;
            end if;

            J := J + 1;
         end loop;
      end Fill_Scope_Action;

      ---------------------------
      -- Match_Breakpoint_Info --
      ---------------------------

      procedure Match_Breakpoint_Info
        (First       : Integer;
         Last        : Integer;
         Current     : in out Breakpoint_Data;
         Has_Matched : out Boolean)
      is
         Matched : Match_Array (0 .. 10);
         F : Virtual_File;
      begin
         Has_Matched := False;

         Match (File_Name_In_Breakpoint, S (First .. Last - 2), Matched);
         if Matched (0) /= No_Match then
            F := To_File (Kernel, S (Matched (1).First .. Matched (1).Last));
            Current.Location := Kernel.Get_Buffer_Factory.Create_Marker
              (File => F,
               Line => Editable_Line_Type'Value
                 (S (Matched (2).First .. Matched (2).Last)),
               Column => 1);

            Has_Matched := True;
         end if;

         Match (Exception_In_Breakpoint, S (First .. Last - 2), Matched);
         if Matched (0) /= No_Match then
            if Matched (1) /= No_Match then
               if Matched (3) /= No_Match then
                  Current.Except := To_Unbounded_String
                    (S (Matched (3).First .. Matched (3).Last));
               else
                  Current.Except := To_Unbounded_String
                    (S (Matched (2).First .. Matched (2).Last));
               end if;
            else
               if Matched (5) /= No_Match then
                  Current.Except := To_Unbounded_String
                    (S (Matched (5).First .. Matched (5).Last));
               else
                  Current.Except := To_Unbounded_String
                    (S (Matched (4).First .. Matched (4).Last));
               end if;
            end if;
            Has_Matched := True;
         end if;

         Match (Subprogram_In_Breakpoint, S (First .. Last - 2), Matched);
         if Matched (0) /= No_Match then
            Current.Subprogram := To_Unbounded_String
              (S (Matched (1).First .. Matched (1).Last));
            Has_Matched := True;
         end if;
      end Match_Breakpoint_Info;

      ---------------------------------
      -- Match_Extra_Breakpoint_Info --
      ---------------------------------

      procedure Match_Extra_Breakpoint_Info
        (First   : Integer;
         Last    : Integer;
         Current : in out Breakpoint_Data)
      is
         Matched : Match_Array (0 .. 10);
         Index, Index2   : Integer;
      begin
         Match (Condition_In_Breakpoint, S (First .. Last - 2), Matched);
         if Matched (0) /= No_Match then
            Append (Current.Condition,
                    S (Matched (1).First .. Matched (1).Last));
            return;
         end if;

         Match (Ignore_In_Breakpoint, S (First .. Last - 2), Matched);
         if Matched (0) /= No_Match then
            Current.Ignore := Natural'Value
              (S (Matched (1).First .. Matched (1).Last));
            return;
         end if;

         --  List of commands:
         if First + 7 <= S'Last
           and then S (First .. First + 7) = "        "
         then
            Index := First + 8;
            Index2 := Index;
            Skip_To_Char (S, Index2, ASCII.LF);
            Append (Current.Commands, S (Index .. Index2 - 1) & ASCII.LF);
         end if;
      end Match_Extra_Breakpoint_Info;

      Breakpoint_Number : Long_Integer;

   begin
      List.Clear;

      --  Parse each line. The general format looks like:
      --  Num Type           Disp Enb Address    What
      --  1   breakpoint     keep y   0x08052c1f on unhandled exception
      --                                         at a-except.adb:1460
      --          stop only if 0 <= 2
      --          ignore next 200 hits
      --          echo "toto"
      --          print A
      --          cont
      --  2   hw watchpoint  keep y              x
      --          stop only if x = 1000
      --          ignore next 300 hits
      --          print x
      --  3   breakpoint     keep y   <MULTIPLE> 0x08049817
      --          stop only if h = 2
      --          ignore next 20 hits
      --          print h
      --  3.1                     y     0x08049817 in p at p.adb:6
      --  3.1                     y     0x0804984f in p at p.adb:6
      --
      --  For both the breakpoint and watchpoint, the first line is the
      --  condition, the second line is the ignore count, and the rest are
      --  the commands to execute upon stopping.

      declare
         Matched  : Match_Array (0 .. 10);
         M        : Boolean;
         Multiple : Boolean;
         B        : Breakpoint_Data;
      begin
         Index := S'First;
         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Index <= S'Last loop
            Multiple := False;
            Match (Breakpoint_Pattern, S (Index .. S'Last), Matched);

            if Matched (0) /= No_Match then
               Multiple := Matched (5) /= No_Match;

               B := (others => <>);
               B.Num := Breakpoint_Identifier'Value
                 (S (Matched (1).First .. Matched (1).Last));

               case S (Matched (2).First) is
                  when 'b' =>
                     --  "breakpoint"
                     B.The_Type := Breakpoint;
                  when 'a' =>
                     --  "acc watchpoint"
                     B.The_Type := Watchpoint;
                     B.Trigger  := Read_Write;
                  when 'r' =>
                     --  "read watchpoint"
                     B.The_Type := Watchpoint;
                     B.Trigger  := Read;
                  when others =>
                     --  "hw watchpoint"
                     B.The_Type := Watchpoint;
                     B.Trigger  := Write;
               end case;

               case S (Matched (3).First) is
                  when 'k'    => B.Disposition := Keep;
                  when 'd'    => B.Disposition := Disable;
                  when others => B.Disposition := Delete;
               end case;

               B.Enabled := S (Matched (4).First) = 'y';

               if B.The_Type = Breakpoint then
                  B.Address := String_To_Address
                    ("0x" & S (Matched (8).First .. Matched (8).Last));
               else
                  B.Expression := To_Unbounded_String
                    (S (Matched (9).First .. Matched (9).Last));
               end if;

               --  Go to beginning of next line.
               --  If we don't have a new breakpoint, add the line to the
               --  information.

               if Multiple then
                  Skip_To_Char (S, Index, ASCII.LF);
                  Index := Index + 1;

                  --  Get the extra information associated with the breakpoint
                  --  (ignore count, commands, conditions).

                  while Index <= S'Last
                    and then not (S (Index) in '0' .. '9')
                  loop
                     Tmp := Index;
                     Skip_To_Char (S, Index, ASCII.LF);
                     Index := Index + 1;
                     Match_Extra_Breakpoint_Info (Tmp, Index, B);
                  end loop;

                  --  We are now on the line correponding to the first location
                  --  of a multiple locations breakpoint. Get the file name,
                  --  exception name and subprogram name.

                  Tmp := Index;
                  Skip_To_Char (S, Index, ASCII.LF);
                  Index := Index + 1;
                  Match
                    (Multiloc_Breakpoint_Instance_Pattern,
                     S (Tmp .. Index - 2), Matched);
                  Match_Breakpoint_Info (Matched (1).First, Index, B, M);

                  --  Skip the following lines that correspond to other
                  --  instances of the same multiple locations breakpoint.

                  while Index <= S'Last loop
                     Tmp := Index;
                     Parse_Num (S, Tmp, Breakpoint_Number);

                     exit when Integer (Breakpoint_Number) /=
                       Integer (B.Num);

                     Skip_To_Char (S, Index, ASCII.LF);
                     Index := Index + 1;
                  end loop;
               else
                  Tmp := Matched (9).First;
                  Index := Tmp;
               end if;

               if not Multiple then
                  --  For each extra line in the breakpoint info
                  while Index <= S'Last
                    and then not (S (Index) in '0' .. '9')
                  loop
                     Tmp := Index;
                     Skip_To_Char (S, Index, ASCII.LF);
                     Index := Index + 1;

                     --  File name, exception name and subprogram name can be
                     --  found on the same line.

                     Match_Breakpoint_Info (Tmp, Index, B, M);

                     --  If no file/subprogram/exception was found on the line,
                     --  we look for extra information (ignore count, commands,
                     --  conditions).

                     if not M then
                        Match_Extra_Breakpoint_Info (Tmp, Index, B);
                     end if;

                  end loop;
               end if;

               List.Append (B);

            else
               exit;
            end if;
         end loop;

         --  Fill the breakpoints extra information
         if VxWorks_Version (Debugger) = Vx653 then
            Fill_Scope_Action (Debugger);
         end if;
      end;
   end List_Breakpoints;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   overriding procedure Enable_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden) is
   begin
      if Enable then
         Send (Debugger, "enable" & Breakpoint_Identifier'Image (Num),
               Mode => Mode);
      else
         Send (Debugger, "disable" & Breakpoint_Identifier'Image (Num),
               Mode => Mode);
      end if;
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   overriding procedure Remove_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Breakpoint_Identifier;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "delete" & Breakpoint_Identifier'Image (Num),
            Mode => Mode);
   end Remove_Breakpoint;

   ---------------------
   -- List_Exceptions --
   ---------------------

   overriding function List_Exceptions
     (Debugger : access Gdb_Debugger)
     return GVD.Types.Exception_Array
   is
      S     : constant String := Send_And_Get_Clean_Output
        (Debugger, "info exceptions", Mode => Internal);
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
     (Debugger  : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger; File_Name : String) return String
   is
      File : Unbounded_String;
      Line : Natural;
      Addr : Address_Type;

   begin
      --  Given that we no longer process graphic events when sending
      --  commands to the debugger, Command_In_Process should never be true
      --  here, but in any case, let's have this additional protection:

      if Command_In_Process (Get_Process (Debugger)) then
         return File_Name;
      end if;

      Set_Parse_File_Name (Get_Process (Debugger), False);
      Switch_Language (Debugger, "c");

      declare
         Str : constant String := Send_And_Get_Clean_Output
           (Debugger, "info line " & File_Name & ":1", Mode => Internal);
      begin
         Restore_Language (Debugger);
         Set_Parse_File_Name (Get_Process (Debugger), True);
         Found_File_Name (Debugger, Str, File, Line, Addr);

         if Length (File) = 0 then
            return File_Name;
         else
            return To_String (File);
         end if;
      end;
   end Find_File;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_Debugger;
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
     (Debugger    : access Gdb_Debugger;
      Line        : Natural;
      Range_Start : out Address_Type;
      Range_End   : out Address_Type) is
   begin
      Set_Parse_File_Name (Get_Process (Debugger), False);
      Switch_Language (Debugger, "c");

      declare
         S : constant String := Send_And_Get_Clean_Output
           (Debugger, "info line" & Natural'Image (Line), Mode => Internal);
         Matched : Match_Array (0 .. 2);

      begin
         Match (Address_Range_Pattern, S, Matched);

         if Matched (0) /= No_Match then
            Range_Start := String_To_Address
              (S (Matched (1).First .. Matched (1).Last));

            Range_End := String_To_Address
              (S (Matched (2).First .. Matched (2).Last));

         else
            Range_Start := Invalid_Address;
            Range_End   := Invalid_Address;
         end if;
      end;

      Restore_Language (Debugger);
      Set_Parse_File_Name (Get_Process (Debugger), True);
   end Get_Line_Address;

   ----------------
   -- Get_Memory --
   ----------------

   overriding function Get_Memory
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger;
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
     (Debugger : access Gdb_Debugger) return Endian_Type is
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
     (Debugger  : access Gdb_Debugger;
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

   overriding procedure Open_Processes (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Remote_Protocol /= null
        and then (Debugger.Remote_Protocol.all = "wtx"
                  or else Debugger.Remote_Protocol.all = "dfw")
      then
         Debugger.WTX_List :=
            new String'
             (Send_And_Get_Clean_Output
                (Debugger, "info wtx threads", Mode => Internal));

         if Starts_With (Debugger.WTX_List.all, Undefined_Info_Command)
           or else
             Starts_With (Debugger.WTX_List.all, Undefined_Command)
           or else
             Starts_With (Debugger.WTX_List.all,
                          "warning: Unexpected arguments")
         then
            Free (Debugger.WTX_List);
            Debugger.WTX_List :=
               new String'
                (Send_And_Get_Clean_Output
                   (Debugger, "tcl activeTaskNameMap", Mode => Internal));
         end if;

         Debugger.WTX_Index := Debugger.WTX_List'First;

      else
         Open_Processes (Debugger.Handle, Debugger.Kernel);
      end if;
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   overriding procedure Next_Process
     (Debugger : access Gdb_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean)
   is
      First : Natural;
      Blank : Natural;

   begin
      if Debugger.WTX_List /= null then
         if Debugger.WTX_Index >= Debugger.WTX_List'Last then
            Success := False;

         else
            First := Debugger.WTX_Index;
            Skip_To_Blank (Debugger.WTX_List.all, Debugger.WTX_Index);
            Blank := Debugger.WTX_Index;
            Debugger.WTX_Index := Debugger.WTX_Index + 1;

            if Debugger.WTX_List (Debugger.WTX_Index) = '{' then
               Skip_To_Char (Debugger.WTX_List.all, Debugger.WTX_Index, '}');
            end if;

            Skip_To_Blank (Debugger.WTX_List.all, Debugger.WTX_Index);

            Info :=
              (Id_Len   => Blank - First,
               Info_Len => Debugger.WTX_Index - Blank - 1,
               Id       => Debugger.WTX_List (First .. Blank - 1),
               Info     =>
                 Debugger.WTX_List (Blank + 1 .. Debugger.WTX_Index - 1));
            Debugger.WTX_Index := Debugger.WTX_Index + 1;
            Success := True;
         end if;

      else
         Next_Process (Debugger.Handle, Info, Success);
      end if;
   end Next_Process;

   ---------------------
   -- Close_Processes --
   ---------------------

   overriding procedure Close_Processes (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.WTX_List /= null then
         Free (Debugger.WTX_List);
      else
         Close_Processes (Debugger.Handle);
      end if;
   end Close_Processes;

   ---------------------
   -- Detect_Language --
   ---------------------

   overriding procedure Detect_Language (Debugger : access Gdb_Debugger) is
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
     (Debugger : access Gdb_Debugger;
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

   procedure Restore_Language (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Stored_Language /= null then
         Send (Debugger, "set lang " & Debugger.Stored_Language.all);
      end if;
   end Restore_Language;

   -----------------
   -- Support_TTY --
   -----------------

   overriding function Support_TTY
     (Debugger : access Gdb_Debugger) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return True;
   end Support_TTY;

   -------------
   -- Set_TTY --
   -------------

   overriding procedure Set_TTY
     (Debugger : access Gdb_Debugger; TTY : String) is
   begin
      if TTY /= "" then
         Send (Debugger, "tty " & TTY, Mode => Hidden);
      end if;
   end Set_TTY;

   overriding procedure Filter_Output
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String)
   is
      pragma Unreferenced (Debugger, Mode);
      First, Last : Integer := 0;
   begin
      --  Strip lines starting with ^Z^Z.
      Outer_Loop :
      for J in Str'First + 1 .. Str'Last loop
         if Str (J) = ASCII.SUB and then Str (J - 1) = ASCII.SUB then
            First := J - 1;

            for K in J + 1 .. Str'Last loop
               if Str (K) = ASCII.LF then
                  Last := K;
                  exit Outer_Loop;
               end if;
            end loop;

            Last := Str'Last;
            exit Outer_Loop;
         end if;
      end loop Outer_Loop;

      if First = 0 then
         Set_Unbounded_String (Result, Str);
      else
         Set_Unbounded_String (Result, Str (Str'First .. First - 1));
         Append (Result, Str (Last + 1 .. Str'Last));
      end if;
   end Filter_Output;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   overriding function Is_Quit_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean
   is
      pragma Unreferenced (Debugger);
      Quit     : constant String := "quit     ";
   begin
      if Command'Length <= Quit'Length
        and then Command = Quit (1 .. Command'Length)
      then
         return True;
      else
         return False;
      end if;
   end Is_Quit_Command;

end Debugger.Base_Gdb.Gdb_CLI;

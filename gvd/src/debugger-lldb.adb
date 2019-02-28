------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

pragma Ada_2012;

with Ada.Tags;                            use Ada.Tags;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.Expect;                         use GNAT.Expect;
with GNAT.OS_Lib;                         use GNAT.OS_Lib;
with GNAT.Regpat;                         use GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.VFS;                        use GNATCOLL.VFS;
with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.Utils;                      use GNATCOLL.Utils;

with Default_Preferences;                 use Default_Preferences;

with GPS.Kernel.Hooks;                    use GPS.Kernel.Hooks;
with GPS.Markers;

with GVD.Preferences;                     use GVD.Preferences;
with GVD.Proc_Utils;                      use GVD.Proc_Utils;
with GVD.Process;                         use GVD.Process;
with GVD.Trace;                           use GVD.Trace;
with GVD.Types;                           use GVD.Types;

with Language;                            use Language;
with Process_Proxies;                     use Process_Proxies;
with Remote;                              use Remote;

with Language.Debugger;                   use Language.Debugger;
with Language.Debugger.Lldb.C;            use Language.Debugger.Lldb.C;
with Language.Debugger.Lldb.Cpp;          use Language.Debugger.Lldb.Cpp;
with GVD.Variables.Types;                 use GVD.Variables.Types;
with GVD.Variables.Types.Arrays;          use GVD.Variables.Types.Arrays;
with GVD.Variables.Types.Simples;         use GVD.Variables.Types.Simples;
with GVD.Variables.Types.Simples.Strings;
use GVD.Variables.Types.Simples.Strings;
with GVD.Variables.Types.Records;         use GVD.Variables.Types.Records;
with GVD.Variables.Types.Classes;         use GVD.Variables.Types.Classes;

with String_Utils;                        use String_Utils;

package body Debugger.LLDB is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.LLDB", On);

   LLDB_Options                    : constant String := "--debug";
   --  Options always passed to lldb
   --    -d
   --    --debug
   --    Tells the debugger to print out extra information for debugging
   --    itself.

   Prompt_String                   : constant String := "(lldb) ";
   --  The prompt used by the debugger

   Prompt_Regexp                   : constant Pattern_Matcher := Compile
     ("^\(lldb\).*$", Multiple_Lines);
   --  lldb prompt

   Highlight_Pattern               : constant Pattern_Matcher :=
     Compile ("\(lldb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window

   Main_Method_Regexp              : constant Pattern_Matcher := Compile
     ("^procedure\s*(\S*)\s*is", Multiple_Lines);
   --  program's main method

   Binder_File_Name_Regexp         : constant Pattern_Matcher :=
     Compile ("(b(~|_).+\.(adb|c))", Multiple_Lines);
   --  Matches a binder file name

   Image_Lookup_Regexp             : constant Pattern_Matcher := Compile
     ("^(\d+) matches found", Multiple_Lines);
   --  Detects the 'image lookup' command's result

   Image_Lookup_Summary_Regexp     : constant Pattern_Matcher := Compile
     ("Summary:.*at\s*(\S*)", Single_Line);
   --  Retrive file name and line from 'image lookup' command's result

   Image_Lookup_Address_Regexp     : constant Pattern_Matcher := Compile
     ("Address:.*\[(0x[0-9a-zA-Z]+)\]", Single_Line);
   --  Retrive address from 'image lookup' command's result

   Image_Lookup_CompileUnit_Regexp : constant Pattern_Matcher := Compile
     ("CompileUnit: id = {0x[0-9a-zA-Z]+}, " &
        "file = ""(.*)"", language = ""(.*)""", Single_Line);
   --  Retrive full path and language from 'image lookup' command's result

   File_Line_Regexp                : constant Pattern_Matcher := Compile
     ("(.+):(\d+)$", Single_Line);
   --  Regular expression to separate line from file name

   Breakpoint_Regexp               : constant Pattern_Matcher := Compile
     ("^Breakpoint (\d+):|(\d+) breakpoints (deleted|enabled|disabled)",
      Multiple_Lines);
   --  Regular expression for catching breackpoints' changes

   Watchpoint_Regexp               : constant Pattern_Matcher := Compile
     ("^Watchpoint created:|^(\d+) watchpoints (modified|deleted).",
      Multiple_Lines);
   --  Regular expression for catching watchpoints' changes

   Breakpoint_Info_Regexp          : constant Pattern_Matcher := Compile
     ("^(\d+):\s(file = '(\S+)',\sline = (\d+),\sexact_match = (\d+))?" &
        "(name = '(\S+)')?(address =[\s\S]+\[(0x[0-9a-zA-Z]+)\])?," &
        "\slocations = (\d+)",
      Single_Line);
   Breakpoint_Info_Regexp_Address_Idx : constant := 9;
   Breakpoint_Info_Regexp_Last        : constant := 10;
   --  Regular expression for parsing a breackpoint information

   Breakpoint_Num_Regexp              : constant Pattern_Matcher := Compile
     ("^(\d+):", Single_Line);

   Breakpoint_Where_Regexp         : constant Pattern_Matcher := Compile
     ("(\d+.\d+):\swhere = ([\S\s]+),\saddress = " &
        "(\S+)?(\[)?(0x[0-9a-zA-Z]+)(\])?",
      Single_Line);
   --  Regular expression for parsing a breackpoint's where information

   Exception_Breakpoint_Regexp     : constant Pattern_Matcher := Compile
     ("^(\d+): Exception breakpoint \(catch: (on) throw: (on)\)",
      Single_Line);

   Breakpoint_Options_Regexp       : constant Pattern_Matcher := Compile
     ("Options:\s*([\s\S]*)", Single_Line);

   Breakpoint_Condition_Regexp     : constant Pattern_Matcher := Compile
     ("Condition:\s*([\s\S]*)", Single_Line);

   Breakpoint_Where_File_Regexp    : constant Pattern_Matcher := Compile
     ("^([\s\S]+) at (\S+):(\d+)$", Single_Line);
   --  Regular expression for parsing a breackpoint's where information

   Watchpoint_Info_Regexp          : constant Pattern_Matcher := Compile
     ("^Watchpoint (\d+): addr = (0x[0-9a-zA-Z]+) size = (\d+)" &
        " state = (\S+) type = (\S+)",
      Single_Line);
   --  Regular expression for parsing a watchpoint's information

   Watchpoint_Expression_Regexp    : constant Pattern_Matcher := Compile
     ("watchpoint spec = '(.+)'", Single_Line);
   --  Regular expression for parsing a watchpoint's expression

   Watchpoint_Condition_Regexp     : constant Pattern_Matcher := Compile
     ("condition = '(.+)'", Single_Line);
   --  Regular expression for parsing a watchpoint's expression

   Watchpoint_Location_Regexp     : constant Pattern_Matcher := Compile
     ("declare @ '(.+):(\d+)'", Single_Line);
   --  Regular expression for parsing a watchpoint's location

   Error_Regexp                    : constant Pattern_Matcher := Compile
     ("^error: ([\s\S]+)", Multiple_Lines);
   --  Regular expression for detect error

   Frame_Regexp                    : constant Pattern_Matcher := Compile
     ("frame #(\d+): (0x[0-9a-zA-Z]+) (([\S]+)( at (\S+))?)",
      Multiple_Lines);
   --  Regular expression for parse a frame information
   --    frame #0: 0x0000000000402b1c foo`_ada_foo at foo.adb:15

   Running_Regexp                  : constant Pattern_Matcher := Compile
     ("^Process \d+ launched:", Multiple_Lines);
   --  Regular expression for detecting whether a debugging process is running

   Stopped_Regexp                  : constant Pattern_Matcher := Compile
     ("^Process \d+ stopped", Multiple_Lines);
   --  Regular expression for detecting whether a debugging process is stopped

   Disassemble_Regexp              : constant Pattern_Matcher := Compile
     ("^(->)?\s+(0x[0-9a-zA-Z]+) (\<\+\d+\>):\s+(.+)()", Single_Line);
   --  Regular expression for parsing disassemble output

   LineEntry_Regexp              : constant Pattern_Matcher := Compile
     ("LineEntry: \[(0x[0-9a-zA-Z]+)-(0x[0-9a-zA-Z]+)\):", Multiple_Lines);
   --  Regular expression for parsing line lookup

   Register_Regexp              : constant Pattern_Matcher := Compile
     ("(\S+) = (.+)", Single_Line);
   --  Regular expression for parsing registers

   Language_Regexp              : constant Pattern_Matcher := Compile
     ("^CompileUnit:.+, language = \""(\S+)\""", Multiple_Lines);
   --  Expression for detecting language

   Type_Of_Regexp              : constant Pattern_Matcher := Compile
     ("^\((.+)\)", Multiple_Lines);
   --  Expression for extracting type_of information

   Type_Lookuo_Regexp          : constant Pattern_Matcher := Compile
     ("compiler_type = ""([^""]+)""", Multiple_Lines);

   C_Languages_Regexp          : constant Pattern_Matcher := Compile
     ("^c$|^c11$|^c89$|^c99$", Multiple_Lines);

   Cpp_Languages_Regexp        : constant Pattern_Matcher := Compile
     ("^c\+\+", Multiple_Lines);

   Value_Of_Regexp             : constant Pattern_Matcher := Compile
     ("^\([^\)]+\)[^=]+=\s([\s\S]+)$", Multiple_Lines);

   Addr_Of_Variable_Regexp     : constant Pattern_Matcher := Compile
     ("^\(.+\) \$[\d]+ = (0x[0-9a-f]+)$", Multiple_Lines);

   Type_Name_Regexp            : constant Pattern_Matcher := Compile
     ("^\((.+\)) \S+ =", Multiple_Lines);

   procedure Connect_To_Target_If_Needed (Debugger : access LLDB_Debugger);
   --  Check and connect to remoute target if it's needed

   procedure Find_Main_Unit (Debugger : access LLDB_Debugger);
   --  Looks for program's main unit

   procedure Reset_State (Debugger : access LLDB_Debugger);
   --  Resets the Debugger state before sending a command

   procedure Filter_Breakpoint
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Process changes in breakpoints

   function Internal_Set_Breakpoint
     (Debugger  : access LLDB_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier;
   --  Executes breakpoint command and return number of created one

   procedure Run_Helper
     (Debugger  : access LLDB_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean);
   --  Shared code beteen Run/Start

   function Set_File
     (Debugger : access LLDB_Debugger;
      File     : String)
      return Virtual_File;
   --  Retrives file information

   procedure Filter_Running
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);

   procedure Filter_Stopped
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);

   procedure Parse_Disassembled
     (S    : String;
      Code : out Disassemble_Elements);

   procedure Filter_Language
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);

   function Is_Expression (Item : String) return Boolean;
   --  Returns True is Item is expression

   function Convert (F : Value_Format) return String;
   --  Converts format to lldb name

   -----------
   -- Spawn --
   -----------

   overriding procedure Spawn
     (Debugger        : access LLDB_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Debugger_Num    : Natural;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      LLDB_Arguments   : Argument_List_Access :=
        Argument_String_To_List (LLDB_Options);
      Num_Options     : constant Natural := LLDB_Arguments'Length;
      Local_Arguments : Argument_List
        (1 .. Debugger_Args'Length + Num_Options);
      Process         : Visual_Debugger;

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);

   begin
      Local_Arguments (1 .. Num_Options) := LLDB_Arguments.all;
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) :=
        Debugger_Args;
      Free (LLDB_Arguments);

      Debugger.General_Spawn
        (Kernel        => Kernel,
         Arguments     => Local_Arguments,
         Debugger_Name =>
           (if Debugger_Name = ""
            then "lldb"
            else Debugger_Name),
         Debugger_Num  => Debugger_Num,
         Proxy         => Proxy);

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
         Debugger.Remote_Target   := new String'(Remote_Target);
         Debugger.Remote_Protocol := new String'(Remote_Protocol);
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      Process := Convert (Debugger);

      if Process /= null then
         Process.Add_Regexp_Filter
           (Filter_Breakpoint'Access, Breakpoint_Regexp);
         Process.Add_Regexp_Filter
           (Filter_Breakpoint'Access, Watchpoint_Regexp);

         Process.Add_Regexp_Filter (Filter_Language'Access, Language_Regexp);

         Process.Add_Regexp_Filter (Filter_Running'Access, Running_Regexp);
         Process.Add_Regexp_Filter (Filter_Stopped'Access, Stopped_Regexp);

         GVD.Trace.Set_Input_Output_Filter (Process);
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Debugger : access LLDB_Debugger)
   is
      Num     : Expect_Match;
      Lang    : Language_Access;
      Process : Visual_Debugger;

      use GVD;
      pragma Unreferenced (Num, Lang);
   begin
      --  Wait for initial output and prompt (and display it in the window)
      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => -1);

      --  Make sure lldb will not ask too much interactive questions.
      --  Interactive questions are better left to the GUI itself.
      Debugger.Send ("settings set auto-confirm true", Mode => Internal);
      Debugger.Send ("settings set target.move-to-nearest-code true",
                     Mode => Internal);
      Debugger.Send ("settings set interpreter.prompt-on-quit false",
                     Mode => Internal);
      Debugger.Send ("settings set target.process.disable-stdio false",
                     Mode => Internal);
      --  settings set target.source-map ./ ./../

      --  Load the module to debug, if any

      if Debugger.Executable /= GNATCOLL.VFS.No_File then
         Debugger.Set_Executable (Debugger.Executable, Mode => Visible);

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

         Lang := Debugger.Get_Language;

         if Get_Pref (Open_Main_Unit) then
            Find_Main_Unit (Debugger);
         end if;

      end if;

      if Debugger.Executable_Args /= null then
         Debugger.Send
           ("settings set target.run-args " &
              Debugger.Executable_Args.all, Mode => Internal);
      end if;

   exception
      --  If the executable was not found, simply display the prompt before
      --  leaving, nothing else needs to be done.

      when Executable_Not_Found =>
         Debugger.Display_Prompt;
   end Initialize;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Debugger        : access LLDB_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      --  Override Send to recognize some commands such as "target"
      Debugger.Reset_State;
      Debugger.Prepare_Target_For_Send (Cmd);

      --  Call the Parent procedure

      Debugger_Root (Debugger.all).Send
        (Cmd, Empty_Buffer, Wait_For_Prompt, Force_Send, Mode);
   end Send;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   overriding procedure Connect_To_Target
     (Debugger : access LLDB_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Process : constant Visual_Debugger := Convert (Debugger);
      Cmd     : constant String := "target " & Protocol & " " & Target;
      Timeout : constant Integer := Connection_Timeout.Get_Pref;
      Success : Boolean;
   begin
      --  If the debugger is already connected, kill the connection if Force
      --  is True or simply return otherwise.
      if Debugger.Target_Connected then
         if Force then
            Debugger.Interrupt;
            Debugger.Wait_Prompt;
         else
            return;
         end if;
      end if;

      --  Send the command to the debugger in a non-blocking way and check if
      --  it succeed.
      Debugger.Send
        (Cmd             => Cmd,
         Wait_For_Prompt => False,
         Mode            => Mode);
      Success := Debugger.Wait_Prompt (Timeout);

      --  Mark the command as processed, even if did not succeed in the
      --  specified timeout so that we can continue sending other commands.
      Set_Command_In_Process (Get_Process (Debugger), False);
      Free (Process.Current_Command);

      if Success then
         Output_Text
           (Process, Protocol & " debugging using " & Target & ASCII.LF);

         if Protocol = "remote" then
            Debugger.Set_Is_Started (True);
         end if;
      else
         --  If it
         Debugger.Interrupt;
         Debugger.Wait_Prompt;

         Output_Text (Process, "Can't connect to the target using "
                      & Protocol & " protocol on " & Target & ASCII.LF);
      end if;

      Debugger.Target_Connected := Success;
      Debugger.Display_Prompt;
   end Connect_To_Target;

   ----------------------------
   -- Is_Connected_To_Target --
   ----------------------------

   overriding function Is_Connected_To_Target
     (Debugger : access LLDB_Debugger)
      return Boolean
   is
      (Debugger.Target_Connected);

   -------------------
   -- Is_Expression --
   -------------------

   function Is_Expression (Item : String) return Boolean is
   begin
      for Char of Item loop
         if Char not in '0' .. '9'
           and then Char not in 'A' .. 'Z'
           and then Char not in 'a' .. 'z'
           and then Char /= '_'
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Expression;

   -------------------------------
   -- Send_And_Get_Clean_Output --
   -------------------------------

   overriding function Send_And_Get_Clean_Output
     (Debugger : access LLDB_Debugger;
      Cmd      : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
      return String is
   begin
      Debugger.Reset_State;

      declare
         S   : constant String := Debugger.Send_And_Get_Output (Cmd, Mode);
         Pos : Integer;
      begin
         if Ends_With (S, Prompt_String) then
            Pos := S'Last - Prompt_String'Length;
            if Pos >= S'First
              and then S (Pos) = ASCII.LF
            then
               Pos := Pos - 1;
            end if;

            return S (S'First .. Pos);
         else
            return S;
         end if;
      end;
   end Send_And_Get_Clean_Output;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   overriding function Highlighting_Pattern
     (Debugger : access LLDB_Debugger)
      return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   ---------------------
   -- Detect_Language --
   ---------------------

   overriding procedure Detect_Language (Debugger : access LLDB_Debugger) is
      Language : Language_Access;

   begin
      if not Debugger.Is_Started then
         Language := Debugger.Get_Language ("");
         if Language = null then
            Language := Debugger.Get_Language ("c");
            if Language = null then
               Language := new LLDB_C_Language;

               Set_Debugger
                 (Language_Debugger_Access (Language), Debugger.all'Access);
            end if;
         end if;
         Debugger.Set_Language (Language);

      else
         declare
            Responce : constant String := Debugger.Send_And_Get_Clean_Output
              ("frame info", Internal);
            Matched  : Match_Array (0 .. 6);
         begin
            Match (Frame_Regexp, Responce, Matched);
            if Matched (0) /= No_Match then
               declare
                  S : constant String := Debugger.Send_And_Get_Clean_Output
                    ("image lookup --verbose -a " &
                       Responce (Matched (2).First .. Matched (2).Last),
                     Mode => Internal);
                  pragma Unreferenced (S);
               begin
                  null;
               end;
            end if;
         end;
      end if;
   end Detect_Language;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt (Debugger : access LLDB_Debugger) is
      Dummy : Expect_Match;
   begin
      Debugger.Get_Process.Wait (Dummy, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding function Wait_Prompt
     (Debugger : access LLDB_Debugger;
      Timeout  : Integer)
      return Boolean
   is
      Num : Expect_Match;
   begin
      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => Timeout);
      return Num /= Expect_Timeout;
   end Wait_Prompt;

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt (Debugger : access LLDB_Debugger) is
      Proc : constant Visual_Debugger := Convert (Debugger);
   begin
      if Proc /= null then
         Proc.Output_Text
           ("(lldb) ", Is_Command => False, Set_Position => True);
      end if;
   end Display_Prompt;

   -------------
   -- Type_Of --
   -------------

   overriding function Type_Of
     (Debugger : access LLDB_Debugger;
      Entity   : String)
      return String is
   begin
      --  If Entity contains a LF, this is an invalid entity, so give up
      --  immediately.

      for J in reverse Entity'Range loop
         if Entity (J) = ASCII.LF then
            return "";
         end if;
      end loop;

      declare
         S   : constant String := Debugger.Send_And_Get_Clean_Output
           ((if Is_Expression (Entity)
             then "expression --show-types -- "
             else "frame variable --show-types ") & Entity,
            Mode => Internal);
         Matched : Match_Array (0 .. 1);
      begin
         Match (Type_Of_Regexp, S, Matched);
         if Matched (1) /= No_Match then
            return S (Matched (1).First .. Matched (1).Last);
         else
            return Debugger.Get_Type (Entity);
         end if;
      end;
   end Type_Of;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Debugger : access LLDB_Debugger;
      Entity   : String) return String
   is
      use Ada.Strings.Unbounded;

      S   : Unbounded_String := To_Unbounded_String
        (Debugger.Send_And_Get_Clean_Output
           ("image lookup --type " & Entity, Mode => Internal));
      Matched : Match_Array (0 .. 1);
      Result  : Unbounded_String;
   begin
      loop
         Match (Type_Lookuo_Regexp, To_String (S), Matched);
         exit when Matched (1) = No_Match;
         Result := Unbounded_Slice (S, Matched (1).First, Matched (1).Last);
         S := Unbounded_Slice (S, Matched (1).Last + 1, Length (S));
      end loop;

      return To_String (Result);
   end Get_Type;

   -----------------
   -- Info_Locals --
   -----------------

   overriding function Info_Locals
     (Debugger : access LLDB_Debugger)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "frame variable --no-args";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   overriding function Info_Args
     (Debugger : access LLDB_Debugger)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "frame variable --no-locals";
   end Info_Args;

   ----------------
   -- Info_Tasks --
   ----------------

   overriding procedure Info_Tasks
     (Debugger : access LLDB_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Debugger, Info);
   begin
      Len := 0;
   end Info_Tasks;

   ------------------
   -- Info_Threads --
   ------------------

   overriding procedure Info_Threads
     (Debugger : access LLDB_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      Output : constant String := Debugger.Send_And_Get_Clean_Output
        ("thread list", Mode => Internal);
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

         exit when EOL < Output'First and then Len > Info'First + 1;

         Info (Len) :=
           (Num_Fields => 1,
            Information => (1 => New_String (Output (EOL + 1 .. Index))));
         Index := EOL - 1;
      end loop;
   end Info_Threads;

   -------------
   -- Info_PD --
   -------------

   overriding procedure Info_PD
     (Debugger : access LLDB_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Debugger, Info);
   begin
      Len := 0;
   end Info_PD;

   -------------
   -- Convert --
   -------------

   function Convert (F : Value_Format) return String is
   begin
      case F is
         when Default_Format => return "";
         when Decimal        => return "decimal";
         when Binary         => return "binary";
         when Hexadecimal    => return "hex";
         when Octal       => return "octal";
      end case;
   end Convert;

   --------------
   -- Value_Of --
   --------------

   overriding function Value_Of
     (Debugger : access LLDB_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format)
      return String
   is
      S   : constant String := Debugger.Send_And_Get_Clean_Output
        ((if Is_Expression (Entity)
          then "expression "
          else "frame variable ") &
         (if Format = Default_Format
            then ""
            else "--format " & Convert (Format)  & " ") &
           Entity,
         Mode => Internal);
      Matched : Match_Array (0 .. 1);
   begin
      Match (Value_Of_Regexp, S, Matched);
      if Matched (1) /= No_Match then
         return S (Matched (1).First .. Matched (1).Last);
      else
         return "";
      end if;
   end Value_Of;

   -----------------------------
   -- Prepare_Target_For_Send --
   -----------------------------

   procedure Prepare_Target_For_Send
     (Debugger : access LLDB_Debugger;
      Cmd      : String)
   is
      J, K : Integer;
   begin
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

            J := K + 1;
            Skip_Blanks (Cmd, J);
            Free (Debugger.Remote_Target);
            Debugger.Remote_Target := new String'(Cmd (J .. Cmd'Last));
         end if;
      end if;
   end Prepare_Target_For_Send;

   ---------------------
   -- Print_Value_Cmd --
   ---------------------

   overriding function Print_Value_Cmd
     (Debugger : access LLDB_Debugger;
      Entity   : String)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "expression " & Entity;
   end Print_Value_Cmd;

   ----------------------
   -- Change_Directory --
   ----------------------

   overriding procedure Change_Directory
     (Debugger    : access LLDB_Debugger;
      Dir         : GNATCOLL.VFS.Virtual_File;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Directory : constant String := +Dir.Unix_Style_Full_Name;
   begin
      Debugger.Send
        ("platform settings --working-dir " & Directory, Mode => Mode);
   end Change_Directory;

   --------------------
   -- Set_Executable --
   --------------------

   overriding procedure Set_Executable
     (Debugger   : access LLDB_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Mode);

      Remote_Exec         : constant Virtual_File := To_Remote
        (Executable, Get_Nickname (Debug_Server));
      Full_Name           : constant String :=
        +Remote_Exec.Unix_Style_Full_Name;
      No_Such_File_Regexp : constant Pattern_Matcher := Compile
        ("error: unable to find executable for");
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
         Cmd := new String'
           (Command &
            (if Is_Local (Debug_Server)
               then " "
               else " -r ") &
              Full_Name);

         if Process /= null then
            Process.Output_Text (Cmd.all & ASCII.LF, Set_Position => True);
         end if;

         declare
            S : constant String := Debugger.Send_And_Get_Clean_Output
              (Cmd.all, Mode => Hidden);
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
      Launch_Command_And_Output ("target create");

      --  Connect to the remote target if needed

      Connect_To_Target_If_Needed (Debugger);
      Debugger.Set_Is_Started (False);

      --  Report a change in the executable. This has to be done before we
      --  look for the current file and line, so that the explorer can be
      --  correctly updated.
      --  No need to do anything in text-only mode

      if Process /= null then
         Debugger_Executable_Changed_Hook.Run (Process.Kernel, Process);
      end if;

      if Get_Pref (Break_On_Exception) then
         declare
            Cmd : constant String :=
              "breakpoint set --language-exception c " &
              "--on-throw true --on-catch true";
            S   : constant String := Debugger.Send_And_Get_Clean_Output (Cmd);

         begin
            if Process /= null then
               Process.Output_Text (Cmd & ASCII.LF, Set_Position => True);

               if S /= "" then
                  Process.Output_Text (S & ASCII.LF, Set_Position => True);
               end if;

               Debugger.Display_Prompt;
            end if;
         end;
      end if;

      if Get_Pref (Open_Main_Unit) then
         Find_Main_Unit (Debugger);
      end if;
   end Set_Executable;

   --------------------
   -- Get_Executable --
   --------------------

   overriding function Get_Executable
     (Debugger : access LLDB_Debugger)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   overriding procedure Load_Core_File
     (Debugger : access LLDB_Debugger;
      Core     : GNATCOLL.VFS.Virtual_File;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Core, Mode);
   begin
      Trace (Me, "Load_Core_File does not supported");
   end Load_Core_File;

   -----------------
   -- Add_Symbols --
   -----------------

   overriding procedure Add_Symbols
     (Debugger : access LLDB_Debugger;
      Module   : GNATCOLL.VFS.Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Address);

      Symbols : constant String := +Module.Unix_Style_Full_Name;
   begin
      Debugger.Send ("target symbols add " & Symbols, Mode => Mode);

      if Mode in Visible_Command then
         Debugger.Wait_User_Command;
      end if;
   end Add_Symbols;

   ---------------------
   -- Load_Executable --
   ---------------------

   overriding procedure Load_Executable
     (Debugger   : access LLDB_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Debugger.Is_Connected_To_Target then

         if Executable /= GNATCOLL.VFS.No_File then
            Send
              (Debugger, "platform put-file """ &
               (+Executable.Unix_Style_Full_Name) & '"',
               Mode => Mode);
         end if;

         if Mode in Visible_Command then
            Wait_User_Command (Debugger);
         end if;
      end if;
   end Load_Executable;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Debugger : access LLDB_Debugger) is
   begin
      Debugger.Breakpoints_Changed := False;
   end Reset_State;

   ----------------
   -- Run_Helper --
   ----------------

   procedure Run_Helper
     (Debugger  : access LLDB_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean) is
   begin
      if Arguments /= "" then
         Debugger.Send
           ("settings set target.run-args " & Arguments, Mode => Internal);
      end if;

      Debugger.Send
        ("process launch" & (if Start then " --stop-at-entry" else ""),
         Mode => Mode);

      Debugger.Set_Is_Started (True);
   end Run_Helper;

   ---------
   -- Run --
   ---------

   overriding procedure Run
     (Debugger  : access LLDB_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => False);
   end Run;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Debugger : access LLDB_Debugger;
      Arguments : String := "";
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => True);
   end Start;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access LLDB_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Name : Boolean := False;
   begin
      for C of Process loop
         if C not in '0' .. '9' then
            Name := True;
            exit;
         end if;
      end loop;

      if Name then
         Send (Debugger, "process attach --name " & Process, Mode => Mode);
      else
         Send (Debugger, "process attach --pid " & Process, Mode => Mode);
      end if;

      Debugger.Set_Is_Started (True);

      if Mode in Visible_Command then
         Debugger.Wait_User_Command;
      end if;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   overriding procedure Detach_Process
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("process detach", Mode => Mode);
      Debugger.Set_Is_Started (False);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("process kill", Mode => Mode);
      Debugger.Set_Is_Started (False);
   end Kill_Process;

   ---------------
   -- Step_Into --
   ---------------

   overriding procedure Step_Into
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("thread step-in", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   overriding procedure Step_Over
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("thread step-over", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   overriding procedure Step_Into_Instruction
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("thread step-inst", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   overriding procedure Step_Over_Instruction
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("thread step-inst-over", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("process continue", Mode => Mode);
   end Continue;

   -----------------------------
   -- Continue_Until_Location --
   -----------------------------

   overriding procedure Continue_Until_Location
     (Debugger : access LLDB_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      BP_Identifier : GVD.Types.Breakpoint_Identifier with Unreferenced;
   begin
      --  There is no real equivalent of the GDB "until" command in LLDB so
      --  set a temporary breakpoint and continue until we reach it instead.
      BP_Identifier := Debugger.Break_Source
        (File      => File,
         Line      => Line,
         Temporary => True,
         Mode      => Mode);
      Debugger.Continue (Mode => Mode);
   end Continue_Until_Location;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access LLDB_Debugger) is
      Proxy      : constant Process_Proxy_Access      := Debugger.Get_Process;
      Descriptor : constant Process_Descriptor_Access := Proxy.Get_Descriptor;

   begin
      Descriptor.Interrupt;
      Proxy.Set_Interrupted;
      --  Consider using 'process interrupt' when not Command_In_Process???
      --  Send (Debugger, "process interrupt", Mode => Internal);
   end Interrupt;

   ------------------
   -- Command_Kind --
   ------------------

   overriding function Command_Kind
     (Debugger : access LLDB_Debugger;
      Command  : String)
      return Command_Category
   is
      Index : Natural;
   begin
      if Command = "" then
         Debugger.Current_Command_Kind := Misc_Command;
         return Misc_Command;
      end if;

      Index := Command'First;
      Skip_Word (Command, Index);

      if Starts_With (Command, "target create") then
         Debugger.Current_Command_Kind := Load_Command;

      elsif Starts_With (Command, "process launch")
        or else Starts_With (Command, "process continue")
        or else Starts_With (Command, "process detach")
        or else Starts_With (Command, "thread step")
        or else Command (Command'First .. Index - 1) = "run"
        or else Command (Command'First .. Index - 1) = "r"
        or else Command (Command'First .. Index - 1) = "continue"
        or else Command (Command'First .. Index - 1) = "c"
        or else Command (Command'First .. Index - 1) = "next"
        or else Command (Command'First .. Index - 1) = "nexti"
        or else Command (Command'First .. Index - 1) = "ni"
        or else Command (Command'First .. Index - 1) = "s"
        or else Command (Command'First .. Index - 1) = "si"
        or else Command (Command'First .. Index - 1) = "sif"
        or else Command (Command'First .. Index - 1) = "step"
        or else Command (Command'First .. Index - 1) = "stepi"
        or else Command (Command'First .. Index - 1) = "finish"
      then
         Debugger.Current_Command_Kind := Execution_Command;

      elsif Starts_With (Command, "process status")
        or else Starts_With (Command, "process kill")
        or else Starts_With (Command, "target symbols")
        or else Starts_With (Command, "frame info")
        or else Starts_With (Command, "platform settings")

        or else Command (Command'First .. Index - 1) = "settings"
        or else Command (Command'First .. Index - 1) = "run-args"
        or else Command (Command'First .. Index - 1) = "breakpoint"
        or else Command (Command'First .. Index - 1) = "watchpoint"
        or else Command (Command'First .. Index - 1) = "rbreak"
        or else Command (Command'First .. Index - 1) = "tbreak"
        or else Command (Command'First .. Index - 1) = "b"
        or else Command (Command'First .. Index - 1) = "source"
        or else Command (Command'First .. Index - 1) = "image"
        or else Command (Command'First .. Index - 1) = "thread"
        or else Command (Command'First .. Index - 1) = "quit"
        or else Command (Command'First .. Index - 1) = "disassemble"
        or else Command (Command'First .. Index - 1) = "register"

        or else Command (Command'First .. Index - 1) = "x"
        or else Command (Command'First .. Index - 1) = "bt"
        or else Command (Command'First .. Index - 1) = "call"
        or else Command (Command'First .. Index - 1) = "di"
        or else Command (Command'First .. Index - 1) = "dis"
        or else Command (Command'First .. Index - 1) = "display"
        or else Command (Command'First .. Index - 1) = "l"
        or else Command (Command'First .. Index - 1) = "list"
        or else Command (Command'First .. Index - 1) = "p"
        or else Command (Command'First .. Index - 1) = "parray"
        or else Command (Command'First .. Index - 1) = "po"
        or else Command (Command'First .. Index - 1) = "poarray"
        or else Command (Command'First .. Index - 1) = "print"
        or else Command (Command'First .. Index - 1) = "expression"
      then
         Debugger.Current_Command_Kind := Misc_Command;

      elsif Starts_With (Command, "frame select")
        or else Command (Command'First .. Index - 1) = "down"
        or else Command (Command'First .. Index - 1) = "up"
        or else Command (Command'First .. Index - 1) = "f"
      then
         Debugger.Current_Command_Kind := Context_Command;

      else
         Debugger.Current_Command_Kind := Misc_Command;
         Me.Trace ("Unimplemented Command_Kind command:" & Command);
      end if;

      return Debugger.Current_Command_Kind;
   end Command_Kind;

   -------------------------
   -- Breakpoints_Changed --
   -------------------------

   overriding function Breakpoints_Changed
     (Debugger : access LLDB_Debugger;
      Command  : String)
      return Boolean
   is
   begin
      return Debugger.Breakpoints_Changed
        or else (Starts_With (Command, "breakpoint")
                 and then not Starts_With (Command, "breakpoint list")
                 and then not Starts_With (Command, "breakpoint name"))
        or else (Starts_With (Command, "watchpoint")
                 and then not Starts_With (Command, "watchpoint list"))
        or else Starts_With (Command, "b")
        or else Starts_With (Command, "rbreak")
        or else Starts_With (Command, "tbreak");
   end Breakpoints_Changed;

   ----------------
   -- Stack_Down --
   ----------------

   overriding procedure Stack_Down
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("frame select --relative -1", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("frame select --relative 1", Mode => Mode);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access LLDB_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("frame select" & Natural'Image (Frame - 1), Mode => Mode);
   end Stack_Frame;

   -----------------------
   -- Filter_Breakpoint --
   -----------------------

   procedure Filter_Breakpoint
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      LLDB_Debugger (Process.Debugger.all).Breakpoints_Changed := True;
   end Filter_Breakpoint;

   ---------------------
   -- Filter_Language --
   ---------------------

   procedure Filter_Language
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Debugger : constant Debugger_Access := Process.Debugger;
      Lang     : constant String :=
        Str (Matched (1).First .. Matched (1).Last);
      Language : Language_Access;

      C   : Match_Array (0 .. 1);
      Cpp : Match_Array (0 .. 1);
   begin
      --  Is this a language we have seen before ? If yes, reuse it in case
      --  it needs to dynamically query the debugger to find out if a
      --  feature is supported, to avoid doing it every time we switch to
      --  that language

      Match (C_Languages_Regexp, Lang, C);
      if C (0) /= No_Match then
         Language := Debugger.Get_Language ("c");
      end if;

      Match (Cpp_Languages_Regexp, Lang, Cpp);
      if Cpp (0) /= No_Match then
         Language := Debugger.Get_Language ("c++");
      end if;

      if Language = null then
         if C (0) /= No_Match then
            Language := new LLDB_C_Language;

         elsif Cpp (0) /= No_Match then
            Language := new LLDB_Cpp_Language;

--           elsif Lang = "ada" then
--              Language := new Gdb_Ada_Language;

         else
            Output_Error
              (Process.Kernel,
               "Language unknown, defaulting to C: " & Lang);

            --  We need to check whether we already have C defined:
            Language := Debugger.Get_Language ("c");
            if Language = null then
               Language := new LLDB_C_Language;
            end if;
         end if;

         Set_Debugger
           (Language_Debugger_Access (Language), Debugger.all'Access);
      end if;

      Debugger.Set_Language (Language);
   end Filter_Language;

   -------------------
   -- Filter_Output --
   -------------------

   overriding procedure Filter_Output
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String)
   is
      pragma Unreferenced (Debugger, Mode);

   begin
      Set_Unbounded_String (Result, Str);
   end Filter_Output;

   --------------------
   -- Filter_Running --
   --------------------

   procedure Filter_Running
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Matched, Str);
   begin
      LLDB_Debugger (Process.Debugger.all).Is_Running := True;
   end Filter_Running;

   --------------------
   -- Filter_Stopped --
   --------------------

   procedure Filter_Stopped
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Matched, Str);
   begin
      LLDB_Debugger (Process.Debugger.all).Is_Running := False;
   end Filter_Stopped;

   --------------------
   -- Find_Main_Unit --
   --------------------

   procedure Find_Main_Unit (Debugger : access LLDB_Debugger)
   is
      Str : constant String := Ada.Characters.Handling.To_Lower
        (Debugger.Send_And_Get_Clean_Output
           ("source list --count 1", Mode => Internal));
      Matched : Match_Array (0 .. 1);
   begin
      Match (Main_Method_Regexp, Str, Matched);
      if Matched (0) = No_Match then
         return;
      end if;

      Debugger.Send
        ("image lookup --no-inlines --verbose --function " &
           Str (Matched (1).First .. Matched (1).Last),
         Mode => Internal);
   end Find_Main_Unit;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("thread step-out", Mode => Mode);
   end Finish;

   ---------------------
   -- Found_File_Name --
   ---------------------

   overriding procedure Found_File_Name
     (Debugger    : access LLDB_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      Matched  : Match_Array (0 .. 6);
      FMatched : Match_Array (0 .. 2);
   begin
      --  Default values if nothing better is found
      Addr := Invalid_Address;
      Name := Null_Unbounded_String;
      Line := 0;

      Match (Frame_Regexp, Str, Matched);
      if Matched (0) /= No_Match then
         Addr := String_To_Address
           (Str (Matched (2).First .. Matched (2).Last));

         if Matched (6) /= No_Match then
            Match
              (File_Line_Regexp,
               Str (Matched (6).First .. Matched (6).Last),
               FMatched);

            if FMatched (0) /= No_Match then
               Name := To_Unbounded_String
                 (+Set_File
                    (Debugger,
                     Str
                       (FMatched (1).First .. FMatched (1).Last)).Full_Name);

               Line := Natural'Value
                 (Str (FMatched (2).First .. FMatched (2).Last));
            else
               Name := To_Unbounded_String
                 (+Set_File
                    (Debugger,
                     Str (Matched (6).First .. Matched (6).Last)).Full_Name);
            end if;
         end if;

         return;
      end if;

      Match (Image_Lookup_Regexp, Str, Matched);
      if Matched (0) /= No_Match then
         declare
            List         : String_List_Access;
            File_Match   : Match_Array (0 .. 3);
         begin
            List := Split (Str, ASCII.LF);

            for L of List.all loop
               --  Address
               Match (Image_Lookup_Address_Regexp, L.all, Matched);
               if Matched (0) /= No_Match then
                  Addr := String_To_Address
                    (L (Matched (1).First .. Matched (1).Last));

                  Name := Null_Unbounded_String;
                  Line := 0;
               end if;

               --  Summary
               Match (Image_Lookup_Summary_Regexp, L.all, Matched);
               if Matched (0) /= No_Match then
                  Match
                    (File_Line_Regexp,
                     L (Matched (1).First .. Matched (1).Last),
                     File_Match);

                  Name := To_Unbounded_String
                    (L (File_Match (1).First .. File_Match (1).Last));

                  Line := Integer'Value
                    (L (File_Match (2).First .. File_Match (2).Last));

                  Match
                    (Binder_File_Name_Regexp,
                     To_String (Name),
                     File_Match);

                  if Matched (0) /= No_Match then
                     Name := Null_Unbounded_String;
                     Line := 0;
                  end if;
               end if;

               --  CompileUnit
               if Name /= Null_Unbounded_String then
                  Match (Image_Lookup_CompileUnit_Regexp, L.all, Matched);
                  if Matched (0) /= No_Match then
                     Name := To_Unbounded_String
                       (L (File_Match (1).First .. File_Match (1).Last));

                     Free (List);
                     return;
                  end if;
               end if;
            end loop;

            Free (List);
         end;

         return;
      end if;

   exception
      when E : others =>
         Me.Trace (E, Str);

         Addr := Invalid_Address;
         Name := Null_Unbounded_String;
         Line := 0;
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   overriding procedure Found_Frame_Info
     (Debugger : access LLDB_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger);

      Matched  : Match_Array (0 .. 6);

   begin
      Frame   := Null_Unbounded_String;
      Message := No_Debug_Info;

      Match (Frame_Regexp, Str, Matched);
      if Matched (0) /= No_Match then
         if Matched (5) /= No_Match then
            Message := Location_Found;
         else
            Message := No_Debug_Info;
         end if;

         Frame   := To_Unbounded_String
           (Str (Matched (1).First .. Matched (1).Last));
      end if;

   exception
      when E : others =>
         Me.Trace (E, Str);
   end Found_Frame_Info;

   ---------------
   -- Backtrace --
   ---------------

   overriding procedure Backtrace
     (Debugger : access LLDB_Debugger;
      From     : Integer;
      To       : Integer;
      Value    : out Backtrace_Vector)
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

      Responce : constant String := Debugger.Send_And_Get_Clean_Output
        ("thread backtrace" &
         (if From >= 0
            then " --start" & Integer'Image (From) &
              " --count" & Integer'Image (To - From + 1)
            else ""),
         Internal);
      Matched  : Match_Array (0 .. 6);
      FMatched : Match_Array (0 .. 2);
   begin
      Match (Error_Regexp, Responce, Matched);
      if Matched (0) /= No_Match then
         return;

      else
         declare
            Lines : String_List_Access;
         begin
            Lines := Split (Responce, ASCII.LF);
            for Line of Lines.all loop
               Match (Frame_Regexp, Line.all, Matched);
               if Matched (0) /= No_Match then
                  declare
                     Rec : Backtrace_Record;
                  begin
                     Rec.Selected := Ada.Strings.Fixed.Index
                       (Line (Line'First .. Matched (1).First - 1), "*") >=
                       Line'First;

                     Rec.Frame_Id := Natural'Value
                       (Line (Matched (1).First .. Matched (1).Last));

                     Rec.Address := String_To_Address
                       (Line (Matched (2).First .. Matched (2).Last));

                     if Matched (4) /= No_Match then
                        declare
                           N : constant String :=
                             Line (Matched (4).First .. Matched (4).Last);
                           Idx : constant Natural :=
                             Ada.Strings.Fixed.Index (N, "`");
                        begin
                           if Idx < N'First then
                              Rec.Subprogram := new String'(N);
                           else
                              Rec.Subprogram := new String'
                                (N (Idx + 1 .. N'Last));
                           end if;
                        end;
                     end if;

                     if Matched (6) /= No_Match then
                        Match
                          (File_Line_Regexp,
                           Line (Matched (6).First .. Matched (6).Last),
                           FMatched);

                        if FMatched (0) /= No_Match then
                           Rec.File := Set_File
                             (Debugger,
                              Line (FMatched (1).First .. FMatched (1).Last));

                           Rec.Line := Natural'Value
                             (Line (FMatched (2).First .. FMatched (2).Last));
                        else
                           Rec.File := Set_File
                             (Debugger,
                              Line (Matched (6).First .. Matched (6).Last));
                        end if;
                     end if;

                     Value.Append (Rec);
                  end;
               end if;
            end loop;

            Free (Lines);
         end;
      end if;

   exception
      when E : others =>
         Me.Trace (E, Responce);
   end Backtrace;

   -------------------
   -- Current_Frame --
   -------------------

   overriding function Current_Frame
     (Debugger : access LLDB_Debugger)
      return Integer
   is
      Responce : constant String := Debugger.Send_And_Get_Clean_Output
        ("frame info", Internal);
      Matched  : Match_Array (0 .. 6);
   begin
      Match (Frame_Regexp, Responce, Matched);
      if Matched (0) /= No_Match then
         return Integer'Value
           (Responce (Matched (1).First .. Matched (1).Last));
      else
         return 0;
      end if;
   end Current_Frame;

   -------------------------
   -- Configure_Backtrace --
   -------------------------

   overriding procedure Configure_Backtrace
     (Self                 : not null access LLDB_Debugger;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True) is
   begin
      null;
   end Configure_Backtrace;

   -----------------------------
   -- Internal_Set_Breakpoint --
   -----------------------------

   function Internal_Set_Breakpoint
     (Debugger  : access LLDB_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier
   is
      Responce : constant String := Debugger.Send_And_Get_Clean_Output
        (Cmd => Command, Mode => Mode);
      Matched : Match_Array (0 .. 3);
   begin
      Match (Breakpoint_Regexp, Responce, Matched);
      if Matched (0) /= No_Match then
         return Breakpoint_Identifier'Value
           (Responce (Matched (1).First .. Matched (1).Last));
      else
         return No_Breakpoint;
      end if;
   end Internal_Set_Breakpoint;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   overriding function Break_Subprogram
     (Debugger  : access LLDB_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "breakpoint set " &
         (if Temporary then "--one-shot true " else "") &
           "--name " & Name, Mode => Mode);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   overriding function Break_Source
     (Debugger  : access LLDB_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "breakpoint set " & (if Temporary then "--one-shot true " else "") &
           "--line " & Line'Img & " --file """ &
         (+Full_Name (File)) & """",
         Mode);
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access LLDB_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      if Name = "" or else Name = "all" then
         return Internal_Set_Breakpoint
           (Debugger,
            "breakpoint set --language-exception c" &
            (if Temporary then " --one-shot true" else "") &
            (if Unhandled
               then " --on-throw true"
               else " --on-catch true"),
            Mode);
      else
         return Internal_Set_Breakpoint
           (Debugger,
            "breakpoint set" &
            (if Temporary then " --one-shot true" else "") &
              " --name " & Name,
           Mode);
      end if;
   end Break_Exception;

   ----------------------
   -- Catch_Assertions --
   ----------------------

   overriding function Catch_Assertions
     (Debugger  : access LLDB_Debugger;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      pragma Unreferenced (Debugger, Temporary, Mode);
   begin
      Trace (Me, "Catch_Assertions does not supported by lldb");
      return 0;
   end Catch_Assertions;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Debugger : access LLDB_Debugger) is
   begin
      --  If the debugger process is dead, do not attempt to communicate
      --  with the underlying process.

      if Debugger.Get_Process /= null
        and then Debugger.Get_Process.Get_Descriptor /= null
        and then Debugger.Get_Process.Get_Descriptor.Get_Pid /=
          GNAT.Expect.Invalid_Pid
      then
         --  In case the debugger is busy processing a command.

         if Debugger.Process.Command_In_Process then
            Debugger.Interrupt;
         end if;

         --  Now exit the debugger
         Debugger.Send ("quit", Wait_For_Prompt => False, Mode => Internal);
      end if;

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -------------------
   -- Break_Address --
   -------------------

   overriding function Break_Address
     (Debugger   : access LLDB_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      Result : GVD.Types.Breakpoint_Identifier;
   begin
      Result := Internal_Set_Breakpoint
        (Debugger,
         "breakpoint set " & (if Temporary then "--one-shot true " else "") &
           "--address " & Address_To_String (Address),
         Mode);

      return Result;
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   overriding function Break_Regexp
     (Debugger   : access LLDB_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "breakpoint set --func-regex " & Regexp &
         (if Temporary
            then " --one-shot true"
            else ""),
         Mode => Mode);
   end Break_Regexp;

   ------------------------
   -- Enable_Breakpoints --
   ------------------------

   overriding procedure Enable_Breakpoints
     (Debugger    : access LLDB_Debugger;
      Breakpoints : GVD.Types.Breakpoint_Identifier_Lists.List;
      Enable      : Boolean := True;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Cmd : Unbounded_String :=
              (if Enable then
                  To_Unbounded_String ("breakpoint enable")
               else
                  To_Unbounded_String ("breakpoint disable"));
   begin
      for Breakpoint of Breakpoints loop
         Cmd := Cmd & Breakpoint_Identifier'Image (Breakpoint);
      end loop;

      Debugger.Send (To_String (Cmd), Mode => Mode);
   end Enable_Breakpoints;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   overriding procedure Remove_Breakpoints
     (Debugger    : access LLDB_Debugger;
      Breakpoints : GVD.Types.Breakpoint_Identifier_Lists.List;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Cmd : Unbounded_String := To_Unbounded_String ("breakpoint delete");
   begin
      for Breakpoint of Breakpoints loop
         Cmd := Cmd & Breakpoint_Identifier'Image (Breakpoint);
      end loop;

      Debugger.Send (To_String (Cmd), Mode => Mode);
   end Remove_Breakpoints;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access LLDB_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, File, Line, Mode);
   begin
      Trace (Me, "Remove_Breakpoint_At does not supported by lldb");
   end Remove_Breakpoint_At;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   --  1: file = 'foo.adb', line = 13, exact_match = 0, locations = 1, ->
   --    resolved = 1, hit count = 0 Options: ignore: 2 enabled
   --      Breakpoint commands:
   --        expression T
   --
   --  Condition: T = 2
   --
   --    1.1: where = foo`_ada_foo + 25, address = 0x0000000000402a6b, ->
   --      resolved, hit count = 0

   overriding procedure List_Breakpoints
     (Debugger  : not null access LLDB_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector)
   is

      Block   : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

      Str     : constant String := Debugger.Send_And_Get_Clean_Output
        ("breakpoint list --full", Internal);
      Lines   : String_List_Access;
      Matched : Match_Array (0 .. Breakpoint_Info_Regexp_Last);
      B       : Breakpoint_Data;

      ------------------
      -- Set_Location --
      ------------------

      procedure Set_Location (File, Line : String);
      procedure Set_Location (File, Line : String) is
         F : constant Virtual_File := Set_File (Debugger, File);
      begin
         B.Location := Kernel.Get_Buffer_Factory.Create_Marker
           (File   => F,
            Line   => Editable_Line_Type'Value (Line),
            Column => 1);
      end Set_Location;

      ------------------
      -- Read_Options --
      ------------------

      procedure Read_Options (Item : String);
      procedure Read_Options (Item : String)
      is
         Matched : Match_Array (0 .. 1);
      begin
         Match (Breakpoint_Options_Regexp, Item, Matched);
         if Matched (0) /= No_Match then
            declare
               L   : String_List_Access := Split
                 (Item (Matched (1).First .. Matched (1).Last), ' ');
               Pos : Natural := L'First;
               I   : GNAT.Strings.String_Access;
            begin
               while Pos <= L'Last loop
                  I := L (Pos);

                  if I.all = "disabled" then
                     B.Enabled := False;

                  elsif I.all = "one-shot" then
                     B.Disposition := Delete;

                  elsif I.all = "ignore:" then
                     Pos := Pos + 1;
                     B.Ignore := Natural'Value (L (Pos).all);
                  end if;

                  Pos := Pos + 1;
               end loop;

               Free (L);
            end;
         end if;
      end Read_Options;

      Item : GNAT.Strings.String_Access;
      Pos  : Natural;
   begin
      List.Clear;

      Lines := Split (Str, ASCII.LF);
      Pos   := Lines'First;

      while Pos <= Lines'Last loop
         Item := Lines (Pos);
--              Scope       : Scope_Type := No_Scope;
--              Action      : Action_Type := No_Action;

         Match (Breakpoint_Info_Regexp, Item.all, Matched);
         if Matched (0) /= No_Match then
            if B /= Null_Breakpoint then
               List.Append (B);
               B := Null_Breakpoint;
            end if;

            B.Num         := Breakpoint_Identifier'Value
              (Item (Matched (1).First .. Matched (1).Last));
            B.The_Type    := Breakpoint;
            B.Enabled     := True;
            B.Disposition := Keep;

            if Matched (3) /= No_Match
              and then Matched (4) /= No_Match
            then
               Set_Location
                 (Item (Matched (3).First .. Matched (3).Last),
                  Item (Matched (4).First .. Matched (4).Last));
            end if;

            if Matched (7) /= No_Match then
               B.Subprogram := To_Unbounded_String
                 (Item (Matched (7).First .. Matched (7).Last));
            end if;

            if Matched (Breakpoint_Info_Regexp_Address_Idx) /= No_Match then
               B.Address := String_To_Address
                 (Item (Matched (Breakpoint_Info_Regexp_Address_Idx).First ..
                    Matched (Breakpoint_Info_Regexp_Address_Idx).Last));
            end if;

            Read_Options (Item.all);
         end if;

         Match (Exception_Breakpoint_Regexp, Item.all, Matched);
         if Matched (0) /= No_Match then
            if B /= Null_Breakpoint then
               List.Append (B);
               B := Null_Breakpoint;
            end if;

            B.Num         := Breakpoint_Identifier'Value
              (Item (Matched (1).First .. Matched (1).Last));
            B.The_Type    := Breakpoint;
            B.Enabled     := True;
            B.Disposition := Keep;
            B.Except      := To_Unbounded_String ("all exceptions");

            Read_Options (Item.all);
         end if;

         Match (Breakpoint_Condition_Regexp, Item.all, Matched);
         if Matched (0) /= No_Match then
            B.Condition := Ada.Strings.Unbounded.To_Unbounded_String
              (Item (Matched (1).First .. Matched (1).Last));
         end if;

         Match (Breakpoint_Where_Regexp, Item.all, Matched);
         if Matched (0) /= No_Match then
            if Matched (5) /= No_Match then
               B.Address := String_To_Address
                 (Item (Matched (5).First .. Matched (5).Last));
            end if;

            if Matched (2) /= No_Match then
               declare
                  F : constant String := Item
                    (Matched (2).First .. Matched (2).Last);
                  FMatched : Match_Array (0 .. 3);

               begin
                  Match (Breakpoint_Where_File_Regexp,  F, FMatched);
                  if FMatched (0) /= No_Match then
                     Set_Location
                       (F (FMatched (2).First .. FMatched (2).Last),
                        F (FMatched (3).First .. FMatched (3).Last));

                     B.Subprogram := To_Unbounded_String
                       (F (FMatched (1).First .. FMatched (1).Last));
                  end if;
               end;
            end if;
         end if;

         if Ada.Strings.Fixed.Index
           (Item.all, "Breakpoint commands:") > Item'First
         then
            Pos := Pos + 1;
            while Pos <= Lines'Last
              and then Lines (Pos).all /= ""
            loop
               if B.Commands /= Null_Unbounded_String then
                  Append (B.Commands, "" & ASCII.LF);
               end if;

               Append (B.Commands, Lines (Pos).all);
               Pos := Pos + 1;
            end loop;
         end if;

         Pos := Pos + 1;
      end loop;

      if B /= Null_Breakpoint then
         List.Append (B);
         B := Null_Breakpoint;
      end if;

      Free (Lines);

--  Watchpoint 4: addr = 0x7fffffffd85c size = 4 state = enabled type = rw
--      declare @ '/gvd_testsuite/parse_c.c:44'
--      watchpoint spec = 'A'
--      new value: 1
--      condition = 'A = 3'

      declare
         Str : constant String := Debugger.Send_And_Get_Clean_Output
           ("watchpoint list --full", Internal);
      begin
         Lines := Split (Str, ASCII.LF);
         Pos   := Lines'First;

         while Pos <= Lines'Last loop
            Item := Lines (Pos);
            Match (Watchpoint_Info_Regexp, Item.all, Matched);
            if Matched (0) /= No_Match then
               if B /= Null_Breakpoint then
                  List.Append (B);
                  B := Null_Breakpoint;
               end if;

               B.Num         := Breakpoint_Identifier'Value
                 (Item (Matched (1).First .. Matched (1).Last));
               B.The_Type    := Watchpoint;
               B.Enabled     := Item
                 (Matched (4).First .. Matched (4).Last) = "enabled";

               if Item (Matched (5).First .. Matched (5).Last) = "rw" then
                  B.Trigger := Read_Write;
               elsif Item (Matched (5).First .. Matched (5).Last) = "r" then
                  B.Trigger := Read;
               else
                  B.Trigger := Write;
               end if;

               if Matched (2) /= No_Match then
                  B.Address := String_To_Address
                    (Item (Matched (2).First .. Matched (2).Last));
               end if;
            end if;

            Match (Watchpoint_Expression_Regexp, Item.all, Matched);
            if Matched (0) /= No_Match then
               B.Expression := Ada.Strings.Unbounded.To_Unbounded_String
                 (Item (Matched (1).First .. Matched (1).Last));
            end if;

            Match (Watchpoint_Condition_Regexp, Item.all, Matched);
            if Matched (0) /= No_Match then
               B.Condition := Ada.Strings.Unbounded.To_Unbounded_String
                 (Item (Matched (1).First .. Matched (1).Last));
            end if;

            Match (Watchpoint_Location_Regexp, Item.all, Matched);
            if Matched (0) /= No_Match then
               Set_Location
                 (Item (Matched (1).First .. Matched (1).Last),
                  Item (Matched (2).First .. Matched (2).Last));
            end if;

            Pos := Pos + 1;
         end loop;

         if B /= Null_Breakpoint then
            List.Append (B);
         end if;
         Free (Lines);
      end;
   exception
      when E : others =>
         Me.Trace (E, Str);
         Free (Lines);
   end List_Breakpoints;

   ---------------------
   -- List_Exceptions --
   ---------------------

   overriding function List_Exceptions
     (Debugger : access LLDB_Debugger) return GVD.Types.Exception_Array
   is
      pragma Unreferenced (Debugger);
      Empty : GVD.Types.Exception_Array (1 .. 0);
   begin
      Trace (Me, "List_Exceptions, lldb does not support ada exceptions");
      return Empty;
   end List_Exceptions;

   ----------------------------
   -- Get_Last_Breakpoint_Id --
   ----------------------------

   overriding function Get_Last_Breakpoint_Id
     (Debugger  : access LLDB_Debugger)
      return GVD.Types.Breakpoint_Identifier
   is
      S       : constant String := Debugger.Send_And_Get_Clean_Output
        ("breakpoint list --brief", Mode => Internal);
      Num     : GVD.Types.Breakpoint_Identifier := 0;
      List    : String_List_Access;
      Matched : Match_Array (0 .. 1);
   begin
      List := Split (S, ASCII.LF);
      for L of List.all loop
         Match (Breakpoint_Num_Regexp, L.all, Matched);
         if Matched (1) /= No_Match then
            Num := GVD.Types.Breakpoint_Identifier'Value
              (L (Matched (1).First .. Matched (1).Last)) + 1;
         end if;
      end loop;
      Free (List);

      return Num;
   end Get_Last_Breakpoint_Id;

   ------------------------------
   -- Set_Breakpoint_Condition --
   ------------------------------

   overriding procedure Set_Breakpoint_Condition
     (Debugger  : access LLDB_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Debugger.Send ("breakpoint modify --condition '" & Condition & "'" &
                       Breakpoint_Identifier'Image (Num), Mode => Mode);
   end Set_Breakpoint_Condition;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   overriding procedure Set_Breakpoint_Command
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Commands = "" or else Commands (Commands'Last) = ASCII.LF then
         Debugger.Send
           ("breakpoint command delete " & Breakpoint_Identifier'Image (Num),
            Mode => Mode);
      else
         Debugger.Send
           ("breakpoint command add " & Breakpoint_Identifier'Image (Num)
               & ASCII.LF & Commands & ASCII.LF & "DONE", Mode => Mode);
      end if;
   end Set_Breakpoint_Command;

   ---------------------------------
   -- Set_Breakpoint_Ignore_Count --
   ---------------------------------

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send
        ("breakpoint modify --ignore-count" & Integer'Image (Count) &
           Breakpoint_Identifier'Image (Num), Mode => Mode);
   end Set_Breakpoint_Ignore_Count;

   ----------------------
   -- Set_Scope_Action --
   ----------------------

   overriding procedure Set_Scope_Action
     (Debugger : access LLDB_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Scope, Action, Num, Mode);
   begin
      Trace (Me, "Set_Scope_Action is not supported");
   end Set_Scope_Action;

   -----------
   -- Watch --
   -----------

   overriding function Watch
     (Debugger  : access LLDB_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      Num : GVD.Types.Breakpoint_Identifier;

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
               return "watchpoint set variable --watch read ";
            when GVD.Types.Write =>
               return "watchpoint set variable --watch write ";
            when GVD.Types.Read_Write =>
               return "watchpoint set variable --watch read_write ";
         end case;
      end Command;

   begin
      Num := Internal_Set_Breakpoint
        (Debugger, Command & " " & Name, Mode => Mode);

      if Condition /= "" then
         Debugger.Send
           ("watchpoint modify --condition '" & Condition & "'" &
              GVD.Types.Breakpoint_Identifier'Image (Num), Mode => Mode);
      end if;

      return Num;
   end Watch;

   -----------------
   -- Task_Switch --
   -----------------

   overriding procedure Task_Switch
     (Debugger : access LLDB_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Task_Num, Mode);
   begin
      Trace (Me, "Task_Switch, lldb does not support ada tasks");
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   overriding procedure Thread_Switch
     (Debugger : access LLDB_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "thread select" & Natural'Image (Thread), Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   overriding procedure PD_Switch
     (Debugger : access LLDB_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, PD, Mode);
   begin
      Trace (Me, "PD_Switch, lldb does not support ada language");
   end PD_Switch;

   --------------------
   -- Open_Processes --
   --------------------

   overriding procedure Open_Processes (Debugger : access LLDB_Debugger) is
   begin
      Open_Processes (Debugger.Handle, Debugger.Kernel);
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   overriding procedure Next_Process
     (Debugger : access LLDB_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean) is
   begin
      Next_Process (Debugger.Handle, Info, Success);
   end Next_Process;

   ---------------------
   -- Close_Processes --
   ---------------------

   overriding procedure Close_Processes (Debugger : access LLDB_Debugger)  is
   begin
      Close_Processes (Debugger.Handle);
   end Close_Processes;

   ------------------------
   -- Parse_Disassembled --
   ------------------------

   procedure Parse_Disassembled
     (S    : String;
      Code : out Disassemble_Elements)
   is
      List    : String_List_Access := Split (S, ASCII.LF);
      Matched : Match_Array (0 .. 4);

   begin
      for Line of List.all loop
         Match (Disassemble_Regexp, Line.all, Matched);
         if Matched (0) /= No_Match then
            declare
               El : Disassemble_Element;
            begin
               El.Address := String_To_Address
                 (Line (Matched (2).First .. Matched (2).Last));
               El.Method_Offset := To_Unbounded_String
                 (Line (Matched (3).First .. Matched (3).Last));

               while String_Utils.Is_Blank (Line (Matched (4).Last)) loop
                  Matched (4).Last := Matched (4).Last - 1;
               end loop;

               El.Instr := To_Unbounded_String
                 (Line (Matched (4).First .. Matched (4).Last));

               Code.Append (El);
            end;
         end if;
      end loop;

      Free (List);
   exception
      when E : others =>
         Free (List);
         Me.Trace (E, S);
   end Parse_Disassembled;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger        : access LLDB_Debugger;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type;
      Code            : out Disassemble_Elements;
      Start_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address     : GVD.Types.Address_Type := GVD.Types.Invalid_Address)
   is
      S : constant String := Address_To_String (Start_Address);
      E : constant String := Address_To_String (End_Address);
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

   begin
      Range_Start := Invalid_Address;
      Range_End   := Invalid_Address;

      if S = "" or else E = "" then
         Parse_Disassembled
           (Debugger.Send_And_Get_Clean_Output
              ("disassemble --frame", Mode => Internal),
            Code);
      else
         Parse_Disassembled
           (Debugger.Send_And_Get_Clean_Output
              ("disassemble --start-address " & S & " --end-address " & E,
               Mode => Internal),
            Code);
      end if;

      if not Code.Is_Empty then
         Range_Start := Code.First_Element.Address;
         Range_End   := Code.Last_Element.Address;
      end if;
   end Get_Machine_Code;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger : access LLDB_Debugger;
      File     : String;
      From     : Natural;
      To       : Natural;
      Code     : out Disassemble_Elements)
   is
      Range_Start : GVD.Types.Address_Type;
      Range_End   : GVD.Types.Address_Type;
      Tmp         : GVD.Types.Address_Type;
      F           : constant Virtual_File :=
        Debugger.Kernel.Create_From_Base (+(File));

   begin
      Debugger.Get_Line_Address (From, F, Range_Start, Tmp);
      Debugger.Get_Line_Address (To, F, Tmp, Range_End);

      if Range_Start /= Invalid_Address
        and then Range_End /= Invalid_Address
      then
         Debugger.Get_Machine_Code
           (Range_Start, Range_End, Code, Range_Start, Range_End);
      end if;
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   overriding procedure Get_Line_Address
     (Debugger        : access LLDB_Debugger;
      Line            : Natural;
      File            : GNATCOLL.VFS.Virtual_File;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type)
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("image lookup --verbose -line" & Line'Img &
           " --file " & (+(Full_Name (File))),
         Mode => Internal);
      Matched : Match_Array (0 .. 2);

   begin
      Match (LineEntry_Regexp, S, Matched);

      if Matched (0) /= No_Match then
         Range_Start := String_To_Address
           (S (Matched (1).First .. Matched (1).Last));

         Range_End := String_To_Address
           (S (Matched (2).First .. Matched (2).Last));

      else
         Range_Start := Invalid_Address;
         Range_End   := Invalid_Address;
      end if;
   end Get_Line_Address;

   ------------------------
   -- Get_Register_Names --
   ------------------------

   overriding function Get_Register_Names
     (Debugger : access LLDB_Debugger)
      return GVD.Types.Strings_Vectors.Vector is
   begin
      if not Debugger.Register_Names.Is_Empty then
         return Debugger.Register_Names;
      end if;

      declare
         Block : Process_Proxies.Parse_File_Switch
           (Debugger.Process) with Unreferenced;

         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("register read --all", Mode => Internal);

         List    : String_List_Access := Split (S, ASCII.LF);
         Matched : Match_Array (0 .. 4);
      begin
         for Line of List.all loop
            Match (Register_Regexp, Line.all, Matched);

            if Matched (0) /= No_Match then
               Debugger.Register_Names.Append
                 (Line (Matched (1).First .. Matched (1).Last));
            end if;
         end loop;

         Free (List);
      exception
         when E : others =>
            Free (List);
            Me.Trace (E, S);
      end;

      return Debugger.Register_Names;
   end Get_Register_Names;

   --------------------------
   -- Get_Registers_Values --
   --------------------------

   overriding function Get_Registers_Values
     (Debugger : access LLDB_Debugger;
      Names    : GVD.Types.Strings_Vectors.Vector;
      Format   : GVD.Types.Registers_Format)
      return GVD.Types.Strings_Vectors.Vector
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

      function Convert (F : GVD.Types.Registers_Format) return String;
      function Convert (F : GVD.Types.Registers_Format) return String is
      begin
         case F is
            when Hexadecimal => return "hex";
            when Octal       => return "octal";
            when Binary      => return "binary";
            when Decimal     => return "decimal";
            when Raw         => return "default";
            when Naturals    => return "unsigned";
         end case;
      end Convert;

      Cmd : Ada.Strings.Unbounded.Unbounded_String :=
        To_Unbounded_String ("register read --format " & Convert (Format));
      S   : Ada.Strings.Unbounded.Unbounded_String;

      List    : String_List_Access;
      Matched : Match_Array (0 .. 4);
      Result  : GVD.Types.Strings_Vectors.Vector;
   begin
      if Names.Is_Empty then
         Append (Cmd, " --all");
      else
         for I of Names loop
            Append (Cmd, " " & I);
         end loop;
      end if;

      S := To_Unbounded_String
        (Debugger.Send_And_Get_Clean_Output
           (To_String (Cmd), Mode => Internal));

      List := Split (To_String (S), ASCII.LF);
      for Line of List.all loop
         Match (Register_Regexp, Line.all, Matched);

         if Matched (0) /= No_Match then
            Result.Append
              (Line (Matched (2).First .. Matched (2).Last - 1));
         end if;
      end loop;

      Free (List);
      return Result;
   exception
      when E : others =>
         Free (List);
         Me.Trace (E, To_String (S));
         return Result;
   end Get_Registers_Values;

   --------------
   -- Set_File --
   --------------

   function Set_File
     (Debugger : access LLDB_Debugger;
      File     : String)
      return Virtual_File
   is
      Result : Virtual_File;
   begin
      if Ada.Strings.Fixed.Index
        (File, "" & GNAT.OS_Lib.Path_Separator) < File'First
      then
         Result := To_File (Debugger.Get_Kernel, File);
      else
         Result := Debugger.Get_Kernel.Create_From_Base (+File);
      end if;

      if not Result.Is_Absolute_Path
        or else not Result.Is_Regular_File
      then
         Result := Debugger.Kernel.Create_From_Base (Result.Full_Name);
      end if;

      return Result;
   end Set_File;

   ------------------
   -- Set_Register --
   ------------------

   overriding procedure Set_Register
     (Debugger : access LLDB_Debugger;
      Name     : String;
      Value    : String)
   is
   begin
      Debugger.Send ("register write " & Name & " " & Value);
   end Set_Register;

   ----------------
   -- Get_Memory --
   ----------------

   overriding function Get_Memory
     (Debugger : access LLDB_Debugger;
      Size     : Integer;
      Address  : String)
      return Memory_Dump_Access
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
        (Debugger.Send_And_Get_Clean_Output
           ("x/" & Image (Image'First + 1 .. Image'Last)
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
     (Debugger : access LLDB_Debugger;
      Address  : String;
      Byte     : String) is
   begin
      Debugger.Send
        ("memory write " & Address & " 0x" & Byte, Mode => Internal);
   end Put_Memory_Byte;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   overriding function Get_Uniq_Id
     (Debugger : access LLDB_Debugger; Entity : String) return String
   is
      S       : constant String := Debugger.Send_And_Get_Clean_Output
        ("expression &(" & Entity & ")", Mode => Internal);
      Matched : Match_Array (0 .. 1);

   begin
      Match (Addr_Of_Variable_Regexp, S, Matched);

      if Matched (1) /= No_Match then
         return S (Matched (1).First .. Matched (1).Last);
      end if;

      return "";
   end Get_Uniq_Id;

   -------------------
   -- Get_Type_Info --
   -------------------

   overriding function Get_Type_Info
     (Debugger  : access LLDB_Debugger;
      Entity    : String;
      Default   : String) return String
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("frame variable " & Entity,
         Mode => Internal);
      Matched : Match_Array (0 .. 1);
   begin
      Match (Type_Name_Regexp, S, Matched);
      if Matched (1) /= No_Match then
         return S (Matched (1).First .. Matched (1).Last);
      else
         return Default;
      end if;
   end Get_Type_Info;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   overriding function Get_Variable_Address
     (Debugger  : access LLDB_Debugger;
      Variable  : String)
      return String
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("expression --raw -- &" & Variable,
         Mode => Internal);
      Matched : Match_Array (0 .. 1);
   begin
      Match (Value_Of_Regexp, S, Matched);
      if Matched (1) /= No_Match then
         return S (Matched (1).First .. Matched (1).Last);
      else
         return "";
      end if;
   end Get_Variable_Address;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   overriding function Get_Endian_Type
     (Debugger : access LLDB_Debugger)
      return Endian_Type
   is
      pragma Unreferenced (Debugger);
   begin
      return Little_Endian;
   end Get_Endian_Type;

   --------------
   -- Complete --
   --------------

   overriding function Complete
     (Debugger  : access LLDB_Debugger;
      Beginning : String)
      return GNAT.Strings.String_List
   is
      S           : constant String :=
        Debugger.Send_And_Get_Clean_Output
          (Beginning & ASCII.HT, Mode => Internal);
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

   ---------------------------------
   -- Connect_To_Target_If_Needed --
   ---------------------------------

   procedure Connect_To_Target_If_Needed (Debugger : access LLDB_Debugger) is
   begin
      if Debugger.Remote_Target /= null
        and then Debugger.Remote_Protocol /= null
        and then not Debugger.Target_Connected
      then
         Debugger.Connect_To_Target
           (Target   => Debugger.Get_Remote_Target,
            Protocol => Debugger.Get_Remote_Protocol,
            Mode     => Visible);
      else
         Debugger.Display_Prompt;
      end if;
   end Connect_To_Target_If_Needed;

   -------------
   -- Set_TTY --
   -------------

   overriding procedure Set_TTY
     (Debugger : access LLDB_Debugger;
      TTY      : String)
   is
      pragma Unreferenced (Debugger, TTY);
   begin
      raise Unknown_Command;
   end Set_TTY;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   overriding function Is_Quit_Command
     (Debugger : access LLDB_Debugger;
      Command : String)
      return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Starts_With (Command, "quit")
        or else Starts_With (Command, "q");
   end Is_Quit_Command;

   --------------------------
   -- Internal_Parse_Value --
   --------------------------

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Entity     : String;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive;
      Parent     : GVD.Variables.Types.GVD_Type_Holder)
   is
      procedure Skip_Parenthesis (Index : in out Natural);
      --  Skip the parenthesis pair starting at Index, taking into account
      --  nested parenthesis

      ----------------------
      -- Skip_Parenthesis --
      ----------------------

      procedure Skip_Parenthesis (Index : in out Natural) is
         Num : Natural := 1;
      begin
         if Index <= Type_Str'Last and then Type_Str (Index) = '(' then
            Index := Index + 1;
            while Num /= 0
              and then Index <= Type_Str'Last
            loop
               if Type_Str (Index) = ')' then
                  Num := Num - 1;
               elsif Type_Str (Index) = '(' then
                  Num := Num + 1;
               end if;
               Index := Index + 1;
            end loop;
            Index := Index + 1;
         end if;
      end Skip_Parenthesis;

      Context : constant Language_Debugger_Context :=
                  Get_Language_Debugger_Context (Lang);
      Dim     : Dimension;

   begin
      Repeat_Num := 1;

      if Looking_At (Type_Str, Index, "Cannot access memory at address") then
         while Index <= Type_Str'Last loop
            exit when Type_Str (Index) = ','
              or else Type_Str (Index) = ')'
              or else Type_Str (Index) = '}'
              or else Type_Str (Index) = '>';
            Index := Index + 1;
         end loop;

         if Result.Get_Type'Tag = GVD_Simple_Type'Tag
           or else Result.Get_Type'Tag = GVD_Range_Type'Tag
           or else Result.Get_Type'Tag = GVD_Mod_Type'Tag
           or else Result.Get_Type'Tag = GVD_Enum_Type'Tag
           or else Result.Get_Type.all in GVD_String_Type'Class
         then
            GVD_Simple_Type_Access (Result.Get_Type).Set_Value ("<???>");

         elsif Result.Get_Type'Tag = GVD_Access_Type'Tag then
            GVD_Simple_Type_Access (Result.Get_Type).Set_Value ("0x0");
         end if;

         return;
      end if;

      -------------------
      -- Simple values --
      -------------------

      if Result.Get_Type'Tag = GVD_Simple_Type'Tag
        or else Result.Get_Type'Tag = GVD_Range_Type'Tag
        or else Result.Get_Type'Tag = GVD_Mod_Type'Tag
        or else Result.Get_Type'Tag = GVD_Enum_Type'Tag
        or else Result.Get_Type.all in GVD_String_Type'Class
      then
         if Type_Str /= "" then
            Skip_Parenthesis (Index);
            declare
               Int : constant Natural := Index;
            begin
               Skip_Simple_Value (Type_Str, Index,
                                  Array_Item_Separator => ',',
                               End_Of_Array         => Context.Array_End,
                                  Repeat_Item_Start    => '<');
               GVD_Simple_Type_Access (Result.Get_Type).Set_Value
                 (Type_Str (Int .. Index - 1));
            end;
         else
            GVD_Simple_Type_Access (Result.Get_Type).Set_Value ("<???>");
         end if;

      -------------------
      -- Access values --
      -------------------
      --  The value looks like:   (access integer) 0xbffff54c
      --  or                  :    0x0
      --  or                  :   (<ref> TstringS29b) @0xbfffdba0
      --  or                  :   (void (*)()) 0x804845c <foo>

      elsif Result.Get_Type'Tag = GVD_Access_Type'Tag then

         if Looking_At (Type_Str, Index, "(null)") then
            GVD_Simple_Type_Access (Result.Get_Type).Set_Value ("0x0");
            Index := Index + 6;

         else
            Skip_Parenthesis (Index);

            --  Access to subprograms are sometimes printed as:
            --     {void ()} 0x402488e4 <gtk_window_destroy>
            if Index <= Type_Str'Last and then Type_Str (Index) = '{' then
               Skip_To_Char (Type_Str, Index, '}');
               Index := Index + 2;
            end if;

            if Index <= Type_Str'Last and then Type_Str (Index) = '@' then
               Index := Index + 1;
            end if;

            declare
               Int : constant Natural := Index;
            begin
               Skip_Hexa_Digit (Type_Str, Index);

               --  If we have an extra indication like
               --      <gtk_window_finalize>
               --  in the value, keep it.

               if Index < Type_Str'Last - 2
                 and then Type_Str (Index + 1) = '<'
                 and then not Looking_At (Type_Str, Index + 2, "repeats ")
               then
                  Skip_To_Char (Type_Str, Index, '>');
                  Index := Index + 1;

                  --  Also keep string indications (for char* in C)
               elsif Index < Type_Str'Last - 2
                 and then (Type_Str (Index + 1) = '"'
                           or else Type_Str (Index + 1) = ''')
               then
                  declare
                     Str      : String (1 .. 0);
                     Str_Last : Natural;
                  begin
                     Index := Index + 1;
                     Parse_Cst_String
                       (Type_Str, Index, Str, Str_Last,
                        Backslash_Special => Get_Language_Context
                          (Lang).Quote_Character = '\');
                     Index := Index - 1;
                  end;
               end if;

               GVD_Simple_Type_Access (Result.Get_Type).Set_Value
                 (Type_Str (Int .. Index - 1));
            end;
         end if;

      -------------------
      -- String values --
      -------------------

      elsif Result.Get_Type'Tag = GVD_Array_Type'Tag
        and then GVD_Array_Type_Access (Result.Get_Type).Num_Dimensions = 1
        and then Type_Str'Length /= 0
        and then
          (Type_Str (Index) = '"'
           or else Type_Str (Index) = ''')
      then
         Dim := GVD_Array_Type_Access (Result.Get_Type).Get_Dimensions (1);

         --  If the dimension was not known when parsing the type, we compute
         --  it directly from the value of the string

         if Dim.Last < Dim.First then
            declare
               Tmp : Natural := Index;
               S   : String (1 .. 0);
               S_Last : Natural;
            begin
               Parse_Cst_String (Type_Str, Tmp, S, S_Last);
               Dim.Last := Long_Integer (Tmp - Index) + Dim.First - 4;
            end;
         end if;

         declare
            S      : String (1 .. Integer (Dim.Last - Dim.First + 1));
            S_Last : Natural;
            Simple : GVD_Type_Holder;

         begin
            Parse_Cst_String
              (Type_Str, Index, S, S_Last,
               Backslash_Special => Get_Language_Context
               (Lang).Quote_Character = '\');
            Simple := GVD_Array_Type_Access
              (Result.Get_Type).Get_Value (Dim.First);

            if Simple = Empty_GVD_Type_Holder then
               Simple := New_Simple_Type;
            end if;

            GVD_Simple_Type_Access (Simple.Get_Type).Set_Value
              (S (S'First .. S_Last));

            --  The index should always be 0, since we add Dim.First before
            --  displaying it.

            GVD_Array_Type_Access (Result.Get_Type).Set_Value
              (Elem_Value => Simple,
               Elem_Index => 0);
            GVD_Array_Type_Access (Result.Get_Type).Shrink_Values;
         end;

      ------------------
      -- Array values --
      ------------------

      elsif Result.Get_Type'Tag = GVD_Array_Type'Tag
        and then Type_Str'Length /= 0   --  for empty Arrays
        and then Type_Str /= "[0]"
      then
         --  Some array types can in fact be transformed into access types.
         --  This is the case for instance in C for empty arrays ("int[0]" can
         --  have a value of "0x..."), or in Ada for unconstrained arrays
         --  ("array (1..1) of string" can have a value of "(0x0").
         --  For such cases, we change the type once and for all, since we will
         --  never need to go back to an array type.
         --  See also "(<ref> TstringS29b) @0xbfffdba0: Index bound unknown.",
         --  which starts with the right character but is in fact an array
         --  type.
         --  There is also
         --  "(<ref> array (...) of string) @0xbffff5fc: ((null), (null))"
         --  where the value of the array is indeed visible, in which case we
         --  try and keep the array as long as possible

         if Index + 11 < Type_Str'Last
           and then Type_Str (Index + 1 .. Index + 11) = "<ref> array"
         then
            declare
               Num_Open : Integer := 0;
            begin
               Index := Index + 12;

               while Num_Open /= -1 loop
                  if Type_Str (Index) = '(' then
                     Num_Open := Num_Open + 1;
                  elsif Type_Str (Index) = ')' then
                     Num_Open := Num_Open - 1;
                  end if;

                  Index := Index + 1;
               end loop;
            end;

            Skip_To_Char (Type_Str, Index, ':');
            Index := Index + 2;
            Internal_Parse_Value
              (Lang, Entity, Type_Str, Index, Result, Repeat_Num, Parent);

         elsif Type_Str (Index) /= Context.Array_Start
           or else (Index + 5 <= Type_Str'Last
                    and then Type_Str (Index + 1 .. Index + 5) = "<ref>")
         then
            --  If we have "(<ref> array (...) of string) @0xbffff5fc: ((null),
            --  (null))", this is still considered as an array, which is
            --  friendlier for the user in the canvas.

            declare
               Tmp : Natural := Index;
            begin
               Skip_To_Char (Type_Str, Tmp, ')');
               Skip_To_Char (Type_Str, Tmp, ':');

               if Tmp < Type_Str'Last
                 and then Type_Str (Tmp .. Tmp + 1) = " ("
               then
                  Index := Tmp;
                  Parse_Array_Value (Lang, Type_Str, Index, Result);
                  return;
               end if;
            end;

            --  Otherwise, we convert to an access type

            if Parent /= Empty_GVD_Type_Holder then
               Result := GVD_Type_Holder
                 (Parent.Get_Type.Replace (Result, New_Access_Type));
            else
               Result := New_Access_Type;
            end if;

            Internal_Parse_Value
              (Lang, Entity, Type_Str, Index, Result, Repeat_Num,
               Parent => Parent);

         else
            Parse_Array_Value (Lang, Type_Str, Index, Result);
         end if;

      -------------------
      -- Record values --
      -------------------

      elsif Result.Get_Type'Tag = GVD_Record_Type'Tag
        or else Result.Get_Type'Tag = GVD_Union_Type'Tag
      then
         declare
            Int : Natural;
            Close_Parentheses : Boolean := False;
         begin
         --  Skip initial '(' if we are still looking at it (we might not
            --  if we are parsing a variant part)

            if Index <= Type_Str'Last
              and then Type_Str (Index) = Context.Record_Start
            then
               Index := Index + 1;
               Close_Parentheses := True;
            end if;

            for J in 1 .. GVD_Record_Type_Access
              (Result.Get_Type).Num_Fields
            loop

               exit when Index >= Type_Str'Last;

               --  If we are expecting a field

               if GVD_Record_Type_Access
                 (Result.Get_Type).Get_Variant_Parts (J) = 0
               then
                  declare
                     V          : GVD_Type_Holder := GVD_Record_Type_Access
                       (Result.Get_Type).Get_Value (J);
                     Repeat_Num : Positive;
                  begin
                     --  Skips '=>'
                     --  This also skips the address part in some "in out"
                     --  parameters, like:
                     --    (<ref> gnat.expect.process_descriptor) @0x818a990: (
                     --     pid => 2012, ...

                     Skip_To_String (Type_Str, Index, Context.Record_Field);
                     Index := Index + 1 + Context.Record_Field_Length;
                     Internal_Parse_Value
                       (Lang, Entity, Type_Str, Index, V, Repeat_Num,
                        Parent => Result);
                  end;

               --  Else we have a variant part record

               else
                  if Type_Str (Index) = ',' then
                     Index := Index + 1;
                     Skip_Blanks (Type_Str, Index);
                  end if;

                  --  Find which part is active
                  --  We simply get the next field name and search for the
                  --  part that defines it. Note that in case with have a
                  --  'null' part, we have to stop at the closing parens.

                  Int := Index;
                  while Int <= Type_Str'Last
                    and then Type_Str (Int) /= ' '
                    and then Type_Str (Int) /= ')'
                  loop
                     Int := Int + 1;
                  end loop;

                  --  Reset the valid flag, so that only one of the variant
                  --  parts is valid.

                  declare
                     Repeat_Num : Positive;
                     V          : GVD_Type_Holder;
                  begin
                     V := GVD_Record_Type_Access
                       (Result.Get_Type).Find_Variant_Part
                       (Field    => J,
                        Contains => Type_Str (Index .. Int - 1));

                     --  Variant part not found. This happens for instance when
                     --  gdb doesn't report the "when others" part of a variant
                     --  record in the type if it has a no field, as in
                     --       type Essai (Discr : Integer := 1) is record
                     --         case Discr is
                     --             when 1 => Field1 : Integer;
                     --             when others => null;
                     --         end case;
                     --       end record;
                     --  ptype reports
                     --    type = record
                     --       discr : integer;
                     --       case discr is
                     --           when 1 => field1 : integer;
                     --       end case;
                     --    end record;

                     if V /= Empty_GVD_Type_Holder then
                        Internal_Parse_Value
                          (Lang, Entity, Type_Str, Index, V, Repeat_Num,
                           Parent => Result);
                     end if;
                  end;
               end if;
            end loop;

            Skip_Blanks (Type_Str, Index);

            --  Skip closing ')', if seen
            if Close_Parentheses and then Index <= Type_Str'Last
              and then Type_Str (Index) = Context.Record_End
            then
               Index := Index + 1;
            end if;
         end;

      ------------------
      -- Class values --
      ------------------

      elsif Result.Get_Type'Tag = GVD_Class_Type'Tag then
         declare
            R : GVD_Type_Holder;
            Close_Parentheses : Boolean := False;
         begin
         --  Skip initial '(' if we are still looking at it (we might not
            --  if we are parsing a variant part)

            if Index <= Type_Str'Last
              and then Type_Str (Index) = Context.Record_Start
            then
               Index := Index + 1;
               Close_Parentheses := True;
            end if;

            for A in 1 .. GVD_Class_Type_Access
              (Result.Get_Type).Get_Num_Ancestors
            loop
               R := GVD_Class_Type_Access (Result.Get_Type).Get_Ancestor (A);
               Internal_Parse_Value
                 (Lang, Entity, Type_Str, Index, R, Repeat_Num,
                  Parent => Result);
            end loop;
            R := GVD_Class_Type_Access (Result.Get_Type).Get_Child;

            if GVD_Record_Type_Access (R.Get_Type).Num_Fields /= 0 then
               Internal_Parse_Value
                 (Lang, Entity, Type_Str, Index, R, Repeat_Num,
                  Parent => Result);
            end if;

            Skip_Blanks (Type_Str, Index);

            --  Skip closing ')', if seen
            if Close_Parentheses and then Index <= Type_Str'Last
              and then Type_Str (Index) = Context.Record_End
            then
               Index := Index + 1;
            end if;
         end;
      end if;

      -------------------
      -- Repeat values --
      -------------------
      --  This only happens inside arrays, so we can simply replace
      --  Result

      Skip_Blanks (Type_Str, Index);
      if Looking_At (Type_Str, Index, "<repeats ") then
         Index := Index + 9;
         Parse_Num (Type_Str,
                    Index,
                    Long_Integer (Repeat_Num));
         Index := Index + 7;  --  skips " times>"
      end if;
   end Internal_Parse_Value;

end Debugger.LLDB;

-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2000-2008, AdaCore                   --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Tags;                use Ada.Tags;
with Ada.Unchecked_Deallocation;

with GNAT.Expect;             use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;         use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Gtk.Window;              use Gtk.Window;

with Config;                  use Config;
with Debugger.Gdb.Ada;        use Debugger.Gdb.Ada;
with Debugger.Gdb.C;          use Debugger.Gdb.C;
with Debugger.Gdb.Cpp;        use Debugger.Gdb.Cpp;
with Default_Preferences;     use Default_Preferences;
with File_Utils;              use File_Utils;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GPS.Intl;                use GPS.Intl;
with GPS.Main_Window;         use GPS.Main_Window;
with GVD.Dialogs;             use GVD.Dialogs;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Scripts;             use GVD.Scripts;
with GVD.Trace;               use GVD.Trace;
with GVD.Types;               use GVD.Types;
with Items.Arrays;            use Items.Arrays;
with Items.Classes;           use Items.Classes;
with Items.Records;           use Items.Records;
with Items.Simples;           use Items.Simples;
with Items;                   use Items;
with Language.Debugger;       use Language.Debugger;
with Language;                use Language;
with Process_Proxies;         use Process_Proxies;
with Remote.Path.Translator;  use Remote, Remote.Path.Translator;
with String_Utils;            use String_Utils;
with VFS;                     use VFS;

package body Debugger.Gdb is

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp             : constant Pattern_Matcher :=
     Compile ("^>*\(gdb\) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_Length             : constant := 6;
   --  Length of the prompt ("(gdb) ").

   Gdb_Command               : constant String := "gdb";
   --  Name of the command to launch gdb.

   Gdb_Options               : constant String := "-nw -q";
   --  Options always passed to gdb.

   Highlight_Pattern         : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window.

   File_Name_Pattern         : constant Pattern_Matcher :=
     Compile (ASCII.SUB & ASCII.SUB
              & "(.+):(\d+):\d+:[^:]+:(0x[0-9a-f]+)$", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output.

   File_Name_Pattern2        : constant Pattern_Matcher :=
     Compile ("^([^:\n]+):(\d+): No such file or directory.", Multiple_Lines);
   --  Second regexp used to detect when the current frame can not be displayed
   --  Note that this pattern should work even when LANG isn't english because
   --  gdb does not seem to take into account this variable at all.

   Language_Pattern          : constant Pattern_Matcher := Compile
     ("^(The current source language is|Current language:) +" &
      """?(auto; currently )?([^""\s]+)("".)?\n", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger.

   Terminate_Pattern         : constant Pattern_Matcher := Compile
     ("^Program exited (normally|with code)", Multiple_Lines);
   --  Pattern used to detect when the debuggee terminates.

   Running_Pattern           : constant Pattern_Matcher := Compile
     ("^The program is not being run.", Multiple_Lines);
   --  Pattern used to detect when the debuggee is not running.

   Frame_Pattern             : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))?$", Multiple_Lines);
   --  Regular expression used to detect and parse callstack frames

   Frame_Pattern_With_File   : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))$", Multiple_Lines);
   --  Regular expression used to detect and parse callstack frames
   --  with no file information

   Breakpoint_Pattern        : constant Pattern_Matcher := Compile
     ("^(\d+)\s+(breakpoint|\w+? watchpoint)\s+(keep|dis|del)\s+([yn])"
      & "\s+((0x0*)?(\S+))?\s+(.*)$",
      Multiple_Lines);
   --  Pattern to match a single line in "info breakpoint"

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

   Question_Filter_Pattern1  : constant Pattern_Matcher :=
     Compile ("^\[0\] .*> ", Multiple_Lines + Single_Line);

   Question_Filter_Pattern2  : constant Pattern_Matcher :=
     Compile ("^(.*\?) \(y or n\) ", Multiple_Lines);
   --  How to detect a question in gdb's output

   Continuation_Line_Pattern : constant Pattern_Matcher := Compile ("^>");

   Address_Range_Pattern     : constant Pattern_Matcher := Compile
     ("starts at address (0x[0-9a-f]+) <[^>]+> and ends at (0x[0-9a-f]+)");
   --  How to get the range of addresses for a given line

   GNAT_Binder_File_Pattern  : constant Pattern_Matcher := Compile
     ("(b~.+\.adb)|(b_.+\.c)");

   No_Definition_Of          : constant String := "No definition of";
   --  String used to detect undefined commands.

   Undefined_Command         : constant String := "Undefined command";
   --  Another string used to detect undefined commands.

   Undefined_Info_Command    : constant String := "Undefined info command";
   --  Another string used to detect undefined info commands.

   List_Lines                : constant String := "^done,lines=[";
   --  Used to parse output of -symbol-list-lines command

   procedure Language_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect a change in the current language.

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the program no longer runs.

   procedure Question_Filter1
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect questions from gdb.

   procedure Question_Filter2
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect y/n questions from gdb.

   procedure Continuation_Line_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect commands handled on multiple lines.

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
   --  Set the debuggee arguments to Arguments.

   procedure Switch_Language
     (Debugger : access Gdb_Debugger;
      Language : in String);
   --  Switch gdb to another language. The possible values for Language are:
   --  "ada", "c", "c++", "asm", "chill", "fortran", "java", "modula-2",
   --  "scheme".
   --  When calling this function, the current language is stored internally
   --  and can be restored by calling Restore_Language.

   procedure Restore_Language
     (Debugger : access Gdb_Debugger);
   --  Restore the language that was active before Switch_Language was called.

   function Get_Module (Executable : VFS.Virtual_File) return String;
   --  Return the name of the module contained in Executable
   --  Assume that the name of the module is the executable file
   --  with no path information and no extension.

   procedure Detect_Debugger_Mode (Debugger : access Gdb_Debugger);
   --  This detects the debugger mode depending on the remote protocol being
   --  used. Ideally, it should be called every time we change the remote
   --  protocol.

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_Debugger);
   --  Connect to the target if not already connected.

   --------------------------
   -- Detect_Debugger_Mode --
   --------------------------

   procedure Detect_Debugger_Mode (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Remote_Protocol = null
        or else Debugger.Remote_Protocol.all = ""
      then
         Debugger.Mode := Native;

      elsif To_Lower (Debugger.Remote_Protocol.all) = "wtx"
        or else To_Lower (Debugger.Remote_Protocol.all) = "dfw"
        or else To_Lower (Debugger.Remote_Protocol.all) = "dfw-rtp"
        or else To_Lower (Debugger.Remote_Protocol.all) = "vxworks"
      then
         Debugger.Mode := VxWorks;

      else
         Debugger.Mode := Cross;
      end if;
   end Detect_Debugger_Mode;

   ---------------------
   -- Language_Filter --
   ---------------------

   procedure Language_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Debugger   : constant Debugger_Access := Process.Debugger;
      Language   : Language_Access;

   begin
      declare
         Lang : constant String := Str (Matched (3).First .. Matched (3).Last);
      begin
         if Lang = "ada" then
            Language := new Gdb_Ada_Language;
         elsif Lang = "c" then
            Language := new Gdb_C_Language;
         elsif Lang = "c++" then
            Language := new Gdb_Cpp_Language;
         elsif Lang = "auto" then
            --  Do not change the current language if gdb isn't able to
            --  tell what the new language is
            return;

         else
            Output_Error
              (GPS_Window (Process.Window).Kernel,
               (-"Language unknown, defaulting to C: ") & Lang);
            Language := new Gdb_C_Language;
         end if;

         Set_Language (Debugger, Language);
         Set_Debugger
           (Language_Debugger_Access (Language), Debugger.all'Access);
      end;
   end Language_Filter;

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

   ---------------------
   -- Question_Filter --
   ---------------------

   procedure Question_Filter1
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Dialog   : Question_Dialog_Access;
      Index    : Natural;
      Debugger : constant Debugger_Access := Process.Debugger;
      Choices  : Question_Array (1 .. 1000);
      --  ??? This is an arbitrary hard-coded limit, that should
      --  be enough. Might be nice to remove it though.

      Num   : Natural := 0;
      First : Positive;
      Last  : Positive := Matched (0).First;

   begin
      --  Always call the hook, even in invisible mode. This is in particular
      --  useful for the automatic testsuite

      declare
         Result : constant String := Run_Debugger_Hook_Until_Not_Empty
           (Process, Debugger_Question_Action_Hook, Str);
      begin
         if Result /= "" then
            Send (Debugger, Result,
                  Mode            => Internal,
                  Empty_Buffer    => False,
                  Wait_For_Prompt => False);
            return;
         end if;
      end;

      --  ??? An issue occurs if the hook returned True, but in fact no reply
      --  was sent to the debugger. In this case, the debugger stays blocked
      --  waiting for input. Since this is visible in the console, that means
      --  the user will have to type (though it still fails in automatic tests,
      --  since commands are sent in invisible mode)

      --  If we are processing an internal command, we cancel any question
      --  dialog we might have, and silently fail
      --  ??? For some reason, we can not use Interrupt here, and we have
      --  to rely on the fact that "Cancel" is the first choice.

      if Get_Command_Mode (Get_Process (Debugger)) = Internal then
         Send (Debugger, "0",
               Mode            => Internal,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);
         return;

      --  For a hidden command, we also cannot afford to wait, so send an
      --  answer. 1 will typically map to "all".

      elsif Get_Command_Mode (Get_Process (Debugger)) = Hidden then
         Send (Debugger, "1",
               Mode            => Hidden,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);
         return;
      end if;

      --  Index is positioned to the last LF character: "[0] ...\n> "

      Index := Matched (0).Last - 2;

      while Last < Index loop
         --  Skips the choice number ("[n] ")
         Last := Last + 4;
         First := Last;

         while Last < Index and then Str (Last) /= ASCII.LF loop
            Last := Last + 1;
         end loop;

         Num := Num + 1;
         Choices (Num).Choice :=
           new String'(Natural'Image (Num - 1));
         Choices (Num).Description :=
           new String'(Str (First .. Last - 1));

         Last := Last + 1;
      end loop;

      Gtk_New
        (Dialog,
         Gtk_Window (Process.Window),
         Debugger,
         True,
         Choices (1 .. Num));
      Show_All (Dialog);

      for J in 1 .. Num loop
         Free (Choices (Num).Choice);
         Free (Choices (Num).Description);
      end loop;
   end Question_Filter1;

   ----------------------
   -- Question_Filter2 --
   ----------------------

   procedure Question_Filter2
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Dialog   : Question_Dialog_Access;
      Debugger : constant Debugger_Access := Process.Debugger;
      Choices  : Question_Array (1 .. 2);
      Mode     : Command_Type;

   begin
      --  Always call the hook, even in invisible mode. This is in particular
      --  useful for the automatic testsuite

      declare
         Output : constant String := Run_Debugger_Hook_Until_Not_Empty
           (Process, Debugger_Question_Action_Hook, Str);
      begin
         if Output /= "" then
            Send (Debugger, Output,
                  Mode            => Internal,
                  Empty_Buffer    => False,
                  Wait_For_Prompt => False);
            return;
         end if;
      end;

      Mode := Get_Command_Mode (Get_Process (Debugger));

      --  For an invisible command, we cannot afford to wait, so send an
      --  answer automatically.

      if Mode in Invisible_Command then
         Send (Debugger, "y",
               Mode            => Mode,
               Empty_Buffer    => False,
               Wait_For_Prompt => False);
         return;
      end if;

      --  Should we display the dialog or not ?

      Choices (1).Choice := new String'("n");
      Choices (1).Description := new String'("No");

      Choices (2).Choice := new String'("y");
      Choices (2).Description := new String'("Yes");

      Gtk_New
        (Dialog,
         Gtk_Window (Process.Window),
         Debugger,
         False,
         Choices,
         Str (Matched (0).First .. Matched (0).Last));
      Show_All (Dialog);

      for J in Choices'Range loop
         Free (Choices (J).Choice);
         Free (Choices (J).Description);
      end loop;
   end Question_Filter2;

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

   ----------
   -- Send --
   ----------

   function Send
     (Debugger : access Gdb_Debugger;
      Cmd      : String;
      Mode     : Invisible_Command := Hidden) return String
   is
      S   : constant String := Send_Full (Debugger, Cmd, Mode);
      Pos : Integer := S'Last - Prompt_Length;
   begin
      if S'Length <= Prompt_Length then
         return "";
      end if;

      if S (Pos) = ASCII.LF then
         Pos := Pos - 1;
      end if;

      return S (S'First .. Pos);
   end Send;

   procedure Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      J, K : Integer;
   begin
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
         Empty_Buffer, Wait_For_Prompt, Mode);
   end Send;

   -------------
   -- Type_Of --
   -------------

   function Type_Of
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
         S   : constant String :=
                 Send (Debugger, "ptype " & Entity, Mode => Internal);
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

   function Info_Locals (Debugger : access Gdb_Debugger) return String is
      pragma Unreferenced (Debugger);
   begin
      return "info locals";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   function Info_Args (Debugger : access Gdb_Debugger) return String is
      pragma Unreferenced (Debugger);
   begin
      return "info args";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   function Info_Registers (Debugger : access Gdb_Debugger) return String is
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
   --  Array used by Value_Of to print values in various formats.

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String
   is
      S : constant String :=
        Send (Debugger, "print" & Fmt_Array (Format) & ' ' & Entity,
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

   function Print_Value_Cmd
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

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String
   is
      --  ??? Probably, this should be language-dependent.
      --  In particular, in C, &(*A) returns A, not an address, which causes
      --  unexpected wrong aliases to be detected.

      S       : constant String :=
                  Send (Debugger, "print &(" & Entity & ")", Mode => Internal);
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

   procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : VFS.Virtual_File := VFS.No_File;
      Debugger_Args   : GNAT.OS_Lib.Argument_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Gdb_Arguments   : Argument_List_Access :=
                          Argument_String_To_List (Gdb_Options);
      Num_Options     : constant Natural := Gdb_Arguments'Length;
      Local_Arguments : Argument_List
                          (1 .. Debugger_Args'Length + Num_Options);

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);

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
      Debugger.Window := Window;

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

      --  Perform an additional guess of the mode of gdb from the name of the
      --  executable launched.
      if Contains (Debugger_Name, "vxworks") then
         Debugger.Mode := VxWorks;
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      if Window /= null then
         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Language_Filter'Access, Language_Pattern);

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Running_Filter'Access, Terminate_Pattern);

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Running_Filter'Access, Running_Pattern);

         --  Set another filter to detect the cases when gdb asks questions,
         --  so that we can display dialogs.

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Question_Filter1'Access, Question_Filter_Pattern1);

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Question_Filter2'Access, Question_Filter_Pattern2);

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Continuation_Line_Filter'Access, Continuation_Line_Pattern);

         Add_Filter
           (Get_Descriptor (Debugger.Process).all,
            Output_Filter'Access, Output,
            Window.all'Address);
         Add_Filter
           (Get_Descriptor (Debugger.Process).all,
            Input_Filter'Access, Input,
            Window.all'Address);
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Gdb_Debugger) is
      Num  : Expect_Match;
      Lang : Language_Access;

      use GVD;

   begin
      --  Wait for initial output and prompt (and display it in the window)
      Wait
        (Get_Process (Debugger), Num, Compile ("^\(.+\).*$", Multiple_Lines),
         Timeout => -1);

      --  Make sure that the prompt is what we are expecting.
      Send (Debugger, "set prompt (gdb) ", Mode => Internal);
      Send (Debugger, "set width 0", Mode => Internal);
      Send (Debugger, "set height 0", Mode => Internal);
      Send (Debugger, "set annotate 1", Mode => Internal);

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

      --  Load the module to debug, if any.

      if Debugger.Executable /= VFS.No_File then
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

         if Debugger.Window /= null then
            Run_Debugger_Hook
              (Convert (Debugger.Window, Debugger),
               Debugger_Executable_Changed_Hook);
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
         Set_Args (Debugger, Debugger.Executable_Args.all, Mode => Visible);
      end if;

      if Debugger.Window /= null
        and then Debugger.Executable = VFS.No_File
      then
         Display_Prompt (Debugger);
      end if;

   exception
      --  If the executable was not found, simply display the prompt before
      --  leaving, nothing else needs to be done.

      when Executable_Not_Found =>
         if Debugger.Window /= null then
            Display_Prompt (Debugger);
         end if;
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Gdb_Debugger) is
      Result : Expect_Match;
   begin
      --  If the debugger process is dead, do not attempt to communicate
      --  with the underlying process.

      if Get_Descriptor (Get_Process (Debugger)) = null
        or else Get_Pid
          (Get_Descriptor
            (Get_Process (Debugger)).all) = GNAT.Expect.Invalid_Pid
      then
         Free (Debugger.Process);
         Free (Debugger.Remote_Target);
         Free (Debugger.Remote_Protocol);
         return;
      end if;

      --  In case the debugger was waiting for some input, or was busy
      --  processing a command.
      --  Try to handle case were gdb is waiting on a user question.

      if Command_In_Process (Debugger.Process) then
         Send (Debugger, "n", Wait_For_Prompt => False, Mode => Internal);
         Interrupt (Debugger);
      end if;

      --  Now exit the debugger
      Send (Debugger, "quit", Wait_For_Prompt => False, Mode => Internal);

      --  Ensure that gdb is terminated before closing the pipes and trying to
      --  kill it abruptly.

      begin
         Wait (Get_Process (Debugger), Result, ".+", Timeout => 200);
      exception
         when Process_Died =>
            --  This is somewhat expected... RIP.
            null;
      end;

      begin
         Close (Get_Descriptor (Get_Process (Debugger)).all);
      exception
         when Process_Died =>
            null;
      end;

      Free (Debugger.Process);
      Free (Debugger.Remote_Target);
      Free (Debugger.Remote_Protocol);
   end Close;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   procedure Connect_To_Target
     (Debugger : access Gdb_Debugger;
      Target   : String;
      Protocol : String;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "target " & Protocol & " " & Target, Mode => Mode);
   end Connect_To_Target;

   --------------
   -- Set_Args --
   --------------

   procedure Set_Args
     (Debugger  : access Gdb_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger, "set args " & Arguments, Mode => Mode);
   end Set_Args;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable
     (Debugger : access Gdb_Debugger) return VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   ---------------------------------
   -- Connect_To_Target_If_Needed --
   ---------------------------------

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Remote_Target /= null
        and then not Debugger.Target_Connected
      then
         declare
            Cmd : constant String :=
                    "target " & Debugger.Remote_Protocol.all & " "
                      & Debugger.Remote_Target.all;
         begin
            if Debugger.Window = null then
               Send (Debugger, Cmd, Mode => Internal);

            else
               Output_Text
                 (Convert (Debugger.Window, Debugger),
                  Send (Debugger, Cmd, Mode => Internal) & ASCII.LF);
            end if;

            if Debugger.Remote_Protocol.all = "remote" then
               Set_Is_Started (Debugger, True);
            end if;
         end;

         Debugger.Target_Connected := True;
      end if;
   end Connect_To_Target_If_Needed;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : VFS.Virtual_File;
      Mode       : Command_Type := Hidden)
   is
      pragma Unreferenced (Mode);

      No_Such_File_Regexp : constant Pattern_Matcher :=
                              Compile ("No such file or directory.");
      --  Note that this pattern should work even when LANG isn't english
      --  because gdb does not seem to take into account this variable at all.

      Remote_Exec         : constant String :=
                              To_Remote (Full_Name (Executable, True).all,
                                         Debug_Server, True);
      Exec_Has_Spaces     : constant Boolean :=
                              Index (Remote_Exec, " ") /= 0;
      Process             : Visual_Debugger;

      procedure Launch_Command_And_Output (Command : String);
      --  Launch a "file" or "load" command and display it if relevant.

      -------------------------------
      -- Launch_Command_And_Output --
      -------------------------------

      procedure Launch_Command_And_Output (Command : String) is
         Cmd : GNAT.Strings.String_Access;
      begin
         if Exec_Has_Spaces then
            Cmd := new String'(Command & " """ & Remote_Exec & '"');
         else
            Cmd := new String'(Command & " " & Remote_Exec);
         end if;

         if Debugger.Window /= null then
            Process := Convert (Debugger.Window, Debugger);
            Output_Text (Process, Cmd.all & ASCII.LF, Set_Position => True);
         end if;

         declare
            S : constant String := Send (Debugger, Cmd.all, Mode => Hidden);
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
      Debugger.Executable := Executable;

      --  Send the "file" command if needed

      if Debugger.Mode /= VxWorks then
         Launch_Command_And_Output ("file");
      end if;

      --  Connect to the remote target if needed.

      Connect_To_Target_If_Needed (Debugger);

      --  Send the "load" command if needed

      if Debugger.Mode /= Native then
         Launch_Command_And_Output ("load");
      end if;

      if Debugger.Window /= null then
         Display_Prompt (Debugger);
      end if;

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

      if Debugger.Window /= null then
         Run_Debugger_Hook
           (Convert (Debugger.Window, Debugger),
            Debugger_Executable_Changed_Hook);
      end if;

      --  Get the name and line of the initial file.

      if Get_Language (Debugger).all in Gdb_Ada_Language'Class then
         Switch_Language (Debugger, "c");
         Send (Debugger, "list adainit", Mode => Internal);
         Restore_Language (Debugger);

      else
         Send (Debugger, "list main,main", Mode => Internal);
      end if;

      if Get_Pref (Break_On_Exception) then
         declare
            Cmd : constant String :=
                    Break_Exception
                      (Language_Debugger_Access (Get_Language (Debugger)));
            S   : constant String := Send (Debugger, Cmd);

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
            Str         : constant String :=
                            Send (Debugger, "info line", Mode => Internal);
            Matched     : Match_Array (0 .. 2);
            File_First  : Natural := 0;
            File_Last   : Positive;
            Line        : Natural := 0;
            First, Last : Natural;
            Addr_First,
            Addr_Last   : Natural;

         begin
            Set_Parse_File_Name (Get_Process (Debugger), True);
            Found_File_Name
              (Debugger,
               Str, File_First, File_Last, First, Last, Line,
               Addr_First, Addr_Last);

            if First /= 0 then
               Match
                 (GNAT_Binder_File_Pattern,
                  Str (File_First .. File_Last), Matched);

               --  If we find a file that looks like a GNAT binder file, load
               --  the corresponding main file.

               if Matched (0) /= No_Match then
                  Send
                    (Debugger,
                     "info line " &
                     Str (Matched (0).First + 2 .. Matched (0).Last) & ":1",
                     Mode => Internal);
                  return;
               end if;
            end if;

            Send (Debugger, "info line", Mode => Internal);
         end;
      end if;
   end Set_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   procedure Load_Core_File
     (Debugger : access Gdb_Debugger;
      Core     : String;
      Mode     : Command_Type := Hidden)
   is
      Core_File : constant String := To_Unix_Pathname (Core);
   begin
      Set_Is_Started (Debugger, False);
      Send (Debugger, "core " & Core_File, Mode => Mode);

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

   procedure Add_Symbols
     (Debugger : access Gdb_Debugger;
      Module   : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Symbols : constant String := To_Unix_Pathname (Module);
   begin
      Send (Debugger, "add-symbol-file " & Symbols, Mode => Mode);

      if Mode in Visible_Command then
         Wait_User_Command (Debugger);
      end if;
   end Add_Symbols;

   --------------------
   -- Attach_Process --
   --------------------

   procedure Attach_Process
     (Debugger : access Gdb_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden)
   is
      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural;
      Addr_First,
      Addr_Last   : Natural;

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
            Str : constant String := Send (Debugger, "up", Mode => Internal);
         begin
            Set_Parse_File_Name (Get_Process (Debugger), True);

            --  If attach failed, "up" will return an error message

            if Str = "No stack." then
               Set_Is_Started (Debugger, False);
               return;
            end if;

            exit when Str = "Initial frame selected; you cannot go up.";

            Found_File_Name
              (Debugger,
               Str, File_First, File_Last, First, Last, Line,
               Addr_First, Addr_Last);

            exit when First /= 0;
         end;
      end loop;

      Send (Debugger, "frame", Mode => Internal);

   exception
      when Constraint_Error =>
         --  Most likely the underlying process died.
         null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   procedure Detach_Process
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "detach", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "kill", Mode => Mode);
      Set_Is_Started (Debugger, False);
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : access Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   function Wait_Prompt
     (Debugger : access Gdb_Debugger;
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

   function Get_Module (Executable : VFS.Virtual_File) return String is
      Exec      : constant String := Base_Name (Executable);
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

   procedure Run
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= VFS.No_File
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

   procedure Start
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      if Debugger.Has_Start_Cmd = -1 then
         declare
            S : constant String := Send
                  (Debugger, "help start", Mode => Internal);
         begin
            if S'Length > Undefined_Command'Length
              and then S (S'First ..
                          S'First + Undefined_Command'Length - 1) =
                Undefined_Command
            then
               Debugger.Has_Start_Cmd := 0;
            else
               Debugger.Has_Start_Cmd := 1;
            end if;
         end;
      end if;

      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= VFS.No_File
      then
         declare
            Module : constant String := Get_Module (Debugger.Executable);
         begin
            if Debugger.Has_Start_Cmd = 1 then
               Send (Debugger, "start " & Module, Mode => Mode);
            else
               Send
                 (Debugger,
                  Start (Language_Debugger_Access (Get_Language (Debugger))) &
                    " " & Module, Mode => Mode);
            end if;
         end;

      else
         if Debugger.Has_Start_Cmd = 1 then
            Send (Debugger, "start " & Arguments, Mode => Mode);
         else
            Send
              (Debugger,
               Start (Language_Debugger_Access (Get_Language (Debugger))) &
                 " " & Arguments, Mode => Mode);
         end if;
      end if;

      Set_Is_Started (Debugger, True);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "next", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   procedure Step_Into_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "stepi", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   procedure Step_Over_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "nexti", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "cont", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Debugger : access Gdb_Debugger) is
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
   end Interrupt;

   ------------------------
   -- Is_Context_Command --
   ------------------------

   function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return
        (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "thread")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "task")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "core")
        or else (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "attach");
   end Is_Context_Command;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   function Is_Execution_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
      Index : Natural := Command'First;
   begin
      --  Note: some of commands below can have a numeric parameter, that needs
      --  to be ignored (e.g/ cont 99)

      if Command = "" then
         return False;
      end if;

      Skip_Word (Command, Index);

      return    Command (Command'First .. Index - 1) = "step"
        or else Command (Command'First .. Index - 1) = "stepi"
        or else Command (Command'First .. Index - 1) = "s"
        or else Command (Command'First .. Index - 1) = "si"
        or else Command (Command'First .. Index - 1) = "next"
        or else Command (Command'First .. Index - 1) = "n"
        or else Command (Command'First .. Index - 1) = "nexti"
        or else Command (Command'First .. Index - 1) = "cont"
        or else Command (Command'First .. Index - 1) = "c"
        or else Command (Command'First .. Index - 1) = "run"
        or else Command (Command'First .. Index - 1) = "r"
        or else Command = "finish"
        or else (Command'Length >= 6
          and then
            (Command (Command'First .. Command'First + 5) = "attach"
             or else Command (Command'First .. Command'First + 5) = "target"))
        or else (Command'Length >= 3
          and then Command (Command'First .. Command'First + 2) = "run")
        or else (Command'Length >= 2
          and then Command (Command'First .. Command'First + 1) = "r ")
        or else (Command'Length >= 5
          and then
            (Command (Command'First .. Command'First + 4) = "begin"
             or else Command (Command'First .. Command'First + 4) = "start"));
   end Is_Execution_Command;

   ---------------------
   -- Is_Load_Command --
   ---------------------

   function Is_Load_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return
        (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "file")
        or else (Command'Length >= 15
          and then Command (Command'First .. Command'First + 14) =
            "add-symbol-file")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "load");
   end Is_Load_Command;

   ----------------------
   -- Is_Break_Command --
   ----------------------

   function Is_Break_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Looking_At (Command, Command'First, "break")
        or else Looking_At (Command, Command'First + 1, "break")
        or else Looking_At (Command, Command'First, "b ")
        or else Looking_At (Command, Command'First, "watch")
        or else Looking_At (Command, Command'First, "awatch")
        or else Looking_At (Command, Command'First, "rwatch")
        or else Looking_At (Command, Command'First, "delete")
        or else Looking_At (Command, Command'First, "del ")
        or else Looking_At (Command, Command'First, "disable")
        or else Looking_At (Command, Command'First, "enable")
        or else Looking_At (Command, Command'First, "begin")
        or else Looking_At (Command, Command'First, "start")
        or else Looking_At (Command, Command'First, "ignore")
        or else Looking_At (Command, Command'First, "command")
        or else Looking_At (Command, Command'First, "condition")
        or else Looking_At (Command, Command'First, "set break-command")
        or else Looking_At (Command, Command'First, "change-break");
   end Is_Break_Command;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "down", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "up", Mode => Mode);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   procedure Stack_Frame
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

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural) is
   begin
      Parse_Backtrace_Info
        (Send (Debugger, "where", Mode => Internal), Value, Len);
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Temporary then
         Send (Debugger, "tbreak " & Name, Mode => Mode);
      else
         Send (Debugger, "break " & Name, Mode => Mode);
      end if;
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden) is
   begin
      if Temporary then
         Send (Debugger,
               "tbreak " & Base_Name (File) & ":" & Image (Line),
               Mode => Mode);

      else
         Send (Debugger,
               "break " & Base_Name (File) & ':' & Image (Line),
               Mode => Mode);
      end if;
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger,
            Break_Exception
              (Language_Debugger_Access (Get_Language (Debugger)),
               Name, Temporary, Unhandled),
            Mode => Mode);
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   procedure Break_Address
     (Debugger  : access Gdb_Debugger;
      Address   : GVD.Types.Address_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden) is
   begin
      if Temporary then
         Send
           (Debugger, "tbreak *" & Address_To_String (Address), Mode => Mode);
      else
         Send
           (Debugger, "break *" & Address_To_String (Address), Mode => Mode);
      end if;
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   procedure Break_Regexp
     (Debugger  : access Gdb_Debugger;
      Regexp    : String;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden) is
   begin
      if Temporary then
         raise Unknown_Command;
         --  Error ("Temporary regexp breakpoints not supported");
      else
         Send (Debugger, "rbreak " & Regexp, Mode => Mode);
      end if;
   end Break_Regexp;

   ----------------------------
   -- Get_Last_Breakpoint_Id --
   ----------------------------

   function Get_Last_Breakpoint_Id
     (Debugger : access Gdb_Debugger) return Breakpoint_Identifier
   is
      S            : constant String :=
                       Send (Debugger, "print $bpnum", Mode => Internal);
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

   procedure Set_Breakpoint_Condition
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

   procedure Set_Breakpoint_Command
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

   procedure Set_Breakpoint_Ignore_Count
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

   procedure Set_Scope_Action
     (Debugger : access Gdb_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Scope_String  : GNAT.Strings.String_Access;
      Action_String : GNAT.Strings.String_Access;
   begin
      if Scope /= No_Scope then
         if Scope = Current_Task then
            Scope_String := Task_String'Access;
         elsif Scope = Tasks_In_PD then
            Scope_String := PD_String'Access;
         elsif Scope = Any_Task then
            Scope_String := Any_String'Access;
         end if;

         --  If the breakpoint identifier is 0, then set the
         --  session-wide default scope, otherwise change only the
         --  scope of the breakpoint indicated by Num

         --  These commands are sent as Internal to avoid the call
         --  to Update_Breakpoints in Send_Internal_Post, which causes
         --  the Action radio button to jump to its previous setting

         if Num = 0 then
            Send (Debugger, "set break-command-scope "
                  & Scope_String.all, Mode => GVD.Types.Internal);
         else
            Send (Debugger, "change-breakpoint-scope" & Num'Img
                  & " " & Scope_String.all, Mode => GVD.Types.Internal);
         end if;
      end if;

      if Action /= No_Action then
         if Action = Current_Task then
            Action_String := Task_String'Access;
         elsif Action = Tasks_In_PD then
            Action_String := PD_String'Access;
         elsif Action = All_Tasks then
            Action_String := All_String'Access;
         end if;

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

   procedure Watch
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
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
         Send (Debugger, Command & " " & Name, Mode => Mode);
      else
         Send
           (Debugger,
            Command & " " & Name & " if " & Condition,
            Mode => Mode);
      end if;
   end Watch;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "finish", Mode => Mode);
   end Finish;

   -----------------
   -- Task_Switch --
   -----------------

   procedure Task_Switch
     (Debugger : access Gdb_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "task" & Natural'Image (Task_Num), Mode => Mode);
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   procedure Thread_Switch
     (Debugger : access Gdb_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "thread" & Natural'Image (Thread), Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   procedure PD_Switch
     (Debugger : access Gdb_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "pd " & PD, Mode => Mode);
   end PD_Switch;

   ----------------
   -- Info_Tasks --
   ----------------

   procedure Info_Tasks
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      Output      : constant String :=
                      Send (Debugger, "info tasks", Mode => Internal);
      EOL         : Positive;
      Index       : Positive := Output'First;

   begin
      Len := 0;

      if Output = "The program is not being run."
        or else Output = "No stack"
      then
         return;
      end if;

      while Index < Output'Last loop
         Len := Len + 1;
         EOL := Index;

         while EOL <= Output'Last and then Output (EOL) /= ASCII.LF loop
            EOL := EOL + 1;
         end loop;

         Info (Len) :=
           (Num_Fields => 6,
            Information =>
              (New_String       (Output (Index      .. Index + 3)),
               New_String (Trim (Output (Index + 4  .. Index + 13), Left)),
               New_String (Trim (Output (Index + 14 .. Index + 18), Left)),
               New_String (Trim (Output (Index + 19 .. Index + 22), Left)),
               New_String (Trim (Output (Index + 23 .. Index + 45), Left)),
               New_String (Trim (Output (Index + 46 .. EOL - 1), Left))
              ));
         Index := EOL + 1;
      end loop;

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

   procedure Info_Threads
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      Output      : constant String :=
                      Send (Debugger, "info threads", Mode => Internal);
      EOL         : Natural;
      Index       : Integer := Output'Last;

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

   procedure Info_PD
     (Debugger : access Gdb_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural)
   is
      Output      : constant String :=
                      Send (Debugger, "info pds", Mode => Internal);
      EOL         : Positive;
      Start       : Positive := Output'First;
      First       : Positive := Output'First;
      Second      : Positive := Output'First;

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

   --------------
   -- Info_WTX --
   --------------

   procedure Info_WTX
     (Debugger : access Gdb_Debugger;
      Version  : out Natural) is
   begin
      if Debugger.WTX_Version = -1 then
         declare
            Output : constant String :=
              Send (Debugger, "info wtx", Mode => Internal);
         begin
            if Output = "WTX protocol version 2" then
               Debugger.WTX_Version := 2;
            elsif Output = "WTX protocol version 3" then
               Debugger.WTX_Version := 3;
            else
               Debugger.WTX_Version := 0;
            end if;
         end;
      end if;

      Version := Debugger.WTX_Version;
   end Info_WTX;

   ---------------------
   -- Lines_With_Code --
   ---------------------

   Cached_File  : VFS.Virtual_File;
   Cached_Lines : Line_Array_Access;

   procedure Lines_With_Code
     (Debugger : access Gdb_Debugger;
      File     : VFS.Virtual_File;
      Result   : out Boolean;
      Lines    : out Line_Array)
   is
      procedure Parse_List_Lines
        (S         : String;
         Lines     : Line_Array_Access;
         Num_Lines : out Natural);
      --  Parse S and set Num_Lines and Lines accordingly.
      --  S is the output of the command -symbol-list-lines.
      --  If Lines is null, only Num_Lines is computed.

      ----------------------
      -- Parse_List_Lines --
      ----------------------

      procedure Parse_List_Lines
        (S         : String;
         Lines     : Line_Array_Access;
         Num_Lines : out Natural)
      is
         Pos, Last, Val : Natural;
      begin
         Num_Lines := 0;
         Pos := S'First;

         loop
            Pos := Index (S (Pos .. S'Last), "line=");

            exit when Pos = 0;

            Pos  := Pos + 6;
            Last := Index (S (Pos .. S'Last), """");
            Val  := Integer'Value (S (Pos .. Last - 1));
            --  Skip "},{pc="...
            Pos := Last + 12;

            if Val > Num_Lines then
               Num_Lines := Val;
            end if;

            if Lines /= null and then Val in Lines'Range then
               Lines (Val) := True;
            end if;
         end loop;

      exception
         when Constraint_Error =>
            Result    := False;
            Num_Lines := 0;
      end Parse_List_Lines;

   begin
      Result := False;

      if Debugger.Has_Symbol_List = 0 then
         return;
      end if;

      --  We cache the result of -symbol-list-lines for the last file queried,
      --  since Lines_With_Code is typically called for a line subset rather
      --  than the whole file.

      if Cached_File = File then
         Result := True;
      else
         declare
            --  Send the gdb command to get the list of lines with code.
            --  The call to "Format_Pathname" is to work a bug in some versions
            --  of gdb under Windows.
            S              : constant String := Send
              (Debugger,
               "-symbol-list-lines "
               & Format_Pathname (Full_Name (File).all, UNIX),
               Mode => Internal);
            Num_Lines, Pos : Natural;

         begin
            if Debugger.Has_Symbol_List = -1 then
               if S'Length > No_Definition_Of'Length
                 and then S (S'First ..
                             S'First + No_Definition_Of'Length - 1) =
                   No_Definition_Of
               then
                  Debugger.Has_Symbol_List := 0;
                  return;
               else
                  Debugger.Has_Symbol_List := 1;
               end if;
            end if;

            if S'Length < List_Lines'Length
              or else S (S'First ..
                         S'First + List_Lines'Length - 1) /= List_Lines
            then
               return;
            end if;

            Cached_File := File;
            Result      := True;
            Pos         := S'First + List_Lines'Length;
            Parse_List_Lines (S (Pos .. S'Last), null, Num_Lines);
            Free (Cached_Lines);
            Cached_Lines := new Line_Array'(1 .. Num_Lines => False);
            Parse_List_Lines (S (Pos .. S'Last), Cached_Lines, Num_Lines);
         end;
      end if;

      for Val in Lines'Range loop
         if Val in Cached_Lines'Range then
            Lines (Val) := Cached_Lines (Val);
         else
            Lines (Val) := False;
         end if;
      end loop;
   end Lines_With_Code;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt (Debugger : access Gdb_Debugger) is
   begin
      Output_Text
        (Convert (Debugger.Window, Debugger),
         Send_Full (Debugger, "  ", Mode => Internal),
         Is_Command => False,
         Set_Position => True);
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Debugger    : access Gdb_Debugger;
      Dir         : String;
      Mode        : Command_Type := Hidden)
   is
      Directory : constant String := To_Unix_Pathname (Dir);
   begin
      Send (Debugger, "cd " & Directory, Mode => Mode);
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural)
   is
      pragma Unreferenced (Debugger);

      Start    : Natural := Str'First;
      Matched  : Match_Array (0 .. 3);
      Matched2 : Match_Array (0 .. 3);

   begin
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

      First := Matched (0).First;
      Last  := Matched (0).Last;

      if Matched (0) = No_Match then
         --  Try another regexp
         --  This regexp takes longer to execute when the output has a lot of
         --  lines. There wouldn't be any need to test that if we knew what
         --  is the debugger output and what is the user's program output???

         Match (File_Name_Pattern2, Str, Matched);

         if Matched (0) = No_Match then
            Name_First := 0;
            Name_Last  := 1;
            Addr_First := 0;
            Addr_Last  := 0;
            Line       := 0;
            return;
         end if;

         First := Matched (0).First;
         Last  := Matched (0).Last;
      end if;

      if Last < Str'Last and then Str (Last + 1) = ASCII.LF then
         Last := Last + 1;
      end if;

      Name_First := Matched (1).First;
      Name_Last  := Matched (1).Last;
      Addr_First := Matched (3).First;
      Addr_Last  := Matched (3).Last;
      Line       := Natural'Value
        (Str (Matched (2).First .. Matched (2).Last));
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   procedure Found_Frame_Info
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger);

      Matched : Match_Array (0 .. 1);
   begin
      Match (Frame_Pattern, Str, Matched);

      if Matched (1) /= No_Match then
         First := Matched (1).First;
         Last  := Matched (1).Last;
         Match (Frame_Pattern_With_File, Str, Matched);
         if Matched (1) /= No_Match then
            Message := Location_Found;
         else
            Message := No_Debug_Info;
         end if;

      else
         First := 0;
         Last  := 0;
         Message := Location_Not_Found;
      end if;
   end Found_Frame_Info;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Gdb_Debugger) return GNAT.Strings.String_List
   is
      S         : constant String :=
                    Send (Debugger, "info sources", Mode => Internal);
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

   --------------------------
   -- Internal_Parse_Value --
   --------------------------

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive;
      Parent     : Items.Generic_Type_Access)
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
      Dim : Dimension;

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

         if Result'Tag = Simple_Type'Tag
           or else Result'Tag = Range_Type'Tag
           or else Result'Tag = Mod_Type'Tag
           or else Result'Tag = Enum_Type'Tag
         then
            Set_Value (Simple_Type (Result.all), "<???>");

         elsif Result'Tag = Access_Type'Tag then
            Set_Value (Simple_Type (Result.all), "0x0");
         end if;

         return;
      end if;

      -------------------
      -- Simple values --
      -------------------

      if Result'Tag = Simple_Type'Tag
        or else Result'Tag = Range_Type'Tag
        or else Result'Tag = Mod_Type'Tag
        or else Result'Tag = Enum_Type'Tag
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
               Set_Value (Simple_Type (Result.all),
                          Type_Str (Int .. Index - 1));
            end;
         else
            Set_Value (Simple_Type (Result.all), "<???>");
         end if;

      -------------------
      -- Access values --
      -------------------
      --  The value looks like:   (access integer) 0xbffff54c
      --  or                  :    0x0
      --  or                  :   (<ref> TstringS29b) @0xbfffdba0
      --  or                  :   (void (*)()) 0x804845c <foo>

      elsif Result'Tag = Access_Type'Tag then

         if Looking_At (Type_Str, Index, "(null)") then
            Set_Value (Simple_Type (Result.all), "0x0");
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
                     Str    : String (1 .. 0);
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

               Set_Value
                 (Simple_Type (Result.all), Type_Str (Int .. Index - 1));
            end;
         end if;

      -------------------
      -- String values --
      -------------------

      elsif Result'Tag = Array_Type'Tag
        and then Num_Dimensions (Array_Type (Result.all)) = 1
        and then Type_Str'Length /= 0
        and then (Type_Str (Index) = '"' or else Type_Str (Index) = ''')
      then
         Dim := Get_Dimensions (Array_Type (Result.all), 1);

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
            S : String (1 .. Integer (Dim.Last - Dim.First + 1));
            S_Last : Natural;
            Simple : Simple_Type_Access;

         begin
            Parse_Cst_String
              (Type_Str, Index, S, S_Last,
               Backslash_Special => Get_Language_Context
               (Lang).Quote_Character = '\');
            Simple := Simple_Type_Access
              (Get_Value (Array_Type (Result.all), Dim.First));

            if Simple = null then
               Simple := Simple_Type_Access (New_Simple_Type);
            end if;

            Set_Value (Simple.all, S (S'First .. S_Last));

            --  The index should always be 0, since we add Dim.First before
            --  displaying it.

            Set_Value (Item       => Array_Type (Result.all),
                       Elem_Value => Simple,
                       Elem_Index => 0);
            Shrink_Values (Array_Type (Result.all));
         end;

      ------------------
      -- Array values --
      ------------------

      elsif Result'Tag = Array_Type'Tag
        and then Type_Str'Length /= 0   --  for empty Arrays
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
              (Lang, Type_Str, Index, Result, Repeat_Num, Parent);

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
                  Parse_Array_Value
                    (Lang, Type_Str, Index, Array_Type_Access (Result));
                  return;
               end if;
            end;

            --  Otherwise, we convert to an access type

            if Parent /= null then
               Result := Replace (Parent, Result, New_Access_Type);
            else
               Free (Result, Only_Value => False);
               Result := New_Access_Type;
            end if;

            Internal_Parse_Value
              (Lang, Type_Str, Index, Result, Repeat_Num, Parent => Parent);

         else
            Parse_Array_Value
              (Lang, Type_Str, Index, Array_Type_Access (Result));
         end if;

      -------------------
      -- Record values --
      -------------------

      elsif Result'Tag = Record_Type'Tag
        or else Result'Tag = Union_Type'Tag
      then
         declare
            R   : constant Record_Type_Access := Record_Type_Access (Result);
            Int : Natural;
         begin
            --  Skip initial '(' if we are still looking at it (we might not
            --  if we are parsing a variant part)

            if Index <= Type_Str'Last
              and then Type_Str (Index) = Context.Record_Start
            then
               Index := Index + 1;
            end if;

            for J in 1 .. Num_Fields (R.all) loop

               --  If we are expecting a field

               if Get_Variant_Parts (R.all, J) = 0 then
                  declare
                     V          : Generic_Type_Access := Get_Value (R.all, J);
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
                       (Lang, Type_Str, Index, V, Repeat_Num,
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
                     V : Generic_Type_Access;
                  begin
                     V := Find_Variant_Part
                       (Item     => R.all,
                        Field    => J,
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

                     if V /= null then
                        Internal_Parse_Value
                          (Lang, Type_Str, Index, V, Repeat_Num,
                           Parent => Result);
                     end if;
                  end;
               end if;
            end loop;
         end;

         Skip_Blanks (Type_Str, Index);

         --  Skip closing ')', if seen
         if Index <= Type_Str'Last
           and then Type_Str (Index) = Context.Record_End
         then
            Index := Index + 1;
         end if;

      ------------------
      -- Class values --
      ------------------

      elsif Result'Tag = Class_Type'Tag then
         declare
            R : Generic_Type_Access;
         begin
            for A in 1 .. Get_Num_Ancestors (Class_Type (Result.all)) loop
               R := Get_Ancestor (Class_Type (Result.all), A);
               Internal_Parse_Value
                 (Lang, Type_Str, Index, R, Repeat_Num, Parent => Result);
            end loop;
            R := Get_Child (Class_Type (Result.all));

            if Num_Fields (Record_Type (R.all)) /= 0 then
               Internal_Parse_Value
                 (Lang, Type_Str, Index, R, Repeat_Num, Parent => Result);
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

   ----------------------
   -- List_Breakpoints --
   ----------------------

   function List_Breakpoints
     (Debugger  : access Gdb_Debugger) return Breakpoint_Array
   is
      S               : constant String := Send
        (Debugger, "info breakpoints", Mode => Internal);
      Num_Breakpoints : Natural := 0;
      Index           : Natural := S'First;
      Tmp             : Natural;

      WTX_Version : Natural;

      procedure Fill_Scope_Action
        (Debugger        : access Gdb_Debugger;
         List            : in out Breakpoint_Array;
         Num_Breakpoints : Natural);
      --  Assign values of Scope and Action for the breakpoints in
      --  List as reported by the debugger.

      -----------------------
      -- Fill_Scope_Action --
      -----------------------

      procedure Fill_Scope_Action
        (Debugger        : access Gdb_Debugger;
         List            : in out Breakpoint_Array;
         Num_Breakpoints : Natural)
      is
         S : constant String :=
               Send (Debugger, "info breakpoints-extra-info",
                     Mode => Internal);

         Index   : Natural := S'First;
         Matched : Match_Array (0 .. 4);
         Scope   : GVD.Types.Scope_Type;
         Action  : GVD.Types.Action_Type;

      begin
         --  skip the first line

         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         for J in 1 .. Num_Breakpoints loop
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
            end if;
         end loop;
      end Fill_Scope_Action;

   begin
      --  Skip the first line (that indicates there is no breakpoints,
      --  or that gives the title of each column).
      --  A breakpoint exists for each line that starts with a number.

      while Index <= S'Last loop
         if S (Index) in '0' .. '9' then
            Num_Breakpoints := Num_Breakpoints + 1;
         end if;
         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;
      end loop;

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
      --
      --  For both the breakpoint and watchpoint, the first line is the
      --  condition, the second line is the ignore count, and the rest are
      --  the commands to execute upon stopping.

      declare
         Br      : Breakpoint_Array (1 .. Num_Breakpoints);
         Num     : Natural := 1;
         Matched : Match_Array (0 .. 10);
         M       : Boolean;
      begin
         Index := S'First;
         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Num <= Num_Breakpoints loop
            Match (Breakpoint_Pattern, S (Index .. S'Last), Matched);

            if Matched (0) /= No_Match then
               Br (Num).Num := Breakpoint_Identifier'Value
                 (S (Matched (1).First .. Matched (1).Last));

               case S (Matched (2).First) is
                  when 'b' =>
                     --  "breakpoint"
                     Br (Num).The_Type := Breakpoint;
                  when 'a' =>
                     --  "acc watchpoint"
                     Br (Num).The_Type := Watchpoint;
                     Br (Num).Trigger  := Read_Write;
                  when 'r' =>
                     --  "read watchpoint"
                     Br (Num).The_Type := Watchpoint;
                     Br (Num).Trigger  := Read;
                  when others =>
                     --  "hw watchpoint"
                     Br (Num).The_Type := Watchpoint;
                     Br (Num).Trigger  := Write;
               end case;

               case S (Matched (3).First) is
                  when 'k'    => Br (Num).Disposition := Keep;
                  when 'd'    => Br (Num).Disposition := Disable;
                  when others => Br (Num).Disposition := Delete;
               end case;

               Br (Num).Enabled := S (Matched (4).First) = 'y';

               if Br (Num).The_Type = Breakpoint then
                  Br (Num).Address := String_To_Address
                    ("0x" & S (Matched (7).First .. Matched (7).Last));
               else
                  Br (Num).Expression :=
                    new String'(S (Matched (8).First .. Matched (8).Last));
               end if;

               --  Go to beginning of next line.
               --  If we don't have a new breakpoint, add the line to the
               --  information.

               Tmp := Matched (8).First;
               Index := Tmp;

            else
               Tmp := Index;
            end if;

            while Index <= S'Last
              and then not (S (Index) in '0' .. '9')
            loop
               Tmp := Index;
               Skip_To_Char (S, Index, ASCII.LF);
               Index := Index + 1;
               M := False;

               --  File name, exception name and subprogram name can be
               --  found on the same line.

               Match (File_Name_In_Breakpoint, S (Tmp .. Index - 2), Matched);
               if Matched (0) /= No_Match then
                  --  Translate the matched filename into local file if needed.
                  Br (Num).File := Create_From_Base
                    (To_Local
                       (S (Matched (1).First .. Matched (1).Last),
                        Debug_Server),
                     GPS_Window (Debugger.Window).Kernel);

                  Br (Num).Line := Integer'Value
                    (S (Matched (2).First .. Matched (2).Last));
                  M := True;
               end if;

               Match (Exception_In_Breakpoint, S (Tmp .. Index - 2), Matched);
               if Matched (0) /= No_Match then
                  if Matched (1) /= No_Match then
                     if Matched (3) /= No_Match then
                        Br (Num).Except := new String'
                          (S (Matched (3).First .. Matched (3).Last));
                     else
                        Br (Num).Except :=
                          new String'
                            (S (Matched (2).First .. Matched (2).Last));
                     end if;
                  else
                     if Matched (5) /= No_Match then
                        Br (Num).Except := new String'
                          (S (Matched (5).First .. Matched (5).Last));
                     else
                        Br (Num).Except :=
                          new String'
                            (S (Matched (4).First .. Matched (4).Last));
                     end if;
                  end if;
                  M := True;
               end if;

               Match (Subprogram_In_Breakpoint, S (Tmp .. Index - 2), Matched);
               if Matched (0) /= No_Match then
                  Br (Num).Subprogram := new String'
                    (S (Matched (1).First .. Matched (1).Last));
                  M := True;
               end if;

               if not M then
                  Match
                    (Condition_In_Breakpoint, S (Tmp .. Index - 2), Matched);
                  if Matched (0) /= No_Match then
                     Br (Num).Condition := new String'
                       (S (Matched (1).First .. Matched (1).Last));
                     M := True;
                  end if;
               end if;

               if not M then
                  Match (Ignore_In_Breakpoint, S (Tmp .. Index - 2), Matched);
                  if Matched (0) /= No_Match then
                     Br (Num).Ignore := Natural'Value
                       (S (Matched (1).First .. Matched (1).Last));
                     M := True;
                  end if;
               end if;

               if not M then
                  --  List of commands:
                  if Tmp + 7 <= S'Last
                    and then S (Tmp .. Tmp + 7) = "        "
                  then
                     while Index + 7 <= S'Last
                       and then S (Index .. Index + 7) = "        "
                     loop
                        Skip_To_Char (S, Index, ASCII.LF);
                        Index := Index + 1;
                     end loop;

                     if Index /= Tmp then
                        Br (Num).Commands :=
                          new String'(S (Tmp .. Index - 2));
                     end if;
                  end if;
               end if;

--  ??? Why is this code commented out
--               Br (Num).Info := new String'(S (Tmp .. Index - 2));
            end loop;

            Num := Num + 1;
         end loop;

         --  Fill the breakpoints extra information
         Info_WTX (Debugger, WTX_Version);

         if WTX_Version = 3 then
            Fill_Scope_Action (Debugger, Br, Num_Breakpoints);

         else
            for J in 1 .. Num_Breakpoints loop
               Br (J).Scope := No_Scope;
               Br (J).Action := No_Action;
            end loop;
         end if;

         return Br;
      end;
   end List_Breakpoints;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   procedure Enable_Breakpoint
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

   procedure Remove_Breakpoint
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

   function List_Exceptions
     (Debugger : access Gdb_Debugger)
     return GVD.Types.Exception_Array
   is
      S     : constant String :=
                Send (Debugger, "info exceptions", Mode => Internal);
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
            Arr (Num).Name := new String'(S (Start .. Index - 1));
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

   function Get_Type_Info
     (Debugger  : access Gdb_Debugger;
      Entity    : String;
      Default   : String) return String
   is
      S : constant String :=
            Send (Debugger, "whatis " & Entity, Mode => Internal);

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

   function Find_File
     (Debugger : access Gdb_Debugger; File_Name : String) return String
   is
      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural;
      Addr_First,
      Addr_Last   : Natural;

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
         Str : constant String :=
           Send (Debugger, "info line " & File_Name & ":1", Mode => Internal);
      begin
         Restore_Language (Debugger);
         Set_Parse_File_Name (Get_Process (Debugger), True);
         Found_File_Name
           (Debugger,
            Str, File_First, File_Last, First, Last, Line,
            Addr_First, Addr_Last);

         if First = 0 then
            return File_Name;
         else
            return Str (File_First .. File_Last);
         end if;
      end;
   end Find_File;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   procedure Get_Machine_Code
     (Debugger      : access Gdb_Debugger;
      Range_Start   : out GVD.Types.Address_Type;
      Range_End     : out Address_Type;
      Code          : out GNAT.Strings.String_Access;
      Start_Address : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address)
   is
      Disassembled : constant String := Send
        (Debugger,
         "disassemble " &
         Address_To_String (Start_Address) & " " &
         Address_To_String (End_Address),
         Mode => Internal);
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

   procedure Get_Line_Address
     (Debugger    : access Gdb_Debugger;
      Line        : Natural;
      Range_Start : out Address_Type;
      Range_End   : out Address_Type) is
   begin
      Set_Parse_File_Name (Get_Process (Debugger), False);
      Switch_Language (Debugger, "c");

      declare
         S : constant String := Send
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

   function Get_Memory
     (Debugger : access Gdb_Debugger;
      Size     : in Integer;
      Address  : in String) return String
   is
      Endian       : constant Endian_Type := Get_Endian_Type (Debugger);
      Error_String : constant String := "Cannot access memory at";
      Image        : constant String := Integer'Image (Size / 8);
      S            : constant String := Send
        (Debugger,
         "x/" & Image (Image'First + 1 .. Image'Last)
         & "gx " & Address, Mode => Internal);
      Result       : String (1 .. Size * 2);
      S_Index      : Integer := S'First + 2;
      Last_Index   : Integer := S'First + 2;
      Result_Index : Integer := 1;
      Last         : Integer := S'Last;

   begin
      --  Detect "Cannot access memory at ..."

      Skip_To_String (S, Last_Index, Error_String);

      if Last_Index <= S'Last - Error_String'Length then
         --  The error string was detected...

         Last := Last_Index;
      end if;

      while S_Index <= Last loop
         --  Detect actual data : 0xXX... right after an ASCII.HT.

         if S (S_Index) = '0' then
            if S (S_Index - 1) = ASCII.HT then
               Result (Result_Index .. Result_Index + 15) :=
                 S (S_Index + 2 .. S_Index + 17);
               Result_Index := Result_Index + 16;
            end if;
         end if;

         S_Index := S_Index + 1;
      end loop;

      --  Fill the values that could not be accessed with "-".
      while Result_Index <= Result'Last loop
         Result (Result_Index) := '-';
         Result_Index := Result_Index + 1;
      end loop;

      if Endian = Little_Endian then
         --  We need to reverse the blocks.

         for J in 1 .. Result'Length / 16 loop
            declare
               Block : constant String (1 .. 16) :=
                         Result (Result'First + (J - 1) * 16 ..
                                   Result'First + J * 16 - 1);
            begin
               for K in 1 .. 8 loop
                  Result (Result'First + (J - 1) * 16 + (K - 1) * 2
                          .. Result'First + (J - 1) * 16 + K * 2 - 1)
                    := Block (16 - K * 2 + 1 .. 16 - (K - 1) * 2);
               end loop;
            end;
         end loop;
      end if;

      return Result;

   exception
      when Constraint_Error =>
         return "";
   end Get_Memory;

   ---------------------
   -- Put_Memory_Byte --
   ---------------------

   procedure Put_Memory_Byte
     (Debugger : access Gdb_Debugger;
      Address  : in String;
      Byte     : in String) is
   begin
      Switch_Language (Debugger, "c");
      Send (Debugger, "set {char}" & Address & " = 0x" & Byte,
            Mode => Internal);
      Restore_Language (Debugger);
   end Put_Memory_Byte;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   function Get_Variable_Address
     (Debugger : access Gdb_Debugger;
      Variable : in String) return String
   is
      S         : constant String := Send
        (Debugger, "print &(" & Variable & ")", Mode => Internal);
      Index     : Integer := S'Last;
      Error_Msg : constant String := "No ";
      --  Error messages can be "No definition..." or "No symbol..."

   begin
      if S (S'First .. S'First + Error_Msg'Length - 1) = Error_Msg then
         return "";
      end if;

      --  Find the last occurence of "0x" in the string.
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

   function Get_Endian_Type
     (Debugger : access Gdb_Debugger) return Endian_Type is
   begin
      if Debugger.Endian /= Unknown_Endian then
         --  Return the cached value, to avoid too much communication with
         --  the underlying debugger.
         return Debugger.Endian;
      end if;

      declare
         S      : constant String := Send
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

   function Complete
     (Debugger  : access Gdb_Debugger;
      Beginning : String) return GNAT.Strings.String_List
   is
      S           : constant String := Send
        (Debugger, "complete " & Beginning, Mode => Internal);
      First_Index : Integer := S'First;
      Last_Index  : Integer := S'First;
      Num         : Integer := 0;

   begin
      --  Find the number of words in the list.

      for Index in S'Range loop
         if S (Index) = ASCII.LF then
            Num := Num + 1;
         end if;
      end loop;

      if S'Length /= 0 then
         Num := Num + 1;
      end if;

      --  Fill the string array with the proper values.
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

   procedure Open_Processes (Debugger : access Gdb_Debugger) is
   begin
      if Debugger.Remote_Protocol /= null
        and then Debugger.Remote_Protocol.all = "wtx"
      then
         Debugger.WTX_List :=
            new String'
              (Send (Debugger, "info wtx threads", Mode => Internal));

         if Debugger.WTX_List'Length > Undefined_Info_Command'Length
           and then
             Debugger.WTX_List
               (Debugger.WTX_List'First
                 .. Debugger.WTX_List'First + Undefined_Command'Length - 1)
              = Undefined_Command
         then
            Free (Debugger.WTX_List);
            Debugger.WTX_List :=
               new String'
                 (Send (Debugger, "tcl activeTaskNameMap", Mode => Internal));
         end if;

         Debugger.WTX_Index := Debugger.WTX_List'First;

      else
         Open_Processes (Debugger.Handle, Debugger.Kernel);
      end if;
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
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

   procedure Close_Processes (Debugger : access Gdb_Debugger) is
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

   procedure Detect_Language (Debugger : access Gdb_Debugger) is
      S : constant String := Send (Debugger, "show lang", Mode => Internal);
      pragma Unreferenced (S);
   begin
      null;
   end Detect_Language;

   ---------------------
   -- Switch_Language --
   ---------------------

   procedure Switch_Language
     (Debugger : access Gdb_Debugger;
      Language : in String)
   is
      S           : constant String :=
                      Send (Debugger, "show lang", Mode => Internal);
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

   function Support_TTY (Debugger : access Gdb_Debugger) return Boolean is
      pragma Unreferenced (Debugger);
   begin
      return True;
   end Support_TTY;

   -------------
   -- Set_TTY --
   -------------

   procedure Set_TTY (Debugger : access Gdb_Debugger; TTY : String) is
   begin
      Send (Debugger, "tty " & TTY, Mode => Hidden);
   end Set_TTY;

end Debugger.Gdb;

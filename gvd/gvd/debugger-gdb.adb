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

with System;            use System;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Tags;          use Ada.Tags;
with GNAT.Regpat;       use GNAT.Regpat;

pragma Warnings (Off);
with GNAT.Expect;       use GNAT.Expect;
pragma Warnings (On);

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gtk.Window;        use Gtk.Window;

with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;

with Odd_Intl;          use Odd_Intl;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Gdb.Ada;  use Debugger.Gdb.Ada;
with Debugger.Gdb.C;    use Debugger.Gdb.C;
with Debugger.Gdb.Cpp;  use Debugger.Gdb.Cpp;
with Process_Proxies;   use Process_Proxies;
with GVD.Process;       use GVD.Process;
with GVD.Strings;       use GVD.Strings;
with GVD.Dialogs;       use GVD.Dialogs;
with GVD.Types;         use GVD.Types;
with GVD.Trace;         use GVD.Trace;
with GVD.Status_Bar;    use GVD.Status_Bar;
with Items;             use Items;
with Items.Simples;     use Items.Simples;
with Items.Arrays;      use Items.Arrays;
with Items.Records;     use Items.Records;
with Items.Classes;     use Items.Classes;

with Unchecked_Conversion;

package body Debugger.Gdb is

   use String_History;

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher := Compile ("\(gvd\) ");
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_Length : constant := 6;
   --  Length of the prompt ("(gvd) ").

   Gdb_Command   : constant String := "gdb";
   --  Name of the command to launch gdb.

   Gdb_Options   : constant String := "-nw -q";
   --  Options always passed to gdb.
   --  Note that we assume that only one blank is put between each option.

   Highlight_Pattern : constant Pattern_Matcher :=
     Compile ("^\(gvd\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window.

   File_Name_Pattern : constant Pattern_Matcher :=
     Compile (ASCII.SUB & ASCII.SUB
              & "(.+):(\d+):\d+:[^:]+:(0x[0-9a-f]+)$", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output.

   File_Name_Pattern2 : constant Pattern_Matcher :=
     Compile ("^([^:\n]+):(\d+): No such file or directory.", Multiple_Lines);
   --  Second regexp used to detect when the current frame can not be displayed
   --  Note that this pattern should work even when LANG isn't english because
   --  gdb does not seem to take into account this variable at all.

   Language_Pattern : constant Pattern_Matcher := Compile
     ("^(The current source language is|Current language:) +" &
      """?(auto; currently )?([^""\s]+)", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger.

   Frame_Pattern : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))?$", Multiple_Lines);
   --  Regular expression used to detect and parse callstack frames

   Breakpoint_Pattern : constant Pattern_Matcher := Compile
     ("^(\d+)\s+(breakpoint|\w+? watchpoint)\s+(keep|dis|del)\s+([yn])"
      & "\s+((0x0*)?(\S+))\s+(.*)$",
      Multiple_Lines);
   --  Pattern to match a single line in "info breakpoint"

   File_Name_In_Breakpoint : constant Pattern_Matcher := Compile
     ("at (.+):(\d+)$", Multiple_Lines);
   --  How to find file names in the info given by "info breakpoint".
   --  Note that we have to allow for special characters in the directory
   --  or file name, since the user might be using some strange names. The only
   --  restriction is that the name can not contain newline characters.

   Exception_In_Breakpoint : constant Pattern_Matcher := Compile
     ("on ([-\w_:]+)");
   --  How to detect exception names in the info given by "info breakpoint"

   Subprogram_In_Breakpoint : constant Pattern_Matcher := Compile
     ("in (\S+)");
   --  How to detect subprogram names in the info given by "info breakpoint"

   Question_Filter_Pattern1 : constant Pattern_Matcher :=
     Compile ("^\[0\] .*> ", Multiple_Lines + Single_Line);

   Question_Filter_Pattern2 : constant Pattern_Matcher :=
     Compile ("^(.*\?) \(y or n\) ", Multiple_Lines);
   --  How to detect a question in gdb's output

   Address_Range_Pattern : constant Pattern_Matcher := Compile
     ("starts at address (0x[0-9a-f]+) <[^>]+> and ends at (0x[0-9a-f]+)");
   --  How to get the range of addresses for a given line

   procedure Language_Filter
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect a change in the current language.

   procedure Question_Filter1
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect questions from gdb.

   procedure Question_Filter2
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect y/n questions from gdb.

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

   function To_Main_Debug_Window is new
     Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

   ---------------------
   -- Language_Filter --
   ---------------------

   procedure Language_Filter
     (Process : access Debugger_Process_Tab_Record'Class;
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
         else
            Print_Message
              (Process.Window.Statusbar1,
               Error, (-"Language currently not supported by GVD: ") & Lang);
            Language := new Gdb_C_Language;
         end if;

         Set_Language (Debugger, Language);
         Set_Debugger
           (Language_Debugger_Access (Language), Debugger.all'Access);
      end;
   end Language_Filter;

   ---------------------
   -- Question_Filter --
   ---------------------

   procedure Question_Filter1
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Dialog   : Question_Dialog_Access;
      Index    : Natural;
      Debugger : constant Debugger_Access := Process.Debugger;
      Choices  : Question_Array (1 .. 1000);
      --  ??? This is an arbitrary hard-coded limit, that should
      --  be enough. Might be nice to remove it though.

      Num      : Natural := 0;
      First    : Positive;
      Last     : Positive := Matched (0).First;

   begin
      --  If we are processing an internal command, we cancel any question
      --  dialog we might have, and silently fail
      --  ??? For some reason, we can not use Interrupt here, and we have
      --  to rely on the fact that "Cancel" is the first choice.

      if Get_Command_Mode (Get_Process (Debugger)) = Internal then
         Send (Debugger, "0",
               Mode => Internal,
               Empty_Buffer => False,
               Wait_For_Prompt => False);
         return;
      end if;

      --  Index is positioned to the last LF character

      Index := Matched (0).Last - 3;

      while Last < Index loop
         --  Skips the choice number ("[n] ")
         Last := Last + 4;
         First := Last;

         while Last < Index and then Str (Last) /= ASCII.LF loop
            Last := Last + 1;
         end loop;

         Num := Num + 1;
         Choices (Num).Choice :=
           new String' (Natural'Image (Num - 1));
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
     (Process : access Debugger_Process_Tab_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      Dialog   : Question_Dialog_Access;
      Debugger : constant Debugger_Access := Process.Debugger;
      Choices  : Question_Array (1 .. 2);

   begin
      Choices (1).Choice := new String' ("n");
      Choices (1).Description := new String' ("No");

      Choices (2).Choice := new String' ("y");
      Choices (2).Description := new String' ("Yes");

      Gtk_New
        (Dialog,
         Gtk_Window (Process.Window),
         Debugger,
         False,
         Choices (1 .. 2),
         Str (Matched (0).First .. Matched (0).Last));
      Show_All (Dialog);

      Free (Choices (1).Choice);
      Free (Choices (1).Description);
      Free (Choices (2).Choice);
      Free (Choices (2).Description);
   end Question_Filter2;

   ----------
   -- Send --
   ----------

   function Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : Invisible_Command := Hidden) return String
   is
      S : constant String :=
        Send_Full (Debugger, Cmd, Empty_Buffer, Wait_For_Prompt, Mode);
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

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Gdb_Debugger; Entity : String) return String
   is
      S : constant String :=
        Send (Debugger, "ptype " & Entity, Mode => Internal);
   begin
      if S'Length > 6
        and then S (S'First .. S'First + 5) = "type ="
      then
         return S (S'First + 7 .. S'Last);
      else
         return "";
      end if;
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   function Info_Locals (Debugger : access Gdb_Debugger) return String is
   begin
      return "info locals";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   function Info_Args (Debugger : access Gdb_Debugger) return String is
   begin
      return "info args";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   function Info_Registers (Debugger : access Gdb_Debugger) return String is
   begin
      return "info registers";
   end Info_Registers;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String
   is
      S : constant String :=
        Send (Debugger, "print " & Entity, Mode => Internal);
      Index : Natural := S'First;
   begin
      --  The value is valid only if it starts with '$'

      if S (S'First) /= '$' then
         return "";
      end if;

      --  Skip the '$nn =' part
      Skip_To_Char (S, Index, '=');
      Index := Index + 1;

      return S (Index + 1 .. S'Last);
   end Value_Of;

   -----------------
   -- Print_Value --
   -----------------

   procedure Print_Value
     (Debugger : access Gdb_Debugger;
      Entity   : String) is
   begin
      Send (Debugger, "print " & Entity, Mode => Visible);
   end Print_Value;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String
   is
      --  ??? Probably, this should be language-dependent.
      S       : String :=
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
      Executable      : String;
      Debugger_Args   : GNAT.OS_Lib.Argument_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Num_Options     : constant Natural :=
        Standard.Ada.Strings.Fixed.Count (Gdb_Options, " ") + 1;
      Local_Arguments : Argument_List
                          (1 .. Debugger_Args'Length + Num_Options);
      First           : Natural := 1;
      Last            : Natural;

   begin
      Debugger.Window := Window;

      --  Cut each blank separated word into an argument.
      --  Note that we assume here that only one blank is put between each
      --  option (in the computation of Num_Options).

      for J in 1 .. Num_Options - 1 loop
         Last := Index (Gdb_Options (First .. Gdb_Options'Last), " ");
         Local_Arguments (J) := new String' (Gdb_Options (First .. Last - 1));
         First := Index_Non_Blank (Gdb_Options (Last .. Gdb_Options'Last));
      end loop;

      Local_Arguments (Num_Options) :=
        new String' (Gdb_Options (First .. Gdb_Options'Last));
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) :=
        Debugger_Args;

      if Debugger_Name = "" then
         General_Spawn
           (Debugger, Local_Arguments, Gdb_Command, Proxy, Remote_Host);
      else
         General_Spawn
           (Debugger, Local_Arguments, Debugger_Name, Proxy, Remote_Host);
      end if;

      Free (Debugger.Executable);
      Free (Debugger.Executable_Args);
      Free (Debugger.Target_Command);

      for J in 1 .. Num_Options loop
         Free (Local_Arguments (J));
      end loop;

      if Executable /= "" then
         Debugger.Executable := new String' (To_Unix_Pathname (Executable));
      end if;

      if Executable_Args /= "" then
         Debugger.Executable_Args := new String' (Executable_Args);
      end if;

      if Remote_Target = "" then
         Debugger.Remote_Target := False;
      else
         Debugger.Remote_Target := True;
         Debugger.Target_Command :=
           new String' ("target " & Remote_Protocol & " " & Remote_Target);
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      if Window /= null then
         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Language_Filter'Access, Language_Pattern);

         --  Set another filter to detect the cases when gdb asks questions,
         --  so that we can display dialogs.

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Question_Filter1'Access, Question_Filter_Pattern1);

         Add_Regexp_Filter
           (Convert (Window, Debugger),
            Question_Filter2'Access, Question_Filter_Pattern2);

         --  ??? Should avoid the duplication of this code between debugger-*

         if Main_Debug_Window_Access (Window).Debug_Mode then
            Add_Filter
              (Get_Descriptor (Debugger.Process).all,
               Output_Filter'Access, Output,
               Window.all'Address);
            Add_Filter
              (Get_Descriptor (Debugger.Process).all,
               Input_Filter'Access, Input,
               Window.all'Address);
         end if;

         if Main_Debug_Window_Access (Window).TTY_Mode then
            Add_Filter
              (Get_Descriptor (Debugger.Process).all,
               TTY_Filter'Access, Output,
               Debugger.Process.all'Address);
            Add_Filter
              (Get_Descriptor (Debugger.Process).all,
               TTY_Filter'Access, Input,
               Debugger.Process.all'Address);
         end if;
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Gdb_Debugger) is
      Num : Expect_Match;
   begin
      --  Wait for initial output and prompt (and display it in the window)
      Wait (Get_Process (Debugger), Num, "\(.+\).*$", Timeout => -1);

      --  Make sure that the prompt is what we are expecting.
      Send (Debugger, "set prompt (gvd) ", Mode => Internal);
      Send (Debugger, "set width 0", Mode => Internal);
      Send (Debugger, "set height 0", Mode => Internal);
      Send (Debugger, "set annotate 1", Mode => Internal);

      --  Connect to the remote target if needed.

      if Debugger.Target_Command /= null then
         if Debugger.Window = null then
            Send (Debugger, Debugger.Target_Command.all, Mode => Internal);
         else
            Output_Text
              (Convert (Debugger.Window, Debugger),
               Send (Debugger, Debugger.Target_Command.all, Mode => Internal) &
               ASCII.LF);
         end if;
      end if;

      --  Load the module to debug, if any.

      if Debugger.Executable /= null then
         Set_Executable (Debugger, Debugger.Executable.all, Mode => Internal);
      else
         --  Indicate that a new executable is present (even if there is none,
         --  we still need to reset some data).
         --  Do this before looking for the current file, since the explorer
         --  must also be initialized.
         --  No need to do anything in text-only mode

         if Debugger.Window /= null then
            Executable_Changed (Convert (Debugger.Window, Debugger), "");
         end if;

         --  Detect the current language. Note that most of the work is done
         --  in fact directly by Language_Filter.

         Send (Debugger, "show lang", Mode => Internal);

         --  Get the initial file name, so that we can display the appropriate
         --  file in the code editor.
         --  This should be done only after we have detected the current
         --  language, or no color highlighting will be provided.
         --
         --  Note that we need to send the "list" command first, otherwise
         --  info line will not work.

         Send (Debugger, "list", Mode => Internal);
         Send (Debugger, "info line", Mode => Internal);
      end if;

      if Debugger.Executable_Args /= null then
         Set_Args (Debugger, Debugger.Executable_Args.all, Mode => Hidden);
      end if;

   exception
      --  If the executable was not found, nothing to be done, we simply
      --  start with an empty debugger
      when Executable_Not_Found =>
         null;
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Gdb_Debugger) is
      Result : Expect_Match;
   begin
      --  In case the debugger was waiting for some input, or was busy
      --  processing a command.
      --  ??? Problem here if we were waiting on a user question (for instance
      --  in Start)
      Interrupt (Debugger);

      --  Make sure gdb will not complain if the file is being run
      Send (Debugger, "set confirm off");

      --  Now exit the debugger
      Send (Debugger, "quit", Wait_For_Prompt => False, Mode => Internal);

      --  Ensure that gdb is terminated before closing the pipes and trying to
      --  kill it abruptly.

      begin
         Wait (Get_Process (Debugger), Result, ".*", Timeout => 2);
      exception
         when Process_Died =>
            --  This is somewhat expected... RIP.
            null;
      end;

      Close (Get_Descriptor (Get_Process (Debugger)).all);
      Free (Debugger.Process);

      exception
         when Process_Died =>
            Close (Get_Descriptor (Get_Process (Debugger)).all);
            Free (Debugger.Process);
   end Close;

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
   -- Set_Executable --
   --------------------

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : String;
      Mode       : Invisible_Command := Hidden)
   is
      No_Such_File_Regexp : Pattern_Matcher :=
        Compile ("No such file or directory.");
      --  Note that this pattern should work even when LANG isn't english
      --  because gdb does not seem to take into account this variable at all.

   begin
      Set_Is_Started (Debugger, False);

      if Debugger.Remote_Target then
         if Match
           (No_Such_File_Regexp,
            Send (Debugger, "load " & Executable, Mode => Mode)) /= 0
         then
            raise Executable_Not_Found;
         end if;
      else
         if Match
           (No_Such_File_Regexp,
            Send (Debugger, "file " & Executable, Mode => Mode)) /= 0
         then
            raise Executable_Not_Found;
         end if;
      end if;

      --  Report a change in the executable. This has to be done before we
      --  look for the current file and line, so that the explorer can be
      --  correctly updated.
      --  No need to do anything in text-only mode

      if Debugger.Window /= null then
         Executable_Changed (Convert (Debugger.Window, Debugger), Executable);
      end if;

      --  Detect the current language, and get the name and line of the
      --  initial file.

      Send (Debugger, "show lang", Mode => Internal);
      Send (Debugger, "list", Mode => Internal);
      Send (Debugger, "info line", Mode => Internal);
   end Set_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   procedure Load_Core_File
     (Debugger : access Gdb_Debugger;
      Core     : String;
      Mode     : Invisible_Command := Hidden) is
   begin
      Set_Is_Started (Debugger, False);
      Send (Debugger, "core " & Core, Mode => Mode);

      --  Detect the current language, and get the name and line of the
      --  current file.

      Send (Debugger, "show lang", Mode => Internal);
      Send (Debugger, "list", Mode => Internal);
      Send (Debugger, "info line", Mode => Internal);
   end Load_Core_File;

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
      Wait_User_Command (Debugger);
      Set_Is_Started (Debugger, True);

      --  Find the first frame containing source information to be as user
      --  friendly as possible, and also check whether attach was successful

      loop
         Set_Parse_File_Name (Get_Process (Debugger), False);

         declare
            Str : constant String :=
              Send (Debugger, "up", Mode => Internal);
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

   ---------
   -- Run --
   ---------

   procedure Run
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger, "run " & Arguments, Mode => Mode);
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
      Send
        (Debugger,
         Start (Language_Debugger_Access (Get_Language (Debugger)))
           & " " & Arguments, Mode => Mode);
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

   procedure Interrupt
     (Debugger        : access Gdb_Debugger;
      Wait_For_Prompt : Boolean := False) is
   begin
      Interrupt (Get_Descriptor (Get_Process (Debugger)).all);
      Set_Interrupted (Get_Process (Debugger));

      if Wait_For_Prompt then
         Wait_Prompt (Debugger);
      end if;
   end Interrupt;

   ------------------------
   -- Is_Context_Command --
   ------------------------

   function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean is
   begin
      return
        (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "thread")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "task")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "file")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "core")
        or else (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "attach")
        or else (Command'Length >= 15
          and then Command (Command'First .. Command'First + 14) =
            "add-symbol-file")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "load");
   end Is_Context_Command;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   function Is_Execution_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean is
   begin
      return    Command = "step"
        or else Command = "stepi"
        or else Command = "s"
        or else Command = "next"
        or else Command = "n"
        or else Command = "nexti"
        or else Command = "cont"
        or else Command = "c"
        or else Command = "finish"
        or else (Command'Length >= 3
          and then Command (Command'First .. Command'First + 2) = "run")
        or else (Command'Length >= 5
          and then Command (Command'First .. Command'First + 4) = "begin");
   end Is_Execution_Command;

   ----------------------
   -- Is_Break_Command --
   ----------------------

   function Is_Break_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean is
   begin
      return Looking_At (Command, Command'First, "break")
        or else Looking_At (Command, Command'First, "tbreak")
        or else Looking_At (Command, Command'First, "rbreak")
        or else Looking_At (Command, Command'First, "b ")
        or else Looking_At (Command, Command'First, "delete")
        or else Looking_At (Command, Command'First, "del ")
        or else Looking_At (Command, Command'First, "disable")
        or else Looking_At (Command, Command'First, "enable");
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
            Value (Len).Program_Counter := new String' ("");
         else
            Value (Len).Program_Counter :=
              new String' (S (Matched (3).First .. Matched (3).Last));
         end if;

         Value (Len).Subprogram :=
           new String' (S (Matched (4).First .. Matched (4).Last));

         if Matched (5) = No_Match then
            Value (Len).Source_Location := new String' ("");
         else
            Value (Len).Source_Location :=
              new String' (S (Matched (6).First .. Matched (6).Last));
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
      Mode      : Command_Type := Hidden) is
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
      File      : String;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
   is
      Str : constant String := Positive'Image (Line);
   begin
      if Temporary then
         Send (Debugger,
               "tbreak " & Base_File_Name (File)
               & ":" & Str (Str'First + 1 .. Str'Last),
               Mode => Mode);
      else
         Send (Debugger,
               "break " & Base_File_Name (File)
               & ':' & Str (Str'First + 1 .. Str'Last),
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
      Send
        (Debugger,
         Break_Exception (Language_Debugger_Access (Get_Language (Debugger)),
                          Name, Temporary, Unhandled),
         Mode => Mode);
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   procedure Break_Address
     (Debugger   : access Gdb_Debugger;
      Address    : String;
      Temporary  : Boolean := False;
      Mode : Command_Type := Hidden) is
   begin
      if Temporary then
         Send (Debugger, "tbreak *" & Address, Mode => Mode);
      else
         Send (Debugger, "break *" & Address, Mode => Mode);
      end if;
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   procedure Break_Regexp
     (Debugger   : access Gdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : Command_Type := Hidden) is
   begin
      if Temporary then
         raise Unknown_Command;
         --  Error ("Temporary regexp breakpoints not supported");
      else
         Send (Debugger, "rbreak " & Regexp, Mode => Mode);
      end if;
   end Break_Regexp;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Debugger : access Gdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "finish", Mode => Mode);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Gdb_Debugger)
      return Thread_Information_Array is
   begin
      return Parse_Thread_List
        (Language_Debugger_Access (Get_Language (Debugger)),
         Send
           (Debugger,
            Thread_List (Language_Debugger_Access (Get_Language (Debugger))),
            Mode => Internal));
   end Info_Threads;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   function Line_Contains_Code
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive) return Line_Kind
   is
      Line_String : String := Positive'Image (Line);
      --  Use a temporary variable to remove the leading space.

      S : constant String :=
        Send (Debugger, "info line "
              & Base_File_Name (File)
              & ':' &
              Line_String (Line_String'First + 1 .. Line_String'Last),
              Mode => Internal);
   begin
      if Index (S, "starts at address") /= 0 then
         return Have_Code;
      elsif Index (S, "out of range") /= 0 then
         return No_More_Code;
      else
         return No_Code;
      end if;
   end Line_Contains_Code;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Debugger        : access Gdb_Debugger;
      Wait_For_Prompt : Boolean := True) is
   begin
      Output_Text
        (Convert (Debugger.Window, Debugger),
         Send_Full (Debugger, "  ", Wait_For_Prompt => Wait_For_Prompt,
                    Mode => Internal),
         Is_Command => True,
         Set_Position => True);
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Debugger    : access Gdb_Debugger;
      Dir         : String;
      Mode        : Command_Type := Hidden) is
   begin
      Send (Debugger, "cd " & Dir, Mode => Mode);
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
      Start : Natural := Str'First;
      Matched : Match_Array (0 .. 3);
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
      First, Last : out Natural)
   is
      Matched : Match_Array (0 .. 1);
   begin
      Match (Frame_Pattern, Str, Matched);

      if Matched (1) /= No_Match then
         First := Matched (1).First;
         Last  := Matched (1).Last;
      else
         First := 0;
         Last  := 0;
      end if;
   end Found_Frame_Info;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Gdb_Debugger) return GVD.Types.String_Array
   is
      S         : constant String :=
        Send (Debugger, "info sources", Mode => Internal);
      Num_Files : Natural := 0;

   begin
      --  ??? Will fail on
      --  "home/briot/Ada/glide/glide_window_pkg-callbacks: \
      --     No such file or directory."
      --  which can be emitted as the result of "info sources"

      --  Count the number of files

      for J in S'Range loop
         if S (J) = ',' then
            Num_Files := Num_Files + 1;
         end if;
      end loop;

      --  Add two, since there are in fact two lists of files (already
      --  read, and to be read), that do not end with ','

      Num_Files := Num_Files + 2;

      declare
         Result : GVD.Types.String_Array (1 .. Num_Files);
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

            Start := Index;
            while Index <= S'Last
              and then S (Index) /= ','
              and then S (Index) /= ASCII.LF
            loop
               Index := Index + 1;
            end loop;

            if Index <= S'Last
              and then S (Start .. Index - 1) /= "<bad string table offset>"
            then
               Result (Num) := new String' (S (Start .. Index - 1));
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
      Context : constant Language_Debugger_Context :=
        Get_Language_Debugger_Context (Lang);
      Dim : Dimension;

   begin
      Repeat_Num := 1;

      -------------------
      -- Simple values --
      -------------------

      if Result'Tag = Simple_Type'Tag
        or else Result'Tag = Range_Type'Tag
        or else Result'Tag = Mod_Type'Tag
        or else Result'Tag = Enum_Type'Tag
      then
         if Type_Str /= "" then
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

         --  Skip the parenthesis contents if needed
         if Index <= Type_Str'Last and then Type_Str (Index) = '(' then
            declare
               Num : Natural := 1;
            begin
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
            end;
            Index := Index + 1;
         end if;

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
               begin
                  Index := Index + 1;
                  Parse_Cst_String (Type_Str, Index, Str);
                  Index := Index - 1;
               end;
            end if;

            Set_Value (Simple_Type (Result.all), Type_Str (Int .. Index - 1));
         end;

      -------------------
      -- String values --
      -------------------

      elsif Result'Tag = Array_Type'Tag
        and then Num_Dimensions (Array_Type (Result.all)) = 1
        and then Type_Str'Length /= 0
        and then Type_Str (Index) = '"'
      then
         Dim := Get_Dimensions (Array_Type (Result.all), 1);

         --  If the dimension was not known when parsing the type, we compute
         --  it directly from the value of the string

         if Dim.Last < Dim.First then
            declare
               Tmp : Natural := Index;
               S   : String (1 .. 0);
            begin
               Parse_Cst_String (Type_Str, Tmp, S);
               Dim.Last := Long_Integer (Tmp - Index) + Dim.First - 4;
            end;
         end if;

         declare
            S : String (1 .. Integer (Dim.Last - Dim.First + 1));
            Simple : Simple_Type_Access;

         begin
            Parse_Cst_String (Type_Str, Index, S);
            Simple := Simple_Type_Access
              (Get_Value (Array_Type (Result.all), Dim.First));
            if Simple = null then
               Simple := Simple_Type_Access (New_Simple_Type);
            end if;
            Set_Value (Simple.all, S);
            Set_Value (Item       => Array_Type (Result.all),
                       Elem_Value => Simple,
                       Elem_Index => Dim.First);
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
         --  which starts with the right character but is in fact an array type

         if Type_Str (Index) /= Context.Array_Start
           or else (Index + 5 <= Type_Str'Last
                    and then Type_Str (Index + 1 .. Index + 5) = "<ref>")
         then
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
            R : Record_Type_Access := Record_Type_Access (Result);
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

                     Internal_Parse_Value
                       (Lang, Type_Str, Index, V, Repeat_Num,
                        Parent => Result);
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
      Num_Breakpoints : Natural := 0;
      S : constant String :=
        Send (Debugger, "info breakpoints", Mode => Internal);
      Index : Natural := S'First;
      Tmp   : Natural;

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

      declare
         Br      : Breakpoint_Array (1 .. Num_Breakpoints);
         Num     : Natural := 1;
         Matched : Match_Array (0 .. 10);
      begin
         Index := S'First;
         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Num <= Num_Breakpoints loop
            Match (Breakpoint_Pattern, S (Index .. S'Last), Matched);

            if Matched (0) /= No_Match then
               Br (Num).Num :=
                 Integer'Value (S (Matched (1).First .. Matched (1).Last));

               if S (Matched (2).First) = 'b' then
                  Br (Num).The_Type := Breakpoint;
               else
                  Br (Num).The_Type := Watchpoint;
               end if;

               case S (Matched (3).First) is
                  when 'k' => Br (Num).Disposition := Keep;
                  when 'd' => Br (Num).Disposition := Disable;
                  when others => Br (Num).Disposition := Delete;
               end case;

               Br (Num).Enabled := S (Matched (4).First) = 'y';

               if Br (Num).The_Type = Breakpoint then
                  Br (Num).Address :=
                    new String'("0x"
                                & S (Matched (7).First .. Matched (7).Last));
               else
                  Br (Num).Expression :=
                    new String'(S (Matched (7).First .. Matched (7).Last));
               end if;

               --  Go to beginning of next line.
               --  If we don't have a new breackpoint, add the line to the
               --  information.

               Tmp := Matched (8).First;
               Index := Tmp;

            else
               Tmp := Index;
            end if;

            while Index <= S'Last
              and then not (S (Index) in '0' .. '9')
            loop
               Skip_To_Char (S, Index, ASCII.LF);
               Index := Index + 1;
            end loop;

            if Matched (0) /= No_Match then
               Br (Num).Info := new String'(S (Tmp .. Index - 2));
               Match (File_Name_In_Breakpoint, Br (Num).Info.all, Matched);

               if Matched (0) /= No_Match then
                  Br (Num).File := new String'
                    (Base_File_Name (Br (Num).Info
                                     (Matched (1).First .. Matched (1).Last)));
                  Br (Num).Line := Integer'Value
                    (Br (Num).Info (Matched (2).First .. Matched (2).Last));
               end if;

               Match (Exception_In_Breakpoint, Br (Num).Info.all, Matched);

               if Matched (0) /= No_Match then
                  Br (Num).Except := new String'
                    (Br (Num).Info (Matched (1).First .. Matched (1).Last));
               end if;

               Match (Subprogram_In_Breakpoint, Br (Num).Info.all, Matched);

               if Matched (0) /= No_Match then
                  --  When is this memory freed ???
                  Br (Num).Subprogram := new String'
                    (Br (Num).Info (Matched (1).First .. Matched (1).Last));
               end if;

               Num := Num + 1;
            end if;
         end loop;

         return Br;
      end;
   end List_Breakpoints;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   procedure Enable_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Integer;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden) is
   begin
      if Enable then
         Send (Debugger, "enable" & Integer'Image (Num), Mode => Mode);
      else
         Send (Debugger, "disable" & Integer'Image (Num), Mode => Mode);
      end if;
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   procedure Remove_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : Integer;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "delete" & Integer'Image (Num), Mode => Mode);
   end Remove_Breakpoint;

   ------------------------------
   -- Variable_Name_With_Frame --
   ------------------------------

   function Variable_Name_With_Frame
     (Debugger : access Gdb_Debugger;
      Var      : String) return String
   is
      Bt        : Backtrace_Array (1 .. 1);
      Len       : Natural;
      Name_Last : Natural;

   begin
      Name_Last := Var'First;
      Skip_To_Char (Var, Name_Last, ':');

      --  Is there already a block indication ? If yes, do nothing

      if Name_Last < Var'Last
        and then Var (Name_Last + 1) = ':'
      then
         return Var;
      end if;

      --  Else, extract the name of the block from the backtrace.
      --  ??? This should be done directly from the sources, since this would
      --  be closer to what the user would expect.

      Set_Parse_File_Name (Get_Process (Debugger), False);
      Parse_Backtrace_Info
        (Send (Debugger, "frame", Mode => Internal), Bt, Len);
      Set_Parse_File_Name (Get_Process (Debugger), True);

      if Len >= 1 then
         Name_Last := Bt (1).Subprogram'First;
         Skip_To_Blank (Bt (1).Subprogram.all, Name_Last);
         Name_Last := Name_Last - 1;

         return Bt (1).Subprogram (Bt (1).Subprogram'First .. Name_Last)
           & "::" & Var;
      else
         return Var;
      end if;
   end Variable_Name_With_Frame;

   ---------------------
   -- List_Exceptions --
   ---------------------

   function List_Exceptions
     (Debugger : access Gdb_Debugger)
     return GVD.Types.Exception_Array
   is
      S : String := Send (Debugger, "info exceptions", Mode => Internal);
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
         Arr : Exception_Array (1 .. Nums);
         Index : Natural := S'First;
         Num   : Natural := 1;
         Start : Natural;
      begin
         if Nums <= 0 then
            return Arr;
         end if;

         Skip_To_Char (S, Index, ASCII.LF);
         Index := Index + 1;

         while Index <= S'Last
           and then Num <= Nums
         loop
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
      Set_Parse_File_Name (Get_Process (Debugger), False);

      declare
         Str : constant String :=
           Send (Debugger, "info line " & File_Name & ":1",
                 Mode => Internal);
      begin
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
     (Debugger        : access Gdb_Debugger;
      Range_Start     : out Address_Type;
      Range_End       : out Address_Type;
      Range_Start_Len : out Natural;
      Range_End_Len   : out Natural;
      Code            : out GVD.Types.String_Access;
      Start_Address   : String := "";
      End_Address     : String := "")
   is
      Disassembled : constant String := Send
        (Debugger, "disassemble " & Start_Address & " " & End_Address,
         Mode => Internal);
      Tmp,
      Start_Index,
      End_Index   : Natural;

   begin
      Start_Index := Disassembled'First;
      Skip_To_Char (Disassembled, Start_Index, ASCII.LF);
      Start_Index := Start_Index + 1;

      End_Index := Disassembled'Last;
      Skip_To_Char (Disassembled, End_Index, ASCII.LF, Step => -1);
      End_Index := End_Index - 1;

      --  Gdb always return a leading and tailing line, which we don't want
      --  to return.

      Code := new String' (Disassembled (Start_Index .. End_Index));

      --  Always read the actual start and end address from the output of
      --  gdb, in case gdb didn't start disassembling at the exact location,

      Tmp := Start_Index;
      Skip_To_Char (Disassembled, Tmp, ' ');

      if Start_Index < Disassembled'Last
        and then Disassembled (Start_Index .. Start_Index + 1) = "0x"
      then
         Range_Start_Len := Tmp - Start_Index;
         Range_Start (1 .. Range_Start_Len) :=
           Disassembled (Start_Index .. Tmp - 1);
      else
         Range_Start_Len := 0;
      end if;

      --  Get the actual end address, in case the disassembled zone doesn't end
      --  exactly on End_Address.

      Skip_To_Char (Disassembled, End_Index, ASCII.LF, Step => -1);
      End_Index := End_Index + 1;
      Tmp := End_Index;
      Skip_To_Char (Disassembled, Tmp, ' ');

      if End_Index < Disassembled'Last
        and then Disassembled (End_Index .. End_Index + 1) = "0x"
      then
         Range_End_Len := Tmp - End_Index;
         Range_End (1 .. Range_End_Len) := Disassembled (End_Index .. Tmp - 1);
      else
         Range_End_Len := 0;
      end if;
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   procedure Get_Line_Address
     (Debugger        : access Gdb_Debugger;
      Line            : Natural;
      Range_Start     : out Address_Type;
      Range_End       : out Address_Type;
      Range_Start_Len : out Natural;
      Range_End_Len   : out Natural) is
   begin
      Set_Parse_File_Name (Get_Process (Debugger), False);

      declare
         S : constant String := Send
           (Debugger, "info line" & Natural'Image (Line), Mode => Internal);
         Matched : Match_Array (0 .. 2);

      begin
         Match (Address_Range_Pattern, S, Matched);

         if Matched (0) /= No_Match then
            Range_Start_Len := Matched (1).Last - Matched (1).First + 1;
            Range_Start (1 .. Range_Start_Len) :=
              S (Matched (1).First .. Matched (1).Last);

            Range_End_Len := Matched (2).Last - Matched (2).First + 1;
            Range_End (1 .. Range_End_Len) :=
              S (Matched (2).First .. Matched (2).Last);

         else
            Range_Start (1 .. 1) := " ";
            Range_End (1 .. 1) := " ";
            Range_Start_Len := 0;
            Range_End_Len := 0;
         end if;
      end;

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
      Result       : String (1 .. Size * 2);
      Image        : constant String := Integer'Image (Size);
      S            : constant String := Send
        (Debugger,
         "x/"
         & Image (Image'First + 1 .. Image'Last)
         & "bx " & Address, Mode => Internal);
      S_Index      : Integer := S'First + 2;
      Result_Index : Integer := 1;
   begin
      while S_Index <= S'Last loop
         --  Detect actual data : 0xXX right after an ASCII.HT.
         if S (S_Index) = '0' then
            if S (S_Index - 1) = ASCII.HT then
               Result (Result_Index .. Result_Index + 1)
                 := S (S_Index + 2 .. S_Index + 3);
               Result_Index := Result_Index + 2;
            end if;
         end if;

         --  Detect "Cannot access memory at ..."
         if S (S_Index) = 'n' then
            while Result_Index <= Result'Last loop
               Result (Result_Index) := '-';
               Result_Index := Result_Index + 1;
            end loop;
            S_Index := S'Last;
         end if;

         S_Index := S_Index + 1;
      end loop;
      return Result;
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
      Send (Debugger, "set {short_short_integer}" & Address & " = 0x" & Byte,
            Mode => Internal);
      Restore_Language (Debugger);
   end Put_Memory_Byte;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   function Get_Variable_Address
     (Debugger  : access Gdb_Debugger;
      Variable  : in String) return String
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
   -- Switch_Language --
   ---------------------

   procedure Switch_Language
     (Debugger : access Gdb_Debugger;
      Language : in String)
   is
      S           : String := Send (Debugger, "show lang", Mode => Internal);
      First_Index : Integer := S'First;
      End_Index   : Integer;
   begin
      if Debugger.Stored_Language /= null then
         Free (Debugger.Stored_Language);
      end if;

      Skip_To_Char (S, First_Index, '"');
      End_Index := First_Index + 1;

      while S (End_Index) /= '"'
        and then S (End_Index) /= ';'
      loop
         End_Index := End_Index + 1;
      end loop;

      Debugger.Stored_Language := new String'
        (S (First_Index + 1 .. End_Index - 1));

      Send (Debugger, "set lang " & Language);
   end Switch_Language;

   ----------------------
   -- Restore_Language --
   ----------------------

   procedure Restore_Language
     (Debugger : access Gdb_Debugger)
   is
   begin
      if Debugger.Stored_Language /= null then
         Send (Debugger, "set lang " & Debugger.Stored_Language.all);
      end if;
   end Restore_Language;

end Debugger.Gdb;

-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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
with GNAT.Regpat;       use GNAT.Regpat;
with GNAT.Expect;       use GNAT.Expect;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Gdb.Ada;  use Debugger.Gdb.Ada;
with Debugger.Gdb.C;    use Debugger.Gdb.C;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Process_Proxies;   use Process_Proxies;
with Odd.Process;       use Odd.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Unchecked_Conversion;
with Odd.Strings;       use Odd.Strings;
with Gtk.Window;        use Gtk.Window;
with Odd.Dialogs;       use Odd.Dialogs;
with Language.Debugger; use Language.Debugger;
with Generic_Values;    use Generic_Values;
with Ada.Tags;          use Ada.Tags;

package body Debugger.Gdb is

   use String_History;

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.

   Prompt_Length : constant := 6;
   --  Length of the prompt ("(gdb) ").

   Gdb_Command   : constant String := "gdb";
   --  Name of the command to launch gdb.

   Gdb_Options   : constant String := "-nw -q";
   --  Options always passed to gdb.
   --  Note that we assume that only one blank is put between each option.

   Highlight_Pattern : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window.

   File_Name_Pattern : constant Pattern_Matcher :=
     Compile (ASCII.SUB & ASCII.SUB
              & "(.+):(\d+):\d+:beg:0x[0-9a-f]+", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output.

   File_Name_Pattern2 : constant Pattern_Matcher :=
     Compile ("^([^:]+):(\d+): No such file or directory", Multiple_Lines);
   --  Second regexp used to detect when the current frame can not be displayed

   Language_Pattern : constant Pattern_Matcher := Compile
     ("^(The current source language is|Current language:) +" &
      """?(auto; currently )?([^""\s]+)", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger.

   Frame_Pattern : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))?$", Multiple_Lines);

   procedure Language_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address);
   --  Filter used to detect a change in the current language.

   procedure Question_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address);
   --  Filter used to detect questions from gdb.

   function To_Main_Debug_Window is new
     Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

   ---------------------
   -- Language_Filter --
   ---------------------

   procedure Language_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address)
   is
      Matched    : GNAT.Regpat.Match_Array (0 .. 3);
      Debugger   : Debugger_Access;
      Language   : Language_Access;

   begin
      Match (Language_Pattern, Str, Matched);

      if Matched (3) /= No_Match then
         Debugger := Convert
           (To_Main_Debug_Window (Window), Descriptor).Debugger;
         declare
            Lang : String := Str (Matched (3).First .. Matched (3).Last);
         begin
            if Lang = "ada" then
               Language := new Gdb_Ada_Language;
            elsif Lang = "c" then
               Language := new Gdb_C_Language;
            else
               pragma Assert (False, "Language not currently supported");
               raise Program_Error;
            end if;

            Set_Language (Debugger, Language);
            Set_Debugger (Language_Debugger_Access (Language),
                          Debugger.all'Access);
         end;
      end if;
   end Language_Filter;

   ---------------------
   -- Question_Filter --
   ---------------------

   procedure Question_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address)
   is
      function To_Window is new
        Unchecked_Conversion (System.Address, Gtk_Window);

      Dialog     : Question_Dialog_Access;
      Index      : Positive;
   begin
      --  Do we have a question ?

      if Str'Length > 4
        and then Str (Str'First .. Str'First + 3) = "[0] "
      then
         Index := Str'First;
         while Index < Str'Last loop
            if Str (Index) = ASCII.LF
              and then Str (Index + 1) = '>'
            then
               declare
                  Choices : Question_Array (1 .. 1000);
                  --  ??? This is an arbitrary hard-coded limit, that should
                  --  be enough. Might be nice to remove it though.

                  Num     : Natural := 1;
                  First   : Positive;
                  Last    : Positive := Str'First;
               begin
                  while Last < Index loop
                     --  Skips the choice number ("[n] ")
                     Last := Last + 4;
                     First := Last;
                     while Last < Index
                       and then Str (Last) /= ASCII.LF
                     loop
                        Last := Last + 1;
                     end loop;

                     Choices (Num).Choice :=
                       new String'(Natural'Image (Num - 1));
                     Choices (Num).Description :=
                       new String'(Str (First .. Last - 1));
                     Num := Num + 1;

                     Last := Last + 1;
                  end loop;

                  Gtk_New (Dialog,
                           To_Window (Window),
                           Convert (To_Main_Debug_Window (Window),
                                    Descriptor).Debugger,
                           Choices (1 .. Num - 1));
                  Show_All (Dialog);
               end;
               return;
            end if;
            Index := Index + 1;
         end loop;
      end if;
   end Question_Filter;

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Gdb_Debugger; Entity : String) return String is
   begin
      Send (Debugger, "ptype " & Entity);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         if S'Length > Prompt_Length
           and then S (S'First .. S'First + 5) = "type ="
         then
            return S (S'First + 7 .. S'Last - Prompt_Length);
         else
            return "";
         end if;
      end;
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String is
   begin
      Send (Debugger, "print " & Entity);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Index : Natural := S'First;
      begin

         --  The value is valid only if it starts with '$'

         if S (S'First) /= '$' then
            return "";
         end if;

         --  Skip the '$nn =' part
         Skip_To_Char (S, Index, '=');
         Index := Index + 1;

         return S (Index + 1 .. S'Last - Prompt_Length);
      end;
   end Value_Of;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String)
     return String
   is
   begin
      --  ??? Probably, this should be language-dependent.

      Send (debugger, "print &(" & Entity & ")");

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matched : Match_Array (0 .. 1);
      begin
         Match (" (0x[0-9a-zA-Z]+)", S, Matched);
         if Matched (1) /= No_Match then
            return S (Matched (1).First .. Matched (1).Last);
         end if;
      end;
      return "";
   end Get_Uniq_Id;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Executable      : String;
      Arguments       : GNAT.OS_Lib.Argument_List;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Num_Options     : Natural := Count (Gdb_Options, " ") + 1;
      Local_Arguments : Argument_List (1 .. Arguments'Length + Num_Options);
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
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) := Arguments;

      if Debugger_Name = "" then
         General_Spawn
           (Debugger, Local_Arguments, Gdb_Command, Proxy, Remote_Host);
      else
         General_Spawn
           (Debugger, Local_Arguments, Debugger_Name, Proxy, Remote_Host);
      end if;

      Free (Debugger.Executable);
      Free (Debugger.Target_Command);

      if Executable /= "" then
         Debugger.Executable := new String' (Executable);
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

      if Proxy.all in Gui_Process_Proxy'Class then
         Add_Output_Filter
           (Get_Descriptor (Debugger.Process).all,
            Language_Filter'Access,
            Window.all'Address);

         --  Set another filter to detect the cases when gdb asks questions,
         --  so that we can display dialogs.

         Add_Output_Filter
           (Get_Descriptor (Debugger.Process).all,
            Question_Filter'Access,
            Window.all'Address);
      end if;

      Add_Output_Filter (Get_Descriptor (Debugger.Process).all,
                         Trace_Filter'Access);
      Add_Input_Filter (Get_Descriptor (Debugger.Process).all,
                        Trace_Filter'Access);
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Gdb_Debugger) is
   begin
      --  Make sure that the prompt is what odd is expecting.

      Push_Internal_Command_Status (Get_Process (Debugger), True);
      Send (Debugger, "set prompt (gdb) ", Wait_For_Prompt => False);
      Pop_Internal_Command_Status (Get_Process (Debugger));

      --  Wait for initial prompt (and display it in the window)
      Wait_Prompt (Debugger);
      Push_Internal_Command_Status (Get_Process (Debugger), True);

      Send (Debugger, "set width 0");
      Send (Debugger, "set height 0");
      Send (Debugger, "set annotate 1");

      --  We are missing one prompt, because we did not wait for hit after
      --  calling "set prompt"
      Wait_Prompt (Debugger);

      --  Connect to the remote target if needed.

      Pop_Internal_Command_Status (Get_Process (Debugger));

      if Debugger.Target_Command /= null then
         Send (Debugger, Debugger.Target_Command.all);
      end if;

      --  Load the module to debug, if any.

      if Debugger.Executable /= null then
         Set_Executable (Debugger, Debugger.Executable.all);
      else
         Push_Internal_Command_Status (Get_Process (Debugger), True);

         --  Detect the current language. Note that most of the work is done
         --  in fact directly by Language_Filter.

         Send (Debugger, "show lang");

         --  Get the initial file name, so that we can display the appropriate
         --  file in the code editor.
         --  This should be done only after we have detected the current
         --  language, or no color highlighting will be provided.

         Send (Debugger, "list");
         Send (Debugger, "info line");

         --  Make sure everything is hidden
         Pop_Internal_Command_Status (Get_Process (Debugger));
      end if;
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Gdb_Debugger) is
      Result : Expect_Match;
   begin
      Send (Debugger, "quit", Wait_For_Prompt => False);

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
   end Close;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : String) is
   begin
      if Debugger.Remote_Target then
         Send (Debugger, "load " & Executable);
      else
         Send (Debugger, "file " & Executable);
      end if;

      --  Detect the current language, and get the name and line of the
      --  initial file.
      Push_Internal_Command_Status (Get_Process (Debugger), True);
      Send (Debugger, "show lang");
      Send (Debugger, "list");
      Send (Debugger, "info line");
      Pop_Internal_Command_Status (Get_Process (Debugger));
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : access Gdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False) is
   begin
      Send (Debugger, "run", Display => Display);
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False)
   is
      Cmd   : String := Start (Get_Language (Debugger));
      First : Positive;
      Last  : Positive := Cmd'First;
   begin
      if Cmd /= "" then
         while Last <= Cmd'Last loop
            First := Last;
            while Last <= Cmd'Last
              and then Cmd (Last) /= ASCII.LF
            loop
               Last := Last + 1;
            end loop;

            Send (Debugger, Cmd (First .. Last - 1), Display => Display);
            Last := Last + 1;
         end loop;
      end if;
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False) is
   begin
      Send (Debugger, "step", Display => Display);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False) is
   begin
      Send (Debugger, "next", Display => Display);
   end Step_Over;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Debugger : access Gdb_Debugger;
      Display  : Boolean := False) is
   begin
      Send (Debugger, "cont", Display => Display);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Debugger : access Gdb_Debugger) is
   begin
      Interrupt (Get_Descriptor (Get_Process (Debugger)).all);
   end Interrupt;

   ------------------------
   -- Is_Context_Command --
   ------------------------

   function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean is
   begin
      return Is_Execution_Command (Debugger, Command)
        or else (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "thread")
        or else (Command'Length >= 4
          and then Command (Command'First .. Command'First + 3) = "task");
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
        or else Command = "next"
        or else Command = "nexti"
        or else Command = "cont"
        or else Command = "finish"
        or else Command = "begin"
        or else Command = "run";
   end Is_Execution_Command;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down (Debugger : access Gdb_Debugger;
                         Display  : Boolean := False) is
   begin
      Send (Debugger, "down", Display => Display);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up (Debugger : access Gdb_Debugger;
                       Display  : Boolean := False) is
   begin
      Send (Debugger, "up", Display => Display);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   procedure Stack_Frame
     (Debugger : access Gdb_Debugger;
      Frame    : Positive;
      Display  : Boolean := False)
   is
      Str : constant String := "frame" & Natural'Image (Frame - 1);
   begin
      Send (Debugger, Str, Display => Display);
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
   begin
      Send (Debugger, "where");

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matched : Match_Array (0 .. 6);
         First   : Positive := S'First;
      begin
         Len := 0;

         while Len /= Value'Length loop
            Match
              (Frame_Pattern, S (First .. S'Last - Prompt_Length), Matched);

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

            First := Matched (0).Last;
         end loop;
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : access Gdb_Debugger; Name : String) is
   begin
      Send (Debugger, "break " & Name);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger : access Gdb_Debugger;
      File     : String;
      Line     : Positive)
   is
      Str : constant String := Positive'Image (Line);
   begin
      Send (Debugger,
            "break " & Base_File_Name (File)
            & ':' & Str (Str'First + 1 .. Str'Last));
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False) is
   begin
      Send (Debugger,
            Break_Exception (Get_Language (Debugger), Name, Unhandled));
   end Break_Exception;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : access Gdb_Debugger;
                     Display  : Boolean := False) is
   begin
      Send (Debugger, "finish", Display => Display);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Gdb_Debugger)
      return Language.Thread_Information_Array is
   begin
      Send
        (Debugger,
         Thread_List (Get_Language (Debugger)));

      declare
         S : String := Expect_Out (Get_Process (Debugger));
      begin
         return Parse_Thread_List
           (Get_Language (Debugger), S (S'First .. S'Last - Prompt_Length));
      end;
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
   begin
      Send (Debugger, "info line "
            & Base_File_Name (File)
            & ':' &
            Line_String (Line_String'First + 1 .. Line_String'Last));
      if Index
        (Expect_Out (Get_Process (Debugger)), "starts at address") /= 0
      then
         return Have_Code;
      elsif Index
        (Expect_Out (Get_Process (Debugger)), "out of range") /= 0
      then
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

   procedure Display_Prompt (Debugger : access Gdb_Debugger) is
   begin
      Send (Debugger, "  ");
   end Display_Prompt;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural)
   is
      Matched : Match_Array (0 .. 2);
   begin
      Match (File_Name_Pattern, Str, Matched);

      First := Matched (0).First;
      Last  := Matched (0).Last;

      if Matched (0) = No_Match then

         --  Try another regexp
         Match (File_Name_Pattern2, Str, Matched);
         if Matched (0) = No_Match then
            Name_First := 0;
            Name_Last  := 1;
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
      Line       := Natural'Value
        (Str (Matched (2).First .. Matched (2).Last));
   end Found_File_Name;

   -----------------------
   -- Source_Files_List --
   -----------------------

   function Source_Files_List
     (Debugger : access Gdb_Debugger) return Odd.Types.String_Array is
   begin
      Send (Debugger, "info sources");

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Num_Files : Natural := 0;
      begin
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
            Result : Odd.Types.String_Array (1 .. Num_Files);
            Num    : Natural := 1;
            Index  : Positive := S'First;
            Start  : Positive;
         begin
            --  Parse first list (starts with ':')
            Skip_To_Char (S, Index, ':');
            Index := Index + 1;
            Skip_Blanks (S, Index);

            while Index <= S'Last loop
               --  Parse each file
               Start := Index;
               while Index <= S'Last
                 and then S (Index) /= ','
                 and then S (Index) /= ' '
                 and then S (Index) /= ASCII.LF
               loop
                  Index := Index + 1;
               end loop;

               if Index <= S'Last then
                  Result (Num) := new String'(S (Start .. Index - 1));
                  Num := Num + 1;

                  --  End of list ?
                  if S (Index) /= ',' then
                     Skip_To_Char (S, Index, ':');
                     Index := Index + 1;
                  else
                  Index := Index + 1;
                  end if;

                  Skip_Blanks (S, Index);
               end if;
            end loop;
            return Result (1 .. Num - 1);
         end;
      end;
   end Source_Files_List;

   --------------------------
   -- Internal_Parse_Value --
   --------------------------

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Values.Generic_Type_Access;
      Repeat_Num : out Positive;
      Parent     : Generic_Values.Generic_Type_Access)
   is
      Context : constant Language_Context := Get_Language_Context (Lang);
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

      elsif Result'Tag = Access_Type'Tag then

         --  Skip the parenthesis contents if needed
         if Index <= Type_Str'Last and then Type_Str (Index) = '(' then
            Skip_To_Char (Type_Str, Index, ')');
            Index := Index + 2;
         end if;

         declare
            Int : constant Natural := Index;
         begin
            Skip_Hexa_Digit (Type_Str, Index);
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
         declare
            Dim : Dimension := Get_Dimensions (Array_Type (Result.all), 1);
            S : String (1 .. Integer (Dim.Last - Dim.First + 1));
            Simple : Simple_Type_Access;

         begin
            Parse_Cst_String (Type_Str, Index, S);
            Simple := Simple_Type_Access
              (Get_Value (Array_Type (Result.all), 0));
            if Simple = null then
               Simple := Simple_Type_Access (New_Simple_Type);
            end if;
            Set_Value (Simple.all, S);
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

         if Type_Str (Index) /= Context.Array_Start then
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
                  --  part that defines it
                  Int := Index;
                  Skip_To_Char (Type_Str, Int, ' ');

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
            Internal_Parse_Value
              (Lang, Type_Str, Index, R, Repeat_Num, Parent => Result);
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

end Debugger.Gdb;

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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Process_Proxies;   use Process_Proxies;
with Odd.Process;       use Odd.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Unchecked_Conversion;

package body Debugger.Gdb is

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

   Gdb_Options   : constant String := "-nw -q -fullname";
   --  Options always passed to gdb.
   --  Note that we assume that only one blank is put between each option.

   Highlight_Pattern : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window.

   File_Name_Pattern : constant Pattern_Matcher :=
     Compile (ASCII.SUB & ASCII.SUB
              & "(.+):(\d+):\d+:beg:0x[0-9a-f]+", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output.

   Language_Pattern : constant Pattern_Matcher := Compile
     ("^(The current source language is|Current language:) +" &
      """?(auto; currently )?([^""\s]+)", Multiple_Lines);

   Frame_Pattern : constant Pattern_Matcher := Compile
     ("^#(\d+) +((0x[0-9a-f]+) in )?(.+) at (.+)$", Multiple_Lines);

   procedure Language_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address);
   --  Filter used to detect a change in the current language.

   ---------------------
   -- Language_Filter --
   ---------------------

   procedure Language_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address)
   is
      function To_Main_Debug_Window is new
        Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

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
               Language := new Gdb_Ada_Language;
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

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Gdb_Debugger; Entity : String) return String is
   begin
      Send (Get_Process (Debugger), "ptype " & Entity, Empty_Buffer => True);
      Wait_Prompt (Debugger);

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
      --  Empty the buffer.
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), "print " & Entity);
      Wait_Prompt (Debugger);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Index : Natural := S'First;
      begin

         --  Skip the '$nn =' part
         while Index <= S'Last
           and then S (Index) /= '='
         loop
            Index := Index + 1;
         end loop;
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

      --  Empty the buffer.
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), "print &(" & Entity & ")");
      Wait_Prompt (Debugger);

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

      Add_Output_Filter
        (Get_Descriptor (Debugger.Process).all,
         Language_Filter'Access,
         Window.all'Address);

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
      --  Wait for initial prompt (and display it in the window)
      Set_Internal_Command (Get_Process (Debugger), False);
      Wait_Prompt (Debugger);
      Set_Internal_Command (Get_Process (Debugger), True);

      Send (Get_Process (Debugger), "set prompt (gdb) ");
      Wait_Prompt (Debugger);
      Send (Get_Process (Debugger), "set width 0");
      Wait_Prompt (Debugger);
      Send (Get_Process (Debugger), "set height 0");
      Wait_Prompt (Debugger);
      Send (Get_Process (Debugger), "set annotate 1");
      Wait_Prompt (Debugger);

      --  Connect to the remote target if needed.

      if Debugger.Target_Command /= null then
         Send (Get_Process (Debugger), Debugger.Target_Command.all);
         Wait_Prompt (Debugger);
      end if;

      --  Load the module to debug, if any.

      if Debugger.Executable /= null then
         Set_Executable (Debugger, Debugger.Executable.all);
      end if;

      --  Detect the current language. Note that most of the work is done in
      --  fact directly by Language_Filter.

      Send (Get_Process (Debugger), "show lang");
      Wait_Prompt (Debugger);

      --  Get the initial file name, so that we can display the appropriate
      --  file in the code editor.
      --  This should be done only after we have detected the current language,
      --  or no color highlighting will be provided.

      Send (Get_Process (Debugger), "list");
      Wait_Prompt (Debugger);
      Send (Get_Process (Debugger), "info line");
      Wait_Prompt (Debugger);
      Set_Internal_Command (Get_Process (Debugger), False);
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Gdb_Debugger) is
      Result : Expect_Match;
   begin
      Send (Get_Process (Debugger), "quit");

      --  Ensure that gdb is terminated before closing the pipes and trying to
      --  kill it abruptly.

      Wait (Get_Process (Debugger), Result, ".*", Timeout => 2);
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
         Send (Get_Process (Debugger), "load " & Executable);
      else
         Send (Get_Process (Debugger), "file " & Executable);
      end if;

      Wait_Prompt (Debugger);
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

   procedure Run (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "run");
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "begin");
      Wait_Prompt (Debugger);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "step");
      Wait_Prompt (Debugger);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "next");
      Wait_Prompt (Debugger);
   end Step_Over;

   --------------
   -- Continue --
   --------------

   procedure Continue (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "cont");
      Wait_Prompt (Debugger);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Debugger : access Gdb_Debugger) is
   begin
      Interrupt (Get_Descriptor (Get_Process (Debugger)).all);
   end Interrupt;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "down");
      Wait_Prompt (Debugger);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "up");
      Wait_Prompt (Debugger);
   end Stack_Up;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
   begin
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), "where");
      Wait_Prompt (Debugger);

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matched : Match_Array (0 .. 5);
         First   : Positive := S'First;
      begin
         Len := 0;

         loop
            exit when Len = Value'Length;

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
            Value (Len).Source_Location :=
              new String' (S (Matched (5).First .. Matched (5).Last));
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
      Send (Get_Process (Debugger), "break " & Name);
      Wait_Prompt (Debugger);
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
      Send (Get_Process (Debugger),
        "break " & File & ':' & Str (Str'First + 1 .. Str'Last));
      Wait_Prompt (Debugger);
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False) is
   begin
      Send (Get_Process (Debugger),
            Break_Exception (Get_Language (Debugger), Name, Unhandled));
      Wait_Prompt (Debugger);
   end Break_Exception;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : access Gdb_Debugger) is
   begin
      Send (Get_Process (Debugger), "finish");
      Wait_Prompt (Debugger);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Gdb_Debugger)
      return Language.Thread_Information_Array is
   begin
      Empty_Buffer (Get_Process (Debugger));
      Send (Get_Process (Debugger), Thread_List (Get_Language (Debugger)));
      Wait_Prompt (Debugger);

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
      Line     : Positive) return Boolean
   is
      Line_String : String := Positive'Image (Line);
      --  Use a temporary variable to remove the leading space.

      Last        : Natural := File'Last;
      --  We have to use the basename for the file, since gdb does not
      --  recognize the full name.

   begin
      while Last >= File'First loop
         if File (Last) = GNAT.OS_Lib.Directory_Separator then
            exit;
         end if;
         Last := Last - 1;
      end loop;

      Send (Get_Process (Debugger), "info line "
            & File (Last + 1 .. File'Last)
            & ':' &
            Line_String (Line_String'First + 1 .. Line_String'Last));
      Wait_Prompt (Debugger);

      return Index
        (Expect_Out (Get_Process (Debugger)), "starts at address") /= 0;
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
      Send (Get_Process (Debugger), "  ");
      Wait_Prompt (Debugger);
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
         Name_First := 0;
         Name_Last  := 1;
         Line       := 0;
      else
         --  Skip the line feed character.

         if Last < Str'Last and then Str (Last + 1) = ASCII.LF then
            Last := Last + 1;
         end if;

         Name_First := Matched (1).First;
         Name_Last  := Matched (1).Last;
         Line       := Natural'Value
           (Str (Matched (2).First .. Matched (2).Last));
      end if;
   end Found_File_Name;

end Debugger.Gdb;

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
with Debugger.Jdb.Java; use Debugger.Jdb.Java;
with Process_Proxies;   use Process_Proxies;
with Gtk.Window;        use Gtk.Window;
with Odd.Process;       use Odd.Process;

package body Debugger.Jdb is

   use String_History;

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher :=
     Compile ("^(>|\w*\[\d+\]) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.

   Highlight_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("^(>|\w*\[\d+\]) ", Multiple_Lines);
   --  Match everything that should be highlighted in the debugger window.

   Frame_Pattern : constant Pattern_Matcher := Compile
     ("^ +\[(\d+)\] (.+) \((.+:\d+)?\), pc = (\d+)$", Multiple_Lines);

   Jdb_Command : constant String := "jdb";
   --  The default Jdb executable name.

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Jdb_Debugger; Entity : String) return String is
   begin
      Send (Debugger, "fields " & Entity);

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matches : Match_Array (0 .. 0);
      begin
         Match (Prompt_Regexp, S, Matches);
         return S (S'First .. Matches (0).First - 1);
      end;
   end Type_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Debugger : access Jdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Decimal) return String
   is
      Matches : Match_Array (0 .. 0);
   begin
      Send (Debugger, "dump " & Entity);

      declare
         S : String := Expect_Out (Get_Process (Debugger));
         Index : Natural := S'First;
      begin

         --  Skip the 'var =' part
         while Index <= S'Last
           and then S (Index) /= '='
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 1;

         Match (Prompt_Regexp, S, Matches);
         return S (Index + 1 .. Matches (0).First - 1);
      end;
   end Value_Of;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Debugger        : access Jdb_Debugger;
      Executable      : String;
      Arguments       : GNAT.OS_Lib.Argument_List;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") is
   begin
      Debugger.Window := Window;

      if Debugger_Name = "" then
         General_Spawn
           (Debugger, Arguments, Jdb_Command, Proxy, Remote_Host);
      else
         General_Spawn
           (Debugger, Arguments, Debugger_Name, Proxy, Remote_Host);
      end if;

      Free (Debugger.Main_Class);

      if Executable /= "" then
         Debugger.Main_Class := new String' (Executable);
      end if;

      Add_Output_Filter
        (Get_Descriptor (Debugger.Process).all,
         Trace_Filter'Access);
      Add_Input_Filter
        (Get_Descriptor (Debugger.Process).all,
         Trace_Filter'Access);
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Jdb_Debugger) is
      Language   : Language_Access;
   begin
      --  Wait for initial prompt
      Wait_Prompt (Debugger);
      Push_Internal_Command_Status (Get_Process (Debugger), True);

      if Debugger.Main_Class /= null then
         Set_Executable (Debugger, Debugger.Main_Class.all);
      end if;

      Language := new Jdb_Java_Language;
      Set_Language (Debugger, Language);
      Set_Debugger (Language_Debugger_Access (Language), Debugger.all'Access);
      Pop_Internal_Command_Status (Get_Process (Debugger));
   end Initialize;

   -----------
   -- Close --
   -----------

   procedure Close (Debugger : access Jdb_Debugger) is
      Result : Expect_Match;
   begin
      Send (Debugger, "quit", Wait_For_Prompt => False);

      --  Ensure that jdb is terminated before closing the pipes and trying to
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
     (Debugger : access Jdb_Debugger;
      Executable : String) is
   begin
      Send (Debugger, "load " & Executable);
   end Set_Executable;

   -----------------
   -- Wait_Prompt --
   -----------------

   procedure Wait_Prompt (Debugger : access Jdb_Debugger) is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run (Debugger : access Jdb_Debugger;
                  Display  : Boolean := False) is
   begin
      Send (Debugger, "run", Display => Display);
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start (Debugger : access Jdb_Debugger;
                    Display  : Boolean := False) is
   begin
      Send (Debugger, "run", Display => Display);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : access Jdb_Debugger;
                        Display  : Boolean := False) is
   begin
      Send (Debugger, "step", Display => Display);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : access Jdb_Debugger;
                        Display  : Boolean := False) is
   begin
      Send (Debugger, "next", Display => Display);
   end Step_Over;

   --------------
   -- Continue --
   --------------

   procedure Continue (Debugger : access Jdb_Debugger;
                       Display  : Boolean := False) is
   begin
      Send (Debugger, "cont", Display => Display);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Debugger : access Jdb_Debugger) is
   begin
      Send (Debugger, "suspend");
   end Interrupt;

   ------------------------
   -- Is_Context_Command --
   ------------------------

   function Is_Context_Command
     (Debugger : access Jdb_Debugger;
      Command  : String) return Boolean is
   begin
      return Is_Execution_Command (Debugger, Command)
        or else (Command'Length >= 6
          and then Command (Command'First .. Command'First + 5) = "thread");
   end Is_Context_Command;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   function Is_Execution_Command
     (Debugger : access Jdb_Debugger;
      Command : String) return Boolean is
   begin
      return    Command = "step"
        or else Command = "step up"
        or else Command = "stepi"
        or else Command = "next";
   end Is_Execution_Command;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down (Debugger : access Jdb_Debugger;
                         Display  : Boolean := False) is
   begin
      Send (Debugger, "down", Display => Display);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up (Debugger : access Jdb_Debugger;
                       Display  : Boolean := False) is
   begin
      Send (Debugger, "up", Display => Display);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   procedure Stack_Frame
     (Debugger : access Jdb_Debugger;
      Frame    : Positive;
      Display  : Boolean := False) is
   begin
      Send (Debugger, "frame" & Positive'Image (Frame), Display => Display);
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Jdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural) is
   begin
      Send (Debugger, "wherei");

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matched : Match_Array (0 .. 4);
         First   : Positive := S'First;
      begin
         Len := 0;

         while Len /= Value'Length loop
            Match
              (Frame_Pattern, S (First .. S'Last), Matched);

            exit when Matched (0) = No_Match;

            Len := Len + 1;
            Value (Len).Frame_Id :=
              Natural'Value (S (Matched (1).First .. Matched (1).Last));

            Value (Len).Program_Counter :=
              new String' (S (Matched (4).First .. Matched (4).Last));

            Value (Len).Subprogram :=
              new String' (S (Matched (2).First .. Matched (2).Last));

            if Matched (3) = No_Match then
               Value (Len).Source_Location := new String' ("");
            else
               Value (Len).Source_Location :=
                 new String' (S (Matched (3).First .. Matched (3).Last));
            end if;

            First := Matched (0).Last;
         end loop;
      end;
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger : access Jdb_Debugger; Name : String) is
   begin
      Send (Debugger, "stop in " & Name);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger : access Jdb_Debugger;
      File     : String;
      Line     : Positive)
   is
      Str : constant String := Positive'Image (Line);
      Pos : Positive;
   begin
      Pos := File'Last;

      --  Remove the extension from the filename to get an estimation of the
      --  class name.

      while Pos > File'First and then File (Pos) /= '.' loop
         Pos := Pos - 1;
      end loop;

      if File (Pos) = '.' then
         Pos := Pos - 1;
      else
         --  No file extension, assume a valid class name.
         Pos := File'Last;
      end if;

      Send (Debugger,
        "stop at " & File (File'First .. Pos) & ':' &
        Str (Str'First + 1 .. Str'Last));
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Jdb_Debugger;
      Name      : String  := "";
      Unhandled : Boolean := False) is
   begin
      if Unhandled then
         raise Unknown_Command;
      else
         Send (Debugger, "catch " & Name);
      end if;
   end Break_Exception;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : access Jdb_Debugger;
                     Display  : Boolean := False) is
   begin
      Send (Debugger, "step up", Display => Display);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Jdb_Debugger)
      return Language.Thread_Information_Array is
   begin
      Send (Debugger, Thread_List (Get_Language (Debugger)), True);

      declare
         S       : String := Expect_Out (Get_Process (Debugger));
         Matches : Match_Array (0 .. 0);
      begin
         Match (Prompt_Regexp, S, Matches);
         return Parse_Thread_List
           (Get_Language (Debugger), S (S'First .. Matches (0).First - 1));
      end;
   end Info_Threads;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   function Highlighting_Pattern
     (Debugger : access Jdb_Debugger) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   function Line_Contains_Code
     (Debugger : access Jdb_Debugger;
      File     : String;
      Line     : Positive) return Line_Kind is
   begin
      return No_More_Code;
   end Line_Contains_Code;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt (Debugger : access Jdb_Debugger) is
   begin
      Send (Debugger, "  ");
   end Display_Prompt;

end Debugger.Jdb;

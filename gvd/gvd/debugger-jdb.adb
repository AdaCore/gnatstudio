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

with GNAT.Regpat;       use GNAT.Regpat;

pragma Warnings (Off);
with GNAT.Expect;       use GNAT.Expect;
pragma Warnings (On);

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Jdb.Java; use Debugger.Jdb.Java;
with Process_Proxies;   use Process_Proxies;
with Gtk.Window;        use Gtk.Window;
with GVD.Process;       use GVD.Process;
with GVD.Trace;         use GVD.Trace;
with GVD.Types;         use GVD.Types;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;

package body Debugger.Jdb is

   use String_History;

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp : constant Pattern_Matcher :=
     Compile ("^(>|\w*\[(\d+)\]) ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.

   Highlight_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("^(>|\w*\[\d+\]) ", Multiple_Lines);
   --  Match everything that should be highlighted in the debugger window.

   Frame_Pattern : constant Pattern_Matcher := Compile
     ("^ +\[(\d+)\] (.+) \((.+:\d+)?\), pc = (\d+)$", Multiple_Lines);

   Source_Pattern : constant Pattern_Matcher := Compile
     ("\((.+):(\d+)?\)$", Multiple_Lines);

   Jdb_Command : constant String := "jdb";
   --  The default Jdb executable name.

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Debugger : access Jdb_Debugger; Entity : String) return String
   is
      S       : constant String := Send (Debugger, "fields " & Entity);
      Matches : Match_Array (0 .. 0);

   begin
      Match (Prompt_Regexp, S, Matches);
      return S (S'First .. Matches (0).First - 1);
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
      S       : constant String := Send (Debugger, "dump " & Entity);
      Index   : Natural := S'First;
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

      --  ??? Should avoid the duplication of this code

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
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Debugger : access Jdb_Debugger) is
      Language   : Language_Access;
   begin
      --  Wait for initial prompt
      Wait_Prompt (Debugger);

      if Debugger.Main_Class /= null then
         --  Set_Executable (Debugger, Debugger.Main_Class.all);
         null;
      else
         --  Indicate that a new executable is present (even if there is none,
         --  we still need to reset some data).
         if Debugger.Window /= null then
            Executable_Changed (Convert (Debugger.Window, Debugger), "");
         end if;
      end if;

      Language := new Jdb_Java_Language;
      Set_Language (Debugger, Language);
      Set_Debugger (Language_Debugger_Access (Language), Debugger.all'Access);
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
      Executable : String;
      Mode       : Invisible_Command := Internal) is
   begin
      Set_Is_Started (Debugger, False);
      Send (Debugger, "load " & Executable, Mode => Mode);
      Executable_Changed (Convert (Debugger.Window, Debugger), Executable);
   end Set_Executable;

   --------------------
   -- Attach_Process --
   --------------------

   procedure Attach_Process
     (Debugger : access Jdb_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden) is
   begin
      null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   procedure Detach_Process
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      null;
   end Detach_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   In_Wait : Boolean := False;
   --  ??? Temporary kludge to suppress when this unit is correctly implemented

   function Wait_Prompt
     (Debugger : access Jdb_Debugger;
      Timeout  : Integer) return Boolean
   is
      Num     : Expect_Match;
      Matches : Match_Array (0 .. 4);

   begin
      if In_Wait then
         return False;
      end if;

      In_Wait := True;

      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Matches, Timeout);

      if Matches (0) /= No_Match then
         Send (Debugger, "where", Wait_For_Prompt => False, Mode => Internal);
         Wait (Get_Process (Debugger), Num, Prompt_Regexp, Matches, -1);

         --  ??? Shouldn't be using Expect_Out here, but the functional version
         --  of Send.
         if Matches (2) /= No_Match then
            declare
               S : String := Expect_Out (Get_Process (Debugger));
            begin
               Debugger.Frame :=
                 Natural'Value (S (Matches (2).First .. Matches (2).Last));
            end;
         end if;

         In_Wait := False;
         return True;
      end if;

      In_Wait := False;
      return False;
   end Wait_Prompt;

   procedure Wait_Prompt (Debugger : access Jdb_Debugger) is
      Result : Boolean;
   begin
      Result := Wait_Prompt (Debugger, -1);
   end Wait_Prompt;

   ---------
   -- Run --
   ---------

   procedure Run
     (Debugger  : access Jdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden)
   is
   begin
      if Debugger.Main_Class /= null then
         Send (Debugger, "run " & Debugger.Main_Class.all, Mode => Mode);
      else
         Send (Debugger, "run", Mode => Mode);
      end if;

      Set_Is_Started (Debugger, True);
   end Run;

   -----------
   -- Start --
   -----------

   procedure Start
     (Debugger  : access Jdb_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Send (Debugger, "stop in " & Debugger.Main_Class.all & '.' &
         Debugger.Main_Class.all, Mode => Mode);
      Run (Debugger, "", Mode);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into (Debugger : access Jdb_Debugger;
                        Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over (Debugger : access Jdb_Debugger;
                        Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "next", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   procedure Step_Into_Instruction
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "stepi", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   procedure Step_Over_Instruction
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "stepi", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   procedure Continue (Debugger : access Jdb_Debugger;
                       Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "cont", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt
     (Debugger : access Jdb_Debugger;
      Wait_For_Prompt : Boolean := False) is
   begin
      Send (Debugger, "suspend", Wait_For_Prompt => Wait_For_Prompt);
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

   ----------------------
   -- Is_Break_Command --
   ----------------------

   function Is_Break_Command
     (Debugger : access Jdb_Debugger;
      Command : String) return Boolean
   is
   begin
      return Is_Execution_Command (Debugger, Command)
        or else (Command'Length >= 8
          and then Command (Command'First .. Command'First + 7) = "break at")
        or else (Command'Length >= 8
          and then Command (Command'First .. Command'First + 7) = "break in");
   end Is_Break_Command;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down (Debugger : access Jdb_Debugger;
                         Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "down", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up (Debugger : access Jdb_Debugger;
                       Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "up", Mode => Mode);
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   procedure Stack_Frame
     (Debugger : access Jdb_Debugger;
      Frame    : Positive;
      Mode     : Command_Type := Hidden)
   is
      Relative_Frame : Integer := Frame - Debugger.Frame;
   begin
      if Relative_Frame > 0 then
         Send (Debugger, "up" & Positive'Image (Relative_Frame),
               Mode => Mode);
      else
         Send (Debugger, "down" & Positive'Image (-Relative_Frame),
               Mode => Mode);
      end if;
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   procedure Backtrace
     (Debugger : access Jdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
      S       : String := Send (Debugger, "wherei", Mode => Internal);
      Matched : Match_Array (0 .. 6);
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
   end Backtrace;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Debugger  : access Jdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
   is
   begin
      Send (Debugger, "stop in " & Name, Mode => Mode);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger  : access Jdb_Debugger;
      File      : String;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
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
            Str (Str'First + 1 .. Str'Last),
            Mode => Mode);
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   procedure Break_Exception
     (Debugger  : access Jdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : Command_Type := Hidden) is
   begin
      if Unhandled then
         raise Unknown_Command;
      else
         Send (Debugger, "catch " & Name, Mode => Mode);
      end if;
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   procedure Break_Address
     (Debugger   : access Jdb_Debugger;
      Address    : String;
      Temporary  : Boolean := False;
      Mode       : Command_Type := Hidden)
   is
   begin
      raise Unknown_Command;
      --  Error ("Break on address not supported in jdb");
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   procedure Break_Regexp
     (Debugger   : access Jdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : Command_Type := Hidden)
   is
   begin
      raise Unknown_Command;
      --  Error ("Break on regular expression not support in jdb");
   end Break_Regexp;

   ------------
   -- Finish --
   ------------

   procedure Finish (Debugger : access Jdb_Debugger;
                     Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step up", Mode => Mode);
   end Finish;

   ------------------
   -- Info_Threads --
   ------------------

   function Info_Threads
     (Debugger : access Jdb_Debugger)
     return Language.Thread_Information_Array
   is
      S : String :=
        Send (Debugger, Thread_List (Get_Language (Debugger)), True,
              Mode => Internal);
      Matches : Match_Array (0 .. 0);
   begin
      Match (Prompt_Regexp, S, Matches);
      return Parse_Thread_List
        (Get_Language (Debugger), S (S'First .. Matches (0).First - 1));
   end Info_Threads;

   -----------------
   -- Info_Locals --
   -----------------

   function Info_Locals (Debugger : access Jdb_Debugger) return String is
   begin
      return "locals";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   function Info_Args (Debugger : access Jdb_Debugger) return String is
   begin
      raise Unknown_Command;
      return "";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   function Info_Registers (Debugger : access Jdb_Debugger) return String is
   begin
      raise Unknown_Command;
      return "";
   end Info_Registers;

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

   procedure Display_Prompt
     (Debugger        : access Jdb_Debugger;
      Wait_For_Prompt : Boolean := True)
   is
   begin
      Text_Output_Handler
        (Convert (Debugger.Window, Debugger),
         Send_Full (Debugger, "  ", Wait_For_Prompt => Wait_For_Prompt),
         Is_Command => True,
         Set_Position => True);
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Debugger    : access Jdb_Debugger;
      Dir         : String;
      Mode        : Command_Type := Hidden) is
   begin
      Send (Debugger, "cd " & Dir, Mode => Mode);
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name
     (Debugger    : access Jdb_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural)
   is
      Matched : Match_Array (0 .. 2);
   begin
      Match (Source_Pattern, Str, Matched);

      First := Matched (0).First;
      Last := Matched (0).Last;
      Addr_First := 0;
      Addr_Last  := 1;

      if Matched (0) = No_Match then
         Name_First := 0;
         Name_Last  := 1;
         Line       := 0;
         return;
      end if;

      Name_First := Matched (1).First;
      Name_Last  := Matched (1).Last;
      Line := Natural'Value (Str (Matched (2).First .. Matched (2).Last));

      loop
         Match (Source_Pattern, Str (Last + 1 .. Str'Last), Matched);
         exit when Matched (0) = No_Match;
         Last := Matched (0).Last;
      end loop;
   end Found_File_Name;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   function List_Breakpoints
     (Debugger  : access Jdb_Debugger)
     return GVD.Types.Breakpoint_Array
   is
      Br : GVD.Types.Breakpoint_Array (1 .. 0);
   begin
      --  Since jdb doesn't support enabling/disabling breakpoints, we should
      --  keep in the list all the breakpoints that have Enabled set to
      --  false, so that we can emulated that functionnality.
      return Br;
   end List_Breakpoints;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   procedure Enable_Breakpoint
     (Debugger : access Jdb_Debugger;
      Num      : Integer;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden) is
   begin
      null;
      --  ??? Enabling/disabling breakpoints will have to be emulated in jdb
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   procedure Remove_Breakpoint
     (Debugger : access Jdb_Debugger;
      Num      : Integer;
      Mode     : Command_Type := Hidden) is
   begin
      null;
   end Remove_Breakpoint;

   ----------
   -- Send --
   ----------

   function Send
     (Debugger        : access Jdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : Invisible_Command := Hidden) return String
   is
      S : constant String :=
        Send_Full (Debugger, Cmd, Empty_Buffer, Wait_For_Prompt, Mode);
      Index : Positive := S'Last;
   begin
      if S = "" then
         return "";
      else
         while Index >= S'First
           and then S (Index) /= ASCII.LF
         loop
            Index := Index - 1;
         end loop;

         return S (S'First .. Index - 1);
      end if;
   end Send;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   procedure Get_Machine_Code
     (Debugger        : access Jdb_Debugger;
      Range_Start     : out Address_Type;
      Range_End       : out Address_Type;
      Range_Start_Len : out Natural;
      Range_End_Len   : out Natural;
      Code            : out GVD.Types.String_Access;
      Start_Address   : String := "";
      End_Address     : String := "")
   is
      pragma Warnings (Off, Debugger);
      pragma Warnings (Off, Range_Start);
      pragma Warnings (Off, Range_End);
      pragma Warnings (Off, Start_Address);
      pragma Warnings (Off, End_Address);
   begin
      Range_Start (1 .. 1) := " ";
      Range_End (1 .. 1) := " ";
      Range_Start_Len := 0;
      Range_End_Len := 0;
      Code := null;
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   procedure Get_Line_Address
     (Debugger        : access Jdb_Debugger;
      Line            : Natural;
      Range_Start     : out Address_Type;
      Range_End       : out Address_Type;
      Range_Start_Len : out Natural;
      Range_End_Len   : out Natural) is
   begin
      Range_Start (1 .. 1) := " ";
      Range_End (1 .. 1) := " ";
      Range_Start_Len := 0;
      Range_End_Len := 0;
   end Get_Line_Address;

   ----------------
   -- Get_Memory --
   ----------------

   function Get_Memory
     (Debugger : access Jdb_Debugger;
      Size     : in Integer;
      Address  : in String) return String
   is
   begin
      return "";
      --  ??? Must implement this function !
   end Get_Memory;

   ---------------------
   -- Put_Memory_Byte --
   ---------------------

   procedure Put_Memory_Byte
     (Debugger : access Jdb_Debugger;
      Address  : in String;
      Byte     : in String)
   is
   begin
      null;
      --  ??? Must implement this function !
   end Put_Memory_Byte;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   function Get_Variable_Address
     (Debugger  : access Jdb_Debugger;
      Variable  : in String) return String
   is
   begin
      return "";
      --  ??? Must implement this function !
   end Get_Variable_Address;

end Debugger.Jdb;

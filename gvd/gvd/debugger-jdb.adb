-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with GNAT.Regpat;       use GNAT.Regpat;
with GNAT.Expect;       use GNAT.Expect;

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.Jdb.Java; use Debugger.Jdb.Java;
with Process_Proxies;   use Process_Proxies;
with Gtk.Window;        use Gtk.Window;
with GVD.Process;       use GVD.Process;
with GVD.Trace;         use GVD.Trace;
with GVD.Types;         use GVD.Types;
with GVD.Main_Window;   use GVD.Main_Window;
with File_Utils;        use File_Utils;
with VFS;               use VFS;

package body Debugger.Jdb is

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
      Format   : Value_Format := Default_Format) return String
   is
      pragma Unreferenced (Format);
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

   ---------------------
   -- Print_Value_Cmd --
   ---------------------

   function Print_Value_Cmd
     (Debugger : access Jdb_Debugger;
      Entity   : String) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "dump " & Entity;
   end Print_Value_Cmd;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Debugger        : access Jdb_Debugger;
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
      pragma Unreferenced (Remote_Protocol, Executable_Args, Remote_Target);
   begin
      Debugger.Window := Window;

      if Debugger_Name = "" then
         General_Spawn
           (Debugger, Debugger_Args, Jdb_Command, Proxy, Remote_Host);
      else
         General_Spawn
           (Debugger, Debugger_Args, Debugger_Name, Proxy, Remote_Host);
      end if;

      Free (Debugger.Main_Class);

      if Executable /= "" then
         Debugger.Main_Class := new String'(Executable);
      end if;

      --  ??? Should avoid the duplication of this code with debugger-*

      if GVD_Main_Window (Window).Debug_Mode then
         Add_Filter
           (Get_Descriptor (Debugger.Process).all,
            Output_Filter'Access, Output,
            Window.all'Address);
         Add_Filter
           (Get_Descriptor (Debugger.Process).all,
            Input_Filter'Access, Input,
            Window.all'Address);
      end if;

      if GVD_Main_Window (Window).TTY_Mode then
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
      Display_Prompt (Debugger);
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
         Wait (Get_Process (Debugger), Result, ".*", Timeout => 200);
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

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable
     (Debugger   : access Jdb_Debugger;
      Executable : String;
      Mode       : Command_Type := Hidden) is
   begin
      Set_Is_Started (Debugger, False);
      Send (Debugger, "load " & Executable, Mode => Mode);

      if Is_Absolute_Path_Or_URL (Executable) then
         Debugger.Executable := Create (Full_Filename => Executable);
      else
         Debugger.Executable := Create_From_Base (Executable);
      end if;

      if Debugger.Window /= null then
         Executable_Changed (Convert (Debugger.Window, Debugger), Executable);
      end if;
   end Set_Executable;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable
     (Debugger : access Jdb_Debugger) return VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   procedure Load_Core_File
     (Debugger : access Jdb_Debugger;
      Core     : String;
      Mode     : Command_Type := Hidden)
   is
      pragma Unreferenced (Debugger, Core, Mode);
   begin
      null;
   end Load_Core_File;

   -----------------
   -- Add_Symbols --
   -----------------

   procedure Add_Symbols
     (Debugger : access Jdb_Debugger;
      Module   : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Module, Mode);
   begin
      null;
   end Add_Symbols;

   --------------------
   -- Attach_Process --
   --------------------

   procedure Attach_Process
     (Debugger : access Jdb_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden)
   is
      pragma Unreferenced (Debugger, Process, Mode);
   begin
      null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   procedure Detach_Process
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden)
   is
      pragma Unreferenced (Debugger, Mode);
   begin
      null;
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   procedure Kill_Process
     (Debugger : access Jdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Mode);
   begin
      null;
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   function Wait_Prompt
     (Debugger : access Jdb_Debugger;
      Timeout  : Integer) return Boolean
   is
      Num     : Expect_Match;
      Matches : Match_Array (0 .. 4);

   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Matches, Timeout);

      if Matches (0) /= No_Match then
         if Command_In_Process (Get_Process (Debugger)) then
            return True;
         end if;

         declare
            S : constant String :=
              Send_Full (Debugger, "where", Mode => Internal);
         begin
            Match (Prompt_Regexp, S, Matches);

            if Matches (2) /= No_Match then
               Debugger.Frame :=
                 Natural'Value (S (Matches (2).First .. Matches (2).Last));
            end if;
         end;

         return True;
      end if;

      return False;
   end Wait_Prompt;

   procedure Wait_Prompt (Debugger : access Jdb_Debugger) is
      Result : Boolean;
      pragma Unreferenced (Result);
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
      pragma Unreferenced (Arguments);
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
      Mode      : Command_Type := Hidden)
   is
      pragma Unreferenced (Arguments);
   begin
      Send
        (Debugger, "stop in " & Debugger.Main_Class.all & '.' &
         Debugger.Main_Class.all,
         Mode => Mode);
      Send (Debugger, "run " & Debugger.Main_Class.all, Mode => Mode);
      Set_Is_Started (Debugger, True);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   procedure Step_Into
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   procedure Step_Over
     (Debugger : access Jdb_Debugger;
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

   procedure Continue
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "cont", Mode => Mode);
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
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Command'Length >= 6
        and then Command (Command'First .. Command'First + 5) = "thread";
   end Is_Context_Command;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   function Is_Execution_Command
     (Debugger : access Jdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return    Command = "step"
        or else Command = "step up"
        or else Command = "stepi"
        or else Command = "next"
        or else Command = "run";
   end Is_Execution_Command;

   ---------------------
   -- Is_Load_Command --
   ---------------------

   function Is_Load_Command
     (Debugger : access Jdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Command'Length >= 4
        and then Command (Command'First .. Command'First + 3) = "load";
   end Is_Load_Command;

   ----------------------
   -- Is_Break_Command --
   ----------------------

   function Is_Break_Command
     (Debugger : access Jdb_Debugger;
      Command  : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Command'Length >= 6
        and then Command (Command'First .. Command'First + 5) = "break ";
   end Is_Break_Command;

   ----------------
   -- Stack_Down --
   ----------------

   procedure Stack_Down
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "down", Mode => Mode);
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   procedure Stack_Up
     (Debugger : access Jdb_Debugger;
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
      Relative_Frame : constant Integer := Frame - Debugger.Frame;
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
      S       : constant String := Send (Debugger, "wherei", Mode => Internal);
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
           new String'(S (Matched (4).First .. Matched (4).Last));

         Value (Len).Subprogram :=
           new String'(S (Matched (2).First .. Matched (2).Last));

         if Matched (3) = No_Match then
            Value (Len).Source_Location := new String'("");
         else
            Value (Len).Source_Location :=
              new String'(S (Matched (3).First .. Matched (3).Last));
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
      pragma Unreferenced (Temporary);
   begin
      Send (Debugger, "stop in " & Name, Mode => Mode);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Debugger  : access Jdb_Debugger;
      File      : VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
   is
      pragma Unreferenced (Temporary);
      Str : constant String := Positive'Image (Line);
      Pos : Positive;
      Base : constant String := Base_Name (File);

   begin
      Pos := Base'Last;

      --  Remove the extension from the filename to get an estimation of the
      --  class name.

      while Pos > Base'First and then Base (Pos) /= '.' loop
         Pos := Pos - 1;
      end loop;

      if Base (Pos) = '.' then
         Pos := Pos - 1;
      else
         --  No file extension, assume a valid class name.
         Pos := Base'Last;
      end if;

      Send (Debugger,
            "stop at " & Base (Base'First .. Pos) & ':' &
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
      Mode      : Command_Type := Hidden)
   is
      pragma Unreferenced (Temporary);
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
      pragma Unreferenced (Debugger, Address, Temporary, Mode);
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
      pragma Unreferenced (Debugger, Regexp, Temporary, Mode);
   begin
      raise Unknown_Command;
      --  Error ("Break on regular expression not support in jdb");
   end Break_Regexp;

   ------------------------------
   -- Set_Breakpoint_Condition --
   ------------------------------

   procedure Set_Breakpoint_Condition
     (Debugger  : access Jdb_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Num, Condition, Mode);
   begin
      null;
   end Set_Breakpoint_Condition;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   procedure Set_Breakpoint_Command
     (Debugger : access Jdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Num, Commands, Mode);
   begin
      null;
   end Set_Breakpoint_Command;

   ---------------------------------
   -- Set_Breakpoint_Ignore_Count --
   ---------------------------------

   procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Jdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Num, Count, Mode);
   begin
      null;
   end Set_Breakpoint_Ignore_Count;

   ----------------------
   -- Set_Scope_Action --
   ----------------------

   procedure Set_Scope_Action
     (Debugger : access Jdb_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, Num, Scope, Action, Mode);
   begin
      null;
   end Set_Scope_Action;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Debugger : access Jdb_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Send (Debugger, "step up", Mode => Mode);
   end Finish;

   -----------------
   -- Task_Switch --
   -----------------

   procedure Task_Switch
     (Debugger : access Jdb_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Thread_Switch (Debugger, Task_Num, Mode);
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   procedure Thread_Switch
     (Debugger : access Jdb_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Send (Debugger, "thread" & Natural'Image (Thread), Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   procedure PD_Switch
     (Debugger : access Jdb_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Debugger, PD, Mode);
   begin
      null;
   end PD_Switch;

   ----------------
   -- Info_Tasks --
   ----------------

   procedure Info_Tasks
     (Debugger : access Jdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural) is
   begin
      Info_Threads (Debugger, Info, Len);
   end Info_Tasks;

   ------------------
   -- Info_Threads --
   ------------------

   procedure Info_Threads
     (Debugger : access Jdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      S       : constant String :=
        Send (Debugger, "threads", Mode => Internal);
      Matches : Match_Array (0 .. 0);

   begin
      Match (Prompt_Regexp, S, Matches);
      Info (Info'First) := (Num_Fields => 1, Information => (1 => Null_Ptr));
      Len := 0;
      --  Not implemented ???
   end Info_Threads;

   --------------
   -- Info_PD --
   --------------

   procedure Info_PD
      (Debugger : access Jdb_Debugger;
       Info     : out PD_Information_Array;
       Len      : out Natural)
   is
      pragma Unreferenced (Debugger);
   begin
      Info (Info'First) := (Num_Fields => 1, Information => (1 => Null_Ptr));
      Len := 0;
   end Info_PD;

   ----------------
   --  Info_WTX  --
   ----------------

   procedure Info_WTX
     (Debugger : access Jdb_Debugger;
      Version  : out Natural)
   is
      pragma Unreferenced (Debugger);
   begin
      Version := 0;
   end Info_WTX;

   -----------------
   -- Info_Locals --
   -----------------

   function Info_Locals (Debugger : access Jdb_Debugger) return String is
      pragma Unreferenced (Debugger);
   begin
      return "locals";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   function Info_Args (Debugger : access Jdb_Debugger) return String is
      pragma Unreferenced (Debugger);
   begin
      raise Unknown_Command;
      return "";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   function Info_Registers (Debugger : access Jdb_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      raise Unknown_Command;
      return "";
   end Info_Registers;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   function Highlighting_Pattern
     (Debugger : access Jdb_Debugger) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt (Debugger : access Jdb_Debugger) is
   begin
      Output_Text
        (Convert (Debugger.Window, Debugger),
         Send_Full (Debugger, "  "),
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
      pragma Unreferenced (Debugger);
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
      pragma Unreferenced (Debugger);
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
      Num      : Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden)
   is
      pragma Unreferenced (Debugger, Num, Mode, Enable);
   begin
      null;
      --  ??? Enabling/disabling breakpoints will have to be emulated in jdb
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   procedure Remove_Breakpoint
     (Debugger : access Jdb_Debugger;
      Num      : Breakpoint_Identifier;
      Mode     : Command_Type := Hidden)
   is
      pragma Unreferenced (Debugger, Num, Mode);
   begin
      null;
   end Remove_Breakpoint;

   ----------
   -- Send --
   ----------

   function Send
     (Debugger        : access Jdb_Debugger;
      Cmd             : String;
      Mode            : Invisible_Command := Hidden) return String
   is
      S     : constant String := Send_Full (Debugger, Cmd, Mode);
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
      Code            : out Basic_Types.String_Access;
      Start_Address   : String := "";
      End_Address     : String := "")
   is
      pragma Unreferenced (Debugger, Start_Address, End_Address);
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
      Range_End_Len   : out Natural)
   is
      pragma Unreferenced (Debugger, Line);
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
      pragma Unreferenced (Debugger, Size, Address);
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
      pragma Unreferenced (Debugger, Address, Byte);
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
      pragma Unreferenced (Debugger, Variable);
   begin
      return "";
      --  ??? Must implement this function !
   end Get_Variable_Address;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   function Get_Endian_Type
     (Debugger : access Jdb_Debugger) return Endian_Type
   is
      pragma Unreferenced (Debugger);
   begin
      return Unknown_Endian;
      --  ??? Must implement this function !
   end Get_Endian_Type;

   --------------
   -- Complete --
   --------------

   function Complete
     (Debugger  : access Jdb_Debugger;
      Beginning : String) return Basic_Types.String_Array
   is
      pragma Unreferenced (Debugger, Beginning);
      Result : Basic_Types.String_Array (1 .. 0);
   begin
      return Result;
   end Complete;

end Debugger.Jdb;

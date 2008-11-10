-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
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

with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

with Basic_Types;       use Basic_Types;
with Process_Proxies;   use Process_Proxies;
with GVD.Process;       use GVD.Process;
with GVD.Types;         use GVD.Types;
with String_Utils;      use String_Utils;
with Language;          use Language;
with Language.Debugger; use Language.Debugger;
with Debugger.VMS.Ada;  use Debugger.VMS.Ada;

package body Debugger.VMS is

   Prompt_Regexp : constant Pattern_Matcher :=
     Compile ("^DBG> ", Multiple_Lines);
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_Length : constant := 5;
   --  Length of the prompt ("DBG> ")

   Highlight_Pattern : constant Pattern_Matcher :=
     Compile ("^DBG> ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window

   File_Name_Pattern : constant Pattern_Matcher :=
     Compile ("^stepped to (.+)\.%LINE (\d+)$", Multiple_Lines);
   --  Matches a file name/line indication in debug's output

   function Temp_String (Temporary : Boolean) return String;
   --  Return temporary qualifier for breakpoints if temporary is
   --  true, a space otherwise.

   procedure Add_BP (Debugger : access VMS_Debugger; Expr : String);
   --  Add breakpoint in Debugger

   procedure Remove_BP
     (Debugger : access VMS_Debugger; Num : GVD.Types.Breakpoint_Identifier);
   --  Remove breakpoint from Debugger

   function Get_BP
     (Debugger : access VMS_Debugger;
      Num : GVD.Types.Breakpoint_Identifier) return String;
   --  Return given breakpoint from Debugger

   ------------
   -- Add_BP --
   ------------

   procedure Add_BP (Debugger : access VMS_Debugger; Expr : String) is
   begin
      for J in Debugger.Breakpoints'Range loop
         if Length (Debugger.Breakpoints (J).Expression) = 0 then
            Set_Unbounded_String (Debugger.Breakpoints (J).Expression, Expr);
         end if;
      end loop;
   end Add_BP;

   ---------------
   -- Remove_BP --
   ---------------

   procedure Remove_BP
     (Debugger : access VMS_Debugger; Num : GVD.Types.Breakpoint_Identifier) is
   begin
      Set_Unbounded_String
        (Debugger.Breakpoints (Integer (Num)).Expression, "");
   end Remove_BP;

   ------------
   -- Get_BP --
   ------------

   function Get_BP
     (Debugger : access VMS_Debugger;
      Num : GVD.Types.Breakpoint_Identifier) return String
   is
   begin
      return To_String (Debugger.Breakpoints (Integer (Num)).Expression);
   end Get_BP;

   -----------
   -- Spawn --
   -----------

   overriding
   procedure Spawn
     (Debugger        : access VMS_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      pragma Unreferenced
        (Debugger_Args, Window, Remote_Target, Remote_Protocol, Debugger_Name);

      Exec_Args : Argument_List_Access :=
        Argument_String_To_List (Executable_Args);
      Args : Argument_List (1 .. 2 + Exec_Args'Length);

   begin
      Args (1) := new String'("/debug");
      Args (2) := new String'(Executable.Base_Name);
      Args (3 .. Args'Last) := Exec_Args.all;
      Unchecked_Free (Exec_Args);

      Debugger.Executable := Executable;
      General_Spawn (Debugger, Kernel, Args, "run", Proxy);

      for J in Args'Range loop
         Free (Args (J));
      end loop;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Debugger : access VMS_Debugger) is
      Language : Language_Access;
   begin
      --  Wait for initial output and prompt
      Wait_Prompt (Debugger);
      Debugger.Execution_Window := False;
      Display_Prompt (Debugger);

      Language := new VMS_Ada_Language;
      Set_Debugger
        (Language_Debugger_Access (Language), Debugger.all'Access);
      Set_Language (Debugger, Language);
   end Initialize;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   overriding function Highlighting_Pattern
     (Debugger : access VMS_Debugger)
      return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Debugger : access VMS_Debugger) is
   begin
      if Get_Process (Debugger) /= null
        and then Get_Descriptor (Get_Process (Debugger)) /= null
        and then Get_Pid (Get_Descriptor (Get_Process (Debugger)).all) /=
          GNAT.Expect.Invalid_Pid
      then
         if Command_In_Process (Debugger.Process) then
            Interrupt (Debugger);
         end if;

         --  Now exit the debugger
         Send (Debugger, "quit", Wait_For_Prompt => False, Mode => Internal);
      end if;

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt
     (Debugger : access VMS_Debugger)
   is
      Num : Expect_Match;
      pragma Unreferenced (Num);
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   overriding function Wait_Prompt
     (Debugger : access VMS_Debugger;
      Timeout  : Integer)
      return Boolean
   is
      Num : Expect_Match;
   begin
      Wait (Get_Process (Debugger), Num, Prompt_Regexp, Timeout => Timeout);
      return Num /= Expect_Timeout;
   end Wait_Prompt;

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt
     (Debugger : access VMS_Debugger)
   is
      Proc : constant Visual_Debugger := Convert (Debugger);
   begin
      if Proc /= null then
         Output_Text
           (Proc, "DBG> ",
            Is_Command => False,
            Set_Position => True);
      end if;
   end Display_Prompt;

   -------------
   -- Type_Of --
   -------------

   overriding
   function Type_Of
     (Debugger : access VMS_Debugger;
      Entity   : String)
      return String
   is
      pragma Unreferenced (Debugger, Entity);
   begin
      --  ???
      return "";
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   overriding
   function Info_Locals
     (Debugger : access VMS_Debugger)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return "";
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   overriding function Info_Args
     (Debugger : access VMS_Debugger)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return "";
   end Info_Args;

   --------------------
   -- Info_Registers --
   --------------------

   overriding function Info_Registers
     (Debugger : access VMS_Debugger)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return "";
   end Info_Registers;

   --------------
   -- Value_Of --
   --------------

   subtype String_4 is String (1 .. 4);
   Fmt_Array : constant array (Value_Format) of String_4 :=
     (Default_Format => "    ",
      Decimal        => "/dec",
      Binary         => "/bin",
      Hexadecimal    => "/hex",
      Octal          => "/oct");
   --  Array used by Value_Of to print values in various formats

   overriding function Value_Of
     (Debugger : access VMS_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format)
      return String
   is
      S : constant String :=
        Send (Debugger, "evaluate" & Fmt_Array (Format) & ' ' & Entity,
              Mode => Internal);

   begin
      --  The value is valid only if it starts with '$'

      if S = ""
        or else (S'Length >= 7
                 and then S (S'First .. S'First + 6) = "%DEBUG-")
      then
         return "";
      end if;

      return S;
   end Value_Of;

   ---------------------
   -- Print_Value_Cmd --
   ---------------------

   overriding
   function Print_Value_Cmd
     (Debugger : access VMS_Debugger;
      Entity   : String)
      return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "evaluate " & Entity;
   end Print_Value_Cmd;

   ----------------------
   -- Change_Directory --
   ----------------------

   overriding
   procedure Change_Directory
     (Debugger    : access VMS_Debugger;
      Dir         : String;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      null;
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   overriding
   procedure Found_File_Name
     (Debugger    : access VMS_Debugger;
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
      Matched  : Match_Array (0 .. 2);
      Matched2 : Match_Array (0 .. 2);

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

      if Last < Str'Last and then Str (Last + 1) = ASCII.LF then
         Last := Last + 1;
      end if;

      Name_First := Matched (1).First;
      Name_Last  := Matched (1).Last;
      Addr_First := 0;
      Addr_Last  := 0;
      Line       := Natural'Value
        (Str (Matched (2).First .. Matched (2).Last));
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   overriding
   procedure Found_Frame_Info
     (Debugger    : access VMS_Debugger;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type)
   is
      pragma Unreferenced (Debugger, Str, Message);
   begin
      --  ???
      First := 0;
      Last := 0;
   end Found_Frame_Info;

   -----------------------
   -- Source_Files_List --
   -----------------------

   overriding
   function Source_Files_List
     (Debugger : access VMS_Debugger) return GNAT.Strings.String_List
   is
      S : constant String := Send (Debugger, "show module");
      pragma Unreferenced (S);
   begin
      --  Parse "show module" output ???
      return (1 .. 0 => <>);
   end Source_Files_List;

   --------------------
   -- Set_Executable --
   --------------------

   overriding
   procedure Set_Executable
     (Debugger   : access VMS_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Debugger.Executable := Executable;
      Send (Debugger, "set image " & Executable.Base_Name, Mode => Mode);
   end Set_Executable;

   --------------------
   -- Get_Executable --
   --------------------

   overriding function Get_Executable
     (Debugger : access VMS_Debugger)
      return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Debugger.Executable;
   end Get_Executable;

   --------------------
   -- Load_Core_File --
   --------------------

   overriding procedure Load_Core_File
     (Debugger : access VMS_Debugger;
      Core     : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      null;
   end Load_Core_File;

   -----------------
   -- Add_Symbols --
   -----------------

   overriding procedure Add_Symbols
     (Debugger : access VMS_Debugger;
      Module   : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      null;
   end Add_Symbols;

   ---------
   -- Run --
   ---------

   overriding procedure Run
     (Debugger  : access VMS_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Arguments);
   begin
      Send (Debugger, "go", Mode => Mode);
      Set_Is_Started (Debugger, True);
   end Run;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Debugger : access VMS_Debugger;
      Arguments : String := "";
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Arguments);
   begin
      Send (Debugger, "go", Mode => Mode);
      Set_Is_Started (Debugger, True);
   end Start;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access VMS_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   overriding procedure Detach_Process
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      null;
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      --  ???
      null;
   end Kill_Process;

   ---------------
   -- Step_Into --
   ---------------

   overriding procedure Step_Into
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      --  ???
      null;
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   overriding procedure Step_Over
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send (Debugger, "step/over", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   overriding procedure Step_Into_Instruction
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send (Debugger, "step/into/instruction", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   overriding procedure Step_Over_Instruction
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send (Debugger, "step/over/instruction", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send (Debugger, "go", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access VMS_Debugger) is
      Proxy      : constant Process_Proxy_Access := Get_Process (Debugger);
      Descriptor : constant Process_Descriptor_Access :=
                     Get_Descriptor (Proxy);

   begin
      Interrupt (Descriptor.all);
      Set_Interrupted (Proxy);
   end Interrupt;

   --------------------------
   -- Is_Execution_Command --
   --------------------------

   overriding function Is_Execution_Command
     (Debugger : access VMS_Debugger;
      Command  : String)
      return Boolean
   is
      pragma Unreferenced (Debugger, Command);
   begin
      --  ???
      return True;
   end Is_Execution_Command;

   ------------------------
   -- Is_Context_Command --
   ------------------------

   overriding function Is_Context_Command
     (Debugger : access VMS_Debugger;
      Command : String)
      return Boolean
   is
      pragma Unreferenced (Debugger, Command);
   begin
      --  ???
      return False;
   end Is_Context_Command;

   ---------------------
   -- Is_Load_Command --
   ---------------------

   overriding function Is_Load_Command
     (Debugger : access VMS_Debugger;
      Command  : String)
      return Boolean
   is
      pragma Unreferenced (Debugger, Command);
   begin
      --  ???
      return False;
   end Is_Load_Command;

   ----------------------
   -- Is_Break_Command --
   ----------------------

   overriding
   function Is_Break_Command
     (Debugger : access VMS_Debugger;
      Command : String)
      return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return Command'Length > 9
        and then Command (Command'First .. Command'First + 9) = "set break";
   end Is_Break_Command;

   ----------------
   -- Stack_Down --
   ----------------

   overriding procedure Stack_Down
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      --  ???
      null;
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      --  ???
      null;
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access VMS_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      --  ???
      null;
   end Stack_Frame;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send (Debugger, "step/return", Mode => Mode);
   end Finish;

   ---------------
   -- Backtrace --
   ---------------

   overriding
   procedure Backtrace
     (Debugger : access VMS_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
      pragma Unreferenced (Value);

      S : constant String := Send (Debugger, "show calls", Mode => Internal);
      pragma Unreferenced (S);
   begin
      --  ???
      Len := 0;
   end Backtrace;

   -----------------
   -- Temp_String --
   -----------------

   function Temp_String (Temporary : Boolean) return String is
   begin
      if Temporary then
         return "/temp ";
      else
         return " ";
      end if;
   end Temp_String;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   overriding procedure Break_Subprogram
     (Debugger  : access VMS_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send
        (Debugger, "set break" & Temp_String (Temporary) & Name, Mode => Mode);

      if not Temporary then
         Add_BP (Debugger, Name);
      end if;
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   overriding
   procedure Break_Source
     (Debugger  : access VMS_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      S : constant String :=
        File.Base_Name & "\%LINE " & Image (Line);
   begin
      Send
        (Debugger,
         "set break" & Temp_String (Temporary) & S,
         Mode => Mode);

      if not Temporary then
         Add_BP (Debugger, S);
      end if;
   end Break_Source;

   ---------------------
   -- Break_Exception --
   ---------------------

   overriding
   procedure Break_Exception
     (Debugger  : access VMS_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Name);
   begin
      if not Unhandled then
         Send
           (Debugger,
            "set break" & Temp_String (Temporary) & "/exception",
            Mode => Mode);

         if not Temporary then
            Add_BP (Debugger, "/exception");
         end if;
      end if;
   end Break_Exception;

   -------------------
   -- Break_Address --
   -------------------

   overriding
   procedure Break_Address
     (Debugger   : access VMS_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      S  : constant String := Address_To_String (Address);
      S2 : constant String := "%HEX " & S (S'First + 2 .. S'Last);
   begin
      Send
        (Debugger,
         "set break" & Temp_String (Temporary) & S2,
         Mode => Mode);

      if not Temporary then
         Add_BP (Debugger, S2);
      end if;
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   overriding procedure Break_Regexp
     (Debugger   : access VMS_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
   begin
      Send
        (Debugger,
         "set break" & Temp_String (Temporary) & Regexp, Mode => Mode);

      if not Temporary then
         Add_BP (Debugger, Regexp);
      end if;
   end Break_Regexp;

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   overriding procedure Enable_Breakpoint
     (Debugger : access VMS_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      S : constant String := Get_BP (Debugger, Num);
   begin
      if Enable then
         Send (Debugger, "activate break " & S, Mode => Mode);
      else
         Send (Debugger, "deactivate break " & S, Mode => Mode);
      end if;
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   overriding procedure Remove_Breakpoint
     (Debugger : access VMS_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      S : constant String := Get_BP (Debugger, Num);
   begin
      Send (Debugger, "cancel break " & S, Mode => Mode);
      Remove_BP (Debugger, Num);
   end Remove_Breakpoint;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   overriding
   function List_Breakpoints
     (Debugger  : access VMS_Debugger)
      return GVD.Types.Breakpoint_Array
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return (1 .. 0 => <>);
   end List_Breakpoints;

   ----------------------------
   -- Get_Last_Breakpoint_Id --
   ----------------------------

   overriding function Get_Last_Breakpoint_Id
     (Debugger  : access VMS_Debugger)
      return GVD.Types.Breakpoint_Identifier
   is
      pragma Unreferenced (Debugger);
   begin
      --  ???
      return 0;
   end Get_Last_Breakpoint_Id;

   -----------
   -- Watch --
   -----------

   overriding
   procedure Watch
     (Debugger  : access VMS_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      pragma Unreferenced (Trigger);
   begin
      if Condition = "" then
         Send (Debugger, "set watch " & Name, Mode => Mode);
      else
         Send
           (Debugger, "set watch " & Name & "when " & Condition, Mode => Mode);
      end if;
   end Watch;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger        : access VMS_Debugger;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type;
      Code            : out GNAT.Strings.String_Access;
      Start_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address     : GVD.Types.Address_Type := GVD.Types.Invalid_Address)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   overriding procedure Get_Line_Address
     (Debugger        : access VMS_Debugger;
      Line            : Natural;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Get_Line_Address;

   ----------------
   -- Get_Memory --
   ----------------

   overriding
   function Get_Memory
     (Debugger : access VMS_Debugger;
      Size     : Integer;
      Address  : String)
      return String
   is
      S : constant String := Send
        (Debugger,
         "dump/quadword " & Address & ":" & Address & "+" &
         Image (Size / 8), Mode => Internal);
      pragma Unreferenced (S);
   begin
      --  ??? parse S
      return "";
   end Get_Memory;

   ---------------------
   -- Put_Memory_Byte --
   ---------------------

   overriding procedure Put_Memory_Byte
     (Debugger : access VMS_Debugger;
      Address  : String;
      Byte     : String)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Put_Memory_Byte;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   overriding function Get_Variable_Address
     (Debugger  : access VMS_Debugger;
      Variable  : String)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return "";
   end Get_Variable_Address;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   overriding function Get_Endian_Type
     (Debugger : access VMS_Debugger) return Endian_Type
   is
      pragma Unreferenced (Debugger);
   begin
      return Little_Endian;
   end Get_Endian_Type;

   --------------
   -- Complete --
   --------------

   overriding function Complete
     (Debugger  : access VMS_Debugger;
      Beginning : String)
      return GNAT.Strings.String_List
   is
      pragma Unreferenced (Debugger, Beginning);
   begin
      return (1 .. 0 => <>);
   end Complete;

   ----------
   -- Send --
   ----------

   overriding
   function Send
     (Debugger        : access VMS_Debugger;
      Cmd             : String;
      Mode            : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String
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

end Debugger.VMS;

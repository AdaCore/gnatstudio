------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with GNAT.Expect;               use GNAT.Expect;
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with VSS.Strings.Conversions;

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Tribooleans;      use GNATCOLL.Tribooleans;

with Config;                    use Config;
with Default_Preferences;       use Default_Preferences;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Core_Kernels;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Trace;                 use GVD.Trace;
with GVD.Types;                 use GVD.Types;
with MI.Lexer;                  use MI.Lexer;
with Process_Proxies;           use Process_Proxies;
with Remote;                    use Remote;
with String_Utils;              use String_Utils;

with Language;                  use Language;
with Language.Debugger;         use Language.Debugger;
with Debugger.Base_Gdb.Ada;     use Debugger.Base_Gdb.Ada;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

package body Debugger.Base_Gdb.Gdb_MI is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.GDB_MI", On);

   ---------------
   -- Constants --
   ---------------

   Prompt_Str                : constant String := "^~"">""|^\(gdb\) ";
   Prompt_Regexp             : constant Pattern_Matcher :=
     Compile (Prompt_Str, Multiple_Lines);
   --  Regular expressions used to recognize the prompt.
   --  Note that this regexp needs to be as simple as possible, since it will
   --  be used several times when receiving long results from commands.

   Prompt_String             : constant String := "(gdb) ";
   --  The prompt used by the debugger

   Gdb_Command               : constant String := "gdb";
   --  Name of the command to launch gdb

   Gdb_Options               : constant String := "-nw -q --interpreter=mi";
   --  Options always passed to gdb

   Highlight_Pattern         : constant Pattern_Matcher :=
     Compile ("\(gdb\) ", Multiple_Lines);
   --  Matches everything that should be highlighted in the debugger window

   Breakpoint_Pattern        : constant Pattern_Matcher := Compile
     ("^(=breakpoint-|\^done,bkpt=|\^done,bkptno=)", Multiple_Lines);
   --  Pattern used to detect when breakpoints are created/deleted

   Breakpoint_Num_Pattern    : constant Pattern_Matcher := Compile
     ("(=breakpoint-created|\^done),(bkptno=""\d+"",)?bkpt={number=""(\d+)");
   --  Pattern to match the breakpoint number after we just created one.

   Stopped_Regexp            : constant Pattern_Matcher := Compile
     ("^\*stopped,", Multiple_Lines + Single_Line);
   --  Pattern used to detect when the debuggee stops

   Running_Regexp            : constant Pattern_Matcher := Compile
     ("^\*running,", Multiple_Lines);
   --  Pattern used to detect when the debuggee runs

   Terminate_Pattern         : constant Pattern_Matcher := Compile
     ("^=thread-group-exited", Multiple_Lines);
   --  Pattern used to detect when the debuggee terminates

   Not_Running_Pattern       : constant Pattern_Matcher := Compile
     ("^\^error,msg=""The program is not being run.", Multiple_Lines);
   --  Pattern used to detect when the debuggee is not running

   Language_Pattern          : constant Pattern_Matcher := Compile
     ("^(~[""]*The current source language is [\\]?|Current language:)" &
        "[""]*(auto; currently)?[\s]*([^"",\s,\\]+)?", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger

   Address_Range_Pattern     : constant Pattern_Matcher := Compile
     ("starts at address (0x[0-9a-f]+) <[^>]+> and ends at (0x[0-9a-f]+)");
   --  How to get the range of addresses for a given line

   Address_Pattern           : constant Pattern_Matcher := Compile
     ("(0x[0-9a-zA-Z]+)");

   Question_Filter_Pattern1  : constant Pattern_Matcher :=
     Compile ("^~""\[0\][\S\s]*^~"">", Multiple_Lines);

   Question_Filter_Pattern2  : constant Pattern_Matcher :=
     Compile ("^(.*\?) \(y or n\) ", Multiple_Lines);
   --  How to detect a question in gdb's output

   Number_Pattern            : constant Pattern_Matcher :=
     Compile ("^([0-9]+)");
   --  Used to determine position identifiers like in arrays

   String_Value_Pattern      : constant Pattern_Matcher :=
     Compile ("^\^done,value=""\[([0-9]+)\] \\""");

   No_Value_Pattern          : constant Pattern_Matcher :=
     Compile ("^\^done,value=""(\[([0-9]+)\]|{...}|)""");

   No_Get_Value_Pattern      : constant Pattern_Matcher :=
     Compile ("^\[([0-9]+)\]|{...}");

   Exception_In_Breakpoint   : constant Pattern_Matcher := Compile
     ("\b(on (exception ([-\w_:]+)|all|unhandled))" &
      "|((`([-\w_:]+)'|all|unhandled) Ada exception)");

   Error_Str                 : constant String := "^\^error,msg=";
   Error_Pattern             : constant Pattern_Matcher := Compile
     (Error_Str, Multiple_Lines);
   --  Pattern used to detect error

   Error_Or_Prompt_Pattern   : constant Pattern_Matcher := Compile
     (Prompt_Str & "|" & Error_Str, Multiple_Lines);
   --  Pattern used to detect error or prompt

   Continuation_Line_Pattern : constant Pattern_Matcher := Compile
     ("^ ?>$", Multiple_Lines);

   Is_Quit_Pattern : constant Pattern_Matcher := Compile
     ("^\s*(q|qui|quit|-gdb-exit)\s*$");
   --  'qu' can be quit or queue-signal

   procedure Breakpoint_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when a breakpoint is created/deleted

   procedure Terminate_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect when the program no longer runs

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);

   procedure Stopped_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_MI_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier;
   --  Send a command that sets a breakpoint, and retrieve the id of the newly
   --  created breakpoint

   procedure Set_Args
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden);
   --  Set the debuggee arguments to Arguments

   procedure Get_Register_Names
     (Debugger : access Gdb_MI_Debugger);
   --  Retrive names of registers

   function Get_Module (Executable : GNATCOLL.VFS.Virtual_File) return String;
   --  Return the name of the module contained in Executable
   --  Assume that the name of the module is the executable file
   --  with no path information and no extension.

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_MI_Debugger);
   --  Connect to the target if not already connected

   function Get_GDB_Version
     (Debugger : access Gdb_MI_Debugger) return Version_Number;
   --  Return the GDB version number.

   procedure Reset_State (Debugger : access Gdb_MI_Debugger);
   --  Reset some itnernal states.
   --  Typically caleld before sending a new command to the debugger.

   function Find_Identifier
     (C : Token_Lists.Cursor; Value : String) return Token_Lists.Cursor;
   --  Helper function to find the first Udentifier token starting from C
   --  named Value.

   function Is_Identifier
     (C : Token_Lists.Cursor; Value : String) return Boolean;
   --  Return True if element is identifier with given name

   procedure Next (C : in out Token_Lists.Cursor; N : Natural);
   --  Execute Next (C) N times

   function Strip_Escape (S : String) return String;
   --  Strip escape characters, e.g. replace \\ by \, \" by "

   function Get_Value (S : String) return String;

   function Parse_Frame_Info (Info : String) return Frame_Info;

   procedure Parse_Disassembled
     (S    : String;
      Code : out Disassemble_Elements);
   --  Parse result of disassemble request

   function Collect
     (C        : in out Token_Lists.Cursor;
      To       : Token_Code;
      Opposite : Token_Code) return String;
   --  Makes string from all elements till To considering nested elements
   --  started by opposite to To element.

   function Collect
     (Str      : String;
      To       : Character;
      Opposite : Character) return String;
   --  Makes string from all elements till To considering nested elements
   --  started by opposite to To element.

   ---------------------
   -- Find_Identifier --
   ---------------------

   function Find_Identifier
     (C : Token_Lists.Cursor; Value : String) return Token_Lists.Cursor
   is
      use Token_Lists;

      Cur  : Token_Lists.Cursor := C;
      Elem : MI.Lexer.Token_Type;

   begin
      while Has_Element (Cur) loop
         Elem := Element (Cur);

         if Elem.Code = Identifier
           and then Elem.Text /= null
           and then Elem.Text.all = Value
         then
            return Cur;
         end if;

         Next (Cur);
      end loop;

      return Cur;
   end Find_Identifier;

   -------------------
   -- Is_Identifier --
   -------------------

   function Is_Identifier
     (C : Token_Lists.Cursor; Value : String) return Boolean
   is
      use Token_Lists;
   begin
      return Element (C).Code = Identifier
        and then Element (C).Text /= null
        and then Element (C).Text.all = Value;
   end Is_Identifier;

   ----------
   -- Next --
   ----------

   procedure Next (C : in out Token_Lists.Cursor; N : Natural) is
      use Token_Lists;
   begin
      for J in 1 .. N loop
         Next (C);
      end loop;
   end Next;

   ------------------
   -- Strip_Escape --
   ------------------

   function Strip_Escape (S : String) return String is
      Result    : String (1 .. S'Length);
      J, Index  : Natural := 0;
   begin
      J := S'First;
      while J <= S'Last loop
         if S (J) = '\'
           and then J < S'Last
           and then S (J + 1) in '\' | '"'
         then
            J := J + 1;
         end if;

         Index := Index + 1;
         Result (Index) := S (J);
         J := J + 1;
      end loop;

      return Result (1 .. Index);
   end Strip_Escape;

   ---------------------
   -- Get_GDB_Version --
   ---------------------

   function Get_GDB_Version
     (Debugger : access Gdb_MI_Debugger) return Version_Number
   is
      Mode            : GVD.Types.Command_Type renames GVD.Types.Internal;
      Output          : Unbounded_String;
      Log_Output      : Unbounded_String;
      Debuggee_Output : Unbounded_String;
      Results_Output  : Unbounded_String;
   begin
      if Debugger.GDB_Version /= Unknown_Version then
         return Debugger.GDB_Version;
      end if;

      Debugger.Filter_Output
        (Mode,
         Debugger.Send_And_Get_Clean_Output ("-gdb-version", Mode => Mode),
         Console_Output  => Output,
         Log_Output      => Log_Output,
         Debuggee_Output => Debuggee_Output,
         Results_Output  => Results_Output);

      Debugger.GDB_Version := Parse_GDB_Version (To_String (Output));

      return Debugger.GDB_Version;
   end Get_GDB_Version;

   -----------------------
   -- Breakpoint_Filter --
   -----------------------

   procedure Breakpoint_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Gdb_MI_Debugger (Process.Debugger.all).Breakpoints_Changed := True;
   end Breakpoint_Filter;

   ----------------------
   -- Terminate_Filter --
   ----------------------

   procedure Terminate_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Str, Matched);
   begin
      Process.Debugger.Set_Is_Started (None);
   end Terminate_Filter;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Debugger : access Gdb_MI_Debugger) is
   begin
      Debugger.Breakpoints_Changed := False;
      Debugger.Current_Frame       := Null_Frame_Info;
   end Reset_State;

   -------------------------------
   -- Send_And_Get_Clean_Output --
   -------------------------------

   overriding function Send_And_Get_Clean_Output
     (Debugger    : access Gdb_MI_Debugger;
      Cmd         : String;
      Mode        : Command_Type := Hidden;
      Synchronous : Boolean := True) return String is
   begin
      Debugger.Reset_State;

      declare
         S   : constant String := Debugger.Send_And_Get_Output
           (Cmd, Mode, Synchronous);
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

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Debugger        : access Gdb_MI_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Reset_State;
      Debugger.Prepare_Target_For_Send (Cmd);

      --  Call the Parent procedure
      Debugger_Root (Debugger.all).Send
        (Cmd, Empty_Buffer, Wait_For_Prompt, Force_Send, Mode);
   end Send;

   -------------
   -- Type_Of --
   -------------

   overriding function Type_Of
     (Debugger : access Gdb_MI_Debugger; Entity : String) return String is
   begin
      --  If Entity contains a LF, this is an invalid entity, so give up
      --  immediately.

      for J in reverse Entity'Range loop
         if Entity (J) = ASCII.LF then
            return "";
         end if;
      end loop;

      Debugger.Detect_Language;

      declare
         Block : Process_Proxies.Parse_File_Switch
           (Debugger.Process) with Unreferenced;
         S    : constant String := Debugger.Send_And_Get_Clean_Output
           ("ptype " & Entity, Mode => Internal);
         Pos  : Integer := Index (S, "type = ");
         Last : Integer := S'Last;

      begin
         if Pos = 0 then
            return "";
         end if;
         Pos := Pos + 7;

         Skip_To_Char (S, Last, '"', -1);
         Last := Last - 3;

         declare
            Result : String (1 .. Last - Pos + 1) := (others => ' ');
            Len    : Natural := 0;
         begin
            while Pos <= Last loop
               if S (Pos) = ASCII.LF
                 or else S (Pos) = '"'
                 or else S (Pos) = '~'
               then
                  null;

               elsif S (Pos) = '\'
                 and then Pos <= Last
                 and then S (Pos + 1) = 'n'
               then
                  Pos := Pos + 1;
                  Len := Len + 1;
                  Result (Len) := ASCII.LF;

               elsif S (Pos) = ')'
                 and then Len > 0
                 and then Result (Len) = ' '
               then
                  Result (Len) := ')';

               else
                  Len := Len + 1;
                  Result (Len) := S (Pos);
               end if;
               Pos := Pos + 1;
            end loop;

            return Result (1 .. Len);
         end;
      end;
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   overriding function Info_Locals
     (Debugger : access Gdb_MI_Debugger)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("-stack-list-locals 1",
         Mode => GVD.Types.Internal);

      Pattern : constant Pattern_Matcher := Compile ("{name=""([^""]*)""?");
      From    : Integer := S'First;
      Matched : Match_Array (0 .. 1);
      Result  : VSS.String_Vectors.Virtual_String_Vector;
   begin
      while From <= S'Last loop
         Match (Pattern, S (From .. S'Last), Matched);
         exit when Matched (0) = No_Match;
         From := Matched (0).Last + 1;

         Result.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (S (Matched (1).First .. Matched (1).Last)));
      end loop;

      return Result;
   end Info_Locals;

   ---------------
   -- Info_Args --
   ---------------

   overriding function Info_Args
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-stack-list-arguments 1 0 0";
   end Info_Args;

   --------------
   -- Value_Of --
   --------------

   overriding function Value_Of
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format;
      From_API : Boolean := False)
      return String
   is
      use Nodes_Vectors;

      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

      Lang    : constant Language_Access := Debugger.Get_Language;
      Context : constant Language_Debugger_Context :=
        Get_Language_Debugger_Context (Language_Debugger_Access (Lang));

      V       : Variable;
      Matched : Match_Array (0 .. 1);

      function Fmt return String;
      function Data_Evaluate (Name : String) return String;
      function Var_Evaluate  (Name : String) return String;
      function Is_Command (Name : String) return Boolean;

      procedure Update_Value
        (Name  : String;
         Exp   : String;
         Nodes : in out Nodes_Vectors.Vector;
         Node  : Node_Access);

      ---------
      -- Fmt --
      ---------

      function Fmt return String is
      begin
         if Format = Decimal then
            return " -f decimal ";
         elsif Format = Binary then
            return " -f binary ";
         elsif Format = Hexadecimal then
            return " -f hexadecimal ";
         elsif Format = Octal then
            return " -f octal ";
         else
            return " ";
         end if;
      end Fmt;

      -------------------
      -- Data_Evaluate --
      -------------------

      function Data_Evaluate (Name : String) return String is
      begin
         return Get_Value
           (Debugger.Send_And_Get_Clean_Output
              ("-data-evaluate-expression " & '"' & Name & '"',
               Mode => Internal));
      end Data_Evaluate;

      ------------------
      -- Var_Evaluate --
      ------------------

      function Var_Evaluate (Name : String) return String is
      begin
         return Get_Value
           (Debugger.Send_And_Get_Clean_Output
              ("-var-evaluate-expression" & Fmt & Name,
               Mode => Internal));
      end Var_Evaluate;

      ------------------
      -- Update_Value --
      ------------------

      procedure Update_Value
        (Name  : String;
         Exp   : String;
         Nodes : in out Nodes_Vectors.Vector;
         Node  : Node_Access)
      is
         use Token_Lists;

         N   : Node_Access := Node;
         Idx : Natural     := 0;

      begin
         if N = null then
            declare
               S : constant String := Debugger.Send_And_Get_Clean_Output
                 ("-var-evaluate-expression" & Fmt & Name,
                  Mode => Internal);
            begin
               Match (String_Value_Pattern, S, Matched);
               if Matched (0) /= No_Match then
                  declare
                     --  cut "[X] " at begin of string
                     R   : constant String  := Get_Value (S);
                     Idx : constant Natural := Index (R, " ");
                  begin
                     N           := new Gdb_MI.Node (False);
                     N.Name      := To_Unbounded_String (Name);
                     N.Exp       := To_Unbounded_String (Exp);
                     N.Is_String := True;
                     N.Value     := To_Unbounded_String
                       (R (Idx + 1 .. R'Last));
                  end;

                  Append (Nodes, N);
                  return;
               end if;

               Match (No_Value_Pattern, S, Matched);
               if Matched (0) /= No_Match then
                  declare
                     Ch : constant String := Debugger.Send_And_Get_Clean_Output
                       ("-var-list-children " & Name, Mode => Internal);

                     Tokens : Token_List_Controller;
                     T      : Token_Lists.Cursor;
                     N_Name : Standard.Ada.Strings.Unbounded.String_Access;
                     N_Exp  : Standard.Ada.Strings.Unbounded.String_Access;

                  begin
                     Tokens.List := Build_Tokens (Ch);
                     T := Find_Identifier (First (Tokens.List), "child");

                     while T /= Token_Lists.No_Element loop
                        if N = null then
                           N      := new Gdb_MI.Node (True);
                           N.Name := To_Unbounded_String (Name);
                           N.Exp  := To_Unbounded_String (Exp);
                        end if;

                        Next (T, 3);
                        while Element (T).Code /= R_Brace loop -- '}'
                           if Is_Identifier (T, "name") then
                              Next (T, 2);
                              N_Name := Element (T).Text;

                           elsif Is_Identifier (T, "exp") then
                              Next (T, 2);
                              N_Exp := Element (T).Text;
                           end if;

                           Next (T);
                        end loop;

                        Update_Value (N_Name.all, N_Exp.all, N.Childs, null);

                        T := Find_Identifier (T, "child");
                     end loop;
                  end;

               else
                  N       := new Gdb_MI.Node (False);
                  N.Name  := To_Unbounded_String (Name);
                  N.Exp   := To_Unbounded_String (Exp);
                  N.Value := To_Unbounded_String (Get_Value (S));
               end if;

               if N /= null then
                  Append (Nodes, N);
               end if;
            end;

         else
            if Node.Has_Childs then
               for Item of Node.Childs loop
                  Update_Value
                    (To_String (Item.Name),
                     To_String (Item.Exp),
                     Node.Childs,
                     Item);
               end loop;

            else
               if N.Evaluate = Var then
                  declare
                     S : constant String := Get_Value
                       (Debugger.Send_And_Get_Clean_Output
                          ("-var-evaluate-expression" & Fmt &
                             To_String (N.Name),
                           Mode => Internal));
                  begin
                     if N.Is_String then
                        --  cut "[X] " at begin of string
                        Idx     := Index (S, " ");
                        N.Value := To_Unbounded_String (S (Idx + 1 .. S'Last));

                     else
                        N.Value := To_Unbounded_String (S);
                     end if;
                  end;
               else
                  N.Value := To_Unbounded_String
                    (Data_Evaluate (To_String (N.Name)));
               end if;
            end if;
         end if;
      end Update_Value;

      Result : Unbounded_String;

      ------------------
      -- Build_Result --
      ------------------

      procedure Build_Result (Node : Node_Access; Positions : Boolean);
      procedure Build_Result (Node : Node_Access; Positions : Boolean) is
         Pos   : Boolean := False;
         C     : Nodes_Vectors.Cursor;
         C1    : Nodes_Vectors.Cursor;
         V     : Unbounded_String;
         Count : Natural := 0;
      begin
         if Positions
           and then Context.Record_Field /= ""
         then
            Append
              (Result, To_String (Node.Exp) & " " &
                 Context.Record_Field & " ");
         end if;

         if Node.Has_Childs then
            if Length (Result) /= 0
              and then Element (Result, Length (Result)) /= ' '
            then
               Append (Result, " ");
            end if;

            Append (Result, Context.Record_Start);

            for Item of Node.Childs loop
               Match (Number_Pattern, To_String (Item.Exp), Matched);
               if Matched (0) = No_Match then
                  Pos := True;
                  exit;
               end if;
            end loop;

            C := Node.Childs.First;
            while Has_Element (C) loop
               if C = Node.Childs.First then
                  Build_Result (Element (C), True);
               else
                  Append (Result, ", ");
                  Build_Result (Element (C), Pos);
               end if;

               if Lang.Get_Name /= "c"
               --  GDB doesn't calculate repeats for C code in CLI mode
                 and then not Pos
                 and then not Element (C).Has_Childs
               then
                  --  go forvard until we have same value
                  V     := Element (C).Value;
                  Count := 0;
                  C1    := Next (C);
                  loop
                     Next (C);
                     exit when not Has_Element (C)
                       or else Element (C).Has_Childs
                       or else Element (C).Value /= V;
                     Count := Count + 1;
                  end loop;

                  if Count > 0 then
                     if Count < 4 then
                        C := C1;
                     else
                        Append (Result, " <repeats" &
                                  Natural'Image (Count + 1) & " times>");
                     end if;
                  end if;

               else
                  Next (C);
               end if;
            end loop;

            Append (Result, Context.Record_End);

         else
            Append (Result, Node.Value);
         end if;
      end Build_Result;

      ----------------
      -- Is_Command --
      ----------------

      function Is_Command (Name : String) return Boolean is
      begin
         return Standard.Ada.Strings.Fixed.Index (Name, " ") > Name'First;
      end Is_Command;

   begin
      if From_API then
         return Data_Evaluate (Entity);
      end if;

      if Is_Command (Entity) then
         return Debugger.Send_And_Get_Clean_Output (Entity, Mode => Internal);
      end if;

      V := Debugger.Create_Var (Entity);

      if V.Name = "" then
         return "<unknown>";
      end if;

      Debugger.Detect_Language;

      if Is_Empty (V.Nodes) then
         if V.Childs < 2 then
            declare
               N : constant Node_Access := new Node (False);
               R : constant String := Var_Evaluate (To_String (V.Name));
            begin
               Match (No_Get_Value_Pattern, R, Matched);
               if Matched (0) /= No_Match then
                  N.Name     := To_Unbounded_String (Entity);
                  N.Evaluate := Data;
                  N.Value    := To_Unbounded_String (Data_Evaluate (Entity));
               else
                  N.Name     := V.Name;
                  N.Evaluate := Var;
                  N.Value    := To_Unbounded_String (R);
               end if;

               Append (V.Nodes, N);
            end;

         else
            if Lang.Get_Name = "c++" then
               declare
                  N : constant Node_Access := new Gdb_MI.Node (False);
               begin
                  N.Name     := To_Unbounded_String (Entity);
                  N.Exp      := To_Unbounded_String (Entity);
                  N.Evaluate := Data;
                  N.Value := To_Unbounded_String
                    (Data_Evaluate (Entity));

                  Append (V.Nodes, N);
               end;

            else
               Update_Value
                 (To_String (V.Name),
                  To_String (V.Name),
                  V.Nodes,
                  null);
            end if;
         end if;
      else
         Update_Value
           (To_String (V.Name),
            To_String (V.Name),
            V.Nodes,
            First_Element (V.Nodes));
      end if;

      if not Is_Empty (V.Nodes) then
         Build_Result (First_Element (V.Nodes), False);
      end if;

      Free (Debugger, V);
      return To_String (Result);

   exception
      when E : others =>
         Me.Trace (E);
         return "<unknown>";
   end Value_Of;

   ---------------------
   -- Print_Value_Cmd --
   ---------------------

   overriding function Print_Value_Cmd
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-data-evaluate-expression " & Entity;
   end Print_Value_Cmd;

   -----------------
   -- Get_Uniq_Id --
   -----------------

   overriding function Get_Uniq_Id
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String) return String
   is
      --  ??? Probably, this should be language-dependent.
      --  In particular, in C, &(*A) returns A, not an address, which causes
      --  unexpected wrong aliases to be detected.

   begin
      return Get_Value
        (Debugger.Send_And_Get_Clean_Output
           ("-data-evaluate-expression &(" & Entity & ")", Mode => Internal));
   end Get_Uniq_Id;

   -----------
   -- Spawn --
   -----------

   overriding procedure Spawn
     (Debugger        : access Gdb_MI_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Debugger_Args   : GNAT.OS_Lib.Argument_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Debugger_Num    : Natural;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "")
   is
      Gdb_Arguments   : Argument_List_Access :=
                          Argument_String_To_List (Gdb_Options);
      Num_Options     : constant Natural := Gdb_Arguments'Length;
      Local_Arguments : Argument_List
        (1 .. Debugger_Args'Length + Num_Options);
      Process         : Visual_Debugger;

      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Argument_List, Argument_List_Access);

   begin
      Local_Arguments (1 .. Num_Options) := Gdb_Arguments.all;
      Local_Arguments (Num_Options + 1 .. Local_Arguments'Last) :=
        Debugger_Args;
      Free (Gdb_Arguments);

      Debugger.General_Spawn
        (Kernel        => Kernel,
         Arguments     => Local_Arguments,
         Debugger_Name =>
           (if Debugger_Name = ""
            then Gdb_Command
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
         Debugger.Detect_Debugger_Mode;
      end if;

      --  Set up an output filter to detect changes of the current language
      --  We do that only in graphical mode, since the filter needs to
      --  access the main_debug_window.

      Process := Convert (Debugger);

      if Process /= null then
         Process.Add_Regexp_Filter (Language_Filter'Access, Language_Pattern);

         Process.Add_Regexp_Filter
           (Terminate_Filter'Access, Terminate_Pattern);
         Process.Add_Regexp_Filter
           (Terminate_Filter'Access, Not_Running_Pattern);

         Process.Add_Regexp_Filter
           (Breakpoint_Filter'Access, Breakpoint_Pattern);

         Process.Add_Regexp_Filter (Running_Filter'Access, Running_Regexp);
         Process.Add_Regexp_Filter (Stopped_Filter'Access, Stopped_Regexp);

         Process.Add_Regexp_Filter
           (Question_Filter1'Access, Question_Filter_Pattern1);
         Process.Add_Regexp_Filter
           (Question_Filter2'Access, Question_Filter_Pattern2);

         Add_Regexp_Filter
           (Process, Continuation_Line_Filter'Access,
            Continuation_Line_Pattern);

         Set_Input_Output_Filter (Process);
      end if;
   end Spawn;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Debugger : access Gdb_MI_Debugger) is
      Num     : Expect_Match;
      Lang    : Language_Access;
      Process : Visual_Debugger;
      Dummy   : Version_Number;

      use GVD;
   begin
      Debugger.Initializing := True;

      --  Wait for initial output and prompt (and display it in the window)
      Debugger.Get_Process.Wait
        (Num,
         Compile ("^\([^\s]+\).*$", Multiple_Lines),
         Timeout => -1);

      Debugger.Send ("-gdb-set width 0",  Mode => Internal);
      Debugger.Send ("-gdb-set height 0", Mode => Internal);

      --  To get full path to the main file when loading executable
      Debugger.Send ("set annotate 1", Mode => Internal);

      --  Make sure to disable the styling for terminals so that GNAT Studio
      --  can properly parse variable values.

      Send (Debugger, "set style enabled off", Mode => Internal);

      --  Cache result of 'show version', and also as a side effect, includes
      --  output of 'show version' in the log file.

      Dummy := Debugger.Get_GDB_Version;

      if not Is_Local (Debug_Server) then
         --  Workaround the following problem: when telneting to Windows,
         --  using the windows telnet service, then the TERM env variable is
         --  set to vt100, which produces undesirable results (input is
         --  duplicated, see HC08-007). A workaround is to send
         --  'set editing off' to gdb, as this removes this undesired result.
         Debugger.Send ("-gdb-set editing off", Mode => Internal);
      end if;

      if Get_Pref (Execution_Window) and then Is_Local (Debug_Server) then
         if Host = Windows then
            Debugger.Send ("-gdb-set new-console", Mode => Internal);
         end if;

         Debugger.Execution_Window := True;

      else
         Debugger.Execution_Window := False;
      end if;

      --  Load the module to debug, if any

      --  Turn off multiple-choice questions
      if Get_Pref (Cancel_Multiple_Symbols) then
         Debugger.Send ("set multiple-symbols cancel", Mode => Internal);
      end if;

      if Debugger.Executable /= GNATCOLL.VFS.No_File then
         Debugger.Set_Executable (Debugger.Executable);
         Debugger.Catch_Exception;
      else
         --  Connect to the target, if needed. This is normally done by
         --  Set_Executable, but GNAT Studio should also connect immediately
         --  if the corresponding options were passed on the command line.
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

         if Lang /= null and then Lang.all in Gdb_Ada_Language'Class then
            Debugger.Send ("list adainit", Mode => Internal);

         else
            Debugger.Send ("list main,main", Mode => Internal);
         end if;

         if Get_Pref (Open_Main_Unit) then
            Debugger.Send ("info line", Mode => Internal);
            Should_Have_Current_Line (Debugger);
         end if;

         Debugger.Display_Prompt;
      end if;

      if Debugger.Executable_Args /= null then
         Set_Args (Debugger, Debugger.Executable_Args.all, Mode => Internal);
      end if;

      Debugger.Initializing := False;

   exception
      --  If the executable was not found, simply display the prompt before
      --  leaving, nothing else needs to be done.

      when Executable_Not_Found =>
         Debugger.Display_Prompt;
         Debugger.Initializing := False;
   end Initialize;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Debugger : access Gdb_MI_Debugger) is
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
         Debugger.Send
           ("-gdb-exit", Wait_For_Prompt => False, Mode => Internal);
      end if;

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -------------------------
   -- Configure_Backtrace --
   -------------------------

   overriding procedure Configure_Backtrace
     (Self                 : not null access Gdb_MI_Debugger;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True)
   is
      pragma Unreferenced
        (Show_Id, Show_PC, Show_Subprogram_Name, Show_Location);
   begin
      Self.Is_Bt_Parameters := Show_Parameters;
   end Configure_Backtrace;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_MI_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : Command_Type := Hidden)
   is
      Cmd     : constant String := "-target-select " & Protocol & " " & Target;
      Matched : Match_Array (0 .. 2);
   begin
      if Debugger.Target_Connected then
         if Force then
            Debugger.Interrupt;
         else
            return;
         end if;
      end if;

      declare
         Output : constant String := Send_And_Get_Clean_Output
           (Debugger,
            Cmd             => Cmd,
            Synchronous     => False,
            Mode            => Mode);
         Success : Boolean :=
           Index (Output, Pattern => Failed_To_Connect_Pattern) = 0;
      begin

         Match (Error_Pattern, Output, Matched);
         Success := Success and then Matched (0) = No_Match;

         if Success then
            if Protocol = "remote" then
               Debugger.Set_Is_Started (Attached);
            end if;

            Debugger.Set_VxWorks_Version;
         end if;

         Debugger.Target_Connected := Success;
      end;
   end Connect_To_Target;

   ----------------------------
   -- Is_Connected_To_Target --
   ----------------------------

   overriding function Is_Connected_To_Target
     (Debugger : access Gdb_MI_Debugger) return Boolean
   is
      (Debugger.Target_Connected);

   --------------
   -- Set_Args --
   --------------

   procedure Set_Args
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-arguments " & Arguments, Mode => Mode);
   end Set_Args;

   --------------------
   -- Get_Executable --
   --------------------

   overriding function Get_Executable
     (Debugger : access Gdb_MI_Debugger) return GNATCOLL.VFS.Virtual_File is
   begin
      return Debugger.Executable;
   end Get_Executable;

   ---------------------------------
   -- Connect_To_Target_If_Needed --
   ---------------------------------

   procedure Connect_To_Target_If_Needed (Debugger : access Gdb_MI_Debugger) is
   begin
      if Debugger.Remote_Target /= null
        and then Debugger.Remote_Protocol /= null
        and then not Debugger.Target_Connected
      then
         Debugger.Connect_To_Target (Target   => Debugger.Get_Remote_Target,
                                     Protocol => Debugger.Get_Remote_Protocol,
                                     Mode     => Visible);
      end if;
   end Connect_To_Target_If_Needed;

   --------------------
   -- Set_Executable --
   --------------------

   Binder_File_Name_Pattern : constant Pattern_Matcher :=
     Compile ("(b(~|_).+\.(adb|c))", Multiple_Lines);
   --  Matches a binder file name

   overriding procedure Set_Executable
     (Debugger   : access Gdb_MI_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File)
   is

      Remote_Exec         : constant Virtual_File := To_Remote
        (Executable, Get_Nickname (Debug_Server));
      Exec_Has_Spaces     : constant Boolean :=
        Index (Remote_Exec.Display_Full_Name, " ") /= 0;
      Full_Name           : constant String :=
        +Remote_Exec.Unix_Style_Full_Name;

      No_Such_File_Regexp : constant Pattern_Matcher := Compile
        (Full_Name & ": No such file or directory.");
      --  Note that this pattern should work even when LANG isn't english
      --  because gdb does not seem to take into account this variable at all.

      Process : Visual_Debugger;
      Lang    : Language_Access;

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

         --  Send the command and wait until the end of it's execution without
         --  blocking The UI.

         declare
            Output : constant String := Debugger.Send_And_Get_Clean_Output
              (Cmd.all,
               Mode        => Visible,
               Synchronous => False);
         begin
            Free (Cmd);

            if Match (No_Such_File_Regexp, Output) /= 0 then
               raise Executable_Not_Found;
            end if;
         end;
      end Launch_Command_And_Output;

   begin
      Process := Convert (Debugger);

      Debugger.Executable := Executable;

      --  Send the 'file' command and verify that the specified executable
      --  actually exists by filtering the command's output.

      Launch_Command_And_Output ("-file-exec-and-symbols");

      --  Connect to the remote target if needed

      Connect_To_Target_If_Needed (Debugger);

      --  If we are in Cross mode (ie, with the "remote" protocol), the call
      --  to "target" has the side effect of starting the executable.
      --  In all other cases the executable is not started at this stage.

      if Debugger.Remote_Mode = Cross then
         Debugger.Set_Is_Started (Launched);
      else
         Debugger.Set_Is_Started (None);
      end if;

      --  Report a change in the executable. This has to be done before we
      --  look for the current file and line, so that the explorer can be
      --  correctly updated.
      --  No need to do anything in text-only mode

      if Process /= null then
         Debugger_Executable_Changed_Hook.Run (Process.Kernel, Process);
      end if;

      --  Get the name and line of the initial file

      Lang := Debugger.Get_Language;

      if Lang /= null and then Lang.all in Gdb_Ada_Language'Class then
         Debugger.Send ("list adainit", Mode => Internal);

      else
         Debugger.Send ("list main,main", Mode => Internal);
      end if;

      if Get_Pref (Open_Main_Unit) then
         Debugger.Get_Process.Set_Parse_File_Name (False);

         declare
            Str     : constant String := Strip_Escape
              (Debugger.Send_And_Get_Clean_Output
                 ("info line", Mode => Internal));
            Matched : Match_Array (0 .. 2);
            First   : Natural;

         begin
            Debugger.Get_Process.Set_Parse_File_Name (True);
            Match (Binder_File_Name_Pattern, Str, Matched);
            if Matched (0) /= No_Match then
               First := Matched (0).First + 2;

               if Str (First) = '_' then
                  First := First + 1;
               end if;

               Debugger.Send
                 ("info line " & Str (First .. Matched (0).Last) & ":1",
                  Mode => Internal);
               return;
            end if;

            Debugger.Send ("info line", Mode => Internal);
         end;

         Should_Have_Current_Line (Debugger);
      end if;
   end Set_Executable;

   ---------------------
   -- Catch_Exception --
   ---------------------

   overriding procedure Catch_Exception (Debugger : access Gdb_MI_Debugger) is
      Process : Visual_Debugger;
   begin
      Process := Convert (Debugger);

      if Get_Pref (Break_On_Exception) then
         declare
            Cmd : constant String := "-catch-exception";
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
   end Catch_Exception;

   --------------------
   -- Load_Core_File --
   --------------------

   overriding procedure Load_Core_File
     (Debugger : access Gdb_MI_Debugger;
      Core     : Virtual_File;
      Mode     : Command_Type := Hidden)
   is
      Core_File : constant Filesystem_String := Core.Unix_Style_Full_Name;
   begin
      Debugger.Set_Is_Started (None);
      Debugger.Send ("core " & (+Core_File), Mode => Mode);
   end Load_Core_File;

   ---------------------
   -- Load_Executable --
   ---------------------

   overriding procedure Load_Executable
     (Debugger   : access Gdb_MI_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      if Debugger.Is_Connected_To_Target then

         --  The GDB mi '-target-download' command does not allow to specify
         --  a module name: send directly the GDB console 'load' command
         --  instead.
         if Executable /= GNATCOLL.VFS.No_File then
            Send
              (Debugger,
               "-interpreter-exec console ""load \"""
               & (+Executable.Unix_Style_Full_Name) & "\""""",
               Mode => Mode);
         else
            Send
              (Debugger,
               "-interpreter-exec console ""load""",
               Mode => Mode);
         end if;
      end if;
   end Load_Executable;

   -----------------
   -- Add_Symbols --
   -----------------

   overriding procedure Add_Symbols
     (Debugger : access Gdb_MI_Debugger;
      Module   : Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Symbols : constant String := +Module.Unix_Style_Full_Name;
   begin
      Debugger.Test_If_Has_Command
        (Debugger.Has_Wtx_Add_Symbol_File,
         "wtx add-symbol-file");

      if Debugger.Has_Wtx_Add_Symbol_File = GNATCOLL.Tribooleans.True
        and then Address = ""
      then
         Debugger.Send ("wtx add-symbol-file " & Symbols, Mode => Mode);
      else
         Debugger.Send
           ("add-symbol-file " & Symbols & " " & Address, Mode => Mode);
      end if;
   end Add_Symbols;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access Gdb_MI_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden)
   is
      Matched : Match_Array (0 .. 2);

   begin
      Debugger.Send ("-target-attach " & Process, Mode => Mode);
      --  Calling Send_And_Get_Clean_Output in this place emmits
      --  Context_Changed immediately, before Process_Terminated,
      --  and makes the Call stack view empty after
      --  processing Process_Terminated

      --  Find the first frame containing source information to be as user
      --  friendly as possible, and also check whether attach was successful

      loop
         declare
            EMsg : constant String :=
              "Initial frame selected; you cannot go up.";
            --  Error message reported by gdb when first frame selected.

            Block : Process_Proxies.Parse_File_Switch
              (Debugger.Process) with Unreferenced;
            Str  : constant String := Debugger.Send_And_Get_Clean_Output
              ("up", Mode => Internal);
            File : Unbounded_String;
            Line : Natural;
            Addr : Address_Type;

         begin
            --  If attach failed, "up" will return an error message

            Match (Error_Pattern, Str, Matched);

            if Matched (0) /= No_Match then
               --  Matched regexp contains only "error stream mark" without
               --  open quotation mark. Thus, actual message starts from
               --  Matched (0).Last + 2. Likewise, last character is closing
               --  quotation mark, it is ignored too.

               exit when
                 Index (Str (Matched (0).Last + 2 .. Str'Last - 1), EMsg) /= 0;

               Debugger.Set_Is_Started (None);
               Debugger.Kernel.Messages_Window.Insert
                 (Str (Matched (0).Last + 2 .. Str'Last - 1), Mode => Error);

               return;
            end if;

            Debugger.Found_File_Name (Str, File, Line, Addr);

            exit when Length (File) /= 0;
         end;
      end loop;

      --  Attach succeed: consider the debugger as started
      Debugger.Set_Is_Started (Attached);

      Debugger.Send ("-stack-info-frame", Mode => Internal);

   exception
      when Constraint_Error =>
         --  Most likely the underlying process died
         null;
   end Attach_Process;

   --------------------
   -- Detach_Process --
   --------------------

   overriding procedure Detach_Process
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-target-detach", Mode => Mode);
      Debugger.Set_Is_Started (None);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access Gdb_MI_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("kill", Mode => Mode);
      Debugger.Set_Is_Started (None);
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt (Debugger : access Gdb_MI_Debugger) is
      Dummy : Expect_Match;
   begin
      Debugger.Get_Process.Wait (Dummy, Prompt_Regexp, Timeout => -1);

      if Debugger.Is_Running
        and then Debugger.Current_Command_Kind = Execution_Command
      then
         --  We are running, wait for the second 'stopped' prompt
         Debugger.Get_Process.Wait (Dummy, Prompt_Regexp, Timeout => -1);
      end if;
   end Wait_Prompt;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding function Wait_Prompt
     (Debugger : access Gdb_MI_Debugger;
      Timeout  : Integer) return Boolean
   is
      Prompt_Match : Expect_Match;
   begin
      if not Debugger.Second_Wait then
         --  Wait for an error or first prompt.
         Debugger.Get_Process.Wait
           (Prompt_Match, Error_Or_Prompt_Pattern, Timeout => Timeout);

         if Prompt_Match = Expect_Timeout then
            return False;
         end if;
      end if;

      --  Wait for first prompt after error or second prompt in other case
      if Debugger.Is_Running
        and then Debugger.Current_Command_Kind = Execution_Command
      then
         --  We are running, wait for the second 'stopped' prompt
         Debugger.Second_Wait := True;
         Debugger.Get_Process.Wait
           (Prompt_Match, Prompt_Regexp, Timeout => Timeout);

         if Prompt_Match = Expect_Timeout then
            return False;
         end if;
      end if;

      Debugger.Second_Wait := False;
      return True;
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

   ------------------------
   -- Get_Register_Names --
   ------------------------

   procedure Get_Register_Names
     (Debugger : access Gdb_MI_Debugger)
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

   begin
      if not Debugger.Register_Names.Is_Empty then
         return;
      end if;

      --  Try to get values first because request -data-list-register-names
      --  always returns names (incorrect) even gdb has "No registers"
      declare
         S  : constant String := Debugger.Send_And_Get_Clean_Output
           ("-data-list-register-values x 0", Mode => Internal);
         Matched : Match_Array (0 .. 2);
      begin
         Match (Error_Pattern, S, Matched);

         if Matched (0) /= No_Match then
            return;
         end if;
      end;

      declare
         use Token_Lists;

         S       : constant String := Debugger.Send_And_Get_Clean_Output
           ("-data-list-register-names", Mode => Internal);

         Tokens  : Token_List_Controller;
         C       : Token_Lists.Cursor;

      begin
         Tokens.List := Build_Tokens (S);
         C := Find_Identifier (First (Tokens.List), "register-names");

         if C = Token_Lists.No_Element then
            return;
         end if;

         --  Skip register-names=[
         Next (C, 3);
         while Element (C).Code /= R_Bracket loop  -- /= ']'
            Debugger.Registers.Append (Element (C).Text.all);

            if Element (C).Text.all /= "" then
               Debugger.Register_Names.Append (Element (C).Text.all);
            end if;

            Next (C, 1);
            if Element (C).Code = Comma then
               Next (C, 1);
            end if;
         end loop;
      end;
   end Get_Register_Names;

   --------------------------
   -- Get_Registers_Values --
   --------------------------

   overriding function Get_Registers_Values
     (Debugger : access Gdb_MI_Debugger;
      Names    : GVD.Types.Strings_Vectors.Vector;
      Format   : GVD.Types.Registers_Format)
      return GVD.Types.String_To_String_Maps.Map
   is
      use Token_Lists;

      Keys : constant array (GVD.Types.Registers_Format) of Character :=
        (Hexadecimal => 'x',
         Octal       => 'o',
         Binary      => 't',
         Decimal     => 'd',
         Raw         => 'r',
         Naturals    => 'N');

      function Get_Indices return String;
      --  Convert register names to indeces

      -----------------
      -- Get_Indices --
      -----------------

      function Get_Indices return String is
         use GVD.Types.Strings_Vectors;

         Result : Unbounded_String;
         C      : GVD.Types.Strings_Vectors.Cursor;
      begin
         if Names.Is_Empty then
            return "";
         else
            for Item of Names loop
               C := Debugger.Registers.Find (Item);
               if Has_Element (C) then
                  Append (Result, Natural'Image (To_Index (C) - 1));
               end if;
            end loop;

            return To_String (Result);
         end if;
      end Get_Indices;

      Values  : array (1 .. Debugger.Registers.Last_Index) of
        Standard.Ada.Strings.Unbounded.Unbounded_String;

      Result  : GVD.Types.String_To_String_Maps.Map;

      Tokens  : Token_List_Controller;
      C       : Token_Lists.Cursor;
      Num     : Natural;
      Value   : Unbounded.String_Access;
      Matched : Match_Array (0 .. 2);

   begin
      if Debugger.Registers.Is_Empty then
         return Result;
      end if;

      declare
         Block : Process_Proxies.Parse_File_Switch
           (Debugger.Process) with Unreferenced;
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-data-list-register-values " & Keys (Format) & Get_Indices,
            Mode => Internal);
      begin
         Match (Error_Pattern, S, Matched);

         if Matched (0) /= No_Match then
            Debugger.Register_Names.Clear;
            return Result;
         end if;

         Get_Register_Names (Debugger);

         Tokens.List := Build_Tokens (S);
         C := Find_Identifier (First (Tokens.List), "register-values");

         if C = Token_Lists.No_Element then
            return Result;
         end if;

         --  Skip register-values=[
         Next (C, 3);
         while Element (C).Code /= R_Bracket loop  -- /= ']'
            if Element (C).Code = L_Brace then
               Next (C, 1);
               while Element (C).Code /= R_Brace loop
                  if Element (C).Code = Identifier then
                     if Element (C).Text.all = "number" then
                        Next (C, 2);
                        Num := Positive'Value (Element (C).Text.all) + 1;

                     elsif Element (C).Text.all = "value" then
                        Next (C, 2);
                        Value := Element (C).Text;

                     else
                        Next (C, 1);
                     end if;
                  else
                     Next (C, 1);
                  end if;
               end loop;

               if Debugger.Registers.Last_Index >= Num then
                  if Value /= null then
                     Values (Num) := To_Unbounded_String (Value.all);
                  end if;
               end if;
               Value := null;

            else
               Next (C, 1);
            end if;
         end loop;

         declare
            use GVD.Types.Strings_Vectors;
            C : GVD.Types.Strings_Vectors.Cursor;
         begin
            for Name of Names loop
               C := Debugger.Registers.Find (Name);
               if Has_Element (C) then
                  Result.Include (Name, To_String (Values (To_Index (C))));
               else
                  Result.Include (Name, "");
               end if;
            end loop;
         end;

      end;
      return Result;
   end Get_Registers_Values;

   ------------------------
   -- Get_Register_Names --
   ------------------------

   overriding function Get_Register_Names
     (Debugger : access Gdb_MI_Debugger)
      return GVD.Types.Strings_Vectors.Vector is
   begin
      Get_Register_Names (Debugger);
      return Debugger.Register_Names;
   end Get_Register_Names;

   ---------
   -- Run --
   ---------

   procedure Run_Helper
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean);
   --  Shared code beteen Run/Start

   procedure Run_Helper
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String;
      Mode      : Command_Type;
      Start     : Boolean) is
   begin
      if Arguments = "" and then Debugger.Remote_Target /= null
        and then Debugger.Executable /= GNATCOLL.VFS.No_File
      then
         declare
            Module : constant String := Get_Module (Debugger.Executable);
         begin
            Debugger.Send ("-exec-arguments " & Module, Mode => Internal);
         end;

      else
         Debugger.Send ("-exec-arguments " & Arguments, Mode => Internal);
      end if;

      Debugger.Send
        ("-exec-run" & (if Start then " --start" else ""),
         Wait_For_Prompt => False,
         Mode            => Mode);
   end Run_Helper;

   ---------
   -- Run --
   ---------

   overriding procedure Run
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => False);
   end Run;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Debugger  : access Gdb_MI_Debugger;
      Arguments : String := "";
      Mode      : Command_Type := Hidden) is
   begin
      Run_Helper (Debugger, Arguments, Mode, Start => True);
   end Start;

   ---------------
   -- Step_Into --
   ---------------

   overriding procedure Step_Into
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-step", Mode => Mode);
   end Step_Into;

   ---------------
   -- Step_Over --
   ---------------

   overriding procedure Step_Over
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-next", Mode => Mode);
   end Step_Over;

   ---------------------------
   -- Step_Into_Instruction --
   ---------------------------

   overriding procedure Step_Into_Instruction
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-step-instruction", Mode => Mode);
   end Step_Into_Instruction;

   ---------------------------
   -- Step_Over_Instruction --
   ---------------------------

   overriding procedure Step_Over_Instruction
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-next-instruction", Mode => Mode);
   end Step_Over_Instruction;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-continue", Wait_For_Prompt => False, Mode => Mode);
   end Continue;

   -----------------------------
   -- Continue_Until_Location --
   -----------------------------

   overriding procedure Continue_Until_Location
     (Debugger : access Gdb_MI_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send
        ("-exec-until " & (+Base_Name (File)) & ":" & Image (Integer (Line)),
         Wait_For_Prompt => False,
         Mode            => Mode);
   end Continue_Until_Location;

   ------------------------
   -- Line_Contains_Code --
   ------------------------

   overriding function Line_Contains_Code
     (Debugger : not null access Gdb_MI_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type)
      return Boolean
   is
      Block  : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;
      Output : constant String := Debugger.Send_And_Get_Clean_Output
           ("info line "
            & (+Base_Name (File)) & ":" & Image (Integer (Line)),
            Mode => Internal);
   begin
      return Index (Output, Pattern => "no code") = 0;
   end Line_Contains_Code;

   -------------------
   -- Current_Frame --
   -------------------

   overriding function Current_Frame
     (Debugger : access Gdb_MI_Debugger)
      return Integer
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

   begin
      if Debugger.Current_Frame.Frame = -1 then
         Debugger.Detect_Language;
         Debugger.Current_Frame := Parse_Frame_Info
           (Debugger.Send_And_Get_Clean_Output
              ("-stack-info-frame", Mode => Internal));
      end if;

      return Debugger.Current_Frame.Frame;
   end Current_Frame;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access Gdb_MI_Debugger) is
      Proxy      : constant Process_Proxy_Access := Debugger.Get_Process;
      Descriptor : constant Process_Descriptor_Access := Proxy.Get_Descriptor;

   begin
      --  Should only do this when running under Windows, in native mode,
      --  with an external execution window, and the debuggee running.

      if Debugger.Debuggee_Pid /= 0           -- valid pid
        and then Debugger.Is_Started          -- debuggee started
        and then Proxy.Command_In_Process     -- and likely running
        and then Host = Windows               -- Windows host
        and then Is_Local (Debug_Server)      -- no remote debugging
        and then Debugger.Execution_Window    -- external window
      then
         GNAT.Expect.TTY.Interrupt (Debugger.Debuggee_Pid);
      else
         Descriptor.Interrupt;
         Proxy.Set_Interrupted;
      end if;
      --  Consider using -exec-interrupt when not Command_In_Process???
      --  Send (Debugger, "-exec-interrupt", Mode => Internal);
   end Interrupt;

   ------------------
   -- Command_Kind --
   ------------------

   overriding function Command_Kind
     (Debugger : access Gdb_MI_Debugger;
      Command  : String) return Command_Category
   is
      Index : Natural := Command'First;
   begin
      if Command = "" then
         Debugger.Current_Command_Kind := Misc_Command;

      elsif Starts_With (Command, "file")
        or else Starts_With (Command, "add-symbol-file")
        or else Starts_With (Command, "load")
      then
         Debugger.Current_Command_Kind := Load_Command;

      elsif Starts_With (Command, "-target-attach")
        or else Starts_With (Command, "-thread-select")
        or else Starts_With (Command, "thread")
        or else Starts_With (Command, "task")
        or else Starts_With (Command, "core")
        or else Starts_With (Command, "attach")
        or else Starts_With (Command, "set variable")
      then
         Debugger.Current_Command_Kind := Context_Command;

      elsif Starts_With (Command, "-exec-")
        and then not Starts_With (Command, "-exec-arguments")
      then
         Debugger.Current_Command_Kind := Execution_Command;

      elsif Starts_With (Command, "-stack-select-frame")
        or else Starts_With (Command, "up")
        or else Starts_With (Command, "down")
        or else
          (Starts_With (Command, "frame")
           and then Command /= "farme")
      then
         Debugger.Current_Command_Kind := Frame_Command;

      else
         Skip_Word (Command, Index);
         if Command (Command'First .. Index - 1) = "step"
           or else Command (Command'First .. Index - 1) = "stepi"
           or else Command (Command'First .. Index - 1) = "s"
           or else Command (Command'First .. Index - 1) = "si"
           or else Command (Command'First .. Index - 1) = "next"
           or else Command (Command'First .. Index - 1) = "n"
           or else Command (Command'First .. Index - 1) = "nexti"
           or else Command (Command'First .. Index - 1) = "ni"
           or else Command (Command'First .. Index - 1) = "cont"
           or else Command (Command'First .. Index - 1) = "c"
           or else Command (Command'First .. Index - 1) = "continue"
           or else Command (Command'First .. Index - 1) = "run"
           or else Command (Command'First .. Index - 1) = "r"
           or else Starts_With (Command, "fin")  --  fin/fini/finish/...
           or else Starts_With (Command, "target")
           or else Starts_With (Command, "begin")
           or else Starts_With (Command, "start")
         then
            Debugger.Current_Command_Kind := Execution_Command;

         else
            Debugger.Current_Command_Kind := Misc_Command;
         end if;
      end if;

      return Debugger.Current_Command_Kind;
   end Command_Kind;

   -------------------------
   -- Breakpoints_Changed --
   -------------------------

   overriding function Breakpoints_Changed
     (Debugger : access Gdb_MI_Debugger;
      Command  : String) return Boolean is
   begin
      return Debugger.Breakpoints_Changed
        or else (Starts_With (Command, "-break-")
                 and then not Starts_With (Command, "-break-info")
                 and then not Starts_With (Command, "-break-list"))
        or else Starts_With (Command, "-dprintf-insert")
        or else Starts_With (Command, "-catch-")
        or else Starts_With (Command, "-exec-run")
      --  CLI part
        or else Looking_At (Command, Command'First + 1, "break")
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
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden)
   is
      Num : Natural;
   begin
      if Debugger.Current_Frame.Frame /= -1 then
         Num := Debugger.Current_Frame.Frame - 1;
         Debugger.Current_Frame := Null_Frame_Info;
         Debugger.Send ("-stack-select-frame" & Num'Img, Mode => Mode);
         Debugger.Send ("-stack-info-frame", Mode => Internal);

      else
         Debugger.Send ("down", Mode => Mode);
      end if;
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden)
   is
      Num : Natural;
   begin
      if Debugger.Current_Frame.Frame /= -1 then
         Num := Debugger.Current_Frame.Frame + 1;
         Debugger.Current_Frame := Null_Frame_Info;
         Debugger.Send ("-stack-select-frame" & Num'Img, Mode => Mode);
         Debugger.Send ("-stack-info-frame", Mode => Internal);

      else
         Debugger.Send ("up", Mode => Mode);
      end if;
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access Gdb_MI_Debugger;
      Frame    : Natural;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Current_Frame := Null_Frame_Info;
      Debugger.Send
        ("-stack-select-frame" & Natural'Image (Frame),
         Mode => Mode);
      Debugger.Send ("-stack-info-frame", Mode => Internal);
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   overriding procedure Backtrace
     (Debugger : access Gdb_MI_Debugger;
      From     : Integer;
      To       : Integer;
      Value    : out Backtrace_Vector)
   is
      use Token_Lists;
      use Backtrace_Vectors;

      Tokens    : Token_List_Controller;
      C, C2, C3 : Token_Lists.Cursor;
      Id        : Natural;
      Cursor    : Backtrace_Vectors.Cursor;
      Param     : Unbounded_String;
   begin
      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-stack-list-frames" &
            (if From >= 0
               then Integer'Image (From) & Integer'Image (To)
               else ""),
            Mode => Internal);
      begin
         Tokens.List := Build_Tokens (S);
      end;

      C := Find_Identifier (First (Tokens.List), "stack");
      if C = Token_Lists.No_Element then
         return;
      end if;

      Next (C, 3); -- Skip stack=[

      loop
         C := Find_Identifier (C, "frame");
         exit when C = Token_Lists.No_Element;

         Next (C, 3);

         declare
            Rec : Backtrace_Record;
         begin
            C2 := Find_Identifier (C, "level");
            Next (C2, 2);
            Rec.Frame_Id := Natural'Value (Element (C2).Text.all);

            C2 := Find_Identifier (C2, "addr");
            if C2 /= Token_Lists.No_Element then
               Next (C2, 2);
               Rec.Address := String_To_Address (Element (C2).Text.all);
            else
               C2 := C;
            end if;

            C2 := Find_Identifier (C2, "func");
            if C2 /= Token_Lists.No_Element then
               Next (C2, 2);
               Rec.Subprogram := new String'(Element (C2).Text.all);
            else
               Rec.Subprogram := new String'("<>");
               C2 := C;
            end if;

            C2 := Find_Identifier (C2, "fullname");
            if C2 /= Token_Lists.No_Element then
               Next (C2, 2);
               Rec.File := GPS.Core_Kernels.To_File
                 (Debugger.Kernel, Strip_Escape (Element (C2).Text.all));

               C3 := Find_Identifier (C2, "line");
               if C3 /= Token_Lists.No_Element then
                  Next (C3, 2);
                  Rec.Line := Natural'Value (Element (C3).Text.all);
               end if;
            end if;
            Value.Append (Rec);
         end;
      end loop;

      if not Debugger.Is_Bt_Parameters then
         return;
      end if;

      Clear_Token_List (Tokens.List);

      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-stack-list-arguments --no-frame-filters 1", Mode => Internal);
      begin
--     stack-args=
--       [frame={level="0",
--               args=[{name="this",
--                      value=
--                        "(layout => 0x0, side => 0, display => 0x64d010)"},
--                     {name="size",value="5"}]},
--        frame={level="1",args=[]}]

         Tokens.List := Build_Tokens (S);
      end;

      C := Find_Identifier (First (Tokens.List), "stack-args");
      if C = Token_Lists.No_Element then
         return;
      end if;

      Next (C, 3); -- Skip stack-args=[
      loop
         C := Find_Identifier (C, "frame");
         exit when C = Token_Lists.No_Element;

         Next (C, 3); -- Skip frame={
         C2 := Find_Identifier (C, "level");
         Next (C2, 2);
         Id := Natural'Value (Element (C2).Text.all);

         Cursor := Value.First;
         while Has_Element (Cursor)
           and then Element (Cursor).Frame_Id /= Id
         loop
            Next (Cursor);
         end loop;

         if Has_Element (Cursor) then
            declare
               Rec : Backtrace_Record := Element (Cursor);
            begin
               C2 := Find_Identifier (C2, "args");
               if C2 /= Token_Lists.No_Element then
                  Next (C2, 3); -- Skip args=[

                  loop
                     if Element (C2).Code = R_Bracket then
                        --  the end of the args scope
                        exit;

                     elsif Element (C2).Code = L_Brace then
                        --  the begin of the parameter
                        Param := Null_Unbounded_String;

                     elsif Element (C2).Code = R_Brace then
                        --  the end of the parameter
                        Rec.Parameters.Append
                          (Backtrace_Subprogram_Parameter'
                             (Value => new String'(To_String (Param))));

                     elsif Element (C2).Code = Identifier then
                        --  the parameter's values
                        if Element (C2).Text.all = "name" then
                           Next (C2, 2);
                           Append (Param, Element (C2).Text.all);

                        elsif Element (C2).Text.all = "value" then
                           Next (C2, 2);
                           Append (Param, "=" & Element (C2).Text.all);
                        end if;
                     end if;

                     Next (C2);
                  end loop;

                  if not Rec.Parameters.Is_Empty then
                     Value.Replace_Element (Cursor, Rec);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Backtrace;

   -----------------------------
   -- Internal_Set_Breakpoint --
   -----------------------------

   function Internal_Set_Breakpoint
     (Debugger  : access Gdb_MI_Debugger;
      Command   : String;
      Mode      : GVD.Types.Command_Type)
      return GVD.Types.Breakpoint_Identifier
   is
      C : constant String := Debugger.Send_And_Get_Clean_Output
        (Cmd => Command, Mode => Mode);
      M : Match_Array (0 .. 3);
   begin
      Match (Breakpoint_Num_Pattern, C, Matches => M);
      if M (3) /= No_Match then
         return Breakpoint_Identifier'Value
           (C (M (3).First .. M (3).Last));
      else
         return No_Breakpoint;
      end if;
   end Internal_Set_Breakpoint;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   overriding function Break_Subprogram
     (Debugger  : access Gdb_MI_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      Pending : constant Boolean := Get_Pref (Pending_Breakpoints);
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert "
         & (if Temporary then "-t " else "")
         & (if Pending then "-f " else "")
         & Name, Mode => Mode);
   end Break_Subprogram;

   ------------------
   -- Break_Source --
   ------------------

   overriding function Break_Source
     (Debugger  : access Gdb_MI_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      Result : GVD.Types.Breakpoint_Identifier;
      Name   : constant String := +Base_Name (File);
      Pending : constant Boolean := Get_Pref (Pending_Breakpoints);
   begin
      Result := Internal_Set_Breakpoint
        (Debugger,
         "-break-insert "
         & (if Temporary then "-t " else "")
         & (if Pending then "-f " else "")
         & (if Name /= "" then Name & ":" else "")
         & Image (Integer (Line)),
         Mode => Mode);

      Debugger.Remove_Breakpoint_Duplicates (Result);

      return Result;
   end Break_Source;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access Gdb_MI_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send
        ("clear " & File.Display_Base_Name & ":" & Image (Integer (Line)),
         Mode => Mode);
   end Remove_Breakpoint_At;

   ---------------------
   -- Break_Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access Gdb_MI_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "-catch-exception" & (if Temporary then " -t" else "") &
         (if Unhandled then " -u"
            elsif Name /= "" and then Name /= "all"
            then " -e " & Name else ""),
         Mode => Mode);
   end Break_Exception;

   ----------------------
   -- Catch_Assertions --
   ----------------------

   overriding function Catch_Assertions
     (Debugger  : access Gdb_MI_Debugger;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "-catch-assert" & (if Temporary then " -t" else ""),
         Mode => Mode);
   end Catch_Assertions;

   -------------------
   -- Break_Address --
   -------------------

   overriding function Break_Address
     (Debugger  : access Gdb_MI_Debugger;
      Address   : GVD.Types.Address_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier
   is
      Pending : constant Boolean := Get_Pref (Pending_Breakpoints);
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert "
         & (if Temporary then "-t " else "")
         & (if Pending then "-f " else "")
         & "*" & Address_To_String (Address), Mode => Mode);
   end Break_Address;

   ------------------
   -- Break_Regexp --
   ------------------

   overriding function Break_Regexp
     (Debugger  : access Gdb_MI_Debugger;
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
     (Debugger : access Gdb_MI_Debugger) return Breakpoint_Identifier
   is
      S            : constant String := Debugger.Send_And_Get_Clean_Output
        ("print $bpnum", Mode => Internal);
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
     (Debugger  : access Gdb_MI_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send
        ("-break-condition" & Num'Img & " " & Condition, Mode => Mode);
   end Set_Breakpoint_Condition;

   ----------------------------
   -- Set_Breakpoint_Command --
   ----------------------------

   overriding procedure Set_Breakpoint_Command
     (Debugger : access Gdb_MI_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
   is
      Cmd : Unbounded_String;
      Idx : Integer := Commands'First;
      To  : Integer;
   begin
      Append (Cmd, "-break-commands" & Num'Img);

      while Idx < Commands'Last loop
         To := Standard.Ada.Strings.Fixed.Index (Commands, "" & ASCII.LF, Idx);
         if To < Commands'First then
            Append (Cmd, " " & (if Commands (Idx) = '"'
                    then Commands (Idx .. Commands'Last)
                    else '"' & Commands (Idx .. Commands'Last) & '"'));
            exit;
         else
            Append (Cmd, " " & (if Commands (Idx) = '"'
                    then Commands (Idx .. To - 1)
                    else '"' & Commands (Idx .. To - 1) & '"'));

            Idx := To + 1;
         end if;
      end loop;

      Debugger.Send (To_String (Cmd), Mode => Mode);
   end Set_Breakpoint_Command;

   ---------------------------------
   -- Set_Breakpoint_Ignore_Count --
   ---------------------------------

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Gdb_MI_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("-break-after" & Num'Img & Count'Img, Mode => Mode);
   end Set_Breakpoint_Ignore_Count;

   -----------
   -- Watch --
   -----------

   overriding function Watch
     (Debugger  : access Gdb_MI_Debugger;
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
               return " -r ";
            when GVD.Types.Write =>
               return " ";
            when GVD.Types.Read_Write =>
               return " -a ";
         end case;
      end Command;

   begin
      if Condition = "" then
         return Internal_Set_Breakpoint
           (Debugger, "-break-watch" & Command & Name, Mode => Mode);
      else
         return Internal_Set_Breakpoint
           (Debugger,
            "-break-watch" & Command & Name & " if " & Condition,
            Mode => Mode);
      end if;
   end Watch;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-exec-finish", Mode => Mode);
   end Finish;

   -----------------
   -- Task_Switch --
   -----------------

   overriding procedure Task_Switch
     (Debugger : access Gdb_MI_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("task" & Task_Num'Img, Mode => Mode);
   end Task_Switch;

   -------------------
   -- Thread_Switch --
   -------------------

   overriding procedure Thread_Switch
     (Debugger : access Gdb_MI_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("-thread-select" & Thread'Img, Mode => Mode);
   end Thread_Switch;

   ---------------
   -- PD_Switch --
   ---------------

   overriding procedure PD_Switch
     (Debugger : access Gdb_MI_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("pd " & PD, Mode => Mode);
   end PD_Switch;

   ----------------
   -- Info_Tasks --
   ----------------

   overriding procedure Info_Tasks
     (Debugger : access Gdb_MI_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("-ada-task-info", Mode => Internal);
   begin
      Len := 0;

      declare
         use Token_Lists;
         Tokens  : Token_List_Controller;
         C       : Token_Lists.Cursor;
         Current : Boolean;
         P       : Thread_Fields;

      begin
         Tokens.List := Build_Tokens (S);
         C := Find_Identifier (First (Tokens.List), "body");

         if C = Token_Lists.No_Element then
            return;
         end if;

         --  Skip body=[
         Next (C, 3);
         if Element (C).Code = R_Bracket then -- ']'
            return;
         end if;

         Len := Info'First;
         Info (Len) :=
           (Information =>
              ["id",
               "task-id",
               "thread-id",
               "parent-id",
               "priority",
               "state",
               "name"]);

         loop -- over elements
            if Element (C).Code = Comma then
               Next (C, 1); -- skip ',' between elements
            end if;

            Next (C, 1); -- skip "{"
            Current := False;

            Len := Len + 1;
            Info (Len) := (Information => [for J in 1 .. 7 => ""]);
            --                             ^^^  replace by 7 * ""

            while Element (C).Code /= R_Brace loop -- over elemet tags till '}'
               if Element (C).Code = Comma then
                  Next (C, 1); -- skip ',' between tags
               end if;

               if Element (C).Code /= Identifier
                 or else Element (C).Text = null
               then
                  exit;
               end if;

               if Element (C).Text.all = "current" then
                  Current := True;
                  Next (C, 3); -- skip '="*"'

               elsif Element (C).Text.all = "id" then
                  Next (C, 2); -- skip '='
                  Info (Len).Information.Replace
                    (1,
                     VSS.Strings.Conversions.To_Virtual_String
                       ((if Current then "* " else "")
                           & Element (C).Text.all));
               else
                  if Element (C).Text.all = "task-id" then
                     P := 2;
                  elsif Element (C).Text.all = "thread-id" then
                     P := 3;
                  elsif Element (C).Text.all = "parent-id" then
                     P := 4;
                  elsif Element (C).Text.all = "priority" then
                     P := 5;
                  elsif Element (C).Text.all = "state" then
                     P := 6;
                  elsif Element (C).Text.all = "name" then
                     P := 7;
                  end if;

                  Next (C, 2); -- skip '='
                     Info (Len).Information.Replace
                       (P,
                        VSS.Strings.Conversions.To_Virtual_String
                          (Element (C).Text.all));
               end if;

               Next (C, 1); -- next element
            end loop;
            Next (C, 1); -- skip '}' from previouse element

            exit when Element (C).Code = R_Bracket; -- last ']'
         end loop;

      exception
         when others =>
            --  A parsing error occured when filling Info (Len)

            if Len > 0 then
               Len := Len - 1;
            end if;
      end;
   end Info_Tasks;

   -------------
   -- Collect --
   -------------

   function Collect
     (C        : in out Token_Lists.Cursor;
      To       : Token_Code;
      Opposite : Token_Code) return String
   is
      use Token_Lists;

      Count  : Natural := 0;
      Result : Standard.Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         if Element (C).Code = Opposite then
            Count := Count + 1;

         elsif Element (C).Code = To then
            if Count = 0 then
               exit;
            else
               Count := Count - 1;
            end if;
         end if;

         Append (Result, Image (Element (C)));

         Next (C, 1);
      end loop;

      return To_String (Result);
   end Collect;

   -------------
   -- Collect --
   -------------

   function Collect
     (Str      : String;
      To       : Character;
      Opposite : Character) return String
   is
      Count : Natural := 0;
      Idx   : Integer := Str'First;

   begin
      loop
         if Idx > Str'Last then
            return "";
         end if;

         if Str (Idx) = To then
            if Count > 0 then
               Count := Count - 1;
            else
               return Str (Str'First .. Idx - 1);
            end if;
         end if;

         if Str (Idx) = Opposite then
            Count := Count + 1;
         end if;

         Idx := Idx + 1;
      end loop;
   end Collect;

   ------------------
   -- Info_Threads --
   ------------------

   overriding procedure Info_Threads
     (Debugger : access Gdb_MI_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      use Standard.Ada.Strings.Unbounded;
      use Token_Lists;

      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("-thread-info", Mode => Internal);

      Tokens      : Token_List_Controller;
      C, Tmp      : Token_Lists.Cursor;
      Brace_Count : Natural := 0;
      Current     : Standard.Ada.Strings.Unbounded.Unbounded_String;
   begin
      Len := 0;

      Tokens.List := Build_Tokens (S);
      C := Find_Identifier (First (Tokens.List), "threads");

      if C = Token_Lists.No_Element then
         return;
      end if;

      --  Skip 'threads=['
      Next (C, 3);

      if Element (C).Code = R_Bracket then
         return;
      end if;

      Len := Info'First;
      Info (Len) :=
        (Information =>
           ["id",
            "target-id",
            "name",
            "frame",
            "state",
            "core"]);

      Tmp := Find_Identifier (C, "current-thread-id");
      if Tmp /= Token_Lists.No_Element then
         Next (Tmp, 2);
         Current := To_Unbounded_String (Element (Tmp).Text.all);
      end if;

      while Element (C).Code /= R_Bracket loop -- last ']'
         --  over elements

         Next (C, 1); -- skip starting "{"

         Tmp := Find_Identifier (C, "id");
         exit when Tmp = Token_Lists.No_Element;
         Next (Tmp, 2);

         Len := Len + 1;
         Info (Len) := (Information => [for J in 1 .. 6 => ""]);
         --                             ^^^  replace by 6 * ""

         Info (Len).Information.Replace
           (1,
            VSS.Strings.Conversions.To_Virtual_String
              ((if Current = Element (Tmp).Text.all
                  then "* "
                  else "") & Element (Tmp).Text.all));

         Tmp := Find_Identifier (C, "target-id");
         if Tmp /= Token_Lists.No_Element then
            Next (Tmp, 2);
            Info (Len).Information.Replace
              (2,
               VSS.Strings.Conversions.To_Virtual_String
                 (Element (Tmp).Text.all));
         end if;

         Tmp := Find_Identifier (C, "name");
         if Tmp /= Token_Lists.No_Element then
            Next (Tmp, 2);
            Info (Len).Information.Replace
              (3,
               VSS.Strings.Conversions.To_Virtual_String
                 (Element (Tmp).Text.all));
         end if;

         Tmp := Find_Identifier (C, "frame");
         if Tmp /= Token_Lists.No_Element then
            Next (Tmp, 3);
            Info (Len).Information.Replace
              (4,
               VSS.Strings.Conversions.To_Virtual_String
                 (Collect (Tmp, R_Brace, L_Brace)));
         end if;

         Tmp := Find_Identifier (C, "state");
         if Tmp /= Token_Lists.No_Element then
            Next (Tmp, 2);
            Info (Len).Information.Replace
              (5,
               VSS.Strings.Conversions.To_Virtual_String
                 (Element (Tmp).Text.all));
         end if;

         Tmp := Find_Identifier (C, "core");
         if Tmp /= Token_Lists.No_Element then
            Next (Tmp, 2);
            Info (Len).Information.Replace
              (6,
               VSS.Strings.Conversions.To_Virtual_String
                 (Element (Tmp).Text.all));
         end if;

         Brace_Count := 0;
         loop
            if Element (C).Code = L_Brace then --  '{'
               Brace_Count := Brace_Count + 1;

            elsif Element (C).Code = R_Brace then  --  '}'
               if Brace_Count = 0 then
                  exit;
               else
                  Brace_Count := Brace_Count - 1;
               end if;
            end if;

            Next (C, 1);
         end loop;
         Next (C, 1); --  skip ending '}' from prev. element

         if Element (C).Code = Comma then
            Next (C, 1); -- skip ',' between elements
         end if;
      end loop;

   exception
      when E : others =>
         Trace (Me, E);
         if Len > 0 then
            Len := Len - 1;
         end if;
   end Info_Threads;

   --------------------------
   -- Highlighting_Pattern --
   --------------------------

   overriding function Highlighting_Pattern
     (Debugger : access Gdb_MI_Debugger) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Debugger);
   begin
      return Highlight_Pattern;
   end Highlighting_Pattern;

   --------------------
   -- Display_Prompt --
   --------------------

   overriding procedure Display_Prompt (Debugger : access Gdb_MI_Debugger) is
      Proc : constant Visual_Debugger := Convert (Debugger);
   begin
      if Proc /= null then
         Proc.Output_Text
           (Prompt_String, Is_Command => False, Set_Position => True);
      end if;
   end Display_Prompt;

   ----------------------
   -- Change_Directory --
   ----------------------

   overriding procedure Change_Directory
     (Debugger : access Gdb_MI_Debugger;
      Dir      : Virtual_File;
      Mode     : Command_Type := Hidden)
   is
      Directory : constant String := +Dir.Unix_Style_Full_Name;
   begin
      Debugger.Send ("-environment-cd " & Directory, Mode => Mode);
   end Change_Directory;

   ---------------------
   -- Found_File_Name --
   ---------------------

   File_Name_Pattern         : constant Pattern_Matcher := Compile
     ("\\032\\032(.+):(\d+):\d+:[^:]+:(0x[0-9a-f]+)\\n", Multiple_Lines);
   --  Matches a file name/line indication in the annotated gdb's output

   Frame_Pattern     : constant Pattern_Matcher := Compile
     ("^(\*stopped.*|\^done,)frame={(.*)}", Multiple_Lines);

   Line_Pattern     : constant Pattern_Matcher := Compile
     ("^~""Line (\d+) of \\""(.*)\\"" (is at address|starts at address)" &
        " (0x[0-9a-f]+)",
      Multiple_Lines);

   CLI_Frame_Pattern : constant Pattern_Matcher := Compile
     ("^~""#(\d+) +((0x[0-9a-f]+) in )?(.+?)( at (.+))?""$",
      Multiple_Lines);

   Main_Task_Pattern : constant Pattern_Matcher := Compile
     ("^~""(.+)?\((0x[0-9a-f]+)?\) at (.+)""$", Multiple_Lines);

   Frame_File_Line_Pattern : constant Pattern_Matcher := Compile
     ("(.+):(\d+)(\\n)?$", Multiple_Lines);
   --  Regular expression to separate line from file name

   overriding procedure Found_File_Name
     (Debugger    : access Gdb_MI_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      Matched  : Match_Array (0 .. 6);
      Matched2 : Match_Array (0 .. 6);
      Start    : Natural := Str'First;

      procedure Get_File_And_Line (Str : String);
      procedure Get_File_And_Line (Str : String) is
         MN : Match_Array (0 .. 2);
      begin
         Match (Frame_File_Line_Pattern, Str, MN);

         if MN (0) /= No_Match then
            Debugger.Current_Frame.File := To_Unbounded_String
              (Strip_Escape (Str (MN (1).First .. MN (1).Last)));
            Debugger.Current_Frame.Line := Integer'Value
              (Str (MN (2).First .. MN (2).Last));
         end if;

         Name := Debugger.Current_Frame.File;
         Line := Debugger.Current_Frame.Line;
      end Get_File_And_Line;

   begin
      --  Default values if nothing better is found
      Addr := Invalid_Address;
      Name := Null_Unbounded_String;
      Line := 0;

      Matched (0) := No_Match;

      loop
         Match (File_Name_Pattern, Str (Start .. Str'Last), Matched2);
         exit when Matched2 (0) = No_Match;
         Matched := Matched2;
         Start := Matched (0).Last + 1;
      end loop;

      if Matched (0) /= No_Match then
         if Matched (1) /= No_Match then
            Set_Unbounded_String
              (Name, Str (Matched (1).First .. Matched (1).Last));
         end if;

         if Matched (2) /= No_Match then
            if Str (Matched (2).First) = ':' then
               Line := Natural'Value
                 (Str (Matched (2).First + 1 .. Matched (2).Last));

            else
               Line := Natural'Value
                 (Str (Matched (2).First .. Matched (2).Last));
            end if;
         end if;

         if Matched (3) /= No_Match then
            Addr := String_To_Address
              (Str (Matched (3).First .. Matched (3).Last));
         end if;
         return;
      end if;

      Match (Frame_Pattern, Str, Matched);
      if Matched (0) /= No_Match then
         Debugger.Current_Frame := Null_Frame_Info;

         declare
            use Token_Lists;

            Tokens : Token_List_Controller;
            C      : Token_Lists.Cursor;
         begin
            Tokens.List := Build_Tokens
              (Str (Matched (2).First .. Matched (2).Last));

            C := Find_Identifier (First (Tokens.List), "level");
            if C /= Token_Lists.No_Element then
               Next (C, 2);
               Debugger.Current_Frame.Frame :=
                 Integer'Value (Element (C).Text.all);
            end if;

            C := Find_Identifier (First (Tokens.List), "addr");
            if C /= Token_Lists.No_Element then
               Next (C, 2);
               Debugger.Current_Frame.Addr := String_To_Address
                 (Element (C).Text.all);
            end if;

            C := Find_Identifier (First (Tokens.List), "fullname");
            if C /= Token_Lists.No_Element then
               Next (C, 2);
               Debugger.Current_Frame.File := To_Unbounded_String
                 (Strip_Escape (Element (C).Text.all));
            end if;

            C := Find_Identifier (First (Tokens.List), "line");
            if C /= Token_Lists.No_Element then
               Next (C, 2);
               Debugger.Current_Frame.Line := Integer'Value
                 (Element (C).Text.all);
            end if;
         end;

         Addr := Debugger.Current_Frame.Addr;
         Name := Debugger.Current_Frame.File;
         Line := Debugger.Current_Frame.Line;
         return;
      end if;

      Match (Line_Pattern, Str, Matched);
      if Matched (0) /= No_Match then
         Debugger.Current_Frame.Frame := 0;
         Debugger.Current_Frame.Addr := String_To_Address
           (Str (Matched (4).First .. Matched (4).Last));
         Debugger.Current_Frame.File := To_Unbounded_String
           (Strip_Escape (Str (Matched (2).First .. Matched (2).Last)));
         Debugger.Current_Frame.Line := Integer'Value
           (Str (Matched (1).First .. Matched (1).Last));

         Addr := Debugger.Current_Frame.Addr;
         Name := Debugger.Current_Frame.File;
         Line := Debugger.Current_Frame.Line;
         return;
      end if;

      Match (CLI_Frame_Pattern, Str, Matched);
      if Matched (0) /= No_Match then
         Debugger.Current_Frame := Null_Frame_Info;

         Debugger.Current_Frame.Frame :=
           Integer'Value (Str (Matched (1).First .. Matched (1).Last));

         if Matched (3) /= No_Match then
            Debugger.Current_Frame.Addr := String_To_Address
              (Str (Matched (3).First .. Matched (3).Last));
         end if;

         if Matched (6) /= No_Match then
            Get_File_And_Line (Str (Matched (6).First .. Matched (6).Last));
         end if;

         Addr := Debugger.Current_Frame.Addr;
         return;
      end if;

      Match (Main_Task_Pattern, Str, Matched);
      if Matched (0) /= No_Match then
         Debugger.Current_Frame := Null_Frame_Info;

         if Matched (2) /= No_Match then
            Debugger.Current_Frame.Addr := String_To_Address
              (Str (Matched (2).First .. Matched (2).Last));
         end if;

         Get_File_And_Line (Str (Matched (3).First .. Matched (3).Last));

         Addr := Debugger.Current_Frame.Addr;
      end if;

   exception
      when E : others =>
         Me.Trace (E);

         Addr := Invalid_Address;
         Name := Null_Unbounded_String;
         Line := 0;
   end Found_File_Name;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   overriding procedure Found_Frame_Info
     (Debugger : access Gdb_MI_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type)
   is
      Info    : Frame_Info;
      Matched : Match_Array (0 .. 6);

   begin
      Frame := Null_Unbounded_String;

      Info := Parse_Frame_Info (Str);
      if Info /= Null_Frame_Info then
         Debugger.Current_Frame := Info;

         if Info.Addr = GVD.Types.Invalid_Address then
            Message := No_Debug_Info;
         else
            Message := Location_Found;
            Frame   := To_Unbounded_String (Info.Frame'Img);
         end if;

      else
         Match (CLI_Frame_Pattern, Str, Matched);

         if Matched (0) /= No_Match then
            Debugger.Current_Frame.Frame := Natural'Value
              (Str (Matched (1).First .. Matched (1).Last));

            Frame := To_Unbounded_String
              (Str (Matched (1).First .. Matched (1).Last));
            Message := Location_Found;
         else
            Message := No_Debug_Info;
         end if;
      end if;
   end Found_Frame_Info;

   --------------------
   -- Running_Filter --
   --------------------

   procedure Running_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Matched, Str);
   begin
      Gdb_MI_Debugger (Process.Debugger.all).Is_Running := True;
   end Running_Filter;

   --------------------
   -- Stopped_Filter --
   --------------------

   procedure Stopped_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Matched);
      use Token_Lists;

      Debugger : Gdb_MI_Debugger renames
        Gdb_MI_Debugger (Process.Debugger.all);

      Tokens   : Token_List_Controller;
      C        : Token_Lists.Cursor;
      Idx      : Integer;
   begin
      Debugger.Is_Running    := False;
      Debugger.Current_Frame := Null_Frame_Info;

      --  Set the debugger as started if it's not the case yet. This can
      --  happen when attaching a debugger to an existing process.

      if not Debugger.Is_Started then
         Debugger.Set_Is_Started (Attached);
      end if;

      Idx := Index (Str, "frame={");
      if Idx < Str'First then
         return;
      end if;

      declare
         Frame : constant String := Collect
           (Str (Idx + 7 .. Str'Last), '}', '{');
      begin
         Tokens.List := Build_Tokens (Frame);
      end;

      C := Find_Identifier (First (Tokens.List), "addr");
      if C = Token_Lists.No_Element then
         return;
      end if;
      Next (C, 2);
      Debugger.Current_Frame.Addr := String_To_Address (Element (C).Text.all);

   exception
      when E : others =>
         Me.Trace (E);
   end Stopped_Filter;

   ----------
   -- Free --
   ----------

   procedure Free is
     new Standard.Ada.Unchecked_Deallocation (Node, Node_Access);

   procedure Free (Debugger : access Gdb_MI_Debugger; Var : in out Variable)
   is
      use Nodes_Vectors;

      procedure Free    (Nodes : in out Nodes_Vectors.Vector);
      procedure Process (Node  : in out Node_Access);

      ----------
      -- Free --
      ----------

      procedure Free (Nodes : in out Nodes_Vectors.Vector) is

      begin
         for Item of Nodes loop
            Process (Item);
         end loop;
         Clear (Nodes);
      end Free;

      -------------
      -- Process --
      -------------

      procedure Process (Node : in out Node_Access) is
      begin
         if Node.Has_Childs then
            Free (Node.Childs);
         end if;
         Free (Node);
      end Process;

   begin
      if Var.Name /= Null_Unbounded_String then
         Debugger.Send
           ("-var-delete " & To_String (Var.Name), Mode => Internal);
      end if;

      Free (Var.Nodes);
   end Free;

   -----------------------
   -- Source_Files_List --
   -----------------------

   overriding function Source_Files_List
     (Debugger : access Gdb_MI_Debugger) return GNAT.Strings.String_List
   is
      use Token_Lists;
      Tokens   : Token_List_Controller;
      Start, C : Token_Lists.Cursor;
      Count    : Natural := 0;

   begin
      declare
         Block : Process_Proxies.Parse_File_Switch
           (Debugger.Process) with Unreferenced;
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-file-list-exec-source-files", Mode => Internal);
      begin
         Tokens.List := Build_Tokens (S);
      end;

      C := Find_Identifier (First (Tokens.List), "files");

      if C = Token_Lists.No_Element then
         return (1 .. 0 => <>);
      end if;

      --  Skip files=[
      Next (C, 3);
      Start := C;

      --  Count number of files
      loop
         C := Find_Identifier (C, "fullname");
         exit when C = Token_Lists.No_Element;
         Count := Count + 1;
         Next (C);
      end loop;

      declare
         Result : String_List (1 .. Count);
      begin
         C := Start;

         for J in Result'Range loop
            C := Find_Identifier (C, "fullname");
            Next (C, 2);
            Result (J) := new String'(Strip_Escape (Element (C).Text.all));
         end loop;

         return Result;
      end;
   end Source_Files_List;

   ----------------------
   -- List_Breakpoints --
   ----------------------

   overriding procedure List_Breakpoints
     (Debugger  : not null access Gdb_MI_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector)
   is
      use Token_Lists;
      Tokens : Token_List_Controller;
      C      : Token_Lists.Cursor;
      Tmp    : Token_Lists.Cursor;

      procedure Read_Bkpt;

      ---------------
      -- Read_Bkpt --
      ---------------

      procedure Read_Bkpt is
         B        : Breakpoint_Data;
         Multiple : Boolean := False;
         Pending  : Boolean := False;

         procedure Read_Breakpoint_File;
         procedure Read_Neasted_Bkpt;
         procedure Parse_What;

         ----------------
         -- Parse_What --
         ----------------

         procedure Parse_What is
            Matched : Match_Array (0 .. 10);
         begin
            Match (Exception_In_Breakpoint, Element (C).Text.all, Matched);
            if Matched (0) /= No_Match then
               if Matched (1) /= No_Match then
                  if Matched (3) /= No_Match then
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (3).First .. Matched (3).Last));
                  else
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (2).First .. Matched (2).Last));
                  end if;
               else
                  if Matched (6) /= No_Match then
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (6).First .. Matched (6).Last));

                  elsif Matched (5) /= No_Match then
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (5).First .. Matched (5).Last));
                  else
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (4).First .. Matched (4).Last));
                  end if;
               end if;

            elsif Starts_With
              (Element (C).Text.all, "failed Ada assertions")
            then
               B.Except := To_Unbounded_String ("assertions");

            else
               B.Expression := To_Unbounded_String
                 (Element (C).Text.all);
            end if;
         end Parse_What;

         --------------------------
         -- Read_Breakpoint_File --
         --------------------------

         procedure Read_Breakpoint_File is
            F    : Virtual_File       := No_File;
            Line : Editable_Line_Type := 0;
         begin
            loop
               if Pending then
                  if Element (C).Text.all = "pending" then
                     Next (C, 2);
                     declare
                        File_And_Line : constant Unbounded_String_Array :=
                          GNATCOLL.Utils.Split
                            (Str => Element (C).Text.all,
                             On               => ':',
                             Omit_Empty_Lines => True);
                        File          : constant String := To_String
                          (File_And_Line (File_And_Line'First));
                        Line_Str      : constant String := To_String
                          (File_And_Line (File_And_Line'Last));
                     begin
                        F := Debugger.Get_Kernel.Create_From_Base (+File);
                        Line := Editable_Line_Type'Value (Line_Str);
                     exception
                        when others =>
                           --  The pattern file:line was not matched
                           null;
                     end;
                  else
                     exit;
                  end if;
               else
                  if Element (C).Text.all = "func" then
                     Next (C, 2);
                     B.Subprogram := To_Unbounded_String
                       (Element (C).Text.all);

                  elsif Element (C).Text.all = "file" then
                     Next (C, 2);
                     F := Debugger.Get_Kernel.Create_From_Base
                       (+Element (C).Text.all);

                  elsif Element (C).Text.all = "fullname" then
                     Next (C, 2);
                     F := GPS.Core_Kernels.To_File
                       (Kernel, Strip_Escape (Element (C).Text.all));

                  elsif Element (C).Text.all = "line" then
                     Next (C, 2);
                     Line := Editable_Line_Type'Value
                       (Element (C).Text.all);

                  elsif Element (C).Text.all = "what" then
                     Next (C, 2);
                     Parse_What;

                  else
                     exit;
                  end if;
               end if;

               Next (C, 2);
            end loop;

            --  Convert from a path returned by the debugger to the actual
            --  path in the project, in case sources have changed.

            if not F.Is_Absolute_Path or else not F.Is_Regular_File then
               F := Debugger.Kernel.Create_From_Base (F.Full_Name);
            end if;

            if Line /= 0 then
               B.Location := Kernel.Get_Buffer_Factory.Create_Marker
                 (File   => F,
                  Line   => Line,
                  Column => 1);
            end if;
         end Read_Breakpoint_File;

         -----------------------
         -- Read_Neasted_Bkpt --
         -----------------------

         procedure Read_Neasted_Bkpt is
            C1 : Token_Lists.Cursor := Find_Identifier (C, "number");
         begin
            if C1 = Token_Lists.No_Element then
               return;
            end if;

            Next (C1, 2);
            if Index (Element (C1).Text.all, ".") <
              Element (C1).Text'First
            then
               return;
            end if;

            C1 := Find_Identifier (C1, "addr");
            if C1 = Token_Lists.No_Element then
               return;
            end if;
            Next (C1, 2);
            B.Address := String_To_Address (Element (C1).Text.all);

            Next (C1, 2);
            C := C1;
            Read_Breakpoint_File;
         end Read_Neasted_Bkpt;

      begin
         C := Find_Identifier (C, "number");
         Next (C, 2);
         B.Num := Breakpoint_Identifier'Value (Element (C).Text.all);

         C := Find_Identifier (C, "type");
         Next (C, 2);
         if Index (Element (C).Text.all, "breakpoint") in
           Element (C).Text.all'Range
         then
            B.The_Type := Breakpoint;

         elsif Index (Element (C).Text.all, "watchpoint") in
           Element (C).Text.all'Range
         then
            B.The_Type := Watchpoint;

         elsif Index
           (Element (C).Text.all, "catchpoint") in Element (C).Text.all'Range
         then
            B.The_Type := Catchpoint;

         else
            B.The_Type := Other;
            B.The_Type_Name := To_Unbounded_String (Element (C).Text.all);
         end if;

         C := Find_Identifier (C, "disp");
         Next (C, 2);

         if Element (C).Text.all = "keep" then
            B.Disposition := Keep;

         elsif Element (C).Text.all = "disable" then
            B.Disposition := Disable;

         else
            B.Disposition := Delete;
         end if;

         C := Find_Identifier (C, "enabled");
         Next (C, 2);
         B.Enabled := Element (C).Text.all = "y";

         --  Try to find the breakpoint's address. If not present, go to the
         --  next identifier token: this can happen for exception catchpoints
         --  for instance (e.g: "catch exception Program_Error" -> there will
         --  be no address in this case).

         Tmp := Find_Identifier (C, "addr");
         if Tmp /= No_Element then
            C := Tmp;
            Next (C, 2);

            if Element (C).Text.all = "<MULTIPLE>" then
               Multiple := True;
            elsif Element (C).Text.all = "<PENDING>" then
               Pending := True;
            else
               B.Address := String_To_Address (Element (C).Text.all);
            end if;

            --  ??? missing Trigger & Expression for watchpoints

            Next (C, 2);
            if not Multiple
              and then B.The_Type = Breakpoint
            then
               Read_Breakpoint_File;
            end if;
         else
            Next (C, 2);
         end if;

         loop
            exit when Element (C).Code /= Identifier;

            declare
               Name : constant String := Element (C).Text.all;
            begin
               Next (C, 2);

               if Name = "cond" then
                  B.Condition := To_Unbounded_String (Element (C).Text.all);

               elsif Name = "ignore" then
                  B.Ignore := Integer'Value (Element (C).Text.all);

               elsif Name = "what" then
                  Parse_What;

               elsif Name = "thread-groups" then
                  while Element (C).Code /= R_Bracket loop
                     Next (C);
                  end loop;

               elsif Name = "script" then
                  Next (C); -- skip '{'

                  if Element (C).Code = L_Bracket then
                     --  skip '[' on newer GDB versions, since script
                     --  instuctions are now provided in a list.
                     Next (C);
                  end if;

                  while Element (C).Code not in R_Brace | R_Bracket loop
                     if Length (B.Commands) > 0 then
                        Append (B.Commands, "" & ASCII.LF);
                     end if;

                     Append (B.Commands, Element (C).Text.all);
                     Next (C);
                     if Element (C).Code = Comma then
                        Next (C);
                     end if;
                  end loop;

                  if Element (C).Code = R_Bracket then
                     --  skip ']' on newer GDB versions, since script
                     --  instuctions are now provided in a list.
                     Next (C);
                  end if;

                  Next (C); -- skip '}'
               end if;
            end;

            Next (C, 2);
         end loop;

         if Multiple
           and then B.The_Type = Breakpoint
         then
            Read_Neasted_Bkpt;
         end if;

         List.Append (B);
      end Read_Bkpt;

   begin
      List.Clear;

      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-break-list", Mode => Internal);
      begin
            Tokens.List := Build_Tokens (S);

      exception
         when E : others =>
            Me.Trace (E);
            return;
      end;

      C := Find_Identifier (First (Tokens.List), "body");
      if C = Token_Lists.No_Element then
         return;
      end if;

      loop
         C := Find_Identifier (C, "bkpt");
         exit when C = Token_Lists.No_Element;
         Read_Bkpt;
      end loop;
   end List_Breakpoints;

   ------------------------
   -- Enable_Breakpoints --
   ------------------------

   overriding procedure Enable_Breakpoints
     (Debugger    : access Gdb_MI_Debugger;
      Breakpoints : GVD.Types.Breakpoint_Identifier_Lists.List;
      Enable      : Boolean := True;
      Mode        : Command_Type := Hidden)
   is
      Cmd : Unbounded_String := (if Enable then
                                    To_Unbounded_String ("-break-enable")
                                 else
                                    To_Unbounded_String ("-break-disable"));
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
     (Debugger    : access Gdb_MI_Debugger;
      Breakpoints : GVD.Types.Breakpoint_Identifier_Lists.List;
      Mode        : Command_Type := Hidden)
   is
      Cmd : Unbounded_String := To_Unbounded_String ("delete");
   begin
      for Breakpoint of Breakpoints loop
         Cmd := Cmd & Breakpoint_Identifier'Image (Breakpoint);
      end loop;

      Debugger.Send (To_String (Cmd), Mode => Mode);
   end Remove_Breakpoints;

   ---------------------
   -- List_Exceptions --
   ---------------------

   overriding function List_Exceptions
     (Debugger : access Gdb_MI_Debugger)
     return GVD.Types.Exception_Array
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("-info-ada-exceptions", Mode => Internal);

      use Token_Lists;
      Tokens : Token_List_Controller;
      C, B   : Token_Lists.Cursor;
      Nums   : Natural := 0;

      Empty : Exception_Array (1 .. 0);
   begin
      Tokens.List := Build_Tokens (S);
      C := Find_Identifier (First (Tokens.List), "body");

      if C = Token_Lists.No_Element then
         return Empty;
      end if;

      --  Skip body=[
      Next (C, 3);
      B := C;

      --  Count the number of exceptions listed
      loop
         C := Find_Identifier (C, "name");
         exit when C = Token_Lists.No_Element;
         Nums := Nums + 1;
         Next (C, 2);
      end loop;

      if Nums = 0 then
         return Empty;
      end if;

      C := B;
      declare
         Arr : Exception_Array (1 .. Nums);
         Num : Natural := 1;
      begin
         loop
            C := Find_Identifier (C, "name");
            exit when C = Token_Lists.No_Element;
            Next (C, 2);
            Arr (Num).Name := To_Unbounded_String (Element (C).Text.all);
            Num := Num + 1;
         end loop;

         return Arr;
      end;
   end List_Exceptions;

   -------------------
   -- Get_Type_Info --
   -------------------

   overriding function Get_Type_Info
     (Debugger  : access Gdb_MI_Debugger;
      Entity    : String;
      Default   : String) return String
   is
      V      : Variable := Debugger.Create_Var (Entity);
      Result : constant String := To_String (V.Var_Type);
   begin
      if V.Name = "" or else Result = "" then
         Free (Debugger, V);
         return Default;
      else
         Free (Debugger, V);
         return Result;
      end if;
   end Get_Type_Info;

   ---------------
   -- Find_File --
   ---------------

   overriding function Find_File
     (Debugger : access Gdb_MI_Debugger; File_Name : String) return String is
   begin
      --  Given that we no longer process graphic events when sending
      --  commands to the debugger, Command_In_Process should never be true
      --  here, but in any case, let's have this additional protection:

      if Debugger.Get_Process.Command_In_Process then
         return File_Name;
      end if;

      declare
         Block : Process_Proxies.Parse_File_Switch
           (Debugger.Process) with Unreferenced;
         Str  : constant String := Debugger.Send_And_Get_Clean_Output
           ("info line " & File_Name & ":1", Mode => Internal);
         File : Unbounded_String;
         Line : Natural;
         Addr : GVD.Types.Address_Type;

      begin
         Debugger.Found_File_Name (Str, File, Line, Addr);

         if Length (File) = 0 then
            return File_Name;
         else
            return To_String (File);
         end if;
      end;
   end Find_File;

   ------------------------
   -- Parse_Disassembled --
   ------------------------

   procedure Parse_Disassembled
     (S    : String;
      Code : out Disassemble_Elements)
   is
      use Token_Lists;

      procedure Parse_Element
        (Cursor : in out Token_Lists.Cursor;
         Item   : out Disassemble_Element);
      --  Parse tokens for one instruction {address="...",func-name="...",...}

      procedure Parse_Element
        (Cursor : in out Token_Lists.Cursor;
         Item   : out Disassemble_Element)
      is
         Name   : Unbounded_String;
         Offset : Unbounded_String;
      begin
         pragma Assert (Element (Cursor).Code = L_Brace);
         Next (Cursor, 1);  --  Skip '{'

         while Element (Cursor).Code /= R_Brace loop
            if Element (Cursor).Code = Identifier then
               declare
                  Text : constant String := Element (Cursor).Text.all;
               begin
                  Next (Cursor, 2);  --  Skip 'name', '='

                  if Text = "address" then
                     Item.Address := String_To_Address
                       (Element (Cursor).Text.all);
                  elsif Text = "func-name" then
                     Name := To_Unbounded_String (Element (Cursor).Text.all);
                  elsif Text = "offset" then
                     Offset := To_Unbounded_String (Element (Cursor).Text.all);
                  elsif Text = "inst" then
                     Item.Instr :=
                       To_Unbounded_String (Element (Cursor).Text.all);
                  elsif Text = "opcodes" then
                     Item.Opcodes :=
                       To_Unbounded_String (Element (Cursor).Text.all);
                  end if;

                  Next (Cursor, 1);  --  Skip 'value'
               end;
            elsif Element (Cursor).Code = Comma then
               Next (Cursor, 1);  --  Skip ','
            end if;
         end loop;

         Next (Cursor, 1);  --  Skip '}'

         Item.Method_Offset := Name & "+" & Offset;
      end Parse_Element;

      Tokens       : Token_List_Controller;
      C            : Token_Lists.Cursor;
      Matched      : Match_Array (0 .. 2);

   begin
      Match (Error_Pattern, S, Matched);

      if Matched (0) /= No_Match then
         return;
      end if;

      Tokens.List := Build_Tokens (S);

      --  Look for '['
      for J in Tokens.List.Iterate loop
         if Element (J).Code = L_Bracket then
            C := J;
            exit;
         end if;
      end loop;

      if not Has_Element (C) then
         return;
      end if;

      Next (C, 1);  --  Skip '['

      while C /= Token_Lists.No_Element loop
         if Element (C).Code = L_Brace then
            declare
               El : Disassemble_Element;
            begin
               Parse_Element (C, El);
               Code.Append (El);
            end;
         elsif Element (C).Code = Comma then
            Next (C, 1);  --  Skip ','
         elsif Element (C).Code = R_Bracket then
            Next (C, 1);  --  Skip ']'
            exit;
         end if;
      end loop;
   end Parse_Disassembled;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_MI_Debugger;
      Range_Start   : out Address_Type;
      Range_End     : out Address_Type;
      Code          : out Disassemble_Elements;
      Start_Address : Address_Type := GVD.Types.Invalid_Address;
      End_Address   : Address_Type := GVD.Types.Invalid_Address)
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
              ("-data-disassemble -s $pc -e $pc+1 -- 2", Mode => Internal),
            Code);
      else
         Parse_Disassembled
           (Debugger.Send_And_Get_Clean_Output
              ("-data-disassemble -s " & S & " -e " & E & " -- 2",
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
     (Debugger : access Gdb_MI_Debugger;
      File     : String;
      From     : Natural;
      To       : Natural;
      Code     : out Disassemble_Elements)
   is
      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

   begin
      Parse_Disassembled
        (Debugger.Send_And_Get_Clean_Output
           ("-data-disassemble -f " & File & " -l" &
              From'Img & " -n" &
            (if To > 0
               then Natural'Image (To - From + 1)
               else " -1") &
              " -- 2", Mode => Internal),
         Code);
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   overriding procedure Get_Line_Address
     (Debugger    : access Gdb_MI_Debugger;
      Line        : Natural;
      File        : GNATCOLL.VFS.Virtual_File;
      Range_Start : out Address_Type;
      Range_End   : out Address_Type)
   is
      pragma Unreferenced (File);

      Block : Process_Proxies.Parse_File_Switch
        (Debugger.Process) with Unreferenced;

   begin

      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("info line" & Line'Img, Mode => Internal);
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

   end Get_Line_Address;

   ----------------
   -- Get_Memory --
   ----------------

   overriding function Get_Memory
     (Debugger : access Gdb_MI_Debugger;
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

      Endian : constant Endian_Type := Debugger.Get_Endian_Type;

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

               --  Detect actual data : 0xXX... right after an \t
               if S (S_Index) = '0'
                 and then S_Index - 2 > S'First
                 and then S (S_Index - 1) = 't'
                 and then S (S_Index - 1) = '\'
               then
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
            --  Detect actual data : 0xXX... right after an \t

            if S (S_Index) = '0'
              and then S_Index - 2 > S'First
              and then S (S_Index - 1) = 't'
              and then S (S_Index - 2) = '\'
            then
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
     (Debugger : access Gdb_MI_Debugger;
      Address  : String;
      Byte     : String) is
   begin
      Debugger.Send
        ("-data-write-memory-bytes " & Address & " 0x" & Byte,
         Mode => Internal);
   end Put_Memory_Byte;

   ----------------------
   -- Parse_Frame_Info --
   ----------------------

   Frame_Contents_Pattern : constant Pattern_Matcher := Compile
     ("frame={(.*)}", Multiple_Lines);

   function Parse_Frame_Info (Info : String) return Frame_Info
   is
      use Token_Lists;

      Result  : Frame_Info := Null_Frame_Info;

      Matched : Match_Array (0 .. 5);
      Tokens  : Token_List_Controller;
      C, C1   : Token_Lists.Cursor;

   begin
      --  Default values if nothing better is found
      Match (Frame_Contents_Pattern, Info, Matched);
      if Matched (0) = No_Match then
         return Null_Frame_Info;
      end if;

      Tokens.List := Build_Tokens
        (Info (Matched (1).First .. Matched (1).Last));

      C := Find_Identifier (First (Tokens.List), "level");

      if C /= Token_Lists.No_Element then
         Next (C, 2);
         Result.Frame := Integer'Value (Element (C).Text.all);

         C1 := Find_Identifier (C, "addr");
         if C1 /= Token_Lists.No_Element then
            Next (C1, 2);
            Result.Addr := String_To_Address (Element (C1).Text.all);
         end if;

         C1 := Find_Identifier (C, "fullname");
         if C1 /= Token_Lists.No_Element then
            Next (C1, 2);
            Result.File := To_Unbounded_String (Element (C1).Text.all);
         end if;

         C1 := Find_Identifier (C, "line");
         if C1 /= Token_Lists.No_Element then
            Next (C1, 2);
            Result.Line := Natural'Value (Element (C1).Text.all);
         end if;
      end if;

      return Result;

   exception
      when E : others =>
         Trace (Me, E);
         return Null_Frame_Info;
   end Parse_Frame_Info;

   ----------------
   -- Create_Var --
   ----------------

   function Create_Var
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String)
      return Variable
   is
      Result : Variable;
      S : constant String := Debugger.Send_And_Get_Output
        ("-var-create - * " & '"' & Entity & '"', Mode => Internal);

      use Token_Lists;
      Tokens : Token_List_Controller;
      T      : Token_Lists.Cursor;
      Dummy  : Boolean;

   begin
      --  name="var2",numchild="0",value="5",type="positive",has_more="0"
      Tokens.List := Build_Tokens (S);
      T := Find_Identifier (First (Tokens.List), "name");

      if T /= Token_Lists.No_Element then
         Next (T, 2);
         Result.Name := To_Unbounded_String (Element (T).Text.all);

         T := Find_Identifier (T, "numchild");
         if T /= Token_Lists.No_Element then
            Next (T, 2);
            Result.Childs := Natural'Value (Element (T).Text.all);
         end if;

         T := Find_Identifier (T, "type");
         if T /= Token_Lists.No_Element then
            Next (T, 2);
            Result.Var_Type := To_Unbounded_String (Element (T).Text.all);
         end if;
         return Result;
      end if;

      return Result;
   end Create_Var;

      ---------------
      -- Get_Value --
      ---------------

   function Get_Value (S : String) return String is
      use Token_Lists;
      Tokens : Token_List_Controller;
      T      : Token_Lists.Cursor;

   begin
      Tokens.List := Build_Tokens (S);
      T := Find_Identifier (First (Tokens.List), "value");

      if T /= Token_Lists.No_Element then
         Next (T, 2);
         return Element (T).Text.all;
      end if;

      T := Find_Identifier (First (Tokens.List), "msg");
      if T /= Token_Lists.No_Element then
         Next (T, 2);
         return Element (T).Text.all;
      end if;

      return "";
   end Get_Value;

   --------------------------
   -- Get_Variable_Address --
   --------------------------

   overriding function Get_Variable_Address
     (Debugger : access Gdb_MI_Debugger;
      Variable : String) return String
   is
      S       : constant String := Debugger.Send_And_Get_Clean_Output
        ("-data-evaluate-expression &(" & Variable & ")",
         Mode => Internal);
      Matched : Match_Array (0 .. 1);
      Last    : Match_Array (0 .. 1);
   begin
      --  find last occurence
      Match (Address_Pattern, S, Matched);
      loop
         exit when Matched (1) = No_Match;
         Last := Matched;
         Match (Address_Pattern, S (Last (1).Last + 1 .. S'Last), Matched);
      end loop;

      if Last (1) /= No_Match then
         return S (Last (1).First .. Last (1).Last);
      else
         return "";
      end if;
   end Get_Variable_Address;

   ---------------------
   -- Get_Endian_Type --
   ---------------------

   overriding function Get_Endian_Type
     (Debugger : access Gdb_MI_Debugger) return Endian_Type is
   begin
      if Debugger.Endian /= Unknown_Endian then
         --  Return the cached value, to avoid too much communication with
         --  the underlying debugger.
         return Debugger.Endian;
      end if;

      declare
         S      : constant String := Debugger.Send_And_Get_Clean_Output
           ("show endian", Mode => Internal);
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
     (Debugger  : access Gdb_MI_Debugger;
      Beginning : String) return GNAT.Strings.String_List
   is
      S           : constant String := Debugger.Send_And_Get_Clean_Output
        ("complete " & Beginning, Mode => Internal);
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
         Last   : Natural := 0;
      begin
         while Last_Index < S'Last loop
            if S (Last_Index) = '&'
              or else S (Last_Index) = '^'
            then
               --  skip &"complete .." and ^done
               Skip_To_Char (S, Last_Index, ASCII.LF);
               Last_Index := Last_Index + 1;

            else
               --  line looks like ~"...\n", skip ~" at begin
               Skip_To_Char (S, Last_Index, '"');
               Last_Index := Last_Index + 1;
               First_Index := Last_Index;

               --  find ending '"'
               Skip_To_Char (S, Last_Index, '"');
               Last := Last + 1;

               --  get contents between ~"...\n"
               Result (Last) := new String'(S (First_Index .. Last_Index - 3));
               Skip_To_Char (S, Last_Index, ASCII.LF);
               Last_Index := Last_Index + 1;
            end if;
         end loop;

         return Result (1 .. Last);
      end;
   end Complete;

   use GVD.Proc_Utils;

   --------------------
   -- Open_Processes --
   --------------------

   overriding procedure Open_Processes (Debugger : access Gdb_MI_Debugger) is
   begin
      Open_Processes (Debugger.Handle, Debugger.Kernel);
   end Open_Processes;

   ------------------
   -- Next_Process --
   ------------------

   overriding procedure Next_Process
     (Debugger : access Gdb_MI_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean) is
   begin
      Next_Process (Debugger.Handle, Info, Success);
   end Next_Process;

   ---------------------
   -- Close_Processes --
   ---------------------

   overriding procedure Close_Processes (Debugger : access Gdb_MI_Debugger) is
   begin
      Close_Processes (Debugger.Handle);
   end Close_Processes;

   ---------------------
   -- Detect_Language --
   ---------------------

   overriding procedure Detect_Language (Debugger : access Gdb_MI_Debugger) is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("show lang", Mode => Internal);
      pragma Unreferenced (S);
   begin
      null;
   end Detect_Language;

   -----------------
   -- Support_TTY --
   -----------------

   overriding function Support_TTY
     (Debugger : access Gdb_MI_Debugger) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return True;
   end Support_TTY;

   -------------
   -- Set_TTY --
   -------------

   overriding procedure Set_TTY
     (Debugger : access Gdb_MI_Debugger; TTY : String) is
   begin
      if TTY /= "" then
         Debugger.Send ("-inferior-tty-set " & TTY, Mode => Hidden);
      end if;
   end Set_TTY;

   -------------------
   -- Filter_Output --
   -------------------

   overriding procedure Filter_Output
     (Debugger        : access Gdb_MI_Debugger;
      Mode            : GVD.Types.Command_Type;
      Str             : String;
      Console_Output  : out Unbounded_String;
      Log_Output      : out Unbounded_String;
      Debuggee_Output : out Unbounded_String;
      Results_Output  : out Unbounded_String)
   is
      J         : Integer := Str'First;
      New_Line  : Boolean := True;
      Ret_Value : Boolean := False;
      First_Log : Boolean := True;
      Log       : Unbounded_String;
      Target    : Unbounded_String;
      Error     : Unbounded_String;

      procedure Parse_And_Escape_String (Res : in out Unbounded_String);
      --  Parse the debugger output and return when '"' is found.
      --  The parsed output will be append to Res

      procedure Ignore_Line;
      --  Ignore all the character until a EOL character

      function Lookup (S : String) return Boolean;
      --  Check if the next characters matched S. If true consume S.

      -----------------------------
      -- Parse_And_Escape_String --
      -----------------------------

      procedure Parse_And_Escape_String (Res : in out Unbounded_String) is
      begin
         while J <= Str'Last and then Str (J) /= '"' loop
            if Str (J) /= '\'
              or else J = Str'Last
            then
               Append (Res, Str (J));
            else
               J := J + 1;
               case Str (J) is
                  when '\' | '"' =>
                     Append (Res, Str (J));
                  when 'n' =>
                     Append (Res, ASCII.LF);
                  when 'r' =>
                     --  Remove the "\r" we are only handling the "\n"
                     null;
                  when 't' =>
                     Append (Res, ASCII.HT);
                  when others =>
                     Append (Res, Str (J - 1 .. J));
               end case;
            end if;

            J := J + 1;
         end loop;
      end Parse_And_Escape_String;

      -----------------
      -- Ignore_Line --
      -----------------

      procedure Ignore_Line is
      begin
         while J <= Str'Last and then Str (J) /= ASCII.LF loop
            J := J + 1;
         end loop;
      end Ignore_Line;

      ------------
      -- Lookup --
      ------------

      function Lookup (S : String) return Boolean is
      begin
         if J + S'Length - 1 < Str'Last
           and then Str (J .. J + S'Length - 1) = S
         then
            J := J + S'Length;
            return True;
         end if;
         return False;
      end Lookup;

      Is_Thread_Msg : Boolean;
   begin
      --  We need to parse the 3 following streams:
      --  "~" string-output: The console output stream contains text that
      --  should be displayed in the CLI console window. It contains the
      --  textual responses to CLI commands.
      --  "@" string-output: The target output stream contains any textual
      --  output from the running target.
      --  "&" string-output: The log stream contains debugging messages being
      --  produced by GDB's internals.

      while J <= Str'Last loop
         if Str (J) = ASCII.LF then
            --  Suppress all the ASCII.LF, the multilines are already done
            --  via \n and \r
            null;
         elsif New_Line and then J + 2 < Str'Last then

            --  Suppress the digits at the beginning of the line
            --  result-record ==>
            --         [ token ] "^" result-class ( "," result )* nl
            --  token ==>
            --         any sequence of digits.
            while Str (J) in '0' .. '9' loop
               J := J + 1;
            end loop;

            if Str (J) = '~'
              and then Str (J + 1) = '"'
            then
               --  Replace ~"...." by ....
               J := J + 2;
               Parse_And_Escape_String (Console_Output);

            elsif Str (J) = '&'
              and then Str (J + 1) = '"'
            then
               --  & indicates a log output: we need to keep it to retrieve
               --  the error messages coming from the CLI commands
               J := J + 2;
               Parse_And_Escape_String (Log);

               if First_Log then
                  --  The first log contains the command, ignore it
                  First_Log := False;
                  Log := Null_Unbounded_String;
               end if;

            elsif Str (J) = '@'
              and then Str (J + 1) = '"'
            then
               --  We need to parse the target stream: however this is unused
               --  for now.
               J := J + 2;
               Parse_And_Escape_String (Target);

            elsif Lookup ("^done") then
               declare
                  Added : Boolean := False;
               begin
                  if Lookup (",value=""") then
                     --  result for the print variable command
                     Append (Console_Output, "value=""");
                     while J <= Str'Last and then Str (J) /= '"' loop
                        Append (Console_Output, Str (J));
                        J := J + 1;
                     end loop;
                     Append (Console_Output, '"' & ASCII.LF);
                     J := J + 1;

                  else
                     --  If we have extra information after the ^done then
                     --  we need to skip the next character which will be ','
                     --  and we need to add a new line.
                     while J <= Str'Last and then Str (J) /= ASCII.LF loop
                        if not Added then
                           Added := True;
                        else
                           Append (Results_Output, Str (J));
                        end if;
                        J := J + 1;
                     end loop;

                     if Added then
                        Append (Results_Output, ASCII.LF);
                     end if;
                  end if;
               end;

            elsif Str (J) in '^' | '*' | '=' then
               --  Strip [^*=]...
               J := J + 1;

               if Lookup ("error,msg=""") then
                  Parse_And_Escape_String (Error);
                  --  There is no '\n' at the end of the error message
                  Append (Error, ASCII.LF);
                  Ignore_Line;

               else
                  if Lookup ("stopped") then
                     --  Display info to the user
                     Append (Console_Output, "[program stopped");

                     if Lookup (",reason=""") then
                        Append (Console_Output, ": ");

                        while J <= Str'Last and then Str (J) /= '"' loop
                           Append (Console_Output, Str (J));
                           J := J + 1;
                        end loop;
                     end if;

                     Append (Console_Output, "]" & ASCII.LF);

                     loop
                        --  looking for a return value
                        if not Ret_Value
                          and then Lookup ("gdb-result-var=""")
                        then
                           Append (Console_Output, "Return value: ");
                           while J <= Str'Last and then Str (J) /= '"' loop
                              Append (Console_Output, Str (J));
                              J := J + 1;
                           end loop;

                           Append (Console_Output, "=");
                           Ret_Value := True;
                        end if;

                        if Ret_Value
                          and then Lookup ("return-value=""")
                        then
                           while J <= Str'Last and then Str (J) /= '"' loop
                              Append (Console_Output, Str (J));
                              J := J + 1;
                           end loop;

                           Append (Console_Output, "" & ASCII.LF);
                           exit;
                        end if;

                        exit when J > Str'Last or else Str (J) = ASCII.LF;
                        J := J + 1;
                     end loop;

                  elsif Str (J - 1) = '^'
                    and then Lookup ("running")
                  then
                     --  Display info to the user
                     Append (Console_Output, "[program running]" & ASCII.LF);

                  elsif Str (J - 1) = '*'
                    and then Lookup ("running")
                  then
                     --  T107-038 do not flood by "thread" messages
                     Is_Thread_Msg := Lookup (",thread-id=");

                     if not Is_Thread_Msg then
                        --  Display info to the user
                        Append (Console_Output, "[program running");
                     end if;

                     while J <= Str'Last and then Str (J) /= ASCII.LF loop
                        if not Is_Thread_Msg then
                           Append (Console_Output, Str (J));
                        end if;
                        J := J + 1;
                     end loop;

                     if not Is_Thread_Msg then
                        Append (Console_Output, "]" & ASCII.LF);
                     end if;

                     J := J + 1;
                  end if;

                  Ignore_Line;
               end if;

            elsif Lookup ("Sending packet:") then
               Ignore_Line;

            elsif Lookup ("Packet received:") then
               Ignore_Line;

            elsif Lookup (Prompt_String) then
               Append (Console_Output, Prompt_String);
               J := J + Prompt_String'Length;
            else
               Parse_And_Escape_String (Console_Output);
            end if;

         elsif Lookup (Prompt_String) then
            Append (Console_Output, Prompt_String);
            J := J + Prompt_String'Length;
         else
            Parse_And_Escape_String (Console_Output);
         end if;

         New_Line := J <= Str'Last and then Str (J) = ASCII.LF;
         J := J + 1;
      end loop;

      --  Add the error messages
      if Error /= Null_Unbounded_String then
         Append (Error, Console_Output);
         Console_Output := Error;
      end if;

      Debuggee_Output := Target;
      Log_Output := Log;
   end Filter_Output;

   ---------------------
   -- Is_Quit_Command --
   ---------------------

   overriding function Is_Quit_Command
     (Debugger : access Gdb_MI_Debugger;
      Command : String) return Boolean
   is
      pragma Unreferenced (Debugger);
   begin
      return Match (Is_Quit_Pattern, Command);
   end Is_Quit_Command;

end Debugger.Base_Gdb.Gdb_MI;

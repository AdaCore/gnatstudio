------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Finalization;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Tribooleans;      use GNATCOLL.Tribooleans;

with Config;                    use Config;
with Default_Preferences;       use Default_Preferences;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
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

   Me : constant Trace_Handle := Create ("Gdb_MI", Off);

   ---------------
   -- Constants --
   ---------------

   Prompt_Regexp             : constant Pattern_Matcher :=
     Compile ("^\(gdb\) ", Multiple_Lines);
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
     ("^(=breakpoint-|\^done,bkpt=)", Multiple_Lines);
   --  Pattern used to detect when breakpoints are created/deleted

   Breakpoint_Num_Pattern    : constant Pattern_Matcher := Compile
     ("^(=breakpoint-created|\^done),bkpt={number=""(\d+)");
   --  Pattern to match the breakpoint number after we just created one.

   Stopped_Regexp            : constant Pattern_Matcher := Compile
     ("^\*stopped,", Multiple_Lines + Single_Line);
   --  Pattern used to detect when the debuggee stops

   Terminate_Pattern         : constant Pattern_Matcher := Compile
     ("^=thread-group-exited", Multiple_Lines);
   --  Pattern used to detect when the debuggee terminates

   Not_Running_Pattern       : constant Pattern_Matcher := Compile
     ("^\^error,msg=""The program is not being run.", Multiple_Lines);
   --  Pattern used to detect when the debuggee is not running

   Version_Pattern           : constant Pattern_Matcher := Compile
     ("^GNU gdb( \(GDB\))? ([0-9]+)\.([0-9]+)(.[0-9]+)? .*");
   --  To detect the version of GDB

   Language_Pattern          : constant Pattern_Matcher := Compile
     ("^(~[""]*The current source language is [\\]?|Current language:)" &
        "[""]*(auto; currently)?[\s]*([^"",\s,\\]+)?", Multiple_Lines);
   --  Pattern used to detect language changes in the debugger

   Address_Range_Pattern     : constant Pattern_Matcher := Compile
     ("starts at address (0x[0-9a-f]+) <[^>]+> and ends at (0x[0-9a-f]+)");
   --  How to get the range of addresses for a given line

   Address_Pattern           : constant Pattern_Matcher := Compile
     (" (0x[0-9a-zA-Z]+)");

   Question_Filter_Pattern1  : constant Pattern_Matcher :=
     Compile ("^~""\[0\](.|\n)*^~""> ", Multiple_Lines);

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

   procedure Frame_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect current frame

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

   procedure Switch_Language
     (Debugger : access Gdb_MI_Debugger;
      Language : String);
   --  Switch gdb to another language. The possible values for Language are:
   --  "ada", "c", "c++", "asm", "chill", "fortran", "java", "modula-2",
   --  "scheme".
   --  When calling this function, the current language is stored internally
   --  and can be restored by calling Restore_Language.

   procedure Restore_Language
     (Debugger : access Gdb_MI_Debugger);
   --  Restore the language that was active before Switch_Language was called

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

   type Token_List_Controller is
     new Standard.Ada.Finalization.Limited_Controlled with record
      List : Token_List;
   end record;

   overriding procedure Finalize (This : in out Token_List_Controller);

   function Get_Value (S : String) return String;

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
     (Debugger : access Gdb_MI_Debugger) return Version_Number is
   begin
      if Debugger.GDB_Version /= Unknown_Version then
         return Debugger.GDB_Version;
      end if;

      declare
         S       : constant String := Debugger.Send_And_Get_Clean_Output
           ("-gdb-version", Mode => Internal);
         Matched : Match_Array (0 .. 3);

      begin
         Match (Version_Pattern, S, Matched);

         if Matched (0) = No_Match then
            Debugger.GDB_Version := (0, 1);
         else
            Debugger.GDB_Version :=
              (Natural'Value (S (Matched (2).First .. Matched (2).Last)),
               Natural'Value (S (Matched (3).First .. Matched (3).Last)));
         end if;
      end;

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
      Process.Debugger.Set_Is_Started (False);
   end Terminate_Filter;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Debugger : access Gdb_MI_Debugger) is
   begin
      Debugger.Breakpoints_Changed := False;

      if Debugger.Current_Command_Kind /= Misc_Command then
         Debugger.Current_Frame := Null_Frame_Info;
      end if;
   end Reset_State;

   -------------------------------
   -- Send_And_Get_Clean_Output --
   -------------------------------

   overriding function Send_And_Get_Clean_Output
     (Debugger : access Gdb_MI_Debugger;
      Cmd      : String;
      Mode     : Command_Type := Hidden) return String is
   begin
      Debugger.Reset_State;

      declare
         S   : constant String := Debugger.Send_And_Get_Output (Cmd, Mode);
         Pos : Integer;
      begin
         if Ends_With (S, Prompt_String) then
            Pos := S'Last - Prompt_String'Length;
            if S (Pos) = ASCII.LF then
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
      Send
        (Debugger_Root (Debugger.all)'Access, Cmd,
         Empty_Buffer, Wait_For_Prompt, Force_Send, Mode);
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

      declare
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

            Trace
              (Me, "<- Type_Of " & Entity & ":" & Result (1 .. Len) & "'");

            return Result (1 .. Len);
         end;
      end;
   end Type_Of;

   -----------------
   -- Info_Locals --
   -----------------

   overriding function Info_Locals
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-stack-list-locals 1";
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

   --------------------
   -- Info_Registers --
   --------------------

   overriding function Info_Registers
     (Debugger : access Gdb_MI_Debugger) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "-data-list-register-values N";
   end Info_Registers;

   --------------
   -- Value_Of --
   --------------

   overriding function Value_Of
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String
   is
      use Nodes_Vectors;

      Lang    : constant Language_Access := Debugger.Get_Language;
      Context : constant Language_Debugger_Context :=
        Get_Language_Debugger_Context (Language_Debugger_Access (Lang));

      C       : constant Vars_Maps.Cursor := Debugger.Find_Var (Entity);
      V       : Variable;
      Matched : Match_Array (0 .. 1);

      function Fmt return String;
      function Data_Evaluate (Name : String) return String;
      function Var_Evaluate  (Name : String) return String;

      procedure Update_Value
        (Name  : String;
         Exp   : String;
         Nodes : in out Nodes_Vectors.Vector;
         Node  : Node_Access);

--        procedure Process_Variable (Name : String);

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

   begin
      if not Has_Element (C) then
         return "";
      end if;

      Debugger.Update_Var (C);

      Trace (Me, "Value_Of " & Entity & " " & Format'Img);

      V := Element (C);
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
                 (To_String (V.Name), To_String (V.Name), V.Nodes, null);
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

      Trace (Me, "<- Value_Of:" & To_String (Result) & "'");
      return To_String (Result);
   end Value_Of;

   ----------------
   -- Update_Var --
   ----------------

   procedure Update_Var
     (Debugger : access Gdb_MI_Debugger;
      C        : Vars_Maps.Cursor)
   is
      Var : Variable := Element (C);
   begin
      if Var.Updated /= Debugger.Command_No then
         Debugger.Send ("-var-update " & To_String (Var.Name));
         Var.Updated := Debugger.Command_No;
         Debugger.Variables.Replace_Element (C, Var);
      end if;
   end Update_Var;

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

      if Debugger_Name = "" then
         Debugger.General_Spawn (Kernel, Local_Arguments, Gdb_Command, Proxy);
      else
         Debugger.General_Spawn
           (Kernel, Local_Arguments, Debugger_Name, Proxy);
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

         Process.Add_Regexp_Filter (Frame_Filter'Access, Stopped_Regexp);

         Process.Add_Regexp_Filter
           (Question_Filter1'Access, Question_Filter_Pattern1);
         Process.Add_Regexp_Filter
           (Question_Filter2'Access, Question_Filter_Pattern2);

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
      pragma Unreferenced (Num);
   begin
      Debugger.Initializing := True;

      --  Wait for initial output and prompt (and display it in the window)
      Debugger.Get_Process.Wait
        (Num,
         Compile ("^\([^\s]+\).*$", Multiple_Lines),
         Timeout => -1);

      Debugger.Send ("-gdb-set width 0",  Mode => Internal);
      Debugger.Send ("-gdb-set height 0", Mode => Internal);

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

      --  Make sure gdb will not ask too much interactive questions.
      --  Interactive questions are better left to the GUI itself.
      Debugger.Send ("-gdb-set confirm off", Mode => Internal);

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

         if Lang /= null and then Lang.all in Gdb_Ada_Language'Class then
            Debugger.Send ("list adainit", Mode => Internal);

         else
            Debugger.Send ("list main,main", Mode => Internal);
         end if;

         if Get_Pref (Open_Main_Unit) then
            Debugger.Send ("info line", Mode => Internal);
         end if;

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

      for Item of Debugger.Variables loop
         Free (Item);
      end loop;
      Clear (Debugger.Variables);

      Close (Debugger_Root (Debugger.all)'Access);
   end Close;

   -----------------------
   -- Connect_To_Target --
   -----------------------

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_MI_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : Invisible_Command := Hidden)
   is
      Process : constant Visual_Debugger := Convert (Debugger);
      Cmd     : constant String := "-target-select " & Protocol & " " & Target;
      Timeout : constant := 2_000;
      Success : Boolean;
   begin
      if Debugger.Target_Connected then
         if Force then
            Debugger.Interrupt;
         else
            return;
         end if;
      end if;

      --  Send the command to the debugger in a non-blocking way and check if
      --  it succeed.
      Send (Debugger,
            Cmd             => Cmd,
            Wait_For_Prompt => False,
            Mode            => Mode);
      Success := Wait_Prompt (Debugger, Timeout);

      --  Mark the command as processed, even if did not succeed in the
      --  specified timeout so that we can continue sending other commands.
      Set_Command_In_Process (Get_Process (Debugger), False);
      Free (Process.Current_Command);

      if Success then
         if Process /= null then
            Output_Text
              (Process, Protocol & " debugging using " & Target & ASCII.LF);
         end if;

         if Protocol = "remote" then
            Set_Is_Started (Debugger, True);
         end if;

         Debugger.Set_VxWorks_Version;
      else
         Debugger.Interrupt;

         if Process /= null then
            Output_Text (Process, "Can't connect to the target using "
                         & Protocol & " protocol on " & Target & ASCII.LF);
         end if;
      end if;

      Debugger.Display_Prompt;
      Debugger.Target_Connected := Success;
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
                                     Mode     => Internal);
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
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : Command_Type := Hidden)
   is
      pragma Unreferenced (Mode);

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

         if Process /= null then
            Process.Output_Text (Cmd.all & ASCII.LF, Set_Position => True);
         end if;

         declare
            S      : constant String := Debugger.Send_And_Get_Clean_Output
              (Cmd.all, Mode => Hidden);
            Result : Unbounded_String;
         begin
            Free (Cmd);

            if Match (No_Such_File_Regexp, S) /= 0 then
               raise Executable_Not_Found;
            end if;

            if Process /= null and then S /= "" then
               Debugger.Filter_Output (Hidden, S, Result);
               Process.Output_Text
                 (To_String (Result) & ASCII.LF,
                  Set_Position => True);
            end if;
         end;
      end Launch_Command_And_Output;

   begin
      Process := Convert (Debugger);

      Debugger.Executable := Executable;
      Launch_Command_And_Output ("file");

      --  Connect to the remote target if needed

      Connect_To_Target_If_Needed (Debugger);

      --  If we are in Cross mode (ie, with the "remote" protocol), the call
      --  to "target" has the side effect of starting the executable.
      --  In all other cases the executable is not started at this stage.

      if Debugger.Remote_Mode = Cross then
         Debugger.Set_Is_Started (True);
      else
         Debugger.Set_Is_Started (False);
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
      end if;
   end Set_Executable;

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
      Debugger.Set_Is_Started (False);
      Debugger.Send ("core " & (+Core_File), Mode => Mode);

      if Mode in Visible_Command then
         Debugger.Wait_User_Command;
      end if;
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

         if Mode in Visible_Command then
            Wait_User_Command (Debugger);
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

      if Mode in Visible_Command then
         Debugger.Wait_User_Command;
      end if;
   end Add_Symbols;

   --------------------
   -- Attach_Process --
   --------------------

   overriding procedure Attach_Process
     (Debugger : access Gdb_MI_Debugger;
      Process  : String;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-target-attach " & Process, Mode => Mode);
      Debugger.Set_Is_Started (True);

      if Mode in Visible_Command then
         Debugger.Wait_User_Command;
      end if;

      --  Find the first frame containing source information to be as user
      --  friendly as possible, and also check whether attach was successful

      loop
         Debugger.Get_Process.Set_Parse_File_Name (False);

         declare
            Str  : constant String := Debugger.Send_And_Get_Clean_Output
              ("up", Mode => Internal);
            File : Unbounded_String;
            Line : Natural;
            Addr : Address_Type;
            pragma Unreferenced (Line, Addr);
         begin
            Debugger.Get_Process.Set_Parse_File_Name (True);

            --  If attach failed, "up" will return an error message

            if Str = "No stack." then
               Debugger.Set_Is_Started (False);
               return;
            end if;

            exit when Str = "Initial frame selected; you cannot go up.";

            Debugger.Found_File_Name (Str, File, Line, Addr);

            exit when Length (File) /= 0;
         end;
      end loop;

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
      Debugger.Set_Is_Started (False);
   end Detach_Process;

   ------------------
   -- Kill_Process --
   ------------------

   overriding procedure Kill_Process
     (Debugger : access Gdb_MI_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send ("kill", Mode => Mode);
      Debugger.Set_Is_Started (False);
   end Kill_Process;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding procedure Wait_Prompt (Debugger : access Gdb_MI_Debugger) is
      Num : Expect_Match;
      pragma Unreferenced (Num);
   begin
      if Debugger.Current_Command_Kind = Execution_Command then
         Debugger.Get_Process.Wait (Num, Stopped_Regexp, Timeout => -1);
      end if;

      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => -1);
   end Wait_Prompt;

   -----------------
   -- Wait_Prompt --
   -----------------

   overriding function Wait_Prompt
     (Debugger : access Gdb_MI_Debugger;
      Timeout  : Integer) return Boolean
   is
      Num : Expect_Match;
   begin
      if Debugger.Current_Command_Kind = Execution_Command then
         Debugger.Get_Process.Wait (Num, Stopped_Regexp, Timeout => Timeout);
         if Num = Expect_Timeout then
            return False;
         end if;
      end if;

      Debugger.Get_Process.Wait (Num, Prompt_Regexp, Timeout => Timeout);
      return Num /= Expect_Timeout;
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
        ("-exec-run" & (if Start then " --start" else ""), Mode => Mode);
      Debugger.Set_Is_Started (True);
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
      Debugger.Send ("-exec-continue", Mode => Mode);
   end Continue;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Debugger : access Gdb_MI_Debugger) is
      Proxy      : constant Process_Proxy_Access := Debugger.Get_Process;
      Descriptor : constant Process_Descriptor_Access := Proxy.Get_Descriptor;
      use GVD;

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

      elsif Starts_With (Command, "thread")
        or else Starts_With (Command, "task")
        or else Starts_With (Command, "core")
        or else Starts_With (Command, "attach")
      then
         Debugger.Current_Command_Kind := Context_Command;

      elsif Starts_With (Command, "-exec-")
        and then not Starts_With (Command, "-exec-arguments")
      then
         Debugger.Current_Command_Kind := Execution_Command;

      else
         Skip_Word (Command, Index);
         if Command (Command'First .. Index - 1) = "step"
           or else Command (Command'First .. Index - 1) = "stepi"
           or else Command (Command'First .. Index - 1) = "s"
           or else Command (Command'First .. Index - 1) = "si"
           or else Command (Command'First .. Index - 1) = "next"
           or else Command (Command'First .. Index - 1) = "n"
           or else Command (Command'First .. Index - 1) = "nexti"
           or else Command (Command'First .. Index - 1) = "cont"
           or else Command (Command'First .. Index - 1) = "c"
           or else Command (Command'First .. Index - 1) = "continue"
           or else Command (Command'First .. Index - 1) = "run"
           or else Command (Command'First .. Index - 1) = "r"
           or else Starts_With (Command, "fin")  --  fin/fini/finish/...
           or else Starts_With (Command, "target")
           or else Starts_With (Command, "begin")
           or else Starts_With (Command, "start")
           or else Starts_With (Command, "set variable")
         then
            Debugger.Current_Command_Kind := Execution_Command;

         else
            Debugger.Current_Command_Kind := Misc_Command;
         end if;
      end if;

      if Debugger.Current_Command_Kind = Execution_Command then
         Debugger.Command_No := Debugger.Command_No + 1;
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
        or else Starts_With (Command, "-catch-");
   end Breakpoints_Changed;

   ----------------
   -- Stack_Down --
   ----------------

   overriding procedure Stack_Down
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      if Debugger.Current_Frame.Frame /= -1 then
         Debugger.Send
           ("-stack-select-frame" &
              Integer'Image (Debugger.Current_Frame.Frame - 1),
            Mode => Mode);
      else
         Debugger.Send ("down", Mode => Mode);
      end if;
   end Stack_Down;

   --------------
   -- Stack_Up --
   --------------

   overriding procedure Stack_Up
     (Debugger : access Gdb_MI_Debugger;
      Mode     : Command_Type := Hidden) is
   begin
      if Debugger.Current_Frame.Frame /= -1 then
         Debugger.Send
           ("-stack-select-frame" &
              Integer'Image (Debugger.Current_Frame.Frame + 1),
            Mode => Mode);
      else
         Debugger.Send ("up", Mode => Mode);
      end if;
   end Stack_Up;

   -----------------
   -- Stack_Frame --
   -----------------

   overriding procedure Stack_Frame
     (Debugger : access Gdb_MI_Debugger;
      Frame    : Positive;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send
        ("-stack-select-frame" & Natural'Image (Frame - 1),
         Mode => Mode);
   end Stack_Frame;

   ---------------
   -- Backtrace --
   ---------------

   overriding procedure Backtrace
     (Debugger : access Gdb_MI_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural)
   is
      use Token_Lists;

      S          : constant String := Debugger.Send_And_Get_Clean_Output
        ("-stack-list-frames", Mode => Internal);
      Tokens     : Token_List_Controller;
      C, C2, C3  : Token_Lists.Cursor;

   begin
      Tokens.List := Build_Tokens (S);
      Len := 0;
      C   := Find_Identifier (First (Tokens.List), "stack");

      if C = Token_Lists.No_Element then
         return;
      end if;

      --  Skip stack=[
      Next (C, 3);

      loop
         C := Find_Identifier (C, "frame");

         exit when C = Token_Lists.No_Element;

         Len := Len + 1;
         Next (C, 3);

         C2 := Find_Identifier (C, "level");
         Next (C2, 2);
         Value (Len).Frame_Id := Natural'Value (Element (C2).Text.all);

         C2 := Find_Identifier (C2, "addr");
         if C2 /= Token_Lists.No_Element then
            Next (C2, 2);
            Value (Len).Program_Counter := new String'(Element (C2).Text.all);
         else
            Value (Len).Program_Counter := new String'("<>");
            C2 := C;
         end if;

         C2 := Find_Identifier (C2, "func");
         if C2 /= Token_Lists.No_Element then
            Next (C2, 2);
            Value (Len).Subprogram := new String'(Element (C2).Text.all);
         else
            Value (Len).Subprogram := new String'("<>");
            C2 := C;
         end if;

         C2 := Find_Identifier (C2, "fullname");
         if C2 /= Token_Lists.No_Element then
            Next (C2, 2);
            C3 := Find_Identifier (C2, "line");
            if C3 /= Token_Lists.No_Element then
               Next (C3, 2);
               Value (Len).Source_Location :=
                 new String'
                   (Strip_Escape
                      (Element (C2).Text.all) & ':' & Element (C3).Text.all);
            else
               Value (Len).Source_Location :=
                 new String'(Strip_Escape (Element (C2).Text.all) & ": <>");
            end if;
         else
            Value (Len).Source_Location := new String'("<>");
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
      M : Match_Array (0 .. 2);
   begin
      Match (Breakpoint_Num_Pattern, C, Matches => M);
      if M (2) /= No_Match then
         return Breakpoint_Identifier'Value
           (C (M (2).First .. M (2).Last));
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
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert " & (if Temporary then "-t " else "")
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
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger,
         "-break-insert " & (if Temporary then "-t " else "")
         & (+Base_Name (File)) & ':' & Image (Integer (Line)),
         Mode => Mode);
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

   -------------------
   -- Break_Address --
   -------------------

   overriding function Break_Address
     (Debugger  : access Gdb_MI_Debugger;
      Address   : GVD.Types.Address_Type;
      Temporary : Boolean := False;
      Mode      : Command_Type := Hidden)
      return GVD.Types.Breakpoint_Identifier is
   begin
      return Internal_Set_Breakpoint
        (Debugger, "-break-insert " & (if Temporary then "-t " else "")
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
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden) is
   begin
      Debugger.Send
        ("-break-commands" & Num'Img & " " &
         (if Commands (Commands'First) = '"'
            then Commands
            else '"' & Commands & '"'),
         Mode => Mode);
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
         P       : Interfaces.C.size_t;

      begin
         Tokens.List := Build_Tokens (S);
         C := Find_Identifier (First (Tokens.List), "body");

         if C = Token_Lists.No_Element then
            return;
         end if;

         --  Skip body=[
         Next (C, 3);
         if Element (C).Code = R_Bracket then -- /= ']'
            return;
         end if;

         Len := Info'First;
         Info (Len) :=
           (Num_Fields => 7,
            Information =>
              (New_String ("id"),
               New_String ("task-id"),
               New_String ("thread-id"),
               New_String ("parent-id"),
               New_String ("priority"),
               New_String ("state"),
               New_String ("name")));

         loop -- over elements
            if Element (C).Code = Comma then
               Next (C, 1); -- skip ',' between elements
            end if;

            Next (C, 1); -- skip "{"
            Current := False;

            while Element (C).Code /= R_Brace loop -- over elemet tags till '}'
               if Element (C).Code = Comma then
                  Next (C, 1); -- skip ',' between tags
               end if;

               if Element (C).Code /= Identifier
                 or else Element (C).Text = null
               then
                  exit;
               end if;

               Len := Len + 1;
               Info (Len) :=
                 (Num_Fields => 7,
                  Information => (others => <>));

               if Element (C).Text.all = "current" then
                  Current := True;
                  Next (C, 3); -- skip '="*"'

               elsif Element (C).Text.all = "id" then
                  Next (C, 2); -- skip '='
                  Info (Len).Information (1) := New_String
                    ((if Current then "* " & Element (C).Text.all
                     else Element (C).Text.all));
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
                  Info (Len).Information (P) :=
                    New_String (Element (C).Text.all);
               end if;

               Next (C, 1); -- next element
            end loop;
            Next (C, 1); -- skip '}' from previouse element

            for P in Interfaces.C.size_t'(1) .. 7 loop
               if Info (Len).Information (P) = Null_Ptr then
                  Info (Len).Information (P) := New_String ("");
               end if;
            end loop;

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

   ------------------
   -- Info_Threads --
   ------------------

   overriding procedure Info_Threads
     (Debugger : access Gdb_MI_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural)
   is
      S : constant String := Debugger.Send_And_Get_Clean_Output
        ("-thread-info", Mode => Internal);

   begin
      Len := 0;

      declare
         use Standard.Ada.Strings.Unbounded;
         use Token_Lists;
         Tokens      : Token_List_Controller;
         C           : Token_Lists.Cursor;
         Brace_Count : Natural;
         Item        : Unbounded_String;

      begin
         Tokens.List := Build_Tokens (S);
         C := Find_Identifier (First (Tokens.List), "threads");

         if C = Token_Lists.No_Element then
            return;
         end if;

         --  Skip threads=[
         Next (C, 3);
         if Element (C).Code = R_Bracket then -- /= ']'
            return;
         end if;

         Len := Info'First;
         Info (Len) :=
           (Num_Fields => 1,
            Information => (1 => New_String ("Thread")));

         loop -- over elements
            Brace_Count := 0;

            if Element (C).Code = Comma then
               Next (C, 1); -- skip ',' between elements
            end if;

            Next (C, 1); -- skip "{"
            while Element (C).Code /= R_Brace
              or else Brace_Count /= 0
            loop
               if Element (C).Code /= L_Brace then
                  Brace_Count := Brace_Count + 1;
                  Append (Item, "{");

               elsif Element (C).Code /= R_Brace then
                  Brace_Count := Brace_Count - 1;
                  Append (Item, "}");

               elsif Element (C).Code = L_Bracket then
                  Append (Item, "[");

               elsif Element (C).Code = R_Bracket then
                  Append (Item, "]");

               elsif Element (C).Code = Comma then
                  Append (Item, ",");

               elsif Element (C).Code = Identifier then
                  Append (Item, Element (C).Text.all);

               elsif Element (C).Code = C_String then
                  Append (Item, Element (C).Text.all);
               end if;

               Next (C, 1);
            end loop;
            Len := Len + 1;
            Info (Len) :=
              (Num_Fields => 1,
               Information => (1 => New_String (To_String (Item))));

            Next (C, 1); -- skip '}' from previouse element

            exit when Element (C).Code = R_Bracket; -- last ']'
         end loop;

      exception
         when others =>
            if Len > 0 then
               Len := Len - 1;
            end if;
      end;
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
           ("(gdb) ", Is_Command => False, Set_Position => True);
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

   Frame_Pattern         : constant Pattern_Matcher :=
     Compile ("^\*stopped.*frame={(.*)}", Multiple_Lines);
   Subframe_Pattern      : constant Pattern_Matcher :=
     Compile ("addr=""(0x[\da-hA-H]+)"",.*fullname=""(.*)"",line=""(\d+)""");
   CLI_File_Name_Pattern : constant Pattern_Matcher :=
     Compile (".+(\d+).+\""(.+?)\\\"".+(0x[0-9a-f]+)", Multiple_Lines);
   --  Matches a file name/line indication in gdb's output

   overriding procedure Found_File_Name
     (Debugger    : access Gdb_MI_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type)
   is
      pragma Unreferenced (Debugger);

      Start    : Natural := Str'First;
      Matched  : Match_Array (0 .. 3);
      Matched2 : Match_Array (0 .. 3);

   begin
      --  Default values if nothing better is found
      Name := Null_Unbounded_String;
      Line := 0;
      Addr := Invalid_Address;

      Match (Frame_Pattern, Str, Matched);

      if Matched (0) /= No_Match then
         Match
           (Subframe_Pattern,
            Str (Matched (1).First .. Matched (1).Last),
            Matched);

         if Matched (0) = No_Match then
            return;
         end if;

         Addr := String_To_Address
           (Str (Matched (1).First .. Matched (1).Last));
         Set_Unbounded_String
           (Name, Strip_Escape (Str (Matched (2).First .. Matched (2).Last)));
         Line := Integer'Value (Str (Matched (3).First .. Matched (3).Last));

      else
         loop
            Match (CLI_File_Name_Pattern, Str (Start .. Str'Last), Matched2);
            exit when Matched2 (0) = No_Match;
            Matched := Matched2;
            Start   := Matched (0).Last + 1;
         end loop;

         if Matched (0) = No_Match then
            return;
         end if;

         Set_Unbounded_String
           (Name, Str (Matched (2).First .. Matched (2).Last));

         Addr := String_To_Address
           (Str (Matched (3).First .. Matched (3).Last));

         Line := Natural'Value (Str (Matched (1).First .. Matched (1).Last));
      end if;
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
      pragma Unreferenced (Str);

   begin
      if Debugger.Current_Frame.Frame = -1 then
         Debugger.Get_Frame_Info;
      end if;

      if Debugger.Current_Frame.Line = 0 then
         Message := No_Debug_Info;
      else
         Message := Location_Found;
         Frame   := To_Unbounded_String (Debugger.Current_Frame.Frame'Img);
      end if;
   end Found_Frame_Info;

   ------------------
   -- Frame_Filter --
   ------------------

   procedure Frame_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array)
   is
      pragma Unreferenced (Matched);
      use Token_Lists;
      Tokens  : Token_List_Controller;
      C       : Token_Lists.Cursor;
   begin
      Tokens.List := Build_Tokens (Str);
      C := Find_Identifier (First (Tokens.List), "frame");
      if C = Token_Lists.No_Element then
         return;
      end if;

      Next (C, 3);
      if not Is_Identifier (C, "addr") then
         return;
      end if;

      Next (C, 2);
      Gdb_MI_Debugger (Process.Debugger.all).Current_Frame.Addr :=
        String_To_Address (Element (C).Text.all);
   end Frame_Filter;

   ----------
   -- Free --
   ----------

   procedure Free is
     new Standard.Ada.Unchecked_Deallocation (Node, Node_Access);

   procedure Free (Var : in out Variable)
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
      Debugger.Get_Process.Set_Parse_File_Name (False);
      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-file-list-exec-source-files", Mode => Internal);
      begin
         Debugger.Get_Process.Set_Parse_File_Name (True);
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

      procedure Read_Bkpt;

      ---------------
      -- Read_Bkpt --
      ---------------

      procedure Read_Bkpt is
         B        : Breakpoint_Data;
         Multiple : Boolean := False;

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
                  if Matched (5) /= No_Match then
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (5).First .. Matched (5).Last));
                  else
                     B.Except := To_Unbounded_String
                       (Element (C).Text
                        (Matched (4).First .. Matched (4).Last));
                  end if;
               end if;
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
               if Element (C).Text.all = "func" then
                  Next (C, 2);
                  B.Subprogram := To_Unbounded_String (Element (C).Text.all);

               elsif Element (C).Text.all = "file" then
                  Next (C, 2);
                  F := Debugger.Get_Kernel.Create_From_Base
                    (+Element (C).Text.all);

               elsif Element (C).Text.all = "fullname" then
                  Next (C, 2);
                  F := To_File (Kernel, Strip_Escape (Element (C).Text.all));

               elsif Element (C).Text.all = "line" then
                  Next (C, 2);
                  Line := Editable_Line_Type'Value (Element (C).Text.all);

               elsif Element (C).Text.all = "what" then
                  Next (C, 2);
                  Parse_What;

               else
                  exit;
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
         B.The_Type := Breakpoint_Type'Value (Element (C).Text.all);

         C := Find_Identifier (C, "disp");
         Next (C, 2);
         B.Disposition := Breakpoint_Disposition'Value (Element (C).Text.all);

         C := Find_Identifier (C, "enabled");
         Next (C, 2);
         B.Enabled := Element (C).Text.all = "y";

         C := Find_Identifier (C, "addr");
         Next (C, 2);
         if Element (C).Text.all = "<MULTIPLE>" then
            Multiple := True;
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
               end if;
               --  ??? missing Commands
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

      Debugger.Get_Process.Set_Parse_File_Name (False);
      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-break-list", Mode => Internal);
      begin
         Tokens.List := Build_Tokens (S);
      end;

      Debugger.Get_Process.Set_Parse_File_Name (True);

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

   -----------------------
   -- Enable_Breakpoint --
   -----------------------

   overriding procedure Enable_Breakpoint
     (Debugger : access Gdb_MI_Debugger;
      Num      : Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : Command_Type := Hidden) is
   begin
      if Enable then
         Debugger.Send ("-break-enable" & Num'Img, Mode => Mode);
      else
         Debugger.Send ("-break-disable" & Num'Img, Mode => Mode);
      end if;
   end Enable_Breakpoint;

   -----------------------
   -- Remove_Breakpoint --
   -----------------------

   overriding procedure Remove_Breakpoint
     (Debugger : access Gdb_MI_Debugger;
      Num      : Breakpoint_Identifier;
      Mode     : Command_Type := Hidden) is
   begin
      Debugger.Send ("-break-delete" & Num'Img, Mode => Mode);
   end Remove_Breakpoint;

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
      C : constant Cursor := Debugger.Find_Var (Entity);
   begin
      if not Has_Element (C) then
         return Default;
      end if;

      declare
         S    : constant String := Debugger.Send_And_Get_Clean_Output
           ("-var-info-type " & To_String (Element (C).Name),
            Mode => Internal);
         Idx  : Natural := Index (S, "type=" & '"');
         Idx1 : Natural;
      begin
         if Idx >= S'First then
            Idx  := Idx + 6;
            Idx1 := Idx;
            Skip_To_Char (S, Idx1, '"');
            return S (Idx .. Idx1 - 1);
         end if;
      end;

      return Default;
   end Get_Type_Info;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Token_List_Controller) is
   begin
      Clear_Token_List (This.List);
   end Finalize;

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

      Debugger.Get_Process.Set_Parse_File_Name (False);
      Debugger.Switch_Language ("c");

      declare
         Str  : constant String := Debugger.Send_And_Get_Clean_Output
           ("info line " & File_Name & ":1", Mode => Internal);
         File : Unbounded_String;
         Line : Natural;
         Addr : GVD.Types.Address_Type;

      begin
         Debugger.Restore_Language;
         Debugger.Get_Process.Set_Parse_File_Name (True);
         Debugger.Found_File_Name (Str, File, Line, Addr);

         if Length (File) = 0 then
            return File_Name;
         else
            return To_String (File);
         end if;
      end;
   end Find_File;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_MI_Debugger;
      Range_Start   : out Address_Type;
      Range_End     : out Address_Type;
      Code          : out GNAT.Strings.String_Access;
      Start_Address : Address_Type := GVD.Types.Invalid_Address;
      End_Address   : Address_Type := GVD.Types.Invalid_Address)
   is
      use Token_Lists;
      Tokens  : Token_List_Controller;
      C, Last : Token_Lists.Cursor;

      Result : Unbounded_String;

   begin
      Debugger.Process.Set_Parse_File_Name (False);
      declare
         Disassembled : constant String := Debugger.Send_And_Get_Clean_Output
           ("-data-disassemble -s " &
              Address_To_String (Start_Address) & " -e " &
              Address_To_String (End_Address) & " -- 0",
            Mode => Internal);

         Start_Index, End_Index : Integer;
      begin
         Debugger.Process.Set_Parse_File_Name (True);

         Start_Index := Disassembled'First;
         Skip_To_Char (Disassembled, Start_Index, '[');
         Start_Index := Start_Index + 1;

         End_Index := Disassembled'Last;
         Skip_To_Char (Disassembled, End_Index, ']', Step => -1);
         End_Index := End_Index - 1;

         Tokens.List := Build_Tokens (Disassembled (Start_Index .. End_Index));
      end;

      C := Find_Identifier (First (Tokens.List), "address");

      if C = Token_Lists.No_Element then
         Range_Start := Invalid_Address;
         Range_End   := Invalid_Address;
         Code := new String'("");
         return;
      end if;

      Next (C, 2);
      Range_Start := String_To_Address (Element (C).Text.all);

      while C /= Token_Lists.No_Element loop
         Last := C;
         Append (Result, Element (C).Text.all & " <");
         C := Find_Identifier (C, "func-name");
         if C /= Token_Lists.No_Element then
            Next (C, 2);
            Append (Result, Element (C).Text.all & "+");
         end if;

         C := Find_Identifier (C, "offset");
         if C /= Token_Lists.No_Element then
            Next (C, 2);
            Append (Result, Element (C).Text.all & ">: ");
         end if;

         C := Find_Identifier (C, "inst");
         if C /= Token_Lists.No_Element then
            Next (C, 2);
            Append (Result, Element (C).Text.all & ASCII.LF);
         end if;

         C := Find_Identifier (C, "address");
         if C /= Token_Lists.No_Element then
            Next (C, 2);
         end if;
      end loop;

      Range_End := String_To_Address (Element (Last).Text.all);
      Code := new String'(To_String (Result));
   end Get_Machine_Code;

   ----------------------
   -- Get_Line_Address --
   ----------------------

   overriding procedure Get_Line_Address
     (Debugger    : access Gdb_MI_Debugger;
      Line        : Natural;
      Range_Start : out Address_Type;
      Range_End   : out Address_Type) is
   begin
      Debugger.Get_Process.Set_Parse_File_Name (False);
      Debugger.Switch_Language ("c");

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

      Debugger.Restore_Language;
      Debugger.Get_Process.Set_Parse_File_Name (True);
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
     (Debugger : access Gdb_MI_Debugger;
      Address  : String;
      Byte     : String) is
   begin
      Debugger.Switch_Language ("c");
      Debugger.Send
        ("-data-write-memory-bytes " & Address & " 0x" & Byte,
         Mode => Internal);
      Debugger.Restore_Language;
   end Put_Memory_Byte;

   ----------------------
   -- Found_Frame_Info --
   ----------------------

   procedure Get_Frame_Info (Debugger : access Gdb_MI_Debugger) is
      use Token_Lists;
      Tokens : Token_List_Controller;
      C      : Token_Lists.Cursor;

   begin
      Debugger.Detect_Language;

      declare
         S : constant String := Debugger.Send_And_Get_Clean_Output
           ("-stack-info-frame", Mode => Internal);
      begin
         Tokens.List := Build_Tokens (S);
      end;

      C := Find_Identifier (First (Tokens.List), "level");

      if C /= Token_Lists.No_Element then
         Next (C, 2);
         Debugger.Current_Frame.Frame := Integer'Value (Element (C).Text.all);

         C := Find_Identifier (C, "addr");
         if C /= Token_Lists.No_Element then
            Next (C, 2);
            Debugger.Current_Frame.Addr := String_To_Address
              (Element (C).Text.all);

            C := Find_Identifier (C, "line");
            if C /= Token_Lists.No_Element then
               Next (C, 2);
               Debugger.Current_Frame.Line := Natural'Value
                 (Element (C).Text.all);
            end if;
         end if;
      end if;
   end Get_Frame_Info;

   ----------------------------
   -- Get_Current_Frame_Addr --
   ----------------------------

   function Get_Current_Frame_Addr
     (Debugger : access Gdb_MI_Debugger) return GVD.Types.Address_Type is
   begin
      if Debugger.Current_Frame = Null_Frame_Info then
         Debugger.Get_Frame_Info;
      end if;

      return Debugger.Current_Frame.Addr;
   end Get_Current_Frame_Addr;

   --------------
   -- Find_Var --
   --------------

   function Find_Var
     (Debugger : access Gdb_MI_Debugger;
      Entity   : String)
      return Cursor
   is
      Frame : constant GVD.Types.Address_Type :=
        Debugger.Get_Current_Frame_Addr;
      C     : Cursor;

   begin
      if Frame = Invalid_Address then
         return No_Element;
      end if;

      C := Debugger.Variables.Find (Entity);
      if Has_Element (C) then
         if Element (C).Frame = Frame then
            return C;
         else
            Debugger.Send
              ("-var-delete -c " & To_String (Element (C).Name),
               Mode => Internal);
            declare
               V : Variable := Element (C);
            begin
               Debugger.Variables.Delete (C);
               Free (V);
            end;
         end if;
      end if;

      declare
         S : constant String := Debugger.Send_And_Get_Output
           ("-var-create - * " & '"' & Entity & '"', Mode => Internal);

         use Token_Lists;
         Tokens  : Token_List_Controller;
         T       : Token_Lists.Cursor;
         Var     : Variable;
         Dummy   : Boolean;
      begin
         --  name="var2",numchild="0",value="5",type="positive",has_more="0"
         Tokens.List := Build_Tokens (S);
         T := Find_Identifier (First (Tokens.List), "name");

         if T /= Token_Lists.No_Element then
            Next (T, 2);
            Var.Name := To_Unbounded_String (Element (T).Text.all);

            T := Find_Identifier (T, "numchild");
            if T /= Token_Lists.No_Element then
               Next (T, 2);
               Var.Childs := Natural'Value (Element (T).Text.all);
            end if;

            Var.Frame := Frame;
            Var.Updated := Debugger.Command_No;

            Debugger.Variables.Insert (Entity, Var, C, Dummy);

            return C;
         end if;
      end;

      return No_Element;
   end Find_Var;

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
         Trace (Me, "<- Get_Value:" & Element (T).Text.all & "'");
         return Element (T).Text.all;
      end if;

      T := Find_Identifier (First (Tokens.List), "msg");
      if T /= Token_Lists.No_Element then
         Next (T, 2);
         Trace (Me, "<- Get_Value:" & Element (T).Text.all & "'");
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
      loop
         Match (Address_Pattern, S, Matched);
         exit when Matched (1) = No_Match;
         Last := Matched;
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

   ---------------------
   -- Switch_Language --
   ---------------------

   procedure Switch_Language
     (Debugger : access Gdb_MI_Debugger;
      Language : String) is
   begin
      Free (Debugger.Stored_Language);

      Debugger.Send ("show lang", Mode => Internal);

      if Debugger.Get_Language /= null then
         Debugger.Stored_Language :=
           new String'(Debugger.Get_Language.Get_Name);
      end if;

      Debugger.Send ("set lang " & Language, Mode => Internal);
   end Switch_Language;

   ----------------------
   -- Restore_Language --
   ----------------------

   procedure Restore_Language (Debugger : access Gdb_MI_Debugger) is
   begin
      if Debugger.Stored_Language /= null then
         Debugger.Send ("set lang " & Debugger.Stored_Language.all);
      end if;
   end Restore_Language;

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

   overriding procedure Filter_Output
     (Debugger : access Gdb_MI_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String)
   is
      pragma Unreferenced (Debugger);
      J        : Integer := Str'First;
      First    : Integer;
      New_Line : Boolean := True;

   begin
      while J <= Str'Last loop
         if New_Line and then J + 2 < Str'Last then
            if Str (J) = '~'
              and then Str (J + 1) = '"'
            then
               --  Replace ~"...." by ....

               J := J + 2;
               First := J;

               while J <= Str'Last and then Str (J) /= '"' loop
                  if Str (J) /= '\' then
                     Append (Result, Str (J));
                  else
                     J := J + 1;
                     case Str (J) is
                        when '\' | '"' =>
                           Append (Result, Str (J));
                        when 'n' | 'r' =>
                           null;
                        when others =>
                           Append (Result, Str (J - 1 .. J));
                     end case;
                  end if;

                  J := J + 1;
               end loop;

            elsif Str (J) = '&'
              and then Str (J + 1) = '"'
            then
               --  Strip &"..."
               J := J + 2;

               while J <= Str'Last and then Str (J) /= ASCII.LF loop
                  J := J + 1;
               end loop;

            elsif Mode = User
              and then J + 4 <= Str'Last
              and then Str (J .. J + 4) = "^done"
            then
               --  Strip "^done[,]", keep the rest (result expected by user)
               if Str'Last > J + 4
                 and then Str (J + 5) = ','
               then
                  J := J + 5;
               else
                  J := J + 4;
               end if;

            elsif Str (J) in '^' | '*' | '=' then
               --  Strip [^*=]...
               J := J + 1;

               if J + 12 < Str'Last
                 and then Str (J .. J + 10) = "error,msg="""
               then
                  J := J + 11;
                  First := J;

                  while J <= Str'Last and then Str (J) /= '"' loop
                     J := J + 1;
                  end loop;

                  if J <= Str'Last then
                     Append (Result, Str (First .. J - 1));
                  else
                     Append (Result, Str (First .. Str'Last));
                  end if;
               else
                  if J + 6 < Str'Last
                    and then Str (J .. J + 6) = "stopped"
                  then
                     --  Display info to the user
                     Append (Result, "[program stopped");
                     J := J + 7;

                     if J + 8 < Str'Last
                       and then Str (J .. J + 8) = ",reason="""
                     then
                        Append (Result, ": ");
                        J := J + 9;

                        while J <= Str'Last and then Str (J) /= '"' loop
                           Append (Result, Str (J));
                           J := J + 1;
                        end loop;
                     end if;

                     Append (Result, "]" & ASCII.LF);
                  end if;

                  while J <= Str'Last and then Str (J) /= ASCII.LF loop
                     J := J + 1;
                  end loop;
               end if;
            else
               Append (Result, Str (J));
            end if;

         elsif J = Str'Last
           and then Str (J) = ASCII.LF
           and then J - 6 >= Str'First
           and then Str (J - 6 .. J - 1) = "(gdb) "
         then
            --  Strip last LF after prompt
            null;

         else
            Append (Result, Str (J));
         end if;

         New_Line := J <= Str'Last and then Str (J) = ASCII.LF;
         J := J + 1;
      end loop;
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
      return Starts_With (Command, "-gdb-exit")
        or else Starts_With (Command, "quit")
        or else Starts_With (Command, "q");
   end Is_Quit_Command;

end Debugger.Base_Gdb.Gdb_MI;

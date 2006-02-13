-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004-2006                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Unchecked_Deallocation;
with GNAT.Expect;        use GNAT.Expect;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNAT.Regpat;        use GNAT.Regpat;

with Gtk.Main;           use Gtk.Main;

with Custom_Module;      use Custom_Module;
with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with GPS.Kernel.Timeout; use GPS.Kernel.Timeout;
with Traces;             use Traces;
with Commands;           use Commands;

package body Expect_Interface is

   Me : constant Debug_Handle := Create ("Expect", Off);

   Process_Class_Name : constant String := "Process";

   Command_Cst         : aliased constant String := "command";
   Regexp_Cst          : aliased constant String := "regexp";
   Timeout_Cst         : aliased constant String := "timeout";
   On_Match_Action_Cst : aliased constant String := "on_match";
   On_Exit_Action_Cst  : aliased constant String := "on_exit";
   Add_Lf_Cst          : aliased constant String := "add_lf";
   Constructor_Args : constant Cst_Argument_List :=
     (Command_Cst'Access, Regexp_Cst'Access, On_Match_Action_Cst'Access,
      On_Exit_Action_Cst'Access);
   Send_Args : constant Cst_Argument_List :=
     (Command_Cst'Access, Add_Lf_Cst'Access);
   Expect_Args : constant Cst_Argument_List :=
     (Regexp_Cst'Access, Timeout_Cst'Access);

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Access);

   type Custom_Action_Record is new Root_Command with record
      Pattern    : Pattern_Matcher_Access;
      Command    : Argument_List_Access;
      On_Match   : GPS.Kernel.Scripts.Subprogram_Type;
      On_Exit    : GPS.Kernel.Scripts.Subprogram_Type;
      Fd         : GNAT.Expect.Process_Descriptor_Access;

      Processed_Output : String_Access;
      Unmatched_Output : String_Access;
   end record;

   procedure Free (X : in out Custom_Action_Record);
   --  Free memory associated to X.

   function Execute
     (Command : access Custom_Action_Record) return Command_Return_Type;
   --  Do not do anything. This command is not aimed at being used in the
   --  task manager.

   type Custom_Action_Access is access all Custom_Action_Record;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Custom_Action_Record, Custom_Action_Access);

   procedure Free (X : in out Custom_Action_Access);
   --  Free memory associated to X.

   type Instance_Callback_Data is new Callback_Data_Record with record
      Inst : Class_Instance;
   end record;

   type Action_Property is new Instance_Property_Record with record
      Action : Custom_Action_Access;
   end record;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer);
   --  Called when an external process has finished running

   procedure Output_Cb (Data : Process_Data; Output : String);
   --  Called when an external process has produced some output.

   procedure Concat (S : in out String_Access; S2 : String);
   --  Append S2 at the end of S.

   function To_String (S : String_Access) return String;
   --  Return the contents of S. if S is null, return "".

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Action_Access;
   function Get_Data (Inst : Class_Instance) return Custom_Action_Access;
   --  Get or store some data in an instance of GPS.Process

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the expect interface.

   type Exit_Type is (Matched, Timed_Out, Died);
   function Interactive_Expect
     (Kernel   : access Kernel_Handle_Record'Class;
      Action   : Custom_Action_Access;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Till_End : Boolean := True) return Exit_Type;
   --  Execute a call to Expect, but process the gtk+ events periodically.

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Action_Record) is
   begin
      Free (X.Command);
      Unchecked_Free (X.Pattern);
      Free (X.Unmatched_Output);
      Free (X.Processed_Output);
      Free (X.On_Exit);
      Free (X.On_Match);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Action_Access) is
   begin
      if X /= null then
         Free (X.all);
         Unchecked_Free (X);
      end if;
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Custom_Action_Record) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      return Execute_Again;
   end Execute;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Inst : Class_Instance) return Custom_Action_Access is
   begin
      return Action_Property (Get_Property (Inst, Process_Class_Name)).Action;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Action_Access
   is
      Process_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), Process_Class_Name);
      Inst : constant Class_Instance := Nth_Arg (Data, N, Process_Class);
   begin
      return Get_Data (Inst);
   end Get_Data;

   ---------------
   -- To_String --
   ---------------

   function To_String (S : String_Access) return String is
   begin
      if S = null then
         return "";
      else
         return S.all;
      end if;
   end To_String;

   ------------
   -- Concat --
   ------------

   procedure Concat (S : in out String_Access; S2 : String) is
   begin
      if S = null then
         S := new String'(S2);
      else
         declare
            N : constant String := S.all & S2;
         begin
            Free (S);
            S := new String'(N);
         end;
      end if;
   end Concat;

   -------------
   -- Exit_Cb --
   -------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer) is
      Inst : constant Class_Instance :=
        Instance_Callback_Data (Data.Callback_Data.all).Inst;
      D    : constant Custom_Action_Access := Get_Data (Inst);
      Tmp  : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Trace (Me, "Exited");
      if D.On_Exit /= null then
         declare
            C : Callback_Data'Class := Create
              (Get_Script (Inst), Arguments_Count => 3);
         begin
            Set_Nth_Arg (C, 1, Inst);
            Set_Nth_Arg (C, 2, Status);
            Set_Nth_Arg (C, 3, To_String (D.Processed_Output)
                         & To_String (D.Unmatched_Output));
            Tmp := Execute (D.On_Exit, C);
            Free (C);
         end;
      end if;

      D.Fd := null;

      --  ??? Add exception handler ?
   end Exit_Cb;

   ---------------
   -- Output_Cb --
   ---------------

   procedure Output_Cb (Data : Process_Data; Output : String) is
      Inst : constant Class_Instance :=
        Instance_Callback_Data (Data.Callback_Data.all).Inst;
      D    : constant Custom_Action_Access := Get_Data (Inst);
      Matches   : Match_Array (0 .. Max_Paren_Count);
      Beg_Index : Natural;
      End_Index : Natural;
      Prev_Beg  : Natural;
      Action_To_Execute : Subprogram_Type;

   begin
      Trace (Me, "Output: " & Output);

      if D.Pattern = null
        and then D.On_Exit = null
      then
         --  We are reading output for a command that does not react on
         --  output: do nothing.
         --  ??? Check for expects here.
         return;
      end if;

      --  Deal with the simple case when no expects have been registered.

      if D.On_Match = null then
         --  ??? should look for expects here.

         if D.On_Exit /= null then
            Concat (D.Processed_Output, Output);
         end if;

         return;
      end if;

      --  If we reach this point, this means expects have been registered.

      --  Add the output to the unmatched output.

      Concat (D.Unmatched_Output, Output);

      --  Attempt to match the regexp in the output.

      if D.Unmatched_Output.all = "" then
         return;
      end if;

      Beg_Index := D.Unmatched_Output'First;
      Prev_Beg  := Beg_Index;
      End_Index := D.Unmatched_Output'Last;

      loop
         Action_To_Execute := D.On_Match;
         Match
           (D.Pattern.all,
            D.Unmatched_Output.all,
            Matches,
            Beg_Index, End_Index);

         if Matches (0) = No_Match then
            exit;
         else
            --  We have found a match.

            declare
               C : Callback_Data'Class := Create
                 (Get_Script (Inst), Arguments_Count => 3);
               Tmp  : Boolean;
               pragma Unreferenced (Tmp);
            begin
               Set_Nth_Arg (C, 1, Inst);
               Set_Nth_Arg
                 (C, 2,
                  D.Unmatched_Output (Matches (0).First .. Matches (0).Last));
               Set_Nth_Arg
                 (C, 3,
                  D.Unmatched_Output (Beg_Index .. Matches (0).First - 1));
               Tmp := Execute (Action_To_Execute, C);
               Free (C);
            end;

            Beg_Index := Matches (0).Last + 1;

            if Beg_Index > End_Index then
               exit;
            end if;

            --  Prevent infinite loops happenning for users specifying regexps
            --  that match on empty string.

            if Prev_Beg = Beg_Index then
               exit;
            else
               Prev_Beg := Beg_Index;
            end if;
         end if;
      end loop;

      --  If we have matched something, do the necessary adjustments.

      if Beg_Index > D.Unmatched_Output'First then
         --  Add the matched output to the stored output, if necessary.

         if D.On_Exit /= null then
            Concat (D.Processed_Output,
                    (D.Unmatched_Output
                       (D.Unmatched_Output'First .. Beg_Index - 1)));
         end if;

         --  Reduce the output that we have already matched.

         declare
            S : constant String := D.Unmatched_Output (Beg_Index .. End_Index);
         begin
            Free (D.Unmatched_Output);
            D.Unmatched_Output := new String'(S);
         end;
      end if;

      --  ??? Add exception handler ?
   end Output_Cb;

   ------------------------
   -- Interactive_Expect --
   ------------------------

   function Interactive_Expect
     (Kernel   : access Kernel_Handle_Record'Class;
      Action   : Custom_Action_Access;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Till_End : Boolean := True) return Exit_Type
   is
      Result : Expect_Match;
      T      : Integer := Integer'Min (Timeout, 200);
      Iter   : Integer := 1;
      Dead   : Boolean;
      pragma Unreferenced (Dead);
   begin
      Block_Commands (Kernel, True);
      if Timeout < 0 then
         T := 200;
      end if;

      while Action.Fd /= null loop
         Expect (Descriptor => Action.Fd.all,
                 Result     => Result,
                 Regexp     => Pattern,
                 Timeout    => T);

         if Result = Expect_Timeout then
            if not Till_End
              and then Timeout /= -1
              and then Iter * T >= Timeout
            then
               Trace (Me, "Interactive_Expect: Timeout after "
                      & Integer'Image (T * Iter) & " >= "
                      & Integer'Image (Timeout));
               Block_Commands (Kernel, False);
               return Exit_Type'(Timed_Out);
            end if;

            while Gtk.Main.Events_Pending loop
               Dead := Gtk.Main.Main_Iteration;
            end loop;

         elsif not Till_End then
            Trace (Me, "Interactive_Expect: Matched " & Pattern);
            Block_Commands (Kernel, False);
            return Exit_Type'(Matched);
         end if;

         Iter := Iter + 1;
      end loop;

      Block_Commands (Kernel, False);
      return Exit_Type'(Died);

   exception
      when Process_Died =>
         Trace (Me, "Interactive_Expect: Process died");
         Block_Commands (Kernel, False);
         return Exit_Type'(Died);
   end Interactive_Expect;

   --------------------------
   -- Custom_Spawn_Handler --
   --------------------------

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel        : constant Kernel_Handle :=
        Get_Kernel (Custom_Module_ID.all);
      D             : Custom_Action_Access;
      Process_Class : constant Class_Type := New_Class
        (Kernel, Process_Class_Name);
      E             : Exit_Type;
      pragma Unreferenced (E);

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Args);

         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Process_Class);
            Command_Line  : constant String := Nth_Arg (Data, 2);
            Regexp        : constant String := Nth_Arg (Data, 3, "");
            Success       : Boolean;
         begin
            if Command_Line = "" then
               Set_Error_Msg (Data, -"Argument for command cannot be empty");
               return;
            end if;

            Trace (Me, "Spawning " & Command_Line);

            D          := new Custom_Action_Record;
            D.Command  := Argument_String_To_List (Command_Line);
            D.On_Match := Nth_Arg (Data, 4, null);
            D.On_Exit  := Nth_Arg (Data, 5, null);

            if Regexp /= "" then
               D.Pattern :=
                 new Pattern_Matcher'(Compile (Regexp, Multiple_Lines));
            end if;

            --  All the parameters are correct: launch the process.

            Launch_Process
              (Kernel        => Kernel,
               Command       => D.Command (D.Command'First).all,
               Arguments => D.Command (D.Command'First + 1 .. D.Command'Last),
               Console       => null,
               Callback      => Output_Cb'Access,
               Exit_Cb       => Exit_Cb'Access,
               Success       => Success,
               Show_Command  => False,
               Callback_Data => new Instance_Callback_Data'(Inst => Inst),
               Line_By_Line  => False,
               Directory     => "");

            if Success then
               Set_Property
                 (Inst, Process_Class_Name, Action_Property'(Action => D));
            else
               Free (D);
               Set_Error_Msg
                 (Data, -"Could not launch command """ & Command_Line & """");
            end if;
         end;

      elsif Command = "send" then
         Name_Parameters (Data, Send_Args);
         D := Get_Data (Data, 1);
         if D.Fd /= null then
            Send (D.Fd.all,
                  Str => Nth_Arg (Data, 2),
                  Add_LF => Nth_Arg (Data, 3, True));
         end if;

      elsif Command = "interrupt" then
         D := Get_Data (Data, 1);
         if D.Fd /= null then
            Interrupt (D.Fd.all);
         end if;

      elsif Command = "kill" then
         D := Get_Data (Data, 1);
         if D.Fd /= null then
            Close (D.Fd.all);
         end if;

      elsif Command = "wait" then
         E := Interactive_Expect
           (Kernel   => Get_Kernel (Data),
            Action   => Get_Data (Data, 1),
            Timeout  => -1,
            Pattern  => "@#$%^&",
            Till_End => True);

      elsif Command = "expect" then
         Name_Parameters (Data, Expect_Args);
         D := Get_Data (Data, 1);
         E := Interactive_Expect
           (Kernel   => Get_Kernel (Data),
            Action   => D,
            Timeout  => Nth_Arg (Data, 3, -1),
            Pattern  => Nth_Arg (Data, 2),
            Till_End => False);
         if D.Fd /= null then
            Set_Return_Value (Data, Expect_Out (D.Fd.all));
         else
            Set_Return_Value (Data, "Process terminated");
         end if;
      end if;
   end Custom_Spawn_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Process_Class : constant Class_Type := New_Class
        (Kernel, Process_Class_Name, New_Class (Kernel, "Command"));
   begin
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 4,
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
      Register_Command
        (Kernel, "send",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
      Register_Command
        (Kernel, "interrupt",
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
      Register_Command
        (Kernel, "kill",
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
      Register_Command
        (Kernel, "wait",
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
      Register_Command
        (Kernel, "expect",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
   end Register_Commands;

end Expect_Interface;

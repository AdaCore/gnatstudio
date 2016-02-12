------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Expect;             use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;         use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;            use GNATCOLL.Scripts;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

with Gtk.Main;                use Gtk.Main;

with Basic_Types;             use Basic_Types;
with Custom_Module;           use Custom_Module;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Remote;       use GPS.Kernel.Remote;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Remote;                  use Remote;
with String_Utils;            use String_Utils;
with Commands;                use Commands;

package body Expect_Interface is

   Me : constant Trace_Handle := Create ("Expect", Off);

   Process_Class_Name   : constant String := "Process";

   type End_Action_Record is new Root_Command with record
      Inst             : Class_Instance;
      Status           : Integer := 0;
      On_Exit          : Subprogram_Type;
      Unmatched_Output : String_Access;
   end record;
   --  This action is used to make sure that the On_Exit callback is always
   --  called after the synchronization from Server to Local host in case
   --  of a remote action.

   type End_Action_Access is access all End_Action_Record;

   overriding function Execute
     (Command : access End_Action_Record) return Command_Return_Type;

   overriding procedure Free (X : in out End_Action_Record);
   --  Free memory associated to X

   type Custom_Action_Record is new Root_Command with record
      Pattern          : Pattern_Matcher_Access;
      Server           : Server_Type;
      Command          : Arg_List;
      Show_Command     : Boolean;
      On_Match         : Subprogram_Type;
      On_Exit          : Subprogram_Type;
      Before_Kill      : Subprogram_Type;
      Pd               : Process_Descriptor_Access;
      Status           : Integer := 0;
      Started          : Boolean;
      Inst             : Class_Instance;
      Progress_Regexp  : Pattern_Matcher_Access;
      Progress_Current : Natural := 1;  --  Parenthesis within the regexp
      Progress_Final   : Natural := 2;  --  Parenthesis within the regexp

      Strip_CR         : Boolean := True;
      --  Whether ASCII.CR characters should be stripped from the output of
      --  the process before passing the string on to GPS. This is in general
      --  suitable especially on Windows, but should be set to False if the
      --  program is carefully outputing escape sequences to manage the screen,
      --  as unix shells do for instance

      In_Expect        : Boolean := False;
      --  True if we are processing the 'GPS.Process.expect' function. This
      --  temporarily disables periodically checking for matching output for
      --  On_Match.

      Unmatched_Output : String_Access;
      --  Output of the process since the last time On_Match was called (ie
      --  since the last time Pattern matched).

      End_Action        : End_Action_Access;
   end record;

   overriding function Name (X : access Custom_Action_Record) return String;
   --  Returns the name of the command

   overriding procedure Free (X : in out Custom_Action_Record);
   --  Free memory associated to X

   overriding function Execute
     (Command : access Custom_Action_Record) return Command_Return_Type;
   overriding procedure Interrupt (Command : in out Custom_Action_Record);
   --  Do not do anything. This command is not aimed at being used in the
   --  task manager.

   type Custom_Action_Access is access all Custom_Action_Record;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Custom_Action_Record, Custom_Action_Access);

   procedure Free (X : in out Custom_Action_Access);
   --  Free memory associated to X

   function Get_Process_Class (Kernel : access Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the process class

   type Action_Property is new Instance_Property_Record with record
      Action : Custom_Action_Access;
   end record;
   type Action_Property_Access is access all Action_Property'Class;

   procedure In_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   procedure Out_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   procedure Died_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address);
   --  Various filters to trace the external process executing when traces are
   --  activated

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Exit_Cb (D : in out Custom_Action_Record);
   --  Called when an external process has finished running

   procedure Before_Kill_Cb (D : Custom_Action_Record);
   --  Called before killing the external process

   procedure Output_Cb (D : Custom_Action_Access; Output : String);
   --  Called when an external process has produced some output

   procedure Concat (S : in out String_Access; S2 : String);
   --  Append S2 at the end of S

   function To_String (S : String_Access) return String;
   --  Return the contents of S. if S is null, return ""

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Action_Access;
   function Get_Data (Inst : Class_Instance) return Custom_Action_Access;
   --  Get or store some data in an instance of GPS.Process

   procedure Custom_Spawn_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the expect interface

   type Exit_Type is (Matched, Timed_Out, Died);
   procedure Interactive_Expect
     (Kernel   : access Kernel_Handle_Record'Class;
      Action   : Custom_Action_Access;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Output   : out String_Access;
      Exit_Why : out Exit_Type);
   --  Execute a call to Expect, but process the gtk+ events periodically.
   --  If the pattern is the empty string, this function will only return when
   --  the process has terminated (you must however set the timeout to -1).
   --  On exit, Output contains the whole output of the process since the start
   --  of the call, no matter what was sent to On_Match in the meantime. It is
   --  the responsability of the caller to free Output

   function Strip_CR_Last (S : String) return String;
   --  Strip last character if it's a CR.

   -------------------
   -- Strip_CR_Last --
   -------------------

   function Strip_CR_Last (S : String) return String is
   begin
      if S'Length > 0 and then S (S'Last) = ASCII.CR then
         return S (S'First .. S'Last - 1);
      else
         return S;
      end if;
   end Strip_CR_Last;

   ----------
   -- Name --
   ----------

   overriding function Name (X : access Custom_Action_Record) return String is
   begin
      if X.Command /= Empty_Command_Line then
         return Get_Command (X.Command);
      end if;

      return "expect";
   end Name;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Custom_Action_Record) is
   begin
      Unchecked_Free (X.Pattern);
      Free (X.Unmatched_Output);
      Free (X.On_Exit);
      Free (X.On_Match);
      Unchecked_Free (X.Progress_Regexp);
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

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Custom_Action_Record) is
   begin
      Before_Kill_Cb (Command);
      Interrupt      (Command.Pd.all);
      Close          (Command.Pd.all, Command.Status);
      Exit_Cb        (Command);
      Command.Pd := null;
   end Interrupt;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Custom_Action_Record) return Command_Return_Type
   is
      Res       : Boolean;
      Result    : Expect_Match;
      All_Match : constant Pattern_Matcher := Compile (".+", Single_Line);
   begin
      if not Command.Started then
         Command.Started := True;

         declare
            Kernel : constant Kernel_Handle :=
                       Get_Kernel (Custom_Module_ID.all);
         begin
            Spawn
              (Kernel,
               Command.Command,
               Command.Server,
               Command.Pd,
               Res,
               Console      => Get_Console (Kernel),
               Show_Command => Command.Show_Command);
         exception
            when Invalid_Process =>
               Res := False;
         end;

         if Res then
            if Active (Me) then
               Add_Filter (Command.Pd.all,
                           Filter => In_Trace_Filter'Access,
                           Filter_On => Input);
               Add_Filter (Command.Pd.all,
                           Filter => Out_Trace_Filter'Access,
                           Filter_On => Output);
               Add_Filter (Command.Pd.all,
                           Filter => Died_Trace_Filter'Access,
                           Filter_On => Died);
            end if;

            return Execute_Again;
         else
            return Failure;
         end if;

      elsif Command.Pd /= null then
         if not Command.In_Expect then
            Expect (Command.Pd.all, Result, All_Match, Timeout => 1);
            if Result /= Expect_Timeout then
               if Command.Strip_CR then
                  Output_Cb
                    (Custom_Action_Access (Command),
                     Strip_CR (Strip_CR_Last (Expect_Out (Command.Pd.all))));
               else
                  Output_Cb (Custom_Action_Access (Command),
                             Expect_Out (Command.Pd.all));
               end if;
            end if;
         end if;

         return Execute_Again;
      else
         return Failure;
      end if;

   exception
      when Process_Died =>
         if not Command.In_Expect and then Command.Pd /= null then
            if Command.Strip_CR then
               Output_Cb
                 (Custom_Action_Access (Command),
                  Strip_CR (Strip_CR_Last (Expect_Out (Command.Pd.all))));
            else
               Output_Cb (Custom_Action_Access (Command),
                          Expect_Out (Command.Pd.all));
            end if;
         end if;

         Close (Command.Pd.all, Command.Status);

         if not Command.In_Expect then
            Exit_Cb (Command.all);
         end if;

         Command.Pd := null;

         if Command.Status /= 0 then
            return Failure;
         else
            return Success;
         end if;
   end Execute;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Inst : Class_Instance) return Custom_Action_Access is
      Action : constant Action_Property_Access := Action_Property_Access
        (Instance_Property'(Get_Data (Inst, Process_Class_Name)));
   begin
      if Action = null then
         return null;
      else
         return Action.Action;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Action_Access
   is
      Process_Class : constant Class_Type :=
                        Get_Process_Class (Get_Kernel (Data));
      Inst          : constant Class_Instance :=
                        Nth_Arg (Data, N, Process_Class);
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
      elsif S2 /= "" then
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

   procedure Exit_Cb (D : in out Custom_Action_Record) is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if D.Pd /= null then
         Trace (Me, "Exiting");

         if D.On_Exit /= null then
            if D.End_Action /= null then
               --  An End_Action command is pending: Just update its status
               --  and unmatched_output values.
               D.End_Action.Status           := D.Status;
               D.End_Action.Unmatched_Output := D.Unmatched_Output;
               D.Unmatched_Output := null;

            else
               declare
                  C : Callback_Data'Class := Create
                    (Get_Script (D.Inst), Arguments_Count => 3);
               begin
                  Set_Nth_Arg (C, 1, D.Inst);
                  Set_Nth_Arg (C, 2, D.Status);
                  Set_Nth_Arg (C, 3, To_String (D.Unmatched_Output));
                  Tmp := Execute (D.On_Exit, C);
                  Free (C);
               end;
            end if;

            --  Avoid running a second time
            D.On_Exit := null;
         end if;

         D.Pd := null;
      end if;

      --  ??? Add exception handler ?
   end Exit_Cb;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access End_Action_Record) return Command_Return_Type
   is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
      C : Callback_Data'Class := Create
        (Get_Script (Command.Inst), Arguments_Count => 3);
   begin
      Set_Nth_Arg (C, 1, Command.Inst);
      Set_Nth_Arg (C, 2, Command.Status);
      Set_Nth_Arg (C, 3, To_String (Command.Unmatched_Output));
      Tmp := Execute (Command.On_Exit, C);
      Free (C);

      return Success;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out End_Action_Record) is
   begin
      Free (X.Unmatched_Output);
      Free (X.On_Exit);
   end Free;

   --------------------
   -- Before_Kill_Cb --
   --------------------

   procedure Before_Kill_Cb (D : Custom_Action_Record) is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      if D.Pd /= null then
         if D.Before_Kill /= null then
            declare
               C : Callback_Data'Class := Create
                 (Get_Script (D.Inst), Arguments_Count => 2);
            begin
               Set_Nth_Arg (C, 1, D.Inst);
               Set_Nth_Arg (C, 2, To_String (D.Unmatched_Output));
               Tmp := Execute (D.Before_Kill, C);
               Free (C);
            end;
         end if;

      end if;
   end Before_Kill_Cb;

   ---------------
   -- Output_Cb --
   ---------------

   procedure Output_Cb (D : Custom_Action_Access; Output : String) is
      Matches           : Match_Array (0 .. Max_Paren_Count);
      Beg_Index         : Natural;
      End_Index         : Natural;
      Prev_Beg          : Natural;
      Action_To_Execute : Subprogram_Type;
      Index             : Natural;
      Index_Start       : Natural;
      Current, Final    : Natural;

   begin
      --  First check the progress regexp

      if D.Progress_Regexp /= null then
         Concat (D.Unmatched_Output, Output);

         declare
            Outp : String_Access := D.Unmatched_Output;
         begin
            Index := Outp'First;
            D.Unmatched_Output := new String'("");
            while Index <= Outp'Last loop
               Index_Start := Index;
               Match
                 (D.Progress_Regexp.all, Outp (Index .. Outp'Last), Matches,
                  Data_First => Index);
               exit when Matches (0) = No_Match
                 or else Matches (D.Progress_Current) = No_Match
                 or else Matches (D.Progress_Final) = No_Match;

               Concat
                 (D.Unmatched_Output, Outp (Index .. Matches (0).First - 1));

               Current := Safe_Value
                 (Outp (Matches (D.Progress_Current).First ..
                    Matches (D.Progress_Current).Last));
               Final := Safe_Value
                 (Outp (Matches (D.Progress_Final).First ..
                    Matches (D.Progress_Final).Last));
               Set_Progress
                 (D,
                  Progress_Record'
                    (Activity => Running,
                     Current  => Current,
                     Total    => Final));

               Index := Matches (0).Last + 1;

               --  avoid infinite loops
               exit when Index <= Index_Start;
            end loop;

            if Index <= Outp'Last then
               Concat (D.Unmatched_Output, Outp (Index .. Outp'Last));
            end if;

            Free (Outp);
         end;

      else
         --  Always put Output in Unmatched_Output in case interactive_expect
         --  want them.
         Concat (D.Unmatched_Output, Output);
      end if;

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
         return;
      end if;

      --  If we reach this point, this means expects have been registered.

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
            D.Unmatched_Output (Beg_Index .. End_Index),
            Matches);

         if Matches (0) = No_Match then
            exit;
         else
            --  We have found a match.

            declare
               C : Callback_Data'Class := Create
                 (Get_Script (D.Inst), Arguments_Count => 3);
               Tmp  : Boolean;
               pragma Unreferenced (Tmp);
            begin
               Set_Nth_Arg (C, 1, D.Inst);
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

            --  Prevent infinite loops happenning for users specifying
            --  regexps that match on empty string.

            if Prev_Beg = Beg_Index then
               exit;
            else
               Prev_Beg := Beg_Index;
            end if;
         end if;
      end loop;

      --  If we have matched something, do the necessary adjustments.
      --  Reduce the output that we have already matched.

      if Beg_Index > D.Unmatched_Output'First then
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

   procedure Interactive_Expect
     (Kernel   : access Kernel_Handle_Record'Class;
      Action   : Custom_Action_Access;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Output   : out String_Access;
      Exit_Why : out Exit_Type)
   is
      Regexp  : constant Pattern_Matcher := Compile (Pattern, Multiple_Lines);
      Dead    : Boolean;
      Start   : Ada.Calendar.Time;
      Result  : Expect_Match;
      pragma Unreferenced (Kernel, Dead);

   begin
      if Active (Me) then
         Trace (Me, "Expect " & Pattern & " Timeout=" & Timeout'Img);
      end if;

      Free (Output);
      Output := new String'("");

      --  Set the In_Expect flag so that Execute does not perform its own
      --  call to Expect, thus consuming all characters. This is taken care of
      --  below, instead

      Action.In_Expect := True;

      Start := Ada.Calendar.Clock;

      if Action.Pd = null
        and then Action.Unmatched_Output /= null
      then
         Concat (Output, Action.Unmatched_Output.all);
      end if;

      while Action.Pd /= null loop
         --  Check for timeout

         if Timeout /= -1
           and then Ada.Calendar.Clock > Start + (Duration (Timeout) / 1000.0)
         then
            if Active (Me) then
               Trace (Me, "Interactive_Expect: Timed out");
            end if;

            Action.In_Expect := False;
            Exit_Why := Exit_Type'(Timed_Out);
            return;
         end if;

         --  Check for matching output. We use a very small timeout so that
         --  we can also periodically process gtk+ events

         if Pattern = "" then
            Expect (Action.Pd.all, Result, ".+", Timeout => 5);
         else
            Expect (Action.Pd.all, Result, Regexp, Timeout => 5);
         end if;

         --  Let the On_Match callback know about the output

         if Action.Strip_CR then
            declare
               Str : constant String :=
                       Strip_CR (Strip_CR_Last (Expect_Out (Action.Pd.all)));
            begin
               Output_Cb (Action, Str);
               Concat (Output, Str);
            end;
         else
            declare
               Str : constant String := Expect_Out (Action.Pd.all);
            begin
               Output_Cb (Action, Str);
               Concat (Output, Str);
            end;
         end if;

         if Pattern /= "" and then Result = 1 then
            Trace (Me, "Interactive_Expect: Matched");
            Action.In_Expect := False;
            Exit_Why := Exit_Type'(Matched);
            return;
         end if;

         if Gtk.Main.Events_Pending then
            Dead := Gtk.Main.Main_Iteration;
         end if;
      end loop;

      Action.In_Expect := False;
      Exit_Why := Exit_Type'(Died);

   exception
      when Process_Died =>
         Exit_Cb (Action.all);
         Action.In_Expect := False;
         Exit_Why := Exit_Type'(Died);

      when others =>
         Action.In_Expect := False;
         Exit_Why := Exit_Type'(Died);
   end Interactive_Expect;

   ---------------------
   -- In_Trace_Filter --
   ---------------------

   procedure In_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
      Start : Integer := Str'First;
   begin
      for S in Str'Range loop
         if not Is_Graphic (Str (S)) then
            if S /= Start then
               Trace (Me, "Sending: " & Str (Start .. S - 1));
            end if;
            Trace (Me, "Sending< ASCII." & Character'Image (Str (S)));
            Start := S + 1;
         end if;
      end loop;
      if Str'Last + 1 /= Start then
         Trace (Me, "Sending: " & Str (Start .. Str'Last));
      end if;
   end In_Trace_Filter;

   ----------------------
   -- Out_Trace_Filter --
   ----------------------

   procedure Out_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
      Last : Integer := Str'First;
   begin
      for S in Str'Range loop
         if Str (S) < ' ' then
            if Last < S then
               Trace (Me, "Receiving: " & Str (Last .. S - 1));
            end if;
            Trace (Me, "Receiving< ASCII." & Character'Image (Str (S)));
            Last := S + 1;
         end if;
      end loop;

      if Last <= Str'Last then
         Trace (Me, "Receiving: " & Str (Last .. Str'Last));
      end if;
   end Out_Trace_Filter;

   -----------------------
   -- Died_Trace_Filter --
   -----------------------

   procedure Died_Trace_Filter
     (Description : Process_Descriptor'Class;
      Str         : String;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Description, User_Data);
   begin
      Trace (Me, "Died: " & Str);
   end Died_Trace_Filter;

   Id : Natural := 0;
   function Get_New_Queue_Id return String;
   --  Returns a new unique queue id

   ----------------------
   -- Get_New_Queue_Id --
   ----------------------

   function Get_New_Queue_Id return String is
      Str_Id : constant String := Natural'Image (Id);
   begin
      Id := Id + 1;
      return "expect interface" & Str_Id;
   end Get_New_Queue_Id;

   --------------------------
   -- Custom_Spawn_Handler --
   --------------------------

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel          : constant Kernel_Handle :=
                          Get_Kernel (Custom_Module_ID.all);
      Process_Class   : constant Class_Type :=
                          Get_Process_Class (Get_Kernel (Data));
      D               : Custom_Action_Access;
      E               : Exit_Type;
      Created_Command : Scheduled_Command_Access;
      Dead            : Boolean;
      Output_Str      : String_Access;
      Q_Id            : constant String := Get_New_Queue_Id;
      pragma Unreferenced (Dead);

   begin
      if Command = Constructor_Method then
         declare
            Inst            : constant Class_Instance :=
                                Nth_Arg (Data, 1, Process_Class);
            Command_Line    : constant String := Nth_Arg (Data, 2);
            Regexp          : constant String := Nth_Arg (Data, 3, "");
            Show_Bar        : constant Boolean := Nth_Arg (Data, 6, True);
            Progress_Regexp : constant String := Nth_Arg (Data, 7, "");
            Remote_Server   : constant String := Nth_Arg (Data, 11, "");
            Single_Line     : constant Boolean := Nth_Arg (Data, 13, False);
            Case_Sensitive  : constant Boolean := Nth_Arg (Data, 14, True);
            Strip_CR        : constant Boolean := Nth_Arg (Data, 15, True);
            Active          : constant Boolean := Data.Nth_Arg (16, False);
            Success         : Boolean;
            Flags           : Regexp_Flags := Multiple_Lines;

         begin
            if Command_Line = "" then
               Set_Error_Msg (Data, -"Argument for command cannot be empty");
               return;
            end if;

            Trace (Me, "Spawning Show_Bar=" & Boolean'Image (Show_Bar) &
                   ": " & Command_Line);

            D              := new Custom_Action_Record;
            D.Command      := Parse_String (Command_Line, Separate_Args);
            D.On_Match     := Nth_Arg (Data, 4, null);
            D.On_Exit      := Nth_Arg (Data, 5, null);
            D.Before_Kill  := Nth_Arg (Data, 10, null);
            D.Show_Command := Nth_Arg (Data, 12, False);
            D.Inst         := Inst;
            D.Strip_CR     := Strip_CR;

            if Progress_Regexp /= "" then
               D.Progress_Regexp := new Pattern_Matcher'
                 (Compile (Progress_Regexp, Multiple_Lines));
               D.Progress_Current := Nth_Arg (Data, 8, 1);
               D.Progress_Final   := Nth_Arg (Data, 9, 2);
            end if;

            if Regexp /= "" then
               if Single_Line then
                  Flags := Flags or GNAT.Regpat.Single_Line;
               end if;

               if not Case_Sensitive then
                  Flags := Flags or GNAT.Regpat.Case_Insensitive;
               end if;

               D.Pattern :=
                 new Pattern_Matcher'(Compile (Regexp, Flags));
            end if;

            --  Get the Server_Type value
            begin
               if Remote_Server = "" then
                  D.Server := GPS_Server;
               else
                  D.Server := Server_Type'Value (Remote_Server);
               end if;
            exception
               when Constraint_Error =>
                  D.Server := GPS_Server;
            end;

            if not Is_Local (D.Server) then
               --  In case of remote execution, we cannot spawn the process
               --  here because we need to first synchronize the files between
               --  the local host and the remote server. We will spawn it later
               --  when the first execution of the D command will occur.
               D.Started := False;
               Synchronize (Kernel, GPS_Server, D.Server,
                            Blocking      => False,
                            Print_Command => D.Show_Command,
                            Print_Output  => False,
                            Force         => False,
                            Queue_Id      => Q_Id);
            else
               D.Started := True;

               begin
                  Spawn
                    (Kernel,
                     D.Command,
                     D.Server,
                     D.Pd,
                     Success,
                     Console      => Get_Console (Kernel),
                     Show_Command => D.Show_Command);
               exception
                  when Invalid_Process =>
                     Success := False;
               end;

               if not Success then
                  Free (D);
                  Set_Error_Msg
                    (Data,
                     -"Could not launch command """ & Command_Line & """");

                  return;
               end if;

               if GNATCOLL.Traces.Active (Me) then
                  Add_Filter
                    (D.Pd.all,
                     Filter => In_Trace_Filter'Access,
                     Filter_On => Input);
                  Add_Filter
                    (D.Pd.all,
                     Filter => Out_Trace_Filter'Access,
                     Filter_On => Output);
                  Add_Filter
                    (D.Pd.all,
                     Filter => Died_Trace_Filter'Access,
                     Filter_On => Died);
               end if;
            end if;

            --  Now launch the command
            Created_Command := Launch_Background_Command
              (Kernel   => Kernel,
               Command  => D,
               Active   => Active,
               Show_Bar => Show_Bar,
               Queue_Id => Q_Id);

            if not Is_Local (D.Server) then
               Synchronize (Kernel, D.Server, GPS_Server,
                            Blocking      => False,
                            Print_Command => D.Show_Command,
                            Print_Output  => False,
                            Force         => False,
                            Queue_Id      => Q_Id);

               --  We need to enqueue an End_Action that'll in turn call
               --  On_Exit when executed so that the pending sync action
               --  for remote operation can occur.

               if D.On_Exit /= null then
                  D.End_Action := new End_Action_Record;

                  D.End_Action.Inst    := D.Inst;
                  D.End_Action.On_Exit := D.On_Exit;

                  Launch_Background_Command
                    (Kernel,
                     D.End_Action,
                     Active   => False,
                     Show_Bar => True,
                     Queue_Id => Q_Id);
               end if;
            end if;

            Set_Instance (Created_Command, Get_Script (Data), Inst);
            Set_Data
              (Inst, Process_Class_Name, Action_Property'(Action => D));
         end;

      elsif Command = "send" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Pd /= null then
            Send (D.Pd.all,
                  Str => Nth_Arg (Data, 2),
                  Add_LF => Nth_Arg (Data, 3, True));
         end if;

      elsif Command = "interrupt" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Pd /= null then
            Interrupt (D.Pd.all);
         end if;

      elsif Command = "kill" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Pd /= null then
            Close (D.Pd.all);
         end if;

      elsif Command = "wait" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Pd /= null then
            Interactive_Expect
              (Kernel   => Get_Kernel (Data),
               Action   => D,
               Timeout  => -1,
               Pattern  => "",
               Output   => Output_Str,
               Exit_Why => E);
            Free (Output_Str);
            Set_Return_Value (Data, D.Status);
         end if;

      elsif Command = "set_size" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Pd /= null then
            Set_Size (TTY_Process_Descriptor'Class (D.Pd.all),
                      Nth_Arg (Data, 2), Nth_Arg (Data, 3));
         end if;

      elsif Command = "expect" then
         D := Get_Data (Data, 1);
         if D /= null then
            Interactive_Expect
              (Kernel   => Get_Kernel (Data),
               Action   => D,
               Timeout  => Nth_Arg (Data, 3, -1),
               Pattern  => Nth_Arg (Data, 2),
               Output   => Output_Str,
               Exit_Why => E);

            case E is
               when Matched =>
                  if D.Pd /= null then
                     Set_Return_Value (Data, Output_Str.all);
                  else
                     Set_Error_Msg (Data, "Process terminated");
                  end if;

               when Timed_Out =>
                  Set_Error_Msg (Data, "timed out");

               when Died =>
                  Set_Error_Msg (Data, "Process terminated");
            end case;

            Free (Output_Str);
         end if;

      elsif Command = "get_result" then
         D := Get_Data (Data, 1);

         if D /= null then
            --  Wait till end
            Interactive_Expect
              (Kernel   => Get_Kernel (Data),
               Action   => D,
               Timeout  => -1,
               Pattern  => "",
               Output   => Output_Str,
               Exit_Why => E);

            Set_Return_Value (Data, Output_Str.all);
            Free (Output_Str);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Custom_Spawn_Handler;

   -----------------------
   -- Get_Process_Class --
   -----------------------

   function Get_Process_Class (Kernel : access Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return New_Class
        (Kernel, Process_Class_Name, New_Class (Kernel, "Command"));
   end Get_Process_Class;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Process_Class : constant Class_Type := Get_Process_Class (Kernel);
   begin
      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params => (2  => Param ("command"),
                    3  => Param ("regexp",                Optional => True),
                    4  => Param ("on_match",              Optional => True),
                    5  => Param ("on_exit",               Optional => True),
                    6  => Param ("task_manager",          Optional => True),
                    7  => Param ("progress_regexp",       Optional => True),
                    8  => Param ("progress_current",      Optional => True),
                    9  => Param ("progress_total",        Optional => True),
                    10 => Param ("before_kill",           Optional => True),
                    11 => Param ("remote_server",         Optional => True),
                    12 => Param ("show_command",          Optional => True),
                    13 => Param ("single_line_regexp",    Optional => True),
                    14 => Param ("case_sensitive_regexp", Optional => True),
                    15 => Param ("strip_cr",              Optional => True),
                    16 => Param ("active",                Optional => True)),

         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("send",
         Params => (2  => Param ("command"),
                    3  => Param ("add_lf", Optional => True)),
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("interrupt",
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("kill",
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("wait",
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_result",
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("expect",
         Params      => (2 => Param ("regexp"),
                         3 => Param ("timeout", Optional => True)),
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
      Kernel.Scripts.Register_Command
        ("set_size",
         Params      => (2 => Param ("rows"),
                         3 => Param ("columns")),
         Class        => Process_Class,
         Handler      => Custom_Spawn_Handler'Access);
   end Register_Commands;

end Expect_Interface;

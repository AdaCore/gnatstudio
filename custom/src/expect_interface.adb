-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                       Copyright (C) 2004                          --
--                            ACT-Europe                             --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Expect;               use GNAT.Expect;
with System;
with Ada.Unchecked_Conversion;

with Glide_Intl;                use Glide_Intl;
with Custom_Module;             use Custom_Module;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;

package body Expect_Interface is

   use Expect_Filter_List;

   Command_Cst         : aliased constant String := "command";
   Regexp_Cst          : aliased constant String := "regexp";
   On_Match_Action_Cst : aliased constant String := "on_match";
   On_Exit_Action_Cst  : aliased constant String := "on_exit";
   Add_Lf_Cst          : aliased constant String := "add_lf";
   Constructor_Args : constant Cst_Argument_List :=
     (Command_Cst'Access, Regexp_Cst'Access, On_Match_Action_Cst'Access,
      On_Exit_Action_Cst'Access);
   Send_Args : constant Cst_Argument_List :=
     (Command_Cst'Access, Add_Lf_Cst'Access);
   Expect_Args : constant Cst_Argument_List :=
     (Regexp_Cst'Access, On_Match_Action_Cst'Access);

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
   --  Get or store some data in an instance of GPS.Process

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the expect interface.

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Custom_Action_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Class_Instance, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Class_Instance);

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Action_Access
   is
      Process_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), "Process");
      Value : constant System.Address := Nth_Arg_Data
        (Data, N, Process_Class);
   begin
      return Convert (Value);
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
      Inst : constant Class_Instance := Convert (Data.Callback_Data);
      D    : constant Custom_Action_Access := Convert (Get_Data (Inst));
      Tmp  : Boolean;
      pragma Unreferenced (Tmp);
   begin
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

      --  Decrement the reference counter for this instance, since the process
      --  is dead. We have no actual control on when exactly it will be freed
      Free (Inst);

      --  ??? Add exception handler ?
   end Exit_Cb;

   ---------------
   -- Output_Cb --
   ---------------

   procedure Output_Cb (Data : Process_Data; Output : String) is
      Inst : constant Class_Instance := Convert (Data.Callback_Data);
      D    : constant Custom_Action_Access := Convert (Get_Data (Inst));
      Matches   : Match_Array (0 .. Max_Paren_Count);
      Beg_Index : Natural;
      End_Index : Natural;
      Prev_Beg  : Natural;
      Node      : List_Node;
      Prev      : List_Node;
      Action_To_Execute : Subprogram_Type;

   begin
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
         Matches (0) := No_Match;

         --  Look for a match in the registered expect filters.

         if not Is_Empty (D.Filters) then
            Node := First (D.Filters);
            Prev := Null_Node;

            while Node /= Null_Node loop
               Match
                 (Expect_Filter_List.Data (Node).Pattern.all,
                  D.Unmatched_Output.all,
                  Matches,
                  Beg_Index, End_Index);

               --  A filter has matched: remove the node from the list and
               --  process the action.
               if Matches (0) /= No_Match then
                  Action_To_Execute := Expect_Filter_List.Data (Node).Action;
                  Remove_Nodes (D.Filters, Prev, Node);
                  exit;
               end if;

               Prev := Node;
               Node := Next (Node);
            end loop;
         end if;

         --  If no filter matched, try with the installed regexp.

         if Matches (0) = No_Match then
            Action_To_Execute := D.On_Match;

            Match
              (D.Pattern.all,
               D.Unmatched_Output.all,
               Matches,
               Beg_Index, End_Index);
         end if;

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

   --------------------------
   -- Custom_Spawn_Handler --
   --------------------------

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Custom_Module_ID.Kernel;
      D : Custom_Action_Access;
      Process_Class : constant Class_Type := New_Class (Kernel, "Process");
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
               Callback_Data => Convert (Inst),
               Line_By_Line  => False,
               Directory     => "",
               Fd            => D.Fd);

            if Success then
               Set_Data (Inst, D.all'Address);
            else
               Free (D);
               Set_Error_Msg
                 (Data, -"Could not launch command """ & Command_Line & """");
            end if;

            --  Instance is automatically destroyed when the process exits.
            Ref (Inst);
         end;

      elsif Command = "send" then
         Name_Parameters (Data, Send_Args);
         D := Get_Data (Data, 1);
         Send (D.Fd.all,
               Str => Nth_Arg (Data, 2),
               Add_LF => Nth_Arg (Data, 3, True));

      elsif Command = "interrupt" then
         D := Get_Data (Data, 1);
         Interrupt (D.Fd.all);

      elsif Command = "kill" then
         D := Get_Data (Data, 1);
         Close (D.Fd.all);

      elsif Command = "expect" then
         Name_Parameters (Data, Expect_Args);
         D := Get_Data (Data, 1);

         declare
            Regexp : constant String  := Nth_Arg (Data, 2);
            Filter : Expect_Filter;
         begin
            if Regexp = "" then
               Set_Error_Msg
                 (Data, -"Cannot register expect for an empty regexp.");
               return;
            end if;

            Filter.Action := Nth_Arg (Data, 3);

            if Filter.Action = null then
               Set_Error_Msg (Data, -"Action not found for expect_action");
               return;
            end if;

            Filter.Pattern :=
              new Pattern_Matcher'(Compile (Regexp, Multiple_Lines));

            Append (D.Filters, Filter);
         end;
      end if;
   end Custom_Spawn_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Process_Class : constant Class_Type := New_Class (Kernel, "Process");
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
        (Kernel, "expect",
         Minimum_Args => 2,
         Maximum_Args => 2,
         Class         => Process_Class,
         Handler       => Custom_Spawn_Handler'Access);
   end Register_Commands;

end Expect_Interface;

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

with Glide_Kernel.Actions;      use Glide_Kernel.Actions;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Expect;               use GNAT.Expect;

with Glide_Intl;                use Glide_Intl;

with Custom_Module;             use Custom_Module;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Timeout;      use Glide_Kernel.Timeout;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;

with String_Utils;              use String_Utils;
with System;
with Ada.Unchecked_Conversion;

package body Expect_Interface is

   use Processes_Hash;
   use Expect_Filter_List;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_New_Id return Integer;
   --  Return an available Id to store a command named Name.
   --  Return "" if there is no available resource for storing this command.

   procedure Exit_Cb (Data : Process_Data; Status : Integer);
   --  Called when an external process has finished running

   procedure Output_Cb (Data : Process_Data; Output : String);
   --  Called when an external process has produced some output.

   procedure Concat (S : in out String_Access; S2 : String);
   --  Append S2 at the end of S.

   function To_String (S : String_Access) return String;
   --  Return the contents of S. if S is null, return "".

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Integer);
   function Convert is new Ada.Unchecked_Conversion
     (Integer, System.Address);

   procedure Execute_Action_With_Args
     (Action : Action_Record_Access;
      Desc   : String;
      Args   : String_List_Access);
   --  Execute Action with Args, with Desc as description.

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

   ------------------------------
   -- Execute_Action_With_Args --
   ------------------------------

   procedure Execute_Action_With_Args
     (Action : Action_Record_Access;
      Desc   : String;
      Args   : String_List_Access)
   is
      Custom : Command_Access;
   begin
      Custom := Create_Proxy
        (Action.Command,
         (null,
          null,
          null,
          Args,
          new String'(Desc)));

      Launch_Background_Command
        (Custom_Module_ID.Kernel,
         Custom,
         True,
         True,
         "",
         True);
   end Execute_Action_With_Args;

   -------------
   -- Exit_Cb --
   -------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer) is
      Id : constant Integer := Convert (Data.Callback_Data);
      D  : Custom_Action_Access;

   begin
      D := Get (Custom_Module_ID.Processes, Id);

      if D = null
        or else D.all = No_Custom_Action
      then
         raise Program_Error;
      end if;

      --  Execute end action.

      if D.On_Exit /= null then
         declare
            Args   : GNAT.OS_Lib.String_List_Access;
         begin
            Args := new GNAT.OS_Lib.String_List (1 .. 3);
            Args (1) := new String'(Image (D.Command_Id));
            Args (2) := new String'(Image (Status));
            Args (3) := new String'
              (To_String (D.Processed_Output)
               & To_String (D.Unmatched_Output));

            Execute_Action_With_Args
              (D.On_Exit,
               D.Command (D.Command'First).all,
               Args);
         end;
      end if;

      --  Remove the process from the table.

      Remove (Custom_Module_ID.Processes, Id);

      --  ??? Add exception handler ?
   end Exit_Cb;

   ---------------
   -- Output_Cb --
   ---------------

   procedure Output_Cb (Data : Process_Data; Output : String) is
      Id        : constant Integer := Convert (Data.Callback_Data);
      D         : Custom_Action_Access;
      Matches   : Match_Array (0 .. Max_Paren_Count);
      Beg_Index : Natural;
      End_Index : Natural;
      Prev_Beg  : Natural;
      Node      : List_Node;
      Prev      : List_Node;
      Action_To_Execute : Action_Record_Access;

   begin
      D := Get (Custom_Module_ID.Processes, Id);

      if D = null
        or else D.all = No_Custom_Action
      then
         raise Program_Error;
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
               Args : String_List_Access;
            begin
               Args := new GNAT.OS_Lib.String_List (1 .. 3);
               Args (1) := new String'(Image (D.Command_Id));
               Args (2) := new String'
                 (D.Unmatched_Output (Matches (0).First .. Matches (0).Last));
               Args (3) := new String'
                 (D.Unmatched_Output (Beg_Index .. Matches (0).First - 1));

               Execute_Action_With_Args
                 (Action_To_Execute,
                  -"Matching " & D.Command (D.Command'First).all,
                  Args);
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

   ----------------
   -- Get_New_Id --
   ----------------

   function Get_New_Id return Integer is
      Result : constant Integer := Custom_Module_ID.Available_Id;
   begin
      if Custom_Module_ID.Available_Id = Integer'Last then
         Custom_Module_ID.Available_Id := 1;
      else
         Custom_Module_ID.Available_Id := Custom_Module_ID.Available_Id + 1;
      end if;

      return Result;
   end Get_New_Id;

   --------------------------
   -- Custom_Spawn_Handler --
   --------------------------

   procedure Custom_Spawn_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Custom_Module_ID.Kernel;

      function Get_Process_For_Id (Id : Integer) return Custom_Action_Access;
      --  Return the process for Id. Return null if the process could not be
      --  found.

      ------------------------
      -- Get_Process_For_Id --
      ------------------------

      function Get_Process_For_Id (Id : Integer) return Custom_Action_Access is
         D : Custom_Action_Access;
      begin
         if Id < Integer (Header_Num'First)
           or else Id > Integer (Header_Num'Last)
         then
            Set_Error_Msg
              (Data, -"Not an ID of a process controlled by GPS:" & Id'Img);
            return null;
         end if;

         D := Get (Custom_Module_ID.Processes, Id);

         if D = null
           or else D.all = No_Custom_Action
         then
            Set_Error_Msg (Data, -"Process not found:" & Id'Img);
            return null;
         end if;

         return D;
      end Get_Process_For_Id;

   begin
      if Command = "spawn" then
         declare
            Command_Line  : constant String := Nth_Arg (Data, 1);
            Regexp        : constant String := Nth_Arg (Data, 2);
            On_Match      : constant String := Nth_Arg (Data, 3);
            On_Exit       : constant String := Nth_Arg (Data, 4);

            Custom_Action : Custom_Action_Record;
            Success       : Boolean;
         begin
            if Command_Line = "" then
               Set_Error_Msg (Data, -"Argument for command cannot be empty");
               return;
            end if;

            Custom_Action.Command := Argument_String_To_List (Command_Line);
            Custom_Action.Command_Id := Get_New_Id;

            if On_Exit /= "" then
               Custom_Action.On_Exit := Lookup_Action (Kernel, On_Exit);

               if Custom_Action.On_Exit = null then
                  Set_Error_Msg (Data, -"Action not found for on_exit_action");
                  return;
               end if;
            end if;

            if On_Match /= "" then
               Custom_Action.On_Match := Lookup_Action (Kernel, On_Match);

               if Custom_Action.On_Match = null then
                  Set_Error_Msg
                    (Data, -"Action not found for on_match_action");
                  return;
               end if;
            end if;

            if Regexp /= "" then
               Custom_Action.Pattern :=
                 new Pattern_Matcher'
                   (Compile (Regexp, Multiple_Lines));
            end if;

            --  All the parameters are correct: launch the process.

            Launch_Process
              (Kernel    => Kernel,
               Command   => Custom_Action.Command
                 (Custom_Action.Command'First).all,
               Arguments => Custom_Action.Command
                 (Custom_Action.Command'First .. Custom_Action.Command'Last),

               Console   => null,
               --  ??? Should we add an optional parameter allowing users to
               --  create a console for their processes ?

               Callback  => Output_Cb'Access,
               Exit_Cb   => Exit_Cb'Access,
               Success   => Success,

               Show_Command => False,
               Callback_Data => Convert (Custom_Action.Command_Id),

               Line_By_Line  => False,
               Directory     => "",
               Fd            => Custom_Action.Fd);

            if Success then
               --  Add the process to the HTable of launched processes.

               Set
                 (Custom_Module_ID.Processes,
                  Custom_Action.Command_Id,
                  new Custom_Action_Record'(Custom_Action));

               Set_Return_Value (Data, Custom_Action.Command_Id);
            else
               Free (Custom_Action);

               Set_Error_Msg
                 (Data, -"Could not launch command """ & Command_Line & """");
               return;
            end if;

         end;

      elsif Command = "send" then
         declare
            Id           : constant Integer := Nth_Arg (Data, 1);
            Command_Line : constant String  := Nth_Arg (Data, 2);
            Add_LF       : constant Boolean := Nth_Arg (Data, 3, True);
            D            : Custom_Action_Access;
         begin
            D := Get_Process_For_Id (Id);

            if D = null then
               return;
            end if;

            Send (D.Fd.all, Command_Line, Add_LF);
         end;

      elsif Command = "interrupt" or else Command = "kill" then
         declare
            Id : constant Integer := Nth_Arg (Data, 1);
            D  : Custom_Action_Access;
            S  : Integer;
         begin
            D := Get_Process_For_Id (Id);

            if D = null then
               return;
            end if;

            if Command = "kill" then
               Close (D.Fd.all, S);
            else
               Interrupt (D.Fd.all);
            end if;
         end;

      elsif Command = "expect" then
         declare
            Id     : constant Integer := Nth_Arg (Data, 1);
            Regexp : constant String  := Nth_Arg (Data, 2);
            Action : constant String  := Nth_Arg (Data, 3);
            D      : Custom_Action_Access;
            Filter : Expect_Filter;
         begin
            D := Get_Process_For_Id (Id);

            if D = null then
               return;
            end if;

            if Regexp = "" then
               Set_Error_Msg
                 (Data, -"Cannot register expect for an empty regexp.");
               return;
            end if;

            Filter.Action := Lookup_Action (Custom_Module_ID.Kernel, Action);

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

end Expect_Interface;

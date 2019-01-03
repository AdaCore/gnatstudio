------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, ".*is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, ".*is an internal GNAT unit");

with GNAT.Expect;             use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;         use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Arg_Lists;      use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

with Gtk.Main;

with Basic_Types;             use Basic_Types;
with Custom_Module;           use Custom_Module;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel.Timeout;      use GPS.Kernel.Timeout;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Remote;                  use Remote;
with Commands;                use Commands;

package body Expect_Interface is

   Me : constant Trace_Handle := Create ("Expect", Off);

   Process_Class_Name   : constant String := "Process";

   type Custom_Action_Data is new External_Process_Data with record
      Inst             : Class_Instance;
      On_Match         : Subprogram_Type;
      On_Exit          : Subprogram_Type;
      Before_Kill      : Subprogram_Type;
      Output_Regexp    : GNAT.Expect.Pattern_Matcher_Access;
      Unmatched_Output : Unbounded_String;
   end record;
   overriding procedure Free (Self : in out Custom_Action_Data);
   overriding procedure On_Exit
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class);
   overriding procedure On_Output
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class;
      Output   : String);
   overriding procedure On_Before_Kill
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class);

   function Get_Process_Class (Kernel : access Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the process class

   type Action_Property is new Instance_Property_Record with record
      Action : access Custom_Action_Data;
   end record;
   type Action_Property_Access is access all Action_Property'Class;

   type Exit_Type is (Matched, Timed_Out, Died);
   procedure Interactive_Expect
     (Action   : not null access Custom_Action_Data'Class;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Output   : out Unbounded_String;
      Exit_Why : out Exit_Type);
   --  DO NOT USE IN ADA CODE
   --
   --  This procedure blocks GPS until some output of the external process
   --  matches the given pattern. At this point, the full output of the
   --  process since the beginning of the call to Interactive_Expect is
   --  returned.
   --  GPS will keep refreshing while this is running, which might be an issue
   --  since some events (like mouse events) might result in other actions
   --  being performed and GPS is not meant for that.
   --  The usual calls to On_Output, On_Exit,... are performed by this
   --  procedure.
   --
   --  If the pattern is the empty string, this function will only return when
   --  the process has terminated (you must however set the timeout to -1).

   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
      return access Custom_Action_Data'Class;
   function Get_Data
     (Inst : Class_Instance) return access Custom_Action_Data'Class;
   --  Get or store some data in an instance of GPS.Process

   procedure Custom_Spawn_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the expect interface

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Custom_Action_Data) is
   begin
      Set_Data
        (Self.Inst, Process_Class_Name, Action_Property'(Action => null));

      Free (External_Process_Data (Self));  --  inherited
      Free (Self.On_Exit);
      Free (Self.On_Match);
      Free (Self.Before_Kill);
      Unchecked_Free (Self.Output_Regexp);
   end Free;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Inst : Class_Instance) return access Custom_Action_Data'Class
   is
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
     (Data : Callback_Data'Class; N : Positive)
      return access Custom_Action_Data'Class
   is
      Process_Class : constant Class_Type :=
                        Get_Process_Class (Get_Kernel (Data));
      Inst          : constant Class_Instance :=
                        Nth_Arg (Data, N, Process_Class);
   begin
      return Get_Data (Inst);
   end Get_Data;

   -------------
   -- On_Exit --
   -------------

   overriding procedure On_Exit
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class)
   is
      pragma Unreferenced (External);
   begin
      if Self.On_Exit /= null then
         declare
            Dummy : Boolean;
            C   : Callback_Data'Class := Create
              (Get_Script (Self.Inst), Arguments_Count => 3);
         begin
            Set_Nth_Arg (C, 1, Self.Inst);
            Set_Nth_Arg (C, 2, Self.Exit_Status);
            Set_Nth_Arg (C, 3, To_String (Self.Unmatched_Output));
            Dummy := Execute (Self.On_Exit, C);
            Free (C);
         end;
      end if;
   end On_Exit;

   --------------------
   -- On_Before_Kill --
   --------------------

   overriding procedure On_Before_Kill
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class)
   is
      pragma Unreferenced (External);
      Dummy : Boolean;
   begin
      if Self.Descriptor /= null
        and then Self.Before_Kill /= null
      then
         declare
            C : Callback_Data'Class := Create
              (Get_Script (Self.Inst), Arguments_Count => 2);
         begin
            Set_Nth_Arg (C, 1, Self.Inst);
            Set_Nth_Arg (C, 2, To_String (Self.Unmatched_Output));
            Dummy := Execute (Self.Before_Kill, C);
            Free (C);
         end;
      end if;
   end On_Before_Kill;

   ---------------
   -- On_Output --
   ---------------

   overriding procedure On_Output
     (Self     : not null access Custom_Action_Data;
      External : not null access Root_Command'Class;
      Output   : String)
   is
      pragma Unreferenced (External);
      Matches           : Match_Array (0 .. Max_Paren_Count);
      Index             : Natural;
      Dummy             : Boolean;
   begin
      Append (Self.Unmatched_Output, Output);

      if Self.On_Match = null then
         return;
      end if;

      Index := 1;  --  first index in unbounded string, always

      loop
         declare
            S : Big_String_Access;
            L : Natural;
         begin
            Ada.Strings.Unbounded.Aux.Get_String (Self.Unmatched_Output, S, L);
            exit when Index > L;

            Match
              (Self.Output_Regexp.all,
               Data        => S.all,
               Data_First  => Index,
               Data_Last   => L,
               Matches     => Matches);

            exit when Matches (0) = No_Match;

            declare
               C : Callback_Data'Class := Create
                 (Get_Script (Self.Inst), Arguments_Count => 3);
               Dummy  : Boolean;
            begin
               Set_Nth_Arg (C, 1, Self.Inst);
               Set_Nth_Arg (C, 2, S (Matches (0).First .. Matches (0).Last));
               Set_Nth_Arg (C, 3, S (Index .. Matches (0).First - 1));
               Dummy := Execute (Self.On_Match, C);
               Free (C);
            end;

            --  Prevent infinite loop when matching on empty strings
            Index := Natural'Max (Index + 1, Matches (0).Last + 1);
         end;
      end loop;

      Self.Unmatched_Output := Unbounded_Slice
        (Self.Unmatched_Output, Index, Length (Self.Unmatched_Output));
   end On_Output;

   ------------------------
   -- Interactive_Expect --
   ------------------------

   procedure Interactive_Expect
     (Action   : not null access Custom_Action_Data'Class;
      Timeout  : Integer := 200;
      Pattern  : String := "";
      Output   : out Unbounded_String;
      Exit_Why : out Exit_Type)
   is
      Regexp  : constant Pattern_Matcher := Compile
        ((if Pattern = "" then ".+" else Pattern), Multiple_Lines);
      Dummy   : Boolean;
      Start   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Str     : Unbounded_String;

   begin
      if Active (Me) then
         Trace (Me, "Interactive_Expect " & Pattern
                & " Timeout=" & Timeout'Img);
      end if;

      Output := Action.Unmatched_Output;
      Action.Suspend_Monitoring (True);
      Exit_Why := Exit_Type'(Died);

      loop
         case Action.Expect
           (Regexp, Timeout => 5, Output => Str,
            Stop_At_First_Match => Pattern /= "")
         is
            when Matched =>
               Append (Output, Str);
               Exit_Why := Exit_Type'(Matched);

               if Pattern /= "" then
                  Action.Unmatched_Output := Null_Unbounded_String;
                  exit;
               end if;

            when Timed_Out =>
               Append (Output, Str);
               if Timeout /= -1
                 and then Ada.Calendar.Clock - Start >
                   (Duration (Timeout) / 1000.0)
               then
                  Exit_Why := Exit_Type'(Timed_Out);
                  exit;
               end if;

            when Died =>
               Append (Output, Str);
               --  All callbacks (On_Exit, ...) were already called
               exit;
         end case;

         if Gtk.Main.Events_Pending then
            Dummy := Gtk.Main.Main_Iteration;
         end if;
      end loop;

      Action.Suspend_Monitoring (False);
   exception
      when others =>
         Action.Suspend_Monitoring (False);
         raise;
   end Interactive_Expect;

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
      D               : access Custom_Action_Data'Class;
      E               : Exit_Type;
      Dummy           : Boolean;
      Str             : Unbounded_String;
      CL              : Arg_List;

   begin
      if Command = Constructor_Method then
         --  Do we have a string as parameter ?
         begin
            CL := Parse_String (Data.Nth_Arg (2), Separate_Args);
         exception
            when Invalid_Parameter =>
               --  Assume we have a list
               declare
                  List : constant List_Instance := Data.Nth_Arg (2);
               begin
                  for A in 1 .. List.Number_Of_Arguments loop
                     Append_Argument (CL, List.Nth_Arg (A), One_Arg);
                  end loop;
               end;
         end;

         declare
            Inst  : constant Class_Instance := Data.Nth_Arg (1, Process_Class);
            Remote_Server   : constant String := Data.Nth_Arg (11, "");
            Dirname         : constant String := Data.Nth_Arg (17, "");
            Server          : Server_Type;
            Success         : Boolean;
            Created_Command : Scheduled_Command_Access;
            Dir             : Virtual_File := No_File;
         begin
            if Args_Length (CL) = -1 then
               Set_Error_Msg (Data, -"Argument for command cannot be empty");
               return;
            end if;

            D := new Custom_Action_Data;
            D.Inst         := Inst;
            D.On_Match     := Data.Nth_Arg (4, null);
            D.On_Exit      := Data.Nth_Arg (5, null);
            D.Before_Kill  := Data.Nth_Arg (10, null);
            D.Output_Regexp := new Pattern_Matcher'
              (Compile
                 (Data.Nth_Arg (3, ""),
                  Flags =>
                    Multiple_Lines
                  or
                    (if Data.Nth_Arg (13, False) then Single_Line else 0)
                  or
                    (if Data.Nth_Arg (14, True)
                     then Case_Insensitive else 0)));

            begin
               if Remote_Server = "" then
                  Server := GPS_Server;
               else
                  Server := Server_Type'Value (Remote_Server);
               end if;
            exception
               when Constraint_Error =>
                  Server := GPS_Server;
            end;

            if Dirname /= "" then
               Dir := Create (+Dirname);
            end if;

            D.Set_Progress_Regexp
              (Regexp            => Data.Nth_Arg (7, ""),
               Group_For_Current => Data.Nth_Arg (8, 1),
               Group_For_Total   => Data.Nth_Arg (9, 2));

            GPS.Kernel.Timeout.Launch_Process
              (Scheduled            => Created_Command,
               Success              => Success,
               Data                 => D,
               Kernel               => Kernel,
               CL                   => CL,
               Server               => Server,
               Directory            => Dir,
               Console              => Get_Console (Kernel),
               Show_Command         => Data.Nth_Arg (12, False),
               Show_Output          => False,
               Active               => Data.Nth_Arg (16, False),
               Start_Immediately    => True,
               Line_By_Line         => False,
               Show_In_Task_Manager => Data.Nth_Arg (6, True),
               Name_In_Task_Manager => Get_Command (CL),
               Synchronous          => False,
               Block_Exit           => Data.Nth_Arg (18, True),
               Strip_CR             => Data.Nth_Arg (15, True));

            if not Success then
               Data.Set_Error_Msg
                 (-"Could not launch command """
                  & To_Display_String (CL) & """");
               return;
            end if;

            Set_Command (Inst, Created_Command);
            Set_Data
              (Inst, Process_Class_Name, Action_Property'(Action => D));
         end;

      elsif Command = "send" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Descriptor /= null then
            Send (D.Descriptor.all,
                  Str => Nth_Arg (Data, 2),
                  Add_LF => Nth_Arg (Data, 3, True));
         end if;

      elsif Command = "interrupt" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Descriptor /= null then
            Interrupt (D.Descriptor.all);
         end if;

      elsif Command = "kill" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Descriptor /= null then
            Close (D.Descriptor.all);
         end if;

      elsif Command = "wait" then
         D := Get_Data (Data, 1);
         if D /= null then
            Interactive_Expect
              (Action   => D,
               Timeout  => -1,
               Pattern  => "",
               Output   => Str,
               Exit_Why => E);
            Data.Set_Return_Value (D.Exit_Status);
         else
            Data.Set_Return_Value (-1);
         end if;

      elsif Command = "set_size" then
         D := Get_Data (Data, 1);
         if D /= null and then D.Descriptor /= null then
            Set_Size (TTY_Process_Descriptor'Class (D.Descriptor.all),
                      Nth_Arg (Data, 2), Nth_Arg (Data, 3));
         end if;

      elsif Command = "expect" then
         D := Get_Data (Data, 1);
         if D /= null then
            Interactive_Expect
              (Action   => D,
               Timeout  => Nth_Arg (Data, 3, -1),
               Pattern  => Nth_Arg (Data, 2),
               Output   => Str,
               Exit_Why => E);

            case E is
               when Matched =>
                  if D.Descriptor /= null then
                     Data.Set_Return_Value (To_String (Str));
                  else
                     Data.Set_Error_Msg ("Process terminated");
                  end if;

               when Timed_Out =>
                  Data.Set_Error_Msg ("timed out");

               when Died =>
                  Data.Set_Error_Msg ("Process terminated");
            end case;
         end if;

      elsif Command = "get_result" then
         D := Get_Data (Data, 1);

         if D /= null then
            --  Wait till end
            Interactive_Expect
              (Action   => D,
               Timeout  => -1,
               Pattern  => "",
               Output   => Str,
               Exit_Why => E);
            Data.Set_Return_Value (To_String (Str));
         end if;
      end if;
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
                    16 => Param ("active",                Optional => True),
                    17 => Param ("directory",             Optional => True),
                    18 => Param ("block_exit",            Optional => True)),
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

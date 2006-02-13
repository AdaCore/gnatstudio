------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Text_IO;

with GNAT.Regpat;       use GNAT.Regpat;

with String_Utils;      use String_Utils;

package body GNAT.Expect.TTY.Remote is

   type Compiled_Regexp_Array_Access is access Compiled_Regexp_Array;

   type Remote_Descriptor;
   type Remote_Descriptor_Access is access all Remote_Descriptor;

   type Remote_Descriptor is record
      Name                  : String_Access            := null;
      Start_Cmd             : String_Access            := null;
      Start_Cmd_Common_Args : String_List_Access       := null;
      Start_Cmd_User_Args   : String_List_Access       := null;
      User_Prompt_Ptrn      : Pattern_Matcher_Access   := null;
      Password_Prompt_Ptrn  : Pattern_Matcher_Access   := null;
      Next                  : Remote_Descriptor_Access := null;
   end record;

   type Shell_Descriptor is record
      Name             : String_Access                := null;
      Start_Cmd        : String_Access                := null;
      Init_Cmds        : String_List_Access           := null;
      Exit_Cmds        : String_List_Access           := null;
      Cd_Cmd           : String_Access                := null;
      Get_Status_Cmd   : String_Access                := null;
      Get_Status_Ptrn  : Pattern_Matcher_Access       := null;
      Generic_Prompt   : Pattern_Matcher_Access       := null;
      Prompt           : Pattern_Matcher_Access       := null;
      Echoing          : Boolean                      := False;
      Wrapper          : Cmd_Wrapper_Function         := null;
      Output_Processor : Output_Processor_Procedure   := null;
      Int_Cmd          : String_Access                := null;
      Next             : Shell_Descriptor_Access      := null;
   end record;

   Remote_Descriptor_List : Remote_Descriptor_Access := null;

   Shell_Descriptor_List : Shell_Descriptor_Access := null;

   Shell_Selec         : Config_Selector_Function := null;
   Remote_Access_Selec : Config_Selector_Function := null;

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Filter_Out (Descriptor : Process_Descriptor'Class;
                         Str        : String;
                         User_Data  : System.Address := System.Null_Address);
   --  Used to filter the shell output (removes shell commands)

   ---------
   -- Log --
   ---------

   procedure Log (Where : in String;
                  What  : in String);

   procedure Log (Where : in String;
                  What  : in String) is
      function Clean_Up (Str : in String) return String;
      function Clean_Up (Str : in String) return String is
         Out_S : String := Str;
      begin
         for I in Str'Range loop
            if Str (I) = ASCII.NUL then
               Out_S (I) := '@';
            end if;
         end loop;
         return Out_S;
      end Clean_Up;
   begin
      Ada.Text_IO.Put_Line ("(" & Where & "): '" & Clean_Up (What) & "'");
   end Log;

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function;
      Filter_On  : Filter_Type := Output;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False)
   is
      Current : Filter_List := Descriptor.R_Filters;
   begin
      if Filter_On /= Output then
         Add_Filter (TTY_Process_Descriptor (Descriptor),
                     Filter, Filter_On, User_Data, After);
      elsif After then
         while Current /= null and then Current.Next /= null loop
            Current := Current.Next;
         end loop;

         if Current = null then
            Descriptor.R_Filters :=
              new Filter_List_Elem'
               (Filter => Filter, Filter_On => Filter_On,
                User_Data => User_Data, Next => null);
         else
            Current.Next :=
              new Filter_List_Elem'
              (Filter => Filter, Filter_On => Filter_On,
               User_Data => User_Data, Next => null);
         end if;

      else
         Descriptor.R_Filters :=
           new Filter_List_Elem'
             (Filter => Filter, Filter_On => Filter_On,
              User_Data => User_Data, Next => Descriptor.R_Filters);
      end if;
   end Add_Filter;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function)
   is
      Previous : Filter_List := null;
      Current  : Filter_List := Descriptor.R_Filters;

   begin
      Remove_Filter (TTY_Process_Descriptor (Descriptor),
                     Filter);
      while Current /= null loop
         if Current.Filter = Filter then
            if Previous = null then
               Descriptor.R_Filters := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
         end if;

         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove_Filter;

   ------------------
   -- Lock_Filters --
   ------------------

   procedure Lock_Filters (Descriptor : in out Remote_Process_Descriptor) is
   begin
      Descriptor.R_Filters_Lock := Descriptor.R_Filters_Lock + 1;
   end Lock_Filters;

   --------------------
   -- Unlock_Filters --
   --------------------

   procedure Unlock_Filters (Descriptor : in out Remote_Process_Descriptor) is
   begin
      if Descriptor.R_Filters_Lock > 0 then
         Descriptor.R_Filters_Lock := Descriptor.R_Filters_Lock - 1;
      end if;
   end Unlock_Filters;

   ----------
   -- Send --
   ----------

   procedure Send
     (Descriptor   : in out Remote_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      TTY_Descriptor : TTY_Process_Descriptor
      renames TTY_Process_Descriptor (Descriptor);
   begin
      if Descriptor.Shell.Wrapper /= null then
         Send (TTY_Descriptor, Descriptor.Shell.Wrapper (Str),
               Add_LF, Empty_Buffer);
      else
         Send (TTY_Descriptor, Str, Add_LF, Empty_Buffer);
      end if;
      if Str /= "" then
         Log ("Send", Str);
         Descriptor.Current_Echo_Skipped := False;
      end if;
   end Send;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   procedure Remote_Spawn
     (Descriptor          : out Remote_Process_Descriptor;
      Target_Name         : String;
      Target_Identifier   : String;
      Local_Args          : GNAT.OS_Lib.Argument_List;
      Args                : GNAT.OS_Lib.Argument_List;
      Execution_Directory : String := "";
      User_Name           : String := "";
      Launch_Timeout      : Natural := 5000;
      Err_To_Out          : Boolean := False)
   is
      Res         : Expect_Match;
      Remote_Desc : Remote_Descriptor_Access := Remote_Descriptor_List;
      New_Args    : String_List_Access;
      Old_Args    : String_List_Access;

      function Process_Arg_List (L : String_List) return String_List;
      --  process the list of arguments, replacing tags with actual values

      procedure Wait_For_Prompt (Intermediate : Boolean := False);
      --  Wait for prompt on target

      ----------------------
      -- Process_Arg_List --
      ----------------------

      function Process_Arg_List (L : String_List) return String_List is
         Result : String_List (L'Range);
      begin
         for I in Result'Range loop
            if L (I).all = "%h" then
               Result (I) := new String'(Target_Name);
            elsif L (I).all = "%u" then
               Result (I) := new String'(User_Name);
            elsif L (I).all = "%s" then
               --  Get next args as a single string
               Result (I) := new String'
                 (Argument_List_To_String
                    (Process_Arg_List (L (I + 1 .. L'Last)),
                     Protect_Quotes => False));
               return Result (Result'First .. I);
            else
               Result (I) := new String'(L (I).all);
            end if;
         end loop;
         return Result;
      end Process_Arg_List;

      ---------------------
      -- Wait_For_Prompt --
      ---------------------

      procedure Wait_For_Prompt (Intermediate : Boolean := False) is
      begin
         --  Now wait for prompt
         if not Intermediate then
            Expect (Descriptor,
                    Res,
                    Descriptor.Shell.Prompt.all,
                    Launch_Timeout,
                    False);
         else
            Expect (Descriptor,
                    Res,
                    Descriptor.Shell.Generic_Prompt.all,
                    Launch_Timeout,
                    False);
         end if;
         if Descriptor.Buffer /= null then
            Log ("exp_wait_prompt_after", Descriptor.Buffer.all);
         end if;
         if Res = Expect_Timeout then
            Interrupt (Descriptor);
            Descriptor.Shell := null;
            --  ??? shall free the underlying process descriptor
            --  Free (Descriptor.Underlying_PD);
            Raise_Exception
              (Invalid_Process'Identity,
               "Could not get prompt when connecting to host " &
               Target_Name);
         end if;
      end Wait_For_Prompt;

   begin
      if Shell_Selec = null or Remote_Access_Selec = null then
         Raise_Exception (Invalid_Config_Selector'Identity,
                          "No Config_Selector has been set");
      end if;

      declare
         Shell_Name  : constant String
           := Shell_Selec (Target_Identifier);
         Remote_Name : constant String
           := Remote_Access_Selec (Target_Identifier);
         Shell_Desc  : Shell_Descriptor_Access  := Shell_Descriptor_List;
      begin
         Descriptor.Shell := null;
         while Shell_Desc /= null loop
            if Shell_Desc.Name.all = Shell_Name then
               Descriptor.Shell := Shell_Desc;
               exit;
            end if;
            Shell_Desc := Shell_Desc.Next;
         end loop;
         while Remote_Desc /= null loop
            if Remote_Desc.Name.all = Remote_Name then
               exit;
            end if;
            Remote_Desc := Remote_Desc.Next;
         end loop;
      end;

      if Descriptor.Shell = null then
         Raise_Exception (Invalid_Config_Selector'Identity,
                          "Config_Selector returned an invalid " &
                          "Shell_Descriptor name for " & Target_Name);
      end if;
      if Remote_Desc = null then
         Raise_Exception (Invalid_Config_Selector'Identity,
                          "Config_Selector returned an invalid " &
                          "Remote_Descriptor name for " & Target_Name);
      end if;

      Descriptor.Busy                 := True;
      Descriptor.Current_Echo_Skipped := False;

      --  Construction of the arguments:

      --  1) Set local arguments
      if Local_Args'Length > 0 then
         Old_Args := new Argument_List'(Local_Args);
      end if;

      --  2) Set command
      if Old_Args /= null then
         New_Args := new Argument_List'(Old_Args.all &
                                        Remote_Desc.Start_Cmd);
         Simple_Free (Old_Args);
         Old_Args := New_Args;
      else
         Old_Args := new Argument_List'(1 => Remote_Desc.Start_Cmd);
      end if;

      --  3) Set user argument
      if User_Name /= "" then
         New_Args := new Argument_List'
           (Old_Args.all &
            Process_Arg_List
              (Remote_Desc.Start_Cmd_User_Args.all));
         Simple_Free (Old_Args);
         Old_Args := New_Args;
      end if;

      --  4) Set common arguments and remote command
      New_Args := new Argument_List'
        (Old_Args.all &
         Process_Arg_List
           (Remote_Desc.Start_Cmd_Common_Args.all) &
         Descriptor.Shell.Start_Cmd);
      Simple_Free (Old_Args);
      Old_Args := New_Args;

      --  5) Remove empty arguments
      declare
         Cleaned : Boolean := True;
      begin
         while Cleaned loop
            Cleaned := False;
            Search_Loop :
            for J in Old_Args'Range loop
               if Old_Args (J).all = "" then
                  New_Args := new Argument_List'
                    (Old_Args (Old_Args'First .. J - 1) &
                     Old_Args (J + 1 .. Old_Args'Last));
                  Free (Old_Args (J));
                  Simple_Free (Old_Args);
                  Old_Args := New_Args;
                  Cleaned := True;
                  exit Search_Loop;
               end if;
            end loop Search_Loop;
         end loop;
      end;

      --  Now launch the program remotely accessing the shell
      Log ("spawn",
           Argument_List_To_String (New_Args.all, False));
      Non_Blocking_Spawn
        (Descriptor  => Descriptor,
         Command     => Old_Args (Old_Args'First).all,
         Args        => Old_Args (Old_Args'First + 1 .. Old_Args'Last),
         Buffer_Size => 0,
         Err_To_Out  => Err_To_Out);

      --  ??? wait for login/password prompts

      --  Wait for connection confirmation
      Wait_For_Prompt (True);

      --  Send the initialization commands. These commands are sent
      --  before the synchronization with the prompt because they can
      --  affect it.
      for I in Descriptor.Shell.Init_Cmds'Range loop
         if Descriptor.Buffer /= null then
            Log ("flush_before", Descriptor.Buffer.all);
         end if;
         Flush (Descriptor);
         Send (Descriptor,
               Descriptor.Shell.Init_Cmds (I).all);
         Wait_For_Prompt (True);
      end loop;

      --  Change to working directory
      if Execution_Directory /= "" and then
        Descriptor.Shell.Cd_Cmd /= null
      then
         declare
            Cd_Cmd : String renames Descriptor.Shell.Cd_Cmd.all;
            Idx : constant Natural := Index (Cd_Cmd, "%d");
         begin
            Send (Descriptor,
                  Cd_Cmd (Cd_Cmd'First .. Idx - 1) &
                  Execution_Directory &
                  Cd_Cmd (Idx + 2 .. Cd_Cmd'Last));
            Wait_For_Prompt;
         end;
      end if;

      --  Set filter to intercept the end of the program
      --  This filter also removes echoed lines.
      Add_Filter (Process_Descriptor (Descriptor),
                  Filter_Out'Access,
                  Output,
                  Descriptor'Address);

      Flush (Descriptor);
      --  Now lauch the remote program
      Send (Descriptor,
            Argument_List_To_String (Args, False));
   end Remote_Spawn;

   ----------------
   -- Filter_Out --
   ----------------

   procedure Filter_Out (Descriptor : Process_Descriptor'Class;
                         Str        : String;
                         User_Data  : System.Address := System.Null_Address)
   is
      type Remote_PD_Access is access all Remote_Process_Descriptor;
      function Convert is new Ada.Unchecked_Conversion (System.Address,
                                                        Remote_PD_Access);
      Desc           : constant Remote_PD_Access := Convert (User_Data);
      Size           : Natural;
      Matched        : GNAT.Regpat.Match_Array (0 .. 1);
      Tmp_Buf        : String_Access;
      Current_Filter : Filter_List;
   begin
      Log ("Filter_Out", Str);
      --  The following buffer accesses suppose that the descriptor's buffer
      --  is dynamically allocated (i.e. buffer_size is 0)
      if Desc.Shell.Echoing and then
        not Desc.Current_Echo_Skipped then
         for J in Str'Range loop
            if Str (J) = ASCII.LF then
               Log ("Remove", Str (Str'First .. J));
               Size := Str'Last - J;
               Tmp_Buf := Desc.Buffer;
               Desc.Buffer := new String (1 .. Size);
               Desc.Buffer.all := Str (J + 1 .. Str'Last);
               Desc.Buffer_Index := Size;
               Desc.Current_Echo_Skipped := True;
               if Tmp_Buf /= null then
                  Free (Tmp_Buf);
               end if;
               exit;
            end if;
         end loop;
         if not Desc.Current_Echo_Skipped then
            --  First line not reached. Discard the buffer
            Free (Desc.Buffer);
            Desc.Buffer := new String'("");
            Desc.Buffer_Index := 0;
            return;
         end if;
      end if;
      --  if program not yet terminated, catch a prompt
      if Desc.Last_Index = -1 then
         Match (Desc.Shell.Prompt.all,
                Desc.Buffer (1 .. Desc.Buffer_Index),
                Matched);
         if Matched (0) /= No_Match then
            Tmp_Buf := Desc.Buffer;
            Desc.Buffer := new String'
              (Tmp_Buf (Tmp_Buf'First .. Matched (0).First - 1));
            Desc.Buffer_Index := Matched (0).First - 1;
            Desc.Last_Index := Matched (0).First - 1;
            Free (Tmp_Buf);
            Desc.Busy := False;
            --  try to retrieve the terminated program's status
            if Desc.Shell.Get_Status_Cmd /= null then
               Send (Desc.all, Desc.Shell.Get_Status_Cmd.all);
               Desc.Getting_Status := True;
            end if;
         end if;
      else
         if Desc.Getting_Status then
            Match (Desc.Shell.Get_Status_Ptrn.all, Str, Matched);
            if Matched (0) /= No_Match then
               begin
                  Desc.Status := Integer'Value
                    (Str (Matched (1).First .. Matched (1).Last));
               exception
                  when others =>
                     Desc.Status := 0;
               end;
            end if;
            Desc.Getting_Status := False;
         end if;
         begin
            for I in Desc.Shell.Exit_Cmds'Range loop
               Send (Desc.all,
                     Desc.Shell.Exit_Cmds (I).all);
               delay 0.1;
            end loop;
         exception
            when others =>
               null;
         end;
         Flush (Desc.all);
      end if;

      --  Call filters with modified buffer
      if Desc.R_Filters_Lock = 0 then
         Current_Filter := Desc.R_Filters;

         while Current_Filter /= null loop
            if Current_Filter.Filter_On = Output then
               Current_Filter.Filter
                 (Descriptor, Desc.Buffer.all, Current_Filter.User_Data);
            end if;

            Current_Filter := Current_Filter.Next;
         end loop;
      end if;
   end Filter_Out;

   -----------
   -- Close --
   -----------

   procedure Close
     (Descriptor : in out Remote_Process_Descriptor;
      Status     : out Integer)
   is
      TTY_Descriptor : TTY_Process_Descriptor
        renames TTY_Process_Descriptor (Descriptor);
   begin
      if not Descriptor.Busy then
         for I in Descriptor.Shell.Exit_Cmds'Range loop
            begin
               Flush (Descriptor);
               Send (Descriptor,
                     Descriptor.Shell.Exit_Cmds (I).all);
               delay 0.1;
            exception
               when others =>
                  exit;
            end;
         end loop;
      else
         Interrupt (TTY_Descriptor);
      end if;
      Close (TTY_Descriptor);
      Status := Descriptor.Status;
   end Close;

   -----------
   -- Close --
   -----------

   procedure Close (Descriptor : in out Remote_Process_Descriptor)
   is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Descriptor : in out Remote_Process_Descriptor) is
      pragma Unreferenced (Descriptor);
   begin
      null;
   end Interrupt;

   -------------------------
   -- Set_Config_Selector --
   -------------------------

   procedure Set_Config_Selector
     (Remote_Access_Selector : Config_Selector_Function;
      Shell_Selector         : Config_Selector_Function) is
   begin
      Shell_Selec         := Shell_Selector;
      Remote_Access_Selec := Remote_Access_Selector;
   end Set_Config_Selector;

   ----------------------------------
   -- Add_Remote_Access_Descriptor --
   ----------------------------------

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      User_Prompt_Ptrn          : String;
      Password_Prompt_Ptrn      : String)
   is
      Remote : constant Remote_Descriptor_Access := new Remote_Descriptor;
   begin
      Remote.all :=
        (Name => new String'(Name),
         Start_Cmd => new String'(Start_Command),
         Start_Cmd_Common_Args => new String_List'(Start_Command_Common_Args),
         Start_Cmd_User_Args   => new String_List'(Start_Command_User_Args),
         User_Prompt_Ptrn      => new Pattern_Matcher'(Compile (
           User_Prompt_Ptrn,
           Single_Line + Multiple_Lines)),
         Password_Prompt_Ptrn  => new Pattern_Matcher'(Compile (
           Password_Prompt_Ptrn,
           Single_Line + Multiple_Lines)),
         Next => Remote_Descriptor_List);
      Remote_Descriptor_List := Remote;
   end Add_Remote_Access_Descriptor;

   --------------------------
   -- Add_Shell_Descriptor --
   --------------------------

   procedure Add_Shell_Descriptor
     (Name                : String;
      Start_Command       : String             := "";
      Generic_Prompt      : String             := "";
      Configured_Prompt   : String             := "";
      Init_Commands       : String_List        := Null_String_List;
      Exit_Commands       : String_List        := Null_String_List;
      Cd_Command          : String             := "";
      Get_Status_Command  : String             := "";
      Get_Status_Ptrn     : String             := "";
      Echoing             : Boolean            := False;
      Wrapper             : Cmd_Wrapper_Function := null;
      Output_Processor    : Output_Processor_Procedure := null;
      Interrupt_Command   : String             := "")
   is
      Shell : constant Shell_Descriptor_Access := new Shell_Descriptor;
   begin
      Shell.all :=
        (Name             => new String'(Name),
         Start_Cmd        => new String'(Start_Command),
         Init_Cmds        => new String_List'(Init_Commands),
         Exit_Cmds        => new String_List'(Exit_Commands),
         Cd_Cmd           => new String'(Cd_Command),
         Get_Status_Cmd   => new String'(Get_Status_Command),
         Get_Status_Ptrn  => new Pattern_Matcher'
           (Compile (Get_Status_Ptrn, Single_Line + Multiple_Lines)),
         Generic_Prompt   => new Pattern_Matcher'
           (Compile (Generic_Prompt, Single_Line + Multiple_Lines)),
         Prompt           => new Pattern_Matcher'
           (Compile (Configured_Prompt, Single_Line + Multiple_Lines)),
         Echoing          => Echoing,
         Wrapper          => Wrapper,
         Output_Processor => Output_Processor,
         Int_Cmd          => new String'(Interrupt_Command),
         Next             => Shell_Descriptor_List);
      Shell_Descriptor_List := Shell;
   end Add_Shell_Descriptor;

end GNAT.Expect.TTY.Remote;

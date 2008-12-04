------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2008, AdaCore                     --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;

with Password_Manager;     use Password_Manager;
with String_Utils;         use String_Utils;
with Traces;               use Traces;
with User_Interface_Tools; use User_Interface_Tools;

with Remote_Descriptors;   use Remote_Descriptors;
with Connection_Debuggers; use Connection_Debuggers;

package body GNAT.Expect.TTY.Remote is

   Me : constant Debug_Handle := Create ("GNAT.Expect.TTY.Remote");

   Finalized : Boolean := False;

   type Compiled_Regexp_Array_Access is access Compiled_Regexp_Array;

   Test_Echo_Cmd : constant String := "echo foo";
   Echoing_Regexps : constant Compiled_Regexp_Array
     := (1 => new Pattern_Matcher'
           (Compile ("^echo foo", Multiple_Lines or Single_Line)),
         2 => new Pattern_Matcher'
           (Compile ("^foo", Multiple_Lines or Single_Line)));

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Internal_Sync_Execute
     (Host                : String;
      Args                : GNAT.OS_Lib.Argument_List;
      Execution_Directory : String;
      Get_Output          : Boolean;
      Out_Value           : out GNAT.Strings.String_Access;
      Status              : out Integer;
      Success             : out Boolean);
   --  Execute the command synchronously.

   procedure Filter_Out
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Used to filter the shell output (removes shell commands)

   function Get_Machine_Descriptor
     (Nickname : String) return Remote_Machine_Descriptor_Access;
   --  Get machine descriptor from nickname
   --  Raise Invalid_Nickname if the machine was not found.

   procedure Internal_Handle_Exceptions
     (Desc : in out Remote_Process_Descriptor);
   --  Handle exceptions raised by gnat.expect or gnat.expect.tty

   procedure Get_Or_Init_Session
     (Descriptor        : in out Remote_Process_Descriptor;
      Err_To_Out        : Boolean := True;
      On_New_Connection : access procedure (Target_Name : String) := null);
   --  Retrieve a READY session, or initialize a new one.
   --  Assign it to descriptor upon success
   --  Raises No_Session_Available is all sessions are BUSY

   procedure Log (Where : String; What : String);
   --  Log a debug comment

   ---------
   -- Log --
   ---------

   procedure Log (Where : String; What : String) is

      function Clean_Up (Str : String) return String;
      --  On VMS, traces might begin with NUL character. Remove it for display

      function Clean_Up (Str : String) return String is
         Out_S : String := Str;
      begin
         for J in Str'Range loop
            if Str (J) = ASCII.NUL then
               Out_S (J) := '@';
            end if;
         end loop;

         return Out_S;
      end Clean_Up;

   begin
      if Active (Me) then
         Trace (Me, "(" & Where & "): '" & Clean_Up (What) & "'");
      end if;
   end Log;

   ----------------
   -- Add_Filter --
   ----------------

   overriding procedure Add_Filter
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

   overriding procedure Remove_Filter
     (Descriptor : in out Remote_Process_Descriptor;
      Filter     : Filter_Function)
   is
      Previous : Filter_List := null;
      Current  : Filter_List := Descriptor.R_Filters;
   begin
      Remove_Filter (TTY_Process_Descriptor (Descriptor), Filter);

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

   overriding procedure Lock_Filters
     (Descriptor : in out Remote_Process_Descriptor) is
   begin
      Descriptor.R_Filters_Lock := Descriptor.R_Filters_Lock + 1;
   end Lock_Filters;

   --------------------
   -- Unlock_Filters --
   --------------------

   overriding procedure Unlock_Filters
     (Descriptor : in out Remote_Process_Descriptor) is
   begin
      if Descriptor.R_Filters_Lock > 0 then
         Descriptor.R_Filters_Lock := Descriptor.R_Filters_Lock - 1;
      end if;
   end Unlock_Filters;

   --------------------------------
   -- Internal_Handle_Exceptions --
   --------------------------------

   procedure Internal_Handle_Exceptions
     (Desc : in out Remote_Process_Descriptor) is
   begin
      --  Exception comming from the shell, not from the remote process
      --  If Session_Died is set, the session exception has already been
      --  treated.

      if not Desc.Terminated and not Desc.Session_Died then
         if Desc.Machine /= null
           and then Desc.Session_Nb in Desc.Machine.Sessions'Range
           and then Desc.Machine.Sessions (Desc.Session_Nb).State /= OFF
         then
            Desc.Machine.Sessions (Desc.Session_Nb).State := OFF;
            Close (Desc.Machine.Sessions (Desc.Session_Nb).Pd);
         end if;

         Desc.Terminated   := True;
         Desc.Session_Died := True;
      end if;

   exception
      when others =>
         --  Pd already closed. Do nothing
         null;
   end Internal_Handle_Exceptions;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Descriptor   : in out Remote_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      TTY_Descriptor : TTY_Process_Descriptor
      renames TTY_Process_Descriptor (Descriptor);
   begin
      if Str /= "" then
         if Active (Me) then
            Log ("SND", Str);
         end if;

         if Descriptor.Machine.Desc.Dbg /= null then
            Print (Descriptor.Machine.Desc.Dbg,
                   Str,
                   Input);
         end if;

         --  Do not skip next line if a remote process is running. In fact,
         --  this process will receive the commands, not the shell, and will
         --  not echo them.
         if Descriptor.Terminated then
            Descriptor.Current_Echo_Skipped := False;
         end if;
      end if;

      if Descriptor.Use_Cr_Lf = CRLF and then Add_LF then
         Send (TTY_Descriptor, Str & ASCII.CR, Add_LF, Empty_Buffer);
      else
         Send (TTY_Descriptor, Str, Add_LF, Empty_Buffer);
      end if;
   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Send;

   -------------------------
   -- Get_Or_Init_Session --
   -------------------------

   procedure Get_Or_Init_Session
     (Descriptor        : in out Remote_Process_Descriptor;
      Err_To_Out        : Boolean := True;
      On_New_Connection : access procedure (Target_Name : String) := null)
   is
      Session_Nb   : Natural := 0;
      Remote_Desc  : Remote_Descriptor_Access;
      New_Args     : String_List_Access;
      Old_Args     : String_List_Access;
      Regexp_Array : Compiled_Regexp_Array (1 .. 3);
      Verify_Cr_Lf : Boolean := False;
      First_Call   : Boolean := True;

      function Process_Arg_List (L : String_List) return String_List;
      --  process the list of arguments, replacing tags with actual values

      procedure Wait_For_Prompt
        (Intermediate : Boolean := False);
      --  Wait for prompt on target

      procedure My_Send
        (Descriptor    : in out Process_Descriptor'Class;
         Dbg           : Connection_Debugger;
         Str           : String;
         Use_Cr_Lf     : Boolean;
         Add_LF        : Boolean := True;
         Empty_Buffer  : Boolean := False;
         Password_Mode : Boolean := False);
      --  Special send procedure that handle sending of CR characters

      ----------------------
      -- Process_Arg_List --
      ----------------------

      function Process_Arg_List (L : String_List) return String_List is
         Result : String_List (L'Range);
      begin
         for J in Result'Range loop
            if L (J).all = "%h" then
               Result (J) :=
                 new String'(Descriptor.Machine.Desc.Network_Name.all);
            elsif L (J).all = "%u" then
               Result (J) :=
                 new String'(Descriptor.Machine.Desc.User_Name.all);
            elsif L (J).all = "%s" then
               --  Get next args as a single string
               Result (J) := new String'
                 (Argument_List_To_String
                    (Process_Arg_List (L (J + 1 .. L'Last)),
                     Protect_Quotes => False));
               return Result (Result'First .. J);

            else
               Result (J) := new String'(L (J).all);
            end if;
         end loop;

         return Result;
      end Process_Arg_List;

      ---------------------
      -- Wait_For_Prompt --
      ---------------------

      procedure Wait_For_Prompt
        (Intermediate : Boolean := False)
      is
         Regexp_Array       : Compiled_Regexp_Array (1 .. 4);
         Matched            : GNAT.Regpat.Match_Array (0 .. 1);
         Extra_Regexp_Array : Compiled_Regexp_Array
           (Remote_Desc.Extra_Prompt_Array'Range);
         Res                : Expect_Match;
         Res_Extra          : Natural;
         NL_Regexp          : constant Pattern_Matcher :=
                                Compile ("^[^\n]*\n", Single_Line);
         Force_Password_Ask : Boolean;

      begin
         --  Machine is echoing commands. Skip them.
         if Descriptor.Machine.Echoing then
            Expect (Descriptor.Machine.Sessions (Session_Nb).Pd, Res,
                    NL_Regexp, Descriptor.Machine.Desc.Timeout, False);
         end if;

         --  Now wait for prompt

         if not Intermediate then
            Regexp_Array (1) := Descriptor.Shell.Prompt;
         else
            Regexp_Array (1) := Descriptor.Shell.Generic_Prompt;
         end if;

         Regexp_Array (2) := Remote_Desc.User_Prompt_Ptrn;
         Regexp_Array (3) := Remote_Desc.Password_Prompt_Ptrn;
         Regexp_Array (4) := Remote_Desc.Passphrase_Prompt_Ptrn;

         for J in Remote_Desc.Extra_Prompt_Array'Range loop
            Extra_Regexp_Array (J) :=
              Remote_Desc.Extra_Prompt_Array (J).Ptrn;
         end loop;

         Expect
           (Descriptor.Machine.Sessions (Session_Nb).Pd,
            Res,
            Regexp_Array & Extra_Regexp_Array,
            Matched,
            Descriptor.Machine.Desc.Timeout,
            False);

         if Descriptor.Use_Cr_Lf = Auto then
            declare
               Out_Str : constant String :=
                           Expect_Out
                             (Descriptor.Machine.Sessions (Session_Nb).Pd);
            begin
               if Out_Str'Length > 0
                 and then Index (Out_Str, "" & ASCII.LF) >= Out_Str'First
                 and then Index (Out_Str, "" & ASCII.CR) < Out_Str'First
               then
                  Trace (Me, "Remote uses LF");
                  Descriptor.Use_Cr_Lf := LF;
               else
                  Trace (Me, "Remote uses CR/LF");
                  Descriptor.Use_Cr_Lf := CRLF;
                  --  Indicate that we need to verify the accuracy of this
                  --  assumption
                  Verify_Cr_Lf := True;
               end if;
            end;
         end if;

         if Active (Me) then
            Log
              ("RCV",
               Expect_Out (Descriptor.Machine.Sessions (Session_Nb).Pd));
         end if;

         if Descriptor.Machine.Desc.Dbg /= null then
            Print (Descriptor.Machine.Desc.Dbg,
                   Expect_Out (Descriptor.Machine.Sessions (Session_Nb).Pd),
                   Output);
         end if;

         case Res is
            when Expect_Timeout =>
               Trace (Me, "got timeout in Wait_For_Prompt (intermediate=" &
                      Boolean'Image (Intermediate) & ")");

               if Descriptor.Machine.Desc.Dbg /= null
                 and then Descriptor.Machine.Sessions (Session_Nb).Pd.Buffer /=
                 null

               then
                  Print
                    (Descriptor.Machine.Desc.Dbg,
                     "<Timeout> Cannot get a valid prompt. Received so far:",
                     Input);
                  Print
                    (Descriptor.Machine.Desc.Dbg,
                     Descriptor.Machine.Sessions (Session_Nb).Pd.Buffer.all,
                     Output);
               end if;

               Close (Descriptor.Machine.Sessions (Session_Nb).Pd);

               raise Invalid_Process with
                 "Could not get prompt when connecting to host " &
                 Descriptor.Machine.Desc.Nickname.all;

            when 1 =>
               --  Received shell prompt
               Trace
                 (Me, "got prompt in Wait_For_Prompt");

               --  We need to verify if the remote host really needs CR/LF
               --  being sent:
               --  in some cases this makes the remote machine receive
               --  the equivalent of two LFs (thus two prompts) which
               --  unsynchronizes the expect interface.

               --  This happens whith telnet on AIX, where the AIX server
               --  returns CR/LF, but expects LF as input

               --  We verify this by looking for duplicated prompt after the
               --  first command is sent (so upon the second call to
               --  Wait_For_Prompt)
               if Verify_Cr_Lf and then First_Call then
                  First_Call := False;
               elsif Verify_Cr_Lf then
                  Verify_Cr_Lf := False;

                  if Descriptor.Use_Cr_Lf = CRLF then
                     declare
                        Str : constant String :=
                                Strip_CR
                                  (Expect_Out
                                     (Descriptor.Machine.Sessions
                                        (Session_Nb).Pd));
                        Ref : Integer := -1;

                     begin
                        for J in reverse Str'Range loop
                           if Str (J) = ASCII.LF then
                              Ref := J + 1;
                              exit;
                           end if;
                        end loop;

                        declare
                           Ref_Prompt : constant String :=
                                          Str (Ref .. Str'Last);
                        begin
                           for J in reverse Str'First .. Ref - 1 loop
                              if Str (J) = ASCII.LF
                                and then J >= Str'First + Ref_Prompt'Length
                                and then Str (J - Ref_Prompt'Length .. J - 1) =
                                  Ref_Prompt
                                and then
                                  (J - Ref_Prompt'Length = Str'First
                                   or else
                                     Str
                                       (J - Ref_Prompt'Length - 1) = ASCII.LF)
                              then
                                 --  Found a duplicated prompt
                                 Trace
                                   (Me,
                                    "Deactivating CR/LF as this leads to" &
                                    " duplicated prompts");
                                 Descriptor.Use_Cr_Lf := LF;
                                 exit;
                              end if;
                           end loop;
                        end;
                     end;
                  end if;
               end if;

            when 2 =>
               Trace (Me, "got user name prompt in Wait_For_Prompt");

               --  Received user name prompt
               if Descriptor.Machine.Desc.User_Name.all = "" then
                  Free (Descriptor.Machine.Desc.User_Name);
                  Descriptor.Machine.Desc.User_Name := new String'
                    (Query_User
                       (Expect_Out
                          (Descriptor.Machine.Sessions (Session_Nb).Pd),
                        Password_Mode => False));

                  if Descriptor.Machine.Desc.User_Name.all = "" then
                     Close (Descriptor.Machine.Sessions (Session_Nb).Pd);

                     raise Invalid_Process with "Connection canceled by user";
                  end if;
               end if;

               My_Send
                 (Descriptor.Machine.Sessions (Session_Nb).Pd,
                  Descriptor.Machine.Desc.Dbg,
                  Descriptor.Machine.Desc.User_Name.all,
                  Descriptor.Use_Cr_Lf = CRLF);
               Wait_For_Prompt (Intermediate);

            when 3 | 4 =>
               Trace (Me, "got password prompt in Wait_For_Prompt");

               --  If this password was already tried, let's forget it.
               if Descriptor.Nb_Password_Prompt > 0 then
                  Force_Password_Ask := True;
               else
                  Force_Password_Ask := False;
               end if;

               --  Received password prompt
               Descriptor.Nb_Password_Prompt
                 := Descriptor.Nb_Password_Prompt + 1;

               if Descriptor.Nb_Password_Prompt >
                 Remote_Desc.Max_Password_Prompt
               then
                  Close (Descriptor.Machine.Sessions (Session_Nb).Pd);

                  raise Invalid_Process with "Invalid password for connection";
               end if;

               declare
                  Password : String_Access;
               begin
                  if Res = 3 then
                     --  Password
                     Password := new String'
                       (Get_Password
                          (Descriptor.Machine.Desc.Network_Name.all,
                           Descriptor.Machine.Desc.User_Name.all,
                           Force_Password_Ask));
                  else
                     --  Passphrase
                     Password := new String'
                       (Get_Passphrase
                          (Expect_Out
                             (Descriptor.Machine.Sessions (Session_Nb).Pd)
                               (Matched (1).First .. Matched (1).Last),
                           Force_Password_Ask));
                  end if;

                  if Password.all = "" then
                     Free (Password);
                     Close (Descriptor.Machine.Sessions (Session_Nb).Pd);
                     Descriptor.Machine.Sessions (Session_Nb).State := OFF;

                     raise Invalid_Process with "Connection canceled by user";
                  end if;

                  My_Send (Descriptor.Machine.Sessions (Session_Nb).Pd,
                           Descriptor.Machine.Desc.Dbg,
                           Password.all,
                           Descriptor.Use_Cr_Lf = CRLF,
                           Password_Mode => True);
                  Free (Password);
               end;

               Wait_For_Prompt (Intermediate);

            when others =>
               --  Extra regexp array match
               if Res > 4 then
                  Trace (Me, "got extra regexp prompt in Wait_For_Prompt");
                  Res_Extra := Natural (Res - 4);
               else
                  Trace (Me, "got disconnected in Wait_For_Prompt");
                  Descriptor.Session_Died := True;
                  Close (Descriptor.Machine.Sessions (Session_Nb).Pd);

                  raise Invalid_Process with
                    "Unexpected error when connecting to " &
                  Descriptor.Machine.Desc.Nickname.all;
               end if;

               if Remote_Desc.Extra_Prompt_Array (Res_Extra).Auto_Answer then
                  My_Send
                    (Descriptor.Machine.Sessions (Session_Nb).Pd,
                     Descriptor.Machine.Desc.Dbg,
                     Remote_Desc.Extra_Prompt_Array (Res_Extra).Answer.all,
                     Descriptor.Use_Cr_Lf = CRLF);
               else
                  declare
                     Str : constant String := Query_User
                       (Remote_Desc.Extra_Prompt_Array
                          (Res_Extra).Question.all,
                        False);
                  begin
                     if Str /= "" then
                        My_Send
                          (Descriptor.Machine.Sessions (Session_Nb).Pd,
                           Descriptor.Machine.Desc.Dbg,
                           Str,
                           Descriptor.Use_Cr_Lf = CRLF);
                     else
                        Close (Descriptor.Machine.Sessions (Session_Nb).Pd);
                        Descriptor.Machine.Sessions (Session_Nb).State := OFF;

                        raise Invalid_Process with "Connection canceled.";
                     end if;
                  end;
               end if;

               Wait_For_Prompt (Intermediate);
         end case;
      end Wait_For_Prompt;

      -------------
      -- My_Send --
      -------------

      procedure My_Send
        (Descriptor    : in out Process_Descriptor'Class;
         Dbg           : Connection_Debugger;
         Str           : String;
         Use_Cr_Lf     : Boolean;
         Add_LF        : Boolean := True;
         Empty_Buffer  : Boolean := False;
         Password_Mode : Boolean := False) is
      begin
         if Dbg /= null then
            if Password_Mode then
               Print (Dbg, "******", Input);
               if Active (Me) then
                  Log ("SND", "<Sending password>");
               end if;
            else
               Print (Dbg, Str, Input);
               if Active (Me) then
                  Log ("SND", Str);
               end if;
            end if;
         end if;

         if Use_Cr_Lf and then Add_LF then
            Send (Descriptor, Str & ASCII.CR & ASCII.LF, False, Empty_Buffer);
         else
            Send (Descriptor, Str, Add_LF, Empty_Buffer);
         end if;
      end My_Send;

      Res          : Expect_Match;
      Found_U      : Boolean;

   begin
      --  Search for READY or OFF sessions

      for J in Descriptor.Machine.Sessions'Range loop
         if Descriptor.Machine.Sessions (J).State = OFF and Session_Nb = 0 then
            --  At least one possibility is to launch a new session
            Session_Nb := J;
         elsif Descriptor.Machine.Sessions (J).State = READY then
            --  We have a READY session. Let's stop the search here
            Session_Nb := J;
            exit;
         end if;
      end loop;

      --  No free session available...

      if Session_Nb = 0 then
         raise No_Session_Available;
      end if;

      Descriptor.Session_Nb := Session_Nb;

      Remote_Desc := Get_Descriptor_From_Name
        (Descriptor.Machine.Desc.Access_Name.all);

      if Remote_Desc = null then
         raise Invalid_Nickname with
           "Invalid remote access tool name for " &
         Descriptor.Machine.Desc.Nickname.all &
         ": " & Descriptor.Machine.Desc.Access_Name.all;
      end if;

      if Descriptor.Machine.Sessions (Session_Nb).State = OFF then
         --  Launch a new session

         --  Construction of the arguments:

         --  Set command

         if Old_Args /= null then
            New_Args := new GNAT.OS_Lib.Argument_List'(Old_Args.all &
                                                       Remote_Desc.Start_Cmd);
            Simple_Free (Old_Args);
            Old_Args := New_Args;
         else
            Old_Args := new GNAT.OS_Lib.Argument_List'
              (1 => Remote_Desc.Start_Cmd);
         end if;

         --  Does the common arguments allow user input ?

         Found_U := False;

         for J in Remote_Desc.Start_Cmd_Common_Args'Range loop
            if Remote_Desc.Start_Cmd_Common_Args (J).all = "%u" then
               Found_U := True;
               exit;
            end if;
         end loop;

         --  Set common arguments

         New_Args := new GNAT.OS_Lib.Argument_List'
           (Old_Args.all &
            Process_Arg_List
              (Remote_Desc.Start_Cmd_Common_Args.all));
         Simple_Free (Old_Args);
         Old_Args := New_Args;

         --  Set user argument

         for J in Old_Args'Range loop
            if Old_Args (J).all = "%U" then
               Found_U := True;

               if Descriptor.Machine.Desc.User_Name.all /= "" then
                  --  Replace %U with user arguments.
                  New_Args := new GNAT.OS_Lib.Argument_List'
                    (Old_Args (Old_Args'First .. J - 1) &
                     Process_Arg_List
                       (Remote_Desc.Start_Cmd_User_Args.all) &
                     Old_Args (J + 1 .. Old_Args'Last));
               else
                  --  Remove %U: no user specified
                  New_Args := new GNAT.OS_Lib.Argument_List'
                    (Old_Args (Old_Args'First .. J - 1) &
                     Old_Args (J + 1 .. Old_Args'Last));
               end if;

               Free (Old_Args (J));
               Simple_Free (Old_Args);
               Old_Args := New_Args;

               exit;
            end if;
         end loop;

         if not Found_U
           and then Descriptor.Machine.Desc.User_Name.all /= ""
         then
            --  Compatibility: if %U was not found, then add user arguments at
            --  the begining of the args list.
            New_Args := new GNAT.OS_Lib.Argument_List'
              (Process_Arg_List
                 (Remote_Desc.Start_Cmd_User_Args.all) &
               Old_Args.all);

            Simple_Free (Old_Args);
            Old_Args := New_Args;
         end if;

         --  Set Command argument, if supported

         for J in Old_Args'Range loop
            if Old_Args (J).all = "%C" then
               New_Args := new GNAT.OS_Lib.Argument_List'
                 (Old_Args (Old_Args'First .. J - 1) &
                  new String'(Descriptor.Shell.Start_Cmd.all) &
                  Old_Args (J + 1 .. Old_Args'Last));

               Free (Old_Args (J));
               Simple_Free (Old_Args);
               Old_Args := New_Args;

               exit;
            end if;
         end loop;

         --  Remove empty arguments

         declare
            Cleaned : Boolean := True;
         begin
            while Cleaned loop
               Cleaned := False;

               Search_Loop :
               for J in Old_Args'Range loop
                  if Old_Args (J).all = "" then
                     New_Args := new GNAT.OS_Lib.Argument_List'
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
         --  Do not use pipes, as they prevent password retrieval on windows
         Set_Use_Pipes (Descriptor.Machine.Sessions (Session_Nb).Pd,
                        Remote_Desc.Use_Pipes);
         Non_Blocking_Spawn
           (Descriptor  => Descriptor.Machine.Sessions (Session_Nb).Pd,
            Command     => New_Args (New_Args'First).all,
            Args        => New_Args (New_Args'First + 1 .. New_Args'Last),
            Buffer_Size => 0,
            Err_To_Out  => Err_To_Out);

         --  Wait for connection confirmation

         Descriptor.Use_Cr_Lf := Descriptor.Machine.Desc.Cr_Lf;
         --  First call of Wait_For_Prompt will resolve the case where
         --  Use_Cr_Lf is "Auto"
         Wait_For_Prompt (True);

         --  Determine if the machine echoes commands
         if Descriptor.Shell.No_Echo_Cmd.all /= "" then
            My_Send
              (Descriptor.Machine.Sessions (Session_Nb).Pd,
               Descriptor.Machine.Desc.Dbg,
               Descriptor.Shell.No_Echo_Cmd.all,
               Descriptor.Use_Cr_Lf = CRLF);
            Wait_For_Prompt (True);
         end if;

         if Descriptor.Machine.Determine_Echoing then
            My_Send
              (Descriptor.Machine.Sessions (Session_Nb).Pd,
               Descriptor.Machine.Desc.Dbg,
               Test_Echo_Cmd,
               Descriptor.Use_Cr_Lf = CRLF);
            Expect (Descriptor.Machine.Sessions (Session_Nb).Pd, Res,
                    Echoing_Regexps,
                    Descriptor.Machine.Desc.Timeout, False);

            if Descriptor.Machine.Desc.Dbg /= null then
               Print (Descriptor.Machine.Desc.Dbg,
                      Expect_Out (Descriptor.Machine.Sessions (Session_Nb).Pd),
                      Output);
            end if;

            if Res = 1 then
               if Descriptor.Machine.Desc.Dbg /= null then
                  Print (Descriptor.Machine.Desc.Dbg,
                         " ... <remote echoes commands> ...",
                         Output);
               end if;

               Log ("Init_Session", "remote echoes cmds");
               Descriptor.Machine.Echoing := True;
            elsif Res = 2 then
               if Descriptor.Machine.Desc.Dbg /= null then
                  Print (Descriptor.Machine.Desc.Dbg,
                         " ... <remote does not echo commands> ...",
                         Output);
               end if;

               Log ("Init_Session", "remote does not echo commands");
               Descriptor.Machine.Echoing := False;
            else
               Log ("Init_Session", "unexpected Res when testing echo");
               Descriptor.Machine.Echoing := False;
            end if;

            Descriptor.Machine.Determine_Echoing := False;
            Wait_For_Prompt (True);
         end if;

         --  Send the initialization commands. These commands are sent
         --  before the synchronization with the prompt because they can
         --  affect it.

         for J in Descriptor.Shell.Init_Cmds'Range loop
            Flush (Descriptor.Machine.Sessions (Session_Nb).Pd);
            My_Send
              (Descriptor.Machine.Sessions (Session_Nb).Pd,
               Descriptor.Machine.Desc.Dbg,
               Descriptor.Shell.Init_Cmds (J).all,
               Descriptor.Use_Cr_Lf = CRLF);
            Wait_For_Prompt (False);
         end loop;

         if Descriptor.Machine.Desc.Extra_Init_Commands /= null then
            for J in Descriptor.Machine.Desc.Extra_Init_Commands'Range loop
               Flush (Descriptor.Machine.Sessions (Session_Nb).Pd);

               My_Send
                 (Descriptor.Machine.Sessions (Session_Nb).Pd,
                  Descriptor.Machine.Desc.Dbg,
                  Descriptor.Machine.Desc.Extra_Init_Commands (J).all,
                  Descriptor.Use_Cr_Lf = CRLF);
               Wait_For_Prompt;
            end loop;
         end if;

         Descriptor.Machine.Sessions (Session_Nb).State := READY;
         Descriptor.Machine.Sessions (Session_Nb).Cr_Lf :=
           Descriptor.Use_Cr_Lf;

         if On_New_Connection /= null then
            On_New_Connection (Descriptor.Machine.Desc.Nickname.all);
         end if;
      end if;

      Descriptor.Input_Fd   :=
        Descriptor.Machine.Sessions (Session_Nb).Pd.Input_Fd;
      Descriptor.Output_Fd  :=
        Descriptor.Machine.Sessions (Session_Nb).Pd.Output_Fd;
      Descriptor.Error_Fd   :=
        Descriptor.Machine.Sessions (Session_Nb).Pd.Error_Fd;
      Descriptor.Pid        :=
        Descriptor.Machine.Sessions (Session_Nb).Pd.Pid;
      Descriptor.Process    :=
        Descriptor.Machine.Sessions (Session_Nb).Pd.Process;
      Descriptor.Use_Cr_Lf  :=
        Descriptor.Machine.Sessions (Session_Nb).Cr_Lf;

      --  Set Terminated state as it is not started yet !
      Descriptor.Terminated := True;
      Descriptor.Busy       := False;
      Descriptor.Machine.Sessions (Session_Nb).State := BUSY;
   end Get_Or_Init_Session;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   procedure Remote_Spawn
     (Descriptor          : out Process_Descriptor_Access;
      Target_Nickname     : String;
      Args                : GNAT.OS_Lib.Argument_List;
      Execution_Directory : String := "";
      Err_To_Out          : Boolean := False;
      On_New_Connection   : access procedure (Target_Name : String) := null)
   is
      Res          : Expect_Match;
      The_Args     : GNAT.OS_Lib.Argument_List := Clone (Args);
   begin
      Descriptor := new Remote_Process_Descriptor;

      declare
         Desc : Remote_Process_Descriptor
                  renames Remote_Process_Descriptor (Descriptor.all);
      begin
         Desc.Machine := Get_Machine_Descriptor (Target_Nickname);

         Desc.Shell := Get_Descriptor_From_Name
           (Desc.Machine.Desc.Shell_Name.all);

         if Desc.Shell = null then
            raise Invalid_Nickname with
              Desc.Machine.Desc.Nickname.all & "error: " &
              "shell " & Desc.Machine.Desc.Shell_Name.all &
              " is unknown.";
         end if;

         Desc.Session_Died         := False;

         Get_Or_Init_Session
           (Desc, True, On_New_Connection);

         Desc.Terminated :=  False;
         Desc.Busy := True;

         --  Change to working directory

         if Execution_Directory /= "" and then
           Desc.Shell.Cd_Cmd /= null
         then
            declare
               Cd_Cmd : String renames Desc.Shell.Cd_Cmd.all;
               Idx : constant Natural := Index (Cd_Cmd, "%d");
            begin
               Send (Descriptor.all,
                     Cd_Cmd (Cd_Cmd'First .. Idx - 1) & '"' &
                     Execution_Directory & '"' &
                     Cd_Cmd (Idx + 2 .. Cd_Cmd'Last));
               Expect (Descriptor.all,
                       Res,
                       Desc.Shell.Prompt.all,
                       Desc.Machine.Desc.Timeout,
                       False);

               if Active (Me) then
                  Log ("RCV", Expect_Out (Descriptor.all));
               end if;

               if Desc.Machine.Desc.Dbg /= null then
                  Print (Desc.Machine.Desc.Dbg,
                         Expect_Out (Descriptor.all),
                         Output);
               end if;
            end;
         end if;

         --  First protect spaces in arguments

         for J in The_Args'Range loop
            if The_Args (J)'Length > 0
              and then The_Args (J) (The_Args (J)'First) /= '"'
            then
               Space_Loop :
               for K in The_Args (J)'Range loop
                  if The_Args (J) (K) = ' ' then
                     Free (The_Args (J));
                     The_Args (J) := new String'('"' & Args (J).all & '"');
                     exit Space_Loop;
                  end if;
               end loop Space_Loop;
            end if;
         end loop;

         --  Set filter to intercept the end of the program
         --  This filter also removes echoed lines.
         --  Force call to parent's Add_Filter.

         Add_Filter (Process_Descriptor (Descriptor.all),
                     Filter_Out'Access,
                     Output,
                     Descriptor.all'Address);

         Flush (Desc);

         --  Now lauch the remote program
         Send (Desc, Argument_List_To_String (The_Args, False));

      exception
         when Process_Died =>
            --  Underlying session has died
            for J in The_Args'Range loop
               Free (The_Args (J));
            end loop;

            Internal_Handle_Exceptions (Desc);

            raise Process_Died with
               "Disconnected from host " & Target_Nickname &
               ". Please verify your network connections and retry.";
      end;
   end Remote_Spawn;

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Out_Value             : out GNAT.Strings.String_Access;
      Status                : out Boolean;
      Execution_Directory   : String  := "")
   is
      Status_Nb : Integer;
   begin
      Internal_Sync_Execute
        (Host, Args, Execution_Directory, True, Out_Value, Status_Nb, Status);
   end Sync_Execute;

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Status                : out Boolean;
      Execution_Directory   : String  := "")
   is
      Out_Value : GNAT.Strings.String_Access;
      Status_Nb : Integer;
   begin
      Internal_Sync_Execute
        (Host, Args, Execution_Directory, False, Out_Value, Status_Nb, Status);
   end Sync_Execute;

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Internal_Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Execution_Directory   : String;
      Get_Output            : Boolean;
      Out_Value             : out GNAT.Strings.String_Access;
      Status                : out Integer;
      Success               : out Boolean)
   is
      Fd     : Process_Descriptor_Access;
      Result : Expect_Match;
      Regexp : constant Pattern_Matcher :=
                 Compile ("^[^\n]*\n", Single_Line or Multiple_Lines);

   begin
      Remote_Spawn
        (Fd, Host, Args, Execution_Directory, Err_To_Out => False);

      loop
         Expect (Fd.all, Result, Regexp, Timeout => 5);

         if Result /= Expect_Timeout and then Get_Output then
            declare
               Output : constant String := Strip_CR (Expect_Out (Fd.all));
               Tmp    : String_Access;
            begin
               if Out_Value /= null then
                  Tmp := new String'(Out_Value.all & Output);
                  Free (Out_Value);
                  Out_Value := Tmp;
               else
                  Out_Value := new String'(Output);
               end if;
            end;
         end if;
      end loop;

   exception
      when Process_Died | Invalid_Process =>
         if Fd /= null then
            Close (Process_Descriptor'Class (Fd.all), Status);
            if Status = 0 then
               Success := True;
            else
               Success := False;
            end if;
         else
            Success := False;
         end if;

      when E : others =>
         Trace (Me, "Exception when executing Internal_Sync_Execute: " &
                Exception_Information (E));
         Status := -1;
         Success := False;
   end Internal_Sync_Execute;

   ----------------
   -- Filter_Out --
   ----------------

   procedure Filter_Out
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor);
      type Remote_PD_Access is access all Remote_Process_Descriptor;
      function Convert is new
        Ada.Unchecked_Conversion (System.Address, Remote_PD_Access);

      Desc                : Remote_PD_Access renames Convert (User_Data);
      Size                : Natural;
      Matched             : GNAT.Regpat.Match_Array (0 .. 1);
      Tmp_Buf             : String_Access;
      Current_Filter      : Filter_List;
      Idx_First, Idx_Last : Natural;

   begin
      if Active (Me) then
         Log ("RCV", Str);
      end if;

      Idx_First := Str'First;
      Idx_Last := Str'Last;
      --  The following buffer accesses suppose that the descriptor's buffer
      --  is dynamically allocated (i.e. buffer_size is 0)

      if Desc.Machine.Echoing and then not Desc.Current_Echo_Skipped then
         --  PuTTY used in telnet mode with Windows server echoes all commands
         --  with a trailing '\r' (no \n). We take this into account here.
         for J in Str'Range loop
            if Str (J) = ASCII.LF
              or else
                (Str (J) = ASCII.CR
                 and then
                   (J = Str'Last
                    or else Str (J + 1) /= ASCII.LF))
            then
               Log ("Remove", Str (Str'First .. J));
               Size := Str'Last - J;
               Tmp_Buf := Desc.Buffer;
               Idx_First := J + 1;
               Idx_Last := Str'Last;
               Desc.Buffer := new String (1 .. Size);
               Desc.Buffer.all := Str (Idx_First .. Idx_Last);
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

      if not Desc.Terminated then
         --  Catch a prompt
         Match (Desc.Shell.Prompt.all,
                Desc.Buffer (1 .. Desc.Buffer_Index),
                Matched);

         if Matched (0) /= No_Match then
            if Desc.Machine.Desc.Dbg /= null then
               Print
                 (Desc.Machine.Desc.Dbg,
                  Desc.Buffer (1 .. Matched (0).Last), Output);
            end if;

            if Desc.Buffer'Last - Matched (0).First + 1 > Str'First then
               Idx_Last :=
                 Str'Last - (Desc.Buffer'Last - Matched (0).First + 1);
            else
               Idx_Last := 0;
            end if;

            Tmp_Buf := new String'
              (Desc.Buffer (1 .. Matched (0).First - 1));
            Desc.Buffer_Index := Matched (0).First - 1;
            Free (Desc.Buffer);
            Desc.Buffer := Tmp_Buf;
            Desc.Busy := False;
            Desc.Terminated := True;
         end if;
      end if;

      --  Call filters with modified buffer
      if Desc.R_Filters_Lock = 0 and then Idx_Last > Idx_First then
         Current_Filter := Desc.R_Filters;

         while Current_Filter /= null loop
            if Current_Filter.Filter_On = Output then
               Current_Filter.Filter
                 (Desc.all, Str (Idx_First .. Idx_Last),
                  Current_Filter.User_Data);
            end if;

            Current_Filter := Current_Filter.Next;
         end loop;
      end if;

      if not Desc.Busy
        and then Desc.Terminated
        and then Desc.Buffer'Length = 0
      then
         raise Process_Died;
      end if;
   end Filter_Out;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Descriptor : in out Remote_Process_Descriptor;
      Status     : out Integer)
   is
      Matched   : GNAT.Regpat.Match_Array (0 .. 1);
      Res       : Expect_Match;
      NL_Regexp : constant Pattern_Matcher :=
                    Compile ("^[^\n\r]*(\n|\r)", Single_Line);

   begin
      if Descriptor.Session_Nb = 0 then
         Status := 0;
         return;
      end if;

      if not Finalized then
         if Descriptor.Busy then
            Interrupt (Descriptor);
         end if;

         Remove_Filter (TTY_Process_Descriptor (Descriptor),
                        Filter_Out'Access);
         Status := 0;

         declare
            Desc : TTY_Process_Descriptor renames
              Descriptor.Machine.Sessions (Descriptor.Session_Nb).Pd;
            State : Shell_State_Type renames
              Descriptor.Machine.Sessions (Descriptor.Session_Nb).State;
         begin

            --  Close is called before remote program terminates. Close the
            --  underlying session.
            --  This happens when the session is blocked or has died.

            if not Descriptor.Terminated then
               State := OFF;
               Close (Desc, Status);
               Descriptor.Session_Died := True;

            elsif not Descriptor.Session_Died then
               if Descriptor.Shell.Get_Status_Cmd /= null then
                  --  Try to retrieve the terminated program's status

                  if Descriptor.Use_Cr_Lf = CRLF then
                     Send
                       (Desc, Descriptor.Shell.Get_Status_Cmd.all & ASCII.CR);
                  else
                     Send (Desc, Descriptor.Shell.Get_Status_Cmd.all);
                  end if;

                  if Active (Me) then
                     Log ("SND", Descriptor.Shell.Get_Status_Cmd.all);
                  end if;

                  if Descriptor.Machine.Desc.Dbg /= null then
                     Print (Descriptor.Machine.Desc.Dbg,
                            Descriptor.Shell.Get_Status_Cmd.all,
                            Input);
                  end if;

                  --  Skip echo if needed

                  if Descriptor.Machine.Echoing then
                     Expect (Desc,
                             Res,
                             NL_Regexp,
                             Matched,
                             Descriptor.Machine.Desc.Timeout);
                     Trace
                       (Me, "skipped " & Expect_Out (Desc));
                  end if;

                  --  Get status
                  Expect (Desc,
                          Res,
                          Descriptor.Shell.Get_Status_Ptrn.all,
                          Matched,
                          Descriptor.Machine.Desc.Timeout);

                  if Descriptor.Machine.Desc.Dbg /= null then
                     Print (Descriptor.Machine.Desc.Dbg,
                            Expect_Out (Desc),
                            Output);
                  end if;

                  if Matched (0) /= No_Match then
                     declare
                        Out_Str : constant String :=
                                    Expect_Out (Desc)
                                    (Matched (1).First .. Matched (1).Last);
                     begin
                        Status := Integer'Value (Out_Str);

                        if Active (Me) then
                           Trace (Me, "status is " & Integer'Image (Status));
                        end if;

                     exception
                        when Constraint_Error =>
                           Trace (Me, "could not evaluate status from '" &
                                  Out_Str & "'");
                           Status := 1;
                     end;

                  else

                     if Active (Me) then
                        Trace (Me, "status does not match status pattern");
                        Trace (Me, Desc.Buffer.all);
                     end if;

                     Status := 0;
                  end if;

                  --  Get prompt

                  Expect (Desc, Res, Descriptor.Shell.Prompt.all,
                          Matched,
                          Descriptor.Machine.Desc.Timeout);

                  if Res = Expect_Timeout then
                     --  Shell does not respond to command. Kill it

                     raise Process_Died;

                  elsif Descriptor.Machine.Desc.Dbg /= null then
                     Print (Descriptor.Machine.Desc.Dbg,
                            Expect_Out (Desc),
                            Output);
                  end if;

                  if Active (Me) then
                     Log ("RCV", Expect_Out (Desc));
                  end if;
               end if;

               State := READY;
            end if;

            Descriptor.Input_Fd   := GNAT.OS_Lib.Invalid_FD;
            Descriptor.Output_Fd  := GNAT.OS_Lib.Invalid_FD;
            Descriptor.Error_Fd   := GNAT.OS_Lib.Invalid_FD;
            Descriptor.Pid        := Invalid_Pid;
            Descriptor.Process    := Null_Address;
            Descriptor.Session_Nb := 0;
            Close_Pseudo_Descriptor (Descriptor);
         end;

      else
         Status := 0;
      end if;

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
   end Close;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Descriptor : in out Remote_Process_Descriptor)
   is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (Descriptor : in out Remote_Process_Descriptor)
   is
      Remote_Desc : Remote_Descriptor_Access;
   begin
      if not Descriptor.Terminated then
         Remote_Desc := Get_Descriptor_From_Name
           (Descriptor.Machine.Desc.Access_Name.all);

         if Remote_Desc.Send_Interrupt /= null then
            --  Interrupt characters are understood when they are at the
            --  beginning of a line. Send LF first then, preceded by a space
            --  so that gdb do not try to execute the last command, if any.
            Send (Descriptor, ' ' & ASCII.LF & Remote_Desc.Send_Interrupt.all,
                  Add_LF => False);
         else
            --  Interrupt the session.
            Interrupt (Descriptor.Machine.Sessions (Descriptor.Session_Nb).Pd);
         end if;
      end if;

   exception
      when Constraint_Error =>
         Descriptor.Terminated := True;
   end Interrupt;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Desc : access Remote_Machine_Descriptor_Item) is
      Shell_Desc : Shell_Descriptor_Access;
   begin
      for J in Desc.Sessions'Range loop
         case Desc.Sessions (J).State is
            when OFF =>
               --  Already shutdown
               null;

            when READY =>
               Shell_Desc := Get_Descriptor_From_Name
                 (Desc.Desc.Shell_Name.all);

               if Shell_Desc.Exit_Cmds'Length /= 0 then
                  for K in Shell_Desc.Exit_Cmds'Range loop
                     Send (Desc.Sessions (J).Pd,
                           Shell_Desc.Exit_Cmds (K).all);
                  end loop;
               end if;

               begin
                  Interrupt (Desc.Sessions (J).Pd);
                  Close (Desc.Sessions (J).Pd);
               exception
                  when others =>
                     null;
               end;
               Desc.Sessions (J).State := OFF;

            when BUSY =>
               begin
                  Interrupt (Desc.Sessions (J).Pd);
                  Close (Desc.Sessions (J).Pd);
               exception
                  when others =>
                     null;
               end;
               Desc.Sessions (J).State := OFF;
         end case;
      end loop;
   end Close;

   ---------------
   -- Close_All --
   ---------------

   procedure Close_All is
   begin
      --  Be careful to not trace anything in here. This is called after
      --  the kernel is destroyed, and then cannot trace anymore

      Finalized := True;

      Machine_Descriptors.Close_All;
   end Close_All;

   ----------------------------
   -- Add_Machine_Descriptor --
   ----------------------------

   procedure Add_Machine_Descriptor
     (Desc : Machine_Descriptor)
   is
      Descriptor : Machine_Descriptor_Access;
      Pd         : constant TTY_Process_Descriptor :=
                     (Process_Descriptor with
                      Process   => System.Null_Address,
                      Use_Pipes => False);
   begin
      Descriptor := new Remote_Machine_Descriptor_Item'
        (Max_Nb_Connections => Desc.Max_Nb_Connections,
         Desc               => Desc,
         Echoing            => False,
         Determine_Echoing  => True,
         Next               => null,
         Sessions           => (others => (Pd, Auto, OFF)));
      Desc.Ref := Desc.Ref + 1;

      Register_Machine_Descriptor (Desc, Descriptor);
   end Add_Machine_Descriptor;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor
     (Nickname : String) return Remote_Machine_Descriptor_Access
   is
      Desc : Machine_Descriptor_Access;
   begin
      Desc := Get_Machine_Descriptor_Access (Nickname);

      if Desc.all in Remote_Machine_Descriptor_Item'Class then
         return Remote_Machine_Descriptor_Access (Desc);
      end if;

      raise Invalid_Nickname;
   end Get_Machine_Descriptor;

   -------------------
   -- Is_Configured --
   -------------------

   function Is_Configured (Nickname : String) return Boolean is
   begin
      --  Local machine is always configured

      if Nickname = "" then
         return True;
      end if;

      declare
         Desc : Remote_Machine_Descriptor_Access;
      begin
         Desc := Get_Machine_Descriptor (Nickname);
      exception
         when Invalid_Nickname => return False;
      end;

      return True;
   end Is_Configured;

   ----------------------
   -- Is_Ready_Session --
   ----------------------

   function Is_Ready_Session (Nickname : String) return Boolean is
      Desc : Remote_Machine_Descriptor_Access;
   begin
      Desc := Get_Machine_Descriptor (Nickname);

      for J in Desc.Sessions'Range loop
         if Desc.Sessions (J).State = READY then
            return True;
         end if;
      end loop;

      return False;

   exception
      when Invalid_Nickname =>
         return False;
   end Is_Ready_Session;

   ----------------------
   -- Get_Network_Name --
   ----------------------

   function Get_Network_Name (Nickname : String) return String is
      M : Remote_Machine_Descriptor_Access;
   begin
      M := Get_Machine_Descriptor (Nickname);

      return M.Desc.Network_Name.all;
   end Get_Network_Name;

   ------------
   -- Expect --
   ------------

   procedure Handle_Pre_Disconnect
     (Descriptor : Remote_Process_Descriptor;
      Timeout    : in out Integer);
   procedure Handle_Post_Disconnect
     (Descriptor : Remote_Process_Descriptor;
      Result : Expect_Match);
   --  Handle descriptor termination

   procedure Handle_Pre_Disconnect
     (Descriptor : Remote_Process_Descriptor;
      Timeout    : in out Integer) is
   begin
      if Descriptor.Terminated then
         --  We encountered the shell prompt. First let the caller retrieving
         --  the buffer. If buffer is empty, raise Process_Died.

         if Descriptor.Buffer_Index = 0 then
            raise Process_Died;
         end if;

         --  Don't need to wait for anything. Everything is already in the
         --  buffer

         Timeout := 1;

      elsif Timeout <= 0 then
         --  We wanted infinite timeout. This is too risky in remote mode,
         --  because of remote connections possible problems. Let's change it
         --  to a long timeout.
         Timeout := 60000;
      end if;
   end Handle_Pre_Disconnect;

   procedure Handle_Post_Disconnect
     (Descriptor : Remote_Process_Descriptor;
      Result     : Expect_Match) is
   begin
      if not Descriptor.Terminated
        and then Descriptor.Machine.Desc.Dbg /= null
      then
         Print (Descriptor.Machine.Desc.Dbg, Expect_Out (Descriptor), Output);
      end if;

      if Descriptor.Terminated and then Result = Expect_Timeout then
         raise Process_Died;
      end if;
   end Handle_Post_Disconnect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexp,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexp,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexp,
              Matched,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexp,
              Matched,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexps,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexps,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexps,
              Matched,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

   overriding procedure Expect
     (Descriptor  : in out Remote_Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False)
   is
      The_Timeout : Integer := Timeout;
   begin
      Handle_Pre_Disconnect (Descriptor, The_Timeout);
      Expect (TTY_Process_Descriptor (Descriptor),
              Result,
              Regexps,
              Matched,
              The_Timeout,
              Full_Buffer);
      Handle_Post_Disconnect (Descriptor, Result);

   exception
      when Process_Died =>
         Internal_Handle_Exceptions (Descriptor);
         raise;
   end Expect;

end GNAT.Expect.TTY.Remote;

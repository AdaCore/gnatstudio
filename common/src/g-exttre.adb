------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . E X P E C T . T T Y . R E M O T E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2007 AdaCore                      --
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

with Config;               use Config;
with Filesystem.Unix;
with Filesystem.Windows;
with Password_Manager;     use Password_Manager;
with String_Utils;         use String_Utils;
with Traces;               use Traces;
with User_Interface_Tools; use User_Interface_Tools;

package body GNAT.Expect.TTY.Remote is

   Me : constant Debug_Handle := Create ("GNAT.Expect.TTY.Remote");

   Finalized : Boolean := False;

   Login_Regexp : constant Pattern_Matcher :=
                    Compile ("^[^\n]*([Ll]ogin|[Nn]ame)[^\n]*: *$",
                             Multiple_Lines or Single_Line);
   --  Default regexp for login prompt

   type Compiled_Regexp_Array_Access is access Compiled_Regexp_Array;

   type Shell_State_Type is (OFF, BUSY, READY);
   --  The state of a session.
   --  OFF: the session has not been launched
   --  BUSY: the session is busy processing a remote program
   --  READY: the session has been launched, and is waiting on a shell prompt

   type Session is record
      Pd    : TTY_Process_Descriptor;
      State : Shell_State_Type := OFF;
   end record;
   --  This record represents a machine's session. A session is an opened
   --  connection that can be reused to launch successive remote programs.

   type Session_Array is
     array (Natural range <>) of Session;

   type Remote_Descriptor;
   type Remote_Descriptor_Access is access all Remote_Descriptor;

   type Extra_Prompts_Access is access all Extra_Prompts;

   Test_Echo_Cmd : constant String := "echo foo";
   Echoing_Regexps : constant Compiled_Regexp_Array
     := (1 => new Pattern_Matcher'
           (Compile ("^echo foo", Multiple_Lines or Single_Line)),
         2 => new Pattern_Matcher'
           (Compile ("^foo", Multiple_Lines or Single_Line)));

   type Remote_Descriptor is record
      Name                   : String_Access            := null;
      Start_Cmd              : String_Access            := null;
      Start_Cmd_Common_Args  : String_List_Access       := null;
      Start_Cmd_User_Args    : String_List_Access       := null;
      User_Prompt_Ptrn       : Pattern_Matcher_Access   := null;
      Password_Prompt_Ptrn   : Pattern_Matcher_Access   := null;
      Passphrase_Prompt_Ptrn : Pattern_Matcher_Access   := null;
      Extra_Prompt_Array     : Extra_Prompts_Access     := null;
      Use_Cr_Lf              : Boolean                  := False;
      Use_Pipes              : Boolean                  := False;
      Max_Password_Prompt    : Natural                  := 3;
      Next                   : Remote_Descriptor_Access := null;
   end record;

   type Shell_Descriptor is record
      Name             : String_Access                := null;
      Filesystem       : Filesystem_Access            := null;
      Start_Cmd        : String_Access                := null;
      Init_Cmds        : String_List_Access           := null;
      Exit_Cmds        : String_List_Access           := null;
      Cd_Cmd           : String_Access                := null;
      Get_Status_Cmd   : String_Access                := null;
      Get_Status_Ptrn  : Pattern_Matcher_Access       := null;
      Generic_Prompt   : Pattern_Matcher_Access       := null;
      Prompt           : Pattern_Matcher_Access       := null;
      Next             : Shell_Descriptor_Access      := null;
   end record;

   type Machine_Descriptor_Item (Max_Nb_Connections : Natural) is record
      Desc              : Machine_Descriptor;
      Sessions          : Session_Array (1 .. Max_Nb_Connections);
      Echoing           : Boolean := False;
      Determine_Echoing : Boolean := True;
      Next              : Machine_Descriptor_Access;
   end record;

   Remote_Descriptor_List  : Remote_Descriptor_Access := null;
   Shell_Descriptor_List   : Shell_Descriptor_Access := null;
   Machine_Descriptor_List : Machine_Descriptor_Access := null;
   --  ??? Should get rid of these global variables

   procedure Close (Desc : Machine_Descriptor_Access);
   --  Close all machine sessions

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
     (Nickname : String) return Machine_Descriptor_Access;
   --  Get machine descriptor from nickname

   function Get_Shell_Descriptor
     (Nickname : String) return Shell_Descriptor_Access;
   --  Get shell descriptor from nickname

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

      function Clean_Up (Str : in String) return String;
      --  On VMS, traces might begin with NUL character. Remove it for display

      function Clean_Up (Str : in String) return String is
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

   procedure Send
     (Descriptor   : in out Remote_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      TTY_Descriptor : TTY_Process_Descriptor
      renames TTY_Process_Descriptor (Descriptor);
   begin
      if Str /= "" then
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

      if Descriptor.Use_Cr_Lf and then Add_LF then
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

      function Process_Arg_List (L : String_List) return String_List;
      --  process the list of arguments, replacing tags with actual values

      procedure Wait_For_Prompt (Intermediate : Boolean := False);
      --  Wait for prompt on target

      procedure My_Send
        (Descriptor    : in out Process_Descriptor'Class;
         Dbg           : Connection_Debugger;
         Str           : String;
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

         Expect (Descriptor.Machine.Sessions (Session_Nb).Pd,
                 Res,
                 Regexp_Array & Extra_Regexp_Array,
                 Matched,
                 Descriptor.Machine.Desc.Timeout,
                 False);

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
               Trace (Me, "got prompt in Wait_For_Prompt");

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
                  Descriptor.Machine.Desc.User_Name.all);
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
                     Remote_Desc.Extra_Prompt_Array (Res_Extra).Answer.all);
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
                           Str);
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
         Add_LF        : Boolean := True;
         Empty_Buffer  : Boolean := False;
         Password_Mode : Boolean := False) is
      begin
         if Dbg /= null then
            if Password_Mode then
               Print (Dbg, "******", Input);
            else
               Print (Dbg, Str, Input);
            end if;
         end if;

         if Remote_Desc.Use_Cr_Lf and then Add_LF then
            Send (Descriptor, Str & ASCII.CR, True, Empty_Buffer);
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

      Remote_Desc := Remote_Descriptor_List;

      while Remote_Desc /= null loop
         exit when Remote_Desc.Name.all =
           Descriptor.Machine.Desc.Access_Name.all;

         Remote_Desc := Remote_Desc.Next;
      end loop;

      if Remote_Desc = null then
         raise Invalid_Nickname with
           "Invalid remote access tool name for " &
           Descriptor.Machine.Desc.Nickname.all &
           ": " & Descriptor.Machine.Desc.Access_Name.all;
      end if;

      Descriptor.Use_Cr_Lf := Remote_Desc.Use_Cr_Lf;

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

         Wait_For_Prompt (True);
         Send (Descriptor.Machine.Sessions (Session_Nb).Pd, "");
         --  Send just a LF to force Windows console to scroll, and thus
         --  correctly init the expect interface... strange...
         Wait_For_Prompt (True);

         --  Determine if the machine echoes commands

         if Descriptor.Machine.Determine_Echoing then
            if Descriptor.Machine.Desc.Dbg /= null then
               Print (Descriptor.Machine.Desc.Dbg,
                      Test_Echo_Cmd,
                      Input);
            end if;

            Send (Descriptor.Machine.Sessions (Session_Nb).Pd,
                  Test_Echo_Cmd);
            Expect (Descriptor.Machine.Sessions (Session_Nb).Pd, Res,
                    Echoing_Regexps,
                    Descriptor.Machine.Desc.Timeout, False);

            if Descriptor.Machine.Desc.Dbg /= null then
               Print (Descriptor.Machine.Desc.Dbg,
                      Expect_Out (Descriptor.Machine.Sessions (Session_Nb).Pd),
                      Output);
            end if;

            if Res = 1 then
               Log ("Init_Session", "remote echoes cmds");
               Descriptor.Machine.Echoing := True;
            elsif Res = 2 then
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

            if Descriptor.Machine.Desc.Dbg /= null then
               Print (Descriptor.Machine.Desc.Dbg,
                      Descriptor.Shell.Init_Cmds (J).all,
                      Input);
            end if;

            Send (Descriptor.Machine.Sessions (Session_Nb).Pd,
                  Descriptor.Shell.Init_Cmds (J).all);
            Wait_For_Prompt (True);
         end loop;

         if Descriptor.Machine.Desc.Extra_Init_Commands /= null then
            for J in Descriptor.Machine.Desc.Extra_Init_Commands'Range loop
               Flush (Descriptor.Machine.Sessions (Session_Nb).Pd);

               if Descriptor.Machine.Desc.Dbg /= null then
                  Print (Descriptor.Machine.Desc.Dbg,
                         Descriptor.Machine.Desc.Extra_Init_Commands (J).all,
                         Input);
               end if;

               Send (Descriptor.Machine.Sessions (Session_Nb).Pd,
                     Descriptor.Machine.Desc.Extra_Init_Commands (J).all);
               Wait_For_Prompt;
            end loop;
         end if;

         Descriptor.Machine.Sessions (Session_Nb).State := READY;

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

         declare
            Shell_Desc  : Shell_Descriptor_Access  := Shell_Descriptor_List;
         begin
            Desc.Shell := null;
            while Shell_Desc /= null loop
               if Shell_Desc.Name.all = Desc.Machine.Desc.Shell_Name.all then
                  Desc.Shell := Shell_Desc;
                  exit;
               end if;
               Shell_Desc := Shell_Desc.Next;
            end loop;
         end;

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

               if Desc.Machine.Desc.Dbg /= null then
                  Print (Desc.Machine.Desc.Dbg,
                         Expect_Out (Descriptor.all),
                         Output);
               end if;
            end;
         end if;

         --  First protect spaces in arguments

         for J in The_Args'Range loop
            if The_Args (J) (The_Args (J)'First) /= '"' then
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
      Regexp : constant Pattern_Matcher
        := Compile ("^[^\n]*\n", Single_Line or Multiple_Lines);

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

      Desc           : Remote_PD_Access renames Convert (User_Data);
      Size           : Natural;
      Matched        : GNAT.Regpat.Match_Array (0 .. 1);
      Tmp_Buf        : String_Access;
      Current_Filter : Filter_List;

   begin
      --  The following buffer accesses suppose that the descriptor's buffer
      --  is dynamically allocated (i.e. buffer_size is 0)

      if Desc.Machine.Echoing and then not Desc.Current_Echo_Skipped then
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

            Tmp_Buf := new String'
              (Desc.Buffer (1 .. Matched (0).First - 1));
            Desc.Buffer_Index := Matched (0).First - 1;
            Free (Desc.Buffer);
            Desc.Buffer := Tmp_Buf;
            Desc.Busy := False;
            Desc.Terminated := True;
            if Desc.Buffer'Length = 0 then
               raise Process_Died;
            end if;
         end if;
      end if;

      --  Call filters with modified buffer
      if Desc.R_Filters_Lock = 0 then
         Current_Filter := Desc.R_Filters;

         while Current_Filter /= null loop
            if Current_Filter.Filter_On = Output then
               Current_Filter.Filter
                 (Desc.all, Desc.Buffer.all, Current_Filter.User_Data);
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
      Matched   : GNAT.Regpat.Match_Array (0 .. 1);
      Res       : Expect_Match;
      NL_Regexp : constant Pattern_Matcher :=
                    Compile ("^[^\n]*\n", Single_Line);

   begin
      if Descriptor.Session_Nb = 0 then
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

                  Send (Desc, Descriptor.Shell.Get_Status_Cmd.all);

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
                        Trace (Me, "Status is '" & Out_Str & "'");
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

   procedure Close (Descriptor : in out Remote_Process_Descriptor) is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Descriptor : in out Remote_Process_Descriptor) is
   begin
      if not Descriptor.Terminated then
         --  Interrupt the session. The best case (unix) is that this SIGINT
         --  is transmitted to the remote process. The worst case (windows) is
         --  that the session is killed (thus terminating the remote process).
         Interrupt (Descriptor.Machine.Sessions (Descriptor.Session_Nb).Pd);
      end if;

   exception
      when Constraint_Error =>
         Descriptor.Terminated := True;
   end Interrupt;

   -----------
   -- Close --
   -----------

   procedure Close (Desc : Machine_Descriptor_Access) is
      Shell_Desc : Shell_Descriptor_Access;
   begin
      for J in Desc.Sessions'Range loop
         case Desc.Sessions (J).State is
            when OFF =>
               --  Already shutdown
               null;

            when READY =>
               Shell_Desc := Get_Shell_Descriptor
                 (Desc.Desc.Nickname.all);

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
      Machine : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      --  Be careful to not trace anything in here. This is called after
      --  the kernel is destroyed, and then cannot trace anymore

      Finalized := True;

      while Machine /= null loop
         Close (Machine);
         Machine := Machine.Next;
      end loop;
   end Close_All;

   ----------------------------------
   -- Add_Remote_Access_Descriptor --
   ----------------------------------

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      User_Prompt_Ptrn          : String_Access;
      Password_Prompt_Ptrn      : String_Access;
      Passphrase_Prompt_Ptrn    : String_Access;
      Extra_Prompt_Array        : Extra_Prompts := Null_Extra_Prompts;
      Use_Cr_Lf                 : Boolean := False;
      Use_Pipes                 : Boolean := False)
   is
      --  ??? Add max_password_prompt in parameters
      Remote          : constant Remote_Descriptor_Access :=
                          new Remote_Descriptor;
      Full_Exec       : String_Access;
      Password_Ptrn   : Pattern_Matcher_Access;
      Passphrase_Ptrn : Pattern_Matcher_Access;
      Login_Ptrn      : Pattern_Matcher_Access;

   begin
      Full_Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Start_Command);

      if Full_Exec = null then
         return;
      end if;

      if User_Prompt_Ptrn = null then
         Login_Ptrn := new Pattern_Matcher'(Login_Regexp);
      else
         Login_Ptrn := new Pattern_Matcher'(Compile (
           User_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      if Password_Prompt_Ptrn = null then
         Password_Ptrn := new Pattern_Matcher'(Get_Default_Password_Regexp);
      else
         Password_Ptrn := new Pattern_Matcher'(Compile (
           Password_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      if Passphrase_Prompt_Ptrn = null then
         Passphrase_Ptrn :=
           new Pattern_Matcher'(Get_Default_Passphrase_Regexp);
      else
         Passphrase_Ptrn := new Pattern_Matcher'(Compile (
           Passphrase_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      Remote.all :=
        (Name => new String'(Name),
         Start_Cmd              => Full_Exec,
         Start_Cmd_Common_Args  => new String_List'(Start_Command_Common_Args),
         Start_Cmd_User_Args    => new String_List'(Start_Command_User_Args),
         User_Prompt_Ptrn       => Login_Ptrn,
         Password_Prompt_Ptrn   => Password_Ptrn,
         Passphrase_Prompt_Ptrn => Passphrase_Ptrn,
         Extra_Prompt_Array     => new Extra_Prompts'(Extra_Prompt_Array),
         Use_Cr_Lf              => Use_Cr_Lf,
         Use_Pipes              => Use_Pipes,
         Max_Password_Prompt    => 3,
         Next                   => Remote_Descriptor_List);
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
      FS                  : Filesystem_Record'Class;
      Init_Commands       : String_List        := Null_String_List;
      Exit_Commands       : String_List        := Null_String_List;
      Cd_Command          : String             := "";
      Get_Status_Command  : String             := "";
      Get_Status_Ptrn     : String             := "")
   is
      Shell : constant Shell_Descriptor_Access := new Shell_Descriptor;
      Item  : Shell_Descriptor_Access;
   begin
      Shell.all :=
        (Name             => new String'(Name),
         Filesystem       => new Filesystem_Record'Class'(FS),
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
         Next             => null);

      Item := Shell_Descriptor_List;

      if Item = null or else Item.Name.all > Name then
         Shell.Next := Shell_Descriptor_List;
         Shell_Descriptor_List := Shell;
      else
         while Item /= null loop
            if Item.Next = null or else Item.Next.Name.all > Name then
               Shell.Next := Item.Next;
               Item.Next := Shell;
               exit;
            end if;
            Item := Item.Next;
         end loop;
      end if;
   end Add_Shell_Descriptor;

   -----------
   -- Unref --
   -----------

   procedure Unref (Desc : in out Machine_Descriptor) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Machine_Descriptor_Record'Class, Machine_Descriptor);
   begin
      if Desc.Ref <= 1 then
         Free (Desc.Nickname);
         Free (Desc.Network_Name);
         Free (Desc.Access_Name);
         Free (Desc.Shell_Name);
         Free (Desc.Extra_Init_Commands);
         Free (Desc.User_Name);
         --  ??? Free Desc.Dbg if any.
         Unchecked_Free (Desc);
      else
         Desc.Ref := Desc.Ref - 1;
      end if;
   end Unref;

   ----------------------------
   -- Add_Machine_Descriptor --
   ----------------------------

   procedure Add_Machine_Descriptor
     (Desc : Machine_Descriptor)
   is
      Descriptor : Machine_Descriptor_Access;
      Current    : Machine_Descriptor_Access;
      Prev       : Machine_Descriptor_Access;
      Pd         : constant TTY_Process_Descriptor :=
                     (Process_Descriptor with
                      Process   => System.Null_Address,
                      Use_Pipes => False);
   begin
      Descriptor := new Machine_Descriptor_Item'
        (Max_Nb_Connections => Desc.Max_Nb_Connections,
         Desc               => Desc,
         Echoing            => False,
         Determine_Echoing  => True,
         Next               => null,
         Sessions           => (others => (Pd, OFF)));
      Desc.Ref := Desc.Ref + 1;

      --  Place the new machine in an alphabetically ordered list.
      Current := Machine_Descriptor_List;
      Prev := null;

      while Current /= null loop

         if Current.Desc.Nickname.all = Desc.Nickname.all then
            --  Same nickname : replace it
            Descriptor.Next := Current.Next;

            if Prev /= null then
               Prev.Next := Descriptor;
            else
               Machine_Descriptor_List := Descriptor;
            end if;

            Close (Current);
            Unref (Current.Desc);

            return;

         elsif Current.Desc.Nickname.all > Desc.Nickname.all then
            Descriptor.Next := Current;

            if Prev /= null then
               Prev.Next := Descriptor;
            else
               Machine_Descriptor_List := Descriptor;
            end if;

            return;
         end if;

         Prev := Current;
         Current := Current.Next;
      end loop;

      --  Place the new Descriptor at the end of the list
      if Prev /= null then
         Prev.Next := Descriptor;
      else
         Machine_Descriptor_List := Descriptor;
      end if;
   end Add_Machine_Descriptor;

   -------------------------------
   -- Remove_Machine_Descriptor --
   -------------------------------

   procedure Remove_Machine_Descriptor (Desc : in out Machine_Descriptor) is
      Current : Machine_Descriptor_Access;
      Prev    : Machine_Descriptor_Access;
   begin
      Current := Machine_Descriptor_List;
      Prev := null;

      while Current /= null loop
         if Current.Desc = Desc then
            if Prev /= null then
               Prev.Next := Current.Next;
            else
               Machine_Descriptor_List := Current.Next;
            end if;
            Close (Current);
            Unref (Current.Desc);
            exit;
         end if;

         Prev := Current;
         Current := Current.Next;
      end loop;

      Desc := null;
   end Remove_Machine_Descriptor;

   -------------------------------
   -- Remove_Machine_Descriptor --
   -------------------------------

   procedure Remove_All_Machine_Descriptors is
      Current : Machine_Descriptor_Access;
      Next    : Machine_Descriptor_Access;
   begin
      Current := Machine_Descriptor_List;

      while Current /= null loop
         Next := Current.Next;
         Close (Current);
         Unref (Current.Desc);
         Current := Next;
      end loop;

      Machine_Descriptor_List := null;
   end Remove_All_Machine_Descriptors;

   -----------------------------
   -- Get_Nb_Shell_Descriptor --
   -----------------------------

   function Get_Nb_Shell_Descriptor return Natural is
      N : Natural;
      Desc : Shell_Descriptor_Access;
   begin
      N := 0;

      Desc := Shell_Descriptor_List;

      while Desc /= null loop
         N := N + 1;
         Desc := Desc.Next;
      end loop;

      return N;
   end Get_Nb_Shell_Descriptor;

   -------------------------------
   -- Get_Shell_Descriptor_Name --
   -------------------------------

   function Get_Shell_Descriptor_Name (N : Natural) return String is
      Desc : Shell_Descriptor_Access;
   begin
      Desc := Shell_Descriptor_List;

      for J in 2 .. N loop
         Desc := Desc.Next;
      end loop;

      return Desc.Name.all;
   end Get_Shell_Descriptor_Name;

   -------------------------------
   -- Get_Filesystem_From_Shell --
   -------------------------------

   function Get_Filesystem_From_Shell
     (Shell : String) return Filesystem_Record'Class
   is
      Desc : Shell_Descriptor_Access;
   begin
      Desc := Shell_Descriptor_List;

      while Desc /= null loop
         if Desc.Name.all = Shell then
            return Desc.Filesystem.all;
         end if;

         Desc := Desc.Next;
      end loop;

      return Get_Local_Filesystem;
   end Get_Filesystem_From_Shell;

   -------------------------------------
   -- Get_Nb_Remote_Access_Descriptor --
   -------------------------------------

   function Get_Nb_Remote_Access_Descriptor return Natural is
      N : Natural;
      Desc : Remote_Descriptor_Access;
   begin
      N := 0;
      Desc := Remote_Descriptor_List;

      while Desc /= null loop
         N := N + 1;
         Desc := Desc.Next;
      end loop;

      return N;
   end Get_Nb_Remote_Access_Descriptor;

   ----------------------------
   -- Get_Remote_Access_Name --
   ----------------------------

   function Get_Remote_Access_Name (N : Natural) return String is
      Desc : Remote_Descriptor_Access;
   begin
      Desc := Remote_Descriptor_List;

      for J in 2 .. N loop
         Desc := Desc.Next;
      end loop;

      return Desc.Name.all;
   end Get_Remote_Access_Name;

   -------------------------------
   -- Get_Nb_Machine_Descriptor --
   -------------------------------

   function Get_Nb_Machine_Descriptor return Natural is
      Nb : Natural := 0;
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         Nb := Nb + 1;
         Desc := Desc.Next;
      end loop;

      return Nb;
   end Get_Nb_Machine_Descriptor;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor (N : Natural) return Machine_Descriptor is
      Nb   : Natural := 1;
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         if Nb = N then
            return Desc.Desc;
         end if;

         Nb := Nb + 1;
         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname;
   end Get_Machine_Descriptor;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor
     (Nickname : String) return Machine_Descriptor
   is
      Desc : Machine_Descriptor_Access;
   begin
      Desc := Get_Machine_Descriptor (Nickname);
      return Desc.Desc;
   end Get_Machine_Descriptor;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (N : Natural) return String is
   begin
      return Get_Machine_Descriptor (N).Nickname.all;
   end Get_Nickname;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor
     (Nickname : String) return Machine_Descriptor_Access
   is
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         if Desc.Desc.Nickname.all = Nickname then
            return Desc;
         end if;

         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname;
   end Get_Machine_Descriptor;

   --------------------------
   -- Get_Shell_Descriptor --
   --------------------------

   function Get_Shell_Descriptor
     (Nickname : String) return Shell_Descriptor_Access
   is
      Desc       : Shell_Descriptor_Access := Shell_Descriptor_List;
      Shell_Name : constant String :=
                     Get_Machine_Descriptor (Nickname).Desc.Shell_Name.all;
   begin
      while Desc /= null loop
         if Desc.Name.all = Shell_Name then
            return Desc;
         end if;

         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname;
   end Get_Shell_Descriptor;

   -------------------
   -- Is_Configured --
   -------------------

   function Is_Configured (Nickname : String) return Boolean is
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      --  Local machine is always configured

      if Nickname = "" then
         return True;
      end if;

      while Desc /= null loop
         if Desc.Desc.Nickname.all = Nickname then
            return True;
         end if;

         Desc := Desc.Next;
      end loop;

      return False;
   end Is_Configured;

   ----------------------
   -- Is_Ready_Session --
   ----------------------

   function Is_Ready_Session (Nickname : String) return Boolean is
      Desc : Machine_Descriptor_Access;
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
   begin
      return Get_Machine_Descriptor (Nickname).Desc.Network_Name.all;
   end Get_Network_Name;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Nickname : String) return Filesystem_Record'Class is
   begin
      if Nickname = "" then
         return Get_Local_Filesystem;
      else
         return Get_Shell_Descriptor (Nickname).Filesystem.all;
      end if;
   end Get_Filesystem;

   --------------------------
   -- Get_Local_Filesystem --
   --------------------------

   function Get_Local_Filesystem return Filesystem_Record'Class is
      Windows_FS : Filesystem.Windows.Windows_Filesystem_Record;
      Unix_FS    : Filesystem.Unix.Unix_Filesystem_Record;
   begin
      if Host = Config.Windows then
         return Windows_FS;
      else
         --  Unix and windows support only
         return Unix_FS;
      end if;
   end Get_Local_Filesystem;

   ------------
   -- Expect --
   ------------

   procedure Handle_Pre_Disconnect
     (Descriptor : in out Remote_Process_Descriptor;
      Timeout    : in out Integer);
   procedure Handle_Post_Disconnect
     (Descriptor : in out Remote_Process_Descriptor;
      Result : Expect_Match);
   --  Handle descriptor termination

   procedure Handle_Pre_Disconnect
     (Descriptor : in out Remote_Process_Descriptor;
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
     (Descriptor : in out Remote_Process_Descriptor;
      Result : Expect_Match) is
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

   procedure Expect
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

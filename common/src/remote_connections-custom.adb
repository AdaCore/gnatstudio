-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005                           --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Calendar;      use GNAT.Calendar;
with Gtk.Enums;          use Gtk.Enums;
with Gtkada.Dialogs;     use Gtkada.Dialogs;
with GUI_Utils;          use GUI_Utils;
with OS_Utils;           use OS_Utils;
with String_Utils;       use String_Utils;
with Traces;             use Traces;
with VFS;                use VFS;

package body Remote_Connections.Custom is

   Me : constant Debug_Handle := Create ("Remote_Connections.Custom");
   Full_Me : constant Debug_Handle := Create ("Remote_Connections.Custom_Full",
                                              Off);

   Custom_Root : Custom_Connection_Access := null;
   --  List of all custom connections

   Regexp_Root : Regexp_Access := null;
   --  List of all regexps

   Temporary_Dir : constant String := Get_Tmp_Dir;
   --  Where temporary files are stored. This could also be in ~/.gps or
   --  the root project's object directory.

   Ls_Regexp : constant Pattern_Matcher := Compile
     (".(........)\s+\d+\s+([\w\d]+)\s+([\w\d]+)\s+(\d+)\s+(\w\w\w) (\d\d)"
      & "\s+((\d\d):(\d\d))?\s*(\d\d\d\d)?");
   Ls_Month_Parens : constant := 5;
   Ls_Day_Parens   : constant := 6;
   Ls_Hour_Parens  : constant := 8;
   Ls_Min_Parens   : constant := 9;
   Ls_Year_Parens  : constant := 10;
   --  Regexp to match the output of ls, and extract the required fields

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (TTY_Process_Descriptor, TTY_Process_Descriptor_Access);

   function Get_String (S : in String_Ptr) return String;
   --  returns "" if S is null, S.all else

   function Substitute
     (Cmd                  : String;
      Local_Full_Name      : String;
      Connection           : access Custom_Connection'Class;
      Temporary_File       : String := "") return String;
   --  Substitute the special variables in Cmd (%f, %F, ...)
   --  %t is only defined if Temporary_File is not the empty string.
   --  User_Name_Is_Explicit should be true if the user name was specified by
   --  the user in the command.

   function Get_Regexp_Array (Action : in Action_Access)
                              return Compiled_Regexp_Array;
   --  Get a Compiled_Regexp_Array structure from a list of expected Expects

   procedure Ensure_Connection
     (Connection : access Custom_Connection'Class;
      Result     : out Return_Enum);
   --  Makes sure that the remote is connected

   function Execute_Cmd
     (Connection      : access Custom_Connection'Class;
      Cmd             : in     Action_Access;
      Local_Full_Name : in     String  := "";
      WriteTmpFile    : in     String  := "";
      Is_Mount        : in     Boolean := False)
      return Return_Enum;
   --  Execute the Command given in input for the Connection.
   --  returns the Expect to this command

   function Analyze_Timestamp (Str : in String) return Ada.Calendar.Time;
   --  analyze Str and determine the date

   procedure Trace_Filter_Input
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   procedure Trace_Filter_Output
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Filter all input/output of the shell

   ----------------
   -- Get_String --
   ----------------

   function Get_String (S : in String_Ptr) return String is
   begin
      if S = null then
         Trace (Full_Me, "Get_String from a null ptr");
         return "";
      else
         return S.all;
      end if;
   end Get_String;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Cmd                  : String;
      Local_Full_Name      : String;
      Connection           : access Custom_Connection'Class;
      Temporary_File       : String := "") return String
   is
      function Sub (Param : String; Quoted : Boolean) return String;
      --  Perform the substitution for Param

      function Sub (Param : String; Quoted : Boolean) return String is
         Last : Natural;
         pragma Unreferenced (Quoted);
      begin
         if Param'Length > 1 then
            if User_Is_Specified (Connection) then
               return Substitute (Param, Local_Full_Name,
                                  Connection, Temporary_File);
            else
               return "";
            end if;
         elsif Param = "F" then
            for J in Local_Full_Name'Range loop
               exit when Local_Full_Name (J) = ASCII.NUL;
               Last := J;
            end loop;
            return Local_Full_Name (Local_Full_Name'First .. Last);
         elsif Param = "f" then
            return Base_Name (Local_Full_Name);
         elsif Param = "d" then
            return Dir_Name (Local_Full_Name);
         elsif Param = "h" then
            return Get_Host (Connection);
         elsif Param = "u" then
            return Get_User (Connection);
         elsif Param = "t" then
            for J in Temporary_File'Range loop
               exit when Temporary_File (J) = ASCII.NUL;
               Last := J;
            end loop;
            return Temporary_File (Temporary_File'First .. Last);
         else
            raise Invalid_Substitution;
         end if;
      end Sub;

      C : constant String := Substitute
        (Cmd,
         Substitution_Char => '%',
         Callback          => Sub'Unrestricted_Access);
   begin
      Trace (Full_Me, "Substitute : result is """ & C & """");
      return C;
   end Substitute;

   ----------------------
   -- Get_Regexp_Array --
   ----------------------

   function Get_Regexp_Array (Action : in Action_Access)
                              return Compiled_Regexp_Array
   is
      Expect   : Expect_Access;
      Nb_Items : Natural;
   begin
      if Action = null then
         return (1 .. 0 => null);
      end if;
      --  get the size of the structure
      Expect := Action.Expects;
      Nb_Items := 0;
      while Expect /= null loop
         Nb_Items := Nb_Items + 1;
         Expect := Expect.Next;
      end loop;

      declare
         Regexps : Compiled_Regexp_Array (1 .. Nb_Items);
      begin
         Expect := Action.Expects;
         for J in Regexps'Range loop
            Regexps (J) := Expect.Regexp;
            Expect := Expect.Next;
         end loop;
         return Regexps;
      end;
   end Get_Regexp_Array;

   -----------------------
   -- Ensure_Connection --
   -----------------------

   procedure Ensure_Connection
     (Connection : access Custom_Connection'Class;
      Result     : out Return_Enum)
   is
      Min_Delay_Between_Attempts : constant Duration := 10.0;  --  seconds
   begin
      if not Connection.Is_Open then
         Trace (Me, "Ensure_Connection");
         if Clock - Connection.Last_Connection_Attempt >=
           Min_Delay_Between_Attempts
         then
            Connection.Last_Connection_Attempt := Clock;

            Result := Execute_Cmd (Connection,
                                   Connection.Commands (Open_Session_Cmd),
                                   Is_Mount => True);
         else
            Result := NOK;
         end if;
      else
         Result := OK;
      end if;
   end Ensure_Connection;

   procedure Execute_Action_Recursive
     (Connection      : access Custom_Connection'Class;
      Action          : in     Action_Access;
      Local_Full_Name : in     String := "";
      WriteTmpFile    : in     String := "";
      ReadTmpBase     : in out Temp_File_Name;
      Result          : in     String := "";
      Pd              : in     TTY_Process_Descriptor_Access;
      Keep_Pd         : in out Boolean;
      Ret_Value       :    out Return_Enum);
   --  executes the action recursively

   ------------------------------
   -- Execute_Action_Recursive --
   ------------------------------

   procedure Execute_Action_Recursive
     (Connection      : access Custom_Connection'Class;
      Action          : in     Action_Access;
      Local_Full_Name : in     String := "";
      WriteTmpFile    : in     String := "";
      ReadTmpBase     : in out Temp_File_Name;
      Result          : in     String := "";
      Pd              : in     TTY_Process_Descriptor_Access;
      Keep_Pd         : in out Boolean;
      Ret_Value       :    out Return_Enum)
   is
      Regexps    : constant Compiled_Regexp_Array :=
        Get_Regexp_Array (Action);
      Args       : Argument_List_Access;
      L_Action   : Action_Access;
      Expect_Ptr : Expect_Access;
      Tmp        : String_Access;
      Matched    : Match_Array (0 .. 0);
      Exp_Result : Expect_Match;
      Success    : Boolean;
      Tmp_Fd     : File_Descriptor;
      L_Pd       : TTY_Process_Descriptor_Access;
      New_Pd     : Boolean;

      function Get_Tmp_File return String;
      --  function used to determine if we need read or write tmp file

      ------------------
      -- Get_Tmp_File --
      ------------------

      function Get_Tmp_File return String is
      begin
         if WriteTmpFile = "" then
            return Temporary_Dir & ReadTmpBase;
         else
            return WriteTmpFile;
         end if;
      end Get_Tmp_File;

   begin
      Ret_Value := No_Statement;

      if Action = null then
         return;
      end if;
      L_Pd := Pd;
      New_Pd := False;

      --  for each action, first execute it
      case Action.Kind is
         when Null_Action =>
            null;

         when Spawn =>
            declare
               The_Command : constant String
                 := Substitute (Get_String (Action.Cmd),
                                Local_Full_Name,
                                Connection,
                                Get_Tmp_File);
            begin
               Trace (Me, "Action : Spawn " & The_Command);
               Args := Argument_String_To_List (The_Command);

               L_Pd := new TTY_Process_Descriptor;
               Non_Blocking_Spawn
                 (L_Pd.all,
                  Command     => Args (Args'First).all,
                  Args        => Args (Args'First + 1 .. Args'Last),
                  Buffer_Size => 0,
                  Err_To_Out  => True);
               Keep_Pd := False;
               New_Pd := True;
               if Active (Full_Me) then
                  Add_Filter (L_Pd.all, Trace_Filter_Output'Access, Output);
                  Add_Filter (L_Pd.all, Trace_Filter_Input'Access, Input);
               end if;

               Free (Args);
            end;

         when Set_Session =>
            Trace (Full_Me, "Action : Set_Session");
            Keep_Pd := True;

         when Return_Value =>
            Trace (Full_Me, "Action : ReturnValue");
            Ret_Value := Action.Value;

         when Input_Login =>
            Trace (Full_Me, "Action : InputLogin");
            Send (L_Pd.all, Get_User (Connection), Add_LF => True);

         when Input_Password =>
            Trace (Full_Me, "Action : InputPassword");
            if Connection.Password_Attempts <
              Connection.Max_Password_Attempts then
               Connection.Password_Attempts :=
                 Connection.Password_Attempts + 1;
               declare
                  Passwd : constant String := Query_Password
                    (Get_User (Connection) & "'s password on "
                     & Get_Host (Connection));
               begin
                  Set_Passwd (Connection, Passwd);
                  if Passwd = "" then
                     Trace (Me, "Interrupted password query");
                     Interrupt (L_Pd.all);
                     Ret_Value := NOK_InvalidPassword;
                  else
                     Send (L_Pd.all, Passwd, Add_LF => True);
                  end if;
               end;
            else
               Ret_Value := NOK_InvalidPassword;
            end if;

         when Send =>
            declare
               The_Command : constant String
                 := Substitute (Get_String (Action.Param),
                                Local_Full_Name,
                                Connection,
                                Get_Tmp_File);
            begin
               Trace (Full_Me, "Action : Send '" & The_Command & "'");
               Send (L_Pd.all, The_Command, Add_LF => True);
            end;

         when Send_File =>
            Trace (Full_Me, "Action : Send_File");
            Tmp := Read_File (WriteTmpFile);
            if Tmp /= null then
               Send (L_Pd.all, Tmp.all,
                     Add_LF => True, Empty_Buffer => True);
               Free (Tmp);
            end if;

         when Read_File =>
            Trace (Full_Me, "Action : Read_File");
            Connection.Buffer := new String'(Result);

         when Read_Timestamp =>
            Trace (Full_Me, "Action : Read_Timestamp");
            Connection.Timestamp := Analyze_Timestamp (Result);

         when Read_Tmp_File =>
            Trace (Full_Me, "Action : Read_Tmp_File");
            Connection.Buffer := Read_File (Temporary_Dir & ReadTmpBase);
            Delete_File (Temporary_Dir & ReadTmpBase, Success);

         when Create_Tmp_File =>
            Trace (Full_Me, "Action : Create_Tmp_File");
            Change_Dir (Temporary_Dir);
            Create_Temp_File (Tmp_Fd, ReadTmpBase);
            Close (Tmp_Fd);

         when Force_Reconnect =>
            Trace (Full_Me, "Action : Force_Reconnect");
            Close (Connection.Pd.all);
            Free (Connection.Pd);
            Connection.Is_Open := False;
            Ensure_Connection
              (Connection => Connection,
               Result     => Ret_Value);
            if Ret_Value = OK then
               L_Pd := Connection.Pd;
               Ret_Value := No_Statement; --  reconnected, go on
            end if;
      end case;

      --  once the action executed, wait for Expects (if any)
      if Regexps'Length > 0 then
         while Ret_Value = No_Statement loop
            Trace (Full_Me, "Execute_Action : expecting" &
                   Natural'Image (Regexps'Length) & " Expect(s)");
            Trace (Full_Me, "Execute_Action : timeout value is" &
                   Integer'Image (Action.Timeout.Timeout / 1000) & " seconds");
            Expect (L_Pd.all, Exp_Result, Regexps, Matched,
                    Timeout => Action.Timeout.Timeout);

            Trace (Full_Me, "Expect answered " &
                   Expect_Match'Image (Exp_Result));

            if Exp_Result = Expect_Timeout then
               Trace (Full_Me, "Timeout !");
               --  if no action is defined for timeout
               if Action.Timeout.Actions = null then
                  Ret_Value := NOK_Timeout;
               else
                  L_Action := Action.Timeout.Actions;
                  while Ret_Value = No_Statement and L_Action /= null loop
                     Execute_Action_Recursive
                       (Connection,
                        L_Action,
                        Local_Full_Name,
                        WriteTmpFile,
                        ReadTmpBase,
                        "",
                        L_Pd,
                        Keep_Pd,
                        Ret_Value);
                     L_Action := L_Action.Next;
                  end loop;
               end if;
            else

               Expect_Ptr := Action.Expects;

               for I in 2 .. Exp_Result loop
                  Expect_Ptr := Expect_Ptr.Next;
               end loop;

               if Expect_Ptr = null then
                  Trace (Me, "** ERROR ! Regexp answered " &
                         Expect_Match'Image (Exp_Result) &
                         " which is unexpected");
                  --  should never happend... (except bug in expect call)
                  Ret_Value := NOK_Timeout;
               else
                  declare
                     Output : constant String := Expect_Out (L_Pd.all);
                  begin
                     Trace (Full_Me, "execute Expect's actions");
                     L_Action := Expect_Ptr.Actions;
                     while Ret_Value = No_Statement and L_Action /= null loop
                        Execute_Action_Recursive
                          (Connection,
                           L_Action,
                           Local_Full_Name,
                           WriteTmpFile,
                           ReadTmpBase,
                           Output (Output'First .. Matched (0).First - 2),
                           L_Pd,
                           Keep_Pd,
                           Ret_Value);
                        L_Action := L_Action.Next;
                     end loop;
                  end;
               end if;
            end if;
         end loop;
      end if;
      if New_Pd then
         if Keep_Pd and Ret_Value = OK then
            Connection.Pd := L_Pd;
            Connection.Is_Open := True;
         else
            Close (L_Pd.all);
            Free (L_Pd);
         end if;
      end if;

   exception
      when Process_Died =>
         Trace (Me, "** Process died !");
         raise;
      when E : others =>
         Trace (Me, "** Exception : ");
         Trace (Me, Ada.Exceptions.Exception_Information (E));
         Ret_Value := NOK_Timeout;
   end Execute_Action_Recursive;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Cmd
     (Connection      : access Custom_Connection'Class;
      Cmd             : in     Action_Access;
      Local_Full_Name : in     String  := "";
      WriteTmpFile    : in     String  := "";
      Is_Mount        : in     Boolean := False)
      return Return_Enum
   is
      L_Action      : Action_Access;
      ReadTmpBase   : String (1 .. Temp_File_Len) := (others => ' ');
      Keep_Pd       : Boolean;
      Ret_Value     : Return_Enum;
      Response      : Message_Dialog_Buttons;
      Close_Cnx     : Boolean;
      pragma Unreferenced (Response);
   begin
      if not Is_Mount then
         Ensure_Connection
           (Connection => Connection,
            Result     => Ret_Value);
         if Ret_Value /= OK then
            return Ret_Value;
         end if;
      end if;
      Keep_Pd := True;

      L_Action := Cmd;
      while L_Action /= null loop
         Execute_Action_Recursive
           (Connection      => Connection,
            Action          => L_Action,
            Local_Full_Name => Local_Full_Name,
            WriteTmpFile    => WriteTmpFile,
            ReadTmpBase     => ReadTmpBase,
            Pd              => Connection.Pd,
            Keep_Pd         => Keep_Pd,
            Ret_Value       => Ret_Value);
         L_Action := L_Action.Next;
      end loop;

      Trace (Full_Me, "Ret_Value is " &
             Return_Enum'Image (Ret_Value));

      Close_Cnx := False;
      case Ret_Value is
         when NOK_InvalidPassword =>
            Response := Message_Dialog
              ("Invalid password when connecting to "
               & Get_Host (Connection),
               Dialog_Type   => Error,
               Buttons       => Button_OK,
               Title         => "Invalid password",
               Justification => Justify_Left);
            Close_Cnx := True;

         when NOK_UnknownHost =>
            Response := Message_Dialog
              ("host " & Get_Host (Connection) & " is unknown",
               Dialog_Type   => Error,
               Buttons       => Button_OK,
               Title         => "Unknown host",
               Justification => Justify_Left);
            Close_Cnx := True;

         when NOK_Timeout =>
            Response := Message_Dialog
              ("Timeout when connection to "
               & Get_Host (Connection),
               Dialog_Type   => Error,
               Buttons       => Button_OK,
               Title         => "Time out",
               Justification => Justify_Left);
            Close_Cnx := True;

         when others =>
            Connection.Password_Attempts := 0;
            null;
      end case;

      if Close_Cnx and Connection.Is_Open then
         Close (Connection.Pd.all);
         Free (Connection.Pd);
         Connection.Is_Open := False;
      end if;

      return Ret_Value;
   exception
      when Process_Died =>
         Connection.Is_Open := False;
         return NOK_Timeout;
   end Execute_Cmd;

   -----------------------
   -- Analyze_Timestamp --
   -----------------------

   function Analyze_Timestamp (Str : in String) return Ada.Calendar.Time
   is
      function Safe_Value (S : String; Default : Integer := 1) return Integer;
      --  Same as 'Value but doesn't crash on invalid input

      function Month_Name_To_Number (M : String) return Month_Number;
      --  Return the month number corresponding to M

      ----------------
      -- Safe_Value --
      ----------------

      function Safe_Value
        (S : String; Default : Integer := 1) return Integer is
      begin
         return Integer'Value (S);
      exception
         when Constraint_Error =>
            return Default;
      end Safe_Value;

      --------------------------
      -- Month_Name_To_Number --
      --------------------------

      function Month_Name_To_Number (M : String) return Month_Number is
      begin
         if M = "jan" then
            return 1;
         elsif M = "feb" then
            return 2;
         elsif M = "mar" then
            return 3;
         elsif M = "apr" then
            return 4;
         elsif M = "may" then
            return 5;
         elsif M = "jun" then
            return 6;
         elsif M = "jul" then
            return 7;
         elsif M = "aug" then
            return 8;
         elsif M = "sep" then
            return 9;
         elsif M = "oct" then
            return 10;
         elsif M = "nov" then
            return 11;
         else
            return 12;
         end if;
      end Month_Name_To_Number;

      Month  : Month_Number := 1;
      Day    : Day_Number;
      Year   : Year_Number := Ada.Calendar.Year (Clock);
      Hour   : Hour_Number := 1;
      Minute : Minute_Number := 1;
      Matched : Match_Array (0 .. 20);

   begin
      Trace (Me, "Analyze_Timestamp");
      if Str = "" then
         return VFS.No_Time;
      end if;

      Match (Ls_Regexp, Str, Matched);

      if Matched (0) /= No_Match then
         Month := Month_Name_To_Number
           (To_Lower (Str
                        (Matched (Ls_Month_Parens).First
                         .. Matched (Ls_Month_Parens).Last)));

         Day := Day_Number
           (Safe_Value
              (Str
                 (Matched (Ls_Day_Parens).First
                  .. Matched (Ls_Day_Parens).Last)));

         if Matched (Ls_Hour_Parens) /= No_Match then
            Hour := Hour_Number
              (Safe_Value
                 (Str
                    (Matched (Ls_Hour_Parens).First
                     .. Matched (Ls_Hour_Parens).Last)));
            Minute := Minute_Number
              (Safe_Value
                 (Str
                    (Matched (Ls_Min_Parens).First
                     .. Matched (Ls_Min_Parens).Last)));
         end if;

         if Matched (Ls_Year_Parens) /= No_Match then
            Year := Year_Number
              (Safe_Value
                 (Str
                    (Matched (Ls_Year_Parens).First
                     .. Matched (Ls_Year_Parens).Last)));
         end if;

         return GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, Minute, Second => 0);
      end if;

      return VFS.No_Time;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return VFS.No_Time;

   end Analyze_Timestamp;

   ------------------------
   -- Trace_Filter_Input --
   ------------------------

   procedure Trace_Filter_Input
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor, User_Data);
   begin
      Trace (Full_Me, '>' & Str);
   end Trace_Filter_Input;

   -------------------------
   -- Trace_Filter_Output --
   -------------------------

   procedure Trace_Filter_Output
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor, User_Data);
   begin
      Trace (Full_Me, '<' & Str);
   end Trace_Filter_Output;

   ------------------------------------------------
   -- Initialization functions from the xml file --
   ------------------------------------------------

   procedure Initialize_Regexp
     (Regexp :    out Regexp_Record;
      Top    : in     Glib.Xml_Int.Node_Ptr);
   --  initializes a new answer_record structure from xml node

   procedure Initialize_Action
     (Top     : in     Glib.Xml_Int.Node_Ptr;
      Actions :    out Action_Access);
   --  Initializes a command from xml node

   -----------------------
   -- Initialize_Answer --
   -----------------------

   procedure Initialize_Regexp
     (Regexp :    out Regexp_Record;
      Top    : in     Glib.Xml_Int.Node_Ptr)
   is
      Id      : constant String := Get_Attribute (Top, "id");
   begin
      Trace (Full_Me, "Initialize regexp '" & Id & "'");

      --  Init id field
      Regexp := Null_Regexp_Record;
      if Id = "" then
         Trace (Me, "** Error : the regexp has no 'id' attribute");
         return;
      end if;
      Regexp.Id := new String'(Id);

      --  Init Regexp field
      if Top.Value = null then
         Trace (Me, "** Error : Regexp has no value");
         Glib.Free (Regexp.Id);
         Regexp := Null_Regexp_Record;
         return;
      end if;
      Trace (Full_Me, "Regexp is """ & Top.Value.all & """");
      Regexp.Regexp := +(Compile (Top.Value.all,
                                  Case_Insensitive or Multiple_Lines));
   end Initialize_Regexp;

   ------------------------
   -- Initialize_Expects --
   ------------------------

   procedure Initialize_Regexps (Top : Glib.Xml_Int.Node_Ptr)
   is
      Regexp : Regexp_Record;
      Node   : Node_Ptr;
   begin
      Node := Top.Child;
      while Node /= null loop
         if To_Lower (Node.Tag.all) = "regexp" then
            Initialize_Regexp (Regexp, Node);
            if Regexp /= Null_Regexp_Record then
               Regexp.Next := Regexp_Root;
               Regexp_Root := new Regexp_Record'(Regexp);
            end if;
         end if;
         Node := Node.Next;
      end loop;
   end Initialize_Regexps;

   ------------------------
   -- Initialize_Command --
   ------------------------

   procedure Initialize_Action
     (Top     : in     Glib.Xml_Int.Node_Ptr;
      Actions :    out Action_Access)
   is
      Node          : Node_Ptr;
      Child_Node    : Node_Ptr;
      Action        : Action_Access;
      Action_Last   : Action_Access;
      Is_Valid      : Boolean;
      Action_Kind   : Action_Enum;
      Regexp        : Regexp_Access;
      Expect        : Expect_Record;
   begin
      Trace (Full_Me, "Initialize_Action");
      --  Initialize default values
      Action  := null;
      Actions := null;
      Action_Last := null;

      if Top = null then
         Trace (Full_Me, "no action defined");
         return;
      end if;

      Node := Top.Child;
      while Node /= null loop
         if To_Lower (Node.Tag.all) = "action" then
            Is_Valid := True;
            declare
               Kind : constant String := Get_Attribute (Node, "kind");
            begin
               if Kind = "" then
                  Trace (Me, "** Error: this action has no attribute 'kind'");
                  Is_Valid := False;
               else
                  Action_Kind := Action_Enum'Value (Kind);
                  Trace (Full_Me, "Action Kind is '" & Kind & "'");
               end if;
            exception
               when Constraint_Error =>
                  Trace (Me, "** Error : Invalid Action kind : " & Kind);
                  Is_Valid := False;
            end;
            if Is_Valid then
               --  first initialize the action itself
               case Action_Kind is
                  when Spawn =>
                     declare
                        Cmd          : constant String
                          := Get_Attribute (Node, "param");
                        Spawn_Action : Action_Record (Spawn);
                     begin
                        if Cmd = "" then
                           Trace (Me,
                                  "** Error: this Spawn action has " &
                                  "no attribute 'param'");
                           Is_Valid := False;
                        else
                           Spawn_Action.Cmd := new String'(Cmd);
                           Action := new Action_Record'(Spawn_Action);
                        end if;
                     end;

                  when Return_Value =>
                     declare
                        Value        : constant String
                          := Get_Attribute (Node, "param");
                        Return_Action : Action_Record (Action_Kind);
                     begin
                        if Value = "" then
                           Trace (Me,
                                  "** Error: this Return_Value action has " &
                                  "no attribute 'param'");
                           Is_Valid := False;
                        else
                           Return_Action.Value := Return_Enum'Value (Value);
                           Action := new Action_Record'(Return_Action);
                        end if;
                     exception
                        when Constraint_Error =>
                           Trace (Me, "** Error : Invalid value attribute : " &
                                  Value);
                           Is_Valid := False;
                     end;

                  when Send =>
                     declare
                        Param       : constant String
                          := Get_Attribute (Node, "param");
                        Send_Action : Action_Record (Send);
                     begin
                        Send_Action.Param := new String'(Param);
                        Action := new Action_Record'(Send_Action);
                     end;

                  when others =>
                     Action := new Action_Record (Action_Kind);

               end case;
            end if;
         else
            Is_Valid := False;
         end if;
         if Is_Valid then
            --  retrieve the eventual Expects
            Action.Expects := null;
            Action.Timeout := Default_Timeout_Record;
            Child_Node := Node.Child;
            while Child_Node /= null loop
               if To_Lower (Child_Node.Tag.all) = "expect" then
                  --  try to get the regexp from its id first
                  declare
                     Id : constant String
                       := Get_Attribute (Child_Node, "regexp_id");
                     Regexp_Str : constant String
                       := Get_Attribute (Child_Node, "regexp");
                  begin
                     if Id /= "" then
                        Regexp := Regexp_Root;
                        while Regexp /= null loop
                           if Regexp.Id.all = Id then
                              Trace (Full_Me, "found regexp " & Id);
                              exit;
                           end if;
                           Regexp := Regexp.Next;
                        end loop;
                        if Regexp = null then
                           Trace (Me, "*** Error: no regexp exist with the " &
                                  "id " & Id);
                           Is_Valid := False;
                        else
                           Expect.Regexp := Regexp.Regexp;
                        end if;
                     elsif Regexp_Str /= "" then
                        Expect.Regexp := +(Compile (Regexp_Str,
                            Case_Insensitive or Multiple_Lines));
                     else
                        Trace (Me, "** Error: Invalid expect tag: no regexp " &
                               "or regexp_id attribute is defined");
                        Is_Valid := False;
                     end if;

                  end;
                  --  init the other fields of the Expect
                  if Is_Valid then
                     Initialize_Action
                       (Child_Node,
                        Expect.Actions);
                     Expect.Next := Action.Expects;
                     Action.Expects := new Expect_Record'(Expect);
                  end if;
               elsif To_Lower (Child_Node.Tag.all) = "expect_timeout" then
                  declare
                     Value : constant String :=
                       Get_Attribute (Child_Node, "value");
                  begin
                     Action.Timeout.Timeout := Integer'Value (Value) * 1000;
                     Initialize_Action
                       (Child_Node,
                        Action.Timeout.Actions);
                  exception
                     when Constraint_Error =>
                        Trace (Me, "** ERROR: invalid expect_timeout tag");
                        Action.Timeout := Default_Timeout_Record;
                  end;
               else
                  Trace (Me,
                         "** Warning: unexpected field as child of action: " &
                         Child_Node.Tag.all);
               end if;
               Child_Node := Child_Node.Next;
            end loop;
         end if;
         --  add the new action at the end of the list
         if Is_Valid then
            if Action_Last = null then
               Action.Next := null;
               Actions := Action;
               Action_Last := Action;
            else
               Action.Next := null;
               Action_Last.Next := Action;
               Action_Last := Action;
            end if;
         end if;
         Node := Node.Next;
      end loop;
   end Initialize_Action;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Connection : access Custom_Connection'Class;
      Top        : Glib.Xml_Int.Node_Ptr)
   is
      Node        : Node_Ptr;
      Parent      : Node_Ptr;
      Tmp_Str     : String_Ptr;
      Cmd         : Action_Access;

      function Get_String (S : String_Ptr) return String_Ptr;
      --  Return a deep copy of S, or null if S is null.

      procedure Parse_Natural
        (Node    : in     Node_Ptr;
         Name    : in     String;
         Default : in     Natural;
         Result  :    out Natural);
      --  Parse a natural from field Name in a given Node.
      --  If the field cannot be found, set Result is set to Default

      ----------------
      -- Get_String --
      ----------------

      function Get_String (S : String_Ptr) return String_Ptr is
      begin
         if S = null then
            return null;
         else
            return new String'(S.all);
         end if;
      end Get_String;

      -------------------
      -- Parse_Natural --
      -------------------

      procedure Parse_Natural
        (Node    : in     Node_Ptr;
         Name    : in     String;
         Default : in     Natural;
         Result  :    out Natural)
      is
         Field : String_Ptr;
      begin
         Field := Get_Field (Node, Name);

         if Field /= null then
            Result := Natural'Value (Field.all);
         else
            Result := Default;
         end if;

      exception
         when Constraint_Error =>
            Result := Default;
      end Parse_Natural;

   begin
      Trace (Me, "Initialize");

      --  Insert connection in custom connections list
      Connection.Next := Custom_Root;
      Custom_Root     := Custom_Connection_Access (Connection);

      Connection.Name := new String'(Get_Attribute (Top, "name"));
      if Connection.Name.all = "" then
         Trace (Me, "** ERROR : remoteconnection item shall have an " &
                "attribute name");
         return;
      end if;
      Trace (Me, "Initialize name = " & Connection.Name.all);
      Connection.Description :=
        Get_String (Get_Field (Top, "description"));

      Connection.Password_Attempts := 0;
      Parse_Natural
        (Node    => Top,
         Name    => "max_password_attempts",
         Default => 2,
         Result  => Connection.Max_Password_Attempts);

      --  If Parent is defined, first set Commands to parent's ones
      Tmp_Str := Get_Field (Top, "parent");
      if Tmp_Str /= null then
         declare
            Tmp_Connect : Custom_Connection_Access := Custom_Root.Next;
            The_Name    : constant String := To_Lower (Tmp_Str.all);
         begin
            Trace (Me, "Initialize parent = " & Tmp_Str.all);
            while Tmp_Connect /= null loop
               if To_Lower (Tmp_Str.all) = The_Name then
                  Connection.Commands := Tmp_Connect.Commands;
                  exit;
               end if;

               Tmp_Connect := Tmp_Connect.Next;
            end loop;
         end;
      end if;

      --  Get Commands node
      Parent := Find_Tag (Top.Child, "commands");

      if Parent /= null then
         Node := Find_Tag (Parent.Child, "open_session_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Trace (Me, "Initialize : get open_session_cmd");
            Connection.Commands (Open_Session_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "read_file_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Trace (Me, "Initialize : get read_file_cmd");
            Connection.Commands (Read_File_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "is_regular_file_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Is_Regular_File_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "is_writable_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Is_Writable_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "is_directory_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Is_Directory_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "delete_file_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Delete_File_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "timestamp_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Timestamp_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "write_file_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Write_File_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "set_readable_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Set_Readable_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "set_writable_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Set_Writable_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "set_unreadable_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Set_Unreadable_Cmd) := Cmd;
         end if;

         Node := Find_Tag (Parent.Child, "set_unwritable_cmd");
         Initialize_Action (Node, Cmd);
         if Cmd /= null then
            Connection.Commands (Set_Unwritable_Cmd) := Cmd;
         end if;

      end if;

      Remote_Connections.Register_Protocol (Connection);
   end Initialize;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Connection : access Custom_Connection) return String is
   begin
      return Get_String (Connection.Name);
   end Get_Protocol;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Connection : access Custom_Connection) return String is
   begin
      Trace (Full_Me, "Get_Description");
      return Get_String (Connection.Description);
   end Get_Description;

   -----------
   -- Close --
   -----------

   procedure Close
     (Connection      : access Custom_Connection;
      GPS_Termination : Boolean) is
   begin
      if Connection.Is_Open and GPS_Termination then
         Close (Connection.Pd.all);
         Close
           (Remote_Connection_Record (Connection.all)'Access, GPS_Termination);
      end if;
      --  else :
      --  Do nothing, since we want to keep the connection open as long as
      --  possible
   end Close;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      Result : Return_Enum;
   begin
      Trace (Me, "Is_Regular_File " & Local_Full_Name);
      if Connection.Commands (Is_Regular_File_Cmd) = null then
         declare
            Base : constant String := Base_Name (Local_Full_Name);
         begin
            if Base (Base'First .. Base'First + 1) = ".#" and then
              Base (Base'Last) = '#' then
               --  checking tmp file. assume none exist
               return False;
            else
               --  Assume the file exists, we'll make sure when we try to
               --  fetch it
               return True;
            end if;
         end;
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Is_Regular_File_Cmd),
                                Local_Full_Name);

         return Result = OK;
      end if;
   end Is_Regular_File;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
   is
      Result : Return_Enum;
      pragma Unreferenced (Result);
   begin
      if Connection.Commands (Delete_File_Cmd) = null then
         --  Don't do anything
         null;
      else
         Trace (Me, "Delete " & Local_Full_Name);
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Delete_File_Cmd),
                                Local_Full_Name);
      end if;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      Result      : Return_Enum;
   begin
      Trace (Me, "Is_Writable " & Local_Full_Name);
      if Connection.Commands (Is_Writable_Cmd) = null then
         return Connection.Commands (Write_File_Cmd) /= null;
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Is_Writable_Cmd),
                                Local_Full_Name);
         return Result = OK;
      end if;
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      Result      : Return_Enum;
   begin
      Trace (Me, "Is_Directory " & Local_Full_Name);
      if Connection.Commands (Is_Directory_Cmd) = null then
         --  Assume it is a directory only if it ends with a directory
         --  separator
         return Local_Full_Name (Local_Full_Name'Last) = '/'
           or else Local_Full_Name (Local_Full_Name'Last) = '\';
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Is_Directory_Cmd),
                                Local_Full_Name);
         return Result = OK;
      end if;
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time
   is
      Result      : Return_Enum;
      pragma Unreferenced (Result);
   begin
      Trace (Me, "File_Time_Stamp " & Local_Full_Name);
      if Connection.Commands (Timestamp_Cmd) = null then
         return VFS.No_Time;
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Timestamp_Cmd),
                                Local_Full_Name);
         return Connection.Timestamp;
      end if;
   end File_Time_Stamp;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access
   is
      Result      : Return_Enum;
   begin
      Trace (Me, "Read_File " & Local_Full_Name);

      if Connection.Commands (Read_File_Cmd) = null then
         return null;
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Read_File_Cmd),
                                Local_Full_Name);
         if Result = OK then
            return Connection.Buffer;
         else
            return null;
         end if;
      end if;
   end Read_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String)
   is
      Result : Return_Enum;
      pragma Unreferenced (Result);
   begin
      Trace (Me, "Write " & Local_Full_Name & " - " & Temporary_File);
      if Connection.Commands (Write_File_Cmd) = null then
         return;
      else
         Result := Execute_Cmd (Connection,
                                Connection.Commands (Write_File_Cmd),
                                Local_Full_Name,
                                Temporary_File);
      end if;
   end Write;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean)
   is
      Result : Return_Enum;
      pragma Unreferenced (Result);
   begin
      Trace (Me,
             "Set_Writable " & Local_Full_Name &
             " " & Boolean'Image (Writable));
      if Connection.Commands (Set_Writable_Cmd) = null then
         --  Nothing to do
         null;

      else
         if Writable then
            Result := Execute_Cmd (Connection,
                                   Connection.Commands (Set_Writable_Cmd),
                                   Local_Full_Name);
         else
            Result := Execute_Cmd (Connection,
                                   Connection.Commands (Set_Unwritable_Cmd),
                                   Local_Full_Name);
         end if;
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean)
   is
      Result : Return_Enum;
      pragma Unreferenced (Result);
   begin
      Trace (Me, "Set_Readable " & Local_Full_Name &
            " (" & Boolean'Image (Readable) & ")");
      if Connection.Commands (Set_Readable_Cmd) = null then
         --  Nothing to do
         null;

      else
         if Readable then
            Result := Execute_Cmd (Connection,
                                   Connection.Commands (Set_Readable_Cmd),
                                   Local_Full_Name);
         else
            Result := Execute_Cmd (Connection,
                                   Connection.Commands (Set_Unreadable_Cmd),
                                   Local_Full_Name);
         end if;
      end if;
   end Set_Readable;

   -------------
   -- Factory --
   -------------

   function Factory
     (Protocol   : access Custom_Connection;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection
   is
      C : Custom_Connection_Access;
   begin
      if Reuse then
         return Remote_Connection (Protocol);
      else
         C := new Custom_Connection'(Protocol.all);
         C.Is_Open := False;
         Initialize (C, User, Host, Passwd);
         return Remote_Connection (C);
      end if;
   end Factory;

end Remote_Connections.Custom;

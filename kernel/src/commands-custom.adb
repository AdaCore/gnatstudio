------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Maps;          use Ada; use Ada.Strings.Maps;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Regpat;               use GNAT.Regpat;

with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with XML_Utils;                 use XML_Utils;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget;                use Gtk.Widget;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Interactive;    use GPS.Kernel.Interactive;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel.Preferences;
with Interactive_Consoles;      use Interactive_Consoles;
with Password_Manager;          use Password_Manager;
with Remote;                    use Remote;
with String_Utils;              use String_Utils;

package body Commands.Custom is

   Me : constant Trace_Handle := Create ("Commands.Custom", Off);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   -----------------------
   -- Custom components --
   -----------------------

   type Component_Type is (Component_Shell, Component_External);

   type Custom_Component_Record (The_Type : Component_Type) is
     new Command_Component_Record with
      record
         Show_Command : Boolean := True; --  "show-command" attribute
         Output       : String_Access;   --  use Default if this is null
         Command      : String_Access;

         case The_Type is
            when Component_Shell =>
               Script               : Scripting_Language;
            when Component_External =>
               Server               : Server_Type;      --  "server" attribute
               Check_Password       : Boolean := False; --  "check-password"
               Show_In_Task_Manager : Boolean;          --  "show-task-manager"
               Progress_Regexp      : String_Access;    --  "progress-regexp"
               Progress_Current     : Natural := 1;     --  "progress-current"
               Progress_Final       : Natural := 2;     --  "progress-final"
               Progress_Hide        : Boolean := True;  --  "progress-hide"
               --  Changes to this list must be reflected in the record
               --  Custom_Component_Editor_Record as well
         end case;
      end record;
   type Custom_Component is access all Custom_Component_Record'Class;

   procedure Free (Component : in out Custom_Component);
   --  Free the memory occupied by Component

   ----------------------------
   -- Custom commands editor --
   ----------------------------

   type Component_Type_And_Lang is record
      The_Type : Component_Type;
      Script   : Scripting_Language;
   end record;
   package Component_Type_And_Lang_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Component_Type_And_Lang);
   use Component_Type_And_Lang_Callback;

   ----------------------
   -- Misc subprograms --
   ----------------------

   procedure Check_Save_Output
     (Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array);
   --  Compute whether we should save the output of each commands. This depends
   --  on whether later commands reference this output through %1, %2,...

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class);
   --  Clear all existing consoles that Command will use. Consoles that do not
   --  exist yet are not created.
   --  This is used so that the new output isn't mix with the output of
   --  previous run.
   --  The default GPS console is never cleared.

   pragma Warnings (Off);
   --  These 2 UCs are safe aliasing-wise, so kill warning
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Custom_Command_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Custom_Command_Access, System.Address);
   pragma Warnings (On);

   procedure Free (Execution : in out Custom_Command_Execution);
   --  Free Execution and its contents

   function External_From_XML
     (Command                      : XML_Utils.Node_Ptr;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Command_Component;
   function Shell_From_XML
     (Kernel               : access Kernel_Handle_Record'Class;
      Command              : XML_Utils.Node_Ptr;
      Default_Show_Command : Boolean) return Command_Component;
   --  Create a command component from an XML node

   function From_XML
     (Kernel                       : access Kernel_Handle_Record'Class;
      Command                      : XML_Utils.Node_Ptr;
      Name                         : String;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Components_Array_Access;
   --  Create a list of components from an XML node

   function Output_Substitution
     (Command   : access Custom_Command'Class;
      Component : Integer;
      Ref       : Integer) return Integer;
   --  Return the index of the component to which a ${REF} in Component
   --  refers. For instance, if the 3rd component contains a $1, the result
   --  will be 2.
   --  This properly takes into account the on-failure components.
   --  Return -1 if no Ref is invalid.

   type Custom_Callback_Data is new External_Process_Data with record
      Command : Custom_Command_Access;
   end record;
   type Custom_Callback_Data_Access is access all Custom_Callback_Data'Class;
   overriding procedure On_Output
     (Self     : not null access Custom_Callback_Data;
      External : not null access Root_Command'Class;
      Output   : String);
   overriding procedure On_Exit
     (Self     : not null access Custom_Callback_Data;
      External : not null access Root_Command'Class);

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Command : XML_Utils.Node_Ptr) return Action_Filter
   is
      Filter : Macro_Filter;
      N      : Node_Ptr := Command;
   begin
      while N /= null loop
         if N.Value /= null then
            Filter := Create_Filter (Kernel, N.Value.all, Filter);
         end if;
         N := N.Next;
      end loop;

      return Action_Filter (Filter);
   end Create_Filter;

   -------------
   -- On_Exit --
   -------------

   overriding procedure On_Exit
     (Self     : not null access Custom_Callback_Data;
      External : not null access Root_Command'Class)
   is
      pragma Unreferenced (External);
   begin
      --  Unless the command has already terminated
      if Self.Command.Execution /= null then
         Self.Command.Execution.External_Process_In_Progress := False;
         Self.Command.Execution.Process_Exit_Status := Self.Exit_Status;
      end if;
   end On_Exit;

   ---------------
   -- On_Output --
   ---------------

   overriding procedure On_Output
     (Self     : not null access Custom_Callback_Data;
      External : not null access Root_Command'Class;
      Output   : String)
   is
      pragma Unreferenced (External);
      Command : constant Custom_Command_Access := Self.Command;
      Save_Output : Boolean;

      procedure Append (S : in out String_Access; Value : String);
      --  Append Value to S

      procedure Insert (Message : String);
      --  Insert Message in the current console

      ------------
      -- Insert --
      ------------

      procedure Insert (Message : String) is
         Console : constant Interactive_Console :=
                     Command.Execution.External_Process_Console;
      begin
         if Console /= null then
            Insert (Console, Message, Add_LF => False);
         end if;
      end Insert;

      ------------
      -- Append --
      ------------

      procedure Append (S : in out String_Access; Value : String) is
         Previous : GNAT.Strings.String_Access;
      begin
         if Value /= "" then
            if S = null then
               S := new String'(Value);
            else
               if S'Length >
                 GPS.Kernel.Preferences.Max_Output_Length.Get_Pref
               then
                  --  If we are collecting more output than we should, emit a
                  --  warning and stop collecting.
                  Save_Output := False;
                  Insert (Kernel => Command.Kernel,
                          Text   => (-"Output from command """)
                          & Command.Name.all
                          & (-""" exceeds the maximum length, truncating."),
                          Mode   => Error);
                  Command.Execution.Save_Output
                    (Command.Execution.Cmd_Index) := False;
               else
                  Previous := S;
                  S        := new String (1 .. Previous'Length + Value'Length);
                  S (1 .. Previous'Length) := Previous.all;
                  S (Previous'Length + 1 .. S'Last) := Value;
                  Free (Previous);
               end if;
            end if;
         end if;
      end Append;

      Current, Total : Integer := 0;
      Index, EOL     : Integer;

   begin
      --  Command might have been terminated already
      if Command.Execution = null then
         return;
      end if;

      Save_Output :=
        Command.Execution.Save_Output (Command.Execution.Cmd_Index);

      if Command.Execution.Check_Password then

         declare
            Matched : Match_Array (0 .. 2);
            Force   : Boolean;
            Idx     : Integer := Strings.Fixed.Index (Output, "" & ASCII.LF);
         begin
            --  Only the first received line is taken into account for custom
            --  commands.
            --  This allows custom tools to output text that matches the
            --  password regexp without reacting here.
            if Idx < Output'First then
               Idx := Output'Last;
            end if;

            --  Retrieve password prompt if any
            Match (Get_Default_Password_Regexp,
                   Output (Output'First .. Idx),
                   Matched);

            if Matched (0) /= No_Match then
               Force := Command.Execution.Nb_Password > 0;
               Command.Execution.Nb_Password :=
                 Command.Execution.Nb_Password + 1;

               declare
                  Password : constant String :=
                    Get_Tool_Password (Command.Name.all,
                                       Force);
               begin
                  if Password /= "" then
                     Send (Self.Descriptor.all, Password);

                     --  Do not output password prompt
                     return;

                  end if;
               end;

            end if;

            --  Retrieve passphrase prompt if any
            Match (Get_Default_Passphrase_Regexp,
                   Output (Output'First .. Idx),
                   Matched);

            if Matched (0) /= No_Match then
               Force := Command.Execution.Nb_Password > 0;
               Command.Execution.Nb_Password :=
                 Command.Execution.Nb_Password + 1;

               declare
                  Password : constant String :=
                    Get_Passphrase
                      (Output (Matched (1).First .. Matched (1).Last),
                       Force);
               begin
                  if Password /= "" then
                     Send (Self.Descriptor.all, Password);

                     --  Do not output password prompt
                     return;

                  end if;
               end;
            end if;
         end;

         --  If we received a line without matching any password regexp, and
         --  this line is not empty, then we won't look for password anymore.
         if Output'Length > 0 and then Output /= "" & ASCII.LF then
            Command.Execution.Check_Password := False;
         end if;
      end if;

      if Command.Execution.Progress_Matcher /= null then
         declare
            Matched : Match_Array
              (0 .. Integer'Max (Command.Execution.Current_In_Regexp,
                                 Command.Execution.Total_In_Regexp));
         begin
            Index := Output'First;
            while Index <= Output'Last loop
               EOL := Index;
               while EOL <= Output'Last
                 and then Output (EOL) /= ASCII.LF
               loop
                  EOL := EOL + 1;
               end loop;
               if EOL > Output'Last then
                  EOL := EOL - 1;
               end if;

               Match (Command.Execution.Progress_Matcher.all,
                      Output (Index .. EOL), Matched);
               if Matched (Command.Execution.Current_In_Regexp) = No_Match
                 or else Matched (Command.Execution.Total_In_Regexp) = No_Match
               then
                  Insert (Output (Index .. EOL));
                  if Save_Output then
                     Append
                       (Command.Execution.Current_Output,
                        Output (Index .. EOL));
                  end if;

               else
                  declare
                     Outp : constant String :=
                              Output (Index .. Matched (0).First - 1)
                                & Output (Matched (0).Last + 1 .. EOL);
                  begin
                     if Command.Execution.Hide_Progress then
                        Insert (Outp);
                     else
                        Insert (Output (Index .. EOL));
                     end if;

                     if Save_Output then
                        Append (Command.Execution.Current_Output, Outp);
                     end if;
                  end;

                  Current := Safe_Value
                    (Output
                       (Matched (Command.Execution.Current_In_Regexp).First ..
                          Matched (Command.Execution.Current_In_Regexp).Last));
                  Total := Safe_Value
                    (Output
                       (Matched (Command.Execution.Total_In_Regexp).First
                        .. Matched (Command.Execution.Total_In_Regexp).Last));
                  Set_Progress
                    (Command,
                     Progress_Record'
                       (Activity => Running,
                        Current  => Current,
                        Total    => Total));
               end if;
               Index := EOL + 1;
            end loop;
         end;

      elsif Save_Output then
         Insert (Output);
         Append (Command.Execution.Current_Output, Output);

      else
         Insert (Output);
      end if;
   end On_Output;

   --------------------
   -- Clear_Consoles --
   --------------------

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class)
   is
      procedure Clear_Console (Name : GNAT.Strings.String_Access);
      --  Clear a specific console

      -------------------
      -- Clear_Console --
      -------------------

      procedure Clear_Console (Name : GNAT.Strings.String_Access) is
         Console : Interactive_Console;
      begin
         if Name /= null
           and then Name.all /= No_Output
           and then Name.all /= Console_Output
         then
            Console := Create_Interactive_Console
              (Kernel, Name.all, Create_If_Not_Exist => False);
            if Console /= null then
               Clear (Console);
            end if;
         end if;
      end Clear_Console;

   begin
      Clear_Console (Command.Default_Output_Destination);
      for C in Command.Components'Range loop
         Clear_Console
           (Custom_Component (Command.Components (C).Component).Output);
      end loop;
   end Clear_Consoles;

   -------------------------
   -- Output_Substitution --
   -------------------------

   function Output_Substitution
     (Command   : access Custom_Command'Class;
      Component : Integer;
      Ref       : Integer) return Integer
   is
      Ref_Index : Integer := Component - 1;
      Sub_Index : Integer := Ref;
      Failure   : Integer := Command.Components (Component).On_Failure_For;
   begin
      if Ref > 0 then
         while Ref_Index >= Command.Components'First loop
            --  Handling of recursive on-failure blocks
            if Ref_Index = Failure then
               Failure := Command.Components (Ref_Index).On_Failure_For;
            end if;

            if Command.Components (Ref_Index).On_Failure_For = Failure then
               Sub_Index := Sub_Index - 1;
               exit when Sub_Index = 0;
            end if;

            Ref_Index := Ref_Index - 1;
         end loop;
      end if;

      if Ref_Index >= Command.Components'First then
         return Ref_Index;
      else
         return -1;
      end if;
   end Output_Substitution;

   -----------------------
   -- Check_Save_Output --
   -----------------------

   procedure Check_Save_Output
     (Command     : access Custom_Command'Class;
      Save_Output : out Boolean_Array)
   is
      Index : Natural;

      function Substitution (Param : String; Quoted : Boolean) return String;
      --  Check whether the command has a '%' + digit parameter

      ------------------
      -- Substitution --
      ------------------

      function Substitution (Param : String; Quoted : Boolean) return String is
         pragma Unreferenced (Quoted);
         Sub_Index, Ref_Index : Integer;
      begin
         Sub_Index := Safe_Value (Param, Default => 0);
         if Sub_Index /= 0 then
            Ref_Index := Output_Substitution
              (Command, Index, Sub_Index);

            if Ref_Index >= Command.Components'First then
               Save_Output (Ref_Index) := True;
            end if;
         end if;

         return "";
      end Substitution;

   begin
      Save_Output := (others => False);

      Index := Command.Components'First;
      while Index <= Command.Components'Last loop
         declare
            S : constant String := Substitute
              (Str               => Custom_Component
                 (Command.Components (Index).Component).Command.all,
               Delimiter         => GPS.Kernel.Macros.Special_Character,
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
            pragma Unreferenced (S);
         begin
            null;
         end;

         Index := Index + 1;
      end loop;
   end Check_Save_Output;

   ----------
   -- Free --
   ----------

   procedure Free (Execution : in out Custom_Command_Execution) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Command_Execution_Record, Custom_Command_Execution);
   begin
      if Execution /= null then
         Free              (Execution.Current_Output);
         GNAT.Strings.Free (Execution.Outputs);
         Unchecked_Free    (Execution.Save_Output);
         Unchecked_Free    (Execution.Progress_Matcher);
         Unchecked_Free    (Execution);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Component : in out Custom_Component) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Component_Record'Class, Custom_Component);
   begin
      Free (Component.Output);
      Free (Component.Command);
      case Component.The_Type is
         when Component_Shell =>
            null;
         when Component_External =>
            Free (Component.Progress_Regexp);
      end case;
      Unchecked_Free (Component);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Primitive_Free (X : in out Custom_Command) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Components_Array, Components_Array_Access);
   begin
      Free (X.Default_Output_Destination);
      Free (X.Name);
      Free (X.Execution);

      for C in X.Components'Range loop
         Free (Custom_Component (X.Components (C).Component));
      end loop;
      Unchecked_Free (X.Components);
   end Primitive_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Custom_Command_Access;
      Name    : String;
      Kernel  : Kernel_Handle;
      Command : String;
      Script  : Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Name   := new String'(Name);
      Item.Components := new Components_Array'
        (1 => (Component => new Custom_Component_Record'
                 (Command_Component_Record with
                  The_Type     => Component_Shell,
                  Show_Command => False,
                  Output       => new String'(No_Output),
                  Command      => new String'(Command),
                  Script       => Script),
               On_Failure_For => -1));
   end Create;

   --------------------
   -- Shell_From_XML --
   --------------------

   function Shell_From_XML
     (Kernel               : access Kernel_Handle_Record'Class;
      Command              : XML_Utils.Node_Ptr;
      Default_Show_Command : Boolean) return Command_Component
   is
      Output       : constant String :=
                       Get_Attribute (Command, "output", "@@");
      Script       : constant String :=
                       Get_Attribute (Command, "lang", GPS_Shell_Name);
      Show_Command : constant String :=
                       Get_Attribute (Command, "show-command");
      Show_C       : Boolean := Show_Command = "true";
      Outp         : GNAT.Strings.String_Access := null;

   begin
      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      if Show_Command = "" then
         Show_C := Default_Show_Command;
      end if;

      return new Custom_Component_Record'
        (Command_Component_Record with
         The_Type     => Component_Shell,
         Show_Command => Show_C,
         Output       => Outp,
         Command      => new String'(Command.Value.all),
         Script       => Kernel.Scripts.Lookup_Scripting_Language (Script));
   end Shell_From_XML;

   -----------------------
   -- External_From_XML --
   -----------------------

   function External_From_XML
     (Command                      : XML_Utils.Node_Ptr;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Command_Component
   is
      Output            : constant String :=
                            Get_Attribute (Command, "output", "@@");
      Show_Command      : constant String :=
                            Get_Attribute (Command, "show-command");
      Show_C            : Boolean := Show_Command = "true";
      Show_Task_Manager : constant String :=
                            Get_Attribute (Command, "show-task-manager");
      Show_TM           : Boolean := Show_Task_Manager = "false";
      Progress_Regexp   : constant String :=
                            Get_Attribute (Command, "progress-regexp", "");
      Progress_Current  : constant Integer :=
                            Safe_Value
                              (Get_Attribute
                                 (Command, "progress-current", "1"));
      Progress_Final    : constant Integer :=
                            Safe_Value
                              (Get_Attribute (Command, "progress-final", "2"));
      Progress_Hide     : constant Boolean :=
                            Get_Attribute
                              (Command, "progress-hide", "true") = "true";
      Server            : constant String :=
                            Get_Attribute (Command, "server", "gps_server");
      Check_Password    : constant Boolean :=
                            Get_Attribute
                              (Command, "check-password", "false") = "true";
      Outp              : GNAT.Strings.String_Access := null;
      Server_T          : Server_Type;
   begin
      if Show_Task_Manager = "" then
         Show_TM := Default_Show_In_Task_Manager or else Progress_Regexp /= "";
      end if;

      if Show_Command = "" then
         Show_C := Default_Show_Command;
      end if;

      if Output /= "@@" then
         Outp := new String'(Output);
      end if;

      begin
         Server_T := Server_Type'Value (Server);
      exception
         when Constraint_Error =>
            Server_T := GPS_Server;
      end;

      return new Custom_Component_Record'
        (Command_Component_Record with
         The_Type             => Component_External,
         Server               => Server_T,
         Check_Password       => Check_Password,
         Show_Command         => Show_C,
         Output               => Outp,
         Command              => new String'(Command.Value.all),
         Show_In_Task_Manager => Show_TM,
         Progress_Regexp      => new String'(Progress_Regexp),
         Progress_Current     => Progress_Current,
         Progress_Final       => Progress_Final,
         Progress_Hide        => Progress_Hide);
   end External_From_XML;

   --------------
   -- From_XML --
   --------------

   function From_XML
     (Kernel                       : access Kernel_Handle_Record'Class;
      Command                      : XML_Utils.Node_Ptr;
      Name                         : String;
      Default_Show_In_Task_Manager : Boolean;
      Default_Show_Command         : Boolean) return Components_Array_Access
   is
      N, M       : Node_Ptr := Command;
      Count      : Natural := 0;
      Result     : Components_Array_Access;
      On_Failure : Integer := -1;
   begin
      while N /= null loop
         if N.Tag.all = "shell"
           or else N.Tag.all = "external"
         then
            Count := Count + 1;

         elsif N.Tag.all = "on-failure" then
            if N.Value /= null
              and then N.Value.all /= ""
            then
               Insert (Kernel,
                       "<on-failure> can only contain <shell> and <external>"
                       & " tags, not a string, in definition of action """
                       & Name & """",
                       Mode => Error);
               return new Components_Array (1 .. 0);
            end if;

            M := N.Child;
            while M /= null loop
               if M.Tag.all = "shell"
                 or else M.Tag.all = "external"
               then
                  Count := Count + 1;

               elsif M.Tag.all = "on-failure" then
                  Insert (Kernel,
                          "Nested <on-failure> nodes not supported, in "
                          & "definition of action """ & Name & """",
                          Mode => Error);
                  return new Components_Array (1 .. 0);

               else
                  Insert (Kernel,
                          "Unknown tag in action definition: " & M.Tag.all
                          & " in definition of action """ & Name & """",
                          Mode => Error);
                  return new Components_Array (1 .. 0);
               end if;

               M := M.Next;
            end loop;

         elsif N.Tag.all = "filter"
           or else N.Tag.all = "filter_and"
           or else N.Tag.all = "filter_or"
           or else N.Tag.all = "description"
         then
            --  Taken care of in custom_module.adb
            null;

         else
            Insert (Kernel,
                    "Unknown tag " & N.Tag.all & " in definition of action """
                    & Name & """",
                    Mode => Error);
            return new Components_Array (1 .. 0);
         end if;
         N := N.Next;
      end loop;

      Result := new Components_Array (1 .. Count);
      Count := Result'First;
      N := Command;

      while N /= null loop
         if N.Tag.all = "shell" then
            Result (Count) :=
              (Shell_From_XML (Kernel, N, Default_Show_Command), -1);
            Count := Count + 1;

         elsif N.Tag.all = "external" then
            Result (Count) := (External_From_XML
              (N,
               Default_Show_In_Task_Manager => Default_Show_In_Task_Manager,
               Default_Show_Command         => Default_Show_Command),
              -1);
            Count := Count + 1;

         elsif N.Tag.all = "on-failure" then
            if Count = Result'First
              or else Custom_Component (Result (Count - 1).Component).The_Type
                /= Component_External
            then
               Insert (Kernel,
                       "<on-failure> can only follow an <external> node, in"
                       & " definition of action """ & Name & """",
                       Mode => Error);
               return new Components_Array (1 .. 0);
            end if;

            On_Failure := Count - 1;

            M := N.Child;
            while M /= null loop
               if M.Tag.all = "shell" then
                  Result (Count) :=
                    (Shell_From_XML (Kernel, M, Default_Show_Command),
                     On_Failure);
                  Count := Count + 1;

               elsif M.Tag.all = "external" then
                  Result (Count) := (External_From_XML
                    (M,
                       Default_Show_In_Task_Manager =>
                         Default_Show_In_Task_Manager,
                       Default_Show_Command         => Default_Show_Command),
                    On_Failure);
                  Count := Count + 1;
               end if;

               M := M.Next;
            end loop;
         end if;

         N := N.Next;
      end loop;
      return Result;
   end From_XML;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item                 : out Custom_Command_Access;
      Name                 : String;
      Kernel               : Kernel_Handle;
      Command              : XML_Utils.Node_Ptr;
      Default_Output       : String := Console_Output;
      Show_Command         : Boolean := True;
      Show_In_Task_Manager : Boolean := False) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Default_Output_Destination := new String'(Default_Output);
      Item.Default_Show_Command := Show_Command;
      Item.Name := new String'(Name);
      Item.Components := From_XML
        (Kernel, Command, Name,
         Default_Show_In_Task_Manager => Show_In_Task_Manager,
         Default_Show_Command         => Show_Command);
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Custom_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Success        : Boolean := True;
      Current_Server : Server_Type := GPS_Server;
      Old_Server     : Server_Type := GPS_Server;

      function Dollar_Substitution
        (Param : String;
         Mode  : Command_Line_Mode) return Arg_List;
      --  Substitution function for the "$1" .. "$N", "$*", "$@" parameters

      function Substitution
        (Param  : String;
         Mode   : Command_Line_Mode) return Arg_List;
      --  Substitution function for '%1', '%2',...

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean;
      --  Execute a single command, and return whether it succeeded

      function Terminate_Command return Command_Return_Type;
      --  Terminate the command and free associated memory

      function Execute_Next_Command return Boolean;
      --  Execute the following commands, until the next external one.
      --  Return True if there are still commands to be executed after that
      --  one.

      -------------------------
      -- Dollar_Substitution --
      -------------------------

      function Dollar_Substitution
        (Param : String;
         Mode  : Command_Line_Mode) return Arg_List
      is
         Is_Num    : Boolean;
         Result    : Natural;
         Multiple  : Boolean := False;
         First_Arg : Natural := 1;

         CL        : Arg_List;
         pragma Unreferenced (Mode);
      begin
         if Param = "repeat" then
            return Create (Integer'Image (Context.Repeat_Count));
         elsif Param = "remaining" then
            return Create (Integer'Image (Context.Remaining_Repeat));
         end if;

         if Context.Args = null then
            return Empty_Command_Line;
         end if;

         if Param = "*" then
            Multiple := True;

         elsif Param (Param'Last) = '-' then
            Multiple := True;

            First_Arg := Safe_Value
              (Param (Param'First .. Param'Last - 1), Param'Length + 1);
         end if;

         if Multiple then
            for J in
              Context.Args'First - 1 + First_Arg .. Context.Args'Last
            loop
               if Context.Args (J) /= null then
                  Append_Argument (CL, Context.Args (J).all, Expandable);
               end if;
            end loop;

            return CL;
         else
            Is_Num := True;

            for J in Param'Range loop
               if not Is_Decimal_Digit (Param (J)) then
                  Is_Num := False;
                  exit;
               end if;
            end loop;

            if Is_Num then
               Result := Natural'Value (Param);

               if Result in Context.Args'Range
                 and then Context.Args (Result) /= null
               then
                  return Create (Context.Args (Result).all);
               end if;
            end if;
         end if;

         return Empty_Command_Line;
      end Dollar_Substitution;

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param  : String;
         Mode   : Command_Line_Mode) return Arg_List
      is
         Num   : Integer;
         Done  : aliased Boolean := False;
         Macro : constant String :=
                   Substitute
                     (Param, Command.Execution.Context,
                      False, Done'Access, Current_Server,
                      For_Shell => False);
      begin
         if Done then
            if Macro = "" then
               return Empty_Command_Line;
            else
               return Create (Macro);
            end if;
         end if;

         Num := Safe_Value (Param, Default => 0);

         declare
            Output_Index : constant Integer := Output_Substitution
              (Command, Command.Execution.Cmd_Index, Num);
            Output       : GNAT.Strings.String_Access;
         begin
            if Output_Index = -1 then
               return Empty_Command_Line;
            end if;

            Output := Command.Execution.Outputs (Output_Index);

            if Output = null then
               --  Mark this as invalid, so that the default behavior applies:
               --  depending on the parameters passed to Substitute, the
               --  original substring will either be kept or replaced by an
               --  empty string. This is also needed for proper handling of
               --  "%%" in the substring (returning an empty string here would
               --  not work).
               raise Invalid_Substitution;

            else
               return Parse_String (Output.all, Mode);
            end if;
         end;
      end Substitution;

      ----------------------------
      -- Execute_Simple_Command --
      ----------------------------

      function Execute_Simple_Command
        (Component : Custom_Component) return Boolean
      is
         --  Perform arguments substitutions for the command

         The_Command_Line : Arg_List;
         --  Treatment of the command line is the following:
         --  - first we convert the raw command line string to a command line
         --      (done with the call to parse_string above)
         --  - then we process the command line to process '$' substitution
         --  - then we process the command line to process '%' substitution
         --  - we re-convert the command line to a string at the last moment,
         --      when we pass the command to the script.

         Console         : Interactive_Console;
         Output_Location : GNAT.Strings.String_Access;

         function Execute_Shell
           (Component : Custom_Component_Record'Class) return Boolean;
         function Execute_External
           (Component : Custom_Component_Record'Class) return Boolean;
         --  Execute a shell or an external component. Return the success
         --  status.

         -------------------
         -- Execute_Shell --
         -------------------

         function Execute_Shell
           (Component : Custom_Component_Record'Class) return Boolean
         is
            Errors    : aliased Boolean;
            Old_Dir   : Virtual_File;
            --  has to be determined here so that Current_Server is
            --  correctly set:
            Treatment : GNATCOLL.Arg_Lists.Command_Line_Mode;
         begin
            if Command_Line_Treatment (Component.Script) = Raw_String then
               Treatment := Raw_String;
            else
               Treatment := Separate_Args;
            end if;

            The_Command_Line := Parse_String
              (Component.Command.all, Treatment);

            --  Implement $-substitution
            Substitute
              (The_Command_Line, '$', Dollar_Substitution'Unrestricted_Access);

            --  Implement %-substitution

            Substitute
              (The_Command_Line,
               GPS.Kernel.Macros.Special_Character,
               Substitution'Unrestricted_Access);

            if Context.Dir /= No_File then
               Old_Dir := Get_Current_Dir;
               Change_Dir (Context.Dir);
            end if;

            if Component.Script /= null then
               --  Output the command whether or not we are saving its output
               if Console /= null and then Component.Show_Command then
                  Insert
                    (Console,
                     To_Display_String (The_Command_Line),
                     Add_LF => True);
               end if;

               if Command.Execution.Save_Output
                 (Command.Execution.Cmd_Index)
               then
                  Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
                    new String'
                      (Execute_Command
                           (Component.Script,
                            The_Command_Line,
                            Hide_Output  => Output_Location.all = No_Output,
                            Show_Command => Component.Show_Command,
                            Console      =>
                              Get_Or_Create_Virtual_Console (Console),
                            Errors       => Errors'Unchecked_Access));

               else
                  Execute_Command
                    (Component.Script, The_Command_Line,
                     Hide_Output  => Output_Location.all = No_Output,
                     Show_Command => Component.Show_Command,
                     Console      => Get_Or_Create_Virtual_Console (Console),
                     Errors       => Errors);
               end if;
            end if;

            if Context.Dir /= No_File then
               Change_Dir (Old_Dir);
            end if;

            return not Errors;
         end Execute_Shell;

         ----------------------
         -- Execute_External --
         ----------------------

         function Execute_External
           (Component : Custom_Component_Record'Class) return Boolean
         is
            Data : Custom_Callback_Data_Access;
         begin
            The_Command_Line := Parse_String
              (Trim
                 (Component.Command.all,
                  Left  => To_Set (' ' & ASCII.LF & ASCII.HT),
                  Right => Strings.Maps.Null_Set),
               Separate_Args);

            --  Implement $-substitution
            Substitute
              (The_Command_Line, '$', Dollar_Substitution'Unrestricted_Access);

            --  Implement %-substitution

            Substitute
              (The_Command_Line,
               GPS.Kernel.Macros.Special_Character,
               Substitution'Unrestricted_Access);

            Trace (Me, "Executing external command " & Component.Command.all);

            Free (Command.Execution.Current_Output);
            Command.Execution.Current_Output := new String'("");

            Unchecked_Free (Command.Execution.Progress_Matcher);
            Command.Execution.Current_In_Regexp :=
              Integer'Max (0, Component.Progress_Current);
            Command.Execution.Total_In_Regexp   :=
              Integer'Max (0, Component.Progress_Final);
            Command.Execution.Hide_Progress     := Component.Progress_Hide;
            Command.Execution.Check_Password    := Component.Check_Password;
            Command.Execution.Nb_Password       := 0;

            if Component.Progress_Regexp.all /= "" then
               Command.Execution.Progress_Matcher := new Pattern_Matcher'
                 (Compile (Component.Progress_Regexp.all,
                           Multiple_Lines or Single_Line));
            end if;

            if The_Command_Line = Empty_Command_Line then
               Trace (Me, "Cannot launch empty command");
               Success := False;
               Command.Execution.External_Process_In_Progress := False;
               Command.Execution.Process_Exit_Status := 1;

            else
               --  Set the console before launching the process, so that
               --  synchronous calls correctly output.

               Command.Execution.External_Process_Console := Console;

               Data := new Custom_Callback_Data'
                 (External_Process_Data with
                  Command => Custom_Command_Access (Command));

               Launch_Process
                 (Kernel               => Command.Kernel,
                  CL                   => The_Command_Line,
                  Server               => Component.Server,
                  Console              => Console,
                  Success              => Success,
                  Show_Command         => Component.Show_Command,

                  --  Showing the output is already handled by the callback
                  --  Store_Command_Output.

                  Show_Output          => False,

                  Data                 => Data,
                  Show_In_Task_Manager => Component.Show_In_Task_Manager,
                  Line_By_Line         => False,
                  Synchronous          => Context.Synchronous,
                  Directory            => To_Remote
                    (Context.Dir, Get_Nickname (Component.Server)),
                  Scheduled            => Command.Sub_Command);

               Command.Execution.External_Process_In_Progress := Success;
            end if;

            return Success;
         end Execute_External;

      begin
         if Component.Output = null then
            Output_Location := Command.Default_Output_Destination;
         else
            Output_Location := Component.Output;
         end if;

         if Success and then Output_Location.all /= No_Output then
            Console := Create_Interactive_Console
              (Command.Kernel, Output_Location.all);
         end if;

         --  If substitution failed
         --  ??? Should write message to console
         if not Success then
            return False;
         end if;

         case Component.The_Type is
            when Component_Shell =>
               Success := Execute_Shell (Component.all);

            when Component_External =>
               Old_Server     := Current_Server;
               Current_Server := Component.Server;
               Success        := Execute_External (Component.all);
               Current_Server := Old_Server;
         end case;

         if not Success then
            Trace
              (Me, "Execute_Simple_Command => Command returned with error");
         end if;

         return Success;

      exception
         when E : others =>
            Trace (Me, E);
            Insert (Command.Kernel,
                    -("An unexpected error occurred while executing the custom"
                      & " command. See the log file for more information."),
                    Mode => Error);
            return False;
      end Execute_Simple_Command;

      --------------------------
      -- Execute_Next_Command --
      --------------------------

      function Execute_Next_Command return Boolean is
         Current : Command_Component_Description;
      begin
         while Success
           and then Command.Execution.Cmd_Index <= Command.Components'Last
         loop
            Current := Command.Components (Command.Execution.Cmd_Index);
            if Current.On_Failure_For = Command.Execution.Current_Failure then
               Success := Execute_Simple_Command
                 (Custom_Component (Current.Component));

               if not Context.Synchronous
                 and then Custom_Component (Current.Component).The_Type =
                   Component_External
               then
                  --  We'll have to run again to check for completion
                  return True;
               end if;
            end if;

            --  In case the command has terminated early
            exit when Command.Execution = null;

            if Context.Synchronous
              and then
                Custom_Component (Current.Component).The_Type =
                Component_External
            then
               Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
                 Command.Execution.Current_Output;
               Command.Execution.Current_Output := null;
            end if;

            Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;
         end loop;

         --  No more command to execute
         return False;
      end Execute_Next_Command;

      -----------------------
      -- Terminate_Command --
      -----------------------

      function Terminate_Command return Command_Return_Type is
      begin
         Free (Command.Execution);

         Command_Finished_Status (Command, Success);

         if Success then
            return Commands.Success;
         else
            return Failure;
         end if;
      end Terminate_Command;

      Old_Dir : Virtual_File;

   begin  --  Execute
      --  If there was an external command executing:
      if Command.Execution /= null then
         if Command.Execution.External_Process_In_Progress then
            return Execute_Again;
         end if;

         --  Save current output

         Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
           Command.Execution.Current_Output;
         Command.Execution.Current_Output := null;

         if Command.Execution.Process_Exit_Status /= 0 then
            Command.Execution.Current_Failure := Command.Execution.Cmd_Index;
            Command.Execution.Process_Exit_Status := 0;
         end if;

         --  And pass to the next command

         Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;

      else
         if Context.Dir /= No_File then
            Old_Dir := Get_Current_Dir;
            begin
               Change_Dir (Context.Dir);
            exception
               when Directory_Error =>
                  return Terminate_Command;
            end;
         end if;

         Command.Execution := new Custom_Command_Execution_Record;
         Command.Execution.Outputs     :=
           new GNAT.Strings.String_List (Command.Components'Range);
         Command.Execution.Save_Output :=
           new Boolean_Array (Command.Components'Range);

         if Context.Context = No_Context then
            Command.Execution.Context := Get_Current_Context (Command.Kernel);
         else
            Command.Execution.Context := Context.Context;
         end if;

         Command.Execution.Cmd_Index  := Command.Components'First;

         Check_Save_Output (Command, Command.Execution.Save_Output.all);
         Clear_Consoles (Command.Kernel, Command);

         if Context.Dir /= No_File then
            Change_Dir (Old_Dir);
         end if;

         if not Success then
            return Terminate_Command;
         end if;
      end if;

      if Execute_Next_Command then
         return Execute_Again;
      else
         return Terminate_Command;
      end if;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name (Command : access Custom_Command) return String is
   begin
      return Command.Name.all;
   end Name;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt (Command : in out Custom_Command) is
   begin
      if Command.Execution /= null then
         Command.Execution.Cmd_Index := Command.Components'First;
         Interrupt_Queue (Command.Kernel, Command.Sub_Command);
      end if;
   end Interrupt;

end Commands.Custom;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Timeout; use Glide_Kernel.Timeout;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Glide_Intl;           use Glide_Intl;

with Glib.Xml_Int;         use Glib.Xml_Int;
with Gtkada.MDI;           use Gtkada.MDI;

with Basic_Types;          use Basic_Types;
with Projects;             use Projects;
with Projects.Registry;    use Projects.Registry;
with String_Utils;         use String_Utils;
with VFS;                  use VFS;
with Interactive_Consoles; use Interactive_Consoles;

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Traces;               use Traces;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with GNAT.Regpat;          use GNAT.Regpat;
with System;

package body Commands.Custom is

   Me : constant Debug_Handle := Create ("Commands.Custom");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   function Commands_Count
     (Command : access Custom_Command'Class) return Natural;
   --  Return the number of commands that will be executed as part of Command.

   procedure Check_Save_Output
     (Kernel           : access Kernel_Handle_Record'Class;
      Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array;
      Context          : Selection_Context_Access;
      Context_Is_Valid : out Boolean);
   --  Compute whether we should save the output of each commands. This depends
   --  on whether later commands reference this output through %1, %2,...
   --  This also checks that the context contains information for possible
   --  %p, %f,.. parameters (Set Context_Is_Valid otherwise).

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class);
   --  Clear all existing consoles that Command will use. Consoles that do not
   --  exist yet are not created.
   --  This is used so that the new output isn't mix with the output of
   --  previous run.
   --  The default GPS console is never cleared.

   function Project_From_Param
     (Param : String; Context : Selection_Context_Access) return Project_Type;
   --  Return the project from the parameter. Parameter is the string
   --  following the '%' sign. No_Project is returned if the context doesn't
   --  contain this information

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Custom_Command_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Custom_Command_Access, System.Address);

   procedure Exit_Cb (Data : Process_Data; Status : Integer);
   --  Called when an external process has finished running

   procedure Store_Command_Output (Data : Process_Data; Output : String);
   --  Store the output of the current command

   type Parameters_Filter_Record is new Action_Filter_Record with record
      Need_File, Need_Directory : Boolean := False;
      Need_Project : Character := ' ';
   end record;
   type Parameters_Filter is access all Parameters_Filter_Record'Class;
   --  Check that the current context contains enough information to satisfy
   --  the requirements for a custom command.
   --  Need_Project is 'p' is a current project is needed, 'P' is a root
   --  project is needed, different from the default project loaded by GPS at
   --  startup, and any other character if no project is needed.

   function Filter_Matches_Primitive
     (Filter  : access Parameters_Filter_Record;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean;
   --  See doc for inherited subprogram.

   procedure Free (Execution : in out Custom_Command_Execution);
   --  Free Execution and its contents

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Parameters_Filter_Record;
      Context : Selection_Context_Access;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean
   is
      Project : Project_Type;
   begin
      if Filter.Need_Project = 'p'
        or else Filter.Need_Project = 'P'
      then
         Project := Project_From_Param (Filter.Need_Project & ' ', Context);
         if Project = No_Project then
            Insert (Kernel, -"No project specified", Mode => Error);
            return False;
         end if;
      end if;

      if Filter.Need_File then
         if Context = null
           or else Context.all not in File_Selection_Context'Class
           or else not Has_File_Information
             (File_Selection_Context_Access (Context))
         then
            Insert (Kernel, -"No file specified", Mode => Error);
            return False;
         end if;
      end if;

      if Filter.Need_Directory then
         if Context = null
           or else Context.all not in File_Selection_Context'Class
           or else not Has_Directory_Information
             (File_Selection_Context_Access (Context))
         then
            Insert (Kernel, -"No directory specified", Mode => Error);
            return False;
         end if;
      end if;

      return True;
   end Filter_Matches_Primitive;

   -------------------
   -- Create_Filter --
   -------------------

   function Create_Filter
     (Command : Glib.Xml_Int.Node_Ptr) return Action_Filter
   is
      Filter : Parameters_Filter;

      function Substitution (Param : String) return String;
      --  Check whether the command has a '%' + digit parameter

      function Substitution (Param : String) return String is
      begin
         if Param = "f" or else Param = "F" then
            if Filter = null then
               Filter := new Parameters_Filter_Record;
            end if;
            Filter.Need_File := True;

         elsif Param = "d" then
            if Filter = null then
               Filter := new Parameters_Filter_Record;
            end if;
            Filter.Need_Directory := True;

         elsif Param (Param'First) = 'p' or else Param (Param'First) = 'P' then
            if Param /= "pps" and then Param /= "PPs" then
               if Filter = null then
                  Filter := new Parameters_Filter_Record;
               end if;
               Filter.Need_Project := Param (Param'First);
            end if;
         end if;

         return "";
      end Substitution;

      N : Node_Ptr := Command;
   begin
      while N /= null loop
         declare
            Tmp : constant String := Substitute
              (N.Value.all,
               Substitution_Char => '%',
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
            pragma Unreferenced (Tmp);
         begin
            null;
         end;

         N := N.Next;
      end loop;

      return Action_Filter (Filter);
   end Create_Filter;

   -------------
   -- Exit_Cb --
   -------------

   procedure Exit_Cb (Data : Process_Data; Status : Integer) is
      Command : Custom_Command_Access := Convert (Data.Callback_Data);
   begin
      Command.Execution.External_Process_In_Progress := False;
      Command.Execution.Process_Exit_Status := Status;
   end Exit_Cb;

   --------------------------
   -- Store_Command_Output --
   --------------------------

   procedure Store_Command_Output (Data : Process_Data; Output : String) is
      Command : Custom_Command_Access := Convert (Data.Callback_Data);

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
            Highlight_Child
              (Find_MDI_Child (Get_MDI (Command.Kernel), Console));
         end if;
      end Insert;

      Old : GNAT.OS_Lib.String_Access := Command.Execution.Current_Output;
      Current, Total : Integer;
      Save_Output : constant Boolean :=
        Command.Execution.Save_Output (Command.Execution.Cmd_Index);
   begin
      if Command.Execution.Progress_Matcher /= null then
         declare
            Matched : Match_Array
              (0 .. Integer'Max (Command.Execution.Current_In_Regexp,
                                 Command.Execution.Total_In_Regexp));
         begin
            Match (Command.Execution.Progress_Matcher.all, Output, Matched);
            if Matched (Command.Execution.Current_In_Regexp) = No_Match
              or else Matched (Command.Execution.Total_In_Regexp) = No_Match
            then
               Insert (Output);
               if Save_Output then
                  Command.Execution.Current_Output :=
                    new String'(Old.all & Output);
                  Free (Old);
               end if;
            else
               if Matched (0).Last < Output'Last then
                  declare
                     Outp : constant String :=
                       Output (Output'First .. Matched (0).First - 1)
                     & Output (Matched (0).Last + 1 .. Output'Last);
                  begin
                     if Command.Execution.Hide_Progress then
                        Insert (Outp);
                     else
                        Insert (Output);
                     end if;

                     if Save_Output then
                        Command.Execution.Current_Output := new String'
                          (Old.all & Outp);
                        Free (Old);
                     end if;
                  end;

               else
                  if Command.Execution.Hide_Progress then
                     Insert (Output (Output'First .. Matched (0).First - 1));
                  else
                     Insert (Output);
                  end if;

                  if Save_Output then
                     Command.Execution.Current_Output := new String'
                       (Old.all
                        & Output (Output'First .. Matched (0).First - 1));
                     Free (Old);
                  end if;
               end if;

               Current := Safe_Value
                 (Output
                    (Matched (Command.Execution.Current_In_Regexp).First
                     .. Matched (Command.Execution.Current_In_Regexp).Last));
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
         end;

      elsif Save_Output then
         Insert (Output);
         Command.Execution.Current_Output := new String'(Old.all & Output);
         Free (Old);
      else
         Insert (Output);
      end if;

   end Store_Command_Output;

   --------------------
   -- Clear_Consoles --
   --------------------

   procedure Clear_Consoles
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : access Custom_Command'Class)
   is
      N       : Node_Ptr;
      Console : Interactive_Console;
   begin
      if Command.Command = null then
         N := Command.XML;
         while N /= null loop
            declare
               Console_Name : constant String :=
                 Get_Attribute (N, "output", Console_Output);
            begin
               if Console_Name /= No_Output
                 and then Console_Name /= Console_Output
               then
                  Console := Create_Interactive_Console
                    (Kernel,
                     Console_Name,
                     Create_If_Not_Exist => False);

                  if Console /= null then
                     Clear (Console);
                  end if;
               end if;
            end;

            N := N.Next;
         end loop;
      end if;
   end Clear_Consoles;

   --------------------
   -- Commands_Count --
   --------------------

   function Commands_Count
     (Command : access Custom_Command'Class) return Natural
   is
      N : Node_Ptr;
      Count : Natural := 0;
   begin
      if Command.Command /= null then
         return 1;
      else
         N := Command.XML;
         while N /= null loop
            Count := Count + 1;
            N := N.Next;
         end loop;
         return Count;
      end if;
   end Commands_Count;

   -----------------------
   -- Check_Save_Output --
   -----------------------

   procedure Check_Save_Output
     (Kernel           : access Kernel_Handle_Record'Class;
      Command          : access Custom_Command'Class;
      Save_Output      : out Boolean_Array;
      Context          : Selection_Context_Access;
      Context_Is_Valid : out Boolean)
   is
      N     : Node_Ptr;
      Index : Natural := 1;

      function Substitution (Param : String) return String;
      --  Check whether the command has a '%' + digit parameter

      function Substitution (Param : String) return String is
         Sub_Index : Natural;
      begin
         if Param = "f" or else Param = "F" then
            if Context = null
              or else Context.all not in File_Selection_Context'Class
              or else not Has_File_Information
                (File_Selection_Context_Access (Context))
            then
               Context_Is_Valid := False;
               Insert (Kernel,
                       -"Command not executed: file required",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param = "d" then
            if Context = null
              or else Context.all not in File_Selection_Context'Class
              or else not Has_Directory_Information
                (File_Selection_Context_Access (Context))
            then
               Context_Is_Valid := False;
               Insert (Kernel,
                       -"Command not executed: directory required",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param (Param'First) = 'p' or else Param (Param'First) = 'P' then
            if Param /= "pps" and then Param /= "PPs" then
               if Project_From_Param (Param, Context) = No_Project then
                  Context_Is_Valid := False;
                  Insert (Kernel,
                            -"Command not executed: project required",
                          Mode => Error);
                  raise Invalid_Substitution;
               end if;
            end if;

         else
            Sub_Index := Safe_Value (Param, Default => 0);
            if Sub_Index <= Index - 1
              and then Sub_Index >= 1
            then
               Save_Output (Index - Sub_Index) := True;
            end if;
         end if;

         return "";
      end Substitution;

   begin
      Context_Is_Valid := True;
      Save_Output := (others => False);

      if Command.XML /= null then
         N := Command.XML;
         while N /= null and then Context_Is_Valid loop
            declare
               Tmp : constant String := Substitute
                 (N.Value.all,
                  Substitution_Char => '%',
                  Callback          => Substitution'Unrestricted_Access,
                  Recursive         => False);
               pragma Unreferenced (Tmp);
            begin
               null;
            end;

            Index := Index + 1;

            --  Check the "on-failure" attribute of <external> commands
            if N.Tag.all = "external" then
               declare
                  Fail : constant String := Get_Attribute
                    (N, "on-failure");
               begin
                  if Fail /= "" then
                     declare
                        Tmp : constant String := Substitute
                          (Fail,
                           Substitution_Char => '%',
                           Callback        => Substitution'Unrestricted_Access,
                           Recursive         => False);
                        pragma Unreferenced (Tmp);
                     begin
                        null;
                     end;
                  end if;
               end;
            end if;

            N     := N.Next;
         end loop;
      end if;
   end Check_Save_Output;

   ----------
   -- Free --
   ----------

   procedure Free (Execution : in out Custom_Command_Execution) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Custom_Command_Execution_Record, Custom_Command_Execution);
   begin
      if Execution /= null then
         Free             (Execution.Current_Output);
         GNAT.OS_Lib.Free (Execution.Outputs);
         Unchecked_Free   (Execution.Save_Output);
         Unchecked_Free   (Execution.Progress_Matcher);
         Unref            (Execution.Context);
         Unchecked_Free (Execution);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Command) is
   begin
      Free (X.Command);
      Free (X.XML);
      Free (X.Default_Output_Destination);
      Free (X.Name);
      Free (X.Execution);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Name         : String;
      Kernel       : Kernel_Handle;
      Command      : String;
      Script       : Glide_Kernel.Scripts.Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Command := new String'(Command);
      Item.Script := Script;
      Item.Name   := new String'(Name);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Custom_Command_Access;
      Name           : String;
      Kernel         : Kernel_Handle;
      Command        : Glib.Xml_Int.Node_Ptr;
      Default_Output : String := Console_Output;
      Show_Command   : Boolean := True)
   is
      Node, Previous : Node_Ptr;
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Default_Output_Destination := new String'(Default_Output);
      Item.Default_Show_Command := Show_Command;
      Item.Name   := new String'(Name);

      --  Make a deep copy of the relevant nodes
      Node := Command;
      while Node /= null loop
         if Node.Tag.all = "shell"
           or else Node.Tag.all = "external"
         then
            if Previous = null then
               Item.XML := Deep_Copy (Node);
               Previous := Item.XML;
            else
               Previous.Next := Deep_Copy (Node);
               Previous := Previous.Next;
            end if;
         end if;

         Node := Node.Next;
      end loop;
   end Create;

   ------------------------
   -- Project_From_Param --
   ------------------------

   function Project_From_Param
     (Param   : String;
      Context : Selection_Context_Access) return Project_Type
   is
      File : File_Selection_Context_Access;
      Project : Project_Type := No_Project;
   begin
      if Param (Param'First) = 'P' then
         Project := Get_Project (Get_Kernel (Context));

      elsif Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_Project_Information
          (File_Selection_Context_Access (Context))
      then
         File := File_Selection_Context_Access (Context);
         Project := Project_Information (File);

      elsif Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information
          (File_Selection_Context_Access (Context))
      then
         --  Since the editor doesn't provide the project, we emulate it
         --  here
         Project := Get_Project_From_File
           (Project_Registry (Get_Registry (Get_Kernel (Context))),
            File_Information (File_Selection_Context_Access (Context)),
            Root_If_Not_Found => False);
      end if;

      return Project;
   end Project_From_Param;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command       : access Custom_Command;
      Event         : Gdk.Event.Gdk_Event) return Command_Return_Type
   is
      pragma Unreferenced (Event);

      Success  : Boolean := True;

      function Substitution (Param : String) return String;
      --  Substitution function for the various '%...' parameters
      --  Index is the number of the current command we are executing

      function Execute_Simple_Command
        (Script          : Scripting_Language;
         Command_Line    : String;
         Output_Location : String := No_Output;
         Show_Command    : Boolean := True;
         Progress_Regexp : String := "";
         Current_In_Regexp : Integer := -1;
         Total_In_Regexp   : Integer := -1;
         Hide_Progress     : Boolean := True) return Boolean;
      --  Execute a single command, and return whether it succeeded.
      --  Index is the number of the current command we are executing.
      --  Output_Location is the console where the output of the command should
      --  be displayed:
      --    "none"  => not displayed
      --    ""      => Messages window
      --    others  => A process-specific console.

      function Terminate_Command return Command_Return_Type;
      --  Terminate the command and free associated memory

      function Execute_Next_Command return Boolean;
      --  Execute the following commands, until the next external one.
      --  Return True if there are still commands to executed after that one.

      function Get_Show_Command (N : Node_Ptr) return Boolean;
      --  Return True if the command should be shown for N

      function Get_Nth_Command (Cmd_Index : Integer) return Node_Ptr;
      --  Return the nth command in the list

      ------------------
      -- Substitution --
      ------------------

      function Substitution (Param : String) return String is
         File    : File_Selection_Context_Access;
         Project : Project_Type := No_Project;
         Num     : Integer;
         Index   : Integer;
         Recurse, List_Dirs, List_Sources : Boolean;

      begin
         if Param = "f" or else Param = "F" then
            --  We know from Check_Save_Output that the context is valid
            File := File_Selection_Context_Access (Command.Execution.Context);

            if Param = "f" then
               return Base_Name (File_Information (File));
            else
               return Full_Name (File_Information (File)).all;
            end if;

         elsif Param = "d" then
            --  We know from Check_Save_Output that the context is valid
            File := File_Selection_Context_Access (Command.Execution.Context);
            return Directory_Information (File);

         elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then
            Project := Project_From_Param (Param, Command.Execution.Context);

            if Param = "pps" or else Param = "PPs" then
               if Project = No_Project then
                  return "";
               else
                  return "-P" & Project_Path (Project);
               end if;
            end if;

            if Project = No_Project then
               Success := False;
               raise Invalid_Substitution;
            end if;

            if Param = "p" or else Param = "P" then
               return Project_Name (Project);

            elsif Param = "pp" or else Param = "PP" then
               return Project_Path (Project);

            else
               Recurse := Param (Param'First + 1) = 'r';

               if Recurse then
                  Index := Param'First + 2;
               else
                  Index := Param'First + 1;
               end if;

               if Index <= Param'Last then
                  List_Dirs    := Param (Index) = 'd';
                  List_Sources := Param (Index) = 's';

                  if Index < Param'Last and then Param (Index + 1) = 'f' then
                     --  Append the list to a file.
                     declare
                        File : File_Type;
                        Files_List : File_Array_Access;
                        List : String_Array_Access;
                     begin
                        Create (File);

                        if List_Dirs then
                           List := Source_Dirs (Project, Recurse);
                           if List /= null then
                              for K in List'Range loop
                                 Put_Line (File, List (K).all);
                              end loop;
                              Free (List);
                           end if;
                        end if;

                        if List_Sources then
                           Files_List := Get_Source_Files (Project, Recurse);
                           if Files_List /= null then
                              for K in Files_List'Range loop
                                 Put_Line
                                   (File, URL_File_Name (Files_List (K)));
                              end loop;
                              Unchecked_Free (Files_List);
                           end if;
                        end if;

                        declare
                           N : constant String := Name (File);
                        begin
                           Close (File);
                           return N;
                        end;
                     end;

                  else
                     declare
                        Result : Unbounded_String;
                        List : String_Array_Access;
                        Files_List : File_Array_Access;
                     begin
                        if List_Dirs then
                           List := Source_Dirs (Project, Recurse);
                           if List /= null then
                              for K in List'Range loop
                                 Append (Result, '"' & List (K).all & """ ");
                              end loop;
                              Free (List);
                           end if;
                        end if;

                        if List_Sources then
                           Files_List := Get_Source_Files (Project, Recurse);
                           if Files_List /= null then
                              for K in Files_List'Range loop
                                 Append
                                   (Result,
                                    '"'
                                    & URL_File_Name (Files_List (K)) & """ ");
                              end loop;
                              Unchecked_Free (Files_List);
                           end if;
                        end if;

                        return To_String (Result);
                     end;
                  end if;
               end if;
            end if;

         else
            Num := Safe_Value (Param, Default => 0);
            if Num <= Command.Execution.Cmd_Index - 1
              and then Num >= 1
            then
               if Command.Execution.Outputs
                 (Command.Execution.Cmd_Index - Num) = null
               then
                  return "";
               else
                  --  Remove surrounding quotes if any. This is needed so that
                  --  for instance of the function get_attributes_as_string
                  --  from Python can be used to call an external tool with
                  --  switches propertly interpreted.

                  declare
                     Output : String renames Command.Execution.Outputs
                       (Command.Execution.Cmd_Index - Num).all;
                     Last   : Integer;
                  begin
                     if Output = "" then
                        return Output;
                     end if;

                     Last := Output'Last;
                     while Last >= Output'First
                       and then Output (Last) = ASCII.LF
                     loop
                        Last := Last - 1;
                     end loop;

                     if Output (Output'First) = '''
                       and then Output (Last) = '''
                     then
                        return Output (Output'First + 1 .. Last - 1);

                     elsif Output (Output'First) = '"'
                       and then Output (Output'Last) = '"'
                     then
                        return Output (Output'First + 1 .. Last - 1);

                     else
                        return Output (Output'First .. Last);
                     end if;
                  end;
               end if;
            end if;
         end if;

         --  Keep the percent sign, since this might be useful for the shell
         --  itself
         return '%' & Param;
      end Substitution;

      ----------------------------
      -- Execute_Simple_Command --
      ----------------------------

      function Execute_Simple_Command
        (Script          : Scripting_Language;
         Command_Line    : String;
         Output_Location : String := No_Output;
         Show_Command    : Boolean := True;
         Progress_Regexp : String := "";
         Current_In_Regexp : Integer := -1;
         Total_In_Regexp   : Integer := -1;
         Hide_Progress     : Boolean := True) return Boolean
      is
         --  Perform arguments substitutions for the command.
         Subst_Cmd_Line : constant String := Substitute
           (Command_Line,
            Substitution_Char => '%',
            Callback          => Substitution'Unrestricted_Access,
            Recursive         => False);
         Args           : String_List_Access;
         Errors         : aliased Boolean;
         Console        : Interactive_Console;

      begin
         if Success and then Output_Location /= No_Output then
            Console := Create_Interactive_Console
              (Command.Kernel, Output_Location);
         end if;

         --  Ensure there is at least one new line between two commands
         if Console /= null then
            Insert (Console, "", Add_LF => True);
         end if;

         --  If substitution failed
         if not Success then
            null;

         elsif Script /= null then
            if Command.Execution.Save_Output (Command.Execution.Cmd_Index) then

               --  Insert the command explicitely, since Execute_Command
               --  doesn't do it in this case.
               if Console /= null and then Show_Command then
                  Insert (Console, Subst_Cmd_Line, Add_LF => True);
               end if;

               Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
                 new String'
                   (Execute_Command
                        (Script, Subst_Cmd_Line,
                         Hide_Output  => Output_Location = No_Output,
                         Show_Command => Show_Command,
                         Console      => Console,
                         Errors       => Errors'Unchecked_Access));
            else
               Execute_Command
                 (Script, Subst_Cmd_Line,
                  Hide_Output  => Output_Location = No_Output,
                  Show_Command => Show_Command,
                  Console      => Console,
                  Errors => Errors);
            end if;

            Success := not Errors;

         else
            Trace (Me, "Executing external command " & Command_Line);

            Free (Command.Execution.Current_Output);
            Command.Execution.Current_Output := new String'("");

            Unchecked_Free (Command.Execution.Progress_Matcher);
            Command.Execution.Current_In_Regexp :=
              Integer'Max (0, Current_In_Regexp);
            Command.Execution.Total_In_Regexp   :=
              Integer'Max (0, Total_In_Regexp);
            Command.Execution.Hide_Progress := Hide_Progress;

            if Progress_Regexp /= ""then
               Command.Execution.Progress_Matcher := new Pattern_Matcher'
                 (Compile (Progress_Regexp, Multiple_Lines));
            end if;

            Args := Argument_String_To_List (Subst_Cmd_Line);

            Launch_Process
              (Command.Kernel,
               Command       => Args (Args'First).all,
               Arguments     => Args (Args'First + 1 .. Args'Last),
               Console       => null,
               Callback      => Store_Command_Output'Access,
               Exit_Cb       => Exit_Cb'Access,
               Success       => Success,
               Show_Command  => Show_Command,
               Callback_Data => Convert (Custom_Command_Access (Command)),
               Line_By_Line  => True);
            Free (Args);

            Command.Execution.External_Process_Console := Console;
            Command.Execution.External_Process_In_Progress := True;
         end if;

         return Success;

      exception
         when E : others =>
            Insert (Command.Kernel,
                    -("An unexpected error occurred while executing the custom"
                      & " command. See the log file for more information."),
                    Mode => Error);
            Trace (Me, "Unexpected exception: " & Exception_Information (E));
            return False;
      end Execute_Simple_Command;

      ---------------------
      -- Get_Nth_Command --
      ---------------------

      function Get_Nth_Command (Cmd_Index : Integer) return Node_Ptr is
         Index : Natural := 1;
         N     : Node_Ptr := Command.XML;
      begin
         while Index < Cmd_Index loop
            N := N.Next;
            Index := Index + 1;
         end loop;

         return N;
      end Get_Nth_Command;

      ----------------------
      -- Get_Show_Command --
      ----------------------

      function Get_Show_Command (N : Node_Ptr) return Boolean is
         Att : constant String := Get_Attribute (N, "show-command");
      begin
         if Att /= "" then
            return To_Lower (Att) = "true";
         end if;

         return Command.Default_Show_Command;
      end Get_Show_Command;

      --------------------------
      -- Execute_Next_Command --
      --------------------------

      function Execute_Next_Command return Boolean is
         N : Node_Ptr;
         Show_Command : Boolean;
      begin
         if Command.Command /= null then
            Success := Execute_Simple_Command
              (Command.Script, Command.Command.all);
            return False;

         else
            N := Get_Nth_Command (Command.Execution.Cmd_Index);

            while Success and then N /= null loop
               Set_Progress
                 (Command,
                  Progress_Record'
                    (Activity => Running,
                     Current  => Command.Execution.Cmd_Index,
                     Total    => Command.Execution.Outputs'Length));

               Show_Command := Get_Show_Command (N);

               if To_Lower (N.Tag.all) = "shell" then
                  Success := Execute_Simple_Command
                    (Lookup_Scripting_Language
                       (Command.Kernel,
                        Get_Attribute (N, "lang", GPS_Shell_Name)),
                     N.Value.all,
                     Output_Location =>
                       Get_Attribute (N, "output",
                                      Command.Default_Output_Destination.all),
                     Show_Command => Show_Command);

               elsif To_Lower (N.Tag.all) = "external" then
                  Success := Execute_Simple_Command
                    (null,
                     N.Value.all,
                     Output_Location =>
                       Get_Attribute (N, "output",
                                      Command.Default_Output_Destination.all),
                     Show_Command => Show_Command,
                     Progress_Regexp   => Get_Attribute (N, "progress-regexp"),
                     Current_In_Regexp =>
                       Safe_Value (Get_Attribute (N, "progress-current")),
                     Total_In_Regexp   =>
                       Safe_Value (Get_Attribute (N, "progress-final")),
                     Hide_Progress     => Case_Insensitive_Equal
                       (Get_Attribute (N, "progress-hide", "true"), "true"));

                  --  We'll have to run again to check for completion
                  return True;
               end if;

               N := N.Next;
               Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;
            end loop;

            --  No more command to execute
            return False;
         end if;
      end Execute_Next_Command;

      -----------------------
      -- Terminate_Command --
      -----------------------

      function Terminate_Command return Command_Return_Type is
      begin
         Free (Command.Execution);

         Command_Finished (Command, Success);
         if Success then
            return Commands.Success;
         else
            return Failure;
         end if;
      end Terminate_Command;

   begin
      --  If there was an external command executing:
      if Command.Execution /= null then
         if Command.Execution.External_Process_In_Progress then
            return Execute_Again;
         end if;

         Command.Execution.Outputs (Command.Execution.Cmd_Index) :=
           Command.Execution.Current_Output;
         Command.Execution.Current_Output := null;
         Command.Execution.Cmd_Index := Command.Execution.Cmd_Index + 1;

         if Command.Execution.Process_Exit_Status /= 0 then

            --  We were executing an external command that fail. Time to
            --  execute the "on-failure" fallback
            declare
               N : constant Node_Ptr :=
                 Get_Nth_Command (Command.Execution.Cmd_Index - 1);
               Failure : constant String := Get_Attribute (N, "on-failure");
               Lang    : constant String :=
                 Get_Attribute (N, "on-failure-lang", GPS_Shell_Name);
            begin
               Trace (Me, "Command failed, executing failure command: "
                      & Failure);
               if Failure /= "" then
                  Success := Execute_Simple_Command
                    (Lookup_Scripting_Language (Command.Kernel, Lang),
                     Failure,
                     Output_Location =>
                       Get_Attribute (N, "output",
                                      Command.Default_Output_Destination.all),
                     Show_Command => Get_Show_Command (N));
               end if;
            end;

            Success := False;
            return Terminate_Command;
         end if;

      else
         declare
            Count : constant Natural := Commands_Count (Command);
         begin
            Command.Execution := new Custom_Command_Execution_Record;
            Command.Execution.Outputs     := new Argument_List (1 .. Count);
            Command.Execution.Save_Output := new Boolean_Array (1 .. Count);
            Command.Execution.Context  := Get_Current_Context (Command.Kernel);
            Command.Execution.Cmd_Index   := 1;
            Ref (Command.Execution.Context);
         end;

         Check_Save_Output
           (Command.Kernel, Command, Command.Execution.Save_Output.all,
            Command.Execution.Context, Success);
         Clear_Consoles (Command.Kernel, Command);

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

   function Name (Command : access Custom_Command) return String is
   begin
      return Command.Name.all;
   end Name;

end Commands.Custom;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;                     use GNAT.OS_Lib;

with GNAT.Expect;                     use GNAT.Expect;
with GNAT.Expect.TTY;                 use GNAT.Expect.TTY;

with GNAT.Regpat;                     use GNAT.Regpat;

with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;

with GNATdoc;                         use GNATdoc;

with XML_Utils;                       use XML_Utils;
with XML_Parsers;

with GPS.Customizable_Modules;        use GPS.Customizable_Modules;
with GPS.Scripts;                     use GPS.Scripts;

package body GPS.CLI_Scripts is
   Me : constant Trace_Handle := Create ("SCRIPTS");

   Xml_Cst               : aliased constant String := "xml";
   Xml_Custom_Parameters : constant Cst_Argument_List := (1 => Xml_Cst'Access);
   Docgen_Class_Name     : constant String := "Docgen";
   Skip_C_Files_Cst      : aliased constant String := "skip_c_files";
   Report_Errors_Cst     : aliased constant String := "report_errors";
   Tree_Output_Cst       : aliased constant String := "tree_output";
   With_Comments_Cst     : aliased constant String := "with_comments";
   Process_Parameters    : constant Cst_Argument_List :=
     (1 => Skip_C_Files_Cst'Access,
      2 => Report_Errors_Cst'Access,
      3 => Tree_Output_Cst'Access,
      4 => With_Comments_Cst'Access);

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for misc GPS.* commands

   function Get_Docgen_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Return class for Docgen

   procedure Docgen_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.Docgen.process_project command

   procedure Process_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Hanler for GPS.Project.launch_simple_process command

   procedure Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
   begin
      if Command = "get_share_dir" then
         Set_Return_Value (Data, +Kernel.Get_Share_Dir.Full_Name);

      elsif Command = "parse_xml" then
         Name_Parameters (Data, Xml_Custom_Parameters);

         declare
            File : constant Filesystem_String :=
              +Current_Script (Get_Script (Data));
            Node : Node_Ptr;
            Err  : GNAT.OS_Lib.String_Access;
         begin
            XML_Parsers.Parse_Buffer
              (Buffer     => Nth_Arg (Data, 1),
               From_File  => File,
               Start_Line => 1,
               Tree       => Node,
               Error      => Err);

            if Node /= null then
               Execute_Customization_String
                 (Kernel => Kernel,
                  File   => Create (File),
                  Node   => Node.Child,
                  Level  => Hard_Coded);

            elsif Err.all /= "" then
               Set_Error_Msg (Data, Err.all);
            end if;

            exception
               when E : others =>
                  Trace (Me, E);
                  Set_Error_Msg (Data, "Error while executing parse_xml()");
         end;
      end if;
   end Command_Handler;

   ----------------------------
   -- Docgen_Command_Handler --
   ----------------------------

   procedure Docgen_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
   begin
      if Command = "process_project" then
         Name_Parameters (Data, Process_Parameters);

         declare
            Skip_C_Files  : constant Boolean := Nth_Arg (Data, 1, False);
            Report_Errors : constant String := Nth_Arg (Data, 2, "None");
            Tree_Output   : constant String := Nth_Arg (Data, 3, "Full");
            With_Comments : constant Boolean := Nth_Arg (Data, 4, False);
            Backend_Name  : constant String := Nth_Arg (Data, 5, "html");

            Options : constant GNATdoc.Docgen_Options :=
              (Comments_Filter  => null,
               Disable_Markup   => False,
               Report_Errors    => Report_Errors_Kind'Value (Report_Errors),
               Ignore_Files     => null,
               Leading_Doc      => False,
               Skip_C_Files     => Skip_C_Files,
               Tree_Output      => (Tree_Output_Kind'Value (Tree_Output),
                                    With_Comments),
               Backend_Name     => To_Unbounded_String (Backend_Name),
               Display_Time     => False,
               Document_Bodies  => False,
               Extensions_Enabled => False,
               Process_Bodies   => False,
               Show_Private     => True,
               Output_Comments  => True,
               Quiet_Mode       => False,
               Default_Encoding => new String'("iso-8859-1"));
         begin
            GNATdoc.Process_Project_Files
              (Kernel    => Kernel,
               Options   => Options,
               Project   => Kernel.Registry.Tree.Root_Project,
               Recursive => True);
         exception
            when others =>
               Set_Error_Msg (Data, "Error while executing "  & Command);
         end;
      end if;
   end Docgen_Command_Handler;

   -----------------------------
   -- Process_Command_Handler --
   -----------------------------

   procedure Process_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Args           : Argument_List_Access;
      Pd             : TTY_Process_Descriptor;

      Pattern_Arg    : constant := 1;  --  The pattern parameter
      Log_Arg        : constant := 2;  --  The log parameter
      Command_Arg    : constant := 3;  --  The command parameter

      Argc           : constant Natural := Data.Number_Of_Arguments;
      --  The number of arguments actually passed to the command

      Status         : Integer := 1;
      --  The return code

      Log_Filename   : constant String := Data.Nth_Arg (Log_Arg, "");
      Log_File       : Writable_File := Invalid_File;

      Result         : Expect_Match;
      Pattern_String : constant String := Data.Nth_Arg (Pattern_Arg, "");
      Pattern        : Pattern_Matcher_Access;
      Num_Groups     : Match_Count := 0;

   begin
      --  Open the log file for writing, if needed

      if Log_Filename /= "" then
         begin
            Log_File := Write_File (Create (+Log_Filename));

         exception
            when others =>
               Log_File := Invalid_File;
         end;

         if Log_File = Invalid_File then
            Data.Set_Error_Msg
              ("Could not open file for writing: " & Log_Filename);
            return;
         end if;
      end if;

      --  Construct the argument list

      Args := new Argument_List (Command_Arg + 1 .. Argc);
      for J in Command_Arg + 1 .. Argc loop
         Args (J) := new String'(Data.Nth_Arg (J));
      end loop;

      --  Spawn the process

      begin
         Non_Blocking_Spawn (Descriptor  => Pd,
                             Command     => Data.Nth_Arg (Command_Arg),
                             Args        => Args.all,
                             Buffer_Size => 0,
                             Err_To_Out  => True);
      exception
         when others =>
            Data.Set_Error_Msg ("Could not launch command");
            Free (Args);

            if Log_File /= Invalid_File then
               Close (Log_File);
            end if;

            return;
      end;

      Free (Args);

      --  Precompile the pattern matcher for efficiency

      if Pattern_String /= "" then
         Pattern := new Pattern_Matcher'(Compile (Pattern_String));
         Num_Groups := Paren_Count (Pattern.all);
      end if;

      --  While the process is up, read its output line by line

      begin
         loop
            Expect (Pd, Result, ".+\n");

            declare
               S : constant String := Expect_Out (Pd);
               M : Match_Array (0 .. Num_Groups);
            begin
               --  Log the results if needed
               if Log_File /= Invalid_File then
                  Write (Log_File, S);
               end if;

               if Pattern /= null then
                  --  Look for the progress regexp in the output

                  Match (Pattern.all, S, M);

                  if M (0) /= No_Match then
                     --  We have a match!

                     if Num_Groups = 2 then
                        --  If the match has two subgroups, interpret them as
                        --  current and total and just print this

                        Put_Line (S (M (1).First .. M (1).Last) & '/'
                                  & S (M (2).First .. M (2).Last));
                     else
                        --  otherwise, just print the whole match
                        Put_Line (S (M (0).First .. M (0).Last));
                     end if;
                  end if;
               end if;
            end;
         end loop;
      exception
         when Process_Died =>
            --  This is expected at some point

            Close (Pd, Status);

            if Pattern /= null then
               Free (Pattern);
            end if;

            if Log_File /= Invalid_File then
               Close (Log_File);
            end if;

            Data.Set_Return_Value (Status);
      end;
   end Process_Command_Handler;

   ----------------------
   -- Get_Docgen_Class --
   ----------------------

   function Get_Docgen_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Docgen_Class_Name);
   end Get_Docgen_Class;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, "get_share_dir",
         Handler => Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "parse_xml",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "process_project",
         Class         => Get_Docgen_Class (Kernel),
         Static_Method => True,
         Minimum_Args  => 0,
         Maximum_Args  => 4,
         Handler       => Docgen_Command_Handler'Access);

      Register_Command
        (Kernel.Scripts, "launch_simple_process",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Process_Command_Handler'Access);
      --  GPS.launch_simple_process:
      --  Launches a process, waits for the result, and display progress on
      --  the standard output.
      --
      --  Usage:
      --
      --   r = GPS.launch_simple_process (regex, log, command, arg1, ... argN)
      --
      --  where:
      --
      --      regex is a string representing a regular expression, or empty:
      --        - if it is an empty string, no output is displayed
      --        - if it contains a regular expression:
      --             - if this regular expression contains exactly two
      --               groups, interpret them as 'current' and 'total'
      --               and print "current/total"
      --             - otherwise, print the part of the output that was
      --               matched by the regular expression
      --
      --      log is a string representing the file in which to log
      --          the full output of the command. If log is the empty string,
      --          no logging occurs. If the file could not be found or could
      --          not be opened for writing, an exception is raised
      --
      --      command is the command to launch, for instance "gnatls"
      --
      --      arg1 .. argN contain the arguments to pass to the function
      --
      --  returns the status of the launched process as an integer.
      --  Raises an exception if the process could not be launched.
      --
      --  Example:
      --       status = GPS.launch_simple_process (
      --          "completed (\d+) out of (\d+)",
      --          "log.txt",
      --          "gprbuild", "-d", "-Pcli.gpr")
      --
      --       print "exited with status %s" % status

   end Register_Commands;

end GPS.CLI_Scripts;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

--  This package implements commands that can be used to launch
--  shell commands that depend on the current context.
--
--  When calling Execute on the command, the arguments will be transformed
--  in the following way
--
--      %f  -> base name of the currently opened file.
--      %F  -> absolute name of the currently opened file.
--
--      %d  -> current directory.    %d%f = %F
--
--      %p  -> the current project (associated with the opened file)
--      %P  -> the current root project
--
--      %{p|P}[r]{d|s}[f] ->
--         Substituted by the contents of a project :
--               P : the project is the root project
--               p : the project is the current project
--               r : indicates that the listing should be project-recursive,
--                   ie that sub-projects should be listed as well, and their
--                   subprojects, and so on.
--               d : list the source directories
--               s : list the source files
--               f : output the list into a file and substitute the
--                   parameter with the name of that file.
--
--          Examples :
--            %Ps   ->  replaced by a list of source files in the root project,
--                      not recursively
--            %prs  ->  replaced by a list of files in the current project,
--                      recursively
--            %prdf ->  replaced by the name of a file that contains a list
--                      of source directories in the current project,
--                      recursively
--
--
--     ??? The following still have to be implemented :
--
--      %l, %c -> the current line and column in the current file.

with GNAT.Expect;
with GNATCOLL.Scripts;
with GNAT.Strings;            use GNAT.Strings;

with XML_Utils;

with Commands;                use Commands;
with Commands.Interactive;    use Commands.Interactive;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;
with Interactive_Consoles;

package Commands.Custom is

   type Custom_Command is new Interactive_Command with private;
   type Custom_Command_Access is access all Custom_Command'Class;

   No_Output : constant String := "none";
   --  Value of the "output" attribute that specifies that no output should be
   --  visible.

   Console_Output : constant String := "";
   --  Value of the "output" attribute that specifies that the output should be
   --  sent to the console.

   procedure Create
     (Item    : out Custom_Command_Access;
      Name    : String;
      Kernel  : Kernel_Handle;
      Command : String;
      Script  : GNATCOLL.Scripts.Scripting_Language);
   --  Create a new custom command.
   --  If Script is null, the command is launched as a system
   --  command (Unix or Windows). Otherwise, it is interpreted as a GPS
   --  Internal command in the specific scripting language.
   --  Filter is the filter that needs to be tested to make sure that all
   --  parameters can be satisfied.
   --  Name is used in the progress bar while the command is executing

   procedure Create
     (Item                 : out Custom_Command_Access;
      Name                 : String;
      Kernel               : Kernel_Handle;
      Command              : XML_Utils.Node_Ptr;
      Default_Output       : String := Console_Output;
      Show_Command         : Boolean := True;
      Show_In_Task_Manager : Boolean := False);
   --  Create a new command with a list of <shell> and <external> nodes, as
   --  done in the customization files. Filter is the filter that needs to
   --  be tested to make sure that all parameters can be satisfied.
   --  Each of the commands is executed in turn. Output from one command is
   --  made available to the next through %1, %2,...
   --  Default_Output specifies where the output should be sent by default, if
   --  not overridden by any "output" attribute in the XML tree.
   --  If Show_Command is true, then the command itself will be shown along
   --  with its output if the latter is not No_Output.
   --  If Show_In_Task_Manager is true, then the command will be shown in the
   --  task manager.
   --  Name is used in the progress bar while the command is executing

   function Create_Filter
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Command : XML_Utils.Node_Ptr) return Action_Filter;
   --  Return null or a filter suitable for Command. This filter ensures that
   --  all %f,... parameters can be properly substituted.
   --  This filter should be checked if the command is used as an action in
   --  GPS.

   overriding procedure Primitive_Free (X : in out Custom_Command);
   --  Free memory associated with X.

   overriding function Execute
     (Command : access Custom_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Execute Command, and return Success if the command could be launched
   --  successfully.
   --  Context-related arguments (like "%f", "%p" and so on) are converted
   --  when Execute is called, with parameters obtained from the current
   --  context and the current project. If a parameter could not be converted,
   --  the command is not launched, and Failure is returned.
   --
   --  The custom commands must only be executed through the task_manager,
   --  since they launch external processes in background mode and must wait
   --  for their output.

   overriding procedure Interrupt (Command : in out Custom_Command);
   --  See doc from inherited subprograms

private
   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access Boolean_Array;

   overriding function Name (Command : access Custom_Command) return String;
   --  See doc for inherited subprogram

   type Custom_Command_Execution_Record is record
      In_Process  : Boolean := False;
      --  True if we are processing the command, but there are some external
      --  process to run before completion.

      Cmd_Index   : Natural;
      --  The current command we are executing

      Current_Failure : Integer := -1;
      --  Whether we are processing some on-failure command

      Progress_Matcher  : GNAT.Expect.Pattern_Matcher_Access;
      Current_In_Regexp : Natural;
      Total_In_Regexp   : Natural;
      Hide_Progress     : Boolean;
      --  How to recognize progress indication in the external commands

      External_Process_Console : Interactive_Consoles.Interactive_Console;
      --  Where the output of the current external command should be sent

      External_Process_In_Progress : Boolean := False;
      Process_Exit_Status : Integer;
      --  True if an external process is currently running

      Outputs : String_List_Access;
      --  The output of the various commands, if it was saved

      Current_Output : String_Access;
      --  Output of the external currently executing, if we need to save it

      Save_Output : Boolean_Array_Access;
      --  Whether we should save the output of the nth-command

      Context  : Selection_Context := No_Context;
      --  The context we had at the beginning of the executing

      Check_Password : Boolean;
      --  Check for password/passphrase prompt

      Nb_Password    : Natural;
      --  The count of user password asking
   end record;
   type Custom_Command_Execution is access Custom_Command_Execution_Record;
   --  Stores information relevant only while a command is executing, to
   --  limit the memory footprint when storing a command.

   type Command_Component_Record is abstract tagged null record;
   type Command_Component is access all Command_Component_Record'Class;
   --  A command is usually a succession of small steps to reach a specific
   --  goal. These steps can be defined in a number of ways: either they are
   --  coded in the GPS source code itself or in a module programmed in Ada,
   --  or they are defined by the user in customization files, and are the
   --  result of executing GPS shell or Python scripts, or running external
   --  applications.

   type Command_Component_Description is record
      Component      : Command_Component;
      On_Failure_For : Integer := -1;
      --  Index of the command for which thiis component is part of the
      --  on-failure group. For instance, with the following XML file:
      --      1-   <external />
      --           <on-failure>
      --      2-       <script />
      --      3-       <script />
      --           </on-failure>
      --  then both commands 2 and 3 will have On_Failure_For set to 1
   end record;

   type Components_Array is array (Natural range <>)
      of Command_Component_Description;
   type Components_Array_Access is access Components_Array;

   type Custom_Command is new Interactive_Command with record
      Kernel      : Kernel_Handle;

      Components  : Components_Array_Access;
      --  The various components of the command

      Default_Output_Destination : String_Access;
      --  The default location for the XML tree

      Default_Show_Command : Boolean;
      --  True if by default the various commands must be shown along with
      --  their output. If the output is hidden, the command itself will not
      --  be shown

      Name : String_Access;
      --  The name of the command

      Execution   : Custom_Command_Execution;
      --  The current context for the execution of the command. If this is
      --  null, no command is currently executing

      Sub_Command : Scheduled_Command_Access;
   end record;

end Commands.Custom;

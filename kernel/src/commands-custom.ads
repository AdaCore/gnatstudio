-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
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

with Gdk.Event;
with Glide_Kernel;         use Glide_Kernel;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Commands.Interactive; use Commands.Interactive;
with Glib.Xml_Int;

package Commands.Custom is

   type Custom_Command is new Interactive_Command with private;
   type Custom_Command_Access is access all Custom_Command'Class;

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String;
      Script       : Glide_Kernel.Scripts.Scripting_Language);
   --  Create a new custom command.
   --  If Script is null, the command is launched as a system
   --  command (Unix or Windows). Otherwise, it is interpreted as a GPS
   --  Internal command in the specific scripting language.

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : Glib.Xml_Int.Node_Ptr);
   --  Create a new command with a list of <shell> and <external> nodes, as
   --  done in the customization files.
   --  Each of the commands is executed in turn. Output from one command is
   --  made available to the next through %1, %2,...

   procedure Free (X : in out Custom_Command);
   --  Free memory associated with X.

   function Execute
     (Command       : access Custom_Command;
      Event         : Gdk.Event.Gdk_Event) return Command_Return_Type;
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

private
   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access Boolean_Array;

   type Custom_Command is new Interactive_Command with record
      Kernel      : Kernel_Handle;

      Command     : String_Access;
      Script      : Glide_Kernel.Scripts.Scripting_Language;
      XML         : Glib.Xml_Int.Node_Ptr;
      --  Only (Command, Script) or XML is defined, depending on what version
      --  of Create was used.

      In_Process  : Boolean := False;
      --  True if we are processing the command, but there are some external
      --  process to run before completion

      Cmd_Index   : Natural;
      --  The current command we are executing

      External_Process_In_Progress : Boolean;
      Process_Exit_Status : Integer;
      --  True if an external process is currently running

      Outputs : Argument_List_Access;
      --  The output of the various commands, if it was saved.

      Current_Output : GNAT.OS_Lib.String_Access;
      --  Output of the external currently executing, if we need to save it.

      Save_Output : Boolean_Array_Access;
      --  Whether we should save the output of the nth-command

      Context  : Selection_Context_Access;
      --  The process we had at the beginning of the executing
   end record;

end Commands.Custom;

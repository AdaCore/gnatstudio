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

with Basic_Types;          use Basic_Types;
with Projects;             use Projects;
with String_Utils;         use String_Utils;

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Traces;               use Traces;
with Ada.Unchecked_Deallocation;

package body Commands.Custom is

   Me : constant Debug_Handle := Create ("Commands.Custom");

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Command) is
   begin
      Free (X.Command);
      Free (X.Args);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String;
      Args         : Argument_List_Access;
      Script       : Glide_Kernel.Scripts.Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Command := new String'(Command);
      Item.Args := Args;
      Item.Script := Script;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Custom_Command) return Command_Return_Type
   is
      Context  : constant Selection_Context_Access :=
        Get_Current_Context (Command.Kernel);
      Success  : Boolean := False;
      No_Args  : String_List (1 .. 0);
      New_Args : Argument_List_Access;
      Last     : Integer;
      Index    : Integer;

      Recurse  : Boolean;

      function Command_To_String
        (Command : String; Args : String_List) return String;
      --  Transform a command and its arguments into a string.

      function Substitution (Param : String) return String;
      --  Substitution function for the various '%...' parameters

      function Project_From_Param (Param : String) return Project_Type;
      --  Return the project from the parameter. Parameter is the string
      --  following the '%' sign

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Object => String_List, Name => String_List_Access);

      ------------------------
      -- Project_From_Param --
      ------------------------

      function Project_From_Param (Param : String) return Project_Type is
         File : File_Selection_Context_Access;
      begin
         if Param (Param'First) = 'P' then
            return Get_Project (Command.Kernel);
         elsif Context /= null
           and then Context.all in File_Selection_Context'Class
           and then Has_Project_Information
             (File_Selection_Context_Access (Context))
         then
            File := File_Selection_Context_Access (Context);
            return Project_Information (File);
         else
            Insert (Command.Kernel,
                    -"Command not executed: it requires a project",
                    Mode => Error);
            return No_Project;
         end if;
      end Project_From_Param;

      -----------------------
      -- Command_To_String --
      -----------------------

      function Command_To_String
        (Command : String; Args : String_List) return String
      is
         Length : Integer;
      begin
         Length := Command'Length;

         for J in Args'Range loop
            Length := Length + Args (J)'Length + 1;
         end loop;

         declare
            Result : String (1 .. Length);
            Index : Integer := 1;
         begin
            Result (Index .. Index + Command'Length - 1) := Command;
            Index := Index + Command'Length;

            for J in Args'Range loop
               Result (Index .. Index + Args (J)'Length) := " " & Args (J).all;
               Index := Index + Args (J)'Length + 1;
            end loop;

            return Result;
         end;
      end Command_To_String;

      ------------------
      -- Substitution --
      ------------------

      function Substitution (Param : String) return String is
         File : File_Selection_Context_Access;
         Project  : Project_Type := No_Project;
         List     : String_Array_Access;
      begin
         if Param = "f" or else Param = "F" then
            if Context /= null
              and then Context.all in File_Selection_Context'Class
              and then Has_File_Information
                (File_Selection_Context_Access (Context))
            then
               File := File_Selection_Context_Access (Context);

               if Param = "f" then
                  return File_Information (File);
               else
                  return Directory_Information (File)
                    & File_Information (File);
               end if;

            else
               Insert (Command.Kernel,
                       -"Command not executed: it requires a file",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param = "d" then
            if Context /= null
              and then Context.all in File_Selection_Context'Class
              and then Has_Directory_Information
                 (File_Selection_Context_Access (Context))
            then
               File := File_Selection_Context_Access (Context);
               return Directory_Information (File);
            else
               Insert (Command.Kernel,
                       -"Command not executed: it requires a directory",
                       Mode => Error);
               raise Invalid_Substitution;
            end if;

         elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then

            Project := Project_From_Param (Param);
            if Project = No_Project then
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

               if Param (Index) = 's' then
                  List := Get_Source_Files (Project, Recurse);

               elsif Param (Index) = 'd' then
                  List := Source_Dirs (Project, Recurse);
               end if;

               if List = null then
                  Insert (Command.Kernel,
                          -"Command not executed: it requires a project",
                          Mode => Error);
                  raise Invalid_Substitution;
               end if;

               if Index < Param'Last
                 and then Param (Index + 1) = 'f'
               then
                  --  Append the list to a file.
                  declare
                     File : File_Type;
                  begin
                     Create (File);

                     for K in List'Range loop
                        Put_Line (File, List (K).all);
                     end loop;

                     Free (List);

                     declare
                        N : constant String := Name (File);
                     begin
                        Close (File);
                        return N;
                     end;
                  end;
               end if;
            end if;
         end if;

         Insert (Command.Kernel,
                 -"Unknown substitution parameter: " & Param,
                 Mode => Error);
         raise Invalid_Substitution;
      end Substitution;


   begin
      --  Perform arguments substitutions for the command.

      if Command.Args /= null then
         New_Args := new String_List (Command.Args'Range);
         Last := New_Args'First;

         for J in Command.Args'Range loop
            --  Special case for %prs, since this is a list of files
            if Command.Args (J).all = "prs"
              or else Command.Args (J).all = "Prs"
            then
               declare
                  Project : constant Project_Type := Project_From_Param
                    (Command.Args (J).all);
                  List    : String_Array_Access;
               begin
                  if Project = No_Project then
                     return Failure;
                  end if;

                  List := Get_Source_Files (Project, Recurse);

                  declare
                     New_New_Args : Argument_List_Access := new String_List
                       (New_Args'First .. New_Args'Last + List'Length - 1);
                  begin
                     New_New_Args (New_Args'First .. Last - 1) :=
                       New_Args (New_Args'First .. Last - 1);

                     for K in List'Range loop
                        New_New_Args (Last) := new String'(List (K).all);
                        Last := Last + 1;
                     end loop;

                     Free (List);
                     Unchecked_Free (New_Args);
                     New_Args := New_New_Args;
                  end;
               end;

            else
               New_Args (Last) := new String'
                 (Substitute
                    (Command.Args (J).all,
                     Substitution_Char => '%',
                     Callback          => Substitution'Unrestricted_Access,
                     Recursive         => False));
            end if;

            Last := Last + 1;
         end loop;

         --  Arguments have been substituted, launch the command.

         if Command.Script /= null then
            Execute_Command
              (Command.Script,
               Command_To_String (Command.Command.all, New_Args.all),
               Display_In_Console => True);
            Success := True;
         else
            Trace (Me, "Executing external command " & Command.Command.all);
            Launch_Process
              (Command.Kernel,
               Command.Command.all,
               New_Args.all,
               "",
               null,
               null,
               "",
               Success);
         end if;

         Free (New_Args);

      elsif Command.Script /= null then
         Execute_Command (Command.Script, Command.Command.all,
                          Display_In_Console => True);
         Success := True;

      else
         Launch_Process
           (Command.Kernel,
            Command.Command.all,
            No_Args,
            "",
            null,
            null,
            "",
            Success);
      end if;

      Command_Finished (Command, Success);

      if Success then
         return Commands.Success;
      else
         return Failure;
      end if;

   exception
      when E : others =>
         Insert (Command.Kernel,
                 -("An unexpected error occured while executing the custom"
                   & " command. See the log file for more information."),
                 Mode => Error);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return Failure;
   end Execute;

end Commands.Custom;

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
with Glide_Intl;           use Glide_Intl;
with Shell;                use Shell;

with Basic_Types;          use Basic_Types;
with Projects;             use Projects;

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
      GPS_Command  : Boolean) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Command := new String'(Command);
      Item.Args := Args;
      Item.GPS_Command := GPS_Command;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Custom_Command) return Boolean is
      Context  : constant Selection_Context_Access :=
        Get_Current_Context (Command.Kernel);
      File     : File_Selection_Context_Access;
      Success  : Boolean := False;
      No_Args  : String_List (1 .. 0);
      New_Args : Argument_List_Access;
      Last     : Integer;
      Index    : Integer;

      Project  : Project_Type := No_Project;
      List     : String_Array_Access;
      Recurse  : Boolean;

      procedure Free_Array is new Ada.Unchecked_Deallocation
        (Object => String_List, Name => String_List_Access);

      function Command_To_String
        (Command : String; Args : String_List) return String;
      --  Transform a command and its arguments into a string.

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

   begin
      --  Perform arguments substitutions for the command.

      if Command.Args /= null then
         New_Args := new String_List (Command.Args'Range);
         Last := New_Args'First;

         for J in Command.Args'Range loop
            if Command.Args (J).all (Command.Args (J)'First) /= '%' then
               New_Args (Last) := new String'(Command.Args (J).all);
               Last := Last + 1;

            elsif Command.Args (J).all = "%f" then
               if Context /= null
                 and then Context.all in File_Selection_Context'Class
               then
                  File := File_Selection_Context_Access (Context);

                  if Has_File_Information (File) then
                     New_Args (Last) := new String'(File_Information (File));
                     Last := Last + 1;
                  else
                     return False;
                  end if;
               else
                  return False;
               end if;

            elsif Command.Args (J).all = "%F" then
               if Context /= null
                 and then Context.all in File_Selection_Context'Class
               then
                  File := File_Selection_Context_Access (Context);

                  if Has_File_Information (File) then
                     New_Args (Last) := new String'
                       (Directory_Information (File)
                        & File_Information (File));
                     Last := Last + 1;
                  else
                     return False;
                  end if;
               else
                  return False;
               end if;

            elsif Command.Args (J).all = "%d" then
               if Context /= null
                 and then Context.all in File_Selection_Context'Class
                 and then Has_Directory_Information
                   (File_Selection_Context_Access (Context))
               then
                  File := File_Selection_Context_Access (Context);
                  New_Args (Last) := new String'
                    (Directory_Information (File));
                  Last := Last + 1;
               else
                  return False;
               end if;

            elsif Command.Args (J).all
              (Command.Args (J)'First .. Command.Args (J)'First + 1) = "%P"
              or else Command.Args (J).all
              (Command.Args (J)'First .. Command.Args (J)'First + 1) = "%p"
            then
               --  Determine the project to use

               if Command.Args (J).all
                 (Command.Args (J)'First .. Command.Args (J)'First + 1) = "%P"
               then
                  Project := Get_Project (Command.Kernel);
               elsif Context /= null
                 and then Context.all in File_Selection_Context'Class
               then
                  File := File_Selection_Context_Access (Context);

                  if Has_Project_Information (File) then
                     Project := Project_Information (File);
                  end if;
               end if;

               if Project = No_Project then
                  return False;
               end if;

               if Command.Args (J).all = "%p"
                 or else Command.Args (J).all = "%P"
               then
                  New_Args (Last) := new String'(Project_Name (Project));
               elsif Command.Args (J).all = "%pp"
                 or else Command.Args (J).all = "%PP"
               then
                  New_Args (Last) := new String'(Project_Path (Project));
               else
                  Recurse := (Command.Args (J).all
                              (Command.Args (J)'First + 2) = 'r');

                  if Recurse then
                     Index := Command.Args (J)'First + 3;
                  else
                     Index := Command.Args (J)'First + 2;
                  end if;

                  if Command.Args (J).all (Index) = 's' then
                     List := Get_Source_Files (Project, Recurse);

                  elsif Command.Args (J).all (Index) = 'd' then
                     List := Source_Dirs (Project, Recurse);
                  end if;

                  if List = null then
                     return False;
                  end if;

                  if Index = Command.Args (J)'Last then
                     --  Append the list to the current arguments

                     declare
                        New_New_Args : Argument_List_Access
                          := new String_List
                          (New_Args'First .. New_Args'Last + List'Length - 1);
                     begin
                        New_New_Args (New_Args'First .. Last - 1)
                          := New_Args (New_Args'First .. Last - 1);

                        for K in List'Range loop
                           New_New_Args (Last) := new String'(List (K).all);
                           Last := Last + 1;
                        end loop;

                        Last := Last - 1;
                        Free (List);
                        Free_Array (New_Args);
                        New_Args := New_New_Args;
                     end;

                  elsif Command.Args (J).all (Index + 1) = 'f' then

                     --  Append the list to a file.
                     declare
                        File : File_Type;
                     begin
                        Create (File);

                        for K in List'Range loop
                           Put_Line (File, List (K).all);
                        end loop;

                        Free (List);
                        New_Args (Last) := new String'(Name (File));
                        Close (File);
                     end;
                  end if;
               end if;
            end if;

            Last := Last + 1;

         end loop;

         --  Arguments have been substituted, launch the command.

         if Command.GPS_Command then
            Interpret_Command
              (Command.Kernel,
               Command_To_String (Command.Command.all, New_Args.all));
            Success := True;
         else
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

      elsif Command.GPS_Command then
         Interpret_Command (Command.Kernel, Command.Command.all);
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

      return Success;

   exception
      when E : others =>
         Insert (Command.Kernel,
                 -("An unexpect error occured while executing the customer"
                   & " command. See the log file for more information."),
                 Mode => Error);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Execute;

end Commands.Custom;

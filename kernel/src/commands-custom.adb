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

with Basic_Types;          use Basic_Types;
with Projects;             use Projects;
with Projects.Registry;    use Projects.Registry;
with String_Utils;         use String_Utils;
with VFS;                  use VFS;

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Traces;               use Traces;

package body Commands.Custom is

   Me : constant Debug_Handle := Create ("Commands.Custom");

   type Boolean_Array is array (Natural range <>) of Boolean;

   function Commands_Count
     (Command : access Custom_Command'Class) return Natural;
   --  Return the number of commands that will be executed as part of Command.

   procedure Check_Save_Output
     (Command : access Custom_Command'Class;
      Save_Output : out Boolean_Array);
   --  Compute whether we should save the output of each commands. This depends
   --  on whether later commands reference this output through %1, %2,...

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
     (Command : access Custom_Command'Class;
      Save_Output : out Boolean_Array)
   is
      N     : Node_Ptr;
      Index : Natural := 1;

      function Substitution (Param : String) return String;
      --  Check whether the command has a '%' + digit parameter

      function Substitution (Param : String) return String is
         Sub_Index : constant Natural := Safe_Value (Param, Default => 0);
      begin
         if Sub_Index <= Index - 1
           and then Sub_Index >= 1
         then
            Save_Output (Index - Sub_Index) := True;
         end if;
         return "";
      end Substitution;

   begin
      Save_Output := (others => False);

      if Command.XML /= null then
         N := Command.XML;
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

            Index := Index + 1;
            N     := N.Next;
         end loop;
      end if;
   end Check_Save_Output;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Command) is
   begin
      Free (X.Command);
      Free (X.XML);
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String;
      Script       : Glide_Kernel.Scripts.Scripting_Language) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Command := new String'(Command);
      Item.Script := Script;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : Glib.Xml_Int.Node_Ptr)
   is
      Node, Previous : Node_Ptr;
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;

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

   -------------
   -- Execute --
   -------------

   function Execute
     (Command       : access Custom_Command;
      Event         : Gdk.Event.Gdk_Event) return Command_Return_Type
   is
      pragma Unreferenced (Event);

      Count : constant Natural := Commands_Count (Command);
      Save_Output : Boolean_Array (1 .. Count);
      --  Whether we should save the output of the nth-command

      Outputs : Argument_List (1 .. Count);
      --  The output of the various commands, if it was saved.

      Context  : constant Selection_Context_Access :=
        Get_Current_Context (Command.Kernel);
      Cmd_Index : Natural;
      Success   : Boolean;

      function Substitution (Param : String) return String;
      --  Substitution function for the various '%...' parameters
      --  Index is the number of the current command we are executing

      function Project_From_Param (Param : String) return Project_Type;
      --  Return the project from the parameter. Parameter is the string
      --  following the '%' sign

      function Execute_Simple_Command
        (Script       : Scripting_Language;
         Command_Line : String) return Boolean;
      --  Execute a single command, and return whether it succeeded.
      --  Index is the number of the current command we are executing

--        procedure Unchecked_Free is new Ada.Unchecked_Deallocation
--          (Object => String_List, Name => String_List_Access);

      ------------------------
      -- Project_From_Param --
      ------------------------

      function Project_From_Param (Param : String) return Project_Type is
         File : File_Selection_Context_Access;
         Project : Project_Type := No_Project;
      begin
         if Param (Param'First) = 'P' then
            Project := Get_Project (Command.Kernel);

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
               Root_If_Not_Found => True);
         end if;

         if Project = No_Project then
            Insert (Command.Kernel,
                    -"Command not executed: it requires a project",
                    Mode => Error);
         end if;

         return Project;
      end Project_From_Param;

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
            if Context /= null
              and then Context.all in File_Selection_Context'Class
              and then Has_File_Information
                (File_Selection_Context_Access (Context))
            then
               File := File_Selection_Context_Access (Context);

               if Param = "f" then
                  return Base_Name (File_Information (File));
               else
                  return Full_Name (File_Information (File)).all;
               end if;

            else
               Insert (Command.Kernel,
                       -"Command not executed: it requires a file",
                       Mode => Error);
               Success := False;
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
               Success := False;
               raise Invalid_Substitution;
            end if;

         elsif Param (Param'First) = 'P' or else Param (Param'First) = 'p' then

            Project := Project_From_Param (Param);
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
            if Num <= Cmd_Index - 1
              and then Num >= 1
            then
               if Outputs (Cmd_Index - Num) = null then
                  return "";
               else
                  --  Remove surrounding quotes if any. This is needed so that
                  --  for instance of the function get_attributes_as_string
                  --  from Python can be used to call an external tool with
                  --  switches propertly interpreted.

                  declare
                     Output : String renames Outputs (Cmd_Index - Num).all;
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
        (Script       : Scripting_Language;
         Command_Line : String) return Boolean
      is
         --  Perform arguments substitutions for the command.
         Subst_Cmd_Line : constant String := Substitute
           (Command_Line,
            Substitution_Char => '%',
            Callback          => Substitution'Unrestricted_Access,
            Recursive         => False);
         Args : String_List_Access;

      begin
         --  If substitution failed
         if not Success then
            null;

         elsif Script /= null then
            Trace (Me, "Executing internal command " & Subst_Cmd_Line);

            if Save_Output (Cmd_Index) then
               Outputs (Cmd_Index) := new String'
                 (Execute_Command (Script, Subst_Cmd_Line));
            else
               Execute_Command
                 (Script, Subst_Cmd_Line, Display_In_Console => True);
            end if;

         else
            Trace (Me, "Executing external command " & Command_Line);

            Args := Argument_String_To_List (Subst_Cmd_Line);
            --  ??? Should save output
            Launch_Process
              (Command.Kernel,
               Args (Args'First).all,
               Args (Args'First + 1 .. Args'Last),
               "",
               null,
               null,
               "",
               Success);
            Free (Args);
         end if;

         return Success;

      exception
         when E : others =>
            Insert (Command.Kernel,
                    -("An unexpected error occured while executing the custom"
                      & " command. See the log file for more information."),
                    Mode => Error);
            Trace (Me, "Unexpected exception: " & Exception_Information (E));
            return False;
      end Execute_Simple_Command;


      N : Node_Ptr;
   begin
      Cmd_Index := 1;
      Check_Save_Output (Command, Save_Output);
      Success := True;

      if Command.Command /= null then
         Success := Execute_Simple_Command
           (Command.Script, Command.Command.all);

      else
         N := Command.XML;
         while Success and then N /= null loop
            if To_Lower (N.Tag.all) = "shell" then
               Success := Execute_Simple_Command
                 (Lookup_Scripting_Language
                    (Command.Kernel,
                     Get_Attribute (N, "lang", GPS_Shell_Name)),
                  N.Value.all);

            elsif To_Lower (N.Tag.all) = "external" then
               Success := Execute_Simple_Command
                 (null, N.Value.all);
            end if;

            N := N.Next;
            Cmd_Index := Cmd_Index + 1;
         end loop;
      end if;

      Basic_Types.Free (Outputs);

      Command_Finished (Command, Success);

      if Success then
         return Commands.Success;
      else
         return Failure;
      end if;
   end Execute;

end Commands.Custom;

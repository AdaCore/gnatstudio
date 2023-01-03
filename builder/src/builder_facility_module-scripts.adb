------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
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

with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;

with Build_Configurations;       use Build_Configurations;
with Commands.Builder.Scripts;   use Commands.Builder.Scripts;
with GPS.Core_Kernels;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Macros;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Intl;                   use GPS.Intl;

with Build_Command_Utils;        use Build_Command_Utils;
with Remote;                     use Remote;

package body Builder_Facility_Module.Scripts is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Shell command handler

   -------------------
   -- Shell_Handler --
   -------------------

   procedure Shell_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Target_Class : constant Class_Type :=
                       Get_Target_Class (Get_Kernel (Data));
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "hide" then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Get_Target_Name (Inst);
            Ref  : constant Target_Access
              := Get_Target_From_Name (Registry, Name);
         begin
            if Ref = null then
               Set_Error_Msg (Data, -"Invalid target");
            elsif Get_Properties (Ref).Visible then
               Visible (Ref, False);
               Refresh_Graphical_Elements (Ref);
            end if;
         end;

      elsif Command = "show" then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Get_Target_Name (Inst);
            Ref  : constant Target_Access
              := Get_Target_From_Name (Registry, Name);
         begin
            if Ref = null then
               Set_Error_Msg (Data, -"Invalid target");
            elsif not Get_Properties (Ref).Visible then
               Visible (Ref, True);

               Refresh_Graphical_Elements (Ref);
            end if;
         end;

      elsif Command = "remove" then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Get_Target_Name (Inst);
         begin
            if Name = "" then
               Set_Error_Msg (Data, -"Invalid target");
               return;
            end if;

            Remove_Target (Registry, Name);

            Refresh_All_Graphical_Elements;
         end;

      elsif Command = "clone" then
         declare
            Inst : constant Class_Instance := Nth_Arg (Data, 1, Target_Class);
            Name : constant String := Get_Target_Name (Inst);
            New_Name     : constant String := Nth_Arg (Data, 2);
            New_Category : constant String := Nth_Arg (Data, 3);
         begin
            if Name = "" then
               Set_Error_Msg (Data, -"Invalid target");
               return;
            end if;

            Duplicate_Target (Registry, Name, New_Name, New_Category);

            Refresh_All_Graphical_Elements;
         end;
      elsif Command = "get_command_line" then
         declare
            Inst     : constant Class_Instance :=
                         Nth_Arg (Data, 1, Target_Class);
            Name     : constant String := Get_Target_Name (Inst);
            Target   : constant Target_Access
              := Get_Target_From_Name (Registry, Name);
            Cmd_Line : constant String_List :=
                         Get_Command_Line_Unexpanded (Target);
         begin
            Data.Set_Return_Value_As_List;

            for Arg of Cmd_Line loop
               Data.Set_Return_Value (Arg.all);
            end loop;
         end;

      elsif Command = "get_expanded_command_line" then
         declare
            Inst     : constant Class_Instance :=
                         Nth_Arg (Data, 1, Target_Class);
            Name     : constant String := Get_Target_Name (Inst);
            Target   : constant Target_Access :=
              Get_Target_From_Name (Registry, Name);
            Cmd_Line : constant String_List :=
              Get_Command_Line_Unexpanded (Target);

            Server  : constant Server_Type := Get_Server
              (Builder_Facility_Module.Registry,
               Kernel.Get_Build_Mode,
               Target);
            Subdir  : constant Filesystem_String := Get_Mode_Subdir
              (Builder_Facility_Module.Registry, Kernel.Get_Build_Mode);
            Project : constant Project_Type :=
              Kernel.Get_Project_Tree.Root_Project;
            Main    : Virtual_File;
            Result  : Expansion_Result;
            Failed  : Boolean;
         begin
            declare
               List : String_List_Access :=
                 Project.Attribute_Value (GNATCOLL.Projects.Main_Attribute);
            begin
               if List /= null then
                  Main := GNATCOLL.VFS.Create (+(List (List'First).all));
               end if;
               Free (List);
            end;

            Data.Set_Return_Value_As_List;

            for Arg of Cmd_Line loop
               Expand_Arg
                 (GPS.Core_Kernels.Core_Kernel (Kernel), Target, Arg.all,
                  Server, No_File, Main, Project, Subdir, Failed, Result);

               if Failed then
                  Data.Set_Return_Value (Arg.all);

               else
                  for J in 0 .. Args_Length (Result.Args) loop
                     declare
                        V : constant String := Nth_Arg (Result.Args, J);
                     begin
                        if V /= "" then
                           Data.Set_Return_Value (V);
                        end if;
                     end;
                  end loop;
               end if;
            end loop;
         end;

      elsif Command = "set_build_mode" then
         Kernel.Set_Build_Mode (Nth_Arg (Data, 1, ""));
      elsif Command = "expand_macros" then
         declare
            Param_List : List_Instance'Class := Nth_Arg (Data, 1);
            Done       : aliased Boolean := False;
         begin
            Data.Set_Return_Value_As_List;

            for J in 1 .. Param_List.Number_Of_Arguments loop
               declare
                  Param : constant String := Param_List.Nth_Arg (J);
               begin
                  if Param /= "" and then Param (Param'First) = '%' then
                     declare
                        --  We found a macro => expand it
                        Expanded : constant String :=
                          GPS.Kernel.Macros.Substitute
                            (Param     =>
                               Param (Param'First + 1 .. Param'Last),
                             Context   => Kernel.Get_Current_Context,
                             Quoted    => False,
                             Done      => Done'Access,
                             For_Shell => False);
                     begin
                        if Done then
                           Data.Set_Return_Value (Expanded);
                        else
                           --  Expansion failed => raise an exception
                           Set_Error_Msg (Data, -"Can't expand " & Param);
                        end if;
                     end;
                  else
                     Data.Set_Return_Value (Param);
                  end if;
               end;
            end loop;
            Free (Param_List);
         end;
      end if;
   end Shell_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : GPS.Kernel.Kernel_Handle) is
      Target_Class : constant Class_Type := Get_Target_Class (Kernel);
   begin
      Commands.Builder.Scripts.Register_Commands (Kernel);

      Register_Command
        (Kernel, "hide",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "show",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "remove",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "clone",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "get_command_line",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "get_expanded_command_line",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Target_Class,
         Handler      => Shell_Handler'Access);

      Register_Command
        (Kernel, "expand_macros",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Target_Class,
         Handler       => Shell_Handler'Access,
         Static_Method => True);

      --  Global commands

      Register_Command (Kernel        => Kernel,
                        Command       => "set_build_mode",
                        Minimum_Args  => 1,
                        Maximum_Args  => 1,
                        Handler       => Shell_Handler'Access);
   end Register_Commands;

end Builder_Facility_Module.Scripts;

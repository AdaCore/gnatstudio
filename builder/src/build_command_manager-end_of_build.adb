------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with Build_Configurations;             use Build_Configurations;
with Extending_Environments;           use Extending_Environments;
with Remote;                           use Remote;
with Build_Configurations.Gtkada;      use Build_Configurations.Gtkada;
with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;                use GNATCOLL.Projects;

package body Build_Command_Manager.End_Of_Build is

   procedure Expand_Command_Line
     (Builder : Builder_Context;
      Build   : in out Build_Information);
   --  Expand all macros using parameters from Build and assign result
   --  to Build.Full property.

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   procedure Expand_Command_Line
     (Builder : Builder_Context;
      Build   : in out Build_Information)
   is
      Mode   : constant String := To_String (Build.Mode);
      Server : constant Server_Type := Get_Server
        (Builder.Registry, Mode, Build.Target);
      Subdir : constant Filesystem_String :=
        Get_Mode_Subdir (Builder.Registry, Mode);

      Directory : constant Virtual_File := Build.Full.Dir;

      function Expand_Cmd_Line (CL : String) return String;
      --  Callback for Single_Target_Dialog

      ---------------------
      -- Expand_Cmd_Line --
      ---------------------

      function Expand_Cmd_Line (CL : String) return String is
         CL_Args   : Argument_List_Access := Argument_String_To_List (CL);
         Mode_Args : Argument_List_Access :=
           Apply_Mode_Args (Builder.Registry,
                            Get_Model (Build.Target),
                            Mode,
                            CL_Args.all);
         Res       : constant Expansion_Result :=
           Expand_Command_Line
             (Builder,
              Mode_Args.all & Build.Extra_Args.all,
              Build.Target,
              Server,
              Build.Force_File, Build.Main, Subdir, Build.Shadow,
              Simulate       => True,
              Background_Env => Build.Env);

      begin
         Free (CL_Args);
         Free (Mode_Args);
         return To_Display_String (Res.Args);
      end Expand_Cmd_Line;

      Command_Line   : Argument_List_Access;
   begin
      begin
         if (not Build.Shadow)
           and then (not Build.Background)
           and then
            (Build.Dialog = Force_Dialog
              or else (Build.Dialog = Force_Dialog_Unless_Disabled_By_Target
                        and then Get_Properties (Build.Target).Launch_Mode
                        /= Manually_With_No_Dialog)
              or else (Build.Dialog = Default
                        and then Get_Properties (Build.Target).Launch_Mode
                        = Manually_With_Dialog))
         then
            --  Use the single target dialog to get the unexpanded command line
            Single_Target_Dialog
              (Registry        => Builder.Registry,
               Parent          => Get_Main_Window
                                    (Kernel_Handle (Builder.Kernel)),
               Target          => Get_Name (Build.Target),
               History         => Get_History (Kernel_Handle (Builder.Kernel)),
               Expand_Cmd_Line => Expand_Cmd_Line'Unrestricted_Access,
               Result          => Command_Line);

            if Command_Line = null then
               --  The dialog was cancelled: return
               Build.Launch := False;
               return;
            end if;

            declare
               CL_Mode : Argument_List_Access :=
                 Apply_Mode_Args (Builder.Registry, Get_Model (Build.Target),
                                  Mode, Command_Line.all);
            begin
               Build.Full := Expand_Command_Line
                 (Builder, CL_Mode.all & Build.Extra_Args.all, Build.Target,
                  Server, Build.Force_File, Build.Main, Subdir, False, False,
                  Build.Env);
               Free (Command_Line);
               Free (CL_Mode);
            end;

         else
            declare
               CL      : constant Argument_List :=
                 Get_Command_Line_Unexpanded (Builder.Registry, Build.Target);
               CL_Mode : Argument_List_Access :=
                 Apply_Mode_Args (Builder.Registry,
                                  Get_Model (Build.Target),
                                  Mode,
                                  CL);
            begin
               --  Sanity check that the command line contains at least one
               --  item (the command itself). It can happen that this is not
               --  the case if the user has modified the command by hand.

               if CL_Mode'Length = 0 then
                  Builder.Kernel.Messages_Window.Insert
                    (-"Command line is empty for target: " &
                       Get_Name (Build.Target),
                     Mode => Error);
                  Free (CL_Mode);
                  Build.Launch := False;
                  return;
               end if;

               --  Expand the command line

               Build.Full := Expand_Command_Line
                 (Builder, CL_Mode.all & Build.Extra_Args.all, Build.Target,
                  Server, Build.Force_File,
                  Build.Main, Subdir, Build.Background, False,
                  Build.Env);

               Free (CL_Mode);
            end;
         end if;
      end;

      --  Update Build.Full.Dir to not empty value
      if Build.Full.Dir = No_File then
         if Directory /= No_File then
            Build.Full.Dir := Directory;
         else
            Build.Full.Dir :=
              Builder.Kernel.Registry.Tree.Root_Project.Project_Path.Dir;
         end if;
      end if;
   end Expand_Command_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      Build  : Build_Information := Self.Builder.Get_Last_Build;
   begin
      Expand_Command_Line (Self.Builder, Build);

      if Build.Launch and then not Is_Run (Build.Target) then
         Build.Launch := GPS.Kernel.Compilation_Starting
           (Handle     => Kernel_Handle (Self.Builder.Kernel),
            Category   => To_String (Build.Category),
            Quiet      => Build.Quiet,
            Shadow     => Build.Shadow,
            Background => Build.Background);
      end if;

      --  Update Builder.Last_Build with new Full expanded command line
      Self.Builder.Set_Last_Build (Build);

      return new Parser'(Child   => Child,
                         Builder => Self.Builder,
                         Build   => Self.Builder.Get_Last_Build);
   end Create;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self    : access Output_Parser_Fabric;
      Builder : Builder_Context) is
   begin
      Self.Builder := Builder;
   end Set;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Parser;
      Status  : Integer;
      Command : Command_Access)
   is
      Kernel : Kernel_Handle;
   begin
      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);

      if Is_Run (Self.Build.Target) then
         return;
      end if;

      Kernel := Kernel_Handle (Self.Builder.Kernel);

      Destroy (Self.Build.Env);

      if Self.Build.Background then
         --  We remove the previous background build data messages only when
         --  the new background build is completed.

         Get_Messages_Container (Kernel).Remove_Category
           (Previous_Background_Build_Id (Self.Builder),
            Background_Message_Flags);

         Background_Build_Finished (Self.Builder);
      end if;

      --  ??? should also pass the Status value to Compilation_Finished
      --  and to the corresponding hook

      Compilation_Finished
        (Kernel,
         To_String (Self.Build.Category),
         Get_Name (Self.Build.Target),
         To_String (Self.Build.Mode),
         Self.Build.Shadow,
         Self.Build.Background,
         Status);
   end End_Of_Stream;

end Build_Command_Manager.End_Of_Build;

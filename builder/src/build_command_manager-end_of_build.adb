------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Location_View;
with GPS.Main_Window;                  use GPS.Main_Window;
with Build_Configurations;             use Build_Configurations;
with Command_Lines;                    use Command_Lines;
with Commands.Builder;
with Extending_Environments;           use Extending_Environments;
with Remote;                           use Remote;
with Build_Configurations.Gtkada;      use Build_Configurations.Gtkada;
with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Utils;           use GNATCOLL.Scripts.Utils;
with Gtk.Window;                       use Gtk.Window;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;

package body Build_Command_Manager.End_Of_Build is

   procedure Local_Expand_Command_Line
     (Builder : Builder_Context;
      Build   : in out Build_Information);
   --  Expand all macros using parameters from Build and assign result
   --  to Build.Full property.

   -------------------------------
   -- Local_Expand_Command_Line --
   -------------------------------

   procedure Local_Expand_Command_Line
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

      function Should_Display_Dialog return Boolean;
      --  Return whether we should display the target command line dialog

      ---------------------
      -- Expand_Cmd_Line --
      ---------------------

      function Expand_Cmd_Line (CL : String) return String is
         --  GNAT.OS_Lib.Argument_String_To_List does not properly handle
         --  quotes, as in  %python("foo")
         CL_Args    : Argument_List_Access :=
           Argument_String_To_List_With_Triple_Quotes (CL);
         Mode_Args  : Command_Line :=
           Build.Target.Apply_Mode_Args (Mode, CL_Args.all);
         Res : Expansion_Result;

      begin
         Mode_Args.Append_Switches (Build.Extra_Args.all);
         Res := Expand_Command_Line
           (Builder,
            Mode_Args,
            Build.Target,
            Server,
            Build.Force_File,
            Main           => Build.Main,
            Main_Project   => Build.Main_Project,
            Subdir         => Subdir,
            Background     => Build.Shadow,
            Simulate       => True,
            Background_Env => Build.Env);

         Free (CL_Args);
         return To_Display_String (Res.Args);
      end Expand_Cmd_Line;

      ---------------------------
      -- Should_Display_Dialog --
      ---------------------------

      function Should_Display_Dialog return Boolean is
      begin
         if Build.Shadow
           or else Build.Background
         then
            return False;
         end if;

         case Build.Dialog is
            when Force_Dialog =>
               return True;

            when Force_No_Dialog =>
               return False;

            when Force_Dialog_Unless_Disabled_By_Target =>
               return Get_Properties (Build.Target).Launch_Mode /=
                 Manually_With_No_Dialog;

            when Default =>
               case Get_Properties (Build.Target).Launch_Mode is
                  when Manually =>
                     return Build.Via_Menu;

                  when Manually_With_Dialog =>
                     return True;

                  when Manually_With_No_Dialog
                     | On_File_Save
                     | In_Background =>
                     return False;
               end case;
         end case;
      end Should_Display_Dialog;

      procedure Set_Size (W : not null access Gtk_Window_Record'Class);
      --  Set the size of the window based on previous sizes set by the user

      procedure Set_Size (W : not null access Gtk_Window_Record'Class) is
      begin
         Set_Default_Size_From_History
            (W, "builder-single-target", Kernel_Handle (Builder.Kernel),
             800, 500);
      end Set_Size;

      Command_Line   : Argument_List_Access;

   begin
      if Should_Display_Dialog then
         --  Use the single target dialog to get the unexpanded command line
         Single_Target_Dialog
           (Registry        => Builder.Registry,
            Set_Default_Size_From_History => Set_Size'Access,
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
            CL_Mode    : Command_Lines.Command_Line :=
              Build.Target.Apply_Mode_Args (Mode, Command_Line.all);
         begin
            CL_Mode.Append_Switches (Build.Extra_Args.all);
            Build.Full := Expand_Command_Line
              (Builder,
               CL_Mode,
               Build.Target,
               Server,
               Force_File     => Build.Force_File,
               Main           => Build.Main,
               Main_Project   => Build.Main_Project,
               Subdir         => Subdir,
               Background     => False,
               Simulate       => False,
               Background_Env => Build.Env);
            Free (Command_Line);
         end;

      else
         declare
            CL         : constant Argument_List :=
                        Get_Command_Line_Unexpanded (Build.Target);
            CL_Mode    : Command_Lines.Command_Line :=
              Build.Target.Apply_Mode_Args (Mode, CL);
         begin
            --  Sanity check that the command line contains at least one
            --  item (the command itself). It can happen that this is not
            --  the case if the user has modified the command by hand.

            if CL_Mode.Is_Empty then
               Builder.Kernel.Messages_Window.Insert
                 (-"Command line is empty for target: " &
                    Get_Name (Build.Target),
                  Mode => Error);
               Build.Launch := False;
               return;
            end if;

            --  Expand the command line

            CL_Mode.Append_Switches (Build.Extra_Args.all);

            Build.Full := Expand_Command_Line
              (Builder, CL_Mode, Build.Target,
               Server, Build.Force_File,
               Main           => Build.Main,
               Main_Project   => Build.Main_Project,
               Subdir         => Subdir,
               Background     => Build.Background,
               Simulate       => False,
               Background_Env => Build.Env);
         end;
      end if;

      --  Update Build.Full.Dir to not empty value
      if Build.Full.Dir = No_File then
         if Directory /= No_File then
            Build.Full.Dir := Directory;
         else
            Build.Full.Dir :=
              Builder.Kernel.Registry.Tree.Root_Project.Project_Path.Dir;
         end if;
      end if;
   end Local_Expand_Command_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      Build      : Build_Information := Self.Builder.Get_Last_Build;
      Force_File : Virtual_File;
   begin
      Local_Expand_Command_Line (Self.Builder, Build);

      if Build.Full.Args = Empty_Command_Line then
         Build.Launch := False;
      elsif Build.Launch
        and then not Is_Run (Build.Target)
        and then not Uses_Python (Build.Target)
      then
         Build.Launch := Compilation_Starting_Hook.Run
           (Kernel     => Kernel_Handle (Self.Builder.Kernel),
            Category   => To_String (Build.Category),
            Quiet      => Build.Quiet,
            Shadow     => Build.Shadow,
            Background => Build.Background);
      end if;

      Force_File := Self.Builder.Get_Last_Build.Force_File;
      if Force_File = No_File then
         Force_File := Build.Force_File;
      end if;

      --  Update Builder.Last_Build with new Full expanded command line
      Self.Builder.Set_Last_Build (Build);

      return new Parser'(Child      => Child,
                         Builder    => Self.Builder,
                         Build      => Self.Builder.Get_Last_Build,
                         Force_File => Force_File);
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
      Command : access Root_Command'Class)
   is
      Kernel : Kernel_Handle;
   begin
      Tools_Output_Parser (Self.all).End_Of_Stream (Status, Command);

      --  Run the On_Exit program, if any

      if Self.Build.On_Exit /= null then
         declare
            Script  : constant Scripting_Language :=
              Get_Script (Self.Build.On_Exit.all);
            Args    : Callback_Data'Class := Create (Script, 1);
            Ignored : Boolean;
         begin
            Set_Nth_Arg (Args, 1, Status);
            Ignored := Execute (Self.Build.On_Exit, Args);
         end;
      end if;

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

      Compilation_Finished_Hook.Run
        (Kernel,
         To_String (Self.Build.Category),
         Get_Name (Self.Build.Target),
         To_String (Self.Build.Mode),
         Self.Build.Shadow,
         Self.Build.Background,
         Status);

      --  Reopen Locations view for same file
      if Self.Force_File /= No_File
        and then Get_Messages
          (Get_Messages_Container (Kernel),
           Ada.Strings.Unbounded.To_Unbounded_String
             (Commands.Builder.Error_Category),
           Self.Force_File)'Length > 0
      then
         GPS.Location_View.Expand_File
           (GPS.Location_View.Get_Or_Create_Location_View (Kernel),
            Commands.Builder.Error_Category,
            Self.Force_File);
      end if;
   end End_Of_Stream;

end Build_Command_Manager.End_Of_Build;

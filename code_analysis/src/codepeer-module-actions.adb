------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2014-2024, AdaCore                     --
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

with GNAT.Strings;      use GNAT.Strings;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with VSS.Strings.Conversions;

with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Actions;  use GPS.Kernel.Actions;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with GPS.Kernel.Messages; use GPS.Kernel.Messages;
with GPS.Kernel.Preferences;
with GPS.Kernel.Project;  use GPS.Kernel.Project;

with Build_Command_Utils;  use Build_Command_Utils;
with Build_Configurations; use Build_Configurations;
with String_Utils;         use String_Utils;

with CodePeer.Module.Bridge;
with CodePeer.Module.Editors;
with CodePeer.Shell_Commands;
with Projects.Views;
with Gtkada.File_Selector;

package body CodePeer.Module.Actions is

   Switches_Attribute : constant Attribute_Pkg_List :=
     Build (CodePeer.GPR_Name, "Switches");

   function Is_Show_Hide_Allowed
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Returns True when show/hide annotations is allowed.

   function Execute_Internal_With_Baseline
     (Context   : Interactive_Command_Context;
      Target_ID : String;
      Title     : String;
      Action    : CodePeer_Action)
      return Command_Return_Type;
   --  Open a dialog to select a baseline and execute a BuiltTarget with
   --  the selected file.

   function Get_Codepeer_Messages
     (Messages : GPS.Kernel.Messages.Message_Array)
      return CodePeer.Message_Vectors.Vector;
   --  Get CodePeer messages form the array

   ---------------------------
   -- Get_Codepeer_Messages --
   ---------------------------

   function Get_Codepeer_Messages
     (Messages : GPS.Kernel.Messages.Message_Array)
      return CodePeer.Message_Vectors.Vector
   is
      Vector : CodePeer.Message_Vectors.Vector;
   begin
      for Message of Messages loop
         if Message.all in CodePeer.Message'Class then
            Vector.Append (CodePeer.Message_Access (Message));
         end if;
      end loop;

      return Vector;
   end Get_Codepeer_Messages;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Analyze_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

   begin
      Self.Module.Review
        (False,
         "Run "
           & VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
           & "...");

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Analyze_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

   begin
      Self.Module.Review
        (True,
         "Run "
           & VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name));

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Analyze_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

   begin
      --  Clean up all the messages from the previous run of CodePeer on
      --  one file.

      Get_Messages_Container (Kernel).Remove_Category
        (CodePeer.Module_Name & " (one file)", Flags => (others => True));

      --  Run the Build Target
      Module.Review
        (Force        => True,
         Build_Target =>
           "Run "
             & VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
             & " File");

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Analyze_File_By_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Build_Target : constant String :=
        "Run "
          & VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
          & " File By File";

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project  : constant Project_Type  := Get_Project (Kernel);
      Builder  : constant Builder_Context := Builder_Context
        (Kernel.Module (Builder_Context_Record'Tag));
      Switches : String_List_Access;

   begin
      if Project.Has_Attribute (Switches_Attribute) then
         Switches := Project.Attribute_Value (Switches_Attribute);
         Set_Project_Switches
           (Get_Target_From_Name (Builder.Registry, Build_Target),
            To_String (Switches.all));
         Free (Switches);
      end if;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel,
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, Build_Target),
         Force       => False,
         Synchronous => False);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Display_Code_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

   begin
      if CodePeer.Is_GNATSAS then
         --  Generate the report for gnatsas
         Module.Action := Load_UI;
         CodePeer.Shell_Commands.Build_Target_Execute
           (Kernel_Handle (Module.Kernel),
            CodePeer.Shell_Commands.Build_Target
              (Module.Get_Kernel, "Run GNATSAS Report"),
            Force           => True,
            Build_Mode      => CodePeer.Build_Mode,
            Synchronous     => False,
            Dir             => CodePeer_Object_Directory (Module.Kernel),
            Preserve_Output => True);
      else
         CodePeer.Module.Bridge.Inspection (Self.Module, False);
      end if;
      return Success;
   end Execute;

   ------------------------------------
   -- Execute_Internal_With_Baseline --
   ------------------------------------

   function Execute_Internal_With_Baseline
     (Context   : Interactive_Command_Context;
      Target_ID : String;
      Title     : String;
      Action    : CodePeer_Action)
      return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir    : constant Virtual_File :=
        CodePeer.Module.Codepeer_SAM_Directory (Kernel);
      File   : constant Virtual_File :=
        Gtkada.File_Selector.Select_File
          (Title             => -Title,
           Base_Directory    => Dir,
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog =>
             GPS.Kernel.Preferences.Use_Native_Dialogs.Get_Pref,
           Kind              => Gtkada.File_Selector.Open_File,
           File_Pattern      => "*.sam;*",
           Pattern_Name      => -"SAM files;All files",
           History           => Get_History (Kernel));
   begin
      if File /= No_File then
         Module.Action := Action;
         CodePeer.Shell_Commands.Build_Target_Execute
           (Kernel_Handle (Module.Kernel),
            CodePeer.Shell_Commands.Build_Target
              (Module.Get_Kernel, Target_ID),
            Force           => True,
            File            => File,
            Build_Mode      => CodePeer.Build_Mode,
            Synchronous     => False,
            Dir             => Dir,
            Preserve_Output => True);
      end if;
      return Success;
   end Execute_Internal_With_Baseline;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Display_Baseline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      return
        Execute_Internal_With_Baseline
          (Context   => Context,
           Target_ID => "Load GNATSAS Report",
           Title     => "Select Report File",
           Action    => Load_UI);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Baseline_Bump_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Module.Action := Report;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel_Handle (Module.Kernel),
         CodePeer.Shell_Commands.Build_Target
           (Module.Get_Kernel, "GNATSAS Baseline Bump"),
         Force           => True,
         Build_Mode      => CodePeer.Build_Mode,
         Synchronous     => False,
         Dir             => CodePeer_Object_Directory (Module.Kernel),
         Preserve_Output => True);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Baseline_Set_Baseline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      return
        Execute_Internal_With_Baseline
          (Context   => Context,
           Target_ID => "GNATSAS Baseline Set Baseline",
           Title     => "Select New Baseline",
           Action    => Report);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Baseline_Set_Current_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      return
        Execute_Internal_With_Baseline
          (Context   => Context,
           Target_ID => "GNATSAS Baseline Set Current",
           Title     => "Select Run To Replace Current Run",
           Action    => Report);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Display_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
   begin
      Open_HTML_Report (Get_Kernel (Context.Context));

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Generate_CSV_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Build_Target : constant String := "Generate CSV Report";

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project  : constant Project_Type  := Get_Project (Kernel);
      Builder  : constant Builder_Context := Builder_Context
        (Kernel.Module (Builder_Context_Record'Tag));
      Switches : String_List_Access;

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Object_Dir : constant Virtual_File :=
        CodePeer_Object_Directory (Project);

   begin
      if Project.Has_Attribute (Switches_Attribute) then
         Switches := Project.Attribute_Value (Switches_Attribute);
         Set_Project_Switches
           (Get_Target_From_Name (Builder.Registry, Build_Target),
            To_String (Switches.all));
         Free (Switches);
      end if;

      Self.Module.Inspection_File :=
        Object_Dir.Create_From_Dir (+CodePeer.Package_Name & ".csv");
      Self.Module.Action := Load_CSV;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel_Handle (Self.Module.Kernel),
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, Build_Target),
         Build_Mode  => CodePeer.Build_Mode,
         Synchronous => False,
         Dir         => Object_Dir);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Generate_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Build_Target : constant String := "Generate HTML Report";

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project  : constant Project_Type  := Get_Project (Kernel);
      Builder  : constant Builder_Context := Builder_Context
        (Kernel.Module (Builder_Context_Record'Tag));
      Switches : String_List_Access;

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Object_Dir : constant Virtual_File :=
        CodePeer_Object_Directory (Project);

   begin
      if Project.Has_Attribute (Switches_Attribute) then
         Switches := Project.Attribute_Value (Switches_Attribute);
         Set_Project_Switches
           (Get_Target_From_Name (Builder.Registry, Build_Target),
            To_String (Switches.all));
         Free (Switches);
      end if;

      Self.Module.Action := Open_HTML;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel_Handle (Self.Module.Kernel),
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, Build_Target),
         Build_Mode  => CodePeer.Build_Mode,
         Synchronous => False,
         Dir         => Object_Dir);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Generate_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

   begin
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel,
         Target_ID   =>
           CodePeer.Shell_Commands.Build_Target
             (Kernel, "Generate SCIL"),
         Force       => False,
         Synchronous => False);
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Log_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Log_File : constant Virtual_File :=
        Codepeer_Log_Directory (Kernel).Create_From_Dir
        (if Is_GNATSAS then "gnatsas.log" else "Insepection.log");

   begin
      if Log_File.Is_Regular_File then
         Open_File_Action_Hook.Run
           (Kernel       => Kernel,
            File         => Log_File,
            Project      => Project_Information (Context.Context),
            New_File     => False,
            Force_Reload => True);

         return Success;

      else
         Kernel.Insert
           (Text => -"cannot find log file: " & Log_File.Display_Full_Name,
            Mode => GPS.Kernel.Error);

         return Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Regenerate_Report_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      Kernel : constant Kernel_Handle := Kernel_Handle (Self.Module.Kernel);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);
      --  Inspection_Info_File and Review will change the builder mode
      --  so switch it here for both subprograms.

   begin
      Review
        (Self.Module,
         False,
         "Regenerate "
           & VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
           & " Report",
         Need_Reload => False);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Lock_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel       : constant Kernel_Handle := Get_Kernel (Context.Context);
      Lock_File    : constant Virtual_File :=
        Inspector_Output_Directory (Kernel).Create_From_Dir ("inspector.lock");
      Project      : constant Project_Type := Get_Project (Kernel);
      Deleted      : Boolean;
      Command      : Command_Return_Type;
      --  Try deleting lock files if they exist. If lock files remain after
      --  execution, fails. If no lock file exists, succeeds.

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Lock_File : Virtual_File;
         Command : in out Command_Return_Type);

      procedure Delete
        (Lock_File : Virtual_File;
         Command : in out Command_Return_Type)
      is
      begin
         if Is_Regular_File (Lock_File) then
            Delete (Lock_File, Deleted);

            if Deleted then
               Kernel.Insert
                 (-"deleted: " & Lock_File.Display_Full_Name);
            else
               Kernel.Insert
                 (-"could not delete: " &
                    Lock_File.Display_Full_Name);
               Command := Failure;
            end if;
         else
            Kernel.Insert
              (-"not found: " & Lock_File.Display_Full_Name);
         end if;

      end Delete;

   begin
      Kernel.Insert (-"Checking locks:");
      Command := Success;
      Delete (Lock_File, Command);

      if Is_GNATSAS then
         declare
            Lock_File_GNATSAS : constant Virtual_File :=
              Codepeer_GNATSAS_Directory (Kernel).Create_From_Dir
              ("gnatsas.lock");
         begin
            Delete (Lock_File_GNATSAS, Command);
         end;
      else
         declare
            DB_Lock_File : constant Virtual_File :=
              Codepeer_Database_Directory
                (Project).Create_From_Dir ("Sqlite.db.lock");
         begin
            Delete (DB_Lock_File, Command);
         end;
      end if;

      if Command = Success then
         Kernel.Insert
           (-"No remaining lock files, you may now run an analysis.");
      else
         Kernel.Insert
           (-("Some locks could not be deleted, try manually deleting them "
            & "before running an analysis."));
      end if;

      return Command;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Temp_SCIL : constant Filesystem_String := "Insp_";
      Obj_Dirs  : constant GNATCOLL.VFS.File_Array :=
                    Object_Path
                      (Project             => Get_Project (Kernel),
                       Recursive           => True,
                       Including_Libraries => True,
                       Exclude_Externally  => True);
      Dirs      : File_Array_Access;
      Ignore    : Boolean;

   begin
      Kernel.Insert (-"Deleting SCIL directories...");

      --  Remove all SCIL and Insp_* directories under each <obj>/codepeer dir.
      --  Ignore errors on e.g. read-only or non-existent directories.

      for Obj_Dir of Obj_Dirs loop
         begin
            Dirs := Obj_Dir.Create_From_Dir ("codepeer").Read_Dir (Dirs_Only);

         exception
            when VFS_Directory_Error =>
               Dirs := null;
         end;

         if Dirs /= null then
            for Dir of Dirs.all loop
               declare
                  Base : constant Filesystem_String := Dir.Base_Name;

               begin
                  if Base = "SCIL"
                    or else
                      (Base'Length > Temp_SCIL'Length
                       and then
                         Base (Base'First .. Base'First + Temp_SCIL'Length - 1)
                           = Temp_SCIL)
                  then
                     Remove_Dir (Dir, True, Ignore);
                  end if;
               end;
            end loop;

            Unchecked_Free (Dirs);
         end if;
      end loop;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel,
         Target_ID   =>
           CodePeer.Shell_Commands.Build_Target (Kernel, "Remove SCIL"),
         Force       => True,
         Build_Mode  => CodePeer.Build_Mode,
         Synchronous => False);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_SCIL_DB_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Obj_Dirs : constant GNATCOLL.VFS.File_Array :=
                   Object_Path
                     (Project             => Get_Project (Kernel),
                      Recursive           => True,
                      Including_Libraries => True,
                      Exclude_Externally  => True);
      Ignore   : Boolean;

   begin
      --  Remove all <obj>/codepeer dirs. Ignore errors on e.g. read-only
      --  or non-existent directories.

      for Dir of Obj_Dirs loop
         Remove_Dir (Dir, True, Ignore);
      end loop;

      Kernel.Insert
        (Text => -"Deleted all CodePeer artefacts.", Add_LF => False);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_XML_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      Kernel : constant Kernel_Handle := Kernel_Handle (Self.Module.Kernel);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

   begin
      CodePeer.Module.Bridge.Remove_Inspection_Cache_File (Self.Module);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Hide_Annotations_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      if Is_Show_Hide_Allowed (Self.Module, Context.Context) then
         declare
            Project_Node : constant Code_Analysis.Project_Access :=
                             Code_Analysis.Get_Or_Create
                               (Self.Module.Tree,
                                Projects.Views.Create_Project_View_Reference
                                  (Get_Kernel (Context.Context),
                                   Project_Information (Context.Context)));
            File_Node    : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node, File_Information (Context.Context));

         begin
            CodePeer.Module.Editors.Hide_Annotations
              (Self.Module.all, File_Node);
         end;
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Show_Annotations_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      if Is_Show_Hide_Allowed (Self.Module, Context.Context) then
         declare
            Project_Node : constant Code_Analysis.Project_Access :=
                             Code_Analysis.Get_Or_Create
                               (Self.Module.Tree,
                                Projects.Views.Create_Project_View_Reference
                                  (Get_Kernel (Context.Context),
                                   Project_Information (Context.Context)));
            File_Node    : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node, File_Information (Context.Context));

         begin
            CodePeer.Module.Editors.Show_Annotations
              (Self.Module.all, File_Node);
         end;
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Review_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      CodePeer.Module.Review_Messages
        (Self        => Self.Module,
         Messages    => Get_Codepeer_Messages
           (Messages_Information (Context.Context)),
         Need_Reload => False);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Annotate_Messages_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      CodePeer.Module.Annotate_Messages
        (Self     => Self.Module,
         Messages => Get_Codepeer_Messages
           (Messages_Information (Context.Context)));

      return Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Hide_Annotations_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Is_Show_Hide_Allowed (Filter.Module, Context) then
         return False;
      end if;

      declare
         use type GPS.Editors.Editor_Buffer'Class;

         Project_Node    : constant Code_Analysis.Project_Access :=
                             Code_Analysis.Get_Or_Create
                               (Filter.Module.Tree,
                                Projects.Views.Create_Project_View_Reference
                                  (Get_Kernel (Context),
                                   Project_Information (Context)));
         File_Node       : constant Code_Analysis.File_Access :=
           Code_Analysis.Get_Or_Create
             (Project_Node, File_Information (Context));
         Subprogram_Node : Code_Analysis.Subprogram_Access;
         Subprogram_Data : CodePeer.Subprogram_Data_Access;
         Kernel          : constant GPS.Kernel.Kernel_Handle :=
           GPS.Kernel.Get_Kernel (Context);
         Buffer          : constant GPS.Editors.Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get
             (File_Node.Name, False, False, False);

      begin
         if not File_Node.Subprograms.Is_Empty then
            Subprogram_Node :=
              Code_Analysis.Subprogram_Maps.Element
                (File_Node.Subprograms.First);
            Subprogram_Data :=
              CodePeer.Subprogram_Data_Access
                (Subprogram_Node.Analysis_Data.CodePeer_Data);

            if Buffer /= GPS.Editors.Nil_Editor_Buffer then
               if not Subprogram_Data.Mark.Is_Empty then
                  return True;
               end if;
            end if;
         end if;
      end;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Show_Annotations_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Is_Show_Hide_Allowed (Filter.Module, Context) then
         return False;
      end if;

      declare
         use type GPS.Editors.Editor_Buffer'Class;

         Project_Node    : constant Code_Analysis.Project_Access :=
                             Code_Analysis.Get_Or_Create
                               (Filter.Module.Tree,
                                Projects.Views.Create_Project_View_Reference
                                  (Get_Kernel (Context),
                                   Project_Information (Context)));
         File_Node       : constant Code_Analysis.File_Access :=
           Code_Analysis.Get_Or_Create
             (Project_Node, File_Information (Context));
         Subprogram_Node : Code_Analysis.Subprogram_Access;
         Subprogram_Data : CodePeer.Subprogram_Data_Access;
         Kernel          : constant GPS.Kernel.Kernel_Handle :=
           GPS.Kernel.Get_Kernel (Context);
         Buffer          : constant GPS.Editors.Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get
             (File_Node.Name, False, False, False);

      begin
         if not File_Node.Subprograms.Is_Empty then
            Subprogram_Node := File_Node.Subprograms.First_Element;
            Subprogram_Data :=
              CodePeer.Subprogram_Data_Access
                (Subprogram_Node.Analysis_Data.CodePeer_Data);

            if Buffer /= GPS.Editors.Nil_Editor_Buffer then
               if Subprogram_Data.Mark.Is_Empty then
                  return True;
               end if;
            end if;
         end if;
      end;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_Local_Mode_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);

      Project : constant Project_Type :=
        GPS.Kernel.Project.Get_Project (Self.Module.Kernel);

   begin
      return Codepeer_Server_URL (Project) = "";
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Review_Messages_Filter;
      Context : Selection_Context) return Boolean is
   begin
      if not Is_Show_Hide_Allowed (Filter.Module, Context) then
         return False;
      end if;

      for Message of Messages_Information (Context) loop
         if Message.all in CodePeer.Message'Class then
            return True;
         end if;
      end loop;

      return False;
   end Filter_Matches_Primitive;

   --------------------------
   -- Is_Show_Hide_Allowed --
   --------------------------

   function Is_Show_Hide_Allowed
     (Module  : CodePeer.Module.CodePeer_Module_Id;
      Context : GPS.Kernel.Selection_Context) return Boolean is
   begin
      return
        Module.Tree /= null
          and then GPS.Kernel.Contexts.Has_File_Information (Context);
   end Is_Show_Hide_Allowed;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Module : not null CodePeer_Module_Id)
   is
      Is_Local_Mode : constant Action_Filter :=
        new Is_Local_Mode_Filter (Module);
      Is_Review     : constant Action_Filter :=
        new Is_Review_Messages_Filter (Module);

   begin
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " analyze...",
         Command => new Analyze_Command (Module),
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " analyze all",
         Command => new Analyze_All_Command (Module),
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " analyze file",
         Command => new Analyze_File_Command,
         Filter  =>
           Lookup_Filter (Module.Kernel, "File")
             and Create (Language => "ada")
             and Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " display code review",
         Command => new Display_Code_Review_Command (Module));
      if CodePeer.Is_GNATSAS then
         Register_Action
           (Kernel  => Module.Kernel,
            Name    => CodePeer.Package_Name & " display baseline",
            Command => new Display_Baseline_Command (Module));
         Register_Action
           (Kernel  => Module.Kernel,
            Name    => CodePeer.Package_Name & " bump",
            Command => new Baseline_Bump_Command (Module));
         Register_Action
           (Kernel  => Module.Kernel,
            Name    => CodePeer.Package_Name & " baseline set",
            Command => new Baseline_Set_Baseline_Command (Module));
         Register_Action
           (Kernel  => Module.Kernel,
            Name    => CodePeer.Package_Name & " baseline replace",
            Command => new Baseline_Set_Current_Command (Module));
      else
         Register_Action
           (Kernel  => Module.Kernel,
            Name    => CodePeer.Package_Name & " analyze file by file",
            Command => new Analyze_File_By_File_Command,
            Filter  => Is_Local_Mode);
      end if;
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " display html",
         Command => new Display_HTML_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " generate csv",
         Command => new Generate_CSV_Command (Module),
         Filter => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " generate html",
         Command => new Generate_HTML_Command (Module),
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " generate scil",
         Command => new Generate_SCIL_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " log",
         Command => new Log_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " regenerate report",
         Command => new Regenerate_Report_Command (Module),
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " remove lock",
         Command => new Remove_Lock_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " remove scil",
         Command => new Remove_SCIL_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " remove scil and db",
         Command => new Remove_SCIL_DB_Command,
         Filter  => Is_Local_Mode);
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => CodePeer.Package_Name & " remove xml review",
         Command => new Remove_XML_Review_Command (Module),
         Filter  => Is_Local_Mode);

      --  Commands for contextual menu

      Register_Action
        (Kernel   => Module.Kernel,
         Name     => "show codepeer annotations",
         Command  => new Show_Annotations_Command (Module),
         Category =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name),
         Filter   => new Is_Show_Annotations_Filter (Module));
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Module.Kernel,
         Action => "show codepeer annotations",
         Label  =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
             & "/Show annotations");

      Register_Action
        (Kernel   => Module.Kernel,
         Name     => "hide codepeer annotations",
         Command  => new Hide_Annotations_Command (Module),
         Category =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name),
         Filter   => new Is_Hide_Annotations_Filter (Module));
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Module.Kernel,
         Action => "hide codepeer annotations",
         Label  =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
             & "/Hide annotations");

      Register_Action
        (Kernel   => Module.Kernel,
         Name     => "review codepeer messages",
         Command  => new Review_Messages_Command (Module),
         Category =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name),
         Filter   => Is_Review);
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Module.Kernel,
         Action => "review codepeer messages",
         Label  =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
             & "/Review messages");

      Register_Action
        (Kernel   => Module.Kernel,
         Name     => "annotate codepeer messages",
         Command  => new Annotate_Messages_Command (Module),
         Category =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name),
         Filter   => Is_Review);
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel => Module.Kernel,
         Action => "annotate codepeer messages",
         Label  =>
           VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
             & "/Annotate messages");
   end Register_Actions;

end CodePeer.Module.Actions;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014-2018, AdaCore                   --
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

with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Actions;  use GPS.Kernel.Actions;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with GPS.Kernel.Messages; use GPS.Kernel.Messages;
with GPS.Kernel.Project;  use GPS.Kernel.Project;
with CodePeer.Module.Bridge;
with CodePeer.Shell_Commands;

package body CodePeer.Module.Actions is

   function Inspection_Info_File
     (Kernel : not null access Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File;
   --  Returns path to Inspection_Info.xml file.

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Analyze_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

   begin
      Self.Module.Review (False, "Run CodePeer...");

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
      Self.Module.Review (True, "Run CodePeer");

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
        ("CodePeer (one file)", Flags => (others => True));

      --  Run the CodePeer target

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel,
         Target_ID   => CodePeer.Shell_Commands.Build_Target (Kernel,
           "Run CodePeer File"),
         Force       => True,
         Synchronous => False);
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

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);

   begin
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel,
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, "Run CodePeer File By File"),
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
      CodePeer.Module.Bridge.Inspection (Self.Module, False);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Display_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      Kernel    : constant Kernel_Handle := Get_Kernel (Context.Context);
      HTML_File : constant Virtual_File :=
        Get_Project (Kernel).Object_Dir.Create_From_Dir
          ("gnathub/html-report/index.html");

   begin
      if not HTML_File.Is_Regular_File then
         Kernel.Insert
           (Text => HTML_File.Display_Full_Name
            & (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);

      else
         --  ??? In the future, would be nice to provide a menu to launch
         --  the codepeer/gnathub web server and then a browser. This is
         --  more general than CodePeer since part of the gnathub integration
         --  so leave this aside for now.
         --  Html_Action_Hook.Run
         --    (Kernel, String (Full_Name (HTML_File).all));

         Kernel.Insert
           (Text => -("Please launch the codepeer web server and connect to it"
                      & " with your browser."));
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Generate_CSV_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project : constant Project_Type  := Get_Project (Kernel);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Info_File  : constant Virtual_File := Inspection_Info_File (Kernel);
      Object_Dir : constant Virtual_File :=
        CodePeer_Object_Directory (Project);

   begin
      if not Info_File.Is_Regular_File then
         Kernel.Insert
           (Text => Info_File.Display_Full_Name
            & (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);

         return Failure;
      end if;

      Self.Module.Inspection_File :=
        Object_Dir.Create_From_Dir ("codepeer.csv");
      Self.Module.Action := Load_CSV;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel_Handle (Self.Module.Kernel),
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, "Generate CSV Report"),
         Build_Mode  => "codepeer",
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
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Project : constant Project_Type  := Get_Project (Kernel);

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Info_File  : constant Virtual_File := Inspection_Info_File (Kernel);
      Object_Dir : constant Virtual_File :=
        CodePeer_Object_Directory (Project);

   begin
      if not Info_File.Is_Regular_File then
         Kernel.Insert
           (Text => Info_File.Display_Full_Name
            & (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);

         return Failure;
      end if;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel      => Kernel_Handle (Self.Module.Kernel),
         Target_ID   => CodePeer.Shell_Commands.Build_Target
           (Kernel, "Generate HTML Report"),
         Build_Mode  => "codepeer",
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
           CodePeer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
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
        Codepeer_Output_Directory (Kernel).Create_From_Dir ("Inspection.log");

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
      --  Inspection_Info_File and Review switches builder mode then necessary,
      --  so switch it here for both subprograms.

      Info_File : constant Virtual_File := Inspection_Info_File (Kernel);

   begin
      if not Info_File.Is_Regular_File then
         Kernel.Insert
           (Text => Info_File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);

      else
         Review
           (Self.Module, False, "Regenerate CodePeer Report");
      end if;
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

      Kernel    : constant Kernel_Handle := Get_Kernel (Context.Context);
      Lock_File : constant Virtual_File :=
        Codepeer_Output_Directory (Kernel).Create_From_Dir ("inspector.lock");
      Deleted   : Boolean;

   begin
      if Is_Regular_File (Lock_File) then
         Delete (Lock_File, Deleted);

         if Deleted then
            Kernel.Insert
              (-"deleted lock file: " & Lock_File.Display_Full_Name);

         else
            Kernel.Insert
              (-"could not delete lock file: " &
               Lock_File.Display_Full_Name);
         end if;

         return Success;

      else
         Kernel.Insert
           (-"no lock file found: " & Lock_File.Display_Full_Name);

         return Failure;
      end if;
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
      pragma Unreferenced (Ignore);

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
         Build_Mode  => "codepeer",
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
      pragma Unreferenced (Ignore);

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

   --------------------------
   -- Inspection_Info_File --
   --------------------------

   function Inspection_Info_File
     (Kernel : not null access Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return
        Codepeer_Output_Directory
          (Kernel).Create_From_Dir ("Inspection_Info.xml");
   end Inspection_Info_File;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Module : not null CodePeer_Module_Id) is
   begin
      Register_Action
        (Module.Kernel, "codepeer analyze...", new Analyze_Command (Module));
      Register_Action
        (Module.Kernel,
         "codepeer analyze all",
         new Analyze_All_Command (Module));
      Register_Action
        (Kernel  => Module.Kernel,
         Name    => "codepeer analyze file",
         Command => new Analyze_File_Command,
         Filter  =>
           Lookup_Filter (Module.Kernel, "File")
             and Create (Language => "ada"));
      Register_Action
        (Module.Kernel,
         "codepeer analyze file by file",
         new Analyze_File_By_File_Command);
      Register_Action
        (Module.Kernel,
         "codepeer display code review",
         new Display_Code_Review_Command (Module));
      Register_Action
        (Module.Kernel, "codepeer display html", new Display_HTML_Command);
      Register_Action
        (Module.Kernel,
         "codepeer generate csv",
         new Generate_CSV_Command (Module));
      Register_Action
        (Module.Kernel,
         "codepeer generate html",
         new Generate_HTML_Command (Module));
      Register_Action
        (Module.Kernel, "codepeer generate scil", new Generate_SCIL_Command);
      Register_Action (Module.Kernel, "codepeer log", new Log_Command);
      Register_Action
        (Module.Kernel,
         "codepeer regenerate report",
         new Regenerate_Report_Command (Module));
      Register_Action
        (Module.Kernel, "codepeer remove lock", new Remove_Lock_Command);
      Register_Action
        (Module.Kernel, "codepeer remove scil", new Remove_SCIL_Command);
      Register_Action
        (Module.Kernel,
         "codepeer remove scil and db",
         new Remove_SCIL_DB_Command);
      Register_Action
        (Module.Kernel,
         "codepeer remove xml review",
         new Remove_XML_Review_Command (Module));
   end Register_Actions;

end CodePeer.Module.Actions;

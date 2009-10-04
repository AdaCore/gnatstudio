------------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;          use GNAT.OS_Lib;

with Input_Sources.File;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Object;
with Gtk.Widget;

with Traces; use Traces;

with Basic_Types;
with GPS.Editors;
with GPS.Intl;             use GPS.Intl;
with GPS.Kernel.Contexts;  use GPS.Kernel.Contexts;
with GPS.Kernel.Console;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;   use GPS.Kernel.Project;
with GPS.Kernel.Locations; use GPS.Kernel.Locations;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Projects.Registry;

with Code_Peer.Bridge.Audit_Trail_Readers;
with Code_Peer.Bridge.Inspection_Readers;
with Code_Peer.Message_Review_Dialogs;
with Code_Peer.Module.Bridge;
with Code_Peer.Shell_Commands; use Code_Peer.Shell_Commands;
with Commands.Code_Peer;
with Code_Analysis_GUI;

package body Code_Peer.Module is

   use type GPS.Editors.Editor_Mark'Class;
   use type GPS.Editors.Editor_Buffer'Class;

   Me : constant Debug_Handle := Create ("CodePeer");
   Support_Multiple_Dirs : constant Debug_Handle :=
     Create ("CodePeer.Multiple_Dirs");

   type Module_Context is record
      Module  : Code_Peer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
      Message : Code_Peer.Message_Access;
   end record;

   Annotation_Style_Name                : constant String
     := "CodePeer editor annotations";
   High_Probability_Style_Name          : constant String
     := "CodePeer high probability messages";
   Medium_Probability_Style_Name        : constant String
     := "CodePeer medium probability messages";
   Low_Probability_Style_Name           : constant String
     := "CodePeer low probability messages";
   Informational_Probability_Style_Name : constant String
     := "CodePeer informational probability messages";

   package Context_CB is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Module_Context);

   procedure On_Hide_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Show_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Show_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Hide_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Destroy
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze All" menu item is activated

   procedure On_Quick_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Quick Analyze All" menu item is activated

   procedure On_Generate_SCIL
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Generate SCIL" menu item is activated

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "compilation_finished" hook, to schedule other tasks

   procedure On_Run_Analysis_Manually
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Run CodePeer" menu item is activated

   procedure On_Display_Code_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Display code review" menu item is activated

   procedure On_Regenerate_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Regenerate Report" menu item is activated

   procedure On_HTML_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "HTML Report" menu item is activated

   procedure On_Edit_Text_Overview
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Edit Text Overview" menu item is activated

   procedure On_Edit_Text_Listing
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Edit Text Listing" menu item is activated

   procedure On_Edit_Log
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Edit CodePeer Log" menu item is activated

   procedure On_Remove_Lock
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Remove Lock" menu item is activated

   procedure On_Remove_XML_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Advanced->Remove XML Code Review" menu item is activated

   procedure On_Criteria_Changed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Message_Reviewed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_File_Closed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been closed

   procedure On_File_Edited_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Called when a file has been opened

   procedure Review
     (Module      : Code_Peer.Module.Code_Peer_Module_Id;
      Force       : Boolean;
      Output_Only : Boolean := False;
      Quick       : Boolean := False);
   --  Launch CodePeer review.
   --  If Force is True, no dialog is displayed to change codepeer switches.
   --  If Output_Only is True, run CodePeer in "output only" mode.
   --  If Quick is True, run "Run CodePeer Quickly" target instead of
   --  "Run CodePeer"

   Code_Peer_Category_Name : constant String := "CodePeer messages";

   Module : Code_Peer_Module_Id;
   --  Global variable for store CodePeer plugin module. Used in the main menu
   --  callbacks.

   procedure Create_Library_File (Project : Projects.Project_Type);
   --  Create CodePeer library file

   -------------------------
   -- Use_CodePeer_Subdir --
   -------------------------

   function Use_CodePeer_Subdir (Kernel : Kernel_Handle) return Boolean is
      Object_Directory : constant Virtual_File :=
                           Projects.Object_Path (Get_Project (Kernel));
   begin
      return GNATCOLL.VFS.Is_Directory
        (Create_From_Dir (Object_Directory, "codepeer"));
   end Use_CodePeer_Subdir;

   -------------------------
   -- Create_Library_File --
   -------------------------

   procedure Create_Library_File (Project : Projects.Project_Type) is
      File : Ada.Text_IO.File_Type;
      Objs : constant GNATCOLL.VFS.File_Array :=
              Projects.Object_Path (Project, True, True);

   begin
      Ada.Text_IO.Create
        (File,
         Ada.Text_IO.Out_File,
         String (Codepeer_Library_File_Name (Project).Full_Name.all));

      Ada.Text_IO.Put_Line
        (File,
         "Output_Dir := """
         & (+Codepeer_Output_Directory (Project).Full_Name) & """;");
      Ada.Text_IO.New_Line (File);

      Ada.Text_IO.Put_Line
        (File,
         "Database_Dir := """
         & (+Codepeer_Database_Directory (Project).Full_Name) & """;");
      Ada.Text_IO.New_Line (File);

      if Active (Support_Multiple_Dirs) then
         for J in Objs'Range loop
            Ada.Text_IO.Put_Line
              (File,
               "Source (Directory => """ &
               String (Objs (J).Full_Name.all) & "SCIL"",");
            Ada.Text_IO.Put_Line
              (File, "        Files     => (""*.scil""),");
            Ada.Text_IO.Put_Line
              (File, "        Language  => SCIL);");
         end loop;
      else
         Ada.Text_IO.Put_Line
           (File, "Source (Directory => ""SCIL"",");
         Ada.Text_IO.Put_Line
           (File, "        Files     => (""*.scil""),");
         Ada.Text_IO.Put_Line
           (File, "        Language  => SCIL);");
      end if;

      Ada.Text_IO.Close (File);
   end Create_Library_File;

   ------------
   -- Review --
   ------------

   procedure Review
     (Module      : Code_Peer.Module.Code_Peer_Module_Id;
      Force       : Boolean;
      Output_Only : Boolean := False;
      Quick       : Boolean := False)
   is
      Mode             : constant String :=
                           Code_Peer.Shell_Commands.Get_Build_Mode
                             (Kernel_Handle (Module.Kernel));
      Project          : constant Projects.Project_Type :=
                           Get_Project (Module.Kernel);
      CodePeer_Subdir  : constant Boolean :=
                           Use_CodePeer_Subdir (Kernel_Handle (Module.Kernel));

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      Module.Action := Load_UI;
      Create_Library_File (Project);

      if Output_Only then
         Code_Peer.Shell_Commands.Build_Target_Execute
           (Kernel_Handle (Module.Kernel),
            Code_Peer.Shell_Commands.Build_Target
              (Module.Get_Kernel, "Regenerate CodePeer Report"),
            Force       => Force,
            Build_Mode  => "codepeer",
            Synchronous => False,
            Dir         => Projects.Object_Path (Project));
      elsif Quick then
         Code_Peer.Shell_Commands.Build_Target_Execute
           (Kernel_Handle (Module.Kernel),
            Code_Peer.Shell_Commands.Build_Target
              (Module.Get_Kernel, "Run CodePeer Quickly"),
            Force       => Force,
            Build_Mode  => "codepeer",
            Synchronous => False,
            Dir         => Projects.Object_Path (Project));
      else
         Code_Peer.Shell_Commands.Build_Target_Execute
           (Kernel_Handle (Module.Kernel),
            Code_Peer.Shell_Commands.Build_Target
              (Module.Get_Kernel, "Run CodePeer"),
            Force       => Force,
            Build_Mode  => "codepeer",
            Synchronous => False,
            Dir         => Projects.Object_Path (Project));
      end if;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;
   end Review;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      use type Code_Analysis.Code_Analysis_Tree;

      Item : Gtk.Menu_Item.Gtk_Menu_Item;

   begin
      if Factory.Module.Tree = null then
         return;
      end if;

      if GPS.Kernel.Contexts.Has_File_Information (Context) then
         declare
            Project_Node    : constant Code_Analysis.Project_Access :=
              Code_Analysis.Get_Or_Create
                (Factory.Module.Tree,
                 GPS.Kernel.Contexts.Project_Information (Context));
            File_Node       : constant Code_Analysis.File_Access :=
              Code_Analysis.Get_Or_Create
                (Project_Node,
                 GPS.Kernel.Contexts.File_Information (Context));
            Subprogram_Node : Code_Analysis.Subprogram_Access;
            Subprogram_Data : Code_Peer.Subprogram_Data_Access;
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
                 Code_Peer.Subprogram_Data_Access
                   (Subprogram_Node.Analysis_Data.Code_Peer_Data);

               if Buffer /= GPS.Editors.Nil_Editor_Buffer then
                  if Subprogram_Data.Mark /= null then
                     Gtk.Menu_Item.Gtk_New (Item, -"Hide annotations");
                     Menu.Append (Item);
                     Context_CB.Connect
                       (Item,
                        Gtk.Menu_Item.Signal_Activate,
                        Context_CB.To_Marshaller
                          (On_Hide_Annotations'Access),
                        Module_Context'
                          (Code_Peer_Module_Id (Factory.Module),
                           Project_Node,
                           File_Node,
                           null));

                  else
                     Gtk.Menu_Item.Gtk_New (Item, -"Show annotations");
                     Menu.Append (Item);
                     Context_CB.Connect
                       (Item,
                        Gtk.Menu_Item.Signal_Activate,
                        Context_CB.To_Marshaller
                          (On_Show_Annotations'Access),
                        Module_Context'
                          (Code_Peer_Module_Id (Factory.Module),
                           Project_Node,
                           File_Node,
                           null));
                  end if;
               end if;

               Gtk.Menu_Item.Gtk_New (Item, -"Show messages");
               Menu.Append (Item);
               Context_CB.Connect
                 (Item,
                  Gtk.Menu_Item.Signal_Activate,
                  Context_CB.To_Marshaller (On_Show_Messages'Access),
                  Module_Context'
                    (Code_Peer_Module_Id (Factory.Module),
                     Project_Node,
                     File_Node,
                     null));

               Gtk.Menu_Item.Gtk_New (Item, -"Hide messages");
               Menu.Append (Item);
               Context_CB.Connect
                 (Item,
                  Gtk.Menu_Item.Signal_Activate,
                  Context_CB.To_Marshaller (On_Hide_Messages'Access),
                  Module_Context'
                    (Code_Peer_Module_Id (Factory.Module),
                     Project_Node,
                     File_Node,
                     null));
            end if;
         end;
      end if;
   end Append_To_Menu;

   ---------------------------------
   -- Codepeer_Database_Directory --
   ---------------------------------

   function Codepeer_Database_Directory
     (Project : Projects.Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS.Filesystem_String
          (Ada.Characters.Handling.To_Lower
               (String (Projects.Project_Path (Project).Base_Name)));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
        Projects.Project_Path (Project).File_Extension;

   begin
      return
        Create_From_Dir
          (Projects.Object_Path (Project),
           Name (Name'First .. Name'Last - Extension'Length) & ".db");
   end Codepeer_Database_Directory;

   --------------------------------
   -- Codepeer_Library_File_Name --
   --------------------------------

   function Codepeer_Library_File_Name
     (Project : Projects.Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS.Filesystem_String
          (Ada.Characters.Handling.To_Lower
               (String (Projects.Project_Path (Project).Base_Name)));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
        Projects.Project_Path (Project).File_Extension;

   begin
      return
        Create_From_Dir
          (Projects.Object_Path (Project),
           Name (Name'First .. Name'Last - Extension'Length) & ".library");
   end Codepeer_Library_File_Name;

   -------------------------------
   -- Codepeer_Output_Directory --
   -------------------------------

   function Codepeer_Output_Directory
     (Project : Projects.Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant Filesystem_String :=
                    Filesystem_String
                      (Ada.Characters.Handling.To_Lower
                         (String (Projects.Project_Path (Project).Base_Name)));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
        Projects.Project_Path (Project).File_Extension;

   begin
      return
        GNATCOLL.VFS.Create_From_Dir
          (Projects.Object_Path (Project),
           Name (Name'First .. Name'Last - Extension'Length) & ".output");
   end Codepeer_Output_Directory;

   ----------------------
   -- Hide_Annotations --
   ----------------------

   procedure Hide_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Kernel : constant GPS.Kernel.Kernel_Handle := Self.Get_Kernel;
      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File.Name, False, False, False);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is

         procedure Free is
           new Ada.Unchecked_Deallocation
             (GPS.Editors.Editor_Mark'Class, Editor_Mark_Access);

         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
         renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);

      begin
         if Data.Mark /= null
           and then Data.Mark.Is_Present
         then
            Buffer.Remove_Special_Lines (Data.Mark.all, Data.Special_Lines);
         end if;

         Free (Data.Mark);
         Data.Special_Lines := 0;
      end Process;

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer then
         File.Subprograms.Iterate (Process'Access);
      end if;
   end Hide_Annotations;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self : access Module_Id_Record'Class;
      File : Virtual_File)
   is
      use type Code_Peer.Summary_Reports.Summary_Report;
      use type Code_Analysis.Code_Analysis_Tree;
      use type GPS.Kernel.MDI.GPS_MDI_Child;

      Input   : Input_Sources.File.File_Input;
      Reader  : Code_Peer.Bridge.Inspection_Readers.Reader;

      procedure Process_Project (Position : Code_Analysis.Project_Maps.Cursor);
      --  ???

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor);
      --  ???

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor) is
         File : constant Code_Analysis.File_Access :=
                  Code_Analysis.File_Maps.Element (Position);

      begin
         Self.Filter_Criteria.Files.Insert (File);
      end Process_File;

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project
        (Position : Code_Analysis.Project_Maps.Cursor)
      is
         Project : constant Code_Analysis.Project_Access :=
                     Code_Analysis.Project_Maps.Element (Position);

      begin
         Project.Files.Iterate (Process_File'Access);
      end Process_Project;

   begin
      if Self.Report_Subwindow /= null then
         --  Destroy old report window if present

         Self.Report_Subwindow.Destroy;
      end if;

      --  Load code review information

      Input_Sources.File.Open (+File.Full_Name, Input);
      Reader.Parse (Input, GPS.Kernel.Kernel_Handle (Self.Kernel), Self.Tree);
      Input_Sources.File.Close (Input);

      --  Create codepeer report window

      Code_Peer.Summary_Reports.Gtk_New
        (Self.Report,
         GPS.Kernel.Kernel_Handle (Self.Kernel),
         GPS.Kernel.Modules.Module_ID (Self),
         Self.Tree);
      Context_CB.Connect
        (Self.Report,
         Code_Peer.Summary_Reports.Signal_Activated,
         Context_CB.To_Marshaller (On_Activate'Access),
         Module_Context'(Code_Peer_Module_Id (Self), null, null, null));
      Context_CB.Connect
        (Self.Report,
         Gtk.Object.Signal_Destroy,
         Context_CB.To_Marshaller (On_Destroy'Access),
         Module_Context'(Code_Peer_Module_Id (Self), null, null, null));
      Context_CB.Connect
        (Self.Report,
         Code_Peer.Summary_Reports.Signal_Criteria_Changed,
         Context_CB.To_Marshaller (On_Criteria_Changed'Access),
         Module_Context'(Code_Peer_Module_Id (Self), null, null, null));

      GPS.Kernel.MDI.Gtk_New
        (Self.Report_Subwindow, Self.Report, Module => Self);
      Self.Report_Subwindow.Set_Title (-"CodePeer report");
      GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.Report_Subwindow);

      --  Setup filter criteria

      Self.Filter_Criteria.Files.Clear;
      Self.Tree.Iterate (Process_Project'Access);
      Self.Report.Update_Criteria (Self.Filter_Criteria);

      --  Update location view

      Self.Update_Location_View;

      --  Raise report window

      Self.Report_Subwindow.Raise_Child;
   end Load;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

      --  use type Code_Analysis.File_Access;
      --  ??? Uncomment this line after I120-013 will be fixed
      use type Code_Analysis.Subprogram_Access;

      File       : constant Code_Analysis.File_Access :=
                     Context.Module.Report.Get_Selected_File;
      Subprogram : constant Code_Analysis.Subprogram_Access :=
                     Context.Module.Report.Get_Selected_Subprogram;

   begin
      if Subprogram /= null then
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name,
            Subprogram.Line,
            Basic_Types.Visible_Column_Type (Subprogram.Column));

      elsif File /= null then
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name);
      end if;

      Context.Module.Filter_Criteria.Files.Include (File);
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Activate;

   ----------------------------
   -- On_Display_Code_Review --
   ----------------------------

   procedure On_Display_Code_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode             : constant String := Get_Build_Mode (Kernel);
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      Code_Peer.Module.Bridge.Inspection (Module);

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Display_Code_Review;

   --------------------------
   -- On_Regenerate_Report --
   --------------------------

   procedure On_Regenerate_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode             : constant String := Get_Build_Mode (Kernel);
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         Info_File : constant Virtual_File :=
                       Create_From_Dir (Codepeer_Output_Directory
                                        (Get_Project (Kernel)),
                                        "Inspection_Info.xml");

      begin
         if not Is_Regular_File (Info_File) then
            Console.Insert
              (Kernel,
               Info_File.Display_Full_Name &
               (-" does not exist. Please perform a full analysis first"),
               Mode => Console.Error);
         else
            Review (Module, Force => False, Output_Only => True);
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Regenerate_Report;

   --------------------------
   -- On_Regenerate_Report --
   --------------------------

   procedure On_HTML_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode             : constant String := Get_Build_Mode (Kernel);
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         HTML_File : constant Virtual_File :=
                       Create_From_Dir (Codepeer_Output_Directory
                                        (Get_Project (Kernel)),
                                        "/html/index.html");

      begin
         if not Is_Regular_File (HTML_File) then
            Console.Insert
              (Kernel,
               HTML_File.Display_Full_Name &
               (-" does not exist. Please perform a full analysis first"),
               Mode => Console.Error);
         else
            Open_Html (Kernel, String (Full_Name (HTML_File).all));
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_HTML_Report;

   --------------------------
   -- On_Edit_Text_Listing --
   --------------------------

   procedure On_Edit_Text_Listing
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Mode             : constant String :=
                           Get_Build_Mode (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         Text_File : constant Virtual_File :=
                       Create_From_Dir
                         (Codepeer_Output_Directory (Get_Project (Kernel)),
                          "list/" &
                          (+File_Information
                             (Context).Display_Base_Name) & ".txt");
      begin
         if Is_Regular_File (Text_File) then
            Open_File_Editor
              (Kernel,
               Text_File,
               New_File     => False,
               Force_Reload => True);
         else
            Console.Insert
              (Kernel,
               -"cannot find text listing: " & Text_File.Display_Full_Name,
               Mode => Console.Error);
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Edit_Text_Listing;

   ---------------------------
   -- On_Edit_Text_Overview --
   ---------------------------

   procedure On_Edit_Text_Overview
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode             : constant String :=
                           Get_Build_Mode (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         Text_File : constant Virtual_File :=
                       Create_From_Dir
                         (Codepeer_Output_Directory (Get_Project (Kernel)),
                          "list/overview.txt");
      begin
         if Is_Regular_File (Text_File) then
            Open_File_Editor
              (Kernel,
               Text_File,
               New_File     => False,
               Force_Reload => True);
         else
            Console.Insert
              (Kernel,
               -"cannot find text overview: " & Text_File.Display_Full_Name,
               Mode => Console.Error);
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Edit_Text_Overview;

   -----------------
   -- On_Edit_Log --
   -----------------

   procedure On_Edit_Log
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Mode             : constant String :=
                           Get_Build_Mode (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         Log_File : constant Virtual_File :=
                      Create_From_Dir (Codepeer_Output_Directory
                                       (Get_Project (Kernel)),
                                       "Inspection.log");
      begin
         if Is_Regular_File (Log_File) then
            Open_File_Editor
              (Kernel,
               Log_File,
               New_File     => False,
               Force_Reload => True);
         else
            Console.Insert
              (Kernel, -"cannot find log file: " & Log_File.Display_Full_Name,
               Mode => Console.Error);
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Edit_Log;

   -------------------------
   -- On_File_Closed_Hook --
   -------------------------

   procedure On_File_Closed_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      use type Code_Analysis.Code_Analysis_Tree;

      D            : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Project_Node : Code_Analysis.Project_Access;

   begin
      if Module.Tree /= null
        and then Module.Tree.Contains
          (Projects.Registry.Get_Project_From_File
               (GPS.Kernel.Project.Get_Registry (Module.Kernel).all,
                D.File,
                False))
      then
         Project_Node :=
           Module.Tree.Element
             (Projects.Registry.Get_Project_From_File
                  (GPS.Kernel.Project.Get_Registry (Module.Kernel).all,
                   D.File,
                   False));

         if Project_Node.Files.Contains (D.File) then
            Hide_Annotations (Module, Project_Node.Files.Element (D.File));
         end if;
      end if;
   end On_File_Closed_Hook;

   -------------------------
   -- On_File_Edited_Hook --
   -------------------------

   procedure On_File_Edited_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      use type Code_Analysis.Code_Analysis_Tree;

      D            : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Project_Node : Code_Analysis.Project_Access;

   begin
      if Module.Tree /= null
        and then Module.Tree.Contains
          (Projects.Registry.Get_Project_From_File
               (GPS.Kernel.Project.Get_Registry (Module.Kernel).all,
                D.File,
                False))
      then
         Project_Node :=
           Module.Tree.Element
             (Projects.Registry.Get_Project_From_File
                  (GPS.Kernel.Project.Get_Registry (Module.Kernel).all,
                   D.File,
                   False));

         if Project_Node.Files.Contains (D.File) then
            Show_Annotations (Module, Project_Node.Files.Element (D.File));
         end if;
      end if;
   end On_File_Edited_Hook;

   --------------------
   -- On_Remove_Lock --
   --------------------

   procedure On_Remove_Lock
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Mode             : constant String :=
                           Get_Build_Mode (Kernel_Handle (Module.Kernel));
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      declare
         Lock_File : constant Virtual_File :=
                       Create_From_Dir (Codepeer_Output_Directory
                                         (Get_Project (Kernel)),
                                        "inspection.lock");
         Success   : Boolean;

      begin
         if Is_Regular_File (Lock_File) then
            Delete (Lock_File, Success);

            if Success then
               Console.Insert
                 (Kernel,
                  -"deleted lock file: " & Lock_File.Display_Full_Name);
            else
               Console.Insert
                 (Kernel,
                  -"could not delete lock file: " &
                  Lock_File.Display_Full_Name);
            end if;
         else
            Console.Insert (Kernel, -"no lock file to delete.");
         end if;
      end;

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Remove_Lock;

   ------------------------------
   -- On_Run_Analysis_Manually --
   ------------------------------

   procedure On_Run_Analysis_Manually
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      Review (Module, Force => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Run_Analysis_Manually;

   --------------------
   -- On_Analyze_All --
   --------------------

   procedure On_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Module.Action := Run;
      Code_Peer.Shell_Commands.Build_Target_Execute
        (Kernel,
         Code_Peer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => True,
         Build_Mode  => "codepeer",
         Synchronous => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Analyze_All;

   --------------------------
   -- On_Quick_Analyze_All --
   --------------------------

   procedure On_Quick_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Module.Action := Quick_Run;
      Code_Peer.Shell_Commands.Build_Target_Execute
        (Kernel,
         Code_Peer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => True,
         Build_Mode  => "codepeer",
         Synchronous => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Quick_Analyze_All;

   ----------------------
   -- On_Generate_SCIL --
   ----------------------

   procedure On_Generate_SCIL
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Code_Peer.Shell_Commands.Build_Target_Execute
        (Kernel,
         Code_Peer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => False,
         Build_Mode  => "codepeer",
         Synchronous => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Generate_SCIL;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      Hook_Data : constant
        GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args :=
          GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args (Data.all);
      Mode      : constant String :=
        Code_Peer.Shell_Commands.Get_Build_Mode
          (GPS.Kernel.Kernel_Handle (Kernel));
      Action : constant CodePeer_Action := Module.Action;

   begin
      Module.Action := None;

      if Hook_Data.Status /= 0
        or else Action = None
        or else Hook_Data.Category /= "CodePeer"
      then
         return;
      end if;

      case Action is
         when Run =>
            Review (Module, Force => True);

         when Quick_Run =>
            Review (Module, Force => True, Quick => True);

         when Load_UI =>
            declare
               CodePeer_Subdir : constant Boolean :=
                                   Use_CodePeer_Subdir
                                     (Kernel_Handle (Kernel));

            begin
               --  If a codepeer dir is found in the object dir, then use this
               --  directory, otherwise use the default object dir (advanced
               --  mode).

               if CodePeer_Subdir then
                  Code_Peer.Shell_Commands.Set_Build_Mode
                    (GPS.Kernel.Kernel_Handle (Kernel), "codepeer");
               end if;

               Code_Peer.Module.Bridge.Inspection (Module);

               if CodePeer_Subdir then
                  Code_Peer.Shell_Commands.Set_Build_Mode
                    (GPS.Kernel.Kernel_Handle (Kernel), Mode);
               end if;
            end;

         when None => null;
      end case;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Compilation_Finished;

   -------------------------
   -- On_Criteria_Changed --
   -------------------------

   procedure On_Criteria_Changed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Report.Update_Criteria (Context.Module.Filter_Criteria);
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Criteria_Changed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

      procedure Process_Project (Position : Code_Analysis.Project_Maps.Cursor);

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor) is
         File : constant Code_Analysis.File_Access :=
                  Code_Analysis.File_Maps.Element (Position);

      begin
         Context.Module.Hide_Annotations (File);
      end Process_File;

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project
        (Position : Code_Analysis.Project_Maps.Cursor)
      is
         Project : constant Code_Analysis.Project_Access :=
                     Code_Analysis.Project_Maps.Element (Position);

      begin
         Project.Files.Iterate (Process_File'Access);
      end Process_Project;

   begin
      --  Hide all annotations

      Context.Module.Tree.Iterate (Process_Project'Access);

      --  Cleanup location view

      Remove_Location_Category
        (Context.Module.Kernel, Code_Peer_Category_Name);

      --  Cleanup filter criteria

      Context.Module.Filter_Criteria.Files.Clear;
      Context.Module.Filter_Criteria.Categories.Clear;

      --  Mark report as destroyed

      Context.Module.Report := null;
      Context.Module.Report_Subwindow := null;

      --  Cleanup project tree

      Code_Analysis.Free_Code_Analysis (Context.Module.Tree);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Destroy;

   -------------------------
   -- On_Hide_Annotations --
   -------------------------

   procedure On_Hide_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Hide_Annotations (Context.File);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Hide_Annotations;

   ----------------------
   -- On_Hide_Messages --
   ----------------------

   procedure On_Hide_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Filter_Criteria.Files.Delete (Context.File);
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Hide_Messages;

   -------------------------
   -- On_Message_Reviewed --
   -------------------------

   procedure On_Message_Reviewed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Code_Peer.Module.Bridge.Add_Audit_Record
        (Context.Module, Context.Message);
      Context.Module.Report.Update;
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Message_Reviewed;

   --------------------------
   -- On_Remove_XML_Review --
   --------------------------

   procedure On_Remove_XML_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode             : constant String := Get_Build_Mode (Kernel);
      CodePeer_Subdir  : constant Boolean := Use_CodePeer_Subdir (Kernel);

   begin
      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), "codepeer");
      end if;

      Bridge.Remove_Inspection_Cache_File (Module);

      if CodePeer_Subdir then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
      end if;

   exception
      when E : others =>
         Trace (Me, E);

         if CodePeer_Subdir then
            Code_Peer.Shell_Commands.Set_Build_Mode
              (Kernel_Handle (Module.Kernel), Mode);
         end if;
   end On_Remove_XML_Review;

   -------------------------
   -- On_Show_Annotations --
   -------------------------

   procedure On_Show_Annotations
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Show_Annotations (Context.File);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Show_Annotations;

   ----------------------
   -- On_Show_Messages --
   ----------------------

   procedure On_Show_Messages
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

   begin
      Context.Module.Filter_Criteria.Files.Insert (Context.File);
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Show_Messages;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : Code_Peer.Message_Access;
      File    : Virtual_File)
   is
      pragma Unreferenced (Self);

      Input  : Input_Sources.File.File_Input;
      Reader : Code_Peer.Bridge.Audit_Trail_Readers.Reader;

   begin
      --  Load inspection information

      Input_Sources.File.Open (+File.Full_Name, Input);
      Reader.Parse (Input, Message.Audit);
      Input_Sources.File.Close (Input);

      Message.Audit_Loaded := True;

      Module.Review_Message (Message);
   end Review_Message;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : Code_Peer.Message_Access)
   is
      Review : Code_Peer.Message_Review_Dialogs.Message_Review_Dialog;

   begin
      if not Message.Audit_Loaded then
         Code_Peer.Module.Bridge.Review_Message
           (Code_Peer_Module_Id (Self), Message);

      else
         --  Create and show review dialog

         Code_Peer.Message_Review_Dialogs.Gtk_New (Review, Message);
         Review.Set_Transient_For (Self.Kernel.Get_Main_Window);
         Review.Show_All;
         Context_CB.Connect
           (Review,
            Code_Peer.Message_Review_Dialogs.Signal_Ok_Activated,
            Context_CB.To_Marshaller (On_Message_Reviewed'Access),
            (Code_Peer_Module_Id (Self), null, null, Message));
      end if;
   end Review_Message;

   ----------------------
   -- Show_Annotations --
   ----------------------

   procedure Show_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor);

      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Self.Get_Kernel.Get_Buffer_Factory.Get (File.Name);

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor)
      is

         procedure Process_Annotation
           (Position : Code_Peer.Annotation_Vectors.Cursor);

         procedure Process_Annotations
           (Position : Code_Peer.Annotation_Maps.Cursor);

         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
           renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);
         Indent          : constant String :=
                             Ada.Strings.Fixed."*"
                               (Subprogram_Node.Column - 1, ' ');

         ------------------------
         -- Process_Annotation --
         ------------------------

         procedure Process_Annotation
           (Position : Code_Peer.Annotation_Vectors.Cursor)
         is
            Annotation : constant Code_Peer.Annotation_Access :=
                           Code_Peer.Annotation_Vectors.Element (Position);

         begin
            Buffer.Add_Special_Line
              (Subprogram_Node.Line,
               Indent & "--    " & Annotation.Text.all,
               Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;
         end Process_Annotation;

         -------------------------
         -- Process_Annotations --
         -------------------------

         procedure Process_Annotations
           (Position : Code_Peer.Annotation_Maps.Cursor)
         is
            Key     : constant Code_Peer.Annotation_Category_Access :=
                        Code_Peer.Annotation_Maps.Key (Position);
            Element : constant Code_Peer.Annotation_Vector_Access :=
                        Code_Peer.Annotation_Maps.Element (Position);

         begin
            Buffer.Add_Special_Line
              (Subprogram_Node.Line,
               Indent & "--  " & Key.Text.all & ":",
               Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;

            Element.Iterate (Process_Annotation'Access);

            Buffer.Add_Special_Line
              (Subprogram_Node.Line, Indent & "--", Annotation_Style_Name);
            Data.Special_Lines := Data.Special_Lines + 1;
         end Process_Annotations;

      begin
         Data.Mark :=
           new GPS.Editors.Editor_Mark'Class'
             (Buffer.Add_Special_Line
                  (Subprogram_Node.Line,
                   Indent & "--",
                   Annotation_Style_Name));
         Data.Special_Lines := Data.Special_Lines + 1;

         Buffer.Add_Special_Line
           (Subprogram_Node.Line,
            Indent & "--  Subprogram: " & Subprogram_Node.Name.all,
            Annotation_Style_Name);
         Data.Special_Lines := Data.Special_Lines + 1;

         Buffer.Add_Special_Line
           (Subprogram_Node.Line, Indent & "--", Annotation_Style_Name);
         Data.Special_Lines := Data.Special_Lines + 1;

         Data.Annotations.Iterate (Process_Annotations'Access);
      end Process_Subprogram;

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer then
         File.Subprograms.Iterate (Process_Subprogram'Access);
      end if;
   end Show_Annotations;

   --------------------------
   -- Update_Location_View --
   --------------------------

   procedure Update_Location_View (Self : access Module_Id_Record'Class) is

      procedure Process_File (Position : Code_Peer.File_Sets.Cursor);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (Position : Code_Peer.File_Sets.Cursor) is

         procedure Process_Subprogram
           (Position : Code_Analysis.Subprogram_Maps.Cursor);

         procedure Process_Message
           (Position : Code_Peer.Message_Vectors.Cursor);

         File : constant Code_Analysis.File_Access :=
                  Code_Peer.File_Sets.Element (Position);

         ---------------------
         -- Process_Message --
         ---------------------

         procedure Process_Message
           (Position : Code_Peer.Message_Vectors.Cursor)
         is
            Message : constant Code_Peer.Message_Access :=
              Code_Peer.Message_Vectors.Element (Position);
            Review  : GPS.Kernel.Standard_Hooks.Action_Item;

            function Probability_Image
              (Message : Code_Peer.Message_Access) return String;
            --  Returns an suitable Image correpsonding to Message's
            --  probability

            function Image
              (Message : Code_Peer.Message_Access) return String;
            --  Returns complete text of the Message

            -----------
            -- Image --
            -----------

            function Image
              (Message : Code_Peer.Message_Access) return String
            is
            begin
               if Message.Text'Length = 0
                 or else Message.Text (Message.Text'First) = ':'
               then
                  return
                    Probability_Image (Message)
                    & Message.Category.Name.all
                    & Message.Text.all;
               else
                  return
                    Probability_Image (Message)
                    & Message.Category.Name.all & " "
                    & Message.Text.all;
               end if;
            end Image;

            -----------------------
            -- Probability_Image --
            -----------------------

            function Probability_Image
              (Message : Code_Peer.Message_Access) return String
            is
               Category : String renames Message.Category.Name.all;
            begin
               if Category = "dead code"
                 or else Category = "test predetermined"
                 or else Category = "suspicious precondition"
                 or else (Category'Length >= 17
                          and then Category
                            (Category'First .. Category'First + 6) = "unused ")
               then
                  return "warning: ";
               end if;

               case Message.Current_Probability is
                  when Code_Peer.High =>
                     return "high: ";

                  when Code_Peer.Medium =>
                     return "medium: ";

                  when Code_Peer.Low =>
                     return "low: ";

                  when Code_Peer.Informational =>
                     return "info: ";

                  when Code_Peer.Suppressed =>
                     return "suppressed: ";
               end case;
            end Probability_Image;

         begin
            if Message.Current_Probability /= Code_Peer.Suppressed
              and then Self.Filter_Criteria.Lineages (Message.Lifeage)
              and then Self.Filter_Criteria.Probabilities
                         (Message.Current_Probability)
              and then Self.Filter_Criteria.Categories.Contains
                (Message.Category)
            then
               Insert_Location
                 (Kernel       => Self.Kernel,
                  Category     => Code_Peer_Category_Name,
                  File         => File.Name,
                  Text         => Image (Message),
                  Line         => Message.Line,
                  Column       =>
                    Basic_Types.Visible_Column_Type (Message.Column),
                  Highlight    => True,
                  Highlight_Category =>
                    Module.Message_Styles (Message.Current_Probability),
                  Quiet        => True,
                  Sort_In_File => True);

               Review :=
                 new GPS.Editors.Line_Information_Record'
                   (Text               => null,
                    Tooltip_Text       => new String'("Review message"),
                    Image              =>
                      Gtk.Widget.Gtk_Widget
                        (Self.Kernel.Get_Main_Window).Render_Icon
                        (Code_Analysis_GUI.Post_Analysis_Cst,
                         Gtk.Enums.Icon_Size_Menu),
                    Associated_Command =>
                    new Commands.Code_Peer.Review_Message_Command'
                      (Commands.Root_Command with
                       Code_Peer_Module_Id (Self), Message));

               GPS.Kernel.Standard_Hooks.Add_Location_Action
                 (Kernel     => Self.Kernel,
                  Identifier => "CodePeer",
                  Category   => Code_Peer_Category_Name,
                  File       => File.Name,
                  Line       => Message.Line,
                  Column     => Message.Column,
                  Message    => Image (Message),
                  Action     => Review);
            end if;
         end Process_Message;

         ------------------------
         -- Process_Subprogram --
         ------------------------

         procedure Process_Subprogram
           (Position : Code_Analysis.Subprogram_Maps.Cursor)
         is
            Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
              Code_Analysis.Subprogram_Maps.Element (Position);
            Data            : Code_Peer.Subprogram_Data'Class
            renames Code_Peer.Subprogram_Data'Class
              (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);

         begin
            Data.Messages.Iterate (Process_Message'Access);
         end Process_Subprogram;

      begin
         File.Subprograms.Iterate (Process_Subprogram'Access);
      end Process_File;

   begin
      Remove_Location_Category (Self.Kernel, Code_Peer_Category_Name);

      Self.Filter_Criteria.Files.Iterate (Process_File'Access);
   end Update_Location_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Submenu_Factory    : GPS.Kernel.Modules.Submenu_Factory;
      Menu               : constant String := -"/_CodePeer";
      Advanced_Menu      : constant String := Menu & (-"/Advanced");
      Str                : String_Access := Locate_Exec_On_Path ("codepeer");
      Src_Editor_Context : constant Action_Filter :=
                             Lookup_Filter (Kernel, "Source editor");
      Mitem              : Gtk.Menu_Item.Gtk_Menu_Item;

   begin
      if Str = null then
         --  Do not register the CodePeer module if the codepeer executable
         --  cannot be found.
         return;
      end if;

      Free (Str);
      Module          := new Module_Id_Record (Kernel);
      Submenu_Factory := new Submenu_Factory_Record (Module);

      Module.Register_Module (Kernel, "CodePeer");
      GPS.Kernel.Modules.Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => "CodePeer",
         Label   => -"CodePeer",
         Filter  => GPS.Kernel.Lookup_Filter (Kernel, "Project only")
           or GPS.Kernel.Lookup_Filter (Kernel, "In project"),
         Submenu => Submenu_Factory);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Analyze All",
         Ref_Item    => -"Window",
         Callback    => On_Analyze_All'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Display Code Review",
         Callback    => On_Display_Code_Review'Access);

      Gtk.Menu_Item.Gtk_New (Mitem);
      GPS.Kernel.Modules.Register_Menu (Kernel, Menu, Mitem);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Quick Analyze All",
         Callback    => On_Quick_Analyze_All'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Generate SCIL",
         Callback    => On_Generate_SCIL'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Run _CodePeer",
         Callback    => On_Run_Analysis_Manually'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Regenerate Report",
         Callback    => On_Regenerate_Report'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Display _HTML Report",
         Callback    => On_HTML_Report'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"_Text Overview",
         Callback    => On_Edit_Text_Overview'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"_Text Listing",
         Callback    => On_Edit_Text_Listing'Access,
         Filter      => Src_Editor_Context);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"CodePeer _Log",
         Callback    => On_Edit_Log'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"Remove Lock",
         Callback    => On_Remove_Lock'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"Remove XML Code Review",
         Callback    => On_Remove_XML_Review'Access);

      Module.Annotation_Style :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (GPS.Kernel.Kernel_Handle (Kernel), Annotation_Style_Name);
      GPS.Kernel.Styles.Set_Background (Module.Annotation_Style, "#E9E9E9");

      Module.Message_Styles (Code_Peer.High) :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (GPS.Kernel.Kernel_Handle (Kernel), High_Probability_Style_Name);
      GPS.Kernel.Styles.Set_Background
        (Module.Message_Styles (Code_Peer.High), "#FFCCCC");

      Module.Message_Styles (Code_Peer.Medium) :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (GPS.Kernel.Kernel_Handle (Kernel), Medium_Probability_Style_Name);
      GPS.Kernel.Styles.Set_Background
        (Module.Message_Styles (Code_Peer.Medium), "#FFFFCC");

      Module.Message_Styles (Code_Peer.Low) :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (GPS.Kernel.Kernel_Handle (Kernel), Low_Probability_Style_Name);
      GPS.Kernel.Styles.Set_Background
        (Module.Message_Styles (Code_Peer.Low), "#CCFFFF");

      Module.Message_Styles (Code_Peer.Informational) :=
        GPS.Kernel.Styles.Get_Or_Create_Style
          (GPS.Kernel.Kernel_Handle (Kernel),
           Informational_Probability_Style_Name);
      GPS.Kernel.Styles.Set_Background
        (Module.Message_Styles (Code_Peer.Informational), "#EFEFEF");

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Compilation_Finished_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Compilation_Finished'Access),
         Name => "codepeer.compilation_finished");
      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.File_Closed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Closed_Hook'Access),
         Name  => "codepeer.file_closed");
      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.File_Edited_Hook,
         GPS.Kernel.Hooks.Wrapper (On_File_Edited_Hook'Access),
         Name  => "codepeer.file_edited");
   end Register_Module;

end Code_Peer.Module;

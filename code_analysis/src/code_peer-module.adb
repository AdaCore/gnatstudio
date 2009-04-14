-----------------------------------------------------------------------
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

with Ada.Strings.Fixed;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Input_Sources.File;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Object;
with Gtk.Text_Mark;
with Gtk.Widget;

with Basic_Types;
with GPS.Editors;
with GPS.Intl; use GPS.Intl;
with GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;
with GPS.Location_View;

with Code_Peer.Bridge.Audit_Trail_Readers;
with Code_Peer.Bridge.Inspection_Readers;
with Code_Peer.Message_Review_Dialogs;
with Code_Peer.Module.Bridge;
with Code_Peer.Module.Codepeer;
with Code_Peer.Shell_Commands;
with Commands.Code_Peer;
with Code_Analysis_GUI;

package body Code_Peer.Module is

   use type Gtk.Text_Mark.Gtk_Text_Mark;
   use type GPS.Editors.Editor_Mark'Class;
   use type GPS.Editors.Editor_Buffer'Class;

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

   procedure On_Run_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);

   procedure On_Load
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);

   procedure On_Criteria_Changed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Message_Reviewed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   Code_Peer_Category_Name : constant String := "CodePeer messages";

   Module : Code_Peer_Module_Id;
   --  Global variable for store CodePeer plugin module. Used in the main menu
   --  callbacks.

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
            Kernel          : constant GPS.Kernel.Kernel_Handle :=
                                GPS.Kernel.Get_Kernel (Context);
            Buffer          : constant GPS.Editors.Editor_Buffer'Class :=
                                Kernel.Get_Buffer_Factory.Get
                                  (File_Node.Name, False, False);

         begin
            if not File_Node.Subprograms.Is_Empty then
               Subprogram_Node :=
                 Code_Analysis.Subprogram_Maps.Element
                   (File_Node.Subprograms.First);

               if Buffer /= GPS.Editors.Nil_Editor_Buffer then
                  declare
                     Mark : constant GPS.Editors.Editor_Mark'Class :=
                              Buffer.Get_Mark
                                (Code_Peer_Editor_Mark_Name_Prefix
                                 & Subprogram_Node.Name.all);

                  begin
                     if Mark /= GPS.Editors.Nil_Editor_Mark then
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
                  end;
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
         Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                             Code_Analysis.Subprogram_Maps.Element (Position);
         Data            : Code_Peer.Subprogram_Data'Class
         renames Code_Peer.Subprogram_Data'Class
           (Subprogram_Node.Analysis_Data.Code_Peer_Data.all);
         Mark            : constant GPS.Editors.Editor_Mark'Class :=
                             Buffer.Get_Mark
                               (Code_Peer_Editor_Mark_Name_Prefix
                                & Subprogram_Node.Name.all);

      begin
         if Mark /= GPS.Editors.Nil_Editor_Mark
           and then Mark.Is_Present
         then
            Buffer.Remove_Special_Lines (Mark, Data.Special_Lines);
            Mark.Delete;
            Data.Special_Lines := 0;
         end if;
      end Process;

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer then
         File.Subprograms.Iterate (Process'Access);
      end if;
   end Hide_Annotations;

   -----------------------
   -- Is_In_Expret_Mode --
   -----------------------

   function Is_In_Expert_Mode
     (Self : access Module_Id_Record'Class) return Boolean is
   begin
      return Self.Expert_Mode.Get_Pref;
   end Is_In_Expert_Mode;

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

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor);

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
         Context.Module.Hide_Annotations (File);
         Context.Module.Show_Annotations (File);
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name,
            Subprogram.Line,
            Basic_Types.Visible_Column_Type (Subprogram.Column));

      elsif File /= null then
         Context.Module.Hide_Annotations (File);
         Context.Module.Show_Annotations (File);
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name);
      end if;

      Context.Module.Filter_Criteria.Files.Include (File);
      Context.Module.Update_Location_View;
   end On_Activate;

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

   begin
      if Hook_Data.Mode_Name = "codepeer"
        and then Hook_Data.Status = 0
        and then not Is_In_Expert_Mode (Module)
      then
         Code_Peer.Shell_Commands.Set_Build_Mode
           (GPS.Kernel.Kernel_Handle (Kernel), "codepeer");

         Code_Peer.Module.Codepeer.Review (Module);

         Code_Peer.Shell_Commands.Set_Build_Mode
           (GPS.Kernel.Kernel_Handle (Kernel), Mode);
      end if;
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

      GPS.Location_View.Remove_Location_Category
        (Context.Module.Kernel, Code_Peer_Category_Name);

      --  Cleanup filter criteria

      Context.Module.Filter_Criteria.Files.Clear;
      Context.Module.Filter_Criteria.Categories.Clear;

      --  Mark report as destroyed

      Context.Module.Report := null;
      Context.Module.Report_Subwindow := null;

      --  Cleanup project tree

      Code_Analysis.Free_Code_Analysis (Context.Module.Tree);
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
   end On_Hide_Messages;

   -------------
   -- On_Load --
   -------------

   procedure On_Load
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);

   begin
      Code_Peer.Module.Bridge.Inspection (Module);
   end On_Load;

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
   end On_Message_Reviewed;

   -------------------
   -- On_Run_Review --
   -------------------

   procedure On_Run_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      if Is_In_Expert_Mode (Module) then
         Code_Peer.Module.Codepeer.Review (Module);

      else
         Code_Peer.Shell_Commands.Build_Target_Execute
           (Kernel,
            Code_Peer.Shell_Commands.Build_Target (Kernel, "Build All"),
            Force => True,
            Build_Mode => "codepeer");
      end if;
   end On_Run_Review;

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
   end On_Show_Messages;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use type GPS.Kernel.Action_Filter;

      Submenu_Factory : GPS.Kernel.Modules.Submenu_Factory;
      Str : String_Access := Locate_Exec_On_Path ("codepeer");

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
         Parent_Path => '/' & "Tools" & '/' & "CodePeer",
         Text        => -"Run code review",
         Ref_Item    => "Documentation",
         Callback    => On_Run_Review'Access);

      GPS.Kernel.Modules.Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & "Tools" & '/' & "CodePeer",
         Text        => -"Load code review information",
         Add_Before  => True,
         Callback    => On_Load'Access);

      Module.Expert_Mode :=
        Default_Preferences.Create
          (Manager => Kernel.Get_Preferences,
           Name    => "Codepeer-Expert-Mode",
           Default => False,
           Doc     => -("If disabled, GPS runs all programs in the CodePeer " &
                        "toolchain automatically; otherwise user is " &
                        "resposible to call programs in right order"),
           Label   => -"Expert Mode",
           Page    => -"CodePeer");

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
         Name => "codefix.compilation_finished");
   end Register_Module;

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
         Buffer.Add_Special_Line
           (Subprogram_Node.Line,
            Indent & "--",
            Annotation_Style_Name,
            Code_Peer_Editor_Mark_Name_Prefix & Subprogram_Node.Name.all);
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

            function Image
              (Item : Code_Peer.Message_Probability_Level) return String;

            -----------
            -- Image --
            -----------

            function Image
              (Item : Code_Peer.Message_Probability_Level) return String is
            begin
               case Item is
               when Code_Peer.High =>
                  return "high: ";

               when Code_Peer.Medium =>
                  return "medium: ";

               when Code_Peer.Low =>
                  return "low: ";

               when Code_Peer.Informational =>
                  return "info: ";

               when Code_Peer.Suppressed =>
                  return "SUPPRESSED: ";
               end case;
            end Image;

         begin
            if Message.Current_Probability /= Code_Peer.Suppressed
              and then Self.Filter_Criteria.Lineages (Message.Lifeage)
              and then Self.Filter_Criteria.Probabilities
                         (Message.Current_Probability)
              and then Self.Filter_Criteria.Categories.Contains
                (Message.Category)
            then
               GPS.Location_View.Insert_Location
                 (Kernel       => Self.Kernel,
                  Category     => Code_Peer_Category_Name,
                  File         => File.Name,
                  Text         =>
                    Image (Message.Current_Probability)
                    & Message.Category.Name.all
                    & ": "
                    & Message.Text.all,
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
                        (Code_Analysis_GUI.Subp_Pixbuf_Cst,
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
                  Message    =>
                    Image (Message.Current_Probability) & Message.Text.all,
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
      GPS.Location_View.Remove_Location_Category
        (Self.Kernel, Code_Peer_Category_Name);

      Self.Filter_Criteria.Files.Iterate (Process_File'Access);
   end Update_Location_View;

end Code_Peer.Module;

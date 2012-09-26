------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Input_Sources.File;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;    use Gtk.Separator_Menu_Item;
with Gtk.Widget;

with Basic_Types;
with GPS.Editors;
with GPS.Editors.Line_Information;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Console;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages.View;   use GPS.Kernel.Messages.View;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;          use GPS.Kernel.Styles;
with GPS.Styles;                 use GPS.Styles;
with GPS.Styles.UI;              use GPS.Styles.UI;
with Traces;                     use Traces;

with CodePeer.Bridge.Audit_Trail_Readers;
with CodePeer.Bridge.Inspection_Readers;
with CodePeer.Message_Review_Dialogs;
with CodePeer.Messages_Reports;
with CodePeer.Module.Bridge;
with CodePeer.Module.Editors;
with CodePeer.Module.Filters;
with CodePeer.Shell_Commands;   use CodePeer.Shell_Commands;
with Commands.CodePeer;
with Code_Analysis_GUI;

package body CodePeer.Module is

   use type GPS.Editors.Editor_Mark'Class;
   use type GPS.Editors.Editor_Buffer'Class;

   Me : constant Debug_Handle := Create ("CodePeer");

   type Module_Context is record
      Module  : CodePeer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
      Message : CodePeer.Message_Access;
   end record;

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

   procedure Analyze
     (Kernel : GPS.Kernel.Kernel_Handle;
      Action : CodePeer_Action);
   --  Helper functions for On_Analyze_xxx callbacks below

   procedure On_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze All" menu item is activated

   procedure On_Analyze_Root
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze Root Project" menu item is activated

   procedure On_Analyze_File
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze File" menu item is activated

   procedure On_Quick_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Quick Analyze All" menu item is activated

   procedure On_Generate_SCIL
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Generate SCIL" menu item is activated

   procedure On_Remove_SCIL
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Remove SCIL" menu item is activated

   procedure On_Remove_CodePeer
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Remove SCIL & DB" menu item is activated

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

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when project view is changed. Cleanup obsolete messages in
   --  messages container.

   procedure Update_Location_View (Self : access Module_Id_Record'Class);
   --  Update locations view using current messages filter. Note, Hide_Messages
   --  and Show_Messages subprograms must be used to show and hide files,
   --  instead of direct manipulation with filter's criteria and call to
   --  this subprogram.

   procedure Remove_Codepeer_Messages
     (Kernel : access Kernel_Handle_Record'Class);
   --  Remove all messages of all categories, which name starts from
   --  CodePeer_Category_Prefix.

   procedure Fill_Object_Races (Self : access Module_Id_Record'Class);
   --  Fill object races information into Locations view

   procedure Hide_Messages
     (Self : not null access Module_Id_Record'Class;
      File : Code_Analysis.File_Access);
   --  Hides file and its messages in the location view

   procedure Show_Messages
     (Self : not null access Module_Id_Record'Class;
      File : Code_Analysis.File_Access);
   --  Shows file and its messages in the location view. Current messages
   --  fileter is applied. Messages container's messages are created when
   --  necessary.

   procedure Review
     (Module      : CodePeer.Module.CodePeer_Module_Id;
      Force       : Boolean;
      Build_Target : String := "Run CodePeer");
   --  Launch CodePeer review using the specified build target.
   --  If Force is True, no dialog is displayed to change codepeer switches.

   Output_Directory_Attribute   :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Output_Directory");
   Database_Directory_Attribute :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Database_Directory");

   Race_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (Editor_Side => True, Locations => True);

   -----------------------
   -- Fill_Object_Races --
   -----------------------

   procedure Fill_Object_Races (Self : access Module_Id_Record'Class) is

      procedure Process_Object_Race
        (Position : CodePeer.Object_Race_Vectors.Cursor);

      Data : CodePeer.Project_Data'Class
      renames CodePeer.Project_Data'Class
        (Self.Tree.Element
           (GPS.Kernel.Project.Get_Project
              (Self.Kernel)).Analysis_Data.CodePeer_Data.all);

      -------------------------
      -- Process_Object_Race --
      -------------------------

      procedure Process_Object_Race
        (Position : CodePeer.Object_Race_Vectors.Cursor)
      is
         Element  : constant Object_Race_Information :=
                      Object_Race_Vectors.Element (Position);
         Category : constant String :=
                      Race_Condition_Category (Element.Name.all);

         procedure Process_Entry_Point
           (Position : Entry_Point_Object_Access_Vectors.Cursor);

         -------------------------
         -- Process_Entry_Point --
         -------------------------

         procedure Process_Entry_Point
           (Position : Entry_Point_Object_Access_Vectors.Cursor)
         is
            Element     : constant Entry_Point_Object_Access_Information :=
              Entry_Point_Object_Access_Vectors.Element (Position);
            Entry_Point : constant String := Element.Entry_Point.Name.all;

            procedure Process_Object_Access
              (Position : Object_Access_Vectors.Cursor);

            ---------------------------
            -- Process_Object_Access --
            ---------------------------

            procedure Process_Object_Access
              (Position : Object_Access_Vectors.Cursor)
            is
               function Image (Item : Object_Access_Information) return String;
               --  Returns text of message for the specified object access

               -----------
               -- Image --
               -----------

               function Image
                 (Item : Object_Access_Information) return String is
               begin
                  case Item.Kind is
                     when Read =>
                        return "read by " & Entry_Point;

                     when Update =>
                        return "update by " & Entry_Point;
                  end case;
               end Image;

               Element : constant Object_Access_Information :=
                 Object_Access_Vectors.Element (Position);
               Ignore  : Simple_Message_Access;
               pragma Unreferenced (Ignore);

            begin
               Ignore :=
                 Create_Simple_Message
                   (Get_Messages_Container (Self.Kernel),
                    Category,
                    Element.File,
                    Element.Line,
                    Basic_Types.Visible_Column_Type (Element.Column),
                    Image (Element),
                    0,
                    Race_Message_Flags);
            end Process_Object_Access;

         begin
            Element.Object_Accesses.Iterate (Process_Object_Access'Access);
         end Process_Entry_Point;

      begin
         Self.Object_Race_Categories.Include (Category);
         Element.Entry_Points.Iterate (Process_Entry_Point'Access);
      end Process_Object_Race;

   begin
      Data.Object_Races.Iterate (Process_Object_Race'Access);
   end Fill_Object_Races;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (Ranking : CodePeer.Message_Ranking_Level) return Gdk.Color.Gdk_Color is
   begin
      return Module.Message_Colors (Ranking).Get_Pref;
   end Get_Color;

   ------------
   -- Review --
   ------------

   procedure Review
     (Module      : CodePeer.Module.CodePeer_Module_Id;
      Force       : Boolean;
      Build_Target : String := "Run CodePeer")
   is
      Saved_Mode    : constant String :=
        CodePeer.Shell_Commands.Get_Build_Mode
          (Kernel_Handle (Module.Kernel));
      Project : constant Project_Type := Get_Project (Module.Kernel);

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");
      Module.Action := Load_UI;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel_Handle (Module.Kernel),
         CodePeer.Shell_Commands.Build_Target
           (Module.Get_Kernel, Build_Target),
         Force       => Force,
         Build_Mode  => "codepeer",
         Synchronous => False,
         Dir         => Project.Object_Dir);

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Saved_Mode);
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
                  if Subprogram_Data.Mark /= null then
                     Gtk.Menu_Item.Gtk_New (Item, -"Hide annotations");
                     Menu.Append (Item);
                     Context_CB.Connect
                       (Item,
                        Gtk.Menu_Item.Signal_Activate,
                        Context_CB.To_Marshaller
                          (On_Hide_Annotations'Access),
                        Module_Context'
                          (CodePeer_Module_Id (Factory.Module),
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
                          (CodePeer_Module_Id (Factory.Module),
                           Project_Node,
                           File_Node,
                           null));
                  end if;
               end if;

               if Module.Filter_Criteria.Files.Contains (File_Node) then
                  Gtk.Menu_Item.Gtk_New (Item, -"Hide messages");
                  Menu.Append (Item);
                  Context_CB.Connect
                    (Item,
                     Gtk.Menu_Item.Signal_Activate,
                     Context_CB.To_Marshaller (On_Hide_Messages'Access),
                     Module_Context'
                       (CodePeer_Module_Id (Factory.Module),
                        Project_Node,
                        File_Node,
                        null));

               else
                  Gtk.Menu_Item.Gtk_New (Item, -"Show messages");
                  Menu.Append (Item);
                  Context_CB.Connect
                    (Item,
                     Gtk.Menu_Item.Signal_Activate,
                     Context_CB.To_Marshaller (On_Show_Messages'Access),
                     Module_Context'
                       (CodePeer_Module_Id (Factory.Module),
                     Project_Node,
                        File_Node,
                        null));
               end if;
            end if;
         end;
      end if;
   end Append_To_Menu;

   ---------------------------------
   -- Codepeer_Database_Directory --
   ---------------------------------

   function Codepeer_Database_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant GNATCOLL.VFS.Filesystem_String :=
                    GNATCOLL.VFS.Filesystem_String
                      (Ada.Characters.Handling.To_Lower
                         (String (Project_Path (Project).Base_Name)));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
                    Project_Path (Project).File_Extension;

   begin
      if Project.Has_Attribute (Database_Directory_Attribute) then
         declare
            Dir : constant GNATCOLL.VFS.Filesystem_String :=
                    GNATCOLL.VFS.Filesystem_String
                      (Project.Attribute_Value (Database_Directory_Attribute));

         begin
            return
              GNATCOLL.VFS.Create_From_Base
                (Dir, Project.Project_Path.Dir.Full_Name.all);
         end;

      else
         return
           Create_From_Dir
             (Project.Object_Dir,
              Name (Name'First .. Name'Last - Extension'Length) & ".db");
      end if;
   end Codepeer_Database_Directory;

   --------------------------------
   -- Codepeer_Library_File_Name --
   --------------------------------

   function Codepeer_Library_File_Name
     (Project : Project_Type;
      Suffix  : String := "") return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS.Filesystem_String
          (String (Project_Path (Project).Base_Name));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
        Project_Path (Project).File_Extension;

   begin
      --  J506-031: File name must be synchronized with the name used to run
      --  'codepeer' from the builder module (see codepeer.py for builder
      --  models definition).

      return
        Create_From_Dir
          (Project.Object_Dir,
           Name (Name'First .. Name'Last - Extension'Length)
           & Filesystem_String (Suffix)
           & ".library");
   end Codepeer_Library_File_Name;

   -------------------------------
   -- Codepeer_Output_Directory --
   -------------------------------

   function Codepeer_Output_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Name      : constant Filesystem_String :=
                    Filesystem_String
                      (Ada.Characters.Handling.To_Lower
                         (String (Project_Path (Project).Base_Name)));
      Extension : constant GNATCOLL.VFS.Filesystem_String :=
                    Project_Path (Project).File_Extension;

   begin
      if Project.Has_Attribute (Output_Directory_Attribute) then
         declare
            Dir : constant GNATCOLL.VFS.Filesystem_String :=
              GNATCOLL.VFS.Filesystem_String
                (Project.Attribute_Value (Output_Directory_Attribute));

         begin
            return
              GNATCOLL.VFS.Create_From_Base
                (Dir, Project.Project_Path.Dir.Full_Name.all);
         end;

      else
         return
           GNATCOLL.VFS.Create_From_Dir
             (Project.Object_Dir,
              Name (Name'First .. Name'Last - Extension'Length) & ".output");
      end if;
   end Codepeer_Output_Directory;

   -------------------
   -- Hide_Messages --
   -------------------

   procedure Hide_Messages
     (Self : not null access Module_Id_Record'Class;
      File : Code_Analysis.File_Access)
   is
      procedure Process_Subprogram
        (Position : Code_Analysis.Subprogram_Maps.Cursor);

      procedure Process_Message
        (Position : CodePeer.Message_Vectors.Cursor);

      ---------------------
      -- Process_Message --
      ---------------------

      procedure Process_Message
        (Position : CodePeer.Message_Vectors.Cursor)
      is
         Message : constant CodePeer.Message_Access :=
           CodePeer.Message_Vectors.Element (Position);

      begin
         if Message.Message /= null then
            Message.Message.Set_Flags ((others => False));
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
         Data            : CodePeer.Subprogram_Data'Class
                             renames CodePeer.Subprogram_Data'Class
                             (Subprogram_Node.Analysis_Data.CodePeer_Data.all);

      begin
         Data.Messages.Iterate (Process_Message'Access);
      end Process_Subprogram;

   begin
      Self.Filter_Criteria.Files.Delete (File);
      File.Subprograms.Iterate (Process_Subprogram'Access);
   end Hide_Messages;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self : access Module_Id_Record'Class;
      File : Virtual_File)
   is
      use type CodePeer.Messages_Reports.Messages_Report;
      use type Code_Analysis.Code_Analysis_Tree;

      Input   : Input_Sources.File.File_Input;
      Reader  : CodePeer.Bridge.Inspection_Readers.Reader;

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

      --  Remove messages from the messages container

      Module.Listener.Set_Cleanup_Mode (True);
      Get_Messages_Container (Module.Kernel).Remove_Category
        (CodePeer_Category_Name, Empty_Message_Flags);
      Module.Listener.Set_Cleanup_Mode (False);

      --  Load code review information

      if File.Is_Regular_File then
         Input_Sources.File.Open (+File.Full_Name, Input);
         Reader.Parse
           (Input, GPS.Kernel.Kernel_Handle (Self.Kernel), Self.Tree);
         Input_Sources.File.Close (Input);

         --  Create codepeer report window

         CodePeer.Reports.Gtk_New
           (Self.Report,
            GPS.Kernel.Kernel_Handle (Self.Kernel),
            GPS.Kernel.Modules.Module_ID (Self),
            Self.Tree);
         Context_CB.Connect
           (Self.Report,
            Gtk.Widget.Signal_Destroy,
            Context_CB.To_Marshaller (On_Destroy'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null, null));
         Context_CB.Connect
           (Self.Report.Messages_Report,
            CodePeer.Messages_Reports.Signal_Activated,
            Context_CB.To_Marshaller (On_Activate'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null, null));
         Context_CB.Connect
           (Self.Report.Messages_Report,
            CodePeer.Messages_Reports.Signal_Criteria_Changed,
            Context_CB.To_Marshaller (On_Criteria_Changed'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null, null));

         GPS.Kernel.MDI.Gtk_New
           (Self.Report_Subwindow, Self.Report, Module => Self);
         Self.Report_Subwindow.Set_Title (-"CodePeer report");
         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.Report_Subwindow);

         --  Setup filter criteria

         Self.Filter_Criteria.Files.Clear;
         Self.Tree.Iterate (Process_Project'Access);
         Self.Report.Messages_Report.Update_Criteria (Self.Filter_Criteria);

         --  Update location view

         Editors.Show_Annotations_In_Opened_Editors (Self);
         Self.Update_Location_View;
         Self.Fill_Object_Races;

         --  Raise report window

         Self.Report_Subwindow.Raise_Child;

      else
         Console.Insert
           (Self.Kernel,
            File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => Console.Error);
      end if;
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
                     Context.Module.Report.Messages_Report.Get_Selected_File;
      Subprogram : constant Code_Analysis.Subprogram_Access :=
                     Context.Module.Report.
                       Messages_Report.Get_Selected_Subprogram;

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
      Mode : constant String := Get_Build_Mode (Kernel);

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");
      CodePeer.Module.Bridge.Inspection (Module);
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
   end On_Display_Code_Review;

   --------------------------
   -- On_Regenerate_Report --
   --------------------------

   procedure On_Regenerate_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode : constant String := Get_Build_Mode (Kernel);

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

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
            Review
              (Module,
               Force => False,
               Build_Target  => "Regenerate CodePeer Report");
         end if;
      end;

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
   end On_Regenerate_Report;

   --------------------------
   -- On_Regenerate_Report --
   --------------------------

   procedure On_HTML_Report
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode : constant String := Get_Build_Mode (Kernel);
   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

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

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
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
      Mode    : constant String :=
                  Get_Build_Mode (Kernel_Handle (Module.Kernel));

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

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

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
   end On_Edit_Text_Listing;

   ---------------------------
   -- On_Edit_Text_Overview --
   ---------------------------

   procedure On_Edit_Text_Overview
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode : constant String :=
               Get_Build_Mode (Kernel_Handle (Module.Kernel));

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

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

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
   end On_Edit_Text_Overview;

   -----------------
   -- On_Edit_Log --
   -----------------

   procedure On_Edit_Log
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Mode  : constant String :=
                Get_Build_Mode (Kernel_Handle (Module.Kernel));

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

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

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
   end On_Edit_Log;

   --------------------
   -- On_Remove_Lock --
   --------------------

   procedure On_Remove_Lock
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Mode : constant String :=
               Get_Build_Mode (Kernel_Handle (Module.Kernel));

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

      declare
         Lock_File : constant Virtual_File :=
                       Create_From_Dir (Codepeer_Output_Directory
                                         (Get_Project (Kernel)),
                                        "inspector.lock");
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
            Console.Insert
              (Kernel, -"no lock file found: " & Lock_File.Display_Full_Name);
         end if;
      end;

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
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

   -------------
   -- Analyze --
   -------------

   procedure Analyze
     (Kernel : GPS.Kernel.Kernel_Handle;
      Action : CodePeer_Action) is
   begin
      Module.Action := Action;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => True,
         Build_Mode  => "codepeer",
         Synchronous => False);
   exception
      when E : others =>
         Trace (Me, E);
   end Analyze;

   --------------------
   -- On_Analyze_All --
   --------------------

   procedure On_Analyze_All
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Analyze (Kernel, Run_All);
   end On_Analyze_All;

   ---------------------
   -- On_Analyze_Root --
   ---------------------

   procedure On_Analyze_Root
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Analyze (Kernel, Run_Project);
   end On_Analyze_Root;

   ---------------------
   -- On_Analyze_File --
   ---------------------

   procedure On_Analyze_File
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Analyze (Kernel, Run_File);
   end On_Analyze_File;

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
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
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
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => False,
         Build_Mode  => "codepeer",
         Synchronous => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Generate_SCIL;

   --------------------
   -- On_Remove_SCIL --
   --------------------

   procedure On_Remove_SCIL
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Temp_SCIL : constant Filesystem_String := "Insp_";
      Objs      : constant GNATCOLL.VFS.File_Array :=
                    Object_Path (Get_Project (Kernel), True, True);
      Dirs      : File_Array_Access;
      Ignore    : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Console.Insert (Kernel, -"Deleting SCIL directories...");

      --  Remove all SCIL and Insp_* directories under each <obj>/codepeer dir.
      --  Ignore errors on e.g. read-only or non-existent directories.

      for J in Objs'Range loop
         Dirs := Read_Dir (Create_From_Dir (Objs (J), "codepeer"), Dirs_Only);

         for K in Dirs'Range loop
            declare
               Base : constant Filesystem_String := Dirs (K).Base_Name;
            begin
               if Base = "SCIL"
                 or else
                   (Base'Length > Temp_SCIL'Length
                    and then
                      Base (Base'First .. Base'First + Temp_SCIL'Length - 1)
                        = Temp_SCIL)
               then
                  Remove_Dir (Dir       => Dirs (K),
                              Recursive => True,
                              Success   => Ignore);
               end if;
            end;
         end loop;

         Unchecked_Free (Dirs);
      end loop;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Remove SCIL"),
         Force       => True,
         Build_Mode  => "codepeer",
         Synchronous => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Remove_SCIL;

   ------------------------
   -- On_Remove_CodePeer --
   ------------------------

   procedure On_Remove_CodePeer
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Objs   : constant GNATCOLL.VFS.File_Array :=
                Object_Path (Get_Project (Kernel), True, True);
      Ignore : Boolean;
      pragma Unreferenced (Ignore);

   begin
      --  Remove all <obj>/codepeer dirs. Ignore errors on e.g. read-only
      --  or non-existent directories.

      for J in Objs'Range loop
         Remove_Dir (Dir       => Create_From_Dir (Objs (J), "codepeer"),
                     Recursive => True,
                     Success   => Ignore);
      end loop;

      Console.Insert
        (Kernel, -"Deleted all CodePeer artefacts.", Add_LF => False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Remove_CodePeer;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      Hook_Data : constant GPS.Kernel.Standard_Hooks.
                    Compilation_Finished_Hooks_Args :=
                      GPS.Kernel.Standard_Hooks.
                        Compilation_Finished_Hooks_Args (Data.all);
      Mode      : constant String :=
                    CodePeer.Shell_Commands.Get_Build_Mode
                      (GPS.Kernel.Kernel_Handle (Kernel));
      Action    : constant CodePeer_Action := Module.Action;

   begin
      Module.Action := None;

      if (Hook_Data.Status /= 0 and then Action not in CodePeer_Action_Run)
        or else Action = None
        or else Hook_Data.Category /= "CodePeer"
      then
         return;
      end if;

      case Action is
         when Run_All =>
            Review (Module, Force => True, Build_Target => "Run CodePeer");

         when Run_Project =>
            Review
              (Module, Force => True, Build_Target => "Run CodePeer Root");

         when Run_File =>
            Review
              (Module, Force => True, Build_Target => "Run CodePeer File");

         when Quick_Run =>
            Review
              (Module, Force => True, Build_Target => "Run CodePeer Quickly");

         when Load_UI =>
            CodePeer.Shell_Commands.Set_Build_Mode
              (GPS.Kernel.Kernel_Handle (Kernel), "codepeer");
            CodePeer.Module.Bridge.Inspection (Module);
            CodePeer.Shell_Commands.Set_Build_Mode
              (GPS.Kernel.Kernel_Handle (Kernel), Mode);

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
      Context.Module.Report.Messages_Report.Update_Criteria
        (Context.Module.Filter_Criteria);
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

      procedure Process_Category (Position : String_Sets.Cursor);
      --  Removes all messages of the category from messages container

      ----------------------
      -- Process_Category --
      ----------------------

      procedure Process_Category (Position : String_Sets.Cursor) is
         Category : constant String := String_Sets.Element (Position);

      begin
         Get_Messages_Container (Context.Module.Kernel).Remove_Category
           (Category, Race_Message_Flags);
      end Process_Category;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (Position : Code_Analysis.File_Maps.Cursor) is
         File : constant Code_Analysis.File_Access :=
                  Code_Analysis.File_Maps.Element (Position);

      begin
         Editors.Hide_Annotations (Context.Module, File);
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
      --  Switch listener to cleanup mode to allow to destroy messages

      Module.Listener.Set_Cleanup_Mode (True);

      --  Hide all annotations

      Context.Module.Tree.Iterate (Process_Project'Access);

      --  Cleanup location view

      Get_Messages_Container (Context.Module.Kernel).Remove_Category
        (CodePeer_Category_Name, Empty_Message_Flags);
      Context.Module.Object_Race_Categories.Iterate (Process_Category'Access);

      --  Cleanup filter criteria

      Context.Module.Filter_Criteria.Files.Clear;
      Context.Module.Filter_Criteria.Categories.Clear;

      --  Mark report as destroyed

      Context.Module.Report := null;
      Context.Module.Report_Subwindow := null;

      --  Cleanup project tree

      Code_Analysis.Free_Code_Analysis (Context.Module.Tree);

      --  Switch listener back to normal mode

      Module.Listener.Set_Cleanup_Mode (False);

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
      Editors.Hide_Annotations (Context.Module, Context.File);

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
      Context.Module.Hide_Messages (Context.File);

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
      CodePeer.Module.Bridge.Add_Audit_Record
        (Context.Module, Context.Message);
      Context.Module.Report.Messages_Report.Update;
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Message_Reviewed;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);

   begin
      Module.Annotation_Style.Set_Background
        (Module.Annotation_Color.Get_Pref);
      Module.Message_Styles (CodePeer.High).Set_Background
        (Module.Message_Colors (CodePeer.High).Get_Pref);
      Module.Message_Styles (CodePeer.Medium).Set_Background
        (Module.Message_Colors (CodePeer.Medium).Get_Pref);
      Module.Message_Styles (CodePeer.Low).Set_Background
        (Module.Message_Colors (CodePeer.Low).Get_Pref);
      Module.Message_Styles (CodePeer.Informational).Set_Background
        (Module.Message_Colors (CodePeer.Informational).Get_Pref);
      Module.Message_Styles (CodePeer.Suppressed).Set_Background
        (Module.Message_Colors (CodePeer.Suppressed).Get_Pref);
   end On_Preferences_Changed;

   -----------------------------
   -- On_Project_Changed_Hook --
   -----------------------------

   procedure On_Project_Changed_Hook
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Module.Listener.Set_Cleanup_Mode (True);
      Remove_Codepeer_Messages (Kernel);
      --  Remove all messages of all categories starting from
      --  Codepeer_Category_Prefix to be sure that all possible categories are
      --  removed.
      Module.Listener.Set_Cleanup_Mode (False);
   end On_Project_Changed_Hook;

   --------------------------
   -- On_Remove_XML_Review --
   --------------------------

   procedure On_Remove_XML_Review
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Mode : constant String := Get_Build_Mode (Kernel);

   begin
      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), "codepeer");

      Bridge.Remove_Inspection_Cache_File (Module);

      CodePeer.Shell_Commands.Set_Build_Mode
        (Kernel_Handle (Module.Kernel), Mode);

   exception
      when E : others =>
         Trace (Me, E);
         CodePeer.Shell_Commands.Set_Build_Mode
           (Kernel_Handle (Module.Kernel), Mode);
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
      Editors.Show_Annotations (Context.Module, Context.File);

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
      Context.Module.Show_Messages (Context.File);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Show_Messages;

   --------------------------------
   -- Remove_Codepeer_Categories --
   --------------------------------

   procedure Remove_Codepeer_Messages
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Container  : constant GPS.Kernel.Messages.Messages_Container_Access :=
                     Get_Messages_Container (Kernel);
      Categories : constant GPS.Kernel.Messages.Unbounded_String_Array :=
                     Container.Get_Categories;
      Category   : Ada.Strings.Unbounded.Unbounded_String;

   begin
      for J in Categories'Range loop
         Category := Categories (J);

         if Ada.Strings.Unbounded.Length (Category)
              >= CodePeer_Category_Prefix'Length
           and then Ada.Strings.Unbounded.Slice
                      (Category, 1, CodePeer_Category_Prefix'Length)
                          = CodePeer_Category_Prefix
         then
            Container.Remove_Category
              (Ada.Strings.Unbounded.To_String (Category),
               Empty_Message_Flags);
         end if;
      end loop;
   end Remove_Codepeer_Messages;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : CodePeer.Message_Access;
      File    : Virtual_File)
   is
      Input  : Input_Sources.File.File_Input;
      Reader : CodePeer.Bridge.Audit_Trail_Readers.Reader;

   begin
      if File.Is_Regular_File then

         --  Load inspection information

         Input_Sources.File.Open (+File.Full_Name, Input);
         Reader.Parse (Input, Message.Audit);
         Input_Sources.File.Close (Input);

         Message.Audit_Loaded := True;

         Module.Review_Message (Message);

      else
         Console.Insert
           (Self.Kernel,
            File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => Console.Error);
      end if;
   end Review_Message;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : CodePeer.Message_Access)
   is
      Review : CodePeer.Message_Review_Dialogs.Message_Review_Dialog;

   begin
      if not Message.Audit_Loaded then
         CodePeer.Module.Bridge.Review_Message
           (CodePeer_Module_Id (Self), Message);

      else
         --  Create and show review dialog

         CodePeer.Message_Review_Dialogs.Gtk_New (Review, Message);
         Review.Set_Transient_For (Self.Kernel.Get_Main_Window);
         Review.Show_All;
         Context_CB.Connect
           (Review,
            CodePeer.Message_Review_Dialogs.Signal_Ok_Activated,
            Context_CB.To_Marshaller (On_Message_Reviewed'Access),
            (CodePeer_Module_Id (Self), null, null, Message));
      end if;
   end Review_Message;

   --------------------------
   -- Update_Location_View --
   --------------------------

   procedure Update_Location_View (Self : access Module_Id_Record'Class) is

      procedure Process_File (Position : CodePeer.File_Sets.Cursor);

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (Position : CodePeer.File_Sets.Cursor) is

         procedure Process_Subprogram
           (Position : Code_Analysis.Subprogram_Maps.Cursor);

         procedure Process_Message
           (Position : CodePeer.Message_Vectors.Cursor);

         File : constant Code_Analysis.File_Access :=
                  CodePeer.File_Sets.Element (Position);

         ---------------------
         -- Process_Message --
         ---------------------

         procedure Process_Message
           (Position : CodePeer.Message_Vectors.Cursor)
         is
            Message : constant CodePeer.Message_Access :=
              CodePeer.Message_Vectors.Element (Position);

            function Probability_Image
              (Message : CodePeer.Message_Access) return String;
            --  Return an suitable Image corresponding to Message's ranking

            function Image
              (Message : CodePeer.Message_Access) return String;
            --  Return complete text of the Message

            function Flags return GPS.Kernel.Messages.Message_Flags;
            --  Return set of flags depending from lifeage of the
            --  message. "Removed" messages are displayed only in
            --  locations view, others displayed in both locations view
            --  end editor.

            procedure Create_GPS_Message;
            --  Create GPS message

            ------------------------
            -- Create_GPS_Message --
            ------------------------

            procedure Create_GPS_Message is
               Primary : constant Simple_Message_Access :=
                 Create_Simple_Message
                   (Get_Messages_Container (Self.Kernel),
                    CodePeer_Category_Name,
                    File.Name,
                    Message.Line,
                    Basic_Types.Visible_Column_Type (Message.Column),
                    Image (Message),
                    Message_Ranking_Level'Pos (Message.Current_Ranking),
                    Flags);
               Style   : constant Style_Access :=
                 Module.Message_Styles (Message.Current_Ranking);

            begin
               Message.Message := GPS.Kernel.Messages.Message_Access (Primary);

               --  "Removed" messages are not highlighted in the source
               --  editor.

               if Style /= null
                 and then Message.Lifeage /= Removed
               then
                  Primary.Set_Highlighting
                    (Get_Or_Create_Style_Copy
                       (Kernel_Handle (Self.Kernel),
                        Get_Name (Style) & '/' & CodePeer_Category_Name,
                        Style));
               end if;

               Primary.Set_Action
                 (new GPS.Editors.Line_Information.Line_Information_Record'
                    (Text               => null,
                     Tooltip_Text       => new String'("Review message"),
                     Image              =>
                       Gtk.Widget.Gtk_Widget
                         (Self.Kernel.Get_Main_Window).Render_Icon
                         (Code_Analysis_GUI.Post_Analysis_Cst,
                          Gtk.Enums.Icon_Size_Menu),
                     Associated_Command =>
                     new Commands.CodePeer.Review_Message_Command'
                       (Commands.Root_Command with
                          CodePeer_Module_Id (Self), Message)));

               if Message.From_File /= No_File then
                  declare
                     Text : constant String :=
                       "(see also "
                         & String (Message.From_File.Full_Name.all)
                       & ":"
                       & Ada.Strings.Fixed.Trim
                       (Positive'Image (Message.From_Line),
                        Ada.Strings.Both)
                       & ":"
                       & Ada.Strings.Fixed.Trim
                       (Positive'Image (Message.From_Column),
                        Ada.Strings.Both)
                       & ")";

                  begin
                     GPS.Kernel.Messages.Hyperlink.Create_Hyperlink_Message
                       (GPS.Kernel.Messages.Message_Access (Primary),
                        Message.From_File,
                        Message.From_Line,
                        Basic_Types.Visible_Column_Type
                          (Message.From_Column),
                        Text,
                        Text'First + 10,
                        Text'Last - 1,
                        (Editor_Side => False,
                         Locations   => True));
                  end;
               end if;
            end Create_GPS_Message;

            -----------
            -- Flags --
            -----------

            function Flags return GPS.Kernel.Messages.Message_Flags is
            begin
               if Message.Lifeage = Removed then
                  return (Editor_Side => False, Locations => True);

               else
                  return (Editor_Side => True, Locations => True);
               end if;
            end Flags;

            -----------
            -- Image --
            -----------

            function Image
              (Message : CodePeer.Message_Access) return String
            is
            begin
               if Message.Text'Length = 0
                 or else Message.Text (Message.Text'First) = ':'
               then
                  return
                    Probability_Image (Message) & ": "
                    & Message.Category.Name.all
                    & Message.Text.all;
               else
                  return
                    Probability_Image (Message) & ": "
                    & Message.Category.Name.all & " "
                    & Message.Text.all;
               end if;
            end Image;

            -----------------------
            -- Probability_Image --
            -----------------------

            function Probability_Image
              (Message : CodePeer.Message_Access) return String
            is
               function Decorate (S : String) return String;
               --  Append " warning" after S if Message is a warning

               function Decorate (S : String) return String is
               begin
                  if Message.Is_Check then
                     return S;
                  else
                     return S & " warning";
                  end if;
               end Decorate;

            begin
               case Message.Current_Ranking is
                  when CodePeer.High =>
                     return Decorate ("high");

                  when CodePeer.Medium =>
                     return Decorate ("medium");

                  when CodePeer.Low =>
                     return Decorate ("low");

                  when CodePeer.Informational =>
                     return "info";

                  when CodePeer.Suppressed =>
                     return "suppressed";
               end case;
            end Probability_Image;

         begin
            if Self.Filter_Criteria.Lineages (Message.Lifeage)
              and then Self.Filter_Criteria.Rankings (Message.Current_Ranking)
              and then Self.Filter_Criteria.Categories.Contains
                (Message.Category)
            then
               if Message.Message = null then
                  Create_GPS_Message;

               else
                  Message.Message.Set_Flags (Flags);
               end if;

            else
               if Message.Message /= null then
                  Message.Message.Set_Flags ((others => False));
               end if;
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
            Data            : CodePeer.Subprogram_Data'Class
            renames CodePeer.Subprogram_Data'Class
              (Subprogram_Node.Analysis_Data.CodePeer_Data.all);

         begin
            Data.Messages.Iterate (Process_Message'Access);
         end Process_Subprogram;

      begin
         File.Subprograms.Iterate (Process_Subprogram'Access);
      end Process_File;

   begin
      Do_Not_Goto_First_Location (Self.Kernel);
      Get_Messages_Container (Self.Kernel).Set_Sort_Order_Hint
        (CodePeer_Category_Name, Alphabetical);

      Self.Filter_Criteria.Files.Iterate (Process_File'Access);
   end Update_Location_View;

   -----------------------------
   -- Race_Condition_Category --
   -----------------------------

   function Race_Condition_Category (Name : String) return String is
   begin
      return CodePeer_Category_Prefix & Name & " race condition";
   end Race_Condition_Category;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      procedure Initialize_Style
        (Style      : out GPS.Styles.UI.Style_Access;
         Name       : String;
         Preference : Default_Preferences.Color_Preference;
         Speedbar   : Boolean);
      --  Initializes style and sets background color from preference

      ----------------------
      -- Initialize_Style --
      ----------------------

      procedure Initialize_Style
        (Style      : out GPS.Styles.UI.Style_Access;
         Name       : String;
         Preference : Default_Preferences.Color_Preference;
         Speedbar   : Boolean) is
      begin
         Style :=
           GPS.Kernel.Styles.Get_Or_Create_Style
             (GPS.Kernel.Kernel_Handle (Kernel), Name);
         Style.Set_Background (Preference.Get_Pref);
         Style.Set_In_Speedbar (Speedbar);
      end Initialize_Style;

      Submenu_Factory : GPS.Kernel.Modules.UI.Submenu_Factory;
      Menu            : constant String := -"/_CodePeer";
      Advanced_Menu   : constant String := Menu & (-"/Advanced");
      Executable      : constant Virtual_File :=
                          Locate_On_Path ("codepeer");
      Sep             : Gtk_Separator_Menu_Item;
      Filter          : GPS.Kernel.Action_Filter;

   begin
      if Executable = No_File then
         --  Do not register the CodePeer module if the codepeer executable
         --  cannot be found.

         return;
      end if;

      Module          := new Module_Id_Record (Kernel);
      Submenu_Factory := new Submenu_Factory_Record (Module);

      Module.Register_Module (Kernel, "CodePeer");
      Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => "CodePeer",
         Label   => -"CodePeer",
         Filter  => GPS.Kernel.Lookup_Filter (Kernel, "Project only")
           or GPS.Kernel.Lookup_Filter (Kernel, "In project"),
         Submenu => Submenu_Factory);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Analyze All",
         Ref_Item    => -"Window",
         Callback    => On_Analyze_All'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Analyze _Root Project",
         Callback    => On_Analyze_Root'Access);

      Filter := new Filters.Ada_Generic_Filter_Record;
      --  Filter must be created using library level access type, overwise
      --  accessibility level check will fail.

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Analyze _File",
         Callback    => On_Analyze_File'Access,
         Filter      => Lookup_Filter (Kernel, "File")
                          and not Filter
                          and Create (Language => "ada"));

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Display Code Review",
         Callback    => On_Display_Code_Review'Access);

      Gtk_New (Sep);
      Register_Menu (Kernel, Menu, Sep);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Quick Analyze All",
         Callback    => On_Quick_Analyze_All'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Generate SCIL",
         Callback    => On_Generate_SCIL'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Run _CodePeer",
         Callback    => On_Run_Analysis_Manually'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Regenerate Report",
         Callback    => On_Regenerate_Report'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"Display _HTML Report",
         Callback    => On_HTML_Report'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"Text _Overview",
         Callback    => On_Edit_Text_Overview'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"_Text Listing",
         Callback    => On_Edit_Text_Listing'Access,
         Filter      => Lookup_Filter (Kernel, "File")
                          and Create (Language => "ada"));

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"CodePeer _Log",
         Callback    => On_Edit_Log'Access);

      Gtk_New (Sep);
      Register_Menu (Kernel, Advanced_Menu, Sep);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"R_emove Lock",
         Callback    => On_Remove_Lock'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"Remove _XML Code Review",
         Callback    => On_Remove_XML_Review'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"_Remove SCIL",
         Callback    => On_Remove_SCIL'Access);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Advanced_Menu,
         Text        => -"Remove _SCIL & DB",
         Callback    => On_Remove_CodePeer'Access);

      Module.Message_Colors (CodePeer.High) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Messages-High-Background",
           -"Color for 'high' messages",
           -"Plugins/CodePeer",
           -"Color to use for the background of high ranking messages",
           "#F75D59");

      Module.Message_Colors (CodePeer.Medium) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Messages-Medium-Background",
           -"Color for 'medium' messages",
           -"Plugins/CodePeer",
           -"Color to use for the background of medium ranking messages",
           "#F88017");

      Module.Message_Colors (CodePeer.Low) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Messages-Low-Background",
           -"Color for 'low' messages",
           -"Plugins/CodePeer",
           -"Color to use for the background of low ranking messages",
           "#FFE87C");

      Module.Message_Colors (CodePeer.Informational) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Messages-Informational-Background",
           -"Color for 'informational' messages",
           -"Plugins/CodePeer",
           -"Color to use for the background of information messages",
           "#DFDFDF");

      Module.Message_Colors (CodePeer.Suppressed) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Messages-Suppressed-Background",
           -"Color for 'suppressed' messages",
           -"Plugins/CodePeer",
           -"Color to use for the background of suppressed messages"
           & " messages",
           "#EFEFEF");

      Module.Annotation_Color :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           "CodePeer-Annotation-Background",
           -"Color for annotations background",
           -"Plugins/CodePeer",
           -"Color to use for the background of annotations",
           "#E9E9E9");

      Initialize_Style
        (Module.Annotation_Style,
         Annotation_Style_Name,
         Module.Annotation_Color,
         False);
      Initialize_Style
        (Module.Message_Styles (CodePeer.High),
         High_Probability_Style_Name,
         Module.Message_Colors (CodePeer.High),
         True);
      Initialize_Style
        (Module.Message_Styles (CodePeer.Medium),
         Medium_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Medium),
         True);
      Initialize_Style
        (Module.Message_Styles (CodePeer.Low),
         Low_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Low),
         True);
      Initialize_Style
        (Module.Message_Styles (CodePeer.Informational),
         Informational_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Informational),
         True);
      Initialize_Style
        (Module.Message_Styles (CodePeer.Suppressed),
         Suppressed_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Suppressed),
         True);

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Compilation_Finished_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Compilation_Finished'Access),
         Name => "codepeer.compilation_finished");
      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Preferences_Changed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Preferences_Changed'Access),
         "codepeer.preferences_changed");
      GPS.Kernel.Hooks.Add_Hook
        (Kernel,
         GPS.Kernel.Project_Changed_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Project_Changed_Hook'Access),
         "codepeer.project_changed");

      Module.Listener := new CodePeer.Listeners.Listener;
      GPS.Kernel.Messages.Register_Listener
        (GPS.Kernel.Messages.Get_Messages_Container (Kernel),
         GPS.Kernel.Messages.Listener_Access (Module.Listener),
         GPS.Kernel.Messages.Empty_Message_Flags);

      Editors.Register_Module (Kernel);
   end Register_Module;

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Messages
     (Self : not null access Module_Id_Record'Class;
      File : Code_Analysis.File_Access) is
   begin
      Self.Filter_Criteria.Files.Insert (File);
      Self.Update_Location_View;
   end Show_Messages;

end CodePeer.Module;

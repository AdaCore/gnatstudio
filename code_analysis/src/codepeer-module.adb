------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Input_Sources.File;

with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Widget;

with Basic_Types;
with Default_Preferences;        use Default_Preferences;
with GPS.Editors;
with GPS.Editors.Line_Information;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;          use GPS.Kernel.Styles;
with GPS.Styles;                 use GPS.Styles;
with GPS.Styles.UI;              use GPS.Styles.UI;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Xref;

with CodePeer.Backtrace_View;
with CodePeer.Bridge.Audit_Trail_Readers;
with CodePeer.Bridge.Inspection_Readers;
with CodePeer.Bridge.Status_Readers;
with CodePeer.Message_Review_Dialogs_V3;
with CodePeer.Messages_Reports;
with CodePeer.Module.Bridge;
with CodePeer.Module.Editors;
with CodePeer.Module.Filters;
with CodePeer.Shell_Commands;   use CodePeer.Shell_Commands;
with Commands, Commands.Interactive;  use Commands, Commands.Interactive;
with Commands.CodePeer;
with Code_Analysis_GUI;

package body CodePeer.Module is

   use type GPS.Editors.Editor_Mark'Class;
   use type GPS.Editors.Editor_Buffer'Class;

   Me : constant Trace_Handle := Create ("CodePeer");
   CodePeer_Subdir : constant Filesystem_String := "codepeer";

   type Module_Context is record
      Module  : CodePeer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
      Message : CodePeer.Message_Access;
   end record;

   package Context_CB is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Module_Context);

   type CodePeer_Build_Mode (Kernel : Kernel_Handle) is new
     Ada.Finalization.Controlled with
   record
      Mode : Unbounded_String;
   end record;
   overriding procedure Initialize (Self : in out CodePeer_Build_Mode);
   overriding procedure Finalize (Self : in out CodePeer_Build_Mode);
   --  Type used to automatically set build mode to "codepeer" on entry
   --  and reset it to the previous mode on exit. Also takes care of
   --  freezing the xref db.

   type CodePeer_Note is new Abstract_Note with record
      Message : Message_Access;
   end record;

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

   type Analyze_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Analyze_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze All" menu item is activated

   type Analyze_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Analyze_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze File" menu item is activated

   type Analyze_File_By_File_Command is new
     Interactive_Command with null record;
   overriding function Execute
     (Self : access Analyze_File_By_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze File By File" menu item is activated

   type Generate_SCIL_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Generate_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Generate SCIL" menu item is activated

   type Remove_SCIL_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Remove_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Remove SCIL" menu item is activated

   type Remove_Codepeer_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Remove_Codepeer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Remove SCIL & DB" menu item is activated

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "compilation_finished" hook, to schedule other tasks

   type Analyze_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Analyze_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze..." menu item is activated

   type Display_Code_Review_Command
      is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Display_Code_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Display code review" menu item is activated

   type Regenerate_Report_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Regenerate_Report_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Regenerate Report" menu item is activated

   type Display_HTML_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Display_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "HTML Report" menu item is activated

   type Generate_CSV_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Generate_CSV_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Generate CSV Report" menu item is activated

   type Text_Overview_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Text_Overview_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Edit Text Overview" menu item is activated

   type Text_Listing_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Text_Listing_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Edit Text Listing" menu item is activated

   type Log_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Log_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Edit CodePeer Log" menu item is activated

   type Remove_Lock_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Remove_Lock_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Remove Lock" menu item is activated

   type Remove_XML_Review_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self : access Remove_XML_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Remove XML Code Review" menu item is activated

   procedure On_Criteria_Changed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Message_Reviewed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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
   --  filter is applied. Messages container's messages are created when
   --  necessary.

   procedure Review
     (Module       : CodePeer.Module.CodePeer_Module_Id;
      Force        : Boolean;
      Build_Target : String);
   --  Launch CodePeer review using the specified build target.
   --  If Force is True, no dialog is displayed to change codepeer switches.

   procedure Load_CSV
     (Self : access Module_Id_Record'Class;
      File : Virtual_File);
   --  Load CSV file generated by CodePeer

   procedure On_Message_Selected_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when message in Locations view is selected. Updated Backtraces
   --  view.

   Output_Directory_Attribute   :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Output_Directory");
   Database_Directory_Attribute :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Database_Directory");

   Race_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (Editor_Side => True, Locations => True);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out CodePeer_Build_Mode) is
   begin
      Set_Unbounded_String (Self.Mode, Self.Kernel.Get_Build_Mode);
      Module.Kernel.Set_Build_Mode ("codepeer");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out CodePeer_Build_Mode) is
   begin
      Self.Kernel.Set_Build_Mode (To_String (Self.Mode));
   end Finalize;

   -----------------------
   -- Fill_Object_Races --
   -----------------------

   procedure Fill_Object_Races (Self : access Module_Id_Record'Class) is
      Data : CodePeer.Project_Data'Class
        renames CodePeer.Project_Data'Class
          (Self.Tree.Element
             (GPS.Kernel.Project.Get_Project
                (Self.Kernel)).Analysis_Data.CodePeer_Data.all);
      File : GNATCOLL.VFS.Virtual_File;

   begin
      for Object of Data.Object_Races loop
         if Object.Message = null then
            File :=
              (if Object.File /= No_File
               then Object.File
               else GNATCOLL.VFS.Create ("race conditions"));

            Object.Message :=
              GPS.Kernel.Messages.Message_Access
                (Create_Simple_Message
                   (Get_Messages_Container (Self.Kernel),
                    CodePeer_Category_Name,
                    File,
                    Object.Line,
                    GNATCOLL.Xref.Visible_Column (Object.Column),
                    Object.Name.all & " race condition",
                    0,
                    Race_Message_Flags,
                    True));

         else
            Object.Message.Set_Flags (Race_Message_Flags);
         end if;

         for Entry_Point of Object.Entry_Points loop
            for Object_Access of Entry_Point.Object_Accesses loop
               Object_Access.Message :=
                 GPS.Kernel.Messages.Message_Access
                   (Create_Simple_Message
                      (Object.Message,
                       Object_Access.File,
                       Object_Access.Line,
                       Basic_Types.Visible_Column_Type (Object_Access.Column),
                       (case Object_Access.Kind is
                           when Read =>
                              "read by " & Entry_Point.Entry_Point.Name.all,
                           when Update =>
                              "update by " & Entry_Point.Entry_Point.Name.all),
                       Race_Message_Flags));
            end loop;
         end loop;
      end loop;
   end Fill_Object_Races;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color
     (Ranking : CodePeer.Message_Ranking_Level) return Gdk.RGBA.Gdk_RGBA is
   begin
      return Module.Message_Colors (Ranking).Get_Pref;
   end Get_Color;

   ------------
   -- Review --
   ------------

   procedure Review
     (Module       : CodePeer.Module.CodePeer_Module_Id;
      Force        : Boolean;
      Build_Target : String)
   is
      Project : constant Project_Type := Get_Project (Module.Kernel);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel_Handle (Module.Kernel));
      pragma Unreferenced (Ensure_Build_Mode);
   begin
      Module.Action := Load_UI;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel_Handle (Module.Kernel),
         CodePeer.Shell_Commands.Build_Target
           (Module.Get_Kernel, Build_Target),
         Force       => Force,
         Build_Mode  => "codepeer",
         Synchronous => False,
         Dir         => CodePeer_Object_Directory (Project));
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
             (CodePeer_Object_Directory (Project),
              Name (Name'First .. Name'Last - Extension'Length) & ".db");
      end if;
   end Codepeer_Database_Directory;

   -------------------------------
   -- CodePeer_Object_Directory --
   -------------------------------

   function CodePeer_Object_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Object_Dir : constant Virtual_File := Project.Object_Dir;

   begin
      if Object_Dir /= No_File then
         return Object_Dir;

      else
         return Create_From_Dir (Project.Project_Path.Dir, CodePeer_Subdir);
      end if;
   end CodePeer_Object_Directory;

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
             (CodePeer_Object_Directory (Project),
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
     (Self            : access Module_Id_Record'Class;
      Inspection_File : Virtual_File;
      Status_File     : Virtual_File)
   is
      Messages : CodePeer.Message_Maps.Map;

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

      if Inspection_File.Is_Regular_File then
         declare
            Input   : Input_Sources.File.File_Input;
            Reader  : CodePeer.Bridge.Inspection_Readers.Reader;

         begin
            Input_Sources.File.Open (+Inspection_File.Full_Name, Input);
            Reader.Parse
              (Input,
               GPS.Kernel.Kernel_Handle (Self.Kernel),
               Self.Tree,
               Messages,
               Self.Version,
               Self.Race_Category);
            Input_Sources.File.Close (Input);
         end;

         if Self.Version = 3 then
            --  Load messages' review status data.

            if Status_File.Is_Regular_File then
               declare
                  Input   : Input_Sources.File.File_Input;
                  Reader  : CodePeer.Bridge.Status_Readers.Reader;

               begin
                  Input_Sources.File.Open (+Status_File.Full_Name, Input);
                  Reader.Parse (Input, Messages);
                  Input_Sources.File.Close (Input);
               end;

            else
               Self.Kernel.Insert
                 (Status_File.Display_Full_Name &
                  (-" does not exist. Review information is absent."),
                  Mode => GPS.Kernel.Error);
            end if;
         end if;

         --  Create codepeer report window

         CodePeer.Reports.Gtk_New
           (Self.Report,
            GPS.Kernel.Kernel_Handle (Self.Kernel),
            GPS.Kernel.Modules.Module_ID (Self),
            Self.Version,
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

         --  Setup message selection hook

         Add_Hook
           (Self.Kernel,
            Message_Selected_Hook,
            Wrapper (On_Message_Selected_Hook'Access),
            Name  => "codepeer.message_selected",
            Watch => Glib.Object.GObject (Self.Report));

         --  Setup filter criteria

         Self.Filter_Criteria.Files.Clear;

         for Project of Self.Tree.all loop
            for File of Project.Files loop
               Self.Filter_Criteria.Files.Insert (File);
            end loop;
         end loop;

         Self.Report.Messages_Report.Update_Criteria (Self.Filter_Criteria);

         --  Update location view

         Editors.Show_Annotations_In_Opened_Editors (Self);
         Self.Fill_Object_Races;
         Self.Update_Location_View;

         Raise_Locations_Window (Self.Kernel);

         --  Raise report window

         Self.Report_Subwindow.Raise_Child;

      else
         Self.Kernel.Insert
           (Inspection_File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);
      end if;
   end Load;

   --------------
   -- Load_CSV --
   --------------

   procedure Load_CSV
     (Self : access Module_Id_Record'Class;
      File : Virtual_File) is
   begin
      if Is_Regular_File (File) then
         Open_File_Editor
           (Self.Kernel,
            File,
            Project      => No_Project,
            New_File     => False,
            Force_Reload => True);
      else
         Self.Kernel.Insert
           (-"cannot find CSV file: " & File.Display_Full_Name,
            Mode => GPS.Kernel.Error);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Load_CSV;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context)
   is
      pragma Unreferenced (Item);

      --  use type Code_Analysis.File_Access;
      --  ??? Uncomment this line after I120-013 is be fixed
      use type Code_Analysis.Subprogram_Access;

      File       : constant Code_Analysis.File_Access :=
        Context.Module.Report.Messages_Report.Get_Selected_File;
      Project    : constant Code_Analysis.Project_Access :=
        Context.Module.Report.Messages_Report.Get_Selected_Project;
      Subprogram : constant Code_Analysis.Subprogram_Access :=
                     Context.Module.Report.
                       Messages_Report.Get_Selected_Subprogram;

   begin
      if Subprogram /= null then
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name,
            Project.Name,
            Subprogram.Line,
            Basic_Types.Visible_Column_Type (Subprogram.Column));

      elsif File /= null then
         GPS.Kernel.Standard_Hooks.Open_File_Editor
           (Context.Module.Kernel,
            File.Name,
            Project.Name);
      end if;

      if File /= null
        and then not Context.Module.Filter_Criteria.Files.Contains (File)
      then
         Context.Module.Filter_Criteria.Files.Include (File);
         Context.Module.Update_Location_View;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Activate;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Display_Code_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      CodePeer.Module.Bridge.Inspection (Module);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Regenerate_Report_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Info_File : constant Virtual_File :=
                    Create_From_Dir (Codepeer_Output_Directory
                                     (Get_Project (Kernel)),
                                     "Inspection_Info.xml");
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      if not Is_Regular_File (Info_File) then
         Kernel.Insert
           (Info_File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);
      else
         Review
           (Module,
            Force => False,
            Build_Target  => "Regenerate CodePeer Report");
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Display_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      declare
         HTML_File : constant Virtual_File :=
                       Create_From_Dir (Codepeer_Output_Directory
                                        (Get_Project (Kernel)),
                                        "/html/index.html");

      begin
         if not Is_Regular_File (HTML_File) then
            Kernel.Insert
              (HTML_File.Display_Full_Name &
               (-" does not exist. Please perform a full analysis first"),
               Mode => GPS.Kernel.Error);
         else
            Open_Html (Kernel, String (Full_Name (HTML_File).all));
         end if;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Generate_CSV_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Project : constant Project_Type := Get_Project (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      declare
         Info_File : constant Virtual_File :=
           Create_From_Dir (Codepeer_Output_Directory
                            (Get_Project (Kernel)),
                            "Inspection_Info.xml");

      begin
         if not Is_Regular_File (Info_File) then
            Kernel.Insert
              (Info_File.Display_Full_Name &
               (-" does not exist. Please perform a full analysis first"),
               Mode => GPS.Kernel.Error);
            return Commands.Failure;
         end if;
      end;

      Module.Inspection_File :=
        Create_From_Dir (CodePeer_Object_Directory (Project), "codepeer.csv");
      Module.Action := Load_CSV;
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel_Handle (Module.Kernel),
         CodePeer.Shell_Commands.Build_Target
           (Module.Get_Kernel, "Generate CSV Report"),
         Build_Mode  => "codepeer",
         Synchronous => False,
         Dir         => CodePeer_Object_Directory (Project));

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Text_Listing_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Text_File : constant Virtual_File :=
                    Create_From_Dir
                      (Codepeer_Output_Directory (Get_Project (Kernel)),
                       "list/" &
                       (+File_Information
                          (Context.Context).Display_Base_Name) & ".txt");
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      if Is_Regular_File (Text_File) then
         Open_File_Editor
           (Kernel,
            Text_File,
            Project      => Project_Information (Context.Context),
            New_File     => False,
            Force_Reload => True);
         return Commands.Success;
      else
         Kernel.Insert
           (-"cannot find text listing: " & Text_File.Display_Full_Name,
            Mode => GPS.Kernel.Error);
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Text_Overview_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
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
               Project      => Project_Information (Context.Context),
               New_File     => False,
               Force_Reload => True);
         else
            Kernel.Insert
              (-"cannot find text overview: " & Text_File.Display_Full_Name,
               Mode => GPS.Kernel.Error);
         end if;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Log_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Log_File : constant Virtual_File :=
                   Create_From_Dir (Codepeer_Output_Directory
                                    (Get_Project (Kernel)),
                                    "Inspection.log");
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      if Is_Regular_File (Log_File) then
         Open_File_Editor
           (Kernel,
            Log_File,
            Project      => Project_Information (Context.Context),
            New_File     => False,
            Force_Reload => True);
         return Commands.Success;
      else
         Kernel.Insert
           (-"cannot find log file: " & Log_File.Display_Full_Name,
            Mode => GPS.Kernel.Error);
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Remove_Lock_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
      Lock_File : constant Virtual_File :=
        Create_From_Dir (Codepeer_Output_Directory
                         (Get_Project (Kernel)),
                         "inspector.lock");
      Success   : Boolean;

   begin
      if Is_Regular_File (Lock_File) then
         Delete (Lock_File, Success);

         if Success then
            Kernel.Insert
              (-"deleted lock file: " & Lock_File.Display_Full_Name);
         else
            Kernel.Insert
              (-"could not delete lock file: " &
               Lock_File.Display_Full_Name);
         end if;
         return Commands.Success;
      else
         Kernel.Insert
           (-"no lock file found: " & Lock_File.Display_Full_Name);
         return Commands.Failure;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Analyze_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      Review (Module, Force => False, Build_Target => "Run CodePeer...");
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Analyze_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      Review (Module, Force => True, Build_Target => "Run CodePeer");
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Analyze_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      pragma Unreferenced (Self);
   begin
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target
           (Kernel, "Run CodePeer File"),
         Force       => True,
         Synchronous => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Analyze_File_By_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      pragma Unreferenced (Self);
   begin
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target
           (Kernel, "Run CodePeer File By File"),
         Force       => False,
         Synchronous => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Generate_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Generate SCIL"),
         Force       => False,
         Synchronous => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Remove_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Temp_SCIL : constant Filesystem_String := "Insp_";
      Objs      : constant GNATCOLL.VFS.File_Array :=
                    Object_Path (Get_Project (Kernel), True, True);
      Dirs      : File_Array_Access;
      Ignore    : Boolean;
      pragma Unreferenced (Self, Ensure_Build_Mode, Ignore);

   begin
      Kernel.Insert (-"Deleting SCIL directories...");

      --  Remove all SCIL and Insp_* directories under each <obj>/codepeer dir.
      --  Ignore errors on e.g. read-only or non-existent directories.

      for J in Objs'Range loop
         begin
            Dirs :=
              Read_Dir (Create_From_Dir (Objs (J), "codepeer"), Dirs_Only);
         exception
            when VFS_Directory_Error =>
               Dirs := null;
         end;

         if Dirs /= null then
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
         end if;
      end loop;

      CodePeer.Shell_Commands.Build_Target_Execute
        (Kernel,
         CodePeer.Shell_Commands.Build_Target (Kernel, "Remove SCIL"),
         Force       => True,
         Build_Mode  => "codepeer",
         Synchronous => False);

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Remove_Codepeer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      Objs   : constant GNATCOLL.VFS.File_Array :=
                Object_Path (Get_Project (Kernel), True, True);
      Ignore : Boolean;
      pragma Unreferenced (Self, Ensure_Build_Mode, Ignore);
   begin
      --  Remove all <obj>/codepeer dirs. Ignore errors on e.g. read-only
      --  or non-existent directories.

      for J in Objs'Range loop
         Remove_Dir (Dir       => Objs (J),
                     Recursive => True,
                     Success   => Ignore);
      end loop;

      Kernel.Insert
        (-"Deleted all CodePeer artefacts.", Add_LF => False);
      return Commands.Success;
   end Execute;

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
      Action    : constant CodePeer_Action := Module.Action;

   begin
      Module.Action := None;

      if Hook_Data.Status /= 0
        or else Action = None
        or else Hook_Data.Mode_Name /= "codepeer"
      then
         return;
      end if;

      case Action is
         when Load_UI =>
            declare
               Ensure_Build_Mode : CodePeer_Build_Mode
                 (Kernel_Handle (Kernel));
               pragma Unreferenced (Ensure_Build_Mode);
            begin
               CodePeer.Module.Bridge.Inspection (Module);
            end;

         when Audit_Trail =>
            Module.Review_Message
              (Module.Bridge_Message, Module.Inspection_File);

         when Load_Bridge_Results =>
            Module.Load (Module.Inspection_File, Module.Status_File);

         when Load_CSV =>
            Module.Load_CSV (Module.Inspection_File);

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
      --  Close Backtraces view.

      CodePeer.Backtrace_View.Close_Backtraces_View (Context.Module.Kernel);

      --  Switch listener to cleanup mode to allow to destroy messages

      Module.Listener.Set_Cleanup_Mode (True);

      --  Hide all annotations

      Context.Module.Tree.Iterate (Process_Project'Access);

      --  Cleanup location view

      Get_Messages_Container (Context.Module.Kernel).Remove_Category
        (CodePeer_Category_Name, Empty_Message_Flags);

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

   ------------------------------
   -- On_Message_Selected_Hook --
   ------------------------------

   procedure On_Message_Selected_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant Message_Hooks_Args := Message_Hooks_Args (Data.all);

   begin
      if D.Message.Has_Note (CodePeer_Note'Tag) then
         declare
            Message : constant Message_Access :=
              CodePeer_Note
                (D.Message.Get_Note (CodePeer_Note'Tag).all).Message;
         begin
            CodePeer.Backtrace_View.Display_Backtraces
              (Kernel,
               CodePeer.File_Data
                 (Message.File.Analysis_Data.CodePeer_Data.all)
                 .Backtraces_File,
               Message.Subprogram.Name.all,
               Message.Vns);
         end;
      end if;
   end On_Message_Selected_Hook;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      P : constant Preference := Get_Pref (Data);

   begin
      if P = Preference (Module.Annotation_Color) then
         Module.Annotation_Style.Set_Background
           (Module.Annotation_Color.Get_Pref);
      elsif P = Preference (Module.Message_Colors (CodePeer.High)) then
         Module.Message_Styles (CodePeer.High).Set_Background
           (Module.Message_Colors (CodePeer.High).Get_Pref);
      elsif P = Preference (Module.Message_Colors (CodePeer.Medium)) then
         Module.Message_Styles (CodePeer.Medium).Set_Background
           (Module.Message_Colors (CodePeer.Medium).Get_Pref);
      elsif P = Preference (Module.Message_Colors (CodePeer.Low)) then
         Module.Message_Styles (CodePeer.Low).Set_Background
           (Module.Message_Colors (CodePeer.Low).Get_Pref);
      elsif P =
        Preference (Module.Message_Colors (CodePeer.Info))
      then
         Module.Message_Styles (CodePeer.Info).Set_Background
           (Module.Message_Colors (CodePeer.Info).Get_Pref);
      elsif P = Preference (Module.Message_Colors (CodePeer.Suppressed)) then
         Module.Message_Styles (CodePeer.Suppressed).Set_Background
           (Module.Message_Colors (CodePeer.Suppressed).Get_Pref);
      end if;
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Remove_XML_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Self, Ensure_Build_Mode);
   begin
      Bridge.Remove_Inspection_Cache_File (Module);
      return Commands.Success;
   end Execute;

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
      Container : constant GPS.Kernel.Messages.Messages_Container_Access :=
                    Get_Messages_Container (Kernel);

   begin
      Container.Remove_Category (CodePeer_Category_Name, Empty_Message_Flags);
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
         Reader.Parse (Input, Message.Audit_V3);
         Input_Sources.File.Close (Input);
         Message.Audit_Loaded := True;
         Module.Review_Message (Message);

      else
         Self.Kernel.Insert
           (File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);
      end if;
   end Review_Message;

   --------------------
   -- Review_Message --
   --------------------

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : CodePeer.Message_Access)
   is
      Review_V3 : CodePeer.Message_Review_Dialogs_V3.Message_Review_Dialog;

   begin
      if not Message.Audit_Loaded then
         CodePeer.Module.Bridge.Review_Message
           (CodePeer_Module_Id (Self), Message);

      else
         --  Create and show review dialog

         CodePeer.Message_Review_Dialogs_V3.Gtk_New (Review_V3, Message);
         Review_V3.Set_Transient_For (Self.Kernel.Get_Main_Window);
         Review_V3.Show_All;
         Context_CB.Connect
           (Review_V3,
            CodePeer.Message_Review_Dialogs_V3.Signal_Ok_Activated,
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

            function Ranking_Image
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

            function Is_Visible return Boolean;
            --  Compute visibility of the message with current filter criteria

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
                    Message_Ranking_Level'Pos (Message.Ranking),
                    Flags,
                    False);
               Style   : constant Style_Access :=
                 Module.Message_Styles (Message.Ranking);

            begin
               Message.Message := GPS.Kernel.Messages.Message_Access (Primary);
               Primary.Set_Note
                 (new CodePeer_Note'(Abstract_Note with Message => Message));

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
               function Checks_Image return String;
               --  Returns image of set of originating checks for the message.

               ------------------
               -- Checks_Image --
               ------------------

               function Checks_Image return String is
                  Aux : Unbounded_String;

               begin
                  for Check of Message.Checks loop
                     if Length (Aux) = 0 then
                        Append (Aux, "(");

                     else
                        Append (Aux, ", ");
                     end if;

                     Append (Aux, Check.Name.all);
                  end loop;

                  if Length (Aux) /= 0 then
                     Append (Aux, ") ");
                  end if;

                  return To_String (Aux);
               end Checks_Image;

            begin
               if Message.Text'Length = 0
                 or else Message.Text (Message.Text'First) = ':'
               then
                  return
                    Ranking_Image (Message) & ": "
                    & Message.Category.Name.all
                    & Checks_Image
                    & Message.Text.all;
               else
                  return
                    Ranking_Image (Message) & ": "
                    & Message.Category.Name.all & " "
                    & Checks_Image
                    & Message.Text.all;
               end if;
            end Image;

            ----------------
            -- Is_Visible --
            ----------------

            function Is_Visible return Boolean is
            begin
               --  Simple criteria

               if not Self.Filter_Criteria.Lineages (Message.Lifeage)
                 or not Self.Filter_Criteria.Rankings (Message.Ranking)
                 or not Self.Filter_Criteria.Statuses (Message.Status)
               then
                  return False;
               end if;

               --  Category of the message should be selected

               if Self.Filter_Criteria.Categories.Contains
                 (Message.Category)
               then
                  return True;
               end if;

               --  or at least one check of the message should be selected

               if not Self.Filter_Criteria.Categories.Intersection
                 (Message.Checks).Is_Empty
               then
                  return True;
               end if;

               --  otherwise it is not visible

               return False;
            end Is_Visible;

            -------------------
            -- Ranking_Image --
            -------------------

            function Ranking_Image
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
               case Message.Ranking is
                  when CodePeer.High =>
                     return Decorate ("high");

                  when CodePeer.Medium =>
                     return Decorate ("medium");

                  when CodePeer.Low =>
                     return Decorate ("low");

                  when CodePeer.Info =>
                     return "info";

                  when CodePeer.Suppressed =>
                     return "suppressed";
               end case;
            end Ranking_Image;

         begin
            if Is_Visible then
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

      Data : CodePeer.Project_Data'Class
        renames CodePeer.Project_Data'Class
          (Self.Tree.Element
             (GPS.Kernel.Project.Get_Project
                (Self.Kernel)).Analysis_Data.CodePeer_Data.all);

   begin
      Get_Messages_Container (Self.Kernel).Set_Sort_Order_Hint
        (CodePeer_Category_Name, Alphabetical);

      Self.Filter_Criteria.Files.Iterate (Process_File'Access);

      --  Update state of race condition messages

      if Self.Race_Category /= null
        and then Self.Filter_Criteria.Categories.Contains (Self.Race_Category)
      then
         for Object of Data.Object_Races loop
            Object.Message.Set_Flags (Race_Message_Flags);

            for Entry_Point of Object.Entry_Points loop
               for Object_Access of Entry_Point.Object_Accesses loop
                  Object_Access.Message.Set_Flags (Race_Message_Flags);
               end loop;
            end loop;
         end loop;

      else
         for Object of Data.Object_Races loop
            for Entry_Point of Object.Entry_Points loop
               for Object_Access of Entry_Point.Object_Accesses loop
                  Object_Access.Message.Set_Flags (Empty_Message_Flags);
               end loop;
            end loop;

            Object.Message.Set_Flags (Empty_Message_Flags);
         end loop;
      end if;
   end Update_Location_View;

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
      Executable      : constant Virtual_File := Locate_On_Path ("codepeer");
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

      Filter := new Filters.Ada_Generic_Or_Separate_Filter_Record;

      Register_Action
        (Kernel, "codepeer analyze all", new Analyze_All_Command);
      Register_Action
         (Kernel, "codepeer analyze file", new Analyze_File_Command,
          Filter     => Lookup_Filter (Kernel, "File")
                        and not Filter
                        and Create (Language => "ada"));
      Register_Action
        (Kernel, "codepeer display code review",
         new Display_Code_Review_Command);
      Register_Action
        (Kernel, "codepeer analyze file by file",
         new Analyze_File_By_File_Command);
      Register_Action
        (Kernel, "codepeer generate scil", new Generate_SCIL_Command);
      Register_Action
        (Kernel, "codepeer analyze...", new Analyze_Command);
      Register_Action
        (Kernel, "codepeer regenerate report", new Regenerate_Report_Command);
      Register_Action
        (Kernel, "codepeer display html", new Display_HTML_Command);
      Register_Action
        (Kernel, "codepeer generate csv", new Generate_CSV_Command);
      Register_Action
        (Kernel, "codepeer text overview", new Text_Overview_Command);
      Register_Action
         (Kernel, "codepeer text listing", new Text_Listing_Command,
          Filter => Lookup_Filter (Kernel, "File")
                    and Create (Language => "ada"));
      Register_Action (Kernel, "codepeer log", new Log_Command);
      Register_Action
        (Kernel, "codepeer remove lock", new Remove_Lock_Command);
      Register_Action
        (Kernel, "codepeer remove xml review", new Remove_XML_Review_Command);
      Register_Action
        (Kernel, "codepeer remove scil", new Remove_SCIL_Command);
      Register_Action
        (Kernel, "codepeer remove scil and db", new Remove_Codepeer_Command);

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

      Module.Message_Colors (CodePeer.Info) :=
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
        (Module.Message_Styles (CodePeer.Info),
         Informational_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Info),
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
        (Kernel, GPS.Kernel.Preference_Changed_Hook,
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

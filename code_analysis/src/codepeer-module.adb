------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with GNAT.Expect.TTY;
with GNAT.Regpat;

with Input_Sources.File;

with Glib.Object;                    use Glib.Object;
with Gtk.Check_Menu_Item;            use Gtk.Check_Menu_Item;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Menu_Item;                  use Gtk.Menu_Item;

with Basic_Types;
with Default_Preferences;            use Default_Preferences;
with GPS.Editors;
with GPS.Editors.Line_Information;
with GPS.Intl;                       use GPS.Intl;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules.UI;          use GPS.Kernel.Modules.UI;
with GPS.Kernel.Style_Manager;       use GPS.Kernel.Style_Manager;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Xref;
with String_Utils;

with BT.Xml.Reader;
with CodePeer.Backtrace_View;
with CodePeer.Bridge.Audit_Trail_Readers;
with CodePeer.Bridge.Inspection_Readers;
with CodePeer.Bridge.Status_Readers;
with CodePeer.Message_Review_Dialogs_V3;
with CodePeer.Messages_Reports;      use CodePeer.Messages_Reports;
with CodePeer.Module.Actions;
with CodePeer.Module.Bridge;
with CodePeer.Module.Editors;
with CodePeer.Shell_Commands;        use CodePeer.Shell_Commands;
with Commands.CodePeer;
with Commands;                       use Commands;
with Code_Analysis_GUI;

package body CodePeer.Module is

   use type Code_Analysis.Code_Analysis_Tree;
   use type GPS.Editors.Editor_Mark'Class;
   use type GPS.Editors.Editor_Buffer'Class;

   Me : constant Trace_Handle := Create ("CodePeer");
   CodePeer_Subdir : constant Filesystem_String := "codepeer";

   type Module_Context is record
      Module  : CodePeer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
   end record;

   package Context_CB is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Module_Context);

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

   procedure On_Display_Values_Toggled
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);
   --  Handles change of state of display values item

   procedure On_Activate
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Destroy
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   type On_Compilation_Finished is new Compilation_Finished_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background : Boolean;
      Status : Integer);
   --  Callback for the "compilation_finished" hook, to schedule other tasks

   procedure On_Criteria_Changed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   procedure On_Message_Reviewed
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context);

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
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

   procedure Load_CSV
     (Self : access Module_Id_Record'Class;
      File : Virtual_File);
   --  Load CSV file generated by CodePeer

   procedure Check_CodePeer_Version (Self : access Module_Id_Record'Class);
   --  Checks version of CodePeer to enable workaround when necessary.

   type On_Message_Selected is new Message_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_Message_Selected;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Message : GPS.Kernel.Messages.Message_Access);
   --  Called when message in Locations view is selected. Updated Backtraces
   --  view.

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class)
      return Boolean;
   --  Called before GPS exits. Switchs perspective to default.

   Output_Directory_Attribute   :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Output_Directory");
   Database_Directory_Attribute :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "Database_Directory");
   CWE_Attribute :
     constant GNATCOLL.Projects.Attribute_Pkg_String :=
     GNATCOLL.Projects.Build ("CodePeer", "CWE");

   Race_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (Editor_Side => True, Locations => True);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out CodePeer_Build_Mode) is
      Mode : constant String := Self.Kernel.Get_Build_Mode;

   begin
      if Mode = "codepeer" then
         Self.Switch_Mode := False;

      else
         Self.Switch_Mode := True;
         Self.Mode := To_Unbounded_String (Mode);
         Module.Kernel.Set_Build_Mode ("codepeer");
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out CodePeer_Build_Mode) is
   begin
      if Self.Switch_Mode then
         Self.Kernel.Set_Build_Mode (To_String (Self.Mode));
      end if;
   end Finalize;

   ----------------------------
   -- Check_CodePeer_Version --
   ----------------------------

   procedure Check_CodePeer_Version (Self : access Module_Id_Record'Class) is
      Old_Version : aliased String :=
        "CodePeer ([0-9]+)\.([0-9]+)\.([0-9]+)[a-z]? ";
      New_Version : aliased String :=
        "CodePeer [0-9]+\.[0-9]+[a-z]? ";
      Regexps     : constant GNAT.Expect.Regexp_Array :=
        (Old_Version'Unchecked_Access, New_Version'Unchecked_Access);
      Codepeer    : constant Virtual_File := Locate_On_Path ("codepeer");
      Version     : aliased String := "--version";

   begin
      if Codepeer /= No_File then
         declare
            Descriptor : GNAT.Expect.TTY.TTY_Process_Descriptor;
            Match      : GNAT.Expect.Expect_Match;
            Matched    : GNAT.Regpat.Match_Array (0 .. 3);

         begin
            GNAT.Expect.Non_Blocking_Spawn
              (Descriptor  => Descriptor,
               Command     => Codepeer.Display_Full_Name,
               Args        => (1 => Version'Unchecked_Access),
               Err_To_Out  => True);
            GNAT.Expect.TTY.Expect (Descriptor => Descriptor,
                                    Result     => Match,
                                    Regexps    => Regexps,
                                    Matched    => Matched,
                                    Timeout    => 1_000);

            case Match is
               when 1 =>
                  --  Old format of version number

                  declare
                     Output : constant String :=
                       GNAT.Expect.TTY.Expect_Out (Descriptor);
                     Major  : constant Integer :=
                       Integer'Value
                         (Output (Matched (1).First .. Matched (1).Last));
                     Minor  : constant Integer :=
                       Integer'Value
                         (Output (Matched (2).First .. Matched (2).Last));
                     Patch  : constant Integer :=
                       Integer'Value
                         (Output (Matched (3).First .. Matched (3).Last));

                  begin
                     --  Bug in format's version selection was fixed in
                     --  CodePeer 3.1.2, limit version number to 3 for all
                     --  older versions.

                     if Major < 3 or else Minor < 1 or else Patch < 2 then
                        Self.Version_Limit := 3;
                     end if;
                  end;

               when 2 =>
                  --  New format of version number.
                  --
                  --  There is no known bugs here.

                  null;

               when others =>
                  --  Some error was occured.

                  null;
            end case;

            GNAT.Expect.TTY.Close (Descriptor);

         exception
            when GNAT.Expect.Process_Died =>
               GNAT.Expect.TTY.Close (Descriptor);
         end;
      end if;
   end Check_CodePeer_Version;

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

   --------------------------
   -- Get_CodePeer_Message --
   --------------------------

   function Get_CodePeer_Message
     (Message : GPS.Kernel.Messages.Message_Access)
      return CodePeer.Message_Access is
   begin
      if Message.Has_Note (CodePeer_Note'Tag) then
         return
           CodePeer_Note (Message.Get_Note (CodePeer_Note'Tag).all).Message;

      else
         return null;
      end if;
   end Get_CodePeer_Message;

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
     (Module       : not null access Module_Id_Record'Class;
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
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item       : Gtk.Menu_Item.Gtk_Menu_Item;
      Check_Item : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
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
                           File_Node));

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
                           File_Node));
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
                        File_Node));

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
                        File_Node));
               end if;
            end if;
         end;
      end if;

      Gtk.Check_Menu_Item.Gtk_New (Check_Item, -"Display values");
      Check_Item.Set_Active (Module.Display_Values);
      Menu.Append (Check_Item);
      Context_CB.Connect
        (Check_Item,
         Gtk.Check_Menu_Item.Signal_Toggled,
         Context_CB.To_Marshaller (On_Display_Values_Toggled'Access),
         Module_Context'
           (CodePeer_Module_Id (Factory.Module), null, null));
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
     (Kernel : not null access Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File
   is
      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel);
      pragma Unreferenced (Ensure_Build_Mode);

      Project   : constant Project_Type := Get_Project (Kernel);
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
     (Self             : access Module_Id_Record'Class;
      Inspection_File  : Virtual_File;
      Status_File      : Virtual_File;
      Bts_Directory    : Virtual_File;
      Output_Directory : Virtual_File) is
   begin
      if Self.Report_Subwindow /= null then
         --  Destroy old report window if present

         Self.Report_Subwindow.Destroy;
      end if;

      --  Switch to CodePeer perspective.
      Load_Perspective (Self.Kernel, "CodePeer");

      --  Remove messages from the messages container and clear backtraces data
      --  cache.

      Module.Listener.Set_Cleanup_Mode (True);
      Get_Messages_Container (Module.Kernel).Remove_Category
        (CodePeer_Category_Name, Empty_Message_Flags);
      BT.Xml.Reader.Clear;
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
               Self.Messages,
               Self.Version,
               Self.Race_Category);
            Input_Sources.File.Close (Input);
         end;

         --  Load messages' review status data.

         if Status_File.Is_Regular_File then
            declare
               Input  : Input_Sources.File.File_Input;
               Reader : CodePeer.Bridge.Status_Readers.Reader;

            begin
               Input_Sources.File.Open (+Status_File.Full_Name, Input);
               Reader.Parse (Input, Self.Messages);
               Input_Sources.File.Close (Input);
            end;

            Self.Has_Backtraces := Bts_Directory.Is_Directory;

         else
            Self.Kernel.Insert
              (Status_File.Display_Full_Name &
               (-" does not exist. Review information is absent."),
               Mode => GPS.Kernel.Error);
         end if;

         --  Create codepeer report window

         CodePeer.Reports.Gtk_New
           (Self.Report,
            GPS.Kernel.Kernel_Handle (Self.Kernel),
            Self.Version,
            Self.Tree);
         Context_CB.Connect
           (Self.Report,
            Gtk.Widget.Signal_Destroy,
            Context_CB.To_Marshaller (On_Destroy'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null));
         Context_CB.Connect
           (Self.Report.Messages_Report,
            CodePeer.Messages_Reports.Signal_Activated,
            Context_CB.To_Marshaller (On_Activate'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null));
         Context_CB.Connect
           (Self.Report.Messages_Report,
            CodePeer.Messages_Reports.Signal_Criteria_Changed,
            Context_CB.To_Marshaller (On_Criteria_Changed'Access),
            Module_Context'(CodePeer_Module_Id (Self), null, null));

         Self.Report_Subwindow := new Codepeer_Child_Record;
         GPS.Kernel.MDI.Initialize
           (Self.Report_Subwindow, Self.Report, Self.Kernel, Module => Self);
         Self.Report_Subwindow.Set_Title (-"CodePeer report");
         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.Report_Subwindow);

         --  Setup message selection hook

         Message_Selected_Hook.Add
            (new On_Message_Selected, Watch => Self.Report);

         --  Setup before exit hook, it is needed to switch to default
         --  perspective before end of GPS session.

         Before_Exit_Action_Hook.Add
            (new On_Before_Exit, Watch => Self.Report);

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

         --  Reset content of Backtraces view.

         CodePeer.Backtrace_View.Display_Backtraces
           (Self.Kernel,
            No_File,
            No_File,
            null,
            "",
            Natural_Sets.Empty_Set);

         --  Raise report window

         Self.Report_Subwindow.Raise_Child;

         --  Initialize backtraces information loader.

         BT.Xml.Reader.Initialize (String (Output_Directory.Full_Name.all));

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
         Open_File_Action_Hook.Run
           (Kernel       => Self.Kernel,
            File         => File,
            Project      => No_Project,
            New_File     => False,
            Force_Reload => True);
      else
         Self.Kernel.Insert
           (-"cannot find CSV file: " & File.Display_Full_Name,
            Mode => GPS.Kernel.Error);
      end if;
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
         Open_File_Action_Hook.Run
           (Kernel  => Context.Module.Kernel,
            File    => File.Name,
            Project => Project.Name,
            Line    => Subprogram.Line,
            Column  => Basic_Types.Visible_Column_Type (Subprogram.Column));

      elsif File /= null then
         Open_File_Action_Hook.Run
           (Kernel  => Context.Module.Kernel,
            File    => File.Name,
            Project => Project.Name);
      end if;

      if File /= null
        and then not Context.Module.Filter_Criteria.Files.Contains (File)
      then
         Context.Module.Filter_Criteria.Files.Include (File);
         Context.Module.Update_Location_View;
      end if;
   end On_Activate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background : Boolean;
      Status : Integer)
   is
      pragma Unreferenced (Kernel, Self, Category, Target);
      pragma Unreferenced (Shadow, Background);
      Action    : constant CodePeer_Action := Module.Action;
   begin
      Module.Action := None;

      if Status /= 0
        or else Action = None
        or else Mode /= "codepeer"
      then
         return;
      end if;

      case Action is
         when Load_UI =>
            CodePeer.Module.Bridge.Inspection (Module);

         when Audit_Trail =>
            Module.Review_Message
              (Module.Bridge_Message, Module.Inspection_File);

         when Load_Bridge_Results =>
            Module.Load
              (Module.Inspection_File,
               Module.Status_File,
               Module.Bts_Directory,
               Module.Output_Directory);

         when Load_CSV =>
            Module.Load_CSV (Module.Inspection_File);

         when None => null;
      end case;
   end Execute;

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
      --  Load the default perspective before closing the report view. This
      --  is necessary because the position of views is memorized at this
      --  time: if views are closed before the perspective change, their
      --  position is lost.
      Load_Perspective (Context.Module.Kernel, "Default");

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

      --  Clear backtraces data cache.

      BT.Xml.Reader.Clear;

      --  Switch listener back to normal mode

      Module.Listener.Set_Cleanup_Mode (False);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Destroy;

   -------------------------------
   -- On_Display_Values_Toggled --
   -------------------------------

   procedure On_Display_Values_Toggled
     (Item    : access Glib.Object.GObject_Record'Class;
      Context : Module_Context) is
   begin
      Context.Module.Display_Values :=
        Gtk.Check_Menu_Item.Gtk_Check_Menu_Item (Item).Get_Active;
   end On_Display_Values_Toggled;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  Load the default perspective before closing the report view. This
      --  is necessary because the position of views is memorized at this
      --  time: if views are closed before the perspective change, their
      --  position is lost.
      Load_Perspective (Kernel, "Default");

      --  Destroy report window

      Module.Report_Subwindow.Destroy;

      return True;
   end Execute;

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
      Context : Module_Context) is
   begin
      CodePeer.Module.Bridge.Add_Audit_Record
        (Context.Module,
         CodePeer.Message_Review_Dialogs_V3.Message_Review_Dialog_Record'Class
           (Item.all).Get_Messages.First_Element);
      Context.Module.Report.Messages_Report.Update;
      Context.Module.Update_Location_View;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Message_Reviewed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_Message_Selected;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Message : GPS.Kernel.Messages.Message_Access)
   is
      pragma Unreferenced (Self);

      Msg : constant Message_Access := Get_CodePeer_Message (Message);

   begin
      if Module.Has_Backtraces and then Msg /= null then
         CodePeer.Backtrace_View.Display_Backtraces
           (Kernel,
            Module.Output_Directory,
            Msg.File.Name,
            Message,
            Msg.Subprogram.Name.all,
            (if Msg.Is_Check
             then Msg.Vns
             else Natural_Sets.Empty_Set));
         --  Backtraces are displayed only for 'check' messages.
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Module.Listener.Set_Cleanup_Mode (True);

      --  Remove all messages of all categories starting from
      --  Codepeer_Category_Prefix to be sure that all possible categories are
      --  removed.

      Remove_Codepeer_Messages (Kernel);

      --  Clear backtraces data cache.

      BT.Xml.Reader.Clear;

      Module.Listener.Set_Cleanup_Mode (False);
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
         Reader.Parse (Input, Self.Messages);
         Input_Sources.File.Close (Input);
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

         CodePeer.Message_Review_Dialogs_V3.Gtk_New
           (Review_V3, Self.Kernel, Message);
         Review_V3.Set_Transient_For (Self.Kernel.Get_Main_Window);
         Review_V3.Show_All;
         Context_CB.Connect
           (Review_V3,
            CodePeer.Message_Review_Dialogs_V3.Signal_Ok_Activated,
            Context_CB.To_Marshaller (On_Message_Reviewed'Access),
            (CodePeer_Module_Id (Self), null, null));
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
                  Primary.Set_Highlighting (Style);
               end if;

               CodePeer_Module_Id (Self).Review_Command.Ref;
               Primary.Set_Action
                 (new GPS.Editors.Line_Information.Line_Information_Record'
                    (Text               => Null_Unbounded_String,
                     Tooltip_Text       => To_Unbounded_String
                       ("Review message"),
                     Image              => To_Unbounded_String
                       (Code_Analysis_GUI.Post_Analysis_Cst),
                     Message            =>
                       Create (Messages.Message_Access (Primary)),
                     Associated_Command =>
                       CodePeer_Module_Id (Self).Review_Command));

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

               function CWE_Image
                 (Category : Message_Category_Access) return String;
               --  Returns image of set of CWEs of given category

               ------------------
               -- Checks_Image --
               ------------------

               function Checks_Image return String is
                  Aux : Unbounded_String;

               begin
                  for Check of Message.Checks loop
                     if Length (Aux) = 0 then
                        Append (Aux, " (");

                     else
                        Append (Aux, ", ");
                     end if;

                     Append (Aux, Check.Name.all);
                     Append (Aux, CWE_Image (Check));
                  end loop;

                  if Length (Aux) /= 0 then
                     Append (Aux, ")");
                  end if;

                  return To_String (Aux);
               end Checks_Image;

               ---------------
               -- CWE_Image --
               ---------------

               function CWE_Image
                 (Category : Message_Category_Access) return String
               is
                  Project   : constant Project_Type :=
                    GPS.Kernel.Project.Get_Project (Self.Kernel);
                  Aux       : Unbounded_String;
                  Previous  : CWE_Identifier        := 0;
                  Delimiter : Natural               := 0;
                  --  Position of range delimiter.

               begin
                  if not Category.CWEs.Is_Empty
                    and then Project.Has_Attribute (CWE_Attribute)
                    and then
                      Ada.Characters.Handling.To_Lower
                        (Project.Attribute_Value (CWE_Attribute)) = "true"
                  then
                     for CWE of Category.CWEs loop
                        declare
                           Image : constant String :=
                             CWE_Identifier'Image (CWE.Identifier);

                        begin
                           if Length (Aux) = 0 then
                              Append (Aux, " [");
                              Append
                                (Aux, Image (Image'First + 1 .. Image'Last));
                              Delimiter := 0;

                           else
                              if Previous + 1 = CWE.Identifier then
                                 --  Continuous value

                                 if Delimiter = 0 then
                                    Append (Aux, '-');
                                    Delimiter := Length (Aux);
                                    Append
                                      (Aux,
                                       Image (Image'First + 1 .. Image'Last));

                                 else
                                    Replace_Slice
                                      (Aux,
                                       Delimiter + 1,
                                       Length (Aux),
                                       Image (Image'First + 1 .. Image'Last));
                                 end if;

                              else
                                 Delimiter := 0;
                                 Append (Aux, ',');
                                 Append
                                   (Aux,
                                    Image (Image'First + 1 .. Image'Last));
                              end if;
                           end if;

                           Previous := CWE.Identifier;
                        end;
                     end loop;

                     if Length (Aux) /= 0 then
                        Append (Aux, ']');
                     end if;

                     return To_String (Aux);

                  else
                     return "";
                  end if;
               end CWE_Image;

            begin
               if Message.Text'Length = 0
                 or else Message.Text (Message.Text'First) = ':'
               then
                  return
                    Ranking_Image (Message) & ": "
                    & Message.Category.Name.all
                    & Checks_Image
                    & CWE_Image (Message.Category)
                    & Message.Text.all;
               else
                  return
                    Ranking_Image (Message) & ": "
                    & Message.Category.Name.all
                    & Checks_Image
                    & CWE_Image (Message.Category) & " "
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

               --  or at least one CWE of the message or message's category
               --  should be selected

               if not Self.Filter_Criteria.CWEs.Intersection
                 ((if not Message.CWEs.Is_Empty
                  then Message.CWEs
                  else Message.Category.CWEs)).Is_Empty
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

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Codepeer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Report : constant CodePeer.Reports.Report :=
        CodePeer.Reports.Report (GPS_MDI_Child (Self).Get_Actual_Widget);
   begin
      return CodePeer.Reports.Build_Context (Report, Event);
   end Build_Context;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      procedure Initialize_Style
        (Style      : out Style_Access;
         Name       : String;
         Preference : Default_Preferences.Color_Preference;
         Speedbar   : Boolean);
      --  Initializes style and sets background color from preference

      ----------------------
      -- Initialize_Style --
      ----------------------

      procedure Initialize_Style
        (Style      : out Style_Access;
         Name       : String;
         Preference : Default_Preferences.Color_Preference;
         Speedbar   : Boolean) is
      begin
         Style := Get_Style_Manager
           (Kernel_Handle (Kernel)).Create_From_Preferences
             (Name,
              Fg_Pref => null,
              Bg_Pref => Preference);
         Set_In_Speedbar (Style, Speedbar);
      end Initialize_Style;

      Submenu_Factory : GPS.Kernel.Modules.UI.Submenu_Factory;
      Executable      : constant Virtual_File := Locate_On_Path ("codepeer");

   begin
      if Executable = No_File then
         --  Do not register the CodePeer module if the codepeer executable
         --  cannot be found.

         return;
      end if;

      --  Register backtrace view

      CodePeer.Backtrace_View.Register_Module (Kernel);

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

      CodePeer.Module.Actions.Register_Actions (Module);

      Module.Message_Colors (CodePeer.High) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Messages-High-Background",
           Label   => -"Color for 'high' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the background of high" &
             " ranking messages"),
           Default => "#F75D59");

      Module.Message_Colors (CodePeer.Medium) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Messages-Medium-Background",
           Label   => -"Color for 'medium' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the background of medium"
             & " ranking messages"),
           Default => "#F88017");

      Module.Message_Colors (CodePeer.Low) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Messages-Low-Background",
           Label   => -"Color for 'low' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the background of low"
             & " ranking messages"),
           Default => "#FFE87C");

      Module.Message_Colors (CodePeer.Info) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Messages-Informational-Background",
           Label   => -"Color for 'informational' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the background of information"
             & " messages"),
           Default => "#DFDFDF");

      Module.Message_Colors (CodePeer.Suppressed) :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Messages-Suppressed-Background",
           Label   => -"Color for 'suppressed' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the background of suppressed"
             & " messages"),
           Default => "#EFEFEF");

      Module.Annotation_Color :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Annotation-Background",
           Label   => -"Color for annotations background",
           Path    => -"CodePeer:Colors",
           Doc     => -"Color to use for the background of annotations",
           Default => "#E9E9E9");

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

      Compilation_Finished_Hook.Add (new On_Compilation_Finished);
      Project_Changed_Hook.Add (new On_Project_Changed);

      Module.Listener := new CodePeer.Listeners.Listener;
      GPS.Kernel.Messages.Register_Listener
        (GPS.Kernel.Messages.Get_Messages_Container (Kernel),
         GPS.Kernel.Messages.Listener_Access (Module.Listener),
         GPS.Kernel.Messages.Empty_Message_Flags);

      Module.Review_Command :=
        new Commands.CodePeer.Review_Message_Command'
          (Root_Command with Module);
      --  This command is shared for all CodePeer messages.

      Module.Check_CodePeer_Version;

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

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   overriding function Tooltip_Handler
     (Module  : access Module_Id_Record;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      Widget : Gtk_Label;
      Values : BT.Vn_Values_Seqs.Vector;
      Text   : Unbounded_String;

   begin
      if not (Has_File_Information (Context)
        and then Has_Line_Information (Context)
        and then Has_Column_Information (Context)
        and then Module.Display_Values
        and then Module.Tree /= null
        and then Module.Has_Backtraces)
      then
         return null;
      end if;

      declare
         File     : constant Virtual_File := File_Information (Context);
         Buffer   : constant GPS.Editors.Editor_Buffer'Class :=
           Module.Kernel.Get_Buffer_Factory.Get
             (File        => File,
              Force       => False,
              Open_Buffer => False,
              Open_View   => False);
         Location : GPS.Editors.Editor_Location'Class :=
           Buffer.New_Location
             (Line_Information (Context), Column_Information (Context));

      begin
         --  CodePeer associates values with position close to start of
         --  identifier. It can be:
         --   - position of first character of direct name
         --   - position of dot character in expanded name
         --   - position of apostrophe character in attribute reference

         while Values.Is_Empty loop
            Values :=
              BT.Xml.Reader.Get_Srcpos_Vn_Values
                (String (File.Full_Name.all),
                 (Location.Line, Natural (Location.Column)));

            exit when not String_Utils.Is_Entity_Letter
              (Wide_Wide_Character'Val (Location.Get_Char))
                 and Wide_Wide_Character'Val (Location.Get_Char) /= '''
                 and Wide_Wide_Character'Val (Location.Get_Char) /= '.';

            Location := Location.Forward_Char (-1);
         end loop;

         --  CodePeer associates values of indexed component at position after
         --  open bracket character.

         Location := Location.Forward_Word (1);

         if Values.Is_Empty
           and then Wide_Wide_Character'Val (Location.Get_Char) = '('
         then
            Location := Location.Forward_Char (1);
            Values :=
              BT.Xml.Reader.Get_Srcpos_Vn_Values
                (String (File.Full_Name.all),
                 (Location.Line, Natural (Location.Column)));
         end if;
      end;

      if Values.Is_Empty then
         return null;
      end if;

      for Item of Values loop
         if Length (Text) /= 0 then
            Append (Text, Ada.Characters.Latin_1.LF);
         end if;

         Append (Text, Item.Vn_Image);
         Append (Text, ": ");
         Append (Text, Item.Set_Image);
      end loop;

      Gtk_New (Widget, To_String (Text));

      return Gtk.Widget.Gtk_Widget (Widget);
   end Tooltip_Handler;

end CodePeer.Module;

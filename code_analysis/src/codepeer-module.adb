------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

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
with GPS.Default_Styles;             use GPS.Default_Styles;
with GPS.Intl;                       use GPS.Intl;
with GPS.Kernel.Contexts;            use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Hyperlink;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules.UI;          use GPS.Kernel.Modules.UI;
with GPS.Location_View;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Xref;

with BT.Xml.Reader;
with Build_Command_Utils; use Build_Command_Utils;
with Build_Configurations; use Build_Configurations;
with CodePeer.Bridge.Annotations_Readers;
with CodePeer.Bridge.Audit_Trail_Readers;
with CodePeer.Bridge.Inspection_Readers;
with CodePeer.Bridge.Status_Readers;
with CodePeer.Message_Review_Dialogs;
with CodePeer.Messages_Reports; use CodePeer.Messages_Reports;
with CodePeer.Module.Actions;
with CodePeer.Module.Bridge;
with CodePeer.Module.Commands;
with CodePeer.Module.Editors;
with CodePeer.Multiple_Message_Review_Dialogs;
with CodePeer.Shell_Commands; use CodePeer.Shell_Commands;
with CodePeer.Single_Message_Review_Dialogs;
with Commands; use Commands;
with Code_Analysis_GUI;
with String_Utils; use String_Utils;

package body CodePeer.Module is

   use type Code_Analysis.Code_Analysis_Tree;
   use GNATCOLL.Projects;

   Me : constant Trace_Handle := Create ("GPS.CODEPEER.MODULE");
   CodePeer_Subdir : constant Filesystem_String := "codepeer";

   type Module_Context is record
      Module  : CodePeer_Module_Id;
      Project : Code_Analysis.Project_Access;
      File    : Code_Analysis.File_Access;
   end record;

   package Context_CB is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Module_Context);

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

   type On_Before_Exit is new Return_Boolean_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Before_Exit;
      Kernel : not null access Kernel_Handle_Record'Class)
      return Boolean;
   --  Called before GPS exits. Switchs perspective to default.

   type Message_Filter is
     new GPS.Kernel.Messages.Abstract_Message_Filter with null record;
   overriding function Apply
     (Self    : in out Message_Filter;
      Message : GPS.Kernel.Messages.Abstract_Message'Class)
      return GPS.Kernel.Messages.Filter_Result;

   Output_Directory_Attribute   :
     constant Attribute_Pkg_String := Build ("CodePeer", "Output_Directory");
   Database_Directory_Attribute : constant Attribute_Pkg_String :=
       Build ("CodePeer", "Database_Directory");
   Server_URL_Attribute : constant Attribute_Pkg_String :=
       Build ("CodePeer", "Server_URL");
   Message_Patterns_Attribute : constant Attribute_Pkg_String :=
       Build ("CodePeer", "Message_Patterns");
   Additional_Patterns_Attribute : constant Attribute_Pkg_String :=
       Build ("CodePeer", "Additional_Patterns");
   CWE_Attribute : constant Attribute_Pkg_String :=
     Build ("CodePeer", "CWE");
   Switches_Attribute : constant Attribute_Pkg_List :=
     Build ("CodePeer", "Switches");
   Pending_Status_Attribute : constant Attribute_Pkg_List :=
     Build ("CodePeer", "Pending_Status");
   Not_A_Bug_Status_Attribute : constant Attribute_Pkg_List :=
     Build ("CodePeer", "Not_A_Bug_Status");
   Bug_Status_Attribute : constant Attribute_Pkg_List :=
     Build ("CodePeer", "Bug_Status");

   Race_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (Editor_Side => True, Locations => True, Editor_Line => False);

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (Self    : in out Message_Filter;
      Message : GPS.Kernel.Messages.Abstract_Message'Class)
      return GPS.Kernel.Messages.Filter_Result
   is
      pragma Unreferenced (Self);

      function Is_Visible (Message : CodePeer.Message'Class) return Boolean;
      --  Compute visibility of the message with current filter criteria

      function Flags
        (Message : CodePeer.Message'Class)
         return GPS.Kernel.Messages.Message_Flags;
      --  Return set of flags depending from lifeage of the
      --  message. "Removed" messages are displayed only in
      --  locations view, others displayed in both locations view
      --  end editor.

      -----------
      -- Flags --
      -----------

      function Flags
        (Message : CodePeer.Message'Class)
         return GPS.Kernel.Messages.Message_Flags is
      begin
         if Message.Lifeage = Removed then
            return (Editor_Side => False, Locations => True,
                    Editor_Line => False);

         else
            return (Editor_Side => True, Locations => True,
                    Editor_Line => False);
         end if;
      end Flags;

      ----------------
      -- Is_Visible --
      ----------------

      function Is_Visible
        (Message : CodePeer.Message'Class) return Boolean is
      begin
         --  Simple criteria

         if not Module.Filter_Criteria.Lineages (Message.Lifeage)
           or not Module.Filter_Criteria.Rankings (Message.Ranking)
           or not Module.Filter_Criteria.Statuses (Message.Status.Id)
         then
            return False;
         end if;

         --  Category of the message should be selected

         if Module.Filter_Criteria.Categories.Contains (Message.Category) then
            return True;
         end if;

         --  or at least one check of the message should be selected

         if not Module.Filter_Criteria.Categories.Intersection
           (Message.Checks).Is_Empty
         then
            return True;
         end if;

         --  or at least one CWE of the message or message's category
         --  should be selected

         if not Module.Filter_Criteria.CWEs.Intersection
           ((if not Message.CWEs.Is_Empty
            then Message.CWEs
            else Message.Category.CWEs)).Is_Empty
         then
            return True;
         end if;

         --  otherwise it is not visible

         return False;
      end Is_Visible;

   begin
      if Message not in CodePeer.Message'Class then
         return (Non_Applicable => True);
      end if;

      if Is_Visible (CodePeer.Message'Class (Message)) then
         return
           (Non_Applicable => False,
            Flags          => Flags (CodePeer.Message'Class (Message)));

      else
         return (Non_Applicable => False, Flags => (others => False));
      end if;
   end Apply;

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

   -----------------------
   -- Set_Review_Action --
   -----------------------

   procedure Set_Review_Action (Message : Message_Access) is
      use Code_Analysis_GUI;
   begin
      Module.Review_Command.Ref;
      Message.Set_Action
        (new GPS.Editors.Line_Information.Line_Information_Record'
           (Text               => Null_Unbounded_String,
            Tooltip_Text       => To_Unbounded_String
              (if Message.Status.Category = Uncategorized
               then "Manual review"
               else Image (Message.Status) & ASCII.LF &
                 "Update manual review"),
            Image              => To_Unbounded_String
              (case Message.Status.Category is
               when Uncategorized => Grey_Analysis_Cst,
               when Pending       => Purple_Analysis_Cst,
               when Bug           => Red_Analysis_Cst,
               when Not_A_Bug     => Blue_Analysis_Cst),
            Message            =>
              Create (GPS.Kernel.Messages.Message_Access (Message)),
            Associated_Command => Module.Review_Command));
   end Set_Review_Action;

   -----------------------------
   -- Create_CodePeer_Message --
   -----------------------------

   function Create_CodePeer_Message
     (Id               : Natural;
      File             : Code_Analysis.File_Access;
      Subprogram       : Ada.Strings.Unbounded.Unbounded_String;
      Merged           : Natural_Sets.Set;
      Lifeage          : Lifeage_Kinds;
      Line             : Positive;
      Column           : Positive;
      Category         : Message_Category_Access;
      Is_Check         : Boolean;
      Ranking          : Message_Ranking_Level;
      Text             : String;
      From_File        : GNATCOLL.VFS.Virtual_File;
      From_Line        : Positive;
      From_Column      : Positive;
      Checks           : Message_Category_Sets.Set;
      CWEs             : CWE_Category_Sets.Set)
      return Message_Access
   is
      Project : constant Project_Type :=
                  GPS.Kernel.Project.Get_Project (Module.Kernel);
      Message : constant Message_Access := new CodePeer.Message'
        (GPS.Kernel.Messages.Primary_Abstract_Message with
         Id              => Id,
         File            => File,
         Subprogram      => Subprogram,
         Merged          => Merged,
         Lifeage         => Lifeage,
         Category        => Category,
         Is_Check        => Is_Check,
         Ranking         => Ranking,
         Status          => Uncategorized_Status,
         Status_Editable => True,
         Text            => To_Unbounded_String (Text),
         Audit_Loaded    => False,
         Audit           => <>,
         Checks          => Checks,
         CWEs            => CWEs,
         Display_CWEs    =>
           Project.Has_Attribute (CWE_Attribute)
             and then Ada.Characters.Handling.To_Lower
               (Project.Attribute_Value (CWE_Attribute)) = "true",
         Removed_Color   => Module.Removed_Message_Color,
         Show_Msg_Id     => Module.Show_Msg_Id.Get_Pref);
      Style   : constant Style_Access := Module.Message_Styles (Ranking);

      function Get_Message_Importance_From_Ranking
        (Ranking : Message_Ranking_Level)
         return Message_Importance_Type;
      --  Used to map CodePeer messages ranking with the global GPS one
      --  ??? We should remove this at some point

      -----------------------------------------
      -- Get_Message_Importance_From_Ranking --
      -----------------------------------------

      function Get_Message_Importance_From_Ranking
        (Ranking : Message_Ranking_Level)
         return Message_Importance_Type is
      begin
         case Ranking is
            when Not_An_Error .. Info =>
               return Informational;
            when Low =>
               return Low;
            when Medium =>
               return Medium;
            when High =>
               return High;
         end case;
      end Get_Message_Importance_From_Ranking;

   begin
      GPS.Kernel.Messages.Initialize
        (Self          => Message,
         Container     => Get_Messages_Container (Module.Kernel),
         Category      => CodePeer_Category_Name,
         File          => File.Name,
         Line          => Line,
         Column        => Basic_Types.Visible_Column_Type (Column),
         Importance    => Get_Message_Importance_From_Ranking (Ranking),
         Actual_Line   => Line,
         Actual_Column => Column);

      if Style /= null
        and then Message.Lifeage /= Removed
      then
         Message.Set_Highlighting (Style);
      end if;

      Set_Review_Action (Message);

      if From_File /= No_File then
         declare
            Text : constant String :=
                     "(see also "
                     & String (From_File.Full_Name.all)
                     & ":"
                     & Ada.Strings.Fixed.Trim
                     (Positive'Image (From_Line),
                      Ada.Strings.Both)
                     & ":"
                     & Ada.Strings.Fixed.Trim
                     (Positive'Image (From_Column),
                      Ada.Strings.Both)
                     & ")";

         begin
            GPS.Kernel.Messages.Hyperlink.Create_Hyperlink_Message
              (GPS.Kernel.Messages.Message_Access (Message),
               From_File,
               From_Line,
               Basic_Types.Visible_Column_Type (From_Column),
               Text,
               Text'First + 10,
               Text'Last - 1,
               (Editor_Side => False,
                Editor_Line => False,
                Locations   => True));
         end;
      end if;

      return Message;
   end Create_CodePeer_Message;

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
                    To_String (Object.Name) & " race condition",
                    Unspecified,
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
                              "read by "
                              & To_String (Entry_Point.Entry_Point.Name),
                           when Update =>
                              "update by "
                              & To_String (Entry_Point.Entry_Point.Name)),
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
     (Module       : not null access Module_Id_Record'Class;
      Force        : Boolean;
      Build_Target : String)
   is
      Project  : constant Project_Type := Get_Project (Module.Kernel);
      Switches : String_List_Access;
      Builder  : constant Builder_Context := Builder_Context
        (Module.Kernel.Module (Builder_Context_Record'Tag));

      Ensure_Build_Mode : CodePeer_Build_Mode (Kernel_Handle (Module.Kernel));
      pragma Unreferenced (Ensure_Build_Mode);

   begin
      if Project.Has_Attribute (Switches_Attribute) then
         Switches := Project.Attribute_Value (Switches_Attribute);
         Set_Project_Switches
           (Get_Target_From_Name (Builder.Registry, Build_Target),
            To_String (Switches.all));
         Free (Switches);
      end if;

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

         begin
            if not File_Node.Subprograms.Is_Empty then
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
   -- Codepeer_Message_Patterns --
   -------------------------------

   function Codepeer_Message_Patterns
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File is
   begin
      if Project.Has_Attribute (Message_Patterns_Attribute) then
         declare
            File : constant GNATCOLL.VFS.Filesystem_String :=
              GNATCOLL.VFS.Filesystem_String
                (Project.Attribute_Value (Message_Patterns_Attribute));

         begin
            return
              GNATCOLL.VFS.Create_From_Base
                (File, Project.Project_Path.Dir.Full_Name.all);
         end;

      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Codepeer_Message_Patterns;

   -------------------------
   -- Codepeer_Server_URL --
   -------------------------

   function Codepeer_Server_URL
     (Project : Project_Type) return String is
   begin
      if Project.Has_Attribute (Server_URL_Attribute) then
         return Project.Attribute_Value (Server_URL_Attribute);

      else
         return "";
      end if;
   end Codepeer_Server_URL;

   ----------------------------------
   -- Codepeer_Additional_Patterns --
   ----------------------------------

   function Codepeer_Additional_Patterns
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File is
   begin
      if Project.Has_Attribute (Additional_Patterns_Attribute) then
         declare
            File : constant GNATCOLL.VFS.Filesystem_String :=
              GNATCOLL.VFS.Filesystem_String
                (Project.Attribute_Value (Additional_Patterns_Attribute));

         begin
            return
              GNATCOLL.VFS.Create_From_Base
                (File, Project.Project_Path.Dir.Full_Name.all);
         end;

      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Codepeer_Additional_Patterns;

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
--           Message : constant CodePeer.Message_Access :=
--             CodePeer.Message_Vectors.Element (Position);

      begin
         null;
--           if Message.Message /= null then
--              Message.Message.Set_Flags ((others => False));
--           end if;
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
               Self.Annotation_Categories,
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

         Editors.Show_Annotations_In_Opened_Editors (Self.all);
         Self.Fill_Object_Races;
         Self.Update_Location_View;
         GPS.Location_View.Raise_Locations_Window
           (Self.Kernel, Create_If_Needed => True);

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

   ----------------------
   -- Load_Annotations --
   ----------------------

   procedure Load_Annotations
     (Self : access Module_Id_Record'Class;
      File : in out Code_Analysis.File'Class)
   is
      Input  : Input_Sources.File.File_Input;
      Reader : CodePeer.Bridge.Annotations_Readers.Reader;
      Data   : CodePeer.File_Data'Class renames
        CodePeer.File_Data'Class (File.Analysis_Data.CodePeer_Data.all);

   begin
      if Data.Annotations_File.Is_Regular_File then

         --  Load inspection information

         Data.Annotations_Loaded := True;
         Input_Sources.File.Open (+Data.Annotations_File.Full_Name, Input);
         Reader.Parse (Self.Version, Input, Self.Annotation_Categories, File);
         Input_Sources.File.Close (Input);

      else
         Self.Kernel.Insert
           (Data.Annotations_File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);
      end if;
   end Load_Annotations;

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
            CodePeer.Module.Bridge.Inspection (Module, True);

         when Audit_Trail =>
            Module.Review_Messages
              (Module.Bridge_Messages, Module.Inspection_File);

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
         Editors.Hide_Annotations (Context.Module.all, File);
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
      pragma Unreferenced (Self, Kernel);

   begin
      --  Destroy report window

      Module.Report_Subwindow.Destroy;

      return True;
   end Execute;

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
      Messages : constant CodePeer.Message_Vectors.Vector :=
        CodePeer.Message_Review_Dialogs.Message_Review_Dialog_Record'Class
           (Item.all).Get_Messages;

   begin
      CodePeer.Module.Bridge.Add_Audit_Record (Context.Module, Messages);
      Context.Module.Report.Messages_Report.Update;
      Context.Module.Update_Location_View;

      --  Update review icon and tooltip for reviewed messages

      for Message of Messages loop
         Set_Review_Action (Message);
      end loop;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Message_Reviewed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      procedure Add_Statuses
        (Attribute : Attribute_Pkg_List; Category : Audit_Status_Category);
      --  Add the statuses defined by this Attribute, in the given Category.

      ------------------
      -- Add_Statuses --
      ------------------

      procedure Add_Statuses
        (Attribute : Attribute_Pkg_List; Category : Audit_Status_Category)
      is
         Project  : constant Project_Type := Get_Project (Kernel);
         Statuses : String_List_Access;
      begin
         if Project.Has_Attribute (Attribute) then
            Statuses := Project.Attribute_Value (Attribute);

            for Status of Statuses.all loop
               Add_Audit_Status (Status.all, Category);
            end loop;

            Free (Statuses);
         end if;
      end Add_Statuses;

   begin
      Module.Listener.Set_Cleanup_Mode (True);

      --  Remove CodePeer report window if still around to avoid keeping
      --  reference to an old project data structure.

      if Module.Report_Subwindow /= null then
         Module.Report_Subwindow.Destroy;
         Module.Report_Subwindow := null;
      end if;

      --  Remove all messages of all categories starting from
      --  Codepeer_Category_Prefix to be sure that all possible categories are
      --  removed.

      Remove_Codepeer_Messages (Kernel);

      --  Clear backtraces data cache.

      BT.Xml.Reader.Clear;

      Module.Listener.Set_Cleanup_Mode (False);

      --  Reset audit statuses and register predefined ones

      Audit_Statuses.Clear;
      Add_Audit_Status ("Uncategorized", Uncategorized);
      Add_Audit_Status ("Pending", Pending);
      Add_Audit_Status ("Not a bug", Not_A_Bug);
      Add_Audit_Status ("False positive", Not_A_Bug);
      Add_Audit_Status ("Intentional", Not_A_Bug);
      Add_Audit_Status ("Bug", Bug);

      --  Then register the project specific ones, if any

      Add_Statuses (Pending_Status_Attribute, Pending);
      Add_Statuses (Not_A_Bug_Status_Attribute, Not_A_Bug);
      Add_Statuses (Bug_Status_Attribute, Bug);
   end Execute;

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

   ------------------------------
   -- Remove_Codepeer_Messages --
   ------------------------------

   procedure Remove_Codepeer_Messages
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Container : constant GPS.Kernel.Messages_Container_Access :=
                    Kernel.Get_Messages_Container;

   begin
      Container.Remove_Category (CodePeer_Category_Name, Empty_Message_Flags);
   end Remove_Codepeer_Messages;

   ---------------------
   -- Review_Messages --
   ---------------------

   procedure Review_Messages
     (Self     : access Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector;
      File     : Virtual_File)
   is
      Input  : Input_Sources.File.File_Input;
      Reader : CodePeer.Bridge.Audit_Trail_Readers.Reader;

   begin
      if File.Is_Regular_File then

         --  Load inspection information

         Input_Sources.File.Open (+File.Full_Name, Input);
         Reader.Parse (Input, Self.Messages);
         Input_Sources.File.Close (Input);
         Module.Review_Messages (Messages, Need_Reload => False);

      else
         Self.Kernel.Insert
           (File.Display_Full_Name &
            (-" does not exist. Please perform a full analysis first"),
            Mode => GPS.Kernel.Error);
      end if;
   end Review_Messages;

   ---------------------
   -- Review_Messages --
   ---------------------

   procedure Review_Messages
     (Self        : access Module_Id_Record'Class;
      Messages    : CodePeer.Message_Vectors.Vector;
      Need_Reload : Boolean)
   is
      use type Ada.Containers.Count_Type;

      Single_Review   :
        CodePeer.Single_Message_Review_Dialogs.Message_Review_Dialog;
      Multiple_Review :
        CodePeer.Multiple_Message_Review_Dialogs.Message_Review_Dialog;
      Loaded          : Boolean := not Need_Reload;

   begin
      --  Check that all messages have loaded audit trail.
      --  In client/server mode, always reload since another user might have
      --  posted a manual analysis under another session.

      if Need_Reload
        and then Codepeer_Server_URL (Get_Project (Module.Kernel)) = ""
      then
         for Message of Messages loop
            Loaded := Message.Audit_Loaded;
            exit when not Loaded;
         end loop;
      end if;

      if not Loaded then
         CodePeer.Module.Bridge.Load_Audit_Trail
           (CodePeer_Module_Id (Self), Messages);
      else
         --  Create and show review dialog

         if Messages.Length = 1 then
            CodePeer.Single_Message_Review_Dialogs.Gtk_New
              (Single_Review, Self.Kernel, Messages.First_Element);
            Single_Review.Set_Transient_For (Self.Kernel.Get_Main_Window);
            Single_Review.Show_All;
            Context_CB.Connect
              (Single_Review,
               CodePeer.Message_Review_Dialogs.Signal_Ok_Activated,
               Context_CB.To_Marshaller (On_Message_Reviewed'Access),
               (CodePeer_Module_Id (Self), null, null));

         else
            CodePeer.Multiple_Message_Review_Dialogs.Gtk_New
              (Multiple_Review, Self.Kernel, Messages);
            Multiple_Review.Set_Transient_For (Self.Kernel.Get_Main_Window);
            Multiple_Review.Show_All;
            Context_CB.Connect
              (Multiple_Review,
               CodePeer.Message_Review_Dialogs.Signal_Ok_Activated,
               Context_CB.To_Marshaller (On_Message_Reviewed'Access),
               (CodePeer_Module_Id (Self), null, null));
         end if;
      end if;
   end Review_Messages;

   --------------------------
   -- Update_Location_View --
   --------------------------

   procedure Update_Location_View (Self : access Module_Id_Record'Class) is
      Data : CodePeer.Project_Data'Class
        renames CodePeer.Project_Data'Class
          (Self.Tree.Element
             (GPS.Kernel.Project.Get_Project
                (Self.Kernel)).Analysis_Data.CodePeer_Data.all);

   begin
      Get_Messages_Container (Self.Kernel).Set_Sort_Order_Hint
        (CodePeer_Category_Name, Alphabetical);

      --  Activate refiltering of content of Locations view

      Self.Filter.Criteria_Changed;

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

      --  Reuse existing preferences regarding Analysis Tools for low, medium
      --  and high priority messages.

      Module.Message_Colors (CodePeer.High) := High_Messages_Highlight;
      Module.Message_Colors (CodePeer.Medium) := Medium_Messages_Highlight;
      Module.Message_Colors (CodePeer.Low) := Low_Messages_Highlight;
      Module.Message_Colors (CodePeer.Info) := Info_Messages_Highlight;

      --  Suppressed is no longer used in recent versions, so reuse Info

      Module.Message_Colors (CodePeer.Suppressed) := Info_Messages_Highlight;

      --  Create a preference for importing annotations and backtraces

      Module.Import_Annotations :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Import-Annotations",
           Label   => -"Import CodePeer annotations",
           Path    => -"CodePeer:General",
           Doc     => -("Import and display CodePeer annotations in source"
             & " editor"),
           Default => True);

      Module.Import_Backtraces :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Import-Backtraces",
           Label   => -"Import CodePeer backtraces",
           Path    => -"CodePeer:General",
           Doc     => -("Import and display CodePeer backtraces"),
           Default => True);

      Module.Show_Msg_Id :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "CodePeer-Show-Msg-Id",
           Label   => -"Show CodePeer Message IDs",
           Path    => -"CodePeer:General",
           Doc     => -("Show message IDs in Locations view"),
           Default => False);

      --  Create CodePeer own preferences for CodePeer specific messages

      Module.Removed_Message_Color :=
        Default_Preferences.Create
          (Kernel.Get_Preferences,
           Name    => "Messages-Removed-Foreground",
           Label   => -"Color for 'removed' messages",
           Path    => -"CodePeer:Colors",
           Doc     => -("Color to use for the foreground of removed messages"
             & " in the Locations view."),
           Default => "#5A5A5A");

      --  CodePeer styles initialization

      Module.Annotations_Style := Editor_Code_Annotations_Style;

      Module.Message_Styles (CodePeer.High) := Messages_Styles (High);
      Module.Message_Styles (CodePeer.Medium) := Messages_Styles (Medium);
      Module.Message_Styles (CodePeer.Low) := Messages_Styles (Low);

      Initialize_Style
        (Module.Message_Styles (CodePeer.Info),
         Informational_Probability_Style_Name,
         Module.Message_Colors (CodePeer.Info),
         True);

      Compilation_Finished_Hook.Add (new On_Compilation_Finished);
      Project_Changed_Hook.Add (new On_Project_Changed);

      Module.Listener := new CodePeer.Listeners.Listener;
      GPS.Kernel.Messages.Register_Listener
        (Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Module.Listener),
         GPS.Kernel.Messages.Empty_Message_Flags);

      Module.Review_Command :=
        new CodePeer.Module.Commands.Review_Message_Command'
          (Root_Command with Module);
      --  This command is shared for all CodePeer messages.

      Module.Filter := new Message_Filter;
      Kernel.Get_Messages_Container.Register_Filter (Module.Filter);

      Editors.Register_Module (Kernel);
   end Register_Module;

   -------------------
   -- Show_Messages --
   -------------------

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
         Location : constant GPS.Editors.Editor_Location'Class :=
           Buffer.New_Location
             (Line_Information (Context), Column_Information (Context));

      begin
         Values :=
           BT.Xml.Reader.Get_Srcpos_Vn_Values
             (String (File.Full_Name.all),
              (Location.Line, Natural (Location.Column)));
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

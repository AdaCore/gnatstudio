------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNAT.Regexp;
with GNAT.Strings;

with Basic_Types;
with Entities;
with Input_Sources.File;
with Glib.Object;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;       use Gtk.Separator_Menu_Item;
with Gtk.Widget;
with Gtkada.MDI;
with GPS.Editors.Line_Information;
with GPS.Intl;
with GPS.Kernel.Console;
with GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;
with GPS.Kernel.Project;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Standard_Hooks;
with GNATCOLL.Projects;
with Traces;

with GNATStack.CI_Editors;
with GNATStack.CI_Utilities;
with GNATStack.Readers;
with GNATStack.Module.Editors;
with GNATStack.Shell_Commands;

package body GNATStack.Module is

   use Ada.Strings.Unbounded;
   use GPS.Editors.Line_Information;
   use GPS.Intl;
   use GPS.Kernel.Modules.UI;
   use GNATStack.Readers;
   use GNATStack.Data_Model;
   use GNATStack.Data_Model.Object_Information_Vectors;
   use GNATStack.Data_Model.Subprogram_Information_Sets;
   use GNATStack.Data_Model.Subprogram_Information_Vectors;
   use GNATStack.Data_Model.Subprogram_Location_Sets;
   use Traces;
   use type GPS.Kernel.MDI.GPS_MDI_Child;

   Stack_Analysis_Name : constant String := "Stack Analysis";
   Stack_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (GPS.Kernel.Messages.Editor_Side => False,
      GPS.Kernel.Messages.Locations   => True);

   type GNATStack_Submenu_Factory_Record
     (Module : access GNATStack_Module_Id_Record'Class) is
     new GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;

   package Object_Callbacks is
     new Gtk.Handlers.Callback (Glib.Object.GObject_Record);

   package Object_Module_Callbacks is
     new Gtk.Handlers.User_Callback
       (Glib.Object.GObject_Record, GNATStack_Module_Id);

   overriding procedure Append_To_Menu
     (Self    : access GNATStack_Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure On_Analyze_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Analyze stack usage" menu item is activated

   procedure On_Open_CIs_Editor
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Open undefined subprograms editor" menu item is activated

   procedure On_Load_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Load data" menu item is activated

   procedure On_Clear_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Called when "Clear data" menu item is activated

   procedure On_Show_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Shows stack usage information in the editor for selected file.

   procedure On_Hide_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Hides stack usage information in the editor for selected file.

   procedure On_Open_Call_Tree
     (Widget : access Glib.Object.GObject_Record'Class);
   --  Opens call tree for the selected subprogram.

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "compilation_finished" hook, to schedule other tasks

   procedure On_CIs_Editor_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id);
   --  Handle close of CIs editor.

   procedure On_Call_Tree_View_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id);
   --  Handle close of call tree viewer.

   procedure On_Goto_Subprogram
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id);
   --  Handles double click in call tree view.

   procedure Load_Data
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Loads analysis data.

   procedure Open_Report
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Opens stack usage analysis report.

   procedure Open_CI_Editor
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Opens CI editor for unknown subprograms.

   procedure Open_Call_Tree_View
     (Self       : not null access GNATStack_Module_Id_Record'Class;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access);
   --  Opens call tree view to display subprogram.

   procedure Fill_Entry_Points
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Fills locations view by the list of entry points.

   procedure Save_CIs
     (Self : not null access GNATStack_Module_Id_Record'Class);
   --  Saves CIs information.

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Self    : access GNATStack_Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      function Lookup
        (Name   : Ada.Strings.Unbounded.Unbounded_String;
         File   : Ada.Strings.Unbounded.Unbounded_String;
         Line   : Positive;
         Column : Positive)
         return GNATStack.Data_Model.Subprogram_Information_Access;

      ------------
      -- Lookup --
      ------------

      function Lookup
        (Name   : Ada.Strings.Unbounded.Unbounded_String;
         File   : Ada.Strings.Unbounded.Unbounded_String;
         Line   : Positive;
         Column : Positive)
         return GNATStack.Data_Model.Subprogram_Information_Access
      is
         pragma Unreferenced (Name, Column);
         --  GNATStack and GPS entities uses different representation of names
         --  and column positions, ignore them now and return first
         --  approximation.

         Subprogram_Position : Subprogram_Information_Sets.Cursor :=
           Module.Data.Subprogram_Set.First;
         Location_Position   : Subprogram_Location_Sets.Cursor;
         Subprogram          : Subprogram_Information_Access;
         Location            : Subprogram_Location;

      begin
         while Has_Element (Subprogram_Position) loop
            Subprogram := Element (Subprogram_Position);
            Location_Position := Subprogram.Identifier.Locations.First;

            while Has_Element (Location_Position) loop
               Location := Element (Location_Position);

               if Location.Name /= ""
                 and Location.File = File
                 and Location.Line = Line
               then
                  return Subprogram;
               end if;

               Next (Location_Position);
            end loop;

            Next (Subprogram_Position);
         end loop;

         return null;
      end Lookup;

      Item          : Gtk.Menu_Item.Gtk_Menu_Item;
      File_Location : Entities.File_Location;

   begin
      if not Self.Module.Loaded then
         return;
      end if;

      if GPS.Kernel.Contexts.Has_File_Information (Context) then
         Module.File := GPS.Kernel.Contexts.File_Information (Context);
         Gtk.Menu_Item.Gtk_New (Item, -"Show stack usage");
         Menu.Append (Item);
         Object_Callbacks.Connect
           (Item,
            Gtk.Menu_Item.Signal_Activate,
            Object_Callbacks.To_Marshaller (On_Show_Stack_Usage'Access));
         Gtk.Menu_Item.Gtk_New (Item, -"Hide stack usage");
         Menu.Append (Item);
         Object_Callbacks.Connect
           (Item,
            Gtk.Menu_Item.Signal_Activate,
            Object_Callbacks.To_Marshaller (On_Hide_Stack_Usage'Access));
      end if;

      if GPS.Kernel.Contexts.Has_Entity_Name_Information (Context)
        and GPS.Kernel.Contexts.Has_Entity_Column_Information (Context)
      then
         File_Location :=
           Entities.Get_Declaration_Of
             (GPS.Kernel.Contexts.Get_Entity (Context));

         Module.Subprogram :=
           Lookup
             (Ada.Strings.Unbounded.To_Unbounded_String
                (GPS.Kernel.Contexts.Entity_Name_Information (Context)),
              Ada.Strings.Unbounded.To_Unbounded_String
                (String
                   (Entities.Get_Filename (File_Location.File).Full_Name.all)),
              File_Location.Line,
              Positive (File_Location.Column));

         if Module.Subprogram /= null then
            Gtk.Menu_Item.Gtk_New
              (Item,
               (-"Call tree for ")
               & Ada.Strings.Unbounded.To_String
                 (Module.Subprogram.Identifier.Prefix_Name));
            Menu.Append (Item);
            Object_Callbacks.Connect
              (Item,
               Gtk.Menu_Item.Signal_Activate,
               Object_Callbacks.To_Marshaller (On_Open_Call_Tree'Access));
         end if;
      end if;
   end Append_To_Menu;

   --------------------
   -- Fill_Locations --
   --------------------

   procedure Fill_Entry_Points
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor);
      --  Adds messages into messages container for entry point and call
      --  chain.

      -------------------------
      -- Process_Entry_Point --
      -------------------------

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Entry_Subprogram : constant Subprogram_Information_Access :=
                              Element (Position);
         Entry_Message    : GPS.Kernel.Messages.Simple.Simple_Message_Access;

         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor);
         --  Adds secondary message for the subprogram at the specified
         --  position.

         ------------------------
         -- Process_Subprogram --
         ------------------------

         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor)
         is
            Subprogram : constant Subprogram_Information_Access :=
                           Element (Position);

         begin
            if Subprogram.Identifier.Locations.Is_Empty then
               GPS.Kernel.Messages.Simple.Create_Simple_Message
                 (GPS.Kernel.Messages.Message_Access (Entry_Message),
                  GNATCOLL.VFS.No_File,
                  0,
                  0,
                  To_String (Subprogram.Identifier.Prefix_Name)
                    & " : " & Image (Subprogram.Local_Usage),
                  Stack_Message_Flags);

            else
               GPS.Kernel.Messages.Simple.Create_Simple_Message
                 (GPS.Kernel.Messages.Message_Access (Entry_Message),
                  GNATCOLL.VFS.Create
                   (GNATCOLL.VFS.Filesystem_String
                      (To_String
                         (Element
                            (Subprogram.Identifier.Locations.First).File))),
                  Element (Subprogram.Identifier.Locations.First).Line,
                  Basic_Types.Visible_Column_Type
                    (Element
                       (Subprogram.Identifier.Locations.First).Column),
                  To_String (Subprogram.Identifier.Prefix_Name)
                    & " : " & Image (Subprogram.Local_Usage),
                  Stack_Message_Flags);
            end if;
         end Process_Subprogram;

      begin
         if Entry_Subprogram.Identifier.Locations.Is_Empty then
            Entry_Message :=
              GPS.Kernel.Messages.Simple.Create_Simple_Message
                (GPS.Kernel.Messages.Get_Messages_Container (Self.Kernel),
                 Stack_Analysis_Name,
                 GNATCOLL.VFS.No_File,
                 0,
                 0,
                 To_String (Entry_Subprogram.Identifier.Prefix_Name)
                   & " : total " & Image (Entry_Subprogram.Entry_Usage),
                 0,
                 Stack_Message_Flags);

         else
            Entry_Message :=
              GPS.Kernel.Messages.Simple.Create_Simple_Message
                (GPS.Kernel.Messages.Get_Messages_Container (Self.Kernel),
                 Stack_Analysis_Name,
                 GNATCOLL.VFS.Create
                   (GNATCOLL.VFS.Filesystem_String
                      (To_String
                         (Element
                            (Entry_Subprogram.Identifier.Locations.First)
                               .File))),
                 Element (Entry_Subprogram.Identifier.Locations.First).Line,
                 Basic_Types.Visible_Column_Type
                   (Element
                      (Entry_Subprogram.Identifier.Locations.First).Column),
                 To_String (Entry_Subprogram.Identifier.Prefix_Name)
                   & " : total " & Image (Entry_Subprogram.Entry_Usage),
                 0,
                 Stack_Message_Flags);
         end if;

         Entry_Subprogram.Chain.Iterate (Process_Subprogram'Access);
      end Process_Entry_Point;

   begin
      GPS.Kernel.Messages.Get_Messages_Container (Self.Kernel).Remove_Category
        (Stack_Analysis_Name, Stack_Message_Flags);

      Self.Data.Entry_Set.Iterate (Process_Entry_Point'Access);
   end Fill_Entry_Points;

   ---------------
   -- Load_Data --
   ---------------

   procedure Load_Data
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is
      use type GNAT.Strings.String_List_Access;

      Project     : constant GNATCOLL.Projects.Project_Type :=
                      GPS.Kernel.Project.Get_Project (Self.Kernel);
      Project_Dir : constant GNATCOLL.VFS.Virtual_File :=
                      GNATCOLL.VFS.Create
                        (GNATCOLL.Projects.Project_Path (Project).Dir_Name);
      XML_Name    : constant GNATCOLL.VFS.Virtual_File :=
                      GNATCOLL.VFS.Create_From_Dir
                        (Project_Dir, "stack_usage.xml");
      CI_Pattern  : constant GNAT.Regexp.Regexp :=
                      GNAT.Regexp.Compile ("*.ci", True);
      CI_Name     : GNATCOLL.VFS.Virtual_File;
      File        : Input_Sources.File.File_Input;
      Reader      : GNATStack.Readers.Reader;
      Switches    : GNAT.Strings.String_List_Access :=
                      GNATCOLL.Projects.Attribute_Value
                        (Project, GNATCOLL.Projects.Stack_Switches_Attribute);

   begin
      if Self.Loaded then
         if Self.CI_Editor_MDI /= null then
            Self.CI_Editor_MDI.Destroy;
            Self.CI_Editor_MDI := null;
         end if;

         GPS.Kernel.Messages.Get_Messages_Container
           (Self.Kernel).Remove_Category
           (Stack_Analysis_Name, Stack_Message_Flags);
         Editors.Hide_Stack_Usage_In_Opened_Editors (Self);
         Clear (Self.Data);
         Self.Loaded := False;
      end if;

      if XML_Name.Is_Regular_File then
         --  Load analysis data.

         Input_Sources.File.Open (String (XML_Name.Full_Name.all), File);
         Reader.Parse (File);
         Input_Sources.File.Close (File);
         Module.Data := Reader.Get_Data;

         --  Load undefined subprograms information.

         if Switches /= null then
            for J in Switches'Range loop
               if GNAT.Regexp.Match (Switches (J).all, CI_Pattern) then
                  CI_Name :=
                    GNATCOLL.VFS.Create_From_Dir
                      (Project_Dir,
                       GNATCOLL.VFS.Filesystem_String (Switches (J).all));

                  if CI_Name.Is_Regular_File then
                     GNATStack.CI_Utilities.Merge
                       (Module.Data, String (CI_Name.Full_Name.all));

                  else
                     --  File not found, create empty record in data for this
                     --  file.

                     Module.Data.CIs.Append
                       (new GNATStack.Data_Model.CI_Information'
                          (Ada.Strings.Unbounded.To_Unbounded_String
                             (String
                                (GNATCOLL.VFS.Create_From_Dir
                                   (Project_Dir,
                                    GNATCOLL.VFS.Filesystem_String
                                      (Switches (J).all)).Full_Name.all)),
                           Subprograms => <>));
                  end if;
               end if;
            end loop;

            GNAT.Strings.Free (Switches);
         end if;

         Module.Loaded := True;

      else
         GPS.Kernel.Console.Insert
           (Self.Kernel,
            "stack_usage.xml not found. Run GNATStack first.",
            True,
            GPS.Kernel.Console.Error);
      end if;
   end Load_Data;

   ----------------------------
   -- On_Analyze_Stack_Usage --
   ----------------------------

   procedure On_Analyze_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      GNATStack.Shell_Commands.Build_Target_Execute
        (Kernel,
         GNATStack.Shell_Commands.Build_Target (Kernel, "Run GNATStack"),
         Synchronous => False);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Analyze_Stack_Usage;

   -----------------------------
   -- On_Call_Tree_View_Close --
   -----------------------------

   procedure On_Call_Tree_View_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id)
   is
      pragma Unreferenced (Object);

   begin
      Self.Call_Tree_View_MDI := null;
   end On_Call_Tree_View_Close;

   -------------------------
   -- On_CIs_Editor_Close --
   -------------------------

   procedure On_CIs_Editor_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id)
   is
      pragma Unreferenced (Object);

   begin
      Save_CIs (Self);
      Self.CI_Editor_MDI := null;
   end On_CIs_Editor_Close;

   -------------------
   -- On_Clear_Data --
   -------------------

   procedure On_Clear_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);

   begin
      GPS.Kernel.Messages.Get_Messages_Container
        (Module.Kernel).Remove_Category
        (Stack_Analysis_Name, Stack_Message_Flags);
      Editors.Hide_Stack_Usage_In_Opened_Editors (Module);

      if Module.Call_Tree_View_MDI /= null then
         Module.Call_Tree_View_MDI.Destroy;
      end if;

      if Module.CI_Editor_MDI /= null then
         Module.CI_Editor_MDI.Destroy;
      end if;

      Clear (Module.Data);
      Module.Loaded := False;
   end On_Clear_Data;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);

      Hook_Data : constant
        GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args :=
          GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args (Data.all);

   begin
      if Hook_Data.Status = 0
        and Hook_Data.Target_Name = "Run GNATStack"
      then
         Load_Data (Module);
         Open_Report (Module);
         Editors.Show_Stack_Usage_In_Opened_Editors (Module);
         Fill_Entry_Points (Module);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end On_Compilation_Finished;

   ------------------------
   -- On_Goto_Subprogram --
   ------------------------

   procedure On_Goto_Subprogram
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : GNATStack_Module_Id)
   is
      pragma Unreferenced (Object);

      Subprogram : constant Data_Model.Subprogram_Information_Access :=
                     Self.Call_Tree_View.Get_Selected_Subprogram;

   begin
      if not Subprogram.Identifier.Locations.Is_Empty then
         declare
            Location : constant Data_Model.Subprogram_Location :=
                         GNATStack.Data_Model.Subprogram_Location_Sets.Element
                           (Subprogram.Identifier.Locations.First);
            File     : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Create
                           (GNATCOLL.VFS.Filesystem_String
                              (To_String (Location.File)));
            Buffer   : constant GPS.Editors.Editor_Buffer'Class :=
                         Module.Kernel.Get_Buffer_Factory.Get (File);
            Position : constant GPS.Editors.Editor_Location'Class :=
                         Buffer.New_Location
                           (Location.Line,
                            Basic_Types.Visible_Column_Type (Location.Column));

         begin
            Buffer.Current_View.Cursor_Goto (Position, True);
         end;
      end if;
   end On_Goto_Subprogram;

   -------------------------
   -- On_Hide_Stack_Usage --
   -------------------------

   procedure On_Hide_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Widget);

   begin
      Editors.Hide_Stack_Usage (Module, Module.File);
   end On_Hide_Stack_Usage;

   ------------------
   -- On_Load_Data --
   ------------------

   procedure On_Load_Data
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Load_Data (Module);
      Open_Report (Module);
      Editors.Show_Stack_Usage_In_Opened_Editors (Module);
      Fill_Entry_Points (Module);

   exception
      when E : others =>
         GPS.Kernel.Console.Insert
           (Kernel,
            "Unable to load stack usage information.",
            True,
            GPS.Kernel.Console.Error);
         Trace (Exception_Handle, E);
   end On_Load_Data;

   -----------------------
   -- On_Open_Call_Tree --
   -----------------------

   procedure On_Open_Call_Tree
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Widget);

   begin
      Open_Call_Tree_View (Module, Module.Subprogram);
   end On_Open_Call_Tree;

   ------------------------
   -- On_Open_CIs_Editor --
   ------------------------

   procedure On_Open_CIs_Editor
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);

   begin
      Open_CI_Editor (Module);
   end On_Open_CIs_Editor;

   -------------------------
   -- On_Show_Stack_Usage --
   -------------------------

   procedure On_Show_Stack_Usage
     (Widget : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Widget);

   begin
      Editors.Show_Stack_Usage (Module, Module.File);
   end On_Show_Stack_Usage;

   -------------------------
   -- Open_Call_Tree_View --
   -------------------------

   procedure Open_Call_Tree_View
     (Self       : not null access GNATStack_Module_Id_Record'Class;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access)
   is
   begin
      if not Self.Loaded then
         GPS.Kernel.Console.Insert
           (Self.Kernel,
            "No stack usage information loaded.",
            True,
            GPS.Kernel.Console.Error);

      else
         if Self.Call_Tree_View_MDI /= null then
            Self.Call_Tree_View_MDI.Destroy;
         end if;

         GNATStack.Call_Tree_Views.Gtk_New (Self.Call_Tree_View, Subprogram);
         Object_Module_Callbacks.Connect
           (Self.Call_Tree_View,
            GNATStack.Call_Tree_Views.Signal_Double_Clicked,
            Object_Module_Callbacks.To_Marshaller (On_Goto_Subprogram'Access),
            GNATStack_Module_Id (Self));
         GPS.Kernel.MDI.Gtk_New
           (Self.Call_Tree_View_MDI, Self.Call_Tree_View, Module => Self);
         Self.Call_Tree_View_MDI.Set_Title (-"GNATStack: Call Tree");
         Object_Module_Callbacks.Connect
           (Self.Call_Tree_View_MDI,
            Gtk.Widget.Signal_Destroy,
            Object_Module_Callbacks.To_Marshaller
              (On_Call_Tree_View_Close'Access),
            GNATStack_Module_Id (Self));
         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put
           (Self.Call_Tree_View_MDI, Gtkada.MDI.Position_Left);
         Self.Call_Tree_View_MDI.Raise_Child;
      end if;
   end Open_Call_Tree_View;

   --------------------
   -- Open_CI_Editor --
   --------------------

   procedure Open_CI_Editor
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is
      CI_Editor : GNATStack.CI_Editors.CI_Editor;

   begin
      if not Self.Loaded then
         GPS.Kernel.Console.Insert
           (Self.Kernel,
            "No stack usage information loaded.",
            True,
            GPS.Kernel.Console.Error);

      elsif Self.Data.CIs.Is_Empty then
         GPS.Kernel.Console.Insert
           (Self.Kernel,
            "User's CI files not listed in project file.",
            True,
            GPS.Kernel.Console.Error);

      elsif Self.CI_Editor_MDI = null then
         GNATStack.CI_Editors.Gtk_New (CI_Editor, Self.Data'Access);
         GPS.Kernel.MDI.Gtk_New
           (Self.CI_Editor_MDI, CI_Editor, Module => Self);
         Self.CI_Editor_MDI.Set_Title (-"GNATStack: External subprograms");
         Object_Module_Callbacks.Connect
           (Self.CI_Editor_MDI,
            Gtk.Widget.Signal_Destroy,
            Object_Module_Callbacks.To_Marshaller (On_CIs_Editor_Close'Access),
            GNATStack_Module_Id (Self));
         GPS.Kernel.MDI.Get_MDI (Self.Kernel).Put (Self.CI_Editor_MDI);
         Self.CI_Editor_MDI.Raise_Child;
      end if;
   end Open_CI_Editor;

   -----------------
   -- Open_Report --
   -----------------

   procedure Open_Report
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Indirect_Call_Information_Vectors;
      use Subprogram_Information_Ordered_Sets;
      use Subprogram_Information_Vector_Vectors;

      Buffer : constant GPS_Editor_Buffer'Class :=
                 GPS_Editor_Buffer'Class
                   (Self.Kernel.Get_Buffer_Factory.Get_New);

      procedure Add_Line (Item : String := "");
      --  Adds special line at the beginning of the buffer.

      procedure Process_Cycle
        (Position : Subprogram_Information_Vector_Vectors.Cursor);
      --  Outputs cycle's information.

      procedure Process_Unbounded_Objects
        (Position : Subprogram_Information_Sets.Cursor);
      --  Outputs list of unbounded objects in the subprogram.

      procedure Process_External_Subprogram
        (Position : Subprogram_Information_Ordered_Sets.Cursor);
      --  Outputs external subprogram's information.

      procedure Process_Indirect_Calls
        (Position : Subprogram_Information_Sets.Cursor);
      --  Output list of indirect calls in the subprogram.

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor);
      --  Output entry point information.

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (Item : String := "") is
      begin
         Add_Special_Line (Buffer, 1, Item);
      end Add_Line;

      -------------------
      -- Process_Cycle --
      -------------------

      procedure Process_Cycle
        (Position : Subprogram_Information_Vector_Vectors.Cursor)
      is
         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor);

         procedure Process_Subprogram
           (Position : Subprogram_Information_Vectors.Cursor) is
         begin
            Add_Line (To_String (Element (Position).Identifier.Prefix_Name));
         end Process_Subprogram;

      begin
         Add_Special_Line (Buffer, 1, "");

         Element (Position).Iterate (Process_Subprogram'Access);
      end Process_Cycle;

      -------------------------
      -- Process_Entry_Point --
      -------------------------

      procedure Process_Entry_Point
        (Position : Subprogram_Information_Sets.Cursor) is
      begin
         Add_Line
           ("  " & To_String (Element (Position).Identifier.Prefix_Name));
      end Process_Entry_Point;

      ----------------------------
      -- Process_Indirect_Calls --
      ----------------------------

      procedure Process_Indirect_Calls
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram : constant Subprogram_Information_Access :=
                        Element (Position);

         procedure Process_Indirect_Call
           (Position : Indirect_Call_Information_Vectors.Cursor);

         ---------------------------
         -- Process_Indirect_Call --
         ---------------------------

         procedure Process_Indirect_Call
           (Position : Indirect_Call_Information_Vectors.Cursor)
         is
            Info : constant Indirect_Call_Information  :=
                     Element (Position);

         begin
            Add_Line
              ("    at "
               & To_String (Info.File)
               & ":"
               & Trim (Integer'Image (Info.Line), Both));
         end Process_Indirect_Call;

      begin
         Add_Line;
         Add_Line
           (" "
            & Integer'Image (Integer (Subprogram.Indirects.Length))
            & " indirect call in "
            & To_String (Subprogram.Identifier.Prefix_Name));
         Subprogram.Indirects.Iterate (Process_Indirect_Call'Access);
      end Process_Indirect_Calls;

      ---------------------------------
      -- Process_External_Subprogram --
      ---------------------------------

      procedure Process_External_Subprogram
        (Position : Subprogram_Information_Ordered_Sets.Cursor)
      is
      begin
         Add_Line
           ("  " & To_String (Element (Position).Identifier.Prefix_Name));
      end Process_External_Subprogram;

      -------------------------------
      -- Process_Unbounded_Objects --
      -------------------------------

      procedure Process_Unbounded_Objects
        (Position : Subprogram_Information_Sets.Cursor)
      is
         Subprogram : constant Subprogram_Information_Access :=
                        Element (Position);

         procedure Process_Object
           (Position : Object_Information_Vectors.Cursor);
         --  Outputs name and location of the object.

         --------------------
         -- Process_Object --
         --------------------

         procedure Process_Object
           (Position : Object_Information_Vectors.Cursor)
         is
            Object : constant Object_Information := Element (Position);

         begin
            Add_Line
              ("    "
               & To_String (Object.Name)
               & " at "
               & To_String (Object.File)
               & ":"
               & Trim (Integer'Image (Object.Line), Both)
               & ":"
               & Trim (Integer'Image (Object.Column), Both));
         end Process_Object;

      begin
         Add_Line;
         Add_Line
           ("  In " & To_String (Subprogram.Identifier.Prefix_Name));
         Subprogram.Unbounded.Iterate (Process_Object'Access);
      end Process_Unbounded_Objects;

   begin
      Buffer.Set_Read_Only (True);

      if not Self.Data.Unbounded_Set.Is_Empty
        or not Self.Data.External_Set.Is_Empty
        or not Self.Data.Indirect_Set.Is_Empty
        or not Self.Data.Cycle_Set.Is_Empty
      then
         Add_Line ("Worst case analysis is *not* accurate because of");

         if not Self.Data.Cycle_Set.Is_Empty then
            Add_Line (" - cycles");
         end if;

         if not Self.Data.Unbounded_Set.Is_Empty then
            Add_Line (" - unbounded frames");
         end if;

         if not Self.Data.External_Set.Is_Empty then
            Add_Line (" - external calls");
         end if;

         if not Self.Data.Indirect_Set.Is_Empty then
            Add_Line (" - indirect calls");
         end if;
      end if;

      if not Self.Data.Cycle_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of reachable cycles:");
         Self.Data.Cycle_Set.Iterate (Process_Cycle'Access);
      end if;

      if not Self.Data.Unbounded_Set.Is_Empty then
         Add_Line;
         Add_Line
           ("List of reachable subprograms with dynamic unbounded frames");
         Self.Data.Unbounded_Set.Iterate (Process_Unbounded_Objects'Access);
      end if;

      if not Self.Data.External_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of reachable external subprograms:");
         Self.Data.External_Set.Iterate (Process_External_Subprogram'Access);
      end if;

      if not Self.Data.Indirect_Set.Is_Empty then
         Add_Line;
         Add_Line
           ("List of reachable and unresolved indirect"
            & " (including dispatching) calls:");
         Self.Data.Indirect_Set.Iterate (Process_Indirect_Calls'Access);
      end if;

      if not Self.Data.Entry_Set.Is_Empty then
         Add_Line;
         Add_Line ("List of entry points:");
         Self.Data.Entry_Set.Iterate (Process_Entry_Point'Access);
      end if;
   end Open_Report;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use GNATCOLL.VFS;

      Menu           : constant String := -"/Tools/Stac_k Analysis";
      GNATStack_Path : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Locate_On_Path ("gnatstack");
      Factory        : GPS.Kernel.Modules.UI.Submenu_Factory;
      Sep            : Gtk_Separator_Menu_Item;

   begin
      if GNATStack_Path = No_File then
         --  There is no GNATStack executable available, module is not
         --  registered.

         return;
      end if;

      Module := new GNATStack_Module_Id_Record (Kernel);

      --  Registry contextual submenu factory

      Factory := new GNATStack_Submenu_Factory_Record (Module);

      Module.Register_Module (Kernel, Stack_Analysis_Name);
      Register_Contextual_Submenu
        (Kernel  => Kernel,
         Name    => Stack_Analysis_Name,
         Label   => -Stack_Analysis_Name,
         Submenu => Factory);

      --  Registry main menu

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Ref_Item    => -"Macro",
         Text        => -"_Analyze stack usage",
         Callback    => On_Analyze_Stack_Usage'Access);

      Gtk_New (Sep);
      Register_Menu (Kernel, Menu, Sep);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Open undefined subprograms editor",
         Callback    => On_Open_CIs_Editor'Access);

      Gtk_New (Sep);
      Register_Menu (Kernel, Menu, Sep);

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Load last stack usage",
         Callback    => On_Load_Data'Access);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => Menu,
         Text        => -"_Clear stack usage information",
         Callback    => On_Clear_Data'Access);

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Compilation_Finished_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Compilation_Finished'Access),
         Name => "gnatstack.compilation_finished");

      Editors.Register_Module (Module);
   end Register_Module;

   --------------
   -- Save_CIs --
   --------------

   procedure Save_CIs
     (Self : not null access GNATStack_Module_Id_Record'Class)
   is

      procedure Save (Position : Data_Model.CI_Vectors.Cursor);
      --  Save information for specified CI file.

      ----------
      -- Save --
      ----------

      procedure Save (Position : Data_Model.CI_Vectors.Cursor) is
         CI : constant Data_Model.CI_Information_Access :=
                Data_Model.CI_Vectors.Element (Position);

      begin
         GNATStack.CI_Utilities.Write
           (Ada.Strings.Unbounded.To_String (CI.File_Name), CI.Subprograms);
      end Save;

   begin
      Self.Data.CIs.Iterate (Save'Access);
   end Save_CIs;

end GNATStack.Module;

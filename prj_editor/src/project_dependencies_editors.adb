------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;              use GNATCOLL.VFS.GtkAda;

with Glib;                             use Glib;
with Glib.Object;
with Glib.Values;
with Glib_Values_Utils;                use Glib_Values_Utils;

with Gdk.Drag_Contexts;                use Gdk.Drag_Contexts;
with Gdk.Types;                        use Gdk.Types;
with Gtk.Button;                       use Gtk.Button;
with Gtk.Cell_Layout;                  use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;                use Gtk.Cell_Renderer;
with Gtk.Dnd;                          use Gtk.Dnd;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Paned;                        use Gtk.Paned;
with Gtk.Scrolled_Window;              use Gtk.Scrolled_Window;
with Gtk.Selection_Data;               use Gtk.Selection_Data;
with Gtk.Tree_View_Column;             use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;               use Gtk.Tree_Selection;
with Gtk.Tree_Model;                   use Gtk.Tree_Model;
with Gtk.Tree_Store;                   use Gtk.Tree_Store;
with Gtk.Widget;                       use Gtk.Widget;
with Gtk.Window;                       use Gtk.Window;
with Gtkada.Dialogs;                   use Gtkada.Dialogs;
with Gtkada.File_Selector;             use Gtkada.File_Selector;

with Dialog_Utils;                     use Dialog_Utils;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Project;               use GPS.Kernel.Project;
with GPS.Intl;                         use GPS.Intl;
with GUI_Utils;                        use GUI_Utils;
with Projects;                         use Projects;

package body Project_Dependencies_Editors is

   package File_Sets is
     new Ada.Containers.Hashed_Sets (Virtual_File, Full_Name_Hash, "=");

   Cst_Project_Name : aliased String := "Project Name";
   Cst_Limited      : aliased String := "Limited with";

   --  Constants for the dependency editor
   Is_Limited_Column         : constant := 0;
   Project_Info_Column       : constant := 1;
   Full_Path_Column          : constant := 2;
   Can_Change_Limited_Column : constant := 3;
   Use_Base_Name_Column      : constant := 4;

   function Dependency_Column_Types return GType_Array;

   --  Constants for the "Add from known dialog"
   Project_Info_Column2     : constant := 0;
   Full_Path_Column2        : constant := 1;

   function Add_Column_Types return GType_Array;

   procedure Remove_Project
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Add_New_Project
     (Self : access Glib.Object.GObject_Record'Class);
   --  Add or remove a new project dependency

   procedure Add_Predefined_Projects
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type;
      Model   : access Gtk_Tree_Store_Record'Class);
   --  Add the predefined projects in the tree

   procedure Add_Imported_Projects
     (Project : Project_Type;
      Model   : access Gtk_Tree_Store_Record'Class);
   --  Add all projects imported by Project to the tree

   function Get_Project_Info_Label
     (Project_Name : String;
      Full_Path    : String) return String;
   --  Return a label displaying the project's name and its directory to
   --  display in the projects tree views.

   procedure Add_Single_Project
     (Project                   : Project_Type;
      Imported                  : Project_Type;
      Model                     : access Gtk_Tree_Store_Record'Class;
      Ignore_If_Imported        : Boolean := False;
      Column_Project_Info       : Gint;
      Column_Full_Path          : Gint;
      Column_Is_Limited         : Gint := -1;
      Column_Can_Change_Limited : Gint := -1);
   --  Add a single project to the tree.
   --  The tree contains all projects relative to Project
   --  If Ignore_If_Imported is true, then projects already imported by
   --  Project will not be listed

   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : Virtual_File;
      Limited_With          : Boolean;
      Use_Base_Name         : Boolean);
   --  Internal function that creates a dependency between two projects. It
   --  properly handles the case where a project with the same name as
   --  Imported_Project_Path already exists in the project hierarchy.

   procedure Set_Columns
     (Model         : Gtk_Tree_Store;
      Iter          : Gtk_Tree_Iter;
      Project_Info  : String;
      Is_Limited    : Boolean;
      Can_Change    : Boolean;
      Full_Path     : Virtual_File;
      Use_Base_Name : Boolean);
   --  Set model's values

   -------------------------
   -- Drag'N'Drop Support --
   -------------------------

   Drag_N_Drop_Data_Sep : constant Character := '|';
   --  Used to separate the different data fields we want to pass from
   --  one tree to another (Project name and its full path).

   procedure On_Drag_Data_Get
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);
   procedure On_Drag_Data_Received
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Gint;
      Y       : Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);
   --  Support for drag'n'drop between the 'Known Projects' left pane tree view
   --  and the 'Dependencies' one.

   ----------------------
   -- On_Drag_Data_Get --
   ----------------------

   procedure On_Drag_Data_Get
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint)
   is
      pragma Unreferenced (Context, Info, Time);
      Tree         : constant Gtk_Tree_View := Gtk_Tree_View (Self);
      Iter         : Gtk_Tree_Iter;
      Model        : Gtk_Tree_Model;
      Selection    : constant Gtk_Tree_Selection := Tree.Get_Selection;

   begin
      Get_Selected
        (Selection => Selection,
         Model     => Model,
         Iter      => Iter);

      declare
         Project_Info : constant String :=
                          Get_String (Model, Iter, Project_Info_Column2);
         Project_File : constant Virtual_File :=
                          Get_File (Model, Iter, Full_Path_Column2);
         Text_Data    : constant String := Project_Info
                          & Drag_N_Drop_Data_Sep
                          & Project_File.Display_Full_Name
                          & Drag_N_Drop_Data_Sep
                          & Get_String_From_Iter (Model, Iter);
         Success      : Boolean with Unreferenced;
      begin
         Success := Data.Set_Text (Str => Text_Data, Len => Text_Data'Length);
      end;
   end On_Drag_Data_Get;

   ---------------------------
   -- On_Drag_Data_Received --
   ---------------------------

   procedure On_Drag_Data_Received
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Gint;
      Y       : Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint)
   is
      pragma Unreferenced (Context, X, Y, Info, Time);
      Editor    : constant Project_Dependencies_Editor :=
                    Project_Dependencies_Editor (Self);
      Model     : Gtk_Tree_Store := -(Editor.Dependencies_Tree.Get_Model);
      Iter      : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Null_Iter);

      declare
         Text_Data    : constant Unbounded_String_Array :=
                          Split (Data.Get_Data_As_String,
                                 On => Drag_N_Drop_Data_Sep);
         Project_Info : constant String :=
                          To_String (Text_Data (Text_Data'First));
         Full_Path    : constant String :=
                          To_String (Text_Data (Text_Data'First + 1));
         Iter_Path    : constant String :=
                          To_String (Text_Data (Text_Data'First + 2));
         Project_File : constant Virtual_File := Create (+Full_Path);
      begin
         Set_Columns
           (Model, Iter,
            Project_Info  => Project_Info,
            Is_Limited    => False,
            Can_Change    => True,
            Full_Path     => Project_File,
            Use_Base_Name => True);

         Editor.Dependencies_Tree.Scroll_To_Cell
           (Path      => Get_Path (Model, Iter),
            Column    => null,
            Use_Align => False,
            Row_Align => 0.0,
            Col_Align => 0.0);
         Editor.Dependencies_Tree.Get_Selection.Select_Iter (Iter);

         --  Remove the newly dropped row of the 'Known Projects' tree view

         Model := -(Editor.Known_Projects_Tree.Get_Model);
         Iter := Get_Iter_From_String (Model, Iter_Path);
         Model.Remove (Iter);
      end;
   end On_Drag_Data_Received;

   -----------------------------
   -- Add_Dependency_Internal --
   -----------------------------

   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : Virtual_File;
      Limited_With          : Boolean;
      Use_Base_Name         : Boolean)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Kernel.Insert
           (S & ASCII.LF,
            Mode   => GPS.Kernel.Error,
            Add_LF => False);
         Parse_File_Locations (Kernel, S, -"Project add dependency");
      end Report_Error;

      Use_Relative_Path : constant Boolean :=
                            Get_Paths_Type
                              (Importing_Project) = Projects.Relative
                                or else
                                  (Get_Paths_Type
                                     (Importing_Project) = From_Pref
                                   and then Generate_Relative_Paths.Get_Pref);
      Changed           : Import_Project_Error;
      Result            : Message_Dialog_Buttons;
      Must_Recompute    : Boolean := False;

   begin
      Changed := Get_Registry (Kernel).Tree.Add_Imported_Project
        (Project                   => Importing_Project,
         Imported_Project_Location => Imported_Project_Path,
         Errors                    => Report_Error'Unrestricted_Access,
         Use_Base_Name             => Use_Base_Name,
         Use_Relative_Path         => Use_Relative_Path,
         Limited_With              => Limited_With);
      Get_Registry (Kernel).Tree.Recompute_View;

      if Changed = Project_Already_Exists then
         --  If there is already a project by that name in the tree,
         --  confirm whether we should rename it everywhere

         Result := Message_Dialog
           (Msg         => -("A project with this name already exists in the"
            & ASCII.LF
            & "project graph. Do you want to replace all"
            & ASCII.LF
            & "occurences with the new project, or"
            & ASCII.LF
            & "cancel the new dependency ?"),
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_OK or Button_Cancel,
            Title       => -"Project already exists",
            Parent      => Get_Current_Window (Kernel));

         if Result = Button_OK then

            --  Replace all occurrences of the old project with the new one

            declare
               Tree            : constant Project_Tree_Access :=
                                   Get_Registry (Kernel).Tree;
               Imported        : constant Project_Type :=
                                   Tree.Project_From_Name
                                     (+Imported_Project_Path.Base_Name
                                        (Project_File_Extension));
               Iter            : Project_Iterator :=
                                   Get_Registry
                                     (Kernel).Tree.Root_Project.Start;
               Prj             : Project_Type;
               Is_Limited_With : Boolean;
               Imports         : Boolean;
            begin
               loop
                  Prj := Current (Iter);
                  exit when Prj = No_Project;

                  Prj.Project_Imports
                    (Imported,
                     Imports          => Imports,
                     Is_Limited_With  => Is_Limited_With);

                  if Imports then
                     Prj.Remove_Imported_Project (Imported);
                     Tree.Recompute_View;

                     Changed := Tree.Add_Imported_Project
                       (Project                   => Prj,
                        Imported_Project_Location => Imported_Project_Path,
                        Errors                    =>
                          Report_Error'Unrestricted_Access,
                        Use_Base_Name             => Use_Base_Name,
                        Use_Relative_Path         => Use_Relative_Path,
                        Limited_With              => Is_Limited_With);
                     Tree.Recompute_View;
                  end if;

                  Next (Iter);
               end loop;

               Must_Recompute := True;
            end;
         end if;
      end if;

      if Changed = Success or else Must_Recompute then
         Recompute_View (Kernel);
      end if;
   end Add_Dependency_Internal;

   -----------------------------
   -- Add_Predefined_Projects --
   -----------------------------

   procedure Add_Predefined_Projects
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type;
      Model   : access Gtk_Tree_Store_Record'Class)
   is
      Iter             : Gtk_Tree_Iter;
      Project_Path     : constant File_Array :=
        Get_Registry (Kernel).Environment.Predefined_Project_Path;
      Found            : Boolean;
      Files            : File_Array_Access;
      Imported_Prj     : Project_Type;
      Imported         : Project_Iterator;
      Visited          : File_Sets.Set;

   begin
      for J in Project_Path'Range loop
         Found := Visited.Contains (Project_Path (J));

         if not Found then
            Visited.Include (Project_Path (J));
            begin
               Files := Project_Path (J).Read_Dir (Files_Only);

               for K in Files'Range loop
                  declare
                     Base : constant Filesystem_String :=
                       Files (K).Base_Name (".gpr");
                  begin
                     if Has_Suffix (Files (K), ".gpr") then
                        Imported_Prj := Get_Registry (Kernel).Tree
                          .Project_From_Name (+Base);

                        --  If the project is already in the tree, do not
                        --  duplicate its entry.
                        if Imported_Prj = No_Project
                          or else Project_Directory (Imported_Prj) /=
                          Project_Path (J)
                        then
                           Append (Model, Iter, Null_Iter);

                           Set_All_And_Clear
                             (Gtk_Tree_Store (Model),
                              Iter,
                              (Project_Info_Column2 => As_String
                                   (Get_Project_Info_Label
                                      (Project_Name => +Base,
                                       Full_Path    =>
                                         Files (K).Display_Full_Name)),
                               Full_Path_Column2    => As_File (Files (K))));
                        end if;
                     end if;
                  end;
               end loop;

               Unchecked_Free (Files);

            exception
               when VFS_Directory_Error =>
                  null;
            end;
         end if;
      end loop;

      Imported := Start (Root_Project => Get_Project (Kernel));
      while Current (Imported) /= No_Project loop
         Add_Single_Project
           (Project              => Project,
            Imported             => Current (Imported),
            Model                => Model,
            Ignore_If_Imported   => True,
            Column_Project_Info  => Project_Info_Column2,
            Column_Full_Path     => Full_Path_Column2);
         Next (Imported);
      end loop;
   end Add_Predefined_Projects;

   ---------------------------
   -- Add_Imported_Projects --
   ---------------------------

   procedure Add_Imported_Projects
     (Project : Project_Type;
      Model   : access Gtk_Tree_Store_Record'Class)
   is
      Imported : Project_Iterator;
   begin
      Imported := Start (Root_Project => Project, Direct_Only => True);
      while Current (Imported) /= No_Project loop
         Add_Single_Project
           (Project                   => Project,
            Imported                  => Current (Imported),
            Model                     => Model,
            Column_Project_Info       => Project_Info_Column,
            Column_Is_Limited         => Is_Limited_Column,
            Column_Can_Change_Limited => Can_Change_Limited_Column,
            Column_Full_Path          => Full_Path_Column);
         Next (Imported);
      end loop;
   end Add_Imported_Projects;

   -------------------
   -- Get_Row_Label --
   -------------------

   function Get_Project_Info_Label
     (Project_Name : String;
      Full_Path    : String) return String
   is
      (Project_Name & ASCII.LF
       & "<span foreground=""#8c8c8c"" size=""x-small"">"
       & Full_Path & "</span>");

   ------------------------
   -- Add_Single_Project --
   ------------------------

   procedure Add_Single_Project
     (Project                   : Project_Type;
      Imported                  : Project_Type;
      Model                     : access Gtk_Tree_Store_Record'Class;
      Ignore_If_Imported        : Boolean := False;
      Column_Project_Info       : Gint;
      Column_Full_Path          : Gint;
      Column_Is_Limited         : Gint := -1;
      Column_Can_Change_Limited : Gint := -1)
   is
      Iter            : Gtk_Tree_Iter;
      Is_Imported     : Boolean;
      Is_Limited      : Boolean;
      Must_Be_Limited : Boolean := False;
      Imported_Iter   : Project_Iterator;

      Values  : Glib.Values.GValue_Array (1 .. 6);
      Columns : Columns_Array (Values'Range);
      Last    : Gint;

   begin
      if Imported /= Project then
         Project_Imports
           (Parent          => Project,
            Child           => Imported,
            Imports         => Is_Imported,
            Is_Limited_With => Is_Limited);

         if not Is_Imported
           or else not Ignore_If_Imported
         then
            Imported_Iter := Start (Root_Project => Imported);
            while Current (Imported_Iter) /= No_Project loop
               if Current (Imported_Iter) = Project then
                  Must_Be_Limited := True;
                  exit;
               end if;
               Next (Imported_Iter);
            end loop;

            Append (Model, Iter, Null_Iter);

            Columns (1 .. 2) := (Column_Project_Info, Column_Full_Path);
            Values (1 .. 2) :=
              (1 => As_String
                 (Get_Project_Info_Label
                      (Project_Name => Imported.Name,
                       Full_Path    =>
                         Imported.Project_Path.Display_Full_Name)),
               2 => As_File    (Project_Path (Imported)));
            Last := 2;

            if Column_Is_Limited /= -1 then
               Last := Last + 1;
               Columns (Last) := Column_Is_Limited;
               Glib.Values.Init_Set_Boolean
                 (Values (Last), Is_Limited or else Must_Be_Limited);
            end if;

            if Column_Can_Change_Limited /= -1 then
               Last := Last + 1;
               Columns (Last) := Column_Can_Change_Limited;
               Glib.Values.Init_Set_Boolean
                 (Values (Last), not Must_Be_Limited);
            end if;

            Set_And_Clear
              (Gtk_Tree_Store (Model), Iter,
               Columns (1 .. Last), Values (1 .. Last));
         end if;
      end if;
   end Add_Single_Project;

   -----------------------------
   -- Dependency_Column_Types --
   -----------------------------

   function Dependency_Column_Types return GType_Array is
   begin
      return (Is_Limited_Column         => GType_Boolean,
              Project_Info_Column       => GType_String,
              Full_Path_Column          => Get_Virtual_File_Type,
              Can_Change_Limited_Column => GType_Boolean,
              Use_Base_Name_Column      => GType_Boolean);
   end Dependency_Column_Types;

   ----------------------
   -- Add_Column_Types --
   ----------------------

   function Add_Column_Types return GType_Array is
   begin
      return (Project_Info_Column2 => GType_String,
              Full_Path_Column2    => Get_Virtual_File_Type);
   end Add_Column_Types;

   ---------------------
   -- Add_New_Project --
   ---------------------

   procedure Add_New_Project
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor : constant Project_Dependencies_Editor :=
                Project_Dependencies_Editor (Self);
      Model  : constant Gtk_Tree_Store :=
                 -Get_Model (Editor.Dependencies_Tree);
      Name   : constant Virtual_File := Select_File
        (-"Select Project",
         Get_Current_Dir,
         File_Pattern      => "*.gpr",
         Pattern_Name      => "Project files",
         Parent            =>
           Gtk_Window (Get_Toplevel (Editor.Dependencies_Tree)),
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Kind              => Open_File,
         History           => Get_History (Editor.Kernel));
      Iter  : Gtk_Tree_Iter;

   begin
      if Name /= GNATCOLL.VFS.No_File then
         Append (Model, Iter, Null_Iter);

         Set_Columns
           (Model, Iter,
            Project_Info  => Get_Project_Info_Label
              (Project_Name => Name.Display_Base_Name (".gpr"),
               Full_Path    => Name.Display_Full_Name),
            Is_Limited    => False,
            Can_Change    => True,
            Full_Path     => Name,
            Use_Base_Name => False);

         Editor.Dependencies_Tree.Scroll_To_Cell
           (Path      => Get_Path (Model, Iter),
            Column    => null,
            Use_Align => False,
            Row_Align => 0.0,
            Col_Align => 0.0);
         Editor.Dependencies_Tree.Get_Selection.Select_Iter (Iter);
      end if;
   end Add_New_Project;

   --------------------
   -- Remove_Project --
   --------------------

   procedure Remove_Project
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor    : constant Project_Dependencies_Editor :=
                    Project_Dependencies_Editor (Self);
      Selection : constant Gtk_Tree_Selection :=
                    Get_Selection (Editor.Dependencies_Tree);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);

      if Iter /= Null_Iter then
         Remove (-Model, Iter);
      end if;
   end Remove_Project;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Project_Dependencies_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      Left_View           : Dialog_View;
      Dependencies_View   : Dialog_View_With_Button_Box;
      Known_Projects_View : Dialog_View;
      Group_Widget        : Dialog_Group_Widget;
      Pane                : Gtk_Paned;
      Model               : Gtk_Tree_Store;
      Label               : Gtk_Label;
      Button              : Gtk_Button;
      Dummy               : Gtk_Widget;
      Scrolled            : Gtk_Scrolled_Window;
      List                : Cell_Renderer_List.Glist;
   begin
      Dialog_Utils.Initialize (Self);

      Self.Kernel := Kernel_Handle (Kernel);
      Self.Project := Project;

      --  Create the 'Description' group widget and add its description label

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View => Self,
         Group_Name  => "Description");

      Gtk_New (Label);
      Label.Set_Markup
        (-"Sources in a project can have references to files in other"
         & " projects. Such a relation is represented as a project "
         & "dependency. "
         & ASCII.LF
         & "You can drag'n'drop a project from the "
         & "<b>Known Projects</b> panel to the <b>Dependencies</b> one to "
         & "create a new project dependency. "
         & ASCII.LF
         & "You can also click on the <b>+</b> button to add "
         & "any other project.");
      Label.Set_Line_Wrap (True);
      Label.Set_Alignment (0.0, 0.5);
      Group_Widget.Create_Child (Label);

      Gtk_New_Hpaned (Pane);
      Self.Append (Pane, Add_Separator => False);

      Left_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Left_View);
      Pane.Pack1 (Left_View);

      --  Create the 'Dependencies' left pane

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Left_View,
         Group_Name          => "Dependencies",
         Allow_Multi_Columns => False);

      Dependencies_View := new Dialog_View_With_Button_Box_Record;
      Initialize (Dependencies_View, Position => Pos_Left);
      Group_Widget.Append_Child
        (Dependencies_View,
         Expand => True,
         Fill   => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Self.Dependencies_Tree := Create_Tree_View
        (Column_Types      => Dependency_Column_Types,
         Column_Names      =>
           (1 + Is_Limited_Column   => Cst_Limited'Unchecked_Access,
            1 + Project_Info_Column => Cst_Project_Name'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 1 + Project_Info_Column,
         Selection_Mode     => Gtk.Enums.Selection_Single);
      Add (Scrolled, Self.Dependencies_Tree);
      Model := -Get_Model (Self.Dependencies_Tree);
      Dependencies_View.Append (Scrolled, Expand => True, Fill => True);

      List :=
        Get_Cells (+Get_Column (Self.Dependencies_Tree, Is_Limited_Column));
      Add_Attribute
        (Get_Column (Self.Dependencies_Tree, Is_Limited_Column),
         Cell_Renderer_List.Get_Data (List),
         "activatable", Can_Change_Limited_Column);
      Cell_Renderer_List.Free (List);

      Add_Imported_Projects (Self.Project, Model);

      Gtk_New_From_Icon_Name
              (Button,
               Icon_Name => "gps-add-symbolic",
               Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked (Add_New_Project'Access, Slot => Self);
      Button.Set_Sensitive (not Read_Only);
      Dependencies_View.Append_Button (Button);

      Gtk_New_From_Icon_Name
              (Button,
               Icon_Name => "gps-remove-symbolic",
               Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked (Remove_Project'Access, Slot => Self);
      Button.Set_Sensitive (not Read_Only);
      Dependencies_View.Append_Button (Button);

      Dest_Set
        (Widget  => Self.Dependencies_Tree,
         Flags   => Dest_Default_All,
         Actions => Action_Copy or Action_Move);
      Self.Dependencies_Tree.Drag_Dest_Add_Text_Targets;
      Self.Dependencies_Tree.On_Drag_Data_Received
        (On_Drag_Data_Received'Access,
         Slot => Self);

      --  Create the 'Known Projects' left pane

      Known_Projects_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Known_Projects_View);
      Pane.Pack2 (Known_Projects_View);

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Known_Projects_View,
         Group_Name          => "Known Projects",
         Allow_Multi_Columns => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Self.Known_Projects_Tree := Create_Tree_View
        (Column_Types      => Add_Column_Types,
         Column_Names      =>
           (1 + Project_Info_Column2 => Cst_Project_Name'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 1 + Project_Info_Column2,
         Selection_Mode     => Gtk.Enums.Selection_Single);
      Model := -Get_Model (Self.Known_Projects_Tree);
      Add (Scrolled, Self.Known_Projects_Tree);
      Group_Widget.Append_Child (Scrolled, Expand => True, Fill => True);

      Add_Predefined_Projects (Kernel, Project, Model);

      Source_Set
        (Widget            => Self.Known_Projects_Tree,
         Start_Button_Mask => Button1_Mask,
         Actions           => Action_Copy or Action_Move);
      Self.Known_Projects_Tree.Drag_Source_Add_Text_Targets;
      Self.Known_Projects_Tree.On_Drag_Data_Get (On_Drag_Data_Get'Access);

      --  Set the sensitivity of the projects tree views depending on
      --  'Read Only'.

      Self.Dependencies_Tree.Set_Sensitive (not Read_Only);
      Self.Known_Projects_Tree.Set_Sensitive (not Read_Only);
   end Initialize;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Project_Dependencies_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages, Scenario_Variables);
      Model    : constant Gtk_Tree_Store :=
                   -(Self.Dependencies_Tree.Get_Model);
      Iter     : Gtk_Tree_Iter;
      Imported : Project_Iterator := Project.Start (Direct_Only => True);
      Count    : Natural := 0;
      Found    : Boolean;
      Changed  : Boolean := False;
   begin
      while Current (Imported) /= No_Project loop
         if Current (Imported) /= Project then
            Count := Count + 1;
         end if;
         Next (Imported);
      end loop;

      declare
         Projects : array (1 .. Count) of Project_Type;
      begin
         Count := Projects'First;
         Imported := Start (Project, Direct_Only => True);
         while Current (Imported) /= No_Project loop
            if Current (Imported) /= Project then
               Projects (Count) := Current (Imported);
               Count := Count + 1;
            end if;
            Next (Imported);
         end loop;

         --  We do not want to remove dependencies if they are still valid,
         --  since that would break rename statements for instance

         Iter := Get_Iter_First (Model);
         while Iter /= Null_Iter loop
            Found := False;

            declare
               Project_Info : constant String := Get_String
                 (Model, Iter, Project_Info_Column);
               Project_Name : constant String := Project_Info
                 (Project_Info'First .. EOL (Project_Info) - 1);
            begin

               for P in Projects'Range loop
                  if Projects (P) /= No_Project
                    and then
                      Projects (P).Name = Project_Name
                  then
                     Projects (P) := No_Project;
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Add_Dependency_Internal
                    (Kernel                => Kernel,
                     Importing_Project     => Project,
                     Imported_Project_Path =>
                       Get_File (Model, Iter, Full_Path_Column),
                     Limited_With          =>
                       Get_Boolean (Model, Iter, Is_Limited_Column),
                     Use_Base_Name         =>
                       Get_Boolean (Model, Iter, Use_Base_Name_Column));
                  Changed := True;
               end if;

               Next (Model, Iter);
            end;
         end loop;

            for P in Projects'Range loop
               if Projects (P) /= No_Project then
                  Remove_Imported_Project
                    (Project               => Project,
                     Imported_Project      => Projects (P));
                  Changed := True;
               end if;
            end loop;
      end;

      return Changed;
   end Edit_Project;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Model         : Gtk_Tree_Store;
      Iter          : Gtk_Tree_Iter;
      Project_Info  : String;
      Is_Limited    : Boolean;
      Can_Change    : Boolean;
      Full_Path     : Virtual_File;
      Use_Base_Name : Boolean) is
   begin
      Set_All_And_Clear
        (Model, Iter,
         (Is_Limited_Column         => As_Boolean (Is_Limited),
          Project_Info_Column       => As_String  (Project_Info),
          Full_Path_Column          => As_File    (Full_Path),
          Can_Change_Limited_Column => As_Boolean (Can_Change),
          Use_Base_Name_Column      => As_Boolean (Use_Base_Name)));
   end Set_Columns;

end Project_Dependencies_Editors;

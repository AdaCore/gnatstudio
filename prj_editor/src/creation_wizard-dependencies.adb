------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
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
with Glib;                             use Glib;

with Gtk.Box;                          use Gtk.Box;
with Gtk.Button;                       use Gtk.Button;
with Gtk.Dialog;                       use Gtk.Dialog;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Handlers;                     use Gtk.Handlers;
with Gtk.Scrolled_Window;              use Gtk.Scrolled_Window;
with Gtk.Stock;                        use Gtk.Stock;
with Gtk.Cell_Layout;                  use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;                use Gtk.Cell_Renderer;
with Gtk.Tree_View;                    use Gtk.Tree_View;
with Gtk.Tree_View_Column;             use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;               use Gtk.Tree_Selection;
with Gtk.Tree_Model;                   use Gtk.Tree_Model;
with Gtk.Tree_Store;                   use Gtk.Tree_Store;
with Gtk.Vbutton_Box;                  use Gtk.Vbutton_Box;
with Gtk.Widget;                       use Gtk.Widget;
with Gtk.Window;                       use Gtk.Window;
with Gtkada.Dialogs;                   use Gtkada.Dialogs;
with Gtkada.File_Selector;             use Gtkada.File_Selector;

with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Contexts;              use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Project;               use GPS.Kernel.Project;
with GPS.Main_Window;                  use GPS.Main_Window;
with Projects;                         use Projects;
with GPS.Intl;                         use GPS.Intl;
with Creation_Wizard;                  use Creation_Wizard;
with Creation_Wizard.Full;             use Creation_Wizard.Full;
with Wizards;                          use Wizards;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;              use GNATCOLL.VFS.GtkAda;
with Commands.Interactive;             use Commands, Commands.Interactive;
with GUI_Utils;                        use GUI_Utils;

package body Creation_Wizard.Dependencies is

   package Wizard_Page_Handlers is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Project_Wizard_Page);

   package File_Sets is
     new Ada.Containers.Hashed_Sets (Virtual_File, Full_Name_Hash, "=");

   type Dependency_Project_Page is new Project_Wizard_Page_Record with record
      Kernel  : Kernel_Handle;
      Project : Project_Type;
      Tree    : Gtk_Tree_View;
   end record;
   type Dependency_Project_Page_Access
     is access all Dependency_Project_Page'Class;
   overriding function Create_Content
     (Page : access Dependency_Project_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   overriding procedure Generate_Project
     (Page               : access Dependency_Project_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean);
   --  See inherited documentation

   Cst_Project_Name : aliased String := "Project Name";
   Cst_Directory    : aliased String := "Directory";
   Cst_Limited      : aliased String := "Limited with";

   --  Constants for the dependency editor
   Project_Name_Column       : constant := 0;
   Is_Limited_Column         : constant := 1;
   Can_Change_Limited_Column : constant := 2;
   Full_Path_Column          : constant := 3;
   Use_Base_Name_Column      : constant := 4;

   --  Constants for the "Add from known dialog"
   Selected_Column2         : constant := 0;
   Project_Name_Column2     : constant := 1;
   Directory_Column2        : constant := 2;
   Is_Limited_Column2       : constant := 3;
   Full_Path_Column2        : constant := 4;

   procedure Remove_Project
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
   procedure Add_New_Project
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
   procedure Add_New_Project_From_Wizard
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
   procedure Add_New_Project_From_Known
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page);
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

   procedure Add_Single_Project
     (Project                   : Project_Type;
      Imported                  : Project_Type;
      Model                     : access Gtk_Tree_Store_Record'Class;
      Ignore_If_Imported        : Boolean := False;
      Column_Project_Name       : Gint;
      Column_Is_Limited         : Gint;
      Column_Directory          : Gint;
      Column_Can_Change_Limited : Gint;
      Column_Full_Path          : Gint;
      Column_Selected           : Gint := -1);
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
            Mode => GPS.Kernel.Error,
            Add_LF => False);
         Parse_File_Locations (Kernel, S, -"Project add dependency");
      end Report_Error;

      Use_Relative_Path : constant Boolean :=
                            Get_Paths_Type
                              (Importing_Project) = Projects.Relative
                            or else
                              (Get_Paths_Type (Importing_Project) = From_Pref
                               and then Generate_Relative_Paths.Get_Pref);
      Changed           : Import_Project_Error;
      Result            : Message_Dialog_Buttons;
      Must_Recompute    : Boolean := False;

   begin
      loop
         Changed := Get_Registry (Kernel).Tree.Add_Imported_Project
           (Project                   => Importing_Project,
            Imported_Project_Location => Imported_Project_Path,
            Errors                    => Report_Error'Unrestricted_Access,
            Use_Base_Name             => Use_Base_Name,
            Use_Relative_Path         => Use_Relative_Path,
            Limited_With              => Limited_With);

         exit when Changed /= Project_Already_Exists;

         --  If there is already a project by that name in the tree,
         --  confirm whether we should rename it everywhere

         Result := Message_Dialog
           (Msg => -("A project with this name already exists in the"
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

         exit when Result = Button_Cancel;

         --  Replace all occurrences of the old project with the new one

         declare
            Imported : constant Project_Type :=
              Get_Registry (Kernel).Tree.Project_From_Name
              (+Imported_Project_Path.Base_Name (Project_File_Extension));
            Iter : Project_Iterator :=
              Get_Registry (Kernel).Tree.Root_Project.Start;
            Prj : Project_Type;
            Is_Limited_With : Boolean;
            Imports : Boolean;
         begin
            loop
               Prj := Current (Iter);
               exit when Prj = No_Project;

               Prj.Project_Imports
                 (Imported, Imports => Imports,
                  Is_Limited_With => Is_Limited_With);
               if Imports then
                  Prj.Remove_Imported_Project (Imported);
                  Changed := Get_Registry (Kernel).Tree.Add_Imported_Project
                    (Project           => Prj,
                     Imported_Project_Location => Imported_Project_Path,
                     Errors            => Report_Error'Unrestricted_Access,
                     Use_Base_Name     => Use_Base_Name,
                     Use_Relative_Path => Use_Relative_Path,
                     Limited_With      => Is_Limited_With);
               end if;

               Next (Iter);
            end loop;

            Must_Recompute := True;
         end;
      end loop;

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
                           Set (Model, Iter, Selected_Column2, False);
                           Set (Model, Iter, Project_Name_Column2, +Base);
                           Set (Model, Iter, Directory_Column2,
                                Project_Path (J).Display_Full_Name);
                           Set_File
                             (Model, Iter, Full_Path_Column2, Files (K));
                           Set (Model, Iter, Is_Limited_Column2, False);
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
           (Project, Current (Imported), Model, True,
            Column_Project_Name   => Project_Name_Column2,
            Column_Is_Limited     => Is_Limited_Column2,
            Column_Directory      => Directory_Column2,
            Column_Can_Change_Limited => -1,
            Column_Full_Path      => Full_Path_Column2,
            Column_Selected       => Selected_Column2);
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
           (Project, Current (Imported), Model,
            Column_Project_Name   => Project_Name_Column,
            Column_Is_Limited     => Is_Limited_Column,
            Column_Directory      => -1,
            Column_Can_Change_Limited => Can_Change_Limited_Column,
            Column_Full_Path      => Full_Path_Column);
         Next (Imported);
      end loop;
   end Add_Imported_Projects;

   ------------------------
   -- Add_Single_Project --
   ------------------------

   procedure Add_Single_Project
     (Project                   : Project_Type;
      Imported                  : Project_Type;
      Model                     : access Gtk_Tree_Store_Record'Class;
      Ignore_If_Imported        : Boolean := False;
      Column_Project_Name       : Gint;
      Column_Is_Limited         : Gint;
      Column_Directory          : Gint;
      Column_Can_Change_Limited : Gint;
      Column_Full_Path          : Gint;
      Column_Selected           : Gint := -1)
   is
      Iter            : Gtk_Tree_Iter;
      Is_Imported     : Boolean;
      Is_Limited      : Boolean;
      Must_Be_Limited : Boolean := False;
      Imported_Iter   : Project_Iterator;
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
            Set (Model, Iter, Column_Project_Name, Imported.Name);
            Set (Model, Iter, Column_Is_Limited,
                 Is_Limited or else Must_Be_Limited);
            Set_File
              (Model, Iter, Column_Full_Path, Project_Path (Imported));

            if Column_Can_Change_Limited /= -1 then
               Set (Model, Iter, Column_Can_Change_Limited,
                    not Must_Be_Limited);
            end if;

            if Column_Directory /= -1 then
               Set (Model, Iter, Column_Directory,
                    +Full_Name (Project_Directory (Imported)));
            end if;

            if Column_Selected /= -1 then
               Set (Model, Iter, Column_Selected, False);
            end if;
         end if;
      end if;
   end Add_Single_Project;

   --------------------
   -- Create_Content --
   --------------------

   overriding function Create_Content
     (Page : access Dependency_Project_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Model     : Gtk_Tree_Store;
      Box, Hbox : Gtk_Box;
      Bbox      : Gtk_Vbutton_Box;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      Scrolled  : Gtk_Scrolled_Window;
      List      : Cell_Renderer_List.Glist;
   begin
      Page.Kernel := Get_Kernel (Wiz);
      Page.Project := Get_Project (Project_Wizard (Wiz));

      Gtk_New_Vbox (Box, Homogeneous => False);
      Gtk_New
        (Label, -"Sources in a project can have references to files in other"
         & " projects." & ASCII.LF
         & "Such a relation is represented as a project dependency.");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Expand => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Hbox, Scrolled, Expand => True);
      Page.Tree := Create_Tree_View
        (Column_Types      =>
           (Project_Name_Column       => GType_String,
            Is_Limited_Column         => GType_Boolean,
            Can_Change_Limited_Column => GType_Boolean,
            Full_Path_Column          => Get_Virtual_File_Type,
            Use_Base_Name_Column      => GType_Boolean),
         Column_Names      =>
           (1 + Project_Name_Column => Cst_Project_Name'Unchecked_Access,
            1 + Is_Limited_Column   => Cst_Limited'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 1 + Project_Name_Column,
         Selection_Mode     => Gtk.Enums.Selection_Single);
      Add (Scrolled, Page.Tree);
      Model := -Get_Model (Page.Tree);

      List := Get_Cells (+Get_Column (Page.Tree, Is_Limited_Column));
      Add_Attribute
        (Get_Column (Page.Tree, Is_Limited_Column),
         Cell_Renderer_List.Get_Data (List),
         "activatable", Can_Change_Limited_Column);
      Cell_Renderer_List.Free (List);

      Add_Imported_Projects (Get_Project (Project_Wizard (Wiz)), Model);

      Gtk_New (Bbox);
      Pack_Start (Hbox, Bbox, Expand => False);
      Set_Layout (Bbox, Buttonbox_Start);

      Gtk_New (Button, -"Add From File");
      Pack_Start (Bbox, Button);
      Wizard_Page_Handlers.Connect
        (Button, Gtk.Button.Signal_Clicked, Add_New_Project'Access,
         Project_Wizard_Page (Page));

      Gtk_New (Button, -"Add From Wizard");
      Pack_Start (Bbox, Button);
      Wizard_Page_Handlers.Connect
        (Button, Gtk.Button.Signal_Clicked, Add_New_Project_From_Wizard'Access,
         Project_Wizard_Page (Page));

      Gtk_New (Button, -"Add From Known Projects");
      Pack_Start (Bbox, Button);
      Wizard_Page_Handlers.Connect
        (Button, Gtk.Button.Signal_Clicked, Add_New_Project_From_Known'Access,
         Project_Wizard_Page (Page));

      Gtk_New_From_Stock (Button, Stock_Remove);
      Pack_Start (Bbox, Button);
      Wizard_Page_Handlers.Connect
        (Button, Gtk.Button.Signal_Clicked, Remove_Project'Access,
         Project_Wizard_Page (Page));

      return Gtk_Widget (Box);
   end Create_Content;

   --------------------------------
   -- Add_New_Project_From_Known --
   --------------------------------

   procedure Add_New_Project_From_Known
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      P           : constant Dependency_Project_Page_Access :=
                      Dependency_Project_Page_Access (Page);
      Dialog      : Gtk_Dialog;
      Scrolled    : Gtk_Scrolled_Window;
      B           : Gtk_Widget;
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      PModel      : Gtk_Tree_Store;
      Iter, PIter : Gtk_Tree_Iter;
      pragma Unreferenced (B);
   begin
      Gtk_New (Dialog,
               Title  => -"Add project dependency",
               Parent => Gtk_Window (Get_Toplevel (Button)),
               Flags  => Destroy_With_Parent or Modal);
      Set_Default_Size_From_History
         (Dialog, "project-dependencies", P.Kernel, 500, 600);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start
        (Get_Content_Area (Dialog), Scrolled, Expand => True, Fill => True);

      Tree := Create_Tree_View
        (Column_Types      =>
           (Selected_Column2         => GType_Boolean,
            Project_Name_Column2     => GType_String,
            Directory_Column2        => GType_String,
            Is_Limited_Column2       => GType_Boolean,
            Full_Path_Column2        => Get_Virtual_File_Type),
         Column_Names      =>
           (1 + Selected_Column2     => null,
            1 + Project_Name_Column2 => Cst_Project_Name'Unchecked_Access,
            1 + Directory_Column2    => Cst_Directory'Unchecked_Access,
            1 + Is_Limited_Column2   => Cst_Limited'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 1 + Project_Name_Column2,
         Selection_Mode     => Gtk.Enums.Selection_None);
      Add (Scrolled, Tree);
      Model := -Get_Model (Tree);

      Add_Predefined_Projects (P.Kernel, P.Project, Model);

      B := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      B := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Iter := Get_Iter_First (Model);
         while Iter /= Null_Iter loop
            if Get_Boolean (Model, Iter, Selected_Column2) then
               PModel := -Get_Model (P.Tree);
               Append (PModel, PIter, Null_Iter);
               Set (PModel, PIter, Project_Name_Column,
                    Get_String (Model, Iter, Project_Name_Column2));
               Set (PModel, PIter, Is_Limited_Column,
                    Get_Boolean (Model, Iter, Is_Limited_Column2));
               Set (PModel, PIter, Can_Change_Limited_Column,
                    not Get_Boolean (Model, Iter, Is_Limited_Column2));
               Set_File (PModel, PIter, Full_Path_Column,
                    Get_File (Model, Iter, Full_Path_Column2));
               Set (PModel, PIter, Use_Base_Name_Column, True);
            end if;

            Next (Model, Iter);
         end loop;
      end if;
      Destroy (Dialog);
   end Add_New_Project_From_Known;

   ---------------------------------
   -- Add_New_Project_From_Wizard --
   ---------------------------------

   procedure Add_New_Project_From_Wizard
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      pragma Unreferenced (Button);
      B     : constant Dependency_Project_Page_Access :=
                Dependency_Project_Page_Access (Page);
      Model : constant Gtk_Tree_Store :=
                -Get_Model (B.Tree);
      Wiz   : Creation_Wizard.Project_Wizard;
      Iter  : Gtk_Tree_Iter;
      Name  : Virtual_File;
   begin
      Creation_Wizard.Gtk_New (Wiz, B.Kernel, -"Add New Project");
      Add_Full_Wizard_Pages
        (Wiz, Creation_Wizard.Add_Name_And_Location_Page (Wiz), "wizard");

      Name := Creation_Wizard.Run (Wiz);
      if Name /= GNATCOLL.VFS.No_File then
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Project_Name_Column, Display_Base_Name (Name));
         Set (Model, Iter, Is_Limited_Column, False);
         Set (Model, Iter, Can_Change_Limited_Column, True);
         Set_File (Model, Iter, Full_Path_Column, Name);
         Set (Model, Iter, Use_Base_Name_Column, False);
      end if;
   end Add_New_Project_From_Wizard;

   ---------------------
   -- Add_New_Project --
   ---------------------

   procedure Add_New_Project
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      pragma Unreferenced (Button);
      B     : constant Dependency_Project_Page_Access :=
                Dependency_Project_Page_Access (Page);
      Model : constant Gtk_Tree_Store :=
                -Get_Model (B.Tree);
      Name  : constant Virtual_File := Select_File
        (-"Select Project",
         Get_Current_Dir,
         File_Pattern      => "*.gpr",
         Pattern_Name      => "Project files",
         Parent            => Gtk_Window (Get_Toplevel (B.Tree)),
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Kind              => Open_File,
         History           => Get_History (B.Kernel));
      Iter  : Gtk_Tree_Iter;
   begin
      if Name /= GNATCOLL.VFS.No_File then
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Project_Name_Column, Display_Base_Name (Name));
         Set (Model, Iter, Is_Limited_Column, False);
         Set (Model, Iter, Can_Change_Limited_Column, True);
         Set_File (Model, Iter, Full_Path_Column, Name);
         Set (Model, Iter, Use_Base_Name_Column, False);
      end if;
   end Add_New_Project;

   --------------------
   -- Remove_Project --
   --------------------

   procedure Remove_Project
     (Button : access Gtk_Widget_Record'Class;
      Page   : Project_Wizard_Page)
   is
      pragma Unreferenced (Button);
      B         : constant Dependency_Project_Page_Access :=
                    Dependency_Project_Page_Access (Page);
      Selection : constant Gtk_Tree_Selection :=
                    Get_Selection (B.Tree);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter then
         Remove (-Model, Iter);
      end if;
   end Remove_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Dependency_Project_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean)
   is
      Model    : constant Gtk_Tree_Store := -Get_Model (Page.Tree);
      Iter     : Gtk_Tree_Iter;
      pragma Unreferenced (Scenario_Variables);
      Imported : Project_Iterator := Project.Start (Direct_Only => True);
      Count    : Natural := 0;
      Found    : Boolean;
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

            for P in Projects'Range loop
               if Projects (P) /= No_Project
                 and then Projects (P).Name =
                 Get_String (Model, Iter, Project_Name_Column)
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
   end Generate_Project;

   -----------------------------------
   -- Add_Project_Dependencies_Page --
   -----------------------------------

   procedure Add_Project_Dependencies_Page
     (Wiz : access Project_Wizard_Record'Class)
   is
      P : Project_Wizard_Page;
   begin
      P := new Dependency_Project_Page;
      Add_Page (Wiz,
                Page => P,
                Description => -"Dependencies for this project",
                Toc         => -"Dependencies");
   end Add_Project_Dependencies_Page;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Project_Dependency_Wizard_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      Wiz : Project_Wizard;
   begin
      Gtk_New (Wiz, Get_Kernel (Context.Context),
               Project           => Project_Information (Context.Context),
               Title             => -"Project Dependencies",
               Auto_Save_On_Exit => False);
      Add_Project_Dependencies_Page (Wiz);
      Show_All (Wiz);

      if Run (Wiz) = Gtk_Response_Apply then
         Destroy (Wiz);
         return Success;
      else
         Destroy (Wiz);
         return Failure;
      end if;
   end Execute;

end Creation_Wizard.Dependencies;

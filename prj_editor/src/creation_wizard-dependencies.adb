-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004-2005                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Location_View;        use GPS.Location_View;
with Projects;                 use Projects;
with Projects.Editor;          use Projects.Editor;
with Projects.Registry;        use Projects.Registry;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.File_Selector;     use Gtkada.File_Selector;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Glide_Intl;               use Glide_Intl;
with Creation_Wizard.Full;     use Creation_Wizard, Creation_Wizard.Full;
with Wizards;                  use Wizards;
with VFS;                      use VFS;
with File_Utils;               use File_Utils;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Glib;                     use Glib;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GUI_Utils;                use GUI_Utils;

package body Creation_Wizard.Dependencies is

   type Dependency_Project_Page is new Project_Wizard_Page_Record with record
      Tree : Gtk_Tree_View;
   end record;
   function Create_Content
     (Page : access Dependency_Project_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page    : access Dependency_Project_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean);
   --  See inherited documentation

   type Predefined_Dependency_Page is new Project_Wizard_Page_Record with
      record
         Tree : Gtk_Tree_View;
      end record;
   function Create_Content
     (Page : access Predefined_Dependency_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page    : access Predefined_Dependency_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean);
   --  See inherited documentation

   type New_File_Dependency_Page_Content is new Gtk_Box_Record with record
      Tree   : Gtk_Tree_View;
      Kernel : Kernel_Handle;
   end record;
   type New_File_Dependency_Page_Content_Access is access all
     New_File_Dependency_Page_Content'Class;

   type New_File_Dependency_Page is new Project_Wizard_Page_Record with
      null record;
   function Create_Content
     (Page : access New_File_Dependency_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page    : access New_File_Dependency_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean);
   --  See inherited documentation

   procedure Remove_Project (Box : access Gtk_Widget_Record'Class);
   procedure Add_New_Project (Box : access Gtk_Widget_Record'Class);
   procedure Add_New_Project_From_Wizard
     (Box : access Gtk_Widget_Record'Class);
   --  Add or remove a new project dependency


   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : String;
      Limited_With          : Boolean := False);
   --  Internal function that creates a dependency between two projects. If
   --  properly handle the case where a project with the same name as
   --  Imported_Project_Path already exists in the project hierarchy

   -----------------------------
   -- Add_Dependency_Internal --
   -----------------------------

   procedure Add_Dependency_Internal
     (Kernel                : access Kernel_Handle_Record'Class;
      Importing_Project     : Project_Type;
      Imported_Project_Path : String;
      Limited_With          : Boolean := False)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Console.Insert
           (Kernel, S & ASCII.LF,
            Mode => Console.Error,
            Add_LF => False);
         Parse_File_Locations (Kernel, S, -"Project add dependency");
      end Report_Error;

      Base : constant String := Project_Directory (Importing_Project);
      Use_Relative_Path : constant Boolean :=
        Get_Paths_Type (Importing_Project) = Projects.Relative
        or else (Get_Paths_Type (Importing_Project) = From_Pref
                 and then Get_Pref (Kernel, Generate_Relative_Paths));
      Changed : Import_Project_Error;
      Result : Message_Dialog_Buttons;
      Must_Recompute : Boolean := False;
      Imported_Project : Project_Type;

   begin
      loop
         Changed := Add_Imported_Project
           (Root_Project              => Get_Project (Kernel),
            Project                   => Importing_Project,
            Imported_Project_Location =>
              Normalize_Pathname (Imported_Project_Path, Base),
            Report_Errors     => Report_Error'Unrestricted_Access,
            Use_Relative_Path => Use_Relative_Path,
            Limited_With      => Limited_With);

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

         Imported_Project := Load_Or_Find
           (Get_Registry (Kernel).all, Imported_Project_Path);

         Replace_Project_Occurrences
           (Root_Project      => Get_Project (Kernel),
            Project           => Imported_Project,
            Use_Relative_Path => Use_Relative_Path);
         Must_Recompute := True;
      end loop;

      if Changed = Success or else Must_Recompute then
         Recompute_View (Kernel);
      end if;
   end Add_Dependency_Internal;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Dependency_Project_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      procedure Process_Project (Imported_Prj : Project_Type);
      --  Add lines in the tree for Imported_Prj

      Model : Gtk_Tree_Store;

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Imported_Prj : Project_Type) is
         Iter         : Gtk_Tree_Iter;
         Is_Imported  : Boolean;
         Is_Limited   : Boolean;
         Imported     : Imported_Project_Iterator;
      begin
         if Imported_Prj /= Get_Project (Project_Wizard (Wiz)) then
            Project_Imports
              (Parent          => Get_Project (Project_Wizard (Wiz)),
               Child           => Imported_Prj,
               Imports         => Is_Imported,
               Is_Limited_With => Is_Limited);

            if not Is_Imported then
               Imported := Start (Root_Project => Imported_Prj);
               while Current (Imported) /= No_Project loop
                  if Current (Imported) =
                    Get_Project (Project_Wizard (Wiz))
                  then
                     Is_Limited := True;
                     exit;
                  end if;
                  Next (Imported);
               end loop;
            end if;

            Append (Model, Iter, Null_Iter);
            Set (Model, Iter, 0, Is_Imported);
            Set (Model, Iter, 1, Project_Name (Imported_Prj));
            Set (Model, Iter, 3, Is_Limited);
            Set (Model, Iter, 4, Project_Path (Imported_Prj));
            Set (Model, Iter, 5, Is_Imported);

            if Is_Limited then
               if Is_Imported then
                  Set (Model, Iter, 2, "(through a limited with)");
               else
                  Set (Model, Iter, 2, "(Must be through a limited with)");
               end if;
            else
               Set (Model, Iter, 2, "");
            end if;
         end if;
      end Process_Project;

      Box   : Gtk_Box;
      Label : Gtk_Label;
      Imported_Root : Imported_Project_Iterator;
      Scrolled      : Gtk_Scrolled_Window;
      Cst_Project_Name : aliased String := -"Project Name";
      Cst_Dep_Type     : aliased String := -"Dependency Type";
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New
        (Label, -"Sources in a project can have references to files in other"
         & " projects." & ASCII.LF
         & "Such a relation is represented as a project dependency.");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True);
      Page.Tree := Create_Tree_View
        (Column_Types      => (1 => GType_Boolean,  --  imported ?
                               2 => GType_String,   --  project name
                               3 => GType_String,   --  comment
                               4 => GType_Boolean,  --  limited clause ?
                               5 => GType_String,   --  full path
                               6 => GType_Boolean), --  Initial imported status
         Column_Names      => (1 => null,
                               2 => Cst_Project_Name'Unchecked_Access,
                               3 => Cst_Dep_Type'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 2,
         Selection_Mode     => Gtk.Enums.Selection_None);
      Add (Scrolled, Page.Tree);
      Model := Gtk_Tree_Store (Get_Model (Page.Tree));

      Imported_Root := Start (Root_Project => Get_Project (Get_Kernel (Wiz)));
      while Current (Imported_Root) /= No_Project loop
         Process_Project (Current (Imported_Root));
         Next (Imported_Root);
      end loop;

      return Gtk_Widget (Box);
   end Create_Content;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Predefined_Dependency_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Box              : Gtk_Box;
      Label            : Gtk_Label;
      Iter             : Gtk_Tree_Iter;
      Scrolled         : Gtk_Scrolled_Window;
      Cst_Project_Name : aliased String := -"Project Name";
      Cst_Full_Path    : aliased String := -"Directory";
      Project_Path     : constant String := Get_Predefined_Project_Path
        (Get_Registry (Get_Kernel (Wiz)).all);
      Path_Iter        : Path_Iterator;
      Dir              : Dir_Type;
      File             : String (1 .. 1024);
      Last             : Integer;
      Model            : Gtk_Tree_Store;
      Ent              : Gtk_Entry;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New
        (Label,
         -"The predefined projects are found in the following directories:");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Ent);
      Set_Text (Ent, Project_Path);
      Set_Editable (Ent, False);
      Pack_Start (Box, Ent, Expand => False);

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True);
      Page.Tree := Create_Tree_View
        (Column_Types       => (1 => GType_Boolean,   --  imported ?
                                2 => GType_String,    --  project name
                                3 => GType_String,    --  directory
                                4 => GType_String,    --  full path name
                                5 => GType_Boolean),  --  Initial import status
         Column_Names       => (1 => null,
                                2 => Cst_Project_Name'Unchecked_Access,
                                3 => Cst_Full_Path'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 2,
         Selection_Mode     => Gtk.Enums.Selection_None);
      Add (Scrolled, Page.Tree);
      Model := Gtk_Tree_Store (Get_Model (Page.Tree));

      Path_Iter := Start (Project_Path);
      while not At_End (Project_Path, Path_Iter) loop
         declare
            Directory : constant String := Current (Project_Path, Path_Iter);
            Found     : Boolean := False;
            Iter2     : Path_Iterator := Start (Project_Path);
            Is_Imported : Boolean;
            Is_Limited  : Boolean;
            Imported_Prj : Project_Type;
         begin
            if Directory /= "." then
               --  Make sure the path isn't duplicated
               while Iter2 /= Path_Iter loop
                  if Current (Project_Path, Iter2) = Directory then
                     Found := True;
                     exit;
                  end if;
                  Iter2 := Next (Project_Path, Iter2);
               end loop;

               if not Found then
                  Open (Dir, Directory);
                  loop
                     Read (Dir, File, Last);
                     exit when Last = 0;

                     if Last - File'First > 3
                       and then File (Last - 3 .. Last) = ".gpr"
                     then
                        Imported_Prj := Get_Project_From_Name
                          (Registry => Get_Registry (Get_Kernel (Wiz)).all,
                           Name => Get_String (File (File'First .. Last - 4)));

                        --  If we have a project with the same name, and it
                        --  really is the same.
                        if Imported_Prj /= No_Project
                          and then Project_Directory (Imported_Prj) =
                          Name_As_Directory (Directory)
                        then
                           Project_Imports
                             (Parent     => Get_Project (Project_Wizard (Wiz)),
                              Child           => Imported_Prj,
                              Imports         => Is_Imported,
                              Is_Limited_With => Is_Limited);
                        else
                           Is_Imported := False;
                        end if;

                        Append (Model, Iter, Null_Iter);
                        Set (Model, Iter, 0, Is_Imported);
                        Set (Model, Iter, 1, File (File'First .. Last - 4));
                        Set (Model, Iter, 2, Directory);
                        Set (Model, Iter, 3,
                             Name_As_Directory (Directory)
                             & File (File'First .. Last));
                        Set (Model, Iter, 4, Is_Imported);
                     end if;
                  end loop;
                  Close (Dir);
               end if;
            end if;
         exception
            when Directory_Error =>
               null;
         end;

         Path_Iter := Next (Project_Path, Path_Iter);
      end loop;

      return Gtk_Widget (Box);
   end Create_Content;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access New_File_Dependency_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Page);
      Box              : New_File_Dependency_Page_Content_Access;
      Box2             : Gtk_Box;
      Scrolled         : Gtk_Scrolled_Window;
      Cst_Project_Name : aliased String := -"Project Name";
      Cst_Directory    : aliased String := -"Directory";
      Button           : Gtk_Button;
   begin
      Box := new New_File_Dependency_Page_Content;
      Box.Kernel := Get_Kernel (Wiz);
      Initialize_Hbox (Box, Homogeneous => False);
      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled, Expand => True);
      Box.Tree := Create_Tree_View
        (Column_Types       => (1 => GType_String,    --  project_name ?
                                2 => GType_String,    --  directory
                                3 => GType_String),   --  full path name
         Column_Names       => (1 => Cst_Project_Name'Unchecked_Access,
                                2 => Cst_Directory'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => 2,
         Selection_Mode     => Gtk.Enums.Selection_None);
      Add (Scrolled, Box.Tree);

      Gtk_New_Vbox (Box2, Homogeneous => False);
      Pack_Start (Box, Box2, Expand => False);

      Gtk_New (Button, -"Add from file");
      Pack_Start (Box2, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", Add_New_Project'Access, Box);

      Gtk_New (Button, -"Add from wizard");
      Pack_Start (Box2, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", Add_New_Project_From_Wizard'Access, Box);

      Gtk_New_From_Stock (Button, Stock_Remove);
      Pack_Start (Box2, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", Remove_Project'Access, Box);
      return Gtk_Widget (Box);
   end Create_Content;

   ---------------------------------
   -- Add_New_Project_From_Wizard --
   ---------------------------------

   procedure Add_New_Project_From_Wizard
     (Box : access Gtk_Widget_Record'Class)
   is
      B  : constant New_File_Dependency_Page_Content_Access :=
        New_File_Dependency_Page_Content_Access (Box);
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (B.Tree));
      Wiz  : Creation_Wizard.Project_Wizard;
      Iter : Gtk_Tree_Iter;
   begin
      Creation_Wizard.Gtk_New (Wiz, B.Kernel);
      Add_Full_Wizard_Pages
        (Wiz, Creation_Wizard.Add_Name_And_Location_Page (Wiz));

      declare
         Name : constant String := Creation_Wizard.Run (Wiz);
      begin
         if Name /= "" then
            Append (Model, Iter, Null_Iter);
            Set (Model, Iter, 0, Base_Name (Name));
            Set (Model, Iter, 1, Dir_Name (Name));
            Set (Model, Iter, 2, Name);
         end if;
      end;
   end Add_New_Project_From_Wizard;

   ---------------------
   -- Add_New_Project --
   ---------------------

   procedure Add_New_Project (Box : access Gtk_Widget_Record'Class) is
      B  : constant New_File_Dependency_Page_Content_Access :=
        New_File_Dependency_Page_Content_Access (Box);
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (B.Tree));
      Name : constant Virtual_File := Select_File
        (-"Select Project",
         Get_Current_Dir,
         File_Pattern      => "*.gpr",
         Pattern_Name      => "Project files",
         Parent            => Gtk_Window (Get_Toplevel (B.Tree)),
         Use_Native_Dialog => Get_Pref (B.Kernel, Use_Native_Dialogs),
         Kind              => Open_File,
         History           => Get_History (B.Kernel));
      Iter : Gtk_Tree_Iter;
   begin
      if Name /= VFS.No_File then
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, 0, Base_Name (Name));
         Set (Model, Iter, 1, Dir_Name (Name).all);
         Set (Model, Iter, 2, Full_Name (Name).all);
      end if;
   end Add_New_Project;

   ---------------------
   -- Add_New_Project --
   ---------------------

   procedure Remove_Project (Box : access Gtk_Widget_Record'Class) is
      B  : constant New_File_Dependency_Page_Content_Access :=
        New_File_Dependency_Page_Content_Access (Box);
      Selection : constant Gtk_Tree_Selection :=
        Get_Selection (B.Tree);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter then
         Remove (Gtk_Tree_Store (Model), Iter);
      end if;
   end Remove_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page    : access New_File_Dependency_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean)
   is
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store
        (Get_Model
          (New_File_Dependency_Page_Content_Access (Get_Content (Page)).Tree));
      Iter : Gtk_Tree_Iter := Get_Iter_First (Model);
      pragma Unreferenced (Scenario_Variables);
   begin
      while Iter /= Null_Iter loop
         Add_Dependency_Internal
           (Kernel                => Kernel,
            Importing_Project     => Project,
            Imported_Project_Path => Get_String (Model, Iter, 2));
         Changed := True;
         Next (Model, Iter);
      end loop;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page    : access Predefined_Dependency_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean)
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Page.Tree));
      Iter : Gtk_Tree_Iter := Get_Iter_First (Model);
      pragma Unreferenced (Scenario_Variables);
   begin
      while Iter /= Null_Iter loop
         if Get_Boolean (Model, Iter, 0) /= Get_Boolean (Model, Iter, 4) then
            if Get_Boolean (Model, Iter, 0) then
               Add_Dependency_Internal
                 (Kernel                => Kernel,
                  Importing_Project     => Project,
                  Imported_Project_Path => Get_String (Model, Iter, 3));
            else
               Remove_Imported_Project
                 (Project               => Project,
                  Imported_Project      => Get_Project_From_Name
                    (Registry  => Get_Registry (Kernel).all,
                     Name      => Get_String (Get_String (Model, Iter, 1))));
            end if;

            Changed := True;
         end if;

         Next (Model, Iter);
      end loop;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page    : access Dependency_Project_Page;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean)
   is
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Page.Tree));
      Iter : Gtk_Tree_Iter := Get_Iter_First (Model);
      pragma Unreferenced (Scenario_Variables);
   begin
      while Iter /= Null_Iter loop
         if Get_Boolean (Model, Iter, 0) /= Get_Boolean (Model, Iter, 5) then
            if Get_Boolean (Model, Iter, 0) then
               Add_Dependency_Internal
                 (Kernel                => Kernel,
                  Importing_Project     => Project,
                  Imported_Project_Path => Get_String (Model, Iter, 4),
                  Limited_with          => Get_Boolean (Model, Iter, 3));
            else
               Remove_Imported_Project
                 (Project               => Project,
                  Imported_Project      => Get_Project_From_Name
                    (Registry  => Get_Registry (Kernel).all,
                     Name      => Get_String (Get_String (Model, Iter, 1))));
            end if;
            Changed := True;
         end if;

         Next (Model, Iter);
      end loop;
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
                Description =>
                   -"Select the internal dependencies for this project",
                Toc         => -"Internal dependencies");

      P := new Predefined_Dependency_Page;
      Add_Page (Wiz,
                Page => P,
                Description =>
                   -"Select the dependencies on predefined projects",
                Toc         => -"Predefined projects");

      P := new New_File_Dependency_Page;
      Add_Page (Wiz,
                Page => P,
                Description =>
                   -"Select the dependencies on new projects",
                Toc         => -"New projects");
   end Add_Project_Dependencies_Page;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Project_Dependency_Wizard_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      File : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context.Context);
      Wiz : Project_Wizard;
   begin
      Gtk_New (Wiz, Get_Kernel (File),
               Project           => Project_Information (File),
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

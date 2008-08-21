-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2008, AdaCore             --
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

with Commands.Interactive;     use Commands, Commands.Interactive;
with Glib;                     use Glib;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with Projects;                 use Projects;
with Projects.Editor;          use Projects.Editor;
with Projects.Registry;        use Projects.Registry;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;
with Wizards;                  use Wizards;

package body Creation_Wizard.Extending is

   type Extending_Sources_Page is new Project_Wizard_Page_Record with record
      Copy_Files     : Gtk.Check_Button.Gtk_Check_Button;
      Files          : Gtk.Tree_View.Gtk_Tree_View;
      Projects_Count : Natural;
   end record;
   type Extending_Sources_Page_Access is access all Extending_Sources_Page;
   overriding procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   overriding function Create_Content
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation

   type Edit_In_Extended_Project is
     new Commands.Interactive.Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_In_Extended_Project;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Add the current file to the current extending all project (make a copy
   --  of the file if necessary first)

   type Can_Add_To_Extended is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Can_Add_To_Extended;
      Context : Selection_Context) return Boolean;
   --  True if the selected file can be added to an extending project

   procedure Add_Source_Files
     (Kernel       : access Kernel_Handle_Record'Class;
      Root_Project : Project_Type;
      Files        : GNATCOLL.VFS.File_Array;
      File_Project : Project_Type;
      Copy_Files   : Boolean;
      Recompute    : Boolean);
   --  A Files in an extending project. These files must all belong to
   --  File_Project initially

   --------------------------------
   -- Add_Extending_Wizard_Pages --
   --------------------------------

   procedure Add_Extending_Wizard_Pages
     (Wiz : access Project_Wizard_Record'Class)
   is
      Sources : constant Extending_Sources_Page_Access :=
        new Extending_Sources_Page;
   begin
      Add_Page (Wiz, Sources,
                Description => -"Selecting sources",
                Toc         => -"Sources");
   end Add_Extending_Wizard_Pages;

   --------------------
   -- Create_Content --
   --------------------

   overriding function Create_Content
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Wiz);
      Box      : Gtk_Box;
      Frame    : Gtk_Frame;
      Label    : Gtk_Label;
      Scrolled : Gtk_Scrolled_Window;
      PIter    : Imported_Project_Iterator;
      TIter    : Gtk_Tree_Iter;
      FIter    : Gtk_Tree_Iter;
      Project  : Project_Type;
      Model    : Gtk_Tree_Store;
      File     : GNATCOLL.VFS.Virtual_File;
      File_Index : Positive;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New
        (Label,
         -("Select the source files you want to modify."
           & ASCII.LF
           & "You can add sources later by modifying the project"
           & " properties"));
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Page.Copy_Files,
               -"Copy selected files to the project's directory");
      Pack_Start (Box, Page.Copy_Files, Expand => False, Padding => 5);
      Set_Active (Page.Copy_Files, True);

      Gtk_New (Frame);
      Pack_Start (Box, Frame, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scrolled);

      Page.Files := Create_Tree_View
        (Column_Types       => (0 => GType_Boolean,
                                1 => GType_String,
                                2 => GType_String),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Initial_Sort_On    => 2);
      Model := Gtk_Tree_Store (Get_Model (Page.Files));
      Add (Scrolled, Page.Files);

      Page.Projects_Count := 0;

      PIter := Start (Get_Project (Kernel));
      loop
         Project := Current (PIter);
         exit when Project = No_Project;

         Page.Projects_Count := Page.Projects_Count + 1;

         Append (Model, TIter, Null_Iter);
         Set (Model, TIter, 0, False);
         Set (Model, TIter, 1, Project_Name (Project));
         Set (Model, TIter, 2, "");

         File_Index := 1;
         loop
            File := Get_Source_File (Project, File_Index);
            exit when File = GNATCOLL.VFS.No_File;

            Append (Model, FIter, TIter);
            Set (Model, FIter, 0, False);
            Set (Model, FIter, 1, Base_Name (File));
            Set (Model, FIter, 2, Full_Name (File).all);

            File_Index := File_Index + 1;
         end loop;

         Next (PIter);
      end loop;

      return Gtk_Widget (Box);
   end Create_Content;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      Parent   : Project_Type := Get_Project (Kernel);
      PIter, FIter : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Store;
      File     : GNATCOLL.VFS.Virtual_File;
      Prj      : Project_Type;
      P        : Integer;
      Projects : Project_Type_Array (1 .. Page.Projects_Count) :=
        (others => No_Project);
      Files    : array (1 .. Page.Projects_Count) of File_Array_Access;
      Count    : array (1 .. Page.Projects_Count) of Natural := (others => 0);

      procedure Add_File (Prj : Project_Type; File : Virtual_File);
      --  Add a file into the list of files that are locally modified

      --------------
      -- Add_File --
      --------------

      procedure Add_File (Prj : Project_Type; File : Virtual_File) is
         P   : Integer;
      begin
         P := Projects'First;
         while P <= Projects'Last
           and then Projects (P) /= Prj
           and then Projects (P) /= No_Project
         loop
            P := P + 1;
         end loop;

         if Projects (P) = No_Project then
            Projects (P) := Prj;
         end if;

         if Files (P) = null then
            Files (P) := new File_Array (1 .. Direct_Sources_Count (Prj));
         end if;

         Count (P) := Count (P) + 1;
         Files (P)(Count (P)) := File;
      end Add_File;

   begin
      --  The new project will expand the root project. However, if the latter
      --  is itself an expanding project, we prefer to expand the original
      --  project.

      while Parent_Project (Parent) /= No_Project loop
         Parent := Parent_Project (Parent);
      end loop;

      Set_Extended_Project
        (Project, Parent,
         Extend_All         => True,
         Use_Relative_Paths => True);

      Update_Attribute_Value_In_Scenario
        (Project,
         Scenario_Variables => Scenario_Variables,
         Attribute          => Source_Files_Attribute,
         Values             => (1 .. 0 => null));

      --  Find the list of source files that are modified

      Model := Gtk_Tree_Store (Get_Model (Page.Files));
      PIter := Get_Iter_First (Model);
      while PIter /= Null_Iter loop
         Prj := Get_Project_From_Name
           (Get_Registry (Kernel).all,
            Get_String (Get_String (Model, PIter, 1)));

         --  If the project is selected, import all its source files
         if Get_Boolean (Model, PIter, 0) then
            P := 1;
            loop
               File := Get_Source_File (Prj, P);
               exit when File = GNATCOLL.VFS.No_File;
               Add_File (Prj, File);
               P := P + 1;
            end loop;
         end if;

         FIter := Children (Model, PIter);
         while FIter /= Null_Iter loop
            if Get_Boolean (Model, FIter, 0) then
               File := Create (Full_Filename => Get_String (Model, FIter, 2));
               Add_File (Prj, File);
            end if;

            Next (Model, FIter);
         end loop;

         Next (Model, PIter);
      end loop;

      --  Now create each of the extending project needed for the sources
      --  themselves

      for P in Projects'Range loop
         exit when Projects (P) = No_Project;

         Add_Source_Files
           (Kernel => Kernel,
            Root_Project => Project,
            Files        => Files (P) (1 .. Count (P)),
            File_Project => Projects (P),
            Copy_Files   => Get_Active (Page.Copy_Files),
            Recompute    => False);
         Changed := True;

         Unchecked_Free (Files (P));
      end loop;
   end Generate_Project;

   ----------------------
   -- Add_Source_Files --
   ----------------------

   procedure Add_Source_Files
     (Kernel       : access Kernel_Handle_Record'Class;
      Root_Project : Project_Type;
      Files        : GNATCOLL.VFS.File_Array;
      File_Project : Project_Type;
      Copy_Files   : Boolean;
      Recompute    : Boolean)
   is
      Extended   : Project_Type;
      Iter       : Imported_Project_Iterator := Start (Root_Project);
      File_Names : Argument_List (Files'Range);
      Success    : Boolean;
      pragma Warnings (Off, Success);
      Error      : Import_Project_Error;
      Created    : Boolean := False;
      pragma Unreferenced (Error);
   begin
      --  Search whether there is already a project extending File_Project
      while Current (Iter) /= No_Project
        and then Parent_Project (Current (Iter)) /= File_Project
      loop
         Next (Iter);
      end loop;

      Extended := Current (Iter);

      if Extended = No_Project then
         Extended := Create_Project
           (Get_Registry (Kernel).all,
            Name => Project_Name (File_Project) & "_Extending",
            Path => Project_Directory (Root_Project));

         Set_Extended_Project
           (Project            => Extended,
            Extended           => File_Project,
            Extend_All         => False,
            Use_Relative_Paths => True);

         Error := Add_Imported_Project
           (Root_Project,
            Root_Project,
            Imported_Project  => Extended,
            Use_Relative_Path => True);

         Created := True;
      end if;

      for F in Files'Range loop
         File_Names (F) := new String'(Base_Name (Files (F)));
      end loop;

      Update_Attribute_Value_In_Scenario
        (Extended,
         Scenario_Variables => No_Scenario,
         Attribute          => Source_Files_Attribute,
         Values             => File_Names,
         Prepend            => not Created);

      Free (File_Names);

      if Copy_Files then
         for S in Files'Range loop
            Copy_File
              (Name     => Full_Name (Files (S)).all,
               Pathname => Full_Name (Project_Directory (Extended)).all,
               Success  => Success);
         end loop;
      end if;

      Success := Save_Project (Extended);

      if Recompute then
         Recompute_View (Kernel);
      end if;
   end Add_Source_Files;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_In_Extended_Project;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      File     : constant GNATCOLL.VFS.Virtual_File :=
        File_Information (Context.Context);
      Project  : constant Project_Type :=
        Get_Project_From_File (Get_Registry (Kernel).all, File);
      Button : Message_Dialog_Buttons;
   begin
      Button := Message_Dialog
        (-"Should GPS copy the file in the extending project's directory ?"
         & ASCII.LF
         & Full_Name (Project_Directory (Get_Project (Kernel))).all,
         Dialog_Type => Confirmation,
         Buttons     => Button_Yes or Button_No,
         Title       => -"Copy files ?",
         Parent      => Get_Main_Window (Kernel));

      Add_Source_Files
        (Kernel       => Kernel,
         Root_Project => Get_Project (Kernel),
         Files        => (1 => File),
         File_Project => Project,
         Copy_Files   => Button = Button_Yes,
         Recompute    => True);
      return Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Can_Add_To_Extended;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type;
   begin
      --  If the current root project is an extending all project

      if Parent_Project (Get_Project (Kernel)) /= No_Project then
         File := File_Information (Context);
         if File /= GNATCOLL.VFS.No_File then
            Project := Get_Project_From_File
              (Get_Registry (Kernel).all, File);

            --  If the file doesn't already belong to an extending project
            if Project /= No_Project
              and then Parent_Project (Project) = No_Project
            then
               return True;
            end if;
         end if;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -------------------------------
   -- Register_Contextual_Menus --
   -------------------------------

   procedure Register_Contextual_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
      Filter  : constant Action_Filter := new Can_Add_To_Extended;
   begin
      Command := new Edit_In_Extended_Project;
      Register_Contextual_Menu
        (Kernel, "Add to extending project",
         Action => Command,
         Filter => Filter,
         Label  => "Add to extending project");
   end Register_Contextual_Menus;

end Creation_Wizard.Extending;

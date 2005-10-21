-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

with Glib;                     use Glib;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with Projects;                 use Projects;
with Projects.Editor;          use Projects.Editor;
with Projects.Registry;        use Projects.Registry;
with VFS;                      use VFS;
with Wizards;                  use Wizards;

package body Creation_Wizard.Extending is

   type Extending_Sources_Page is new Project_Wizard_Page_Record with record
      Copy_Files     : Gtk.Check_Button.Gtk_Check_Button;
      Files          : Gtk.Tree_View.Gtk_Tree_View;
      Projects_Count : Natural;
   end record;
   type Extending_Sources_Page_Access is access all Extending_Sources_Page;
   procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   function Create_Content
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   function Is_Complete
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Boolean;
   --  See inherited documentation

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

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Boolean
   is
      pragma Unreferenced (Page);
   begin
      Display_Message
        (Wiz,
         -("Select the source files you want to modify."
           & ASCII.LF
           & "You can add sources later by modifying the project"
           & " properties"),
         As_Error => False);
      return True;
   end Is_Complete;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Extending_Sources_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Wiz);
      Box      : Gtk_Box;
      Frame    : Gtk_Frame;
      Scrolled : Gtk_Scrolled_Window;
      PIter    : Imported_Project_Iterator;
      TIter    : Gtk_Tree_Iter;
      FIter    : Gtk_Tree_Iter;
      Project  : Project_Type;
      Model    : Gtk_Tree_Store;
      File     : VFS.Virtual_File;
      File_Index : Positive;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);

      Gtk_New (Page.Copy_Files,
               -"Copy selected files to the project's directory");
      Pack_Start (Box, Page.Copy_Files, Expand => False, Padding => 5);

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
            File := Get_Source_Files (Project, File_Index);
            exit when File = VFS.No_File;

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

   procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      Parent   : Project_Type := Get_Project (Kernel);
      PIter, FIter : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Store;
      File     : VFS.Virtual_File;
      Prj      : Project_Type;
      P        : Integer;
      Projects : Project_Type_Array (1 .. Page.Projects_Count) :=
        (others => No_Project);
      Sources  : array (1 .. Page.Projects_Count) of Argument_List_Access;
      Full     : array (1 .. Page.Projects_Count) of Argument_List_Access;
      Count    : array (1 .. Page.Projects_Count) of Natural := (others => 0);
      Extended : Project_Type;
      Success  : Boolean;
      Error    : Import_Project_Error;
      pragma Unreferenced (Error);

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
         FIter := Children (Model, PIter);
         while FIter /= Null_Iter loop
            if Get_Boolean (Model, FIter, 0) then
               File := Create (Full_Filename => Get_String (Model, FIter, 2));
               Prj  := Get_Project_From_File
                 (Get_Registry (Kernel).all, File);

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

               if Sources (P) = null then
                  Sources (P) := new Argument_List
                    (1 .. Direct_Sources_Count (Prj));
                  Full (P) := new Argument_List
                    (1 .. Direct_Sources_Count (Prj));
               end if;

               Count (P) := Count (P) + 1;

               Sources (P)(Count (P)) := new String'(Base_Name (File));
               Full (P)(Count (P)) := new String'(Full_Name (File).all);
            end if;

            Next (Model, FIter);
         end loop;

         Next (Model, PIter);
      end loop;

      --  Now create each of the extending project needed for the sources
      --  themselves

      for P in Projects'Range loop
         exit when Projects (P) = No_Project;

         Extended := Create_Project
           (Get_Registry (Kernel).all,
            Name => Project_Name (Projects (P)) & "_Extending",
            Path => Project_Directory (Project));

         Set_Extended_Project
           (Project            => Extended,
            Extended           => Projects (P),
            Extend_All         => False,
            Use_Relative_Paths => True);

         Error := Add_Imported_Project
           (Project,
            Project,
            Imported_Project  => Extended,
            Use_Relative_Path => True);

         Update_Attribute_Value_In_Scenario
           (Extended,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Source_Files_Attribute,
            Values             => Sources (P)(1 .. Count (P)));

         if Get_Active (Page.Copy_Files) then
            for S in 1 .. Count (P) loop
               Copy_File (Name => Full (P)(S).all,
                          Pathname => Project_Directory (Project),
                          Success  => Success);
            end loop;
         end if;

         Changed := True;

         Free (Sources (P));
         Free (Full (P));

         Success := Save_Project (Extended);
      end loop;
   end Generate_Project;

end Creation_Wizard.Extending;

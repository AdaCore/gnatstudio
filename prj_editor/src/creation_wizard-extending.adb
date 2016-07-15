------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

with Commands.Interactive;     use Commands, Commands.Interactive;
with Glib;                     use Glib;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Radio_Button;         use Gtk.Radio_Button;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with Projects;                 use Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;      use GNATCOLL.VFS.GtkAda;
with Wizards;                  use Wizards;

package body Creation_Wizard.Extending is

   type Extending_Sources_Page is new Project_Wizard_Page_Record with record
      Copy_Files     : Gtk.Check_Button.Gtk_Check_Button;
      Files          : Gtk.Tree_View.Gtk_Tree_View;
      Projects_Count : Natural;
      Obj_Dir        : Gtk_Entry;
   end record;
   type Extending_Sources_Page_Access is access all Extending_Sources_Page;
   overriding procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
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

   type Remove_From_Extending_Project is
     new Commands.Interactive.Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_From_Extending_Project;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Remove the current file from the extending project, after asking
   --  confirmation to the user.

   type Can_Add_To_Extended is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Can_Add_To_Extended;
      Context : Selection_Context) return Boolean;
   --  True if the selected file can be added to an extending project

   type In_Extending_Project is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Extending_Project;
      Context : Selection_Context) return Boolean;
   --  True if the selected file is in an extending project

   procedure Add_Source_Files
     (Kernel       : access Kernel_Handle_Record'Class;
      Root_Project : Project_Type;
      Files        : GNATCOLL.VFS.File_Array;
      File_Project : Project_Type;
      In_Dir       : Virtual_File;
      Copy_Files   : Boolean;
      Recompute    : Boolean;
      Obj_Dir      : Filesystem_String := "");
   --  A Files in an extending project. These files must all belong to
   --  File_Project initially.
   --  Obj_Dir is used when a new extending project needs to be created, and
   --  is a directory relative to that new project's location.

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
      Box, hbox : Gtk_Box;
      Frame    : Gtk_Frame;
      Label    : Gtk_Label;
      Scrolled : Gtk_Scrolled_Window;
      PIter    : Project_Iterator;
      TIter    : Gtk_Tree_Iter;
      FIter    : Gtk_Tree_Iter;
      Project  : Project_Type;
      Model    : Gtk_Tree_Store;

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
                                2 => Get_Virtual_File_Type),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Initial_Sort_On    => 2);
      Model := -Get_Model (Page.Files);
      Add (Scrolled, Page.Files);

      Page.Projects_Count := 0;

      PIter := Start (Get_Project (Kernel));
      loop
         Project := Current (PIter);
         exit when Project = No_Project;

         Page.Projects_Count := Page.Projects_Count + 1;

         Append (Model, TIter, Null_Iter);
         Set_And_Clear (Model, TIter,
                        (0 => As_Boolean (False),
                         1 => As_String  (Project.Name),
                         2 => As_File    (No_File)));

         declare
            Sources : File_Array_Access := Project.Source_Files;
         begin
            for F in Sources'Range loop
               Append (Model, FIter, TIter);
               Set_And_Clear (Model, FIter,
                              (0 => As_Boolean (False),
                               1 => As_String  (+Sources (F).Base_Name),
                               2 => As_File    (Sources (F))));
            end loop;

            Unchecked_Free (Sources);
         end;

         Next (PIter);
      end loop;

      Gtk_New_Hbox (hbox);
      Box.Pack_Start (hbox, Expand => False);
      Gtk_New (Label, -"Object directory");
      hbox.Pack_Start (Label, Expand => False);
      Gtk_New (Page.Obj_Dir);
      Page.Obj_Dir.Set_Text ("obj");
      hbox.Pack_Start (Page.Obj_Dir);
      Set_Tooltip_Text (Page.Obj_Dir,
        -("The directory in which the compiler puts its output."
          & " This directory is relative to the location of each of the"
          & " project files that will be created."));

      return Gtk_Widget (Box);
   end Create_Content;

   ----------------------
   -- Generate_Project --
   ----------------------

   overriding procedure Generate_Project
     (Page               : access Extending_Sources_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Project            : in out Project_Type;
      Changed            : in out Boolean)
   is
      Parent   : Project_Type := Get_Project (Kernel);
      PIter, FIter : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Store;
      File     : GNATCOLL.VFS.Virtual_File;
      Prj      : Project_Type;
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
         P : Integer;
      begin
         P := Projects'First;
         while P < Projects'Last
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

      Obj_Dir : constant Filesystem_String := +Page.Obj_Dir.Get_Text;

   begin
      --  The new project will expand the root project. However, if the latter
      --  is itself an expanding project, we prefer to expand the original
      --  project.

      while Extended_Project (Parent) /= No_Project loop
         Parent := Extended_Project (Parent);
      end loop;

      Project.Set_Extended_Project
        (Parent,
         Extend_All         => True,
         Use_Relative_Paths => True);

      Project.Set_Attribute
        (Scenario  => Scenario_Variables,
         Attribute => Source_Files_Attribute,
         Values    => (1 .. 0 => null));

      Project.Set_Attribute
        (Obj_Dir_Attribute,
         Create_From_Dir
           (Project_Directory (Project), Obj_Dir).Display_Full_Name);

      --  Find the list of source files that are modified

      Model := -Get_Model (Page.Files);
      PIter := Get_Iter_First (Model);
      while PIter /= Null_Iter loop
         Prj := Get_Registry (Kernel).Tree.Project_From_Name
           (Get_String (Model, PIter, 1));

         --  If the project is selected, import all its source files
         if Get_Boolean (Model, PIter, 0) then
            declare
               Sources : File_Array_Access := Prj.Source_Files;
            begin
               for P in Sources'Range loop
                  Add_File (Prj, Sources (P));
               end loop;
               Unchecked_Free (Sources);
            end;
         end if;

         FIter := Children (Model, PIter);
         while FIter /= Null_Iter loop
            if Get_Boolean (Model, FIter, 0) then
               File := Get_File (Model, FIter, 2);
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
            In_Dir       => GNATCOLL.VFS.No_File,
            Obj_Dir      => Obj_Dir,
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
      In_Dir       : Virtual_File;
      Copy_Files   : Boolean;
      Recompute    : Boolean;
      Obj_Dir      : Filesystem_String := "")
   is
      Into_Dir     : Virtual_File := In_Dir;
      Extended     : Project_Type;
      Iter         : Project_Iterator := Start (Root_Project);
      File_Names   : Argument_List (Files'Range);
      Created      : Boolean := False;
      Ignore       : Boolean;
      Ignore_Error : Import_Project_Error;
      pragma Unreferenced (Ignore, Ignore_Error);
   begin
      --  Search whether there is already a project extending File_Project
      while Current (Iter) /= No_Project
        and then Extended_Project (Current (Iter)) /= File_Project
      loop
         Next (Iter);
      end loop;

      Extended := Current (Iter);

      if Extended = No_Project then
         Extended := Get_Registry (Kernel).Tree.Create_Project
           (Name => File_Project.Name & "_Extending",
            Path => Project_Directory (Root_Project));

         Extended.Set_Extended_Project
           (Extended           => File_Project,
            Extend_All         => False,
            Use_Relative_Paths => True);

         Extended.Set_Attribute
           (Obj_Dir_Attribute,
            Create_From_Dir
              (Project_Directory (Extended), Obj_Dir).Display_Full_Name);

         Ignore_Error := Root_Project.Add_Imported_Project
           (Imported_Project  => Extended,
            Use_Relative_Path => True);

         Into_Dir := Project_Directory (Extended);

         Created := True;
      end if;

      if Into_Dir = GNATCOLL.VFS.No_File then
         Into_Dir := Project_Directory (Extended);
      end if;

      for F in Files'Range loop
         File_Names (F) := new String'(+Base_Name (Files (F)));
      end loop;

      if Created then
         Extended.Set_Attribute
           (Attribute => Source_Files_Attribute,
            Values    => File_Names);

      else
         declare
            F : File_Array_Access := Extended.Source_Files;
            F2 : Argument_List (F'Range);
         begin
            for S in F'Range loop
               F2 (S) := new String'(+F (S).Base_Name);
            end loop;

            Unchecked_Free (F);

            Extended.Set_Attribute
              (Attribute => Source_Files_Attribute,
               Values    => File_Names & F2);
            Free (F2);
         end;
      end if;

      Free (File_Names);

      if Copy_Files then
         for S in Files'Range loop
            Files (S).Copy (Into_Dir.Full_Name, Success => Ignore);
         end loop;
      end if;

      Ignore := Extended.Save;

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
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      File     : constant GNATCOLL.VFS.Virtual_File :=
        File_Information (Context.Context);
      Project  : constant Project_Type :=
        Project_Information (Context.Context);
      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Ignore : Gtk_Widget;
      Response : Gtk_Response_Type;
      Dirs   : constant GNATCOLL.VFS.File_Array :=
        Get_Project (Kernel).Source_Dirs (Recursive => False);
      Radio  : array (Dirs'Range) of Gtk_Radio_Button;
      Prj_Dir_Radio : Gtk_Radio_Button;
      pragma Unreferenced (Command, Ignore);
   begin
      Gtk_New
        (Dialog,
         Title  => -"Copy file from extended project",
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);

      Gtk_New
        (Label,
         -"Should GPS copy the file into the extending projects source dir ?");
      Label.Set_Selectable (True);
      Label.Set_Justify (Justify_Center);
      Dialog.Get_Content_Area.Pack_Start (Label, Expand => False);

      for D in Dirs'Range loop
         Gtk_New
           (Radio_Button => Radio (D),
            Group        => Radio (Radio'First),
            Label        => Dirs (D).Display_Full_Name);
         Radio (D).Set_Active (D = Dirs'First);
         Dialog.Get_Content_Area.Pack_Start (Radio (D), Expand => False);
      end loop;

      if Dirs'Length = 0 then
         Gtk_New
           (Radio_Button => Prj_Dir_Radio,
            Label        =>
              Project_Directory (Get_Project (Kernel)).Display_Full_Name);
         Prj_Dir_Radio.Set_Active (True);
         Dialog.Get_Content_Area.Pack_Start (Prj_Dir_Radio, Expand => False);
      end if;

      Ignore := Dialog.Add_Button (-"Copy", Gtk_Response_Yes);
      Ignore := Dialog.Add_Button (-"Do not copy", Gtk_Response_No);
      Ignore := Dialog.Add_Button (-"Cancel", Gtk_Response_Cancel);

      Dialog.Show_All;
      Response := Dialog.Run;

      if Response /= Gtk_Response_Cancel then
         if Dirs'Length = 0 then
            Add_Source_Files
              (Kernel       => Kernel,
               Root_Project => Get_Project (Kernel),
               Files        => (1 => File),
               File_Project => Project,
               In_Dir       => Project_Directory (Get_Project (Kernel)),
               Copy_Files   => Response = Gtk_Response_Yes,
               Recompute    => True);
         else
            for R in Radio'Range loop
               if Radio (R).Get_Active then
                  Add_Source_Files
                    (Kernel       => Kernel,
                     Root_Project => Get_Project (Kernel),
                     Files        => (1 => File),
                     File_Project => Project,
                     In_Dir       => Dirs (R),
                     Copy_Files   => Response = Gtk_Response_Yes,
                     Recompute    => True);
               end if;
            end loop;
         end if;
      end if;

      Dialog.Destroy;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_From_Extending_Project;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      File    : constant Virtual_File := File_Information (Context.Context);
      Removed : Boolean;
      Project : Project_Type;
      List    : String_List_Access;
      Dialog  : Gtk_Dialog;
      Ignore  : Gtk_Widget;
      Response : Gtk_Response_Type;
      Label    : Gtk_Label;
      pragma Unreferenced (Command, Ignore);

   begin
      if Active (Testsuite_Handle) then
         --  No confirmation dialog in the testsuite
         Response := Gtk_Response_Yes;
      else
         Gtk_New
           (Dialog,
            Title  => -"Remove file from extended project",
            Parent => Get_Main_Window (Kernel),
            Flags  => Modal or Destroy_With_Parent);

         Gtk_New
           (Label,
            -"Should GPS remove the file from the disk as well ?");
         Label.Set_Selectable (True);
         Label.Set_Justify (Justify_Center);
         Dialog.Get_Content_Area.Pack_Start (Label, Expand => False);

         Ignore := Dialog.Add_Button (-"Delete", Gtk_Response_Yes);
         Ignore := Dialog.Add_Button (-"Do not delete", Gtk_Response_No);
         Ignore := Dialog.Add_Button (-"Cancel", Gtk_Response_Cancel);

         Dialog.Show_All;
         Response := Dialog.Run;
         Dialog.Destroy;
      end if;

      if Response = Gtk_Response_Cancel then
         return Success;
      end if;

      if Response = Gtk_Response_Yes then
         Delete (File, Removed);
         if not Removed then
            Kernel.Insert
              (Text => -"Failed to remove " & File.Display_Full_Name,
               Mode => GPS.Kernel.Error);
            return Failure;
         end if;
      end if;

      Project := Project_Information (Context.Context);
      if Project.Has_Attribute (Source_Files_Attribute) then
         List := Project.Attribute_Value (Source_Files_Attribute);
         for L in List'Range loop
            if List (L).all = +File.Base_Name then
               Free (List (L));
               exit;
            end if;
         end loop;

         Project.Set_Attribute (Source_Files_Attribute, List.all);
         Free (List);

      elsif Project.Has_Attribute (Source_List_File_Attribute) then
         Kernel.Insert
           (Text => -"Project '"
            & Project.Name
            & (-("' specifies its sources via a"
              & " Source_List_File attribute, which wasn't edited"
              & " automatically")));
      end if;

      Recompute_View (Kernel);

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

      if Extended_Project (Get_Project (Kernel)) /= No_Project then
         File := File_Information (Context);
         if File /= GNATCOLL.VFS.No_File then
            --  If the file doesn't already belong to an extending project

            Project := Project_Information (Context);
            return Project /= No_Project
              and then Extended_Project (Project) = No_Project;
         end if;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access In_Extending_Project;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context);
      File    : constant Virtual_File := File_Information (Context);
      Project : Project_Type;
      Set     : File_Info_Set;
   begin
      if File /= GNATCOLL.VFS.No_File then
         Set := Get_Registry (Kernel).Tree.Info_Set (File);

         for Info of Set loop
            declare
               F_Info : constant File_Info'Class := File_Info'Class (Info);
            begin
               Project := F_Info.Project;
            end;
            if Project /= No_Project
              and then Extended_Project (Project) /= No_Project
            then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -------------------------------
   -- Register_Contextual_Menus --
   -------------------------------

   procedure Register_Contextual_Menus
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Action
        (Kernel, "Add to extending project",
         Command     => new Edit_In_Extended_Project,
         Filter      => new Can_Add_To_Extended,
         Description =>
           "Add a copy of the selected file to an extending project (either"
           & " the root project or one the specifically extends the selected"
           & " project",
         Category    => -"Project");
      Register_Contextual_Menu (Kernel, "Add to extending project");

      Register_Action
        (Kernel, "Remove from extending project",
         Command => new Remove_From_Extending_Project,
         Filter  => new In_Extending_Project,
         Description =>
           "If the selected file belongs to an extending project, delete it"
           & " from that project",
         Category    => -"Project");
      Register_Contextual_Menu (Kernel, "Remove from extending project");
   end Register_Contextual_Menus;

end Creation_Wizard.Extending;

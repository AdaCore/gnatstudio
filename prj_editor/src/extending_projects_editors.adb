------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib;                     use Glib;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Radio_Button;         use Gtk.Radio_Button;
with Gtk.Widget;               use Gtk.Widget;

with Commands.Interactive;     use Commands, Commands.Interactive;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with Projects;                 use Projects;

package body Extending_Projects_Editors is

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
         Category    => -"Projects");
      Register_Contextual_Menu (Kernel, "Add to extending project");

      Register_Action
        (Kernel, "Remove from extending project",
         Command => new Remove_From_Extending_Project,
         Filter  => new In_Extending_Project,
         Description =>
           "If the selected file belongs to an extending project, delete it"
           & " from that project",
         Category    => -"Projects");
      Register_Contextual_Menu (Kernel, "Remove from extending project");
   end Register_Contextual_Menus;

end Extending_Projects_Editors;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPR_Creation;              use GPR_Creation;
with GPS.Kernel;              use GPS.Kernel;
with Projects;                  use Projects;
with Directory_Tree;            use Directory_Tree;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Basic_Types;               use Basic_Types;
with Creation_Wizard;           use Creation_Wizard;
with Gtk.Widget;                use Gtk.Widget;
with Wizards;                   use Wizards;

package body Creation_Wizard.Simple is

   type Dirs_Selection_Page is new Project_Wizard_Page_Record with record
      Dirs : Directory_Tree.Directory_Selector;
   end record;
   type Dirs_Selection_Page_Access is access all Dirs_Selection_Page'Class;
   procedure Generate_Project
     (Page               : access Dirs_Selection_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   function Create_Content
     (Page : access Dirs_Selection_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation

   type Obj_Dirs_Page is new Dirs_Selection_Page with record
      Src_Dirs : Dirs_Selection_Page_Access;
   end record;
   type Obj_Dirs_Page_Access is access all Obj_Dirs_Page'Class;
   function Create_Content
     (Page : access Obj_Dirs_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   procedure Generate_Project
     (Page               : access Obj_Dirs_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   --  See inherited documentation

   -----------------------------
   -- Add_Simple_Wizard_Pages --
   -----------------------------

   procedure Add_Simple_Wizard_Pages
     (Wiz : access Project_Wizard_Record'Class)
   is
      Src_Dirs : Dirs_Selection_Page_Access;
      Obj_Dirs : Obj_Dirs_Page_Access;
   begin
      Src_Dirs := new Dirs_Selection_Page;
      Add_Page (Wiz, Src_Dirs,
                Description  => "Source directories",
                Toc          => "Source directories");

      Obj_Dirs := new Obj_Dirs_Page;
      Obj_Dirs.Src_Dirs := Src_Dirs;
      Add_Page (Wiz, Obj_Dirs,
                Description  => "Object directories",
                Toc          => "Object directories");
   end Add_Simple_Wizard_Pages;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Dirs_Selection_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Wiz);
      Current : aliased String := Get_Current_Dir;
      Initial_Dirs : constant String_List := (1 => Current'Unchecked_Access);
   begin
      Gtk_New (Page.Dirs,
               Initial_Directory    => Get_Current_Dir,
               Multiple_Directories => True,
               Initial_Selection    => Initial_Dirs);
      return Gtk_Widget (Page.Dirs);
   end Create_Content;

   --------------------
   -- Create_Content --
   --------------------

   function Create_Content
     (Page : access Obj_Dirs_Page;
      Wiz  : access Wizard_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Wiz);
      Selection   : String_List := Get_Multiple_Selection (Page.Src_Dirs.Dirs);
      Current     : GNAT.OS_Lib.String_Access;
      Current_Dir : aliased String := Get_Current_Dir;
   begin
      if Selection'Length /= 0 then
         Current := Selection (Selection'First);
      else
         Current := Current_Dir'Unchecked_Access;
      end if;

      declare
         Initial_Dirs : constant String_List := (1 => Current);
      begin
         Gtk_New (Page.Dirs,
                  Initial_Directory    => Current.all,
                  Multiple_Directories => True,
                  Initial_Selection    => Initial_Dirs);
      end;

      Free (Selection);
      return Gtk_Widget (Page.Dirs);
   end Create_Content;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Dirs_Selection_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      pragma Unreferenced (Page, Project, Kernel, Scenario_Variables, Changed);
   begin
      null;
   end Generate_Project;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Page               : access Obj_Dirs_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean)
   is
      pragma Unreferenced (Scenario_Variables);
      Src_Dirs : String_List := Get_Multiple_Selection (Page.Src_Dirs.Dirs);
      Obj_Dirs : String_List := Get_Multiple_Selection (Page.Dirs);
   begin
      Create_Gpr_Files
        (Registry          => Get_Registry (Kernel).all,
         Root_Project      => Project,
         Source_Dirs       => Src_Dirs,
         Object_Dirs       => Obj_Dirs,
         Spec_Extension    => ".ads",
         Body_Extension    => ".adb",
         Main_Units        => null,
         Builder_Switches  => "-g",
         Compiler_Switches => "-g",
         Binder_Switches   => "-g",
         Linker_Switches   => "",
         Cross_Prefix      => "");
      Free (Src_Dirs);
      Free (Obj_Dirs);
      Changed := True;
   end Generate_Project;

end Creation_Wizard.Simple;

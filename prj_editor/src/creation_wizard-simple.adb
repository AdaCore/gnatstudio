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

with Glide_Kernel.Project;      use Glide_Kernel.Project;
with GPR_Creation;              use GPR_Creation;
with Glide_Kernel;              use Glide_Kernel;
with Projects;                  use Projects;
with Directory_Tree;            use Directory_Tree;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Basic_Types;               use Basic_Types;

package body Creation_Wizard.Simple is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Wiz    : out Simple_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Wiz := new Simple_Wizard_Record;
      Creation_Wizard.Simple.Initialize (Wiz, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Wiz    : access Simple_Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Initial_Dirs : String_List := (1 => new String'(Get_Current_Dir));
   begin
      Creation_Wizard.Initialize
        (Wiz, Kernel,
         Force_Relative_Dirs       => True,
         Ask_About_Loading         => False,
         Activate_Finish_From_Page => 1);

      Gtk_New (Wiz.Src_Dirs,
               Initial_Directory    => Get_Current_Dir,
               Multiple_Directories => True,
               Initial_Selection    => Initial_Dirs);
      Show_All (Wiz.Src_Dirs);
      Add_Page (Wiz, Wiz.Src_Dirs,
                Title        => "Source directories",
                Toc_Contents => "Source directories");

      Gtk_New (Wiz.Obj_Dirs,
               Initial_Directory    => Get_Current_Dir,
               Multiple_Directories => True,
               Initial_Selection    => Initial_Dirs);
      Show_All (Wiz.Obj_Dirs);
      Add_Page (Wiz, Wiz.Obj_Dirs,
                Title        => "Object directories",
                Toc_Contents => "Object directories");

      Free (Initial_Dirs (1));
   end Initialize;

   ----------------------
   -- Generate_Project --
   ----------------------

   procedure Generate_Project
     (Wiz     : access Simple_Wizard_Record;
      Project : in out Projects.Project_Type)
   is
      Src_Dirs : String_List := Get_Multiple_Selection (Wiz.Src_Dirs);
      Obj_Dirs : String_List := Get_Multiple_Selection (Wiz.Obj_Dirs);
   begin
      Create_Gpr_Files
        (Registry          => Get_Registry (Wiz.Kernel).all,
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
   end Generate_Project;

end Creation_Wizard.Simple;

-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Clist;
with Gtk.GEntry;
with Gtk.Menu;
with Gtk.Check_Button;

with Wizards;
with Directory_Tree;
with Switches_Editors;
with Naming_Editors;
with Glide_Kernel;

package Creation_Wizard is

   type Prj_Wizard_Record is new Wizards.Wizard_Record with private;
   type Prj_Wizard is access all Prj_Wizard_Record'Class;

   procedure Gtk_New
     (Wiz : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project wizard

   procedure Initialize
     (Wiz : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal function for the creation of a new wizard

private
   type Prj_Wizard_Record is new Wizards.Wizard_Record with record
      Project_Name      : Gtk.GEntry.Gtk_Entry;
      Project_Location  : Gtk.GEntry.Gtk_Entry;
      Src_Dir_Selection : Directory_Tree.Dir_Tree;
      Src_Dir_List      : Gtk.Clist.Gtk_Clist;
      Obj_Dir_Selection : Directory_Tree.Dir_Tree;
      Switches          : Switches_Editors.Switches_Edit;
      Naming            : Naming_Editors.Naming_Editor;
      Load_Project      : Gtk.Check_Button.Gtk_Check_Button;

      Kernel            : Glide_Kernel.Kernel_Handle;

      Dir_Contextual_Menu : Gtk.Menu.Gtk_Menu;
      Src_Dir_Contextual_Menu : Gtk.Menu.Gtk_Menu;
   end record;

end Creation_Wizard;

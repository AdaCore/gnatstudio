-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
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

with Gtk.Scrolled_Window;
with Gtk.Tree_Store;
with Glide_Kernel;
with Prj.Tree;

package Scenario_Selectors is

   type Project_Selector_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Project_Selector is access all Project_Selector_Record'Class;

   procedure Gtk_New
     (Selector : out Project_Selector;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ref_Project : Prj.Tree.Project_Node_Id);
   --  Create a new project selector.
   --  Ref_Project is the project whose settings are shown in the project
   --  properties editor. As a result, it can never be unselected.

   procedure Initialize
     (Selector : access Project_Selector_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ref_Project : Prj.Tree.Project_Node_Id);
   --  Internal version of Gtk_New


   type Scenario_Selector_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Scenario_Selector is access all Scenario_Selector_Record'Class;

   procedure Gtk_New
     (Selector : out Scenario_Selector;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project selector.
   --  Ref_Project is the project whose settings are shown in the project
   --  properties editor. As a result, it can never be unselected.

   procedure Initialize
     (Selector : access Scenario_Selector_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal version of Gtk_New

private

   type Project_Selector_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
   record
      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Ref_Project : Prj.Tree.Project_Node_Id;
      Kernel      : Glide_Kernel.Kernel_Handle;
      Select_All  : Boolean := True;
   end record;

   type Scenario_Selector_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
   record
      Model      : Gtk.Tree_Store.Gtk_Tree_Store;
      Kernel     : Glide_Kernel.Kernel_Handle;
      Select_All : Boolean := True;
   end record;

end Scenario_Selectors;

-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Store;
with Gtk.Widget;
with New_Variable_Editor_Pkg; use New_Variable_Editor_Pkg;
with Prj.Tree;
with Glide_Kernel;

package Variable_Editors is

   type New_Var_Edit_Record is new New_Variable_Editor_Record with private;
   type New_Var_Edit is access all New_Var_Edit_Record'Class;

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Var    : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node;
      Title  : String);
   --  Create an editor for the variable Var (or for a new variable if
   --  Var is Empty_Node.
   --  If Scenario_Variable_Only is True, then only the options related to
   --  scenario variables can be modified interactively by the user.

   function Update_Variable (Editor : access New_Var_Edit_Record)
      return Boolean;
   --  Called to validate and take into account the contents of the editor.
   --  If everything is valid, the editor is closed, otherwise an error
   --  message is displayed.
   --  If False is returned, the the editor should be run again, since there
   --  was some incorrect information.

   procedure On_Add_Variable
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Callback to add a new scenario variable to a project. The variable
   --  declaration is added to the project file associated with Context, but
   --  will of course be available for all projects in the hierarchy.

private
   type New_Var_Edit_Record is new New_Variable_Editor_Record with record
      Kernel : Glide_Kernel.Kernel_Handle;
      Var : Prj.Tree.Project_Node_Id;
      --  Variable being edited (or Empty_Node for a new variable)

      Model             : Gtk.Tree_Store.Gtk_Tree_Store;
      Editable_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   end record;

end Variable_Editors;

------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Store;
with New_Variable_Editor_Pkg; use New_Variable_Editor_Pkg;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GPS.Kernel;
with Commands.Interactive;

package Variable_Editors is

   type New_Var_Edit_Record is new New_Variable_Editor_Record with private;
   type New_Var_Edit is access all New_Var_Edit_Record'Class;

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Var    : Scenario_Variable := No_Variable;
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
   --  If False is returned, the editor should be run again, since there
   --  was some incorrect information.

   type Add_Variable_Command
     is new Commands.Interactive.Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Variable_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Callback to add a new scenario variable to a project. The variable
   --  declaration is added to the project file associated with Context, but
   --  will of course be available for all projects in the hierarchy.

private
   type New_Var_Edit_Record is new New_Variable_Editor_Record with record
      Var    : Scenario_Variable;
      --  Variable being edited (or Empty_Node for a new variable)

      Model             : Gtk.Tree_Store.Gtk_Tree_Store;
      Editable_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   end record;

end Variable_Editors;

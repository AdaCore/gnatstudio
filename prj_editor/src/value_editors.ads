-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This widget provides a text area to edit the value of variables.
--  It automatically detect references to variables, and highlights them
--  accordingly, so that it is clearer we have a reference.
--  It can also be given a specific size.
--  </description>

with Gtk.Text;
with Gdk.Color;
with Gtk.Handlers;

with Prj;
with Prj.Tree;

with Glide_Kernel;

package Value_Editors is

   type Value_Editor_Record is new Gtk.Text.Gtk_Text_Record with private;
   type Value_Editor is access all Value_Editor_Record'Class;

   procedure Gtk_New (Editor : out Value_Editor);
   --  Create a new value editor for variables

   procedure Initialize (Editor : access Value_Editor_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Configure
     (Editor : access Value_Editor_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Configure the value editor with the appropriate setup based on the
   --  preferences.

   procedure Set_Variable_Kind
     (Editor : access Value_Editor_Record; Expr_Kind : Prj.Variable_Kind);
   --  Set the kind of variable edited in the editor (a single expression or a
   --  list of expressions).

   procedure Set_Visible_Lines
     (Editor : access Value_Editor_Record; Lines : Natural);
   --  Set the number of visible lines in the editor

   procedure Add_Variable_Reference
     (Editor : access Value_Editor_Record; Var_Name : String);
   --  Add a reference to a variable Var_Name at the current location in
   --  the editor.

   procedure Allow_References
     (Editor : access Value_Editor_Record; Allow : Boolean);
   --  If Allow is False, references to other variables are not authorized,
   --  and will be highlighted as such.

   type Validity is
     (Unterminated_Ref,  --  '${' unterminated
      Unexpected_Ref,    --  references not authorized
      Type_Mismatch,     --  Variable and editor don't have the same typ
      Valid);

   function Check_Validity
     (Editor : access Value_Editor_Record) return Validity;
   --  Return True if the value in Editor is valid:
   --  Checks that references to variables are not circular.
   --  Checks that there are not references if these are not authorized for
   --  this editor.

   function Get_Value
     (Editor  : access Value_Editor_Record;
      Project : Prj.Tree.Project_Node_Id) return Prj.Tree.Project_Node_Id;
   --  Return the N_Expression node representing the contents of the editor.
   --  It is assumed that Check_Validity has been called before.
   --  Values in Var are reused as much as possible to save memory.
   --  Var must have been created already.


private
   type Value_Editor_Record is new Gtk.Text.Gtk_Text_Record with record
      Var_Ref     : Gdk.Color.Gdk_Color;
      Invalid_Ref : Gdk.Color.Gdk_Color;
      Insert_Id   : Gtk.Handlers.Handler_Id;
      Delete_Id   : Gtk.Handlers.Handler_Id;

      Allow_Ref   : Boolean := True;
      --  Should be set to False if references to other variables are not
      --  authorized (only static strings)

      Expr_Kind   : Prj.Variable_Kind := Prj.List;
      --  Kind of variable being edited
   end record;
end Value_Editors;

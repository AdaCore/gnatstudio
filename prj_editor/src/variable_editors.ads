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

with Glib;                    use Glib;
with Gtk.Label;
with Gtk.Combo;
with Gtk.GEntry;
with Gtk.Scrolled_Window;
with Variable_Editor_Pkg;     use Variable_Editor_Pkg;
with New_Variable_Editor_Pkg; use New_Variable_Editor_Pkg;
with Value_Editors;
with Prj_API;
with Prj.Tree;
with Glide_Kernel;

package Variable_Editors is

   type Variable_Edit_Record is new Variable_Editor_Record with private;
   type Variable_Edit is access all Variable_Edit_Record'Class;

   type New_Var_Edit_Record is new New_Variable_Editor_Record with private;
   type New_Var_Edit is access all New_Var_Edit_Record'Class;

   procedure Gtk_New
     (Editor  : out Variable_Edit;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node);
   --  Create a new variable editor, associated with Project (and possibly
   --  one of the packages inside that project).
   --  The editor is empty initially. You can either add specific variables
   --  to it, or call refresh to automatically add all defined variables.

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Var_Edit : access Variable_Edit_Record'Class;
      Var : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node;
      Scenario_Variable_Only : Boolean);
   --  Create an editor for the variable Var (or for a new variable if
   --  Var is Empty_Node.
   --  If Scenario_Variable_Only is True, then only the options related to
   --  scenario variables can be modified interactively by the user.

   procedure Refresh
     (Editor : access Variable_Edit_Record;
      Var    : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node);
   --  Refresh the value either for a specific variable or for all variables
   --  in the project if Var is Empty_Node.
   --  If Var is not present in the editor already, it is added at the end.

   procedure Update_Variable (Editor : access New_Var_Edit_Record);
   --  Called to validate and take into account the contents of the editor.
   --  If everything is valid, the editor is closed, otherwise an error
   --  message is displayed.

   procedure Name_Changed (Editor : access New_Var_Edit_Record);
   --  Called when the name of a variable is changed. This is used to visually
   --  report that fact (change the label of the name frame).

   procedure Display_Expr
     (Editable : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Expr : Prj_API.String_List_Iterator);
   --  Display Expr in GEntry, with variable references replaced with
   --  '$<name>'.
   --  It also displays the other expressions in the list.
   --  GEntry is not deleted first.

private

   type Row_Data is record
      Var : Prj.Tree.Project_Node_Id;
      Name_Label : Gtk.Label.Gtk_Label;
      Type_Combo : Gtk.Combo.Gtk_Combo := null;
      Value_Edit : Value_Editors.Value_Editor := null;
      Scrolled   : Gtk.Scrolled_Window.Gtk_Scrolled_Window := null;
      Env_Label  : Gtk.Label.Gtk_Label;
   end record;
   --  Unfortunately, there is currently no way to get the widgets at a
   --  specific location in a Gtk_Table. Thus, we need to store the data
   --  separately in our own array (An array is used for efficiency reasons)

   type Row_Data_Array is array (Guint range <>) of Row_Data;
   type Row_Data_Array_Access is access Row_Data_Array;

   type Variable_Edit_Record is new Variable_Editor_Record with record
      Num_Rows : Guint := 2;
      Kernel   : Glide_Kernel.Kernel_Handle;
      Pkg      : Prj.Tree.Project_Node_Id;
      Data     : Row_Data_Array_Access := null;
   end record;

   type New_Var_Edit_Record is new New_Variable_Editor_Record with record
      Var_Editor : Variable_Edit;
      Var : Prj.Tree.Project_Node_Id;
      --  Variable being edited (or Empty_Node for a new variable)

      Name_Was_Changed : Boolean := False;
      --  Set to true the first time the name is changed
   end record;

end Variable_Editors;

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
with Variable_Editor_Pkg;     use Variable_Editor_Pkg;
with New_Variable_Editor_Pkg; use New_Variable_Editor_Pkg;
with Prj.Tree;

package Variable_Editors is

   type Variable_Edit_Record is new Variable_Editor_Record with private;
   type Variable_Edit is access all Variable_Edit_Record'Class;

   type New_Var_Edit_Record is new New_Variable_Editor_Record with private;
   type New_Var_Edit is access all New_Var_Edit_Record'Class;

   procedure Gtk_New (Editor : out Variable_Edit);
   --  Create a new variable editor

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Var : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node);
   --  Create an editor for the variable Var (or for a new variable if
   --  Var is Empty_Node.

   procedure Add_Variable
     (Editor : access Variable_Edit_Record'Class;
      Var    : Prj.Tree.Project_Node_Id);
   --  Add a new entry in the editor for Var.

private

   type Variable_Edit_Record is new Variable_Editor_Record with record
      Num_Rows : Guint := 2;
   end record;

   type New_Var_Edit_Record is new New_Variable_Editor_Record with record
      Var : Prj.Tree.Project_Node_Id;
      --  Variable being edited (or Empty_Node for a new variable)
   end record;

end Variable_Editors;

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

with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib;                     use Glib;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.List;                 use Gtk.List;
with Gtk.List_Item;            use Gtk.List_Item;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gui_Support;              use Gui_Support;
with Gtkada.Dialogs;           use Gtkada.Dialogs;

with Prj.Tree;   use Prj.Tree;
with Prj;        use Prj;
with Prj.Ext;    use Prj.Ext;
with Prj_Normalize; use Prj_Normalize;

with Types;      use Types;
with Stringt;    use Stringt;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;     use System;

with Prj_API;              use Prj_API;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Intl;           use Glide_Intl;
with Traces;               use Traces;

package body Variable_Editors is

   Me : Debug_Handle := Create ("Variable_Editors");

   New_Value_Name : constant String := -"<Enter value name>";
   --  Name used for the new variables.

   function Get_Nth_Environment (Index : Natural) return chars_ptr;
   pragma Import (C, Get_Nth_Environment, "get_nth_environment");
   --  Return the string describing the nth environment variable. The strings
   --  have the format "name=value".

   procedure Variable_Editor_Set
     (Tree_Store  : access Gtk_Tree_Store_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Default  : Boolean;
      Value       : String;
      Is_Editable : Boolean := False);
   --  Convenient function to populate the tree.

   procedure New_Variable (Editor : access Gtk_Widget_Record'Class);
   procedure Rename_Variable (Editor : access Gtk_Widget_Record'Class);
   procedure Delete_Variable (Editor : access Gtk_Widget_Record'Class);
   --  Callback for the buttons at the bottom of the possible values list

   procedure Edited_Callback
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "edited" signal

   Default_Value_Column : constant := 0;
   Value_Column         : constant := 1;
   Editable_Column      : constant := 2;
   Initial_Value_Column : constant := 3;
   Column_Types : constant GType_Array :=
     (Default_Value_Column => GType_Boolean,
      Value_Column         => GType_String,
      Editable_Column      => GType_Boolean,
      Initial_Value_Column => GType_String);

   -------------------------
   -- Variable_Editor_Set --
   -------------------------

   procedure Variable_Editor_Set
     (Tree_Store  : access Gtk_Tree_Store_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Default  : Boolean;
      Value       : String;
      Is_Editable : Boolean := False)
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1 : Gint; Value1 : Gint;
         Col2 : Gint; Value2 : String;
         Col3 : Gint; Value3 : Gint;
         Col4 : Gint; Value4 : String;
         Final : Gint := -1);
      pragma Import (C, Internal, "gtk_tree_store_set");
   begin
      Internal
        (Get_Object (Tree_Store), Iter'Address,
         Default_Value_Column,  Boolean'Pos (Is_Default),
         Value_Column,          Value & ASCII.NUL,
         Editable_Column,       Boolean'Pos (Is_Editable),
         Initial_Value_Column,  Value & ASCII.NUL);
   end Variable_Editor_Set;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Var    : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node)
   is
      Item          : Gtk_List_Item;
      Index         : Natural := 0;
      S             : chars_ptr;
      Col           : Gtk_Tree_View_Column;
      Col_Number    : Gint;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;
      Button        : Gtk_Widget;
      Iter          : Gtk_Tree_Iter;
      E             : String_List_Iterator;
      Expr          : Project_Node_Id;
      Is_Default    : Boolean;
   begin
      Editor := new New_Var_Edit_Record;
      Editor.Var := Var;
      Editor.Kernel := Kernel_Handle (Kernel);
      New_Variable_Editor_Pkg.Initialize (Editor);
      Set_Transient_For (Editor, Get_Main_Window (Kernel));

      Set_Mode (Get_Selection (Editor.Values_List), Selection_Single);

      --  Create the model that contains the data to show in the tree

      Gtk_New (Editor.Model, Column_Types'Length, Column_Types);
      Set_Model (Editor.Values_List, Gtk_Tree_Model (Editor.Model));

      --  Create the cell renderers that are needed to display the tree view

      Gtk_New (Editor.Editable_Renderer);

      Gtk_New (Toggle_Renderer);
      Set_Radio_And_Callback
        (Editor.Model, Toggle_Renderer, Default_Value_Column);

      --  Add the columns to the tree view, and associated them with the
      --  appropriate cell renderers.

      Gtk_New (Col);
      Col_Number := Append_Column (Editor.Values_List, Col);
      Set_Title (Col, -"Default");
      Pack_Start (Col, Toggle_Renderer, False);

      Add_Attribute (Col, Toggle_Renderer, "active", Default_Value_Column);

      Gtk_New (Col);
      Col_Number := Append_Column (Editor.Values_List, Col);
      Set_Title (Col, -"Value");
      Pack_Start (Col, Editor.Editable_Renderer, True);
      Add_Attribute (Col, Editor.Editable_Renderer, "text", Value_Column);
      Add_Attribute
        (Col, Editor.Editable_Renderer, "editable", Editable_Column);

      Set_Editable_And_Callback
        (Editor.Model, Editor.Editable_Renderer, Value_Column);
      Widget_Callback.Object_Connect
        (Editor.Editable_Renderer, "edited", Edited_Callback'Access, Editor);

      --  Add the dialog buttons at the bottom. This is done so that Run can be
      --  called on the dialog.

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Widget_Callback.Object_Connect
        (Editor.New_Variable, "clicked",
         Widget_Callback.To_Marshaller (New_Variable'Access), Editor);
      Widget_Callback.Object_Connect
        (Editor.Rename_Variable, "clicked",
         Widget_Callback.To_Marshaller (Rename_Variable'Access), Editor);
      Widget_Callback.Object_Connect
        (Editor.Delete_Variable, "clicked",
         Widget_Callback.To_Marshaller (Delete_Variable'Access), Editor);

      --  Fill the list of existing environment variables before we put the
      --  currently referenced variable (in case it doesn't represent an
      --  existing one.

      loop
         S := Get_Nth_Environment (Index);
         exit when S = Null_Ptr;
         declare
            S2 : constant String := Value (S);
            J : Natural := S2'First;
         begin
            while J <= S2'Last and then S2 (J) /= '=' loop
               J := J + 1;
            end loop;
            Gtk_New (Item, S2 (S2'First .. J - 1));
            Show (Item);
            Add (Get_List (Editor.Variable_Name), Item);
            Index := Index + 1;
         end;
      end loop;

      --  Fill the information for the variable

      if Var /= Empty_Node then
         Set_Text (Get_Entry (Editor.Variable_Name),
                   Get_String (Get_Environment (Var)));

         Expr := External_Default_Of
           (Current_Term (First_Term (Expression_Of (Var))));
         if Expr /= Empty_Node
           and then Kind_Of (Expr) /= N_Literal_String
         then
            Expr := Empty_Node;
         end if;

         Is_Default := True;

         E := Type_Values (Var);
         while not Done (E) loop
            Append (Editor.Model, Iter, Null_Iter);

            if Expr /= Empty_Node then
               Is_Default := String_Equal
                 (String_Value_Of (Data (E)), String_Value_Of (Expr));
            end if;

            Variable_Editor_Set
              (Editor.Model, Iter,
               Is_Default => Is_Default,
               Value => Get_String (String_Value_Of (Data (E))),
               Is_Editable => False);

            Is_Default := False;
            E := Next (E);
         end loop;
      end if;
   end Gtk_New;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      M : Gtk_Tree_Store := Gtk_Tree_Store (New_Var_Edit (Editor).Model);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : Gtk_Tree_Iter;
   begin
      --  Once a cell has been edited, it is no longer editable, until the user
      --  presses the Rename button.
      Iter := Get_Iter_From_String (M, Path_String);
      Set (M, Iter, Editable_Column, False);
   end Edited_Callback;

   ------------------
   -- New_Variable --
   ------------------

   procedure New_Variable (Editor : access Gtk_Widget_Record'Class) is
      E    : New_Var_Edit := New_Var_Edit (Editor);
      Iter : Gtk_Tree_Iter;
   begin
      --  Add a new entry. This will become the default value if this is also
      --  the first one in the list.
      Iter := Get_Iter_Root (E.Model);
      Append (E.Model, Iter, Null_Iter);
      Variable_Editor_Set
        (E.Model, Iter,
         Is_Default  => Iter = Null_Iter,
         Value       => New_Value_Name,
         Is_Editable => True);
   end New_Variable;

   ---------------------
   -- Rename_Variable --
   ---------------------

   procedure Rename_Variable (Editor : access Gtk_Widget_Record'Class) is
      E           : New_Var_Edit := New_Var_Edit (Editor);
      Iter        : Gtk_Tree_Iter := Null_Iter;
      Success     : Boolean;
      Selection   : Gtk_Tree_Selection := Get_Selection (E.Values_List);
   begin
      Success := Get_Selected (Selection, E.Model, Iter);
      if Success then
         Set (E.Model, Iter, Editable_Column, True);
         Set_Cursor
           (E.Values_List,
            Path => Get_Path (E.Model, Iter),
            Focus_Column => Get_Column (E.Values_List, Value_Column),
            Start_Editing => True);
      end if;
   end Rename_Variable;

   ---------------------
   -- Delete_Variable --
   ---------------------

   procedure Delete_Variable (Editor : access Gtk_Widget_Record'Class) is
      E           : New_Var_Edit := New_Var_Edit (Editor);
      Iter        : Gtk_Tree_Iter := Null_Iter;
      Success     : Boolean;
      Selection   : Gtk_Tree_Selection := Get_Selection (E.Values_List);
   begin
      Success := Get_Selected (Selection, E.Model, Iter);
      if Success then
         Remove (E.Model, Iter);
      end if;
   end Delete_Variable;

   ---------------------
   -- Update_Variable --
   ---------------------

   function Update_Variable (Editor : access New_Var_Edit_Record)
     return Boolean
   is
      New_Name : constant String :=
        Get_Text (Get_Entry (Editor.Variable_Name));
      Var : Project_Node_Id := Editor.Var;
      Changed  : Boolean := False;
      Iter     : Gtk_Tree_Iter;
      Val_Iter : String_List_Iterator;
      Found    : Boolean;
      Num_Rows : Natural := 0;
      Message  : Message_Dialog_Buttons;
   begin
      if New_Name = "" then
         Message := Message_Dialog
           (Msg     => -"You must specify a name for the variable",
            Buttons => Button_OK,
            Parent  => Gtk_Window (Editor));
         return False;
      end if;

      Iter := Get_Iter_Root (Editor.Model);
      while Iter /= Null_Iter loop
         if Get_String (Editor.Model, Iter, Value_Column) /=
           New_Value_Name
         then
            Num_Rows := Num_Rows + 1;
         end if;
         Next (Editor.Model, Iter);
      end loop;

      if Num_Rows = 0 then
         Message := Message_Dialog
           (Msg     => -"You must specify some possible values",
            Buttons => Button_OK,
            Parent  => Gtk_Window (Editor));
         return False;
      end if;

      if Get_String (Get_Environment (Var)) /= New_Name then
         declare
            Vars : constant Project_Node_Array :=
              Scenario_Variables (Editor.Kernel);
         begin
            for V in Vars'Range loop
               if New_Name = Get_String (Get_Environment (Vars (V))) then
                  Message := Message_Dialog
                    (Msg     => -"There is already a variable with this name",
                     Buttons => Button_OK,
                     Parent  => Gtk_Window (Editor));
                  return False;
               end if;
            end loop;
         end;
      end if;

      Normalize (Get_Project (Editor.Kernel), Recurse => True);

      --  Create the variable if necessary

      if Var = Empty_Node then
         Var := Create_Type (Get_Project (Editor.Kernel), New_Name & "_Type");
         Var := Create_Typed_Variable
           (Get_Project (Editor.Kernel), New_Name, Var,
            Add_Before_First_Case_Or_Pkg => True);
         Set_Value_As_External (Var, New_Name);
      end if;

      --  Rename the value appropriately (this has to be done separately, so
      --  that the case statements are changed appropriately).

      if Editor.Var /= Empty_Node then
         Iter := Get_Iter_Root (Editor.Model);
         while Iter /= Null_Iter loop
            Num_Rows := Num_Rows + 1;

            if Editor.Var /= Empty_Node then
               declare
                  Old_Val : constant String :=
                    Get_String (Editor.Model, Iter, Initial_Value_Column);
                  New_Val : constant String :=
                    Get_String (Editor.Model, Iter, Value_Column);
               begin
                  if Old_Val /= New_Val then
                     Trace (Me, "Renaming value for variable "
                            & Get_String (Get_Environment (Var))
                            & " from " & Old_Val & " to " & New_Val);

                     Start_String;
                     Store_String_Chars (New_Val);
                     Rename_Value_For_External_Variable
                       (Get_Project (Editor.Kernel),
                        Ext_Variable_Name =>
                          Get_String (Get_Environment (Var)),
                        Old_Value_Name => Old_Val,
                        New_Value_Name => End_String);
                     Changed := True;
                  end if;
               end;
            end if;

            Next (Editor.Model, Iter);
         end loop;

         --  Delete the values that no longer exist

         Val_Iter := Type_Values (Var);
         while not Done (Val_Iter) loop
            Found := False;

            Iter := Get_Iter_Root (Editor.Model);
            while Iter /= Null_Iter loop
               if Get_String (Editor.Model, Iter, Value_Column) =
                 Get_String (String_Value_Of (Data (Val_Iter)))
               then
                  Found := True;
                  exit;
               end if;
               Next (Editor.Model, Iter);
            end loop;

            if not Found then
               Remove_Value (Get_Project (Editor.Kernel),
                             Get_String (Get_Environment (Var)),
                             Get_String (String_Value_Of (Data (Val_Iter))));
               Changed := True;
            end if;

            Val_Iter := Next (Val_Iter);
         end loop;
      end if;

      --  Add the new values

      declare
         Values : String_Id_Array (1 .. Num_Rows);
            Num_Values : Natural := Values'First - 1;
      begin
         Iter := Get_Iter_Root (Editor.Model);
         while Iter /= Null_Iter loop
            Found := False;

            declare
               Name : constant String := Get_String
                 (Editor.Model, Iter, Value_Column);
               Default : Boolean := Boolean_Get
                 (Editor.Model, Iter, Default_Value_Column);
               Id : String_Id := No_String;
            begin
               if Name /= New_Value_Name then
                  Val_Iter := Type_Values (Var);
                  while not Done (Val_Iter) loop
                     if Name =
                       Get_String (String_Value_Of (Data (Val_Iter)))
                     then
                        Found := True;
                        exit;
                     end if;

                     Val_Iter := Next (Val_Iter);
                  end loop;

                  if not Found then
                     Num_Values := Num_Values + 1;
                     Start_String;
                     Store_String_Chars (Name);
                     Id := End_String;
                     Values (Num_Values) := Id;
                     Trace (Me, "Adding new value " & Name & " for "
                            & Get_String (Get_Environment (Var)));
                  end if;

                  if Default then
                     if Id = No_String then
                        Start_String;
                        Store_String_Chars (Name);
                        Id := End_String;
                     end if;

                     Trace (Me, "Setting default value of "
                            & Get_String (Get_Environment (Var))
                            & " to " & Name);
                     Set_Default_Value_For_External_Variable
                       (Get_Project (Editor.Kernel),
                        Get_String (Get_Environment (Var)),
                        Id);
                  end if;
               end if;
            end;

            Next (Editor.Model, Iter);
         end loop;

         if Num_Values >= Values'First then
            Add_Scenario_Variable_Values
              (Get_Project (Editor.Kernel),
               Get_Environment (Var),
               Values (Values'First .. Num_Values));
            Changed := True;
         end if;
      end;

      --  Has the variable been renamed ? This should be done last, so that
      --  the previous calls above work with the old name

      if Get_String (Get_Environment (Var)) /= New_Name then
         Trace (Me, "Renaming variable "
                & Get_String (Get_Environment (Var))
                & " to " & New_Name);
         Start_String;
         Store_String_Chars (New_Name);
         Rename_External_Variable
           (Get_Project (Editor.Kernel),
            Old_Name => Get_String (Get_Environment (Var)),
            New_Name => End_String);
         Changed := True;
      end if;

      if Editor.Var = Empty_Node then
         if External_Default (Var) /= Empty_Node then
            Prj.Ext.Add
              (New_Name,
               Get_String (String_Value_Of (External_Default (Var))));
         else
            Iter := Get_Iter_Root (Editor.Model);
            Prj.Ext.Add
              (New_Name, Get_String (Editor.Model, Iter, Value_Column));
         end if;
      end if;

      if Changed then
         Variable_Changed (Editor.Kernel);
      end if;
      return True;
   end Update_Variable;

   ---------------------
   -- On_Add_Variable --
   ---------------------

   procedure On_Add_Variable
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Get_Kernel (Context));
      Show_All (Edit);
      while Run (Edit) = Gtk_Response_OK
        and then not Update_Variable (Edit)
      loop
         null;
      end loop;
      Destroy (Edit);
   end On_Add_Variable;

end Variable_Editors;

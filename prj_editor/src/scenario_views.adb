------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with System.Assertions;

with Glib;                use Glib;
with Glib.Convert;        use Glib.Convert;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Values;         use Glib.Values;
with Gtk.Box;             use Gtk.Box;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.List_Store;      use Gtk.List_Store;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Toolbar;         use Gtk.Toolbar;
with Gtk.Tool_Button;     use Gtk.Tool_Button;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with Gtk.Tree_Selection;  use Gtk.Tree_Selection;
with Gtk.Tree_Store;      use Gtk.Tree_Store;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.MDI;          use Gtkada.MDI;

with Projects;              use Projects;
with Generic_Views;
with GNAT.Case_Util;        use GNAT.Case_Util;
with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GPS.Kernel;            use GPS.Kernel;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GPS.Kernel.Modules;    use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI; use GPS.Kernel.Modules.UI;
with GPS.Kernel.Hooks;      use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;    use GPS.Kernel.Project;
with Project_Viewers;       use Project_Viewers;
with Variable_Editors;      use Variable_Editors;
with GPS.Intl;              use GPS.Intl;
with Traces;                use Traces;

package body Scenario_Views is

   Me : constant Debug_Handle := Create ("Scenario_Views");

   type Scenario_View_Record is new Generic_Views.View_Record with record
      View          : Gtk.Tree_View.Gtk_Tree_View;
      Kernel        : GPS.Kernel.Kernel_Handle;
   end record;
   overriding procedure Create_Toolbar
     (View    : not null access Scenario_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);

   function Initialize
     (View    : access Scenario_View_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk_Widget;
   --  Create a new scenario view associated with Manager.
   --  The view is automatically refreshed every time the project view in
   --  the manager changes.
   --  Returns the focus widget in the view.

   package Scenario_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Scenario_View",
      View_Name          => -"Environment",
      Formal_View_Record => Scenario_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => True,
      Local_Config       => False,
      Position           => Position_Left);
   use Scenario_Views;
   subtype Scenario_View is Scenario_Views.View_Access;

   function Add_Possible_Values
     (Kernel : access Kernel_Handle_Record'Class;
      Var    : Scenario_Variable)
     return Gtk_List_Store;
   --  Returns a model with the list of possible values for Var

   function Selected_Variable
     (View : access Scenario_View_Record'Class)
      return Scenario_Variable;
   --  Returns the currently selected variable

   procedure Select_Value
     (Combo : access Gtk_Combo_Box_Text_Record'Class;
      Value : String);
   pragma Unreferenced (Select_Value);
   --  Selects value in Combo, if present

   procedure Variable_Value_Changed
     (View     : access GObject_Record'Class;
      Path     : UTF8_String;
      New_Iter : Gtk_Tree_Iter);
   --  Called when the value of one of the variables has changed.
   --  This recomputes the scenario view, so that changes are reflected in
   --  other parts of GPS.

   procedure Edit_Variable (View : access GObject_Record'Class);
   --  Called when editing a variable (name and possible values)

   procedure Delete_Variable (View : access GObject_Record'Class);
   --  Called when removing a variable

   type Refresh_Hook_Record is new Function_No_Args with record
      View : Scenario_View;
   end record;
   type Refresh_Hook is access all Refresh_Hook_Record'Class;
   overriding procedure Execute
     (Hook : Refresh_Hook_Record; Kernel : access Kernel_Handle_Record'Class);
   --  Callback when some aspect of the project has changed, to refresh the
   --  view.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View : constant Scenario_View := Scenario_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.View, Fixed_Font => False);
      end if;
   end On_Preferences_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View    : access Scenario_View_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      Hook     : Refresh_Hook;
      Scrolled : Gtk_Scrolled_Window;
      Model    : Gtk_Tree_Store;
      Col      : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;
      Combo    : Gtk_Cell_Renderer_Combo;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);
   begin
      View.Kernel := Kernel_Handle (Kernel);

      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Shadow_Type (Shadow_None);
      View.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New
        (Model,
         (0 => GType_String,   --  Name of the category or variable
          1 => GType_String,   --  Current value
          2 => Gtk.Tree_Model.Get_Type, --  The valid choices for the value
          3 => GType_Boolean)); --  Whether the value is editable
      Gtk_New (View.View, Model);
      View.View.Set_Headers_Visible (False);
      Scrolled.Add (View.View);
      Unref (Model);

      Gtk_New (Col);
      Col.Set_Reorderable (True);
      Col.Set_Resizable (True);
      Col.Set_Clickable (True);
      Col.Set_Sort_Column_Id (0);
      Col_Number := View.View.Append_Column (Col);
      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Add_Attribute (Text, "text", 0);
      Col.Clicked;   --  Ensure sorting

      Gtk_New (Col);
      Col.Set_Reorderable (True);
      Col.Set_Resizable (True);
      Col.Set_Sort_Column_Id (1);
      Col_Number := View.View.Append_Column (Col);
      Gtk_New (Combo);
      Col.Pack_Start (Combo, True);
      Set_Property (Combo, Text_Column_Property, 0);  --  in combo's model
      Col.Add_Attribute (Combo, "text", 1);
      Col.Add_Attribute (Combo, "model", 2);
      Col.Add_Attribute (Combo, "editable", 3);
      Set_Property (Combo, Has_Entry_Property, False);

      Combo.On_Changed (Variable_Value_Changed'Access, View);

      --  We do not need to connect to "project_changed", since it is always
      --  emitted at the same time as a "project_view_changed", and we do the
      --  same thing in both cases.
      Hook := new Refresh_Hook_Record'
        (Function_No_Args with View => Scenario_View (View));

      Add_Hook
        (Kernel, Project_View_Changed_Hook, Hook,
         Name => "scenario.project_view_changed",
         Watch => GObject (View));
      Add_Hook (Kernel, Variable_Changed_Hook, Hook,
                Name => "scenario.variable_changed", Watch => GObject (View));
      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (On_Preferences_Changed'Access),
                Name  => "scenario_views.preferences_changed",
                Watch => GObject (View));
      Set_Font_And_Colors (View.View, Fixed_Font => False);

      --  Update the viewer with the current project
      Execute (Hook.all, Kernel);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.View,
         Object          => View,
         ID              => Scenario_Views.Get_Module,
         Context_Func    => null);

      return Gtk_Widget (View);
   end Initialize;

   -----------------------
   -- Selected_Variable --
   -----------------------

   function Selected_Variable
     (View : access Scenario_View_Record'Class)
      return Scenario_Variable
   is
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store'(-View.View.Get_Model);
      Selection : constant Gtk_Tree_Selection := View.View.Get_Selection;
      M    : Gtk_Tree_Model;
      Iter : Gtk_Tree_Iter;

   begin
      Selection.Get_Selected (M, Iter);
      if Iter /= Null_Iter then
         declare
            Variable : constant String := Model.Get_String (Iter, 0);
         begin
            return Get_Project_Tree
              (View.Kernel).Scenario_Variables (Variable);
         end;
      else
         return No_Variable;
      end if;
   end Selected_Variable;

   ----------------------------
   -- Variable_Value_Changed --
   ----------------------------

   procedure Variable_Value_Changed
     (View     : access GObject_Record'Class;
      Path     : UTF8_String;
      New_Iter : Gtk_Tree_Iter)
   is
      V : constant Scenario_View := Scenario_View (View);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store'(-V.View.Get_Model);
      Iter  : constant Gtk_Tree_Iter := Model.Get_Iter_From_String (Path);
      Variable : constant String := Model.Get_String (Iter, 0);

      Val  : GValue;
      List : Gtk_List_Store;
   begin
      Model.Get_Value (Iter, 2, Val);
      List := Gtk_List_Store (Get_Object (Val));
      Unset (Val);

      declare
         Value : constant String := List.Get_String (New_Iter, 0);
         Var   : Scenario_Variable :=
           Get_Project_Tree (V.Kernel).Scenario_Variables (Variable);
      begin
         Trace (Me, "Set value of '" & Variable & "' to '"
                & Value & "'");
         Set_Value (Var, Value);
         Get_Registry (V.Kernel).Tree.Change_Environment ((1 => Var));
         Recompute_View (V.Kernel);
      end;
   end Variable_Value_Changed;

   -------------------------
   -- Add_Possible_Values --
   -------------------------

   function Add_Possible_Values
     (Kernel : access Kernel_Handle_Record'Class;
      Var    : Scenario_Variable)
      return Gtk_List_Store
   is
      List : Gtk_List_Store;
      Iter : Gtk_Tree_Iter;
   begin
      Gtk_New (List, (0 => GType_String));

      declare
         Values : GNAT.Strings.String_List :=
           Get_Registry (Kernel).Tree.Possible_Values_Of (Var);
      begin
         for Val in Values'Range loop
            List.Append (Iter);
            List.Set (Iter, 0, Locale_To_UTF8 (Values (Val).all));
         end loop;

         Free (Values);
      end;

      return List;

   exception
      when System.Assertions.Assert_Failure =>
         Trace
           (Exception_Handle,
            "Scenario variable not found: " & External_Name (Var));
         return null;
   end Add_Possible_Values;

   ------------------
   -- Select_Value --
   ------------------

   procedure Select_Value
     (Combo : access Gtk_Combo_Box_Text_Record'Class;
      Value : String)
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model renames Combo.Get_Model;
   begin
      Iter := Get_Iter_First (Model);

      while Iter /= Null_Iter loop
         if Get_String (Model, Iter, 0) = Value then
            Combo.Set_Active_Iter (Iter);

            return;
         end if;

         Next (Model, Iter);
      end loop;
   end Select_Value;

   -------------------
   -- Edit_Variable --
   -------------------

   procedure Edit_Variable (View : access GObject_Record'Class) is
      V : constant Scenario_View := Scenario_View (View);
      Variable : constant Scenario_Variable := Selected_Variable (V);

      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, V.Kernel, Variable, -"Editing a variable");
      Show_All (Edit);
      while Run (Edit) = Gtk_Response_OK
        and then not Update_Variable (Edit)
      loop
         null;
      end loop;
      Destroy (Edit);
   end Edit_Variable;

   ---------------------
   -- Delete_Variable --
   ---------------------

   procedure Delete_Variable (View : access GObject_Record'Class) is
      V : constant Scenario_View := Scenario_View (View);
      Var : constant Scenario_Variable := Selected_Variable (V);

      Message : constant String :=
        "Doing so will remove all the configurations associated with"
        & ASCII.LF
        & "that variable, except for the currently selected value";

      Response : constant Message_Dialog_Buttons := Message_Dialog
        (Msg           => (-"Are you sure you want to remove the variable ")
           & '"' & External_Name (Var)
           & """?" & ASCII.LF & (-Message),
         Dialog_Type   => Confirmation,
         Buttons       => Button_OK or Button_Cancel,
         Title         => -"Deleting a variable",
         Justification => Justify_Left,
         Parent        => Get_Current_Window (V.Kernel));
   begin
      if Response = Button_OK then
         Get_Registry (V.Kernel).Tree.Delete_Scenario_Variable
           (External_Name            => External_Name (Var),
            Keep_Choice              => Value (Var),
            Delete_Direct_References => False);
         Run_Hook (V.Kernel, Variable_Changed_Hook);

         --  Recompute the view so that the explorer is updated graphically
         Recompute_View (V.Kernel);

         Trace (Me, "Delete_Variable: " & External_Name (Var));
      end if;
   end Delete_Variable;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Scenario_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
      Button : Gtk_Tool_Button;
   begin
      Add_Button
        (View.Kernel,
         Toolbar  => Toolbar,
         Stock_Id => Stock_Add,
         Action   => Action_Add_Scenario_Variable,
         Tooltip  => -"Add new scenario variable");

      Gtk_New_From_Stock (Button, Stock_Edit);
      Button.Set_Tooltip_Text (-"Edit properties of selected variable");
      Toolbar.Insert (Button);
      Button.On_Clicked (Edit_Variable'Access, View);

      Gtk_New_From_Stock (Button, Stock_Remove);
      Button.Set_Tooltip_Text (-"Delete the selected variable");
      Toolbar.Insert (Button);
      Button.On_Clicked (Delete_Variable'Access, View);
   end Create_Toolbar;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook : Refresh_Hook_Record; Kernel : access Kernel_Handle_Record'Class)
   is
      V      : constant Scenario_View := Hook.View;
      Row    : Guint;
      pragma Unreferenced (Row);
      Iter   : Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter;
      Model  : constant Gtk_Tree_Store := Gtk_Tree_Store'(-V.View.Get_Model);
   begin
      --  There is a small problem here: Refresh might be called while one of
      --  the combo boxes is still displayed. Thus, if we destroy it now, any
      --  pending signal on the combo box (like hiding the popup window) will
      --  generate a segmentation fault.
      --  This also saves some refreshing when the values would be reflected
      --  automatically anyway.

      Model.Clear;

      --  No project => Clean up the scenario viewer
      if Get_Project (Kernel) = No_Project then
         Model.Append (Parent, Null_Iter);
         Model.Set (Parent, 0, "No project is loaded");
         Model.Set (Parent, 3, False);  --  not editable

      else
         declare
            Scenar_Var : constant Scenario_Variable_Array :=
                           Scenario_Variables (Kernel);
            Val : GValue;
            Dummy : Boolean;
            P : Gtk_Tree_Path;
            pragma Unreferenced (Dummy);
         begin
            Model.Append (Parent, Null_Iter);
            Model.Set (Parent, 0, "Scenario Variables");
            Model.Set (Parent, 3, False);  --  not editable

            if Scenar_Var'Length /= 0 then
               for J in Scenar_Var'Range loop
                  Model.Append (Iter, Parent);

                  Row := Guint (J - Scenar_Var'First) + 1;

                  declare
                     Name : String := External_Name (Scenar_Var (J));
                  begin
                     To_Mixed (Name);

                     Model.Set (Iter, 0, Name);
                     Model.Set (Iter, 1, Value (Scenar_Var (J)));
                     Model.Set (Iter, 3, True);  --  editable

                     Init (Val, Gtk.List_Store.Get_Type);
                     Set_Object
                       (Val, Add_Possible_Values (Kernel, Scenar_Var (J)));
                     Model.Set_Value (Iter, 2, Val);
                     Unset (Val);
                  end;

                  P := Model.Get_Path (Parent);
                  Dummy := V.View.Expand_Row (Path => P, Open_All => False);
                  Path_Free (P);
               end loop;
            end if;
         end;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Scenario_Views.Register_Module
        (Kernel, Menu_Name => -"Views/_Scenario");
   end Register_Module;

end Scenario_Views;

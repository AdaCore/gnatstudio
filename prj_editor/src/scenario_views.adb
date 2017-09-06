------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Assertions;

with Glib;                     use Glib;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Combo;  use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Menu;
with Gtk.Check_Menu_Item;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;

with Commands.Interactive;      use Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Projects;                  use Projects;
with Generic_Views;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with Variable_Editors;          use Variable_Editors;
with GPS.Intl;                  use GPS.Intl;
with XML_Utils;                 use XML_Utils;

package body Scenario_Views is

   Me : constant Trace_Handle := Create ("Scenario_Views");

   type Scenario_View_Module_Record is new Module_ID_Record with record
      Modes : Gtk_List_Store;
      --  The list of registered build modes

      Modes_Help : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Scenario_View_Module is access all Scenario_View_Module_Record'Class;
   overriding procedure Destroy (Self : in out Scenario_View_Module_Record);
   overriding procedure Customize
     (Module : access Scenario_View_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   type Scenario_View_Record is new Generic_Views.View_Record with record
      View          : Gtk.Tree_View.Gtk_Tree_View;
      Scenario_Node : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Build_Node    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
   end record;

   overriding procedure Create_Menu
     (View    : not null access Scenario_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   function Initialize
     (View    : access Scenario_View_Record'Class)
      return Gtk_Widget;
   --  Create a new scenario view associated with Manager.
   --  The view is automatically refreshed every time the project view in
   --  the manager changes.
   --  Returns the focus widget in the view.

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Free memory used by Scenario_View_Record

   package Scenario_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Scenario_View",
      View_Name          => -"Scenario",
      Formal_View_Record => Scenario_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
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

   procedure Variable_Value_Changed
     (View     : access GObject_Record'Class;
      Path     : Glib.UTF8_String;
      New_Iter : Gtk_Tree_Iter);
   --  Called when the value of one of the variables has changed.
   --  This recomputes the scenario view, so that changes are reflected in
   --  other parts of GPS.

   type On_Refresh is new Simple_Hooks_Function with record
      View : access Scenario_View_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Callback when some aspect of the project has changed, to refresh the
   --  view.

   procedure On_Force_Refresh (View : access GObject_Record'Class);
   --  Force a refresh of the view when some settings have changed.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type On_Build_Mode_Changed is new String_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Build_Mode_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : String);
   --  Called when a new build mode is selected

   Show_Build_Modes : Boolean_Preference;

   type Command_Edit_Variable is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Command_Edit_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Edit the selected variable

   type Command_Delete_Variable is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Command_Delete_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Deleted selected variable

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Scenario_View_Module_Record) is
   begin
      Unref (Self.Modes);
   end Destroy;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      View : constant Scenario_View := Scenario_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.View, Fixed_Font => False, Pref => Pref);

         if Pref = null
           or else Pref = Preference (Show_Build_Modes)
         then
            On_Force_Refresh (View);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Build_Mode_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : String)
   is
      pragma Unreferenced (Self);
      View  : constant Scenario_View := Scenario_Views.Retrieve_View (Kernel);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store'(-View.View.Get_Model);
      Build : Gtk_Tree_Iter;
   begin
      if View.Build_Node /= Null_Gtk_Tree_Path then
         Build := Model.Get_Iter (View.Build_Node);
         Model.Set (Build, 1, Mode);
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View    : access Scenario_View_Record'Class)
      return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
      Model    : Gtk_Tree_Store;
      Col      : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;
      Combo    : Gtk_Cell_Renderer_Combo;
      Col_Number : Gint;
      Pixbuf   : Gtk_Cell_Renderer_Pixbuf;
      pragma Unreferenced (Col_Number);
   begin
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
          3 => GType_Boolean,  --  Whether the value is editable
          4 => GType_String)); --  The tooltip
      Gtk_New (View.View, Model);
      View.View.Set_Headers_Visible (False);
      Scrolled.Add (View.View);
      Unref (Model);

      View.View.Set_Tooltip_Column (4);

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

      Gtk_New (Pixbuf);
      Col.Pack_Start (Pixbuf, False);
      Set_Property (Pixbuf, Icon_Name_Property, "gps-double-arrow-symbolic");
      Col.Add_Attribute (Pixbuf, "visible", 3);

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
      Project_View_Changed_Hook.Add
         (new On_Refresh'(Simple_Hooks_Function with View => View),
          Watch => View);
      Variable_Changed_Hook.Add
         (new On_Refresh'(Simple_Hooks_Function with View => View),
          Watch => View);
      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);
      Build_Mode_Changed_Hook.Add (new On_Build_Mode_Changed, Watch => View);

      Set_Font_And_Colors (View.View, Fixed_Font => False);

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);

      --  Update the viewer with the current project
      On_Force_Refresh (View);

      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View.View);

      return Gtk_Widget (View.View);
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
      if Iter /= Null_Iter
        and then Model.Parent (Iter) = Model.Get_Iter (View.Scenario_Node)
      then
         declare
            Variable : constant String := Model.Get_String (Iter, 0);
         begin
            return Get_Project_Tree
              (View.Kernel).Scenario_Variables (Variable);
         end;
      elsif Iter /= Null_Iter then
         return No_Variable;
      else
         return No_Variable;
      end if;
   end Selected_Variable;

   ----------------------------
   -- Variable_Value_Changed --
   ----------------------------

   procedure Variable_Value_Changed
     (View     : access GObject_Record'Class;
      Path     : Glib.UTF8_String;
      New_Iter : Gtk_Tree_Iter)
   is
      V : constant Scenario_View := Scenario_View (View);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store'(-V.View.Get_Model);
      Iter  : constant Gtk_Tree_Iter := Model.Get_Iter_From_String (Path);

      Val  : GValue;
      List : Gtk_List_Store;
   begin
      Model.Get_Value (Iter, 2, Val);
      List := Gtk_List_Store (Get_Object (Val));
      Unset (Val);

      --  Have we changed a scenario variable ?

      if V.Scenario_Node = Null_Gtk_Tree_Path
      --  If the Scenario_Node is null, we have necessarily changed a
      --  scenario variable because that means the build mode is not shown

        or else (Model.Parent (Iter) = Model.Get_Iter (V.Scenario_Node))
      then
         declare
            Value : constant String := List.Get_String (New_Iter, 0);
            Variable : constant String := Model.Get_String (Iter, 0);
            Var   : Scenario_Variable :=
              Get_Project_Tree (V.Kernel).Scenario_Variables (Variable);
         begin
            Trace (Me, "Set value of '" & Variable & "' to '"
                   & Value & "'");
            Set_Value (Var, Value);
            Get_Registry (V.Kernel).Tree.Change_Environment ((1 => Var));
            Recompute_View (V.Kernel);
         end;

      else
         --  The build mode
         V.Kernel.Set_Build_Mode (New_Mode => List.Get_String (New_Iter, 0));
      end if;
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
           (Me,
            "Scenario variable not found: " & External_Name (Var));
         return null;
   end Add_Possible_Values;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Command_Edit_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
      V : constant Scenario_View := Scenario_Views.Retrieve_View (K);
      Variable : constant Scenario_Variable := Selected_Variable (V);
      Edit : New_Var_Edit;
   begin
      if Variable /= No_Variable then
         Gtk_New (Edit, V.Kernel, Variable, -"Editing a variable");
         Show_All (Edit);
         while Run (Edit) = Gtk_Response_OK
           and then not Update_Variable (Edit)
         loop
            null;
         end loop;
         Destroy (Edit);
      else
         Trace (Me, "No selected variable");
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Command_Delete_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K   : constant Kernel_Handle := Get_Kernel (Context.Context);
      V   : constant Scenario_View := Scenario_Views.Retrieve_View (K);
      Var : constant Scenario_Variable := Selected_Variable (V);

      Message : constant String :=
        "Doing so will remove all the configurations associated with"
        & ASCII.LF
        & "that variable, except for the currently selected value";

      Response : Message_Dialog_Buttons;
   begin
      if Var /= No_Variable then
         Response := Message_Dialog
           (Msg           => (-"Are you sure you want to remove the variable ")
            & '"' & External_Name (Var)
            & """?" & ASCII.LF & (-Message),
            Dialog_Type   => Confirmation,
            Buttons       => Button_OK or Button_Cancel,
            Title         => -"Deleting a variable",
            Justification => Justify_Left,
            Parent        => Get_Current_Window (V.Kernel));

         if Response = Button_OK then
            Get_Registry (V.Kernel).Tree.Delete_Scenario_Variable
              (External_Name            => External_Name (Var),
               Keep_Choice              => Value (Var),
               Delete_Direct_References => False);
            Variable_Changed_Hook.Run (V.Kernel);

            --  Recompute the view so that the explorer is updated graphically
            Recompute_View (V.Kernel);

            Trace (Me, "Delete_Variable: " & External_Name (Var));
         end if;
      end if;
      return Commands.Success;
   end Execute;

   ----------------------
   -- On_Force_Refresh --
   ----------------------

   procedure On_Force_Refresh (View : access GObject_Record'Class) is
      V : constant Scenario_View := Scenario_View (View);
      H : aliased On_Refresh;
   begin
      H.View := V;
      H.Execute (V.Kernel);
   end On_Force_Refresh;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      V     : constant Scenario_View := Scenario_View (Widget);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store'(-V.View.Get_Model);
   begin
      Path_Free (V.Scenario_Node);
      Path_Free (V.Build_Node);
      Model.Clear;
   end On_Destroy;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Scenario_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      Append_Menu (Menu, View.Kernel, Show_Build_Modes);
   end Create_Menu;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      Module : constant Scenario_View_Module :=
        Scenario_View_Module (Scenario_Views.Get_Module);
      V      : constant Scenario_View := Scenario_View (Self.View);
      Row    : Guint;
      pragma Unreferenced (Row);
      Iter   : Gtk_Tree_Iter;
      Model  : constant Gtk_Tree_Store := Gtk_Tree_Store'(-V.View.Get_Model);

      Scenario   : Gtk_Tree_Iter;
      Show_Build : constant Boolean := Show_Build_Modes.Get_Pref;

   begin
      Trace (Me, "Recomputing list of scenario variables");

      --  There is a small problem here: Refresh might be called while one of
      --  the combo boxes is still displayed. Thus, if we destroy it now, any
      --  pending signal on the combo box (like hiding the popup window) will
      --  generate a segmentation fault.
      --  This also saves some refreshing when the values would be reflected
      --  automatically anyway.

      Path_Free (V.Scenario_Node);
      Path_Free (V.Build_Node);
      Model.Clear;

      if Show_Build then
         Model.Append (Iter, Null_Iter);
         V.Build_Node := Model.Get_Path (Iter);

         Set_And_Clear
           (Model, Iter,
            (0 => As_String     ("Build mode"),
             1 => As_String     (V.Kernel.Get_Build_Mode),
             2 => As_List_Store (Module.Modes),
             3 => As_Boolean    (True),  --  editable,
             4 => As_String     (To_String (Module.Modes_Help))));

         Model.Append (Iter, Null_Iter);
         V.Scenario_Node := Model.Get_Path (Iter);

         Set_And_Clear
           (Model, Iter,
            (0, 3),
            (0 => As_String  ("Scenario Variables"),
             1 => As_Boolean  (False)));

         Scenario := Iter;
         --  Remove_Child_Nodes (Model, Scenario);

      else
         V.Build_Node    := Null_Gtk_Tree_Path;
         V.Scenario_Node := Null_Gtk_Tree_Path;
         Scenario        := Null_Iter;
      end if;

      declare
         Scenar_Var : constant Scenario_Variable_Array :=
           Scenario_Variables (Kernel);
         Dummy : Boolean;
         pragma Unreferenced (Dummy);
      begin
         if Scenar_Var'Length /= 0 then
            for J in Scenar_Var'Range loop
               Model.Append (Iter, Scenario);

               Row := Guint (J - Scenar_Var'First) + 1;

               declare
                  Name : constant String := External_Name (Scenar_Var (J));
               begin
                  Set_And_Clear
                    (Model, Iter,
                     (0 => As_String  (Name),
                      1 => As_String (Value (Scenar_Var (J))),
                      2 => As_List_Store
                        (Add_Possible_Values (Kernel, Scenar_Var (J))),
                      3 => As_Boolean (True)));
               end;
            end loop;

            if Show_Build then
               Dummy := V.View.Expand_Row
                 (Path => V.Scenario_Node, Open_All => False);
            end if;
         end if;
      end;
   end Execute;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Scenario_View_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Iter       : Gtk_Tree_Iter;

   begin
      if Node.Tag.all = "builder-mode" then
         --  Create the mode and add it to the list of supported modes

         declare
            Name : constant String := Get_Attribute (Node, "name", "");
            Description : constant XML_Utils.String_Ptr :=
              Get_Field (Node, "description");
            Shadow : constant XML_Utils.String_Ptr :=
              Get_Field (Node, "shadow");
         begin
            if Name = "" then
               return;
            end if;

            --  Add the mode to the combo if it is not a shadow mode

            if Shadow = null or else not Boolean'Value (Shadow.all) then
               Module.Modes.Append (Iter);
               Module.Modes.Set (Iter, 0, Name);
            end if;

            if Description /= null then
               Append (Module.Modes_Help,
                       "<b>" & Name & "</b>: " & Description.all & ASCII.LF);
            end if;

         exception
            when E : Constraint_Error =>
               Trace (Me, E);
         end;
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class) is
      M : constant Scenario_View_Module := new Scenario_View_Module_Record;
   begin
      Gtk_New (M.Modes, (0 => GType_String));

      --  Make sure it will never be destroyed even if we close the view
      Ref (M.Modes);

      Scenario_Views.Register_Module
        (Kernel,
         ID        => Module_ID (M));

      Show_Build_Modes := Kernel.Get_Preferences.Create_Invisible_Pref
        ("scenario-show-build-modes", True,
         Label => -"Show build modes");

      Register_Action
        (Kernel, "Scenario edit variable",
         new Command_Edit_Variable,
         -"Edit properties of the selected variable",
         Icon_Name => "gps-edit-symbolic",
         Category => -"Scenario");

      Register_Action
        (Kernel, "Scenario delete variable",
         new Command_Delete_Variable,
         -"Delete the selected variable",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Scenario");
   end Register_Module;

end Scenario_Views;

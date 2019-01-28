------------------------------------------------------------------------------
--                                   GPS                                    --
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

with Glib;                     use Glib;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib.Values;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Button_Box;           use Gtk.Button_Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Flow_Box_Child;       use Gtk.Flow_Box_Child;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Info_Bar;             use Gtk.Info_Bar;
with Gtk.Menu;
with Gtk.Message_Dialog;       use Gtk.Message_Dialog;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.MDI;               use Gtkada.MDI;

with Commands.Interactive;     use Commands.Interactive;
with Default_Preferences;      use Default_Preferences;
with Dialog_Utils;             use Dialog_Utils;
with Generic_Views;
with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Kernel;               use GPS.Kernel;
with GUI_Utils;                use GUI_Utils;
with Projects;                 use Projects;
with Variable_Editors;         use Variable_Editors;
with XML_Utils;                use XML_Utils;

package body Scenario_Views is

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.Scenario_Views");

   Revert_Action_Name   : constant String := "Scenario Revert Modification";
   Validate_Action_Name : constant String := "Scenario Validate Variable";
   --  Name of the actions associated to the Scenario view

   Up_To_Date_Icon_Name : constant String := "vcs-up-to-date";
   Modified_Icon_Name   : constant String := "vcs-modified-staged-unstaged";
   Not_Valid_Icon_Name  : constant String := "vcs-removed";
   --  Icons used to display the status of the scenario variables

   package Build_Mode_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use Build_Mode_Lists;

   type Variable_Combo_Box_Record is new Gtk_Combo_Box_Text_Record with record
      Kernel   : Kernel_Handle;
      Var_Name : Unbounded_String;
      Untyped  : Boolean := False;
   end record;
   type Variable_Combo_Box is access all Variable_Combo_Box_Record'Class;

   package Variable_Combo_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type    => Variable_Combo_Box,
      "="             => "=");
   use Variable_Combo_Lists;

   function Get_Current_Value
     (Combo  : not null access Variable_Combo_Box_Record'Class)
      return String;
   --  Return the current value in the project tree of the scenario variable
   --  associated with the given combo.

   function Has_Valid_Value
     (Combo : not null access Variable_Combo_Box_Record'Class)
      return Boolean;
   --  Return True if the combo's active text is a valid value for the
   --  associated variable or False otherwise.

   procedure Apply_Value
     (Combo : not null access Variable_Combo_Box_Record'Class);
   --  Apply the value currently set in the combo to the associated variable.

   type Scenario_View_Module_Record is new Module_ID_Record with record
      Modes : Build_Mode_Lists.List;
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
      View                : Dialog_View;
      --  A dialog view containing a hashmap of dialog_group of scenario var
      Variable_Combo_List : Variable_Combo_Lists.List;
      --  Map containing the name of the scenario variable
      Combo_Build         : Gtk_Combo_Box_Text;
      --  The combo box with the Build Mode choices
      Build_Group         : Dialog_Group_Widget;
      --  The group with the build mode, needed to hide/show it
      Scenar_Group        : Dialog_Group_Widget;
      --  The group with the scenario variable combo box
      Buttons_Group       : Dialog_Group_Widget;
      --  The group with the Apply/Discard buttons
      Scenar_View         : Dialog_View_With_Button_Box;
      --  The view to modify and visualize scenario variables,
      --  needed to refresh the variables
      Apply_Button        : Gtk_Button;
      --  The button to apply the changes
      Discard_Button      : Gtk_Button;
      --  The button to discard the changes
      Error_Info_Bar      : Gtk_Info_Bar;
      --  The error info bar that is displayed when invalid values are
      --  submitted when clicking on the Apply button
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

   function Selected_Variable
     (View : access Scenario_View_Record'Class)
      return Scenario_Variable;
   --  Returns the currently selected variable

   type On_Refresh is new Simple_Hooks_Function with record
      View : access Scenario_View_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Callback when some aspect of the project has changed, to refresh the
   --  view.

   type On_Build_Mode_Changed is new String_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Build_Mode_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Mode   : String);
   --  Called when the build mode is being changed by the user

   procedure On_Force_Refresh (View : access GObject_Record'Class);
   --  Force a refresh of the view when some settings have changed.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   Show_Build_Modes : Boolean_Preference;

   type Command_Validate_Variable is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Command_Validate_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Apply the variable modifications

   type Command_Revert_Modification is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Command_Revert_Modification;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Revert the variable modifications

   package Variable_Combo_Callbacks is new Gtk.Handlers.User_Callback
     (Widget_Type => Variable_Combo_Box_Record,
      User_Type   => Scenario_View);

   procedure Command_Add_Variable
     (View : access Glib.Object.GObject_Record'Class);
   --  Add a new variable

   procedure Command_Delete_Variable
     (View : access Glib.Object.GObject_Record'Class);
   --  Delete selected variable

   procedure Command_Edit_Variable
     (View : access Glib.Object.GObject_Record'Class);
   --  Edit the selected variable

   function Sort_Scenario_By_Name
     (C1 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
      C2 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
      return Glib.Gint;
   --  Function used to sort the scenario variables. Each flow box child must
   --  have the name set to the scenario variable name it's representing

   procedure Fill_Build_Mode (View : Scenario_View);
   --  Fill View.Combo_Build with the build mode value

   procedure On_Variable_Combo_Changed
     (Combo  : access Variable_Combo_Box_Record'Class;
      Params : Glib.Values.GValues;
      View   : Scenario_View);
   --  Compare the value set in memory and the value set in the view

   procedure On_Build_Mode_Combo_Changed
     (Self : access Glib.Object.GObject_Record'Class);

   procedure On_Apply_Button_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the user clicks on the 'Apply' button.
   --  Validate all the values entered in the Scenario view.

   procedure On_Discard_Button_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the user clicks on the 'Discard' button.
   --  Discard all the values entered in the Scenario view.

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Scenario_View_Module_Record) is
   begin
      Self.Modes.Clear;
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
      View : constant Scenario_View :=
        Scenario_Views.Retrieve_View (Kernel, Visible_Only => True);
   begin
      if View /= null then
         Set_Font_And_Colors (View.View, Fixed_Font => False, Pref => Pref);

         for Combo of View.Variable_Combo_List loop
            Set_Font_And_Colors (Combo.Get_Child, Fixed_Font => True);
         end loop;

         if Pref = null
           or else Pref = Preference (Show_Build_Modes)
         then
            On_Force_Refresh (View);
         end if;
      end if;
   end Execute;

   -----------------------
   -- Get_Current_Value --
   -----------------------

   function Get_Current_Value
     (Combo  : not null access Variable_Combo_Box_Record'Class)
      return String
   is
      Tree     : constant Project_Tree_Access := Combo.Kernel.Registry.Tree;
      Var_Name : constant String := To_String (Combo.Var_Name);
   begin
      if Tree = null then
         return "";
      end if;

      if Combo.Untyped then
         declare
            Untyped_Var : constant Untyped_Variable :=
                            Tree.Get_Untyped_Variable
                              (Var_Name);
         begin
            return Value (Untyped_Var);
         end;
      else
         declare
            Typed_Var : constant Scenario_Variable :=
                          Tree.Scenario_Variables
                            (Var_Name);
         begin
            return Value (Typed_Var);
         end;
      end if;
   end Get_Current_Value;

   ---------------------
   -- Has_Valid_Value --
   ---------------------

   function Has_Valid_Value
     (Combo : not null access Variable_Combo_Box_Record'Class)
      return Boolean
   is
      Tree  : constant Project_Tree_Access := Combo.Kernel.Registry.Tree;
      Value : constant String := Combo.Get_Active_Text;
   begin
      if Tree = null then
         return True;
      end if;

      --  Always return True for untyped variables.
      --  For typed ones, verify that the value set in the combo is contained
      --  in the variable's possible values.

      if Combo.Untyped then
         return True;
      else
         declare

            Var             : constant Scenario_Variable :=
                                Tree.Scenario_Variables
                                  (To_String (Combo.Var_Name));
            Possible_Values : GNAT.Strings.String_List :=
                                Tree.Possible_Values_Of (Var);
         begin
            return Is_Valid : constant Boolean :=
              (for some Possible_Value of Possible_Values =>
                 Value = Possible_Value.all)
            do
               GNATCOLL.Utils.Free (Possible_Values);
            end return;
         end;
      end if;
   end Has_Valid_Value;

   -----------------
   -- Apply_Value --
   -----------------

   procedure Apply_Value
     (Combo : not null access Variable_Combo_Box_Record'Class)
   is
      Var_Name : constant String := To_String (Combo.Var_Name);
      Value    : constant String := Combo.Get_Active_Text;
      Tree     : constant Project_Tree_Access := Combo.Kernel.Registry.Tree;
   begin
      if Tree = null then
         return;
      end if;

      Trace (Me, "Set value of '" & Var_Name & "' to '"
             & Value & "'");

      if Combo.Untyped then
         declare
            Untyped_Var : Untyped_Variable :=
                            Tree.Get_Untyped_Variable
                              (Var_Name);
         begin
            Set_Value (Untyped_Var, Value);
            Tree.Change_Environment
              (Vars  => All_Scenarios,
               UVars => (1 => Untyped_Var));
         end;
      else
         declare
            Typed_Var : Scenario_Variable :=
                          Tree.Scenario_Variables
                            (Var_Name);
         begin
            Set_Value (Typed_Var, Value);
            Tree.Change_Environment (Vars => (1 => Typed_Var));
         end;
      end if;
   end Apply_Value;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View    : access Scenario_View_Record'Class)
      return Gtk_Widget
   is
      Button : Gtk_Button;
      Group  : Dialog_Group_Widget;
      Combo  : Gtk_Combo_Box_Text;
      Module : constant Scenario_View_Module :=
                 Scenario_View_Module (Scenario_Views.Get_Module);
   begin
      Initialize_Vbox (View, Homogeneous => False);

      --  Initialize and create the dialog view
      View.View := new Dialog_View_Record;
      Dialog_Utils.Initialize (View.View);
      View.Pack_Start (View.View);
      Get_Style_Context (View.View).Add_Class ("scenario-view");

      --  Initialize and create the view containing the scenario variables
      View.Scenar_View := new Dialog_View_With_Button_Box_Record;
      Dialog_Utils.Initialize (View.Scenar_View, Position => Pos_Left);
      Get_Style_Context (View.Scenar_View).Add_Class
        ("scenario-variables-view");

      --  Create 3 buttons add/suppress/edit new scenario variable
      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-add-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked (Command_Add_Variable'Access, Slot => View);
      View.Scenar_View.Append_Button (Button);

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-remove-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked (Command_Delete_Variable'Access, Slot => View);
      View.Scenar_View.Append_Button (Button);

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-edit-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked (Command_Edit_Variable'Access, Slot => View);
      View.Scenar_View.Append_Button (Button);

      --  Create the build group
      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize (Self => Group,
                               Parent_View => View.View,
                               Group_Name => "Build",
                               Allow_Multi_Columns => False);
      Gtk_New (Combo);
      Create_Child
        (Group, Combo, Label => "Build Mode", Child_Key => "Build Mode");
      Display_Information_On_Child
        (View.View, "Build Mode", To_String (Module.Modes_Help));
      View.Combo_Build := Combo;
      --  Can be called because Combo_Build was created before
      Fill_Build_Mode (View);
      View.Build_Group := Group;

      --  Create the group containing the variable view with buttons
      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize (Self => Group,
                               Parent_View => View.View,
                               Group_Name => "Scenario Variables",
                               Allow_Multi_Columns => False);
      --  Add the Scenario Variable View in the previous Widget Group
      Append_Child
        (Group, View.Scenar_View, Child_Key => "Scenario Variables");

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
      Build_Mode_Changed_Hook.Add
        (new On_Build_Mode_Changed, Watch => View);
      Set_Font_And_Colors (View.View, Fixed_Font => False);

      --  Update the viewer with the current project
      On_Force_Refresh (View);

      return Gtk_Widget (View.View);
   end Initialize;

   -----------------------
   -- Selected_Variable --
   -----------------------

   function Selected_Variable
     (View : access Scenario_View_Record'Class)
      return Scenario_Variable
   is
      List : constant Gtk.Widget.Widget_List.Glist
        := View.Scenar_Group.Get_Selected_Children;
   begin
      if not Gtk.Widget.Widget_List."="
        (List, Gtk.Widget.Widget_List.Null_List)
      then
         declare
            --  Selection_Single was used to create the flow box,
            --  the list is not empty so get the only elem and retrieve its
            --  name.
            Child_Flow_Box : constant Gtk_Flow_Box_Child
              := Gtk_Flow_Box_Child (Gtk.Widget.Widget_List.Get_Data (List));
            Variable_Name  : constant String
              := Child_Flow_Box.Get_Name;
            Scenar         : constant Scenario_Variable_Array
              := Scenario_Variables (View.Kernel);
         begin
            for J in Scenar'Range loop
               if External_Name (Scenar (J)) = Variable_Name then
                  return Scenar (J);
               end if;
            end loop;
         end;
      end if;
      return No_Variable;
   end Selected_Variable;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Command_Validate_Variable;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K      : constant Kernel_Handle     := Get_Kernel (Context.Context);
      V      : constant Scenario_View     := Scenario_Views.Retrieve_View (K);
   begin
      --  Apply the values of the scenario variables

      if not V.Variable_Combo_List.Is_Empty then
         for Variable_Combo of V.Variable_Combo_List loop
            Apply_Value (Variable_Combo);
         end loop;
      end if;

      Recompute_View (K);

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Command_Revert_Modification;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
      V : constant Scenario_View := Scenario_Views.Retrieve_View (K);
      H : aliased On_Refresh;
   begin
      --  Refresh the variable will put back their current values
      H.View := V;
      H.Execute (V.Kernel);
      return Commands.Success;
   end Execute;

   --------------------------
   -- Command_Add_Variable --
   --------------------------

   procedure Command_Add_Variable
     (View : access Glib.Object.GObject_Record'Class)
   is
      V        : constant Scenario_View := Scenario_View (View);
      Edit     : New_Var_Edit;
   begin
      Gtk_New (Edit, V.Kernel,
               Title => -"Creating a new variable");
      Show_All (Edit);

      while Run (Edit) = Gtk_Response_OK
        and then not Update_Variable (Edit)
      loop
         null;
      end loop;

      Destroy (Edit);
   end Command_Add_Variable;

   -----------------------------
   -- Command_Delete_Variable --
   -----------------------------

   procedure Command_Delete_Variable
     (View : access Glib.Object.GObject_Record'Class)
   is
      V        : constant Scenario_View := Scenario_View (View);
      Var      : constant Scenario_Variable := Selected_Variable (V);
      Message  : constant String :=
        "Doing so will remove all the configurations associated with"
        & ASCII.LF
        & "that variable, except for the currently selected value";

      Response : Message_Dialog_Buttons;
      Success  : Boolean;
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
            Trace (Me, "Delete_Variable: " & External_Name (Var));

            Get_Registry (V.Kernel).Tree.Delete_Scenario_Variable
              (External_Name            => External_Name (Var),
               Keep_Choice              => Value (Var),
               Delete_Direct_References => False);

            V.Kernel.Get_Project_Tree.Recompute_View;

            Success := Save_Project
              (Kernel    => V.Kernel,
               Project   => V.Kernel.Get_Project_Tree.Root_Project,
               Recursive => False);
            Variable_Changed_Hook.Run (V.Kernel);

            if not Success then
               Trace
                 (Me, "Failed to save the project after deleting "
                  & "variable:" & External_Name (Var));
            end if;
         end if;
      end if;
   end Command_Delete_Variable;

   ---------------------------
   -- Command_Edit_Variable --
   ---------------------------

   procedure Command_Edit_Variable
     (View : access Glib.Object.GObject_Record'Class)
   is
      V        : constant Scenario_View := Scenario_View (View);
      Variable : constant Scenario_Variable := Selected_Variable (V);
      Edit     : New_Var_Edit;
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
   end Command_Edit_Variable;

   ---------------------------
   -- Sort_Scenario_By_Name --
   ---------------------------

   function Sort_Scenario_By_Name
     (C1 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class;
      C2 : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
      return Glib.Gint
   is
   begin
      if C1.Get_Name <= C2.Get_Name then
         return -1;
      else
         return 1;
      end if;
   end Sort_Scenario_By_Name;

   -------------------------------
   -- On_Variable_Combo_Changed --
   -------------------------------

   procedure On_Variable_Combo_Changed
     (Combo  : access Variable_Combo_Box_Record'Class;
      Params : Glib.Values.GValues;
      View   : Scenario_View)
   is
      pragma Unreferenced (Params);
      Has_Modif : Boolean := False;
      Var_Name  : constant String := To_String (Combo.Var_Name);
      Ent       : constant Gtk_Entry := Gtk_Entry (Combo.Get_Child);
   begin
      --  Hide the error info bar when some variables' values are modified

      if View.Error_Info_Bar /= null then
         View.Error_Info_Bar.Hide;
      end if;

      --  Update the combo's entry icons if the value has been modified

      if Combo.Get_Active_Text /= Get_Current_Value (Combo) then
         Ent.Set_Icon_From_Icon_Name
           (Icon_Pos  => Gtk_Entry_Icon_Primary,
            Icon_Name => Modified_Icon_Name);
         Has_Modif := True;
      else
         Ent.Set_Icon_From_Icon_Name
           (Icon_Pos  => Gtk_Entry_Icon_Primary,
            Icon_Name => Up_To_Date_Icon_Name);
      end if;

      View.Scenar_View.Remove_Information_On_Child
        (Child_Key => Var_Name);

      --  Update the 'Apply' and 'Discard' buttons sensitivity accordingly

      View.Apply_Button.Set_Sensitive (Has_Modif);
      View.Discard_Button.Set_Sensitive (Has_Modif);
   end On_Variable_Combo_Changed;

   ---------------------
   -- Fill_Build_Mode --
   ---------------------

   procedure Fill_Build_Mode (View : Scenario_View)
   is
      Module   : constant Scenario_View_Module :=
        Scenario_View_Module (Scenario_Views.Get_Module);
      Cur_Mode : constant String               := View.Kernel.Get_Build_Mode;
      Iter     : Build_Mode_Lists.Cursor       := Module.Modes.First;
   begin
      --  Clear the combo box, needed by the revert action
      View.Combo_Build.Remove_All;
      --  Fill the possible values of build mode
      while Iter /= Build_Mode_Lists.No_Element loop
         if Cur_Mode = Element (Iter) then
            --  Put the selected value in the first column
            View.Combo_Build.Prepend_Text (Element (Iter));
         else
            View.Combo_Build.Append_Text (Element (Iter));
         end if;
         Next (Iter);
      end loop;
      View.Combo_Build.Set_Active (0);
      View.Combo_Build.On_Changed
        (On_Build_Mode_Combo_Changed'Access,
         Slot => View);
   end Fill_Build_Mode;

   ---------------------------------
   -- On_Build_Mode_Combo_Changed --
   ---------------------------------

   procedure On_Build_Mode_Combo_Changed
     (Self : access Glib.Object.GObject_Record'Class)
   is
      View : constant Scenario_View := Scenario_View (Self);
      Incompatible : Boolean := False;
   begin
      if View.Combo_Build.Get_Active_Text = "debug" then
         --  Check wether the project contains "-s" option

         declare
            Project : constant Project_Type :=
              View.Kernel.Get_Project_Tree.Root_Project;
            Langs    : GNAT.Strings.String_List := Project.Languages;
            Args     : GNAT.Strings.String_List_Access;
            Default  : Boolean;
         begin
            Project.Switches
              (GNATCOLL.Projects.Linker_Package,
               GNATCOLL.VFS.No_File,
               (if Project.Has_Language ("ada")
                then "ada"
                else Langs (Langs'First).all),
               Args,
               Default);

            for J in Args'Range loop
               if Args (J).all = "-s" then
                  Incompatible := True;
                  exit;
               end if;
            end loop;

            GNATCOLL.Utils.Free (Langs);
            GNAT.Strings.Free (Args);
         end;

         if Incompatible then
            View.Kernel.Insert
              ("The project contains 'strip all symbols' option which is " &
                 "incompatible with debugging. Please, remove '-s' from " &
                 "the project.",
               Mode => Error);

            View.Combo_Build.Set_Active (0);
            return;
         end if;
      end if;

      View.Kernel.Set_Build_Mode
        (New_Mode => View.Combo_Build.Get_Active_Text);
   end On_Build_Mode_Combo_Changed;

   -----------------------------
   -- On_Apply_Button_Clicked --
   -----------------------------

   procedure On_Apply_Button_Clicked
     (Self : access Glib.Object.GObject_Record'Class)
   is
      View         : constant Scenario_View := Scenario_View (Self);
      Success      : Boolean with Unreferenced;
      Should_Apply : Boolean := True;
   begin
      for Combo of View.Variable_Combo_List loop
         declare
            Var_Name  : constant String := To_String (Combo.Var_Name);
            Value     : constant String := Combo.Get_Active_Text;
            Ent       : constant Gtk_Entry := Gtk_Entry (Combo.Get_Child);
            Is_Valid  : constant Boolean := Has_Valid_Value (Combo);
         begin
            Should_Apply := Should_Apply and Is_Valid;

            if not Is_Valid then
               Ent.Set_Icon_From_Icon_Name
                 (Icon_Pos  => Gtk_Entry_Icon_Primary,
                  Icon_Name => Not_Valid_Icon_Name);
               View.Scenar_View.Display_Information_On_Child
                 (Child_Key => Var_Name,
                  Message   => "'" & Value & "' is not a valid value for "
                  & Var_Name,
                  Is_Error  => True);
            end if;

            --  Append the value to the combo for untyped variables.

            if Combo.Untyped then
               Add_Unique_Combo_Entry (Combo, Value);
            end if;
         end;
      end loop;

      if Should_Apply then
         Success := Execute_Action
           (View.Kernel,
            Action => Validate_Action_Name);
      else
         View.Error_Info_Bar := Create_Info_Bar
           (Message      => "Some values are not valid",
            Message_Type => Message_Error);
         View.Scenar_View.Insert (View.Error_Info_Bar,
                                  Position      => 0,
                                  Expand        => False,
                                  Add_Separator => False);
         View.Error_Info_Bar.Show_All;
      end if;
   end On_Apply_Button_Clicked;

   -------------------------------
   -- On_Discard_Button_Clicked --
   -------------------------------

   procedure On_Discard_Button_Clicked
     (Self : access Glib.Object.GObject_Record'Class)
   is
      View    : constant Scenario_View := Scenario_View (Self);
      Success : Boolean with Unreferenced;
   begin
      Success := Execute_Action
        (View.Kernel,
         Action => Revert_Action_Name);
   end On_Discard_Button_Clicked;

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
      Combo      : Variable_Combo_Box;
      Ent        : Gtk_Entry;
      Group      : Dialog_Group_Widget;
      View       : constant Scenario_View := Scenario_View (Self.View);
      Show_Build : constant Boolean := Show_Build_Modes.Get_Pref;

      procedure Add_Scenario_Variable_Combo
        (Name            : String;
         Possible_Values : GNAT.Strings.String_List;
         Value           : String;
         Untyped         : Boolean);

      ---------------------------------
      -- Add_Scenario_Variable_Combo --
      ---------------------------------

      procedure Add_Scenario_Variable_Combo
        (Name            : String;
         Possible_Values : GNAT.Strings.String_List;
         Value           : String;
         Untyped         : Boolean)
      is
         Flow_Child : Gtk_Widget;
      begin
         Combo := new Variable_Combo_Box_Record'
           (GObject_Record with
            Kernel   => Kernel_Handle (Kernel),
            Var_Name => To_Unbounded_String (Name),
            Untyped  => Untyped);

         Gtk.Combo_Box_Text.Initialize_With_Entry (Combo);
         Flow_Child := Create_Child
           (Group, Combo,
            Label     => Name,
            Child_Key => Name);

         Set_Font_And_Colors (Combo.Get_Child, Fixed_Font => True);

         --  Set the name to variable name needed by Selected_Variable
         Flow_Child.Set_Name (Name);

         --  Store it in the scenario combo box list
         View.Variable_Combo_List.Append (Combo);

         for Value of Possible_Values loop
            if Value.all /= "" then
               Combo.Append_Text (Locale_To_UTF8 (Value.all));
            end if;
         end loop;

         --  Select the variable's current value
         Set_Active_Text (Combo, Value);

         Ent := Gtk_Entry (Combo.Get_Child);

         --  Decrease the entry's minimum width so that the combobox buttons
         --  are still displayed when reudcing the Scenario view's width.
         Ent.Set_Width_Chars (4);

         --  Display the 'up-to-date' icon in the combo's entry
         Ent.Set_Icon_From_Icon_Name
           (Icon_Pos  => Gtk_Entry_Icon_Primary,
            Icon_Name => Up_To_Date_Icon_Name);

         Variable_Combo_Callbacks.Connect
           (Combo, Gtk.Editable.Signal_Changed,
            On_Variable_Combo_Changed'Access,
            User_Data   => View);

         --  The combo needs to have the name of the variable
         Combo.Set_Name (Name);

         --  Set an 'empty' placeholder to make it clear that the variable
         --  has an empty value.
         if Value = "" then
            Ent.Set_Placeholder_Text ("empty");
         end if;

         --  Disable the combo's button sensitivity for untyped variables
         --  since we don't have any values to propose.
         if Untyped then
            Combo.Set_Button_Sensitivity (Sensitivity_Off);
         end if;
      end Add_Scenario_Variable_Combo;

   begin
      Trace (Me, "Recomputing list of scenario variables");

      --  Clean the View
      View.Scenar_View.Remove_All_Children;
      View.Variable_Combo_List.Clear;
      View.Error_Info_Bar := null;

      declare
         Typed_Vars  : constant Scenario_Variable_Array
           := Scenario_Variables (Kernel);
         Untyped_Vars : constant Untyped_Variable_Array
           := Untyped_Variables (Kernel);
      begin
         --  Create the group containing a combobox for each of the variable
         Group := new Dialog_Group_Widget_Record;
         Dialog_Utils.Initialize (Self => Group,
                                  Parent_View => View.Scenar_View,
                                  Group_Name => "",
                                  Allow_Multi_Columns => False,
                                  Selection => Selection_Single,
                                  Sorting_Function =>
                                    Sort_Scenario_By_Name'Access);
         View.Scenar_Group := Group;

         --  Add the typed scenario variables' combos

         if Typed_Vars'Length /= 0 then
            for J in Typed_Vars'Range loop
               declare
                  Name            : constant String
                    := External_Name (Typed_Vars (J));
                  Value           : constant String := GNATCOLL.Projects.Value
                    (Typed_Vars (J));
                  Possible_Values : GNAT.Strings.String_List
                    := Get_Registry (Kernel).Tree.Possible_Values_Of
                    (Typed_Vars (J));
               begin
                  Add_Scenario_Variable_Combo
                    (Name,
                     Possible_Values => Possible_Values,
                     Value           => Value,
                     Untyped         => False);
                  GNATCOLL.Utils.Free (Possible_Values);
               end;
            end loop;
         end if;

         --  Add the untyped scenario variables' combos

         if Untyped_Vars'Length /= 0 then
            for J in Untyped_Vars'Range loop
               declare
                  Name            : constant String
                    := External_Name (Untyped_Vars (J));
                  Value           : constant String := GNATCOLL.Projects.Value
                    (Untyped_Vars (J));
                  Possible_Values : GNAT.Strings.String_List
                    (1 .. 0) := (others => null);
               begin
                  Add_Scenario_Variable_Combo
                    (Name,
                     Possible_Values => Possible_Values,
                     Value           => Value,
                     Untyped         => True);
                  GNATCOLL.Utils.Free (Possible_Values);
               end;
            end loop;
         end if;

         if Typed_Vars'Length > 0 or else Untyped_Vars'Length > 0 then
            declare
               HButton_Box : Gtk_Button_Box;
            begin
               View.Buttons_Group := new Dialog_Group_Widget_Record;
               Dialog_Utils.Initialize
                 (Self                => View.Buttons_Group,
                  Parent_View         => View.Scenar_View,
                  Group_Name          => "",
                  Allow_Multi_Columns => False);

               Gtk_New (HButton_Box, Orientation => Orientation_Horizontal);
               HButton_Box.Set_Layout (Buttonbox_Center);
               HButton_Box.Set_Spacing (5);

               Get_Style_Context (HButton_Box).Add_Class ("action-box");
               Gtk_New_From_Stock (View.Apply_Button, Stock_Apply);
               View.Apply_Button.On_Clicked
                 (On_Apply_Button_Clicked'Access,
                  Slot => View);
               View.Apply_Button.Set_Sensitive (False);

               HButton_Box.Pack_Start (View.Apply_Button);

               Gtk_New_From_Stock (View.Discard_Button, Stock_Discard);
               View.Discard_Button.On_Clicked
                 (On_Discard_Button_Clicked'Access,
                  Slot => View);
               View.Discard_Button.Set_Sensitive (False);

               HButton_Box.Pack_Start (View.Discard_Button);

               View.Buttons_Group.Append_Child
                 (Widget      => HButton_Box,
                  Expand      => False,
                  Homogeneous => False);
            end;

            --  Force a resort
            Group.Force_Sort;
         end if;
      end;

      --  The View is built after the initialize, Show_All must be called to
      --  show the added widgets.
      Show_All (View);

      if not Show_Build then
         Hide (View.Build_Group);
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
      View : constant Scenario_View := Scenario_Views.Retrieve_View (Kernel);
   begin
      Set_Active_Text
        (Combo => View.Combo_Build,
         Text  => Mode);
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
               Module.Modes.Append (Name);
            end if;

            if Description /= null then
               Append (Module.Modes_Help,
                       Name & ":" & ASCII.LF & ASCII.HT &
                       Description.all & ASCII.LF);
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
      Scenario_Views.Register_Module
        (Kernel,
         ID        => Module_ID (M));

      Show_Build_Modes := Kernel.Get_Preferences.Create_Invisible_Pref
        ("scenario-show-build-modes", True,
         Label => -"Show build modes");

      Register_Action
        (Kernel, Validate_Action_Name,
         new Command_Validate_Variable,
         Description =>
           -("Save all the scenario modifications. The project"
           & " must be built for the changes to be applied"),
         Icon_Name => "gps-syntax-check-symbolic",
         Category => -"Scenario");

      Register_Action
        (Kernel, Revert_Action_Name,
         new Command_Revert_Modification,
         Description =>
           -("Revert the modifications in the view"),
         Icon_Name => "gps-stop-symbolic",
         Category => -"Scenario");
   end Register_Module;

end Scenario_Views;

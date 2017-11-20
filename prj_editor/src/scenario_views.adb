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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Convert;             use Glib.Convert;

with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Button;               use Gtk.Button;
with Gtk.Flow_Box_Child;       use Gtk.Flow_Box_Child;
with Gtk.Label;                use Gtk.Label;
with Gtk.Event_Box;            use Gtk.Event_Box;

with Commands.Interactive;     use Commands.Interactive;
with Default_Preferences;      use Default_Preferences;
with Projects;                 use Projects;
with Generic_Views;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;
with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with Variable_Editors;         use Variable_Editors;
with GPS.Intl;                 use GPS.Intl;
with XML_Utils;                use XML_Utils;
with Dialog_Utils;             use Dialog_Utils;
with GUI_Utils;                use GUI_Utils;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Scenario_Views is

   Me : constant Trace_Handle := Create ("Scenario_Views");

   package Build_Mode_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use Build_Mode_Lists;

   package Scenario_Combo_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Gtk_Combo_Box_Text,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   use Scenario_Combo_Maps;

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
      View          : Dialog_View;
      --  A dialog view containing a hashmap of dialog_group of scenario var
      Combo_Var_Map : Scenario_Combo_Maps.Map;
      --  Map containing the name of the scenario variable
      Combo_Build   : Gtk_Combo_Box_Text;
      --  The combo box with the Build Mode choices
      Build_Group   : Dialog_Group_Widget;
      --  The group with the build mode, needed to hide/show it
      Scenar_Group  : Dialog_Group_Widget;
      --  The group with the scenario variable combo box
      Scenar_View   : Dialog_View_With_Button_Box;
      --  The view to modify and visualize scenario variables,
      --  needed to refresh the variables
      Warning_Lbl   : Gtk_Label;
      --  Warning displaid when the scenario is modified
      Warning_Event : Gtk_Event_Box;
      --  Container of the warning label
      Warning_Group : Dialog_Group_Widget;
      --  The group with the warning label
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

   procedure On_Force_Refresh (View : access GObject_Record'Class);
   --  Force a refresh of the view when some settings have changed.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   Show_Build_Modes : Boolean_Preference;

   procedure Add_New_Value
     (Kernel   : Kernel_Handle;
      New_Val  : String;
      Variable : in out Scenario_Variable);
   --  If New_Val is not already defined in Variable,
   --  add it in the list of possible values.
   --  Change current value of Variable to New_Val.

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
   --  have the name set to the the scenario variable name it's representing

   procedure Show_Msg_If_Modified
     (View : access Glib.Object.GObject_Record'Class);
   --  Compare the value set in memory and the value set in the view

   procedure Fill_Build_Mode (View : Scenario_View);
   --  Fill View.Combo_Build with the build mode value

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

      --  Initialize and create the view containing the scenario variables
      View.Scenar_View := new Dialog_View_With_Button_Box_Record;
      Dialog_Utils.Initialize (View.Scenar_View, Position => Pos_Left);

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

      --  Create the warning label, put it in a group to make it pretty
      Create_Warning_Label ("The scenario is modified, please refresh "
                            & "the view to apply the modifications",
                            View.Warning_Lbl,
                            View.Warning_Event);
      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize (Self => Group,
                               Parent_View => View.View,
                               Allow_Multi_Columns => False);
      Create_Child (Group, View.Warning_Event);
      View.Warning_Group := Group;

      --  Create the build group
      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize (Self => Group,
                               Parent_View => View.View,
                               Group_Name => "Build",
                               Allow_Multi_Columns => False);
      Gtk_New (Combo);
      Create_Child
        (Group, Combo, Label => "Build Mode",
         Doc => To_String (Module.Modes_Help));
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

   -------------------
   -- Add_New_Value --
   -------------------

   procedure Add_New_Value
     (Kernel   : Kernel_Handle;
      New_Val  : String;
      Variable : in out Scenario_Variable)
   is
      Already_Here : Boolean := False;
      List_Values  : constant GNAT.Strings.String_List
        := Get_Registry (Kernel).Tree.Possible_Values_Of (Variable);
      List_New_Val : GNAT.Strings.String_List (1 .. 1);
      Ptr_New_Val  : constant GNAT.Strings.String_Access
        := new String'(New_Val);
   begin
      for Val in List_Values'Range loop
         if List_Values (Val).all = New_Val then
            Already_Here := True;
            exit;
         end if;
      end loop;
      --  If not already here, add the value
      if not Already_Here then
         List_New_Val (1) := Ptr_New_Val;
         Add_Values (Get_Registry (Kernel).Tree.all, Variable, List_New_Val);
         GNAT.Strings.Free (List_New_Val (1));
      end if;
      Set_Value (Variable, New_Val);
   end Add_New_Value;

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
      Scenar : Scenario_Variable_Array    := Scenario_Variables (K);
      Elem   : Gtk_Combo_Box_Text;
      Val    : Unbounded_String;
      Name   : Unbounded_String;
   begin
      --  Set the Build Mode
      V.Kernel.Set_Build_Mode (New_Mode => V.Combo_Build.Get_Active_Text);

      if not V.Combo_Var_Map.Is_Empty then
         declare
            Index  : Scenario_Combo_Maps.Cursor := V.Combo_Var_Map.First;
         begin
            --  Set val of the scenario variables

            while Index /= Scenario_Combo_Maps.No_Element loop
               Elem := Scenario_Combo_Maps.Element (Index);
               Val := To_Unbounded_String (Elem.Get_Active_Text);
               Name := To_Unbounded_String (Scenario_Combo_Maps.Key (Index));

               --  Find the Variable with the same name as Combo
               for J in Scenar'Range loop
                  if External_Name (Scenar (J)) = To_String (Name) then
                     Trace (Me, "Set value of '" & To_String (Name) & "' to '"
                            & To_String (Val) & "'");
                     Add_New_Value (K, To_String (Val), Scenar (J));
                     Get_Registry (K).Tree.Change_Environment
                       ((1 => Scenar (J)));
                  end if;
               end loop;

               --  Next Element
               Index := Scenario_Combo_Maps.Next (Index);
            end loop;
         end;
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
      Fill_Build_Mode (V);
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

   --------------------------
   -- Show_Msg_If_Modified --
   --------------------------

   procedure Show_Msg_If_Modified
     (View : access Glib.Object.GObject_Record'Class)
   is
      V           : constant Scenario_View           := Scenario_View (View);
      Kernel      : constant Kernel_Handle           := V.Kernel;
      Scenar_Var  : constant Scenario_Variable_Array :=
        Scenario_Variables (Kernel);
      Is_Modified : Boolean                          := False;
   begin
      --  Go through the list of scenario variables
      if Scenar_Var'Length /= 0 then
         for J in Scenar_Var'Range loop
            declare
               Name : constant String := External_Name (Scenar_Var (J));
               Val  : constant String :=
                 V.Combo_Var_Map.Element (Name).Get_Active_Text;
            begin
               if Val /= Value (Scenar_Var (J)) then
                  Is_Modified := True;
                  exit;
               end if;
            end;
         end loop;
      end if;
      --  Check if at least a scenario variable is modified
      --  or if the build mode has changed
      if Is_Modified
        or (V.Kernel.Get_Build_Mode /= V.Combo_Build.Get_Active_Text)
      then
         Show (V.Warning_Group);
      else
         Hide (V.Warning_Group);
      end if;
   end Show_Msg_If_Modified;

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
      View.Combo_Build.On_Changed (Show_Msg_If_Modified'Access, View);
   end Fill_Build_Mode;

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
      Combo      : Gtk_Combo_Box_Text;
      Group      : Dialog_Group_Widget;
      View       : constant Scenario_View := Scenario_View (Self.View);
      Show_Build : constant Boolean := Show_Build_Modes.Get_Pref;
   begin
      Trace (Me, "Recomputing list of scenario variables");

      --  Clean the View
      View.Scenar_View.Remove_All_Children;
      View.Combo_Var_Map.Clear;

      declare
         Scenar_Var  : constant Scenario_Variable_Array
           := Scenario_Variables (Kernel);
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

         --  Fill the combobox group
         if Scenar_Var'Length /= 0 then
            for J in Scenar_Var'Range loop
               declare
                  Name         : constant String
                    := External_Name (Scenar_Var (J));
                  Values       : constant GNAT.Strings.String_List
                    := Get_Registry (Kernel).Tree.Possible_Values_Of
                    (Scenar_Var (J));
                  Flow_Child   : Gtk_Widget;
               begin
                  Gtk_New_With_Entry (Combo);
                  Flow_Child := Create_Child
                    (Group, Combo, Label => Name, Child_Key => Name);
                  --  Set the name to variable name needed by Selected_Variable
                  Flow_Child.Set_Name (Name);

                  --  Store in hash map needed to validate the changes
                  View.Combo_Var_Map.Insert (Name, Combo);
                  for Val in Values'Range loop
                     if Values (Val).all = Value (Scenar_Var (J)) then
                        --  Put the selected value in the first column
                        Combo.Prepend_Text (Locale_To_UTF8 (Values (Val).all));
                     else
                        Combo.Append_Text (Locale_To_UTF8 (Values (Val).all));
                     end if;
                  end loop;

                  --  Show the selected value
                  Combo.Set_Active (0);
                  Combo.On_Changed (Show_Msg_If_Modified'Access, View);
                  --  The combo needs to have the name of the variable
                  Combo.Set_Name (Name);
               end;
            end loop;
         end if;

         --  Force a resort
         Group.Force_Sort;
      end;

      --  The View is built after the initialise, Show_All must be called to
      --  show the added widgets.
      Show_All (View);
      --  After a refresh the view is up to date so hide the warning label
      Hide (View.Warning_Group);
      if not Show_Build then
         Hide (View.Build_Group);
      end if;
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
                       Name & ": " & Description.all & ASCII.LF);
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
        (Kernel, "Scenario Validate Variable",
         new Command_Validate_Variable,
         Description =>
           -("Save all the scenario modifications. The project"
           & " must be built for the changes to be applied"),
         Icon_Name => "gps-syntax-check-symbolic",
         Category => -"Scenario");

      Register_Action
        (Kernel, "Scenario Revert Modification",
         new Command_Revert_Modification,
         Description =>
           -("Revert the modifications in the view"),
         Icon_Name => "gps-stop-symbolic",
         Category => -"Scenario");

   end Register_Module;

end Scenario_Views;

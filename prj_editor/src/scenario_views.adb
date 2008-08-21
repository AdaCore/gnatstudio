-----------------------------------------------------------------------
--                               GPS                                 --
--                                                                   --
--                   Copyright (C) 2001-2008, AdaCore                --
--                                                                   --
-- GPS is  free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                use Glib;
with Glib.Convert;        use Glib.Convert;
with Glib.Object;         use Glib.Object;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Event_Box;       use Gtk.Event_Box;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Image;           use Gtk.Image;
with Gtk.Label;           use Gtk.Label;
with Gtk.List;            use Gtk.List;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stock;           use Gtk.Stock;
with Gtk.Table;           use Gtk.Table;
with Gtk.Tooltips;        use Gtk.Tooltips;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Viewport;        use Gtk.Viewport;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Combo;        use Gtkada.Combo;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Glib.Xml_Int;        use Glib.Xml_Int;
with Gtkada.MDI;          use Gtkada.MDI;

with Projects.Editor;    use Projects, Projects.Editor;
with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.MDI;     use GPS.Kernel.MDI;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Project; use GPS.Kernel.Project;
with Variable_Editors;   use Variable_Editors;
with GPS.Intl;           use GPS.Intl;
with String_List_Utils;

with Traces;   use Traces;

package body Scenario_Views is

   Me : constant Debug_Handle := Create ("Scenario_Views");

   type Scenario_View_Record is new Gtk_Scrolled_Window_Record with record
      Vbox          : Gtk.Box.Gtk_Vbox;
      Table         : Gtk.Table.Gtk_Table;
      Kernel        : GPS.Kernel.Kernel_Handle;
      Empty_Event   : Gtk.Event_Box.Gtk_Event_Box;
      Empty_Label   : Gtk.Label.Gtk_Label;
      Combo_Is_Open : Boolean := False;
      --  Flag temporarily set to True when a user is modifying the value of
      --  one of the scenario variable through the combo boxes.
   end record;
   type Scenario_View is access all Scenario_View_Record'Class;

   procedure Gtk_New
     (View    : out Scenario_View;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new scenario view associated with Manager.
   --  The view is automatically refreshed every time the project view in
   --  the manager changes.

   procedure Initialize
     (View    : access Scenario_View_Record'Class;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Internal function for creating new widgets

   procedure Add_Possible_Values
     (Kernel : access Kernel_Handle_Record'Class;
      List : access Gtk_List_Record'Class;
      Var  : Scenario_Variable);
   --  Add all the possible values for type Typ into the List.

   type Variable_User_Data is record
      View : Scenario_View;
      Var  : Scenario_Variable;
   end record;

   procedure Variable_Value_Changed
     (Combo : access Gtk_Widget_Record'Class;
      User  : Variable_User_Data);
   --  Called when the value of one of the variables has changed.
   --  This recomputes the scenario view, so that changes are reflected in
   --  other parts of GPS.

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data);
   --  Called when editing a variable (name and possible values)

   procedure Delete_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data);
   --  Called when removing a variable

   procedure Setup (Data : Variable_User_Data; Id : Handler_Id);
   package View_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Gtk_Widget_Record, Variable_User_Data, Setup);

   type Refresh_Hook_Record is new Function_No_Args with record
      View : Scenario_View;
   end record;
   type Refresh_Hook is access all Refresh_Hook_Record'Class;
   overriding procedure Execute
     (Hook : Refresh_Hook_Record; Kernel : access Kernel_Handle_Record'Class);
   --  Callback when some aspect of the project has changed, to refresh the
   --  view.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handle desktop loading and saving

   procedure On_Open_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open the scenario view if it isn't already open

   type Scenario_Module_Record is new Module_ID_Record with null record;
   Scenario_Module_Id : Module_ID;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Variable_User_Data; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data.View);
   end Setup;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View : out Scenario_View;
      Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      View := new Scenario_View_Record;
      Initialize (View, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Scenario_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Hook : Refresh_Hook;
      Viewport : Gtk_Viewport;
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (View, null, null);
      Set_Policy (View, Policy_Automatic, Policy_Automatic);
      Set_Shadow_Type (View, Shadow_None);

      --  Do not use Add_With_Viewport, since otherwise we do not have
      --  access to the viewport itself to change its shadow
      Gtk_New (Viewport);
      Set_Shadow_Type (Viewport, Shadow_None);
      Add (View, Viewport);

      Gtk_New_Vbox (View.Vbox, Homogeneous => False);
      Add (Viewport, View.Vbox);

      --   Add_With_Viewport (View, View.Vbox);

      Gtk_New
        (View.Table,
         Rows        => 1,
         Columns     => 4,
         Homogeneous => False);
      Pack_Start (View.Vbox, View.Table, Expand => False);
      Set_Col_Spacing (View.Table, 0, 0);
      Set_Col_Spacing (View.Table, 1, 1);

      Gtk_New (View.Empty_Event);
      Gtk_New (View.Empty_Label,
               -"The project contains no scenario variables");
      Add (View.Empty_Event, View.Empty_Label);
      Pack_Start (View.Vbox, View.Empty_Event, Expand => True);

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

      --  Update the viewer with the current project
      Execute (Hook.all, Kernel);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Table,
         Object          => View,
         ID              => Scenario_Module_Id,
         Context_Func    => null);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Empty_Event,
         Object          => View,
         ID              => Scenario_Module_Id,
         Context_Func    => null);
   end Initialize;

   ----------------------------
   -- Variable_Value_Changed --
   ----------------------------

   procedure Variable_Value_Changed
     (Combo : access Gtk_Widget_Record'Class;
      User  : Variable_User_Data)
   is
      Value : constant String := Get_Text (Get_Entry (Gtkada_Combo (Combo)));
   begin
      if Value /= "" then
         Set_Value (User.Var, Value);
         User.View.Combo_Is_Open := True;
         Recompute_View (User.View.Kernel);
         User.View.Combo_Is_Open := False;
      end if;
   end Variable_Value_Changed;

   -------------------------
   -- Add_Possible_Values --
   -------------------------

   procedure Add_Possible_Values
     (Kernel : access Kernel_Handle_Record'Class;
      List   : access Gtk_List_Record'Class;
      Var    : Scenario_Variable)
   is
      use String_List_Utils.String_List;

      Values : String_List_Utils.String_List.List :=
        Enum_Values_Of (Var, Get_Registry (Kernel).all);
      Iter   : String_List_Utils.String_List.List_Node :=
        First (Values);
      Item : Gtk_List_Item;
   begin
      while Iter /= String_List_Utils.String_List.Null_Node loop
         Gtk_New (Item, Locale_To_UTF8
                  (String_List_Utils.String_List.Data (Iter)));
         Add (List, Item);
         Iter := Next (Iter);
      end loop;

      Show_All (List);
      Free (Values);
   end Add_Possible_Values;

   -------------------
   -- Edit_Variable --
   -------------------

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data)
   is
      pragma Unreferenced (Button);
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Data.View.Kernel, Data.Var, -"Editing a variable");
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

   procedure Delete_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data)
   is
      pragma Unreferenced (Button);
      Message : constant String :=
        "Doing so will remove all the configurations associated with"
        & ASCII.LF
        & "that variable, except for the currently selected value";

      Response : constant Message_Dialog_Buttons := Message_Dialog
        (Msg           => (-"Are you sure you want to remove the variable ")
           & '"' & External_Reference_Of (Data.Var)
           & """?" & ASCII.LF & (-Message),
         Dialog_Type   => Confirmation,
         Buttons       => Button_OK or Button_Cancel,
         Title         => -"Deleting a variable",
         Justification => Justify_Left,
         Parent        => Get_Current_Window (Data.View.Kernel));
   begin
      if Response = Button_OK then
         Delete_External_Variable
           (Root_Project             => Get_Project (Data.View.Kernel),
            Ext_Variable_Name        => External_Reference_Of (Data.Var),
            Keep_Choice              => Value_Of (Data.Var),
            Delete_Direct_References => False);
         Run_Hook (Data.View.Kernel, Variable_Changed_Hook);

         --  Recompute the view so that the explorer is updated graphically.
         Recompute_View (Data.View.Kernel);

         Trace (Me, "Delete_Variable: " & External_Reference_Of (Data.Var));
      end if;
   end Delete_Variable;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook : Refresh_Hook_Record; Kernel : access Kernel_Handle_Record'Class)
   is
      V      : constant Scenario_View := Hook.View;
      Label  : Gtk_Label;
      Combo  : Gtkada_Combo;
      Row    : Guint;
      Button : Gtk_Button;
      Pix    : Gtk_Image;

      use type Widget_List.Glist;
      Child, Tmp : Widget_List.Glist;
   begin
      --  There is a small problem here: Refresh might be called while one of
      --  the combo boxes is still displayed. Thus, if we destroy it now, any
      --  pending signal on the combo box (like hiding the popup window) will
      --  generate a segmentation fault.
      --  This also saves some refreshing when the values would be reflected
      --  automatically anyway.

      if V.Combo_Is_Open then
         return;
      end if;

      --  Remove all children, except the edit button.

      Child := Children (V.Table);
      Tmp := Widget_List.First (Child);

      while Tmp /= Widget_List.Null_List loop
         Destroy (Widget_List.Get_Data (Tmp));
         Tmp := Widget_List.Next (Tmp);
      end loop;

      Widget_List.Free (Child);

      --  No project view => Clean up the scenario viewer
      if Get_Project (Kernel) = No_Project then
         Resize (V.Table, Rows => 1, Columns => 4);
         Hide_All (V.Table);
         Show_All (V.Empty_Label);
         Set_Child_Visible (V.Empty_Label, True);

      else
         declare
            Scenar_Var : constant Scenario_Variable_Array :=
              Scenario_Variables (Kernel);
         begin
            if Scenar_Var'Length = 0 then
               Show_All (V.Empty_Label);
               Set_Child_Visible (V.Empty_Label, True);
               Set_USize (V.Empty_Event, -1, -1);
               Hide_All (V.Table);
            else
               Hide_All (V.Empty_Label);
               Set_Child_Visible (V.Empty_Label, False);
               Resize (V.Table,
                       Rows => Guint (Scenar_Var'Length) + 1, Columns => 4);

               for J in Scenar_Var'Range loop
                  Row := Guint (J - Scenar_Var'First) + 1;

                  Gtk_New (Button);
                  Gtk_New (Pix, Stock_Properties, Icon_Size_Small_Toolbar);
                  Add (Button, Pix);
                  Attach
                    (V.Table, Button, 0, 1,
                     Row, Row + 1, Xoptions => 0, Yoptions => 0);
                  View_Callback.Connect
                    (Button, Signal_Clicked, Edit_Variable'Access,
                     (View => V, Var => Scenar_Var (J)));
                  Set_Tip (Get_Tooltips (V.Kernel), Button,
                           -"Edit variable properties");

                  Gtk_New (Button);
                  Gtk_New (Pix, Stock_Delete, Icon_Size_Small_Toolbar);
                  Add (Button, Pix);
                  Attach
                    (V.Table, Button, 1, 2,
                     Row, Row + 1, Xoptions => 0, Yoptions => 0);
                  Set_Tip
                    (Get_Tooltips (V.Kernel), Button, -"Delete variable");
                  View_Callback.Connect
                    (Button, Signal_Clicked, Delete_Variable'Access,
                     (View => V, Var => Scenar_Var (J)));

                  Gtk_New (Label, Locale_To_UTF8
                           (External_Reference_Of (Scenar_Var (J))));
                  Set_Alignment (Label, 0.0, 0.5);
                  Attach (V.Table, Label, 2, 3, Row, Row + 1, Xoptions => Fill,
                          Xpadding => 5);

                  Gtk_New (Combo);
                  Set_Editable (Get_Entry (Combo), False);
                  Set_Width_Chars (Get_Entry (Combo), 0);
                  Attach (V.Table, Combo, 3, 4, Row, Row + 1);

                  Add_Possible_Values
                    (Kernel, Get_List (Combo), Scenar_Var (J));
                  Set_Text (Get_Entry (Combo), Value_Of (Scenar_Var (J)));

                  View_Callback.Connect
                    (Combo, Signal_Changed, Variable_Value_Changed'Access,
                     (View => V, Var => Scenar_Var (J)));
               end loop;

               Show_All (V.Table);
            end if;
         end;
      end if;
   end Execute;

   ------------------
   -- On_Open_View --
   ------------------

   procedure On_Open_View
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Scenario : Scenario_View;
      Child    : GPS_MDI_Child;
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Scenario_View_Record'Tag));
      if Child = null then
         Gtk_New (Scenario, Kernel);
         Gtk_New (Child, Scenario,
                  Default_Width => 215,
                  Group         => Group_View,
                  Module        => Scenario_Module_Id);
         Set_Title (Child, -"Scenario View", -"Scenario View");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Left);
      end if;

      Set_Focus_Child (Child);
      Raise_Child (Child);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_View;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child is
   begin
      if Node.Tag.all = "Scenario_View" then
         On_Open_View (MDI, User);
         return Find_MDI_Child_By_Tag (MDI, Scenario_View_Record'Tag);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Scenario_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Scenario_View");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Scenario_Module_Id := new Scenario_Module_Record;
      Register_Module
        (Module      => Scenario_Module_Id,
         Kernel      => Kernel,
         Module_Name => "Scenario_View");
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu
        (Kernel, '/' & (-"Tools") & '/' & (-"Views"),
         -"_Scenario", "", On_Open_View'Access);
   end Register_Module;

end Scenario_Views;

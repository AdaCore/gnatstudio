-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
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

with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Gtk.Button;      use Gtk.Button;
with Gtk.Combo;       use Gtk.Combo;
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.GEntry;      use Gtk.GEntry;
with Gtk.Image;       use Gtk.Image;
with Gtk.Label;       use Gtk.Label;
with Gtk.List;        use Gtk.List;
with Gtk.List_Item;   use Gtk.List_Item;
with Gtk.Handlers;
with Gtk.Stock;       use Gtk.Stock;
with Gtk.Table;       use Gtk.Table;
with Gtk.Tooltips;    use Gtk.Tooltips;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Dialogs;  use Gtkada.Dialogs;

with Prj_API;          use Prj_API;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Variable_Editors; use Variable_Editors;
with Glide_Intl;    use Glide_Intl;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Stringt;  use Stringt;
with Namet;    use Namet;
with Types;    use Types;
with Traces;   use Traces;

package body Scenario_Views is

   Me : constant Debug_Handle := Create ("Scenario_Views");

   procedure Refresh (View : access GObject_Record'Class; Data : GObject);
   --  Callback when the current view of the project has changed

   procedure Add_Possible_Values
     (List : access Gtk_List_Record'Class; Typ : Project_Node_Id);
   --  Add all the possible values for type Typ into the List.

   type Variable_User_Data is record
      View : Scenario_View;
      Var  : Project_Node_Id;
   end record;

   procedure Variable_Value_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      User   : Variable_User_Data);
   --  Called when the value of one of the variables has changed.
   --  This recomputes the scenario view, so that changes are reflected in
   --  other parts of Glide.

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data);
   --  Called when editing a variable (name and possible values)

   procedure Delete_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Variable_User_Data);
   --  Called when removing a variable

   procedure Setup (Data : Variable_User_Data; Id : Handler_Id);
   package View_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Gtk_Widget_Record, Variable_User_Data, Setup);

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
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Table.Initialize
        (View,
         Rows        => 1,
         Columns     => 4,
         Homogeneous => False);
      Set_Col_Spacing (View, 0, 0);
      Set_Col_Spacing (View, 1, 10);

      --  We do not need to connect to "project_changed", since it is always
      --  emitted at the same time as a "project_view_changed", and we do the
      --  same thing in both cases.
      Object_User_Callback.Connect
        (Kernel, Project_View_Changed_Signal,
         Object_User_Callback.To_Marshaller (Refresh'Access), GObject (View));
      Object_User_Callback.Connect
        (Kernel, Variable_Changed_Signal,
         Object_User_Callback.To_Marshaller (Refresh'Access), GObject (View));

      --  Update the viewer with the current project
      Refresh (Kernel, GObject (View));
   end Initialize;

   ----------------------------
   -- Variable_Value_Changed --
   ----------------------------

   procedure Variable_Value_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      User   : Variable_User_Data)
   is
      Value : constant String := Get_Text (Gtk_Entry (GEntry));
   begin
      if Value /= "" then
         String_To_Name_Buffer (External_Reference_Of (User.Var));
         declare
            Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
         begin
            Prj.Ext.Add (Name, Value);
         end;

         User.View.Combo_Is_Open := True;
         Recompute_View (User.View.Kernel);
         User.View.Combo_Is_Open := False;
      end if;
   end Variable_Value_Changed;

   -------------------------
   -- Add_Possible_Values --
   -------------------------

   procedure Add_Possible_Values
     (List : access Gtk_List_Record'Class; Typ : Project_Node_Id)
   is
      Iter : String_List_Iterator := Type_Values (Typ);
      Item : Gtk_List_Item;
   begin
      while not Done (Iter) loop
         --  We know this is a list of static strings
         String_To_Name_Buffer (Data (Iter));
         Gtk_New (Item, Name_Buffer (Name_Buffer'First .. Name_Len));
         Add (List, Item);
         Iter := Next (Iter);
      end loop;
      Show_All (List);
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
      Ext_Variable : constant String :=
        Get_String (External_Reference_Of (Data.Var));
      Str : String_Id;
      Message : constant String :=
        "Doing so will remove all the configurations associated with"
        & ASCII.LF
        & "that variable, except for the currently selected value";

      Response : constant Message_Dialog_Buttons := Message_Dialog
        (Msg           => (-"Are you sure you want to remove the variable ")
           & '"' & Ext_Variable & """?" & ASCII.LF & (-Message),
         Dialog_Type   => Confirmation,
         Buttons       => Button_OK or Button_Cancel,
         Title         => -"Deleting a variable",
         Justification => Justify_Left,
         Parent        => Get_Main_Window (Data.View.Kernel));
   begin
      if Response = Button_OK then
         Name_Len := Ext_Variable'Length;
         Name_Buffer (1 .. Name_Len) := Ext_Variable;
         Str := Prj.Ext.Value_Of (Name_Find);

         Delete_External_Variable
           (Root_Project             => Get_Project (Data.View.Kernel),
            Ext_Variable_Name        => Ext_Variable,
            Keep_Choice              => Get_String (Str),
            Delete_Direct_References => False);
         Variable_Changed (Data.View.Kernel);

         --  Mark all projects in the hierarchy as modified, since they are
         --  potentially all impacted.

         declare
            Iterator : Imported_Project_Iterator := Start
              (Get_Project (Data.View.Kernel), Recursive => True);
         begin
            while Current (Iterator) /= Empty_Node loop
               Set_Project_Modified
                 (Data.View.Kernel, Current (Iterator), True);
               Next (Iterator);
            end loop;
         end;

         --  Recompute the view so that the explorer is updated graphically.
         Recompute_View (Data.View.Kernel);

         Trace (Me, "Delete_Variable: " & Ext_Variable);
      end if;
   end Delete_Variable;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access GObject_Record'Class; Data : GObject) is
      pragma Unreferenced (View);
      V      : constant Scenario_View := Scenario_View (Data);
      Label  : Gtk_Label;
      Combo  : Gtk_Combo;
      Row    : Guint;
      Str    : String_Id;
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

      Child := Children (V);
      Tmp := Widget_List.First (Child);

      while Tmp /= Widget_List.Null_List loop
         Destroy (Widget_List.Get_Data (Tmp));
         Tmp := Widget_List.Next (Tmp);
      end loop;

      Widget_List.Free (Child);


      --  No project view => Clean up the scenario viewer
      if Get_Project_View (V.Kernel) = No_Project then
         Resize (V, Rows => 1, Columns => 4);
         Hide_All (V);

      else
         declare
            Scenar_Var : constant Project_Node_Array :=
              Scenario_Variables (V.Kernel);
         begin
            Resize (V, Rows => Guint (Scenar_Var'Length) + 1, Columns => 4);

            for J in Scenar_Var'Range loop
               Row := Guint (J - Scenar_Var'First) + 1;

               Gtk_New (Button);
               Gtk_New (Pix, Stock_Properties, Icon_Size_Small_Toolbar);
               Add (Button, Pix);
               Attach
                 (V, Button, 0, 1, Row, Row + 1, Xoptions => 0, Yoptions => 0);
               View_Callback.Connect
                 (Button, "clicked",
                  View_Callback.To_Marshaller (Edit_Variable'Access),
                  (View => V, Var => Scenar_Var (J)));
               Set_Tip (Get_Tooltips (V.Kernel), Button,
                        -"Edit variable properties");

               Gtk_New (Button);
               Gtk_New (Pix, Stock_Delete, Icon_Size_Small_Toolbar);
               Add (Button, Pix);
               Attach
                 (V, Button, 1, 2, Row, Row + 1, Xoptions => 0, Yoptions => 0);
               Set_Tip
                 (Get_Tooltips (V.Kernel), Button, -"Delete variable");
               View_Callback.Connect
                 (Button, "clicked",
                  View_Callback.To_Marshaller (Delete_Variable'Access),
                  (View => V, Var => Scenar_Var (J)));

               Gtk_New
                 (Label, Get_String (External_Reference_Of (Scenar_Var (J))));
               Set_Alignment (Label, 0.0, 0.5);
               Attach (V, Label, 2, 3, Row, Row + 1, Xoptions => Fill,
                       Xpadding => 5);

               Gtk_New (Combo);
               Set_Editable (Get_Entry (Combo), False);
               Set_Width_Chars (Get_Entry (Combo), 0);
               Attach (V, Combo, 3, 4, Row, Row + 1);

               Add_Possible_Values
                 (Get_List (Combo), String_Type_Of (Scenar_Var (J)));

               --  The variable necessarily has a current value set in
               --  Prj.Ext, since this is done automatically by the kernel.
               --  Thus we display this value (no need to look at the default
               --  value here).

               Str := External_Reference_Of (Scenar_Var (J));
               pragma Assert
                 (Str /= No_String,
                  "Scenario variable is not an external reference");

               String_To_Name_Buffer (Str);
               Str := Prj.Ext.Value_Of (Name_Find);
               pragma Assert
                 (Str /= No_String, "Value not defined in Prj.Ext");

               String_To_Name_Buffer (Str);
               Set_Text
                 (Get_Entry (Combo),
                  Name_Buffer (Name_Buffer'First .. Name_Len));

               View_Callback.Connect
                 (Get_Entry (Combo),
                  "changed",
                  View_Callback.To_Marshaller (Variable_Value_Changed'Access),
                  (View => V, Var => Scenar_Var (J)));
            end loop;
         end;
         Show_All (V);
      end if;
   end Refresh;

end Scenario_Views;

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

with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Gtk.Button;      use Gtk.Button;
with Gtk.Combo;       use Gtk.Combo;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.GEntry;      use Gtk.GEntry;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk.Label;       use Gtk.Label;
with Gtk.List;        use Gtk.List;
with Gtk.List_Item;   use Gtk.List_Item;
with Gtk.Table;       use Gtk.Table;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;

with Prj_API;          use Prj_API;
with Prj_Manager;      use Prj_Manager;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Variable_Editors; use Variable_Editors;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Stringt;  use Stringt;
with Namet;    use Namet;
with Types;    use Types;

package body Scenario_Views is

   procedure On_Edit_Scenario (View : access Gtk_Widget_Record'Class);
   --  Callback for editing the current scenario

   procedure Refresh (View : access GObject_Record'Class; Data : GObject);
   --  Callback when the current view of the project has changed

   procedure Add_Possible_Values
     (List : access Gtk_List_Record'Class; Typ : Project_Node_Id);
   --  Add all the possible values for type Typ into the List.
   --  ??? This is a duplicate of the one in variable_editors.adb

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

   package View_Callback is new User_Callback
     (Gtk_Widget_Record, Variable_User_Data);

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
         Columns     => 2,
         Homogeneous => False);

      Gtk_New (View.Edit_Button, "Edit Scenario");
      --  Activated only when a project is loaded
      Set_Sensitive (View.Edit_Button, False);
      Attach (View, View.Edit_Button, 0, 2, 0, 1);

      Widget_Callback.Object_Connect
        (View.Edit_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Edit_Scenario'Access), View);

      --  We do not need to connect to "project_changed", since it is always
      --  emitted at the same time as a "project_view_changed", and we do the
      --  same thing in both cases.
      Object_User_Callback.Connect
        (Kernel, "project_view_changed",
         Object_User_Callback.To_Marshaller (Refresh'Access), GObject (View));

      --  Update the viewer with the current project
      Refresh (Kernel, GObject (View));
   end Initialize;

   ----------------------
   -- On_Edit_Scenario --
   ----------------------

   procedure On_Edit_Scenario (View : access Gtk_Widget_Record'Class) is
      V          : constant Scenario_View := Scenario_View (View);
      Edit       : Variable_Edit;
      Scenar_Var : Project_Node_Array :=
        Find_Scenario_Variables (Get_Project (V.Kernel));

   begin
      Gtk_New (Edit, V.Kernel);

      for J in Scenar_Var'Range loop
         Refresh (Edit, Scenar_Var (J));
      end loop;

      Show_All (Edit);
   end On_Edit_Scenario;

   ----------------------------
   -- Variable_Value_Changed --
   ----------------------------

   procedure Variable_Value_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      User   : Variable_User_Data)
   is
      Value : constant String := Get_Chars (Gtk_Entry (GEntry));
   begin
      if Value /= "" then
         String_To_Name_Buffer (External_Reference_Of (User.Var));
         declare
            Name : constant String :=
              Name_Buffer (Name_Buffer'First .. Name_Len);
         begin
            Change_Scenario_Variable (User.View.Kernel, Name, Value);
         end;

         Recompute_View (User.View.Kernel);
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

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access GObject_Record'Class; Data : GObject) is
      V : Scenario_View := Scenario_View (Data);
      Label : Gtk_Label;
      Combo : Gtk_Combo;
      Row : Guint;
      Str : String_Id;
   begin
      --  No project view => Clean up the scenario viewer
      if Get_Project_View (V.Kernel) = No_Project then
         Resize (V, Rows => 1, Columns => 2);
         Hide_All (V);
         Set_Sensitive (V.Edit_Button, False);

      else
         declare
            Scenar_Var : constant Project_Node_Array :=
              Find_Scenario_Variables (Get_Project (V.Kernel));
         begin
            Set_Sensitive (V.Edit_Button, True);
            Resize (V, Rows => Guint (Scenar_Var'Length), Columns => 2);

            for J in Scenar_Var'Range loop
               Row := Guint (J - Scenar_Var'First) + 1;
               Str := External_Reference_Of (Scenar_Var (J));
               String_To_Name_Buffer (Str);
               Gtk_New (Label, Name_Buffer (Name_Buffer'First .. Name_Len));
               Set_Alignment (Label, 0.0, 0.5);
               Attach (V, Label, 0, 1, Row, Row + 1, Xoptions => Fill);

               Gtk_New (Combo);
               Set_Editable (Get_Entry (Combo), False);
               Set_Width_Chars (Get_Entry (Combo), 0);
               Attach (V, Combo, 1, 2, Row, Row + 1);

               Add_Possible_Values
                 (Get_List (Combo), String_Type_Of (Scenar_Var (J)));

               --  Display either the current value of the variable (if set),
               --  or its default value.
               Str := External_Reference_Of (Scenar_Var (J));
               if Str /= No_String then
                  String_To_Name_Buffer (Str);
                  Str := Prj.Ext.Value_Of (Name_Find);
                  if Str /= No_String then
                     String_To_Name_Buffer (Str);
                     Set_Text
                       (Get_Entry (Combo),
                        Name_Buffer (Name_Buffer'First .. Name_Len));
                  else
                     Set_Text (Get_Entry (Combo), "");
                     Display_Expr
                       (Get_Entry (Combo), Value_Of (Scenar_Var (J)));
                  end if;
               end if;

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

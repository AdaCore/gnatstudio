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
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.GEntry;      use Gtk.GEntry;
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
     (View    : access Scenario_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Initialize_Hbox (View, Homogeneous => False, Spacing => 10);

      Gtk_New (View.Edit_Button, "Edit Scenario");
      --  Activated only when a project is loaded
      Set_Sensitive (View.Edit_Button, False);

      Pack_Start (View, View.Edit_Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (View.Edit_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Edit_Scenario'Access), View);

      --  We do not need to connect to "project_changed", since it is always
      --  emitted at the same time as a "project_view_changed", and we do the
      --  same thing in both cases.
      Object_User_Callback.Connect
        (Kernel, "project_view_changed",
         Object_User_Callback.To_Marshaller (Refresh'Access), GObject (View));

      Gtk_New (View.Field, 1024);
      Set_Editable (View.Field, False);
      Pack_Start (View, View.Field, Expand => True, Fill => True);

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

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access GObject_Record'Class; Data : GObject) is
      V : Scenario_View := Scenario_View (Data);
      Str : String_Id;
   begin
      --  No project view => Clean up the tree
      if Get_Project_View (V.Kernel) = No_Project then
         Set_Text (V.Field, "");
         return;
      end if;

      declare
         Vars : Project_Node_Array :=
           Find_Scenario_Variables (Get_Project (V.Kernel));
      begin
         Set_Sensitive (V.Edit_Button, True);
         Set_Text (V.Field, "");

         for Var in Vars'Range loop
            Str := External_Reference_Of (Vars (Var));
            if Str /= No_String then
               String_To_Name_Buffer (Str);
               Str := Prj.Ext.Value_Of  (External_Name => Name_Find);
            end if;

            if Var /= Vars'First then
               Append_Text (V.Field, ", ");
            end if;

            if Str = No_String then
               Append_Text
                 (V.Field,
                  Get_Name_String (Name_Of (Vars (Var))) & "=""");
               Display_Expr (V.Field, Value_Of (Vars (Var)));
               Append_Text (V.Field, """");

            else
               String_To_Name_Buffer (Str);
               Append_Text
                 (V.Field,
                  Get_Name_String (Name_Of (Vars (Var)))
                  & "=""" & Name_Buffer (1 .. Name_Len) & """");
            end if;
         end loop;
      end;
   end Refresh;

end Scenario_Views;

------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2021, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with System;
with GPS.Intl;                 use GPS.Intl;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with Projects;                 use Projects;

with Histories;                use Histories;
with GNATCOLL.Traces;          use GNATCOLL.Traces;

package body Scenario_Selectors is

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.SCENARIO_SELECTORS");

   Selected_Column      : constant := 0;
   Project_Name_Column  : constant := 1;
   Project_Column_Types : constant GType_Array :=
     (Selected_Column     => GType_Boolean,
      Project_Name_Column => GType_String);

   Var_Name_Column      : constant := 1;
   Var_Column_Types     : constant GType_Array :=
     (Selected_Column     => GType_Boolean,
      Var_Name_Column     => GType_String);

   type Project_Type_Array_Access is access Project_Type_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Type_Array, Project_Type_Array_Access);

   procedure Project_Set
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Selected : Boolean;
      Project  : Project_Type);
   --  Add a new project in the tree model

   procedure Add_Project_Recursive
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Project  : Project_Type);
   --  Add project and all its importing projects to the selector

   procedure Project_Selected
     (Selector  : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues);
   --  Called when a project has been selected, to make sure we select all its
   --  occurrences.

   procedure Var_Selected
     (Selector  : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues);
   --  Called when a new variable has been selected

   procedure Reset_Selected_Status
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Project  : Project_Type;
      Selected : Boolean);
   --  Changes the selected status for all the lines that reference Project,
   --  Starting at line Iter.

   procedure Select_All_Project (Selector : access Gtk_Widget_Record'Class);
   --  Select all the projects in Selector

   procedure Show_Variables (Selector : access Scenario_Selector_Record'Class);
   --  Show all the scenario variables

   procedure Select_All_Var (Selector : access Gtk_Widget_Record'Class);
   --  Select all the variables in Selector

   procedure Variable_Selection
     (Selector : access Scenario_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Selected : Boolean);
   --  Called when directly selecting a child node, to select or unselect
   --  all the values.

   procedure Toggle_Hierarchy (Selector : access Gtk_Widget_Record'Class);
   --  Show a different view of the project selector

   package Scenario_Variable_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Scenario_Variable,
      "="          => "=");

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector : out Project_Selector;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Ref_Project : Project_Type) is
   begin
      Selector := new Project_Selector_Record;
      Initialize (Selector, Kernel, Ref_Project);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Selector : access Project_Selector_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Ref_Project : Project_Type)
   is
      View          : Gtk_Tree_View;
      Col           : Gtk_Tree_View_Column;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Ignore        : Gint;
      Scrolled      : Gtk_Scrolled_Window;
      pragma Unreferenced (Ignore);

   begin
      Initialize_Vbox (Selector, Homogeneous => False);

      Gtk_New (Selector.Show_As_Hierarchy, -"Show as hierarchy");
      Pack_Start (Selector, Selector.Show_As_Hierarchy, Expand => False);
      Associate
        (Get_History (Kernel).all, "scenario_selector_show_as_hierarchy",
         Selector.Show_As_Hierarchy);
      Widget_Callback.Object_Connect
        (Selector.Show_As_Hierarchy, Signal_Toggled,
         Toggle_Hierarchy'Access, Selector);

      Gtk_New (Scrolled);
      Pack_Start (Selector, Scrolled, Expand => True, Fill => True);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);

      Selector.Ref_Project := Ref_Project;
      Selector.Kernel      := Kernel_Handle (Kernel);

      Gtk_New (View);
      Add (Scrolled, View);

      Gtk_New (Selector.Model, Project_Column_Types);
      Set_Model (View, +Selector.Model);

      Gtk_New (Col);
      Set_Clickable (Col, True);
      Ignore := Append_Column (View, Col);
      Widget_Callback.Object_Connect
        (Col, Signal_Clicked,
         Select_All_Project'Access, Slot_Object => Selector);

      Gtk_New (Toggle_Render);
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", Selected_Column);
      Widget_Callback.Object_Connect
        (Toggle_Render, Signal_Toggled,
         Project_Selected'Access,
         Slot_Object => Selector);

      Gtk_New (Col);
      Set_Clickable (Col, True);
      Set_Sort_Column_Id (Col, Project_Name_Column);
      Ignore := Append_Column (View, Col);
      Set_Title (Col, -"Project");

      Gtk_New (Text_Render);
      Pack_Start (Col, Text_Render, True);
      Add_Attribute (Col, Text_Render, "text", Project_Name_Column);

      Add_Project_Recursive (Selector, Null_Iter, Get_Project (Kernel));
   end Initialize;

   ----------------------
   -- Toggle_Hierarchy --
   ----------------------

   procedure Toggle_Hierarchy (Selector : access Gtk_Widget_Record'Class) is
      S : constant Project_Selector := Project_Selector (Selector);
   begin
      Clear (S.Model);
      Add_Project_Recursive (S, Null_Iter, Get_Project (S.Kernel));
   end Toggle_Hierarchy;

   ------------------------
   -- Select_All_Project --
   ------------------------

   procedure Select_All_Project (Selector : access Gtk_Widget_Record'Class) is
      S : constant Project_Selector := Project_Selector (Selector);
      Selected : constant Boolean := S.Select_All;

      procedure Select_Recursive (Iter : Gtk_Tree_Iter);
      --  Select or unselect Iter, its siblings and all its children

      ----------------------
      -- Select_Recursive --
      ----------------------

      procedure Select_Recursive (Iter : Gtk_Tree_Iter) is
         It    : Gtk_Tree_Iter := Iter;
      begin
         while It /= Null_Iter loop
            if Get_String (S.Model, It, Project_Name_Column) /=
              S.Ref_Project.Name
            then
               S.Model.Set (It, Selected_Column, Selected);
            end if;

            Select_Recursive (Children (S.Model, It));
            Next (S.Model, It);
         end loop;
      end Select_Recursive;

   begin
      --  We must use a boolean variable for this, we cannot simply have a look
      --  at the first line, since it might be the Ref_Project, whose status
      --  never changes.
      S.Select_All := not S.Select_All;
      Select_Recursive (Get_Iter_First (S.Model));
   end Select_All_Project;

   ---------------------------
   -- Reset_Selected_Status --
   ---------------------------

   procedure Reset_Selected_Status
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Project  : Project_Type;
      Selected : Boolean)
   is
      It : Gtk_Tree_Iter := Iter;
   begin
      while It /= Null_Iter loop
         if Get_String (Selector.Model, It, Project_Name_Column)
           = Project.Name
         then
            Selector.Model.Set (It, Selected_Column, Selected);
         end if;

         Reset_Selected_Status
           (Selector, Children (Selector.Model, It), Project, Selected);
         Next (Selector.Model, It);
      end loop;
   end Reset_Selected_Status;

   ----------------------
   -- Project_Selected --
   ----------------------

   procedure Project_Selected
     (Selector  : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues)
   is
      S           : constant Project_Selector := Project_Selector (Selector);
      Iter        : Gtk_Tree_Iter;
      Path_String : constant String := Get_String (Nth (Params, 1));
      Project     : Project_Type;
      Selected    : Boolean;

   begin
      Iter := Get_Iter_From_String (S.Model, Path_String);

      --  Can't unselect the reference project
      if Iter /= Null_Iter then
         Project := Get_Registry (S.Kernel).Tree.Project_From_Name
           (Get_String (S.Model, Iter, Project_Name_Column));

         if Project /= S.Ref_Project then
            Selected := Get_Boolean (S.Model, Iter, Selected_Column);
            S.Model.Set (Iter, Selected_Column, not Selected);

            if Get_Active (S.Show_As_Hierarchy) then
               Reset_Selected_Status
                 (S, Get_Iter_First (S.Model), Project, not Selected);
            end if;
         end if;
      end if;
   end Project_Selected;

   ---------------------------
   -- Add_Project_Recursive --
   ---------------------------

   procedure Add_Project_Recursive
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Project  : Project_Type)
   is
      package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (String, Ada.Strings.Hash, "=", "=");

      Already_Processed : String_Sets.Set;
      --  Projects that are already processed

      procedure Process (Iter     : Gtk_Tree_Iter;
                         Project  : Project_Type);
      --  Auxiliary recursion function for the case where the selector
      --  is set to show projects as a hierarchy.

      procedure Process  (Iter     : Gtk_Tree_Iter;
                          Project  : Project_Type)
      is
         It       : Gtk_Tree_Iter := Null_Iter;
         Iterator : GNATCOLL.Projects.Project_Iterator;
         Name : constant String := Project.Name;
      begin
         --  We need to properly handle limited-with statements in the
         --  project, which effectively create an infinite project tree.
         --  Thus, we make sure before inserting a node that it isn't already
         --  present in the current tree path.
         if Already_Processed.Contains (Name) then
            return;
         end if;
         Already_Processed.Insert (Name);

         Iterator := Start (Project, Recursive => True, Direct_Only => True);

         Append (Selector.Model, It, Iter);
         Project_Set
           (Selector, It, Project = Selector.Ref_Project, Project);

         while Current (Iterator) /= No_Project loop
            if Current (Iterator) /= Project then
               Process (It, Current (Iterator));
            end if;
            Next (Iterator);
         end loop;
      end Process;

   begin
      if Get_Active (Selector.Show_As_Hierarchy) then
         Process (Iter, Project);

      else
         declare
            It       : Gtk_Tree_Iter := Null_Iter;
            Iterator : GNATCOLL.Projects.Project_Iterator := Start
              (Project, Recursive => True);
         begin
            while Current (Iterator) /= No_Project loop
               Append (Selector.Model, It, Iter);
               Project_Set (Selector, It,
                            Current (Iterator) = Selector.Ref_Project,
                            Current (Iterator));
               Next (Iterator);
            end loop;
         end;
      end if;
   end Add_Project_Recursive;

   -----------------
   -- Project_Set --
   -----------------

   procedure Project_Set
     (Selector : access Project_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Selected : Boolean;
      Project  : Project_Type)
   is
      procedure Internal
        (Tree, Iter : System.Address;
         Col1 : Gint; Value1 : String;
         Col2 : Gint; Value2 : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr_int");

   begin
      Internal
        (Get_Object (Selector.Model), Iter'Address,
         Project_Name_Column, Project.Name & ASCII.NUL,
         Selected_Column,     Boolean'Pos (Selected));
   end Project_Set;

   --------------------------------------------------------------------------
   --   The scenario selector                                              --
   --------------------------------------------------------------------------

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector : out Scenario_Selector;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Selector := new Scenario_Selector_Record;
      Initialize (Selector, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Selector : access Scenario_Selector_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      View          : Gtk_Tree_View;
      Col           : Gtk_Tree_View_Column;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Ignore : Gint;
      pragma Unreferenced (Ignore);

   begin
      Gtk.Scrolled_Window.Initialize (Selector);
      Set_Policy (Selector, Policy_Never, Policy_Automatic);

      Selector.Kernel := Kernel_Handle (Kernel);

      Gtk_New (View);
      Add (Selector, View);

      Gtk_New (Selector.Model, Var_Column_Types);
      Set_Model (View, +Selector.Model);

      Gtk_New (Col);
      Set_Clickable (Col, True);
      Ignore := Append_Column (View, Col);
      Widget_Callback.Object_Connect
        (Col, Signal_Clicked, Select_All_Var'Access, Slot_Object => Selector);

      Gtk_New (Toggle_Render);
      Pack_Start (Col, Toggle_Render, False);
      Add_Attribute (Col, Toggle_Render, "active", Selected_Column);
      Widget_Callback.Object_Connect
        (Toggle_Render, Signal_Toggled,
         Var_Selected'Access,
         Slot_Object => Selector);

      Gtk_New (Col);
      Ignore := Append_Column (View, Col);
      Set_Title (Col, -"Scenario");

      Gtk_New (Text_Render);
      Pack_Start (Col, Text_Render, True);
      Add_Attribute (Col, Text_Render, "text", Var_Name_Column);

      Show_Variables (Selector);
      View.Expand_All;
   end Initialize;

   --------------------
   -- Show_Variables --
   --------------------

   procedure Show_Variables
     (Selector : access Scenario_Selector_Record'Class)
   is
      Vars : constant Scenario_Variable_Array :=
        Scenario_Variables (Selector.Kernel);
      Iter, Child : Gtk_Tree_Iter;
   begin
      for Var of Vars loop
         Append (Selector.Model, Iter, Null_Iter);
         Set_And_Clear
           (Selector.Model, Iter,
            (Selected_Column, Var_Name_Column),
            (0 => As_Boolean (True),
             1 => As_String  (External_Name (Var))));

         declare
            Values : GNAT.Strings.String_List :=
                       Get_Registry (Selector.Kernel).Tree.Possible_Values_Of
                       (Var);
         begin
            for Val in Values'Range loop
               Append (Selector.Model, Child, Iter);
               Set_And_Clear
                 (Selector.Model, Child,
                  (Selected_Column, Var_Name_Column),
                  (0 => As_Boolean (True),
                   1 => As_String  (Values (Val).all)));
            end loop;

            Free (Values);
         end;

      end loop;
   end Show_Variables;

   --------------------
   -- Select_All_Var --
   --------------------

   procedure Select_All_Var (Selector : access Gtk_Widget_Record'Class) is
      S : constant Scenario_Selector := Scenario_Selector (Selector);
      It : Gtk_Tree_Iter := Get_Iter_First (S.Model);
   begin
      while It /= Null_Iter loop
         Variable_Selection (S, It, S.Select_All);
         Next (S.Model, It);
      end loop;

      S.Select_All := not S.Select_All;
   end Select_All_Var;

   ------------------------
   -- Variable_Selection --
   ------------------------

   procedure Variable_Selection
     (Selector : access Scenario_Selector_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Selected : Boolean)
   is

      Child     : Gtk_Tree_Iter;
      Ext_Name  : constant String := Selector.Model.Get_String
        (Iter, Var_Name_Column);
      Variable  : constant Scenario_Variable :=
                    Get_Registry (Selector.Kernel).Tree.Scenario_Variables
                    (External_Name => Ext_Name);
      Cur_Value : constant String := Value (Variable);
   begin
      Selector.Model.Set (Iter, Selected_Column, Selected);

      Child := Children (Selector.Model, Iter);

      while Child /= Null_Iter loop

         --  Don't unselect the current scenario variable's value when
         --  unselecting the whole variable.

         if Selected
           or else Selector.Model.Get_String
             (Child, Var_Name_Column) /= Cur_Value
         then
            Selector.Model.Set (Child, Selected_Column, Selected);
         end if;

         Next (Selector.Model, Child);
      end loop;
   end Variable_Selection;

   ------------------
   -- Var_Selected --
   ------------------

   procedure Var_Selected
     (Selector  : access Gtk_Widget_Record'Class;
      Params    : Glib.Values.GValues)
   is
      S                  : constant Scenario_Selector :=
        Scenario_Selector (Selector);
      It                 : Gtk_Tree_Iter;
      Path_String        : constant String := Get_String (Nth (Params, 1));
      Iter               : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (S.Model, Path_String);
      Selected           : constant Boolean :=
        Get_Boolean (S.Model, Iter, Selected_Column);
      Num_Selected_Child : Gint := 0;

   begin
      --  Are we selecting a variable ?

      if Children (S.Model, Iter) /= Null_Iter then
         Variable_Selection (S, Iter, not Selected);

      --  Are we selecting a variable value ? Unselect the variable

      else
         It := Children (S.Model, Parent (S.Model, Iter));
         while It /= Null_Iter loop
            if Get_Boolean (S.Model, It, Selected_Column) then
               Num_Selected_Child := Num_Selected_Child + 1;
            end if;
               Next (S.Model, It);
         end loop;

         if Selected and then Num_Selected_Child = 1 then
            S.Model.Set (Iter, Selected_Column, True);
         else
            declare
               Var_Iter : constant Gtk_Tree_Iter := S.Model.Parent (Iter);

            begin
               S.Model.Set
                 (Iter,
                  Column => Selected_Column,
                  Value  => not Selected);

               --  If all the values are now selected, select the scenario
               --  variable too.

               if S.Model.Get_Boolean (Iter, Selected_Column)
                 and then
                   Num_Selected_Child = S.Model.N_Children (Var_Iter) - 1
               then
                  S.Model.Set
                    (Var_Iter,
                     Column => Selected_Column,
                     Value  => True);
               else
                  S.Model.Set
                    (Var_Iter,
                     Column => Selected_Column,
                     Value  => False);
               end if;
            end;
         end if;
      end if;
   end Var_Selected;

   -----------
   -- Start --
   -----------

   function Start (Selector : access Project_Selector_Record'Class)
      return Project_Iterator
   is
      Tmp : Project_Type_Array_Access := new Project_Type_Array (1 .. 1);

      procedure Add_Recursive (Iter : Gtk_Tree_Iter);
      --  Add the project pointed to by Iter, if not already in Tmp

      -------------------
      -- Add_Recursive --
      -------------------

      procedure Add_Recursive (Iter : Gtk_Tree_Iter) is
         It    : Gtk_Tree_Iter := Iter;
         Prj   : Project_Type;
         Found : Boolean := False;
         T     : Project_Type_Array_Access;
      begin
         while It /= Null_Iter loop
            if Get_Boolean (Selector.Model, It, Selected_Column) then
               Prj := Get_Registry (Selector.Kernel).Tree.Project_From_Name
                 (Get_String (Selector.Model, It, Project_Name_Column));

               for P in Tmp'Range loop
                  if Tmp (P) = Prj then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  T := Tmp;
                  Tmp := new Project_Type_Array (Tmp'First .. Tmp'Last + 1);
                  Tmp (T'Range) := T.all;
                  Unchecked_Free (T);
                  Tmp (Tmp'Last) := Prj;
               end if;
            end if;

            Add_Recursive (Children (Selector.Model, It));
            Next (Selector.Model, It);
         end loop;
      end Add_Recursive;

   begin
      Tmp (1) := Selector.Ref_Project;
      Add_Recursive (Get_Iter_First (Selector.Model));

      declare
         Result : Project_Iterator (Tmp'Length);
      begin
         Result := (Num_Projects  => Tmp'Length,
                    Project       => Tmp.all,
                    Current       => Tmp'First);
         Unchecked_Free (Tmp);
         return Result;
      end;
   end Start;

   -----------
   -- Count --
   -----------

   function Count (Iter : Project_Iterator) return Natural is
   begin
      return Iter.Num_Projects;
   end Count;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Project_Iterator) is
   begin
      Iter.Current := Iter.Current + 1;
   end Next;

   -------------
   -- Current --
   -------------

   function Current (Iter : Project_Iterator) return Project_Type is
   begin
      if Iter.Current <= Iter.Project'Last then
         return Iter.Project (Iter.Current);
      else
         return No_Project;
      end if;
   end Current;

   -------------------
   -- Get_Scenarios --
   -------------------

   function Get_Scenarios
     (Selector : not null access Scenario_Selector_Record'Class)
      return Scenario_Variable_Array
   is
      Variable_Iter : Gtk_Tree_Iter := Get_Iter_First (Selector.Model);
      Value_Iter    : Gtk_Tree_Iter;
      Scenarios     : Scenario_Variable_Lists.List;
   begin
      --  Iterate over all the scenario variables displayed in the tree

      while Variable_Iter /= Null_Iter loop
         declare
            Ext_Name   : constant String :=
                           Get_String
                             (Selector.Model, Variable_Iter, Var_Name_Column);
            Variable   : Scenario_Variable := Get_Registry
              (Selector.Kernel).Tree.Scenario_Variables (Ext_Name);
         begin
            --  If the scenario variable is not selected itself, iterate over
            --  all its possible values and create a scenario variable for
            --  each value that is selected.

            if not Selector.Model.Get_Boolean
              (Variable_Iter, Selected_Column)
            then
               Value_Iter := Selector.Model.Children (Variable_Iter);

               while Value_Iter /= Null_Iter loop

                  if Selector.Model.Get_Boolean
                    (Value_Iter, Selected_Column)
                  then
                     Set_Value
                       (Variable,
                        Selector.Model.Get_String
                          (Value_Iter, Var_Name_Column));
                     Scenarios.Append (Variable);
                  end if;

                  Next (Selector.Model, Value_Iter);
               end loop;
            end if;

            Next (Selector.Model, Variable_Iter);
         end;
      end loop;

      declare
         Scenarios_Arr : Scenario_Variable_Array
           (1 .. Integer (Scenarios.Length));
         J             : Positive := Scenarios_Arr'First;
      begin
         for Scenario of Scenarios loop
            declare
               Var_Name : constant String := External_Name (Scenario);
               Val      : constant String := Value (Scenario);
            begin
               Trace (Me, Val & " is selected for variable " & Var_Name);
               Scenarios_Arr (J) := Scenario;
               J := J + 1;
            end;
         end loop;

         return Scenarios_Arr;
      end;
   end Get_Scenarios;

end Scenario_Selectors;

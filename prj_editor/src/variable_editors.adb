------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Glib.Object;              use Glib.Object;
with Glib;                     use Glib;
with Glib.Convert;             use Glib.Convert;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GUI_Utils;                use GUI_Utils;
with Gtkada.Dialogs;           use Gtkada.Dialogs;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with GNATCOLL.Traces;                   use GNATCOLL.Traces;

package body Variable_Editors is

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.Variable_Editors");

   New_Value_Name : constant String := -"<Enter value name>";
   --  Name used for the new variables

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
   --  Convenient function to populate the tree

   procedure New_Variable (Editor : access Gtk_Widget_Record'Class);
   procedure Rename_Variable (Editor : access Gtk_Widget_Record'Class);
   procedure Delete_Variable (Editor : access Gtk_Widget_Record'Class);
   --  Callback for the buttons at the bottom of the possible values list

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
      procedure Set
        (Tree, Iter : System.Address;
         Col1  : Gint; Value1 : Gint;
         Col2  : Gint; Value2 : String;
         Col3  : Gint; Value3 : Gint);
      pragma Import (C, Set, "ada_gtk_tree_store_set_int_ptr_int");

      procedure Set2
        (Tree, Iter : System.Address;
         Col4  : Gint; Value4 : String);
      pragma Import (C, Set2, "ada_gtk_tree_store_set_ptr");

   begin
      Set
        (Get_Object (Tree_Store), Iter'Address,
         Default_Value_Column,  Boolean'Pos (Is_Default),
         Value_Column,          Locale_To_UTF8 (Value) & ASCII.NUL,
         Editable_Column,       Boolean'Pos (Is_Editable));
      Set2
        (Get_Object (Tree_Store), Iter'Address,
         Initial_Value_Column,  Locale_To_UTF8 (Value) & ASCII.NUL);
   end Variable_Editor_Set;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Var    : Scenario_Variable :=  No_Variable;
      Title  : String)
   is
      Index           : Natural := 0;
      S               : chars_ptr;
      Col             : Gtk_Tree_View_Column;
      Toggle_Renderer : Gtk_Cell_Renderer_Toggle;

      Ignore          : Gint;
      Ignore_Widget   : Gtk_Widget;
      pragma Unreferenced (Ignore, Ignore_Widget);

      Iter            : Gtk_Tree_Iter;

   begin
      Editor := new New_Var_Edit_Record;
      Editor.Var := Var;
      New_Variable_Editor_Pkg.Initialize (Editor, Title, Kernel);

      if Var /= No_Variable then
         Set_Text (Editor.Label58, -("Rename to:"));
      end if;

      Editor.Set_Screen (Get_Main_Window (Kernel).Get_Screen);

      Set_Mode (Get_Selection (Editor.Values_List), Selection_Single);
      Set_Enable_Search (Editor.Values_List, False);

      --  Create the model that contains the data to show in the tree

      Gtk_New (Editor.Model, Column_Types);
      Set_Model (Editor.Values_List, +Editor.Model);

      --  Create the cell renderers that are needed to display the tree view

      Gtk_New (Editor.Editable_Renderer);

      Gtk_New (Toggle_Renderer);
      Set_Radio_And_Callback
        (Editor.Model, Toggle_Renderer, Default_Value_Column);

      --  Add the columns to the tree view, and associated them with the
      --  appropriate cell renderers.

      Gtk_New (Col);
      Ignore := Append_Column (Editor.Values_List, Col);
      Set_Title (Col, -"Default");
      Pack_Start (Col, Toggle_Renderer, False);

      Add_Attribute (Col, Toggle_Renderer, "active", Default_Value_Column);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.Values_List, Col);
      Set_Title (Col, -"Value");
      Pack_Start (Col, Editor.Editable_Renderer, True);
      Add_Attribute (Col, Editor.Editable_Renderer, "text", Value_Column);
      Add_Attribute
        (Col, Editor.Editable_Renderer, "editable", Editable_Column);

      Set_Editable_And_Callback
        (Editor.Model, Editor.Editable_Renderer, Value_Column);

      --  Add the dialog buttons at the bottom. This is done so that Run can be
      --  called on the dialog.

      Ignore_Widget := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Ignore_Widget := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);

      Widget_Callback.Object_Connect
        (Editor.New_Variable, Signal_Clicked, New_Variable'Access, Editor);
      Widget_Callback.Object_Connect
        (Editor.Rename_Variable, Signal_Clicked,
         Rename_Variable'Access, Editor);
      Widget_Callback.Object_Connect
        (Editor.Delete_Variable, Signal_Clicked,
         Delete_Variable'Access, Editor);

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
            Editor.Variable_Name.Append_Text (S2 (S2'First .. J - 1));
            Index := Index + 1;
         end;
      end loop;

      --  Fill the information for the variable

      if Var /= No_Variable then
         declare
            Values : GNAT.Strings.String_List :=
              Get_Registry (Kernel).Tree.Possible_Values_Of (Var);
         begin
            Set_Text
              (Gtk_Entry (Editor.Variable_Name.Get_Child),
               External_Name (Var));

            for E in Values'Range loop
               Append (Editor.Model, Iter, Null_Iter);
               Variable_Editor_Set
                 (Editor.Model, Iter,
                  Is_Default  => External_Default (Var) = Values (E).all,
                  Value       => Values (E).all,
                  Is_Editable => True);
            end loop;

            Free (Values);
         end;
      end if;
   end Gtk_New;

   ------------------
   -- New_Variable --
   ------------------

   procedure New_Variable (Editor : access Gtk_Widget_Record'Class) is
      E           : constant New_Var_Edit := New_Var_Edit (Editor);
      Iter, Iter2 : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
   begin
      --  Add a new entry. This will become the default value if this is also
      --  the first one in the list.

      Iter2 := Get_Iter_First (E.Model);
      Append (E.Model, Iter, Null_Iter);
      Variable_Editor_Set
        (E.Model, Iter,
         Is_Default  => Iter2 = Null_Iter,
         Value       => New_Value_Name,
         Is_Editable => True);

      --  Select the new entry in editing mode

      Path := Get_Path (E.Model, Iter);
      E.Values_List.Scroll_To_Cell
        (Path      => Path,
         Column    => null,
         Use_Align => False,
         Row_Align => 0.0,
         Col_Align => 0.0);
      E.Values_List.Set_Cursor_On_Cell
        (Path          => Path,
         Focus_Column  => Get_Column (E.Values_List, Value_Column),
         Focus_Cell    => null,
         Start_Editing => True);
      Path_Free (Path);
   end New_Variable;

   ---------------------
   -- Rename_Variable --
   ---------------------

   procedure Rename_Variable (Editor : access Gtk_Widget_Record'Class) is
      E         : constant New_Var_Edit := New_Var_Edit (Editor);
      Selection : constant Gtk_Tree_Selection :=
                    Get_Selection (E.Values_List);
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;

   begin
      Get_Selected (Selection, M, Iter);
      E.Model := -M;

      if Iter /= Null_Iter then
         declare
            Path : constant Gtk_Tree_Path := Get_Path (E.Model, Iter);

         begin
            Set_Cursor
              (E.Values_List,
               Path          => Path,
               Focus_Column  => Get_Column (E.Values_List, Value_Column),
               Start_Editing => True);
            Path_Free (Path);
         end;
      end if;
   end Rename_Variable;

   ---------------------
   -- Delete_Variable --
   ---------------------

   procedure Delete_Variable (Editor : access Gtk_Widget_Record'Class) is
      E         : constant New_Var_Edit := New_Var_Edit (Editor);
      Selection : constant Gtk_Tree_Selection :=
                    Get_Selection (E.Values_List);
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;

   begin
      Get_Selected (Selection, M, Iter);
      E.Model := -M;

      if Iter /= Null_Iter then
         Remove (E.Model, Iter);
      end if;
   end Delete_Variable;

   ---------------------
   -- Update_Variable --
   ---------------------

   function Update_Variable
     (Editor : access New_Var_Edit_Record) return Boolean
   is
      New_Name : constant String :=
                   Get_Active_Text (Editor.Variable_Name);
      Ada_Name : String (New_Name'Range);
      Changed  : Boolean := False;
      Iter     : Gtk_Tree_Iter;
      Index    : Natural;
      Found    : Boolean;
      Num_Rows : Natural := 0;
      Ignore   : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);

   begin
      if New_Name = "" then
         Ignore := Message_Dialog
           (Msg     => -"You must specify a name for the variable",
            Buttons => Button_OK,
            Parent  => Gtk_Window (Editor));
         return False;
      end if;

      --  Convert the name of the local variable in the project file to a
      --  valid Ada identifier. Any character is allowed for the external
      --  reference itself.

      Index := Ada_Name'First;
      for N in New_Name'Range loop
         if (N = New_Name'First and then not Is_Letter (New_Name (N)))
           or else (not Is_Alphanumeric (New_Name (N))
                    and then New_Name (N) /= '_')
         then
            if N = New_Name'First
              or else New_Name (N - 1) = '_'
            then
               Ada_Name (Index) := 'A';
            else
               Ada_Name (Index) := '_';
            end if;
         else
            if N /= New_Name'First
              and then New_Name (N) = '_'
              and then New_Name (N - 1) = '_'
            then
               Ada_Name (Index) := 'A';
            else
               Ada_Name (Index) := New_Name (N);
            end if;
         end if;
         Index := Index + 1;
      end loop;

      Iter := Get_Iter_First (Editor.Model);

      while Iter /= Null_Iter loop
         if Get_String (Editor.Model, Iter, Value_Column) /=
           New_Value_Name
         then
            Num_Rows := Num_Rows + 1;
         end if;
         Next (Editor.Model, Iter);
      end loop;

      if Num_Rows = 0 then
         Ignore := Message_Dialog
           (Msg     => -"You must specify some possible values",
            Buttons => Button_OK,
            Parent  => Gtk_Window (Editor));
         return False;
      end if;

      if Editor.Var /= No_Variable
        and then External_Name (Editor.Var) /= New_Name
      then
         declare
            Vars : constant Scenario_Variable_Array :=
              Scenario_Variables (Editor.Kernel);
         begin
            for V in Vars'Range loop
               if New_Name = External_Name (Vars (V)) then
                  Ignore := Message_Dialog
                    (Msg     => -"There is already a variable with this name",
                     Buttons => Button_OK,
                     Parent  => Gtk_Window (Editor));
                  return False;
               end if;
            end loop;
         end;
      end if;

      --  Create the variable if necessary

      if Editor.Var = No_Variable then
         Editor.Var := Get_Project (Editor.Kernel).Create_Scenario_Variable
           (Name          => Ada_Name,
            Type_Name     => Ada_Name & "_Type",
            External_Name => New_Name);

      --  Rename the value appropriately (this has to be done separately, so
      --  that the case statements are changed appropriately).

      else
         Iter := Get_Iter_First (Editor.Model);
         while Iter /= Null_Iter loop
            Num_Rows := Num_Rows + 1;

            declare
               Old_Val : constant String :=
                           Get_String
                             (Editor.Model, Iter, Initial_Value_Column);
               New_Val : constant String :=
                           Get_String (Editor.Model, Iter, Value_Column);
            begin
               if Old_Val /= New_Val then
                  Trace (Me, "Renaming value for variable "
                         & External_Name (Editor.Var)
                         & " from " & Old_Val & " to " & New_Val);

                  Get_Registry (Editor.Kernel).Tree.Rename_Value
                    (External_Name => External_Name (Editor.Var),
                     Old_Value     => Old_Val,
                     New_Value     => New_Val);
                  Changed := True;
               end if;
            end;

            Next (Editor.Model, Iter);
         end loop;

         --  Delete the values that no longer exist

         declare
            Values : GNAT.Strings.String_List :=
              Get_Registry (Editor.Kernel).Tree.Possible_Values_Of
                (Editor.Var);
         begin
            for E in Values'Range loop
               Found := False;
               Iter := Get_Iter_First (Editor.Model);

               while Iter /= Null_Iter loop
                  if Get_String (Editor.Model, Iter, Value_Column) =
                    Values (E).all
                  then
                     Found := True;
                     exit;
                  end if;

                  Next (Editor.Model, Iter);
               end loop;

               if not Found then
                  Get_Registry (Editor.Kernel).Tree.Remove_Value
                    (External_Name (Editor.Var), Values (E).all);
                  Changed := True;
               end if;
            end loop;

            Free (Values);
         end;
      end if;

      --  Add the new values

      declare
         New_Values : GNAT.Strings.String_List (1 .. Num_Rows);
         Num_Values : Natural := New_Values'First - 1;
      begin
         Iter := Get_Iter_First (Editor.Model);

         while Iter /= Null_Iter loop
            Found := False;

            declare
               Name    : constant String := Get_String
                 (Editor.Model, Iter, Value_Column);
               Default : constant Boolean := Get_Boolean
                 (Editor.Model, Iter, Default_Value_Column);
               Values  : GNAT.Strings.String_List :=
                 Get_Registry (Editor.Kernel).Tree.Possible_Values_Of
                    (Editor.Var);

            begin
               if Name /= New_Value_Name then
                  for V in Values'Range loop
                     if Name = Values (V).all then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Num_Values := Num_Values + 1;
                     New_Values (Num_Values) := new String'(Name);
                     Trace (Me, "Adding new value " & Name & " for "
                            & External_Name (Editor.Var));
                  end if;

                  if Default then
                     declare
                        Expr : constant String :=
                          External_Default (Editor.Var);
                     begin
                        if Expr = "" or else Expr /= Name then
                           Changed := True;

                           Trace (Me, "Setting default value of "
                                  & External_Name (Editor.Var)
                                  & " to " & Name);
                           Get_Registry (Editor.Kernel).Tree.Set_Default_Value
                             (External_Name (Editor.Var), Name);
                        end if;
                     end;
                  end if;
               end if;

               Free (Values);
            end;

            Next (Editor.Model, Iter);
         end loop;

         if Num_Values >= New_Values'First then
            Get_Registry (Editor.Kernel).Tree.Add_Values
              (Editor.Var,
               New_Values (New_Values'First .. Num_Values));
            Free (New_Values);
            Changed := True;
         end if;
      end;

      --  Has the variable been renamed ? This should be done last, so that
      --  the previous calls above work with the old name

      if External_Name (Editor.Var) /= New_Name then
         Trace (Me, "Renaming variable "
                & External_Name (Editor.Var) & " to " & New_Name);
         Get_Registry (Editor.Kernel).Tree.Change_External_Name
           (Editor.Var, New_Name);
         Changed := True;
      end if;

      if Editor.Var /= No_Variable then
         --  ??? The variable is null, we shouldn't do anything. Maybe the
         --  above test should be "/="

         if External_Default (Editor.Var) /= "" then
            Set_Value (Editor.Var, External_Default (Editor.Var));
         else
            Iter := Get_Iter_First (Editor.Model);
            Set_Value (Editor.Var,
                       Get_String (Editor.Model, Iter, Value_Column));
         end if;
         Get_Registry (Editor.Kernel).Tree.Change_Environment
           ((1 => Editor.Var));
      end if;

      if Changed then
         --  Save the project if the scenario variables have changed
         Changed := Save_Project
           (Kernel    => Editor.Kernel,
            Project   => Editor.Kernel.Get_Project_Tree.Root_Project,
            Recursive => False);
         Variable_Changed_Hook.Run (Editor.Kernel);
      end if;

      return True;

   exception
      when E : others =>
         Trace (Me, E);
         Ignore := Message_Dialog
           (Msg     => -"Failed to create the scenario variable",
            Buttons => Button_OK,
            Parent  => Gtk_Window (Editor));
         return False;
   end Update_Variable;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Variable_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Get_Kernel (Context.Context),
               Title => -"Creating a new variable");
      Show_All (Edit);

      while Run (Edit) = Gtk_Response_OK
        and then not Update_Variable (Edit)
      loop
         null;
      end loop;

      Destroy (Edit);
      return Commands.Success;
   end Execute;

end Variable_Editors;

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

with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Gtk.Button;          use Gtk.Button;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.Label;           use Gtk.Label;
with Gtk.Table;           use Gtk.Table;
with Gtk.List;            use Gtk.List;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Text;            use Gtk.Text;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Dialogs;      use Gtkada.Dialogs;
with Gtkada.Handlers;     use Gtkada.Handlers;

with Prj.Tree;   use Prj.Tree;
with Prj;        use Prj;
with Prj.Ext;    use Prj.Ext;

with Namet;      use Namet;
with Stringt;    use Stringt;
with Types;      use Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;     use System;
with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Deallocation;

with Pixmaps_IDE;   use Pixmaps_IDE;
with Prj_API;       use Prj_API;
with Value_Editors; use Value_Editors;

package body Variable_Editors is

   procedure Display_Expr
     (Editable : access Gtk_Entry_Record'Class;
      Expr : String_List_Iterator);
   --  Display Expr in GEntry, with variable references replaced with
   --  '$<name>'.
   --  It also displays the other expressions in the list.
   --  GEntry is not deleted first.

   procedure Display_Expr
     (Editable : access Gtk_Text_Record'Class;
      Expr : String_List_Iterator);
   --  Same as above for a text widget

   procedure Add_Possible_Values
     (List : access Gtk_List_Record'Class; Typ : Project_Node_Id);
   --  Add all the possible values for type Typ into the List.

   procedure Resize_Text_Area
     (Text : access Value_Editor_Record'Class;
      Var : Project_Node_Id;
      Max_Lines : Natural);
   --  Resize a text area depending on the number of lines that need to be
   --  displayed in it. At most Max_Lines at displayed, a scrolled window is
   --  provided otherwise.

   function Get_Nth_Environment (Index : Natural) return chars_ptr;
   pragma Import (C, Get_Nth_Environment, "get_nth_environment");
   --  Return the string describing the nth environment variable. The strings
   --  have the format "name=value".

   type Var_Handler_Data is record
      Var : Project_Node_Id := Empty_Node;
      --  Edited variable

      Editor : Variable_Edit;
      --  The editor
   end record;
   --  Data to be passed to Var_Handler.
   --  ??? Not needed if object_connect could get a user_data.

   package Var_Handler is new User_Callback
     (Gtk_Widget_Record, Var_Handler_Data);

   type Editor_Row_Data is record
      Editor : Variable_Edit;
      Row    : Guint;
   end record;

   package Editor_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Editor_Row_Data);

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Var_Handler_Data);
   --  Called when editing a variable.

   procedure Changed
     (Editor : access Variable_Edit_Record'Class;
      Var    : Project_Node_Id);
   --  Emits the "changed" signal on the editor

   procedure Refresh_Row
     (Editor : access Variable_Edit_Record'Class;
      Row    : Guint;
      Var    : Project_Node_Id);
   --  Refresh the contents of a specific row in the table. If Row is greated
   --  than the number of rows in the table, a new one is created.
   --  The description of Var is displayed in that row.

   procedure Typed_Value_Changed
     (GEntry : access Gtk_Widget_Record'Class; User : Editor_Row_Data);
   --  Called when the entry giving the current value of a typed variable
   --  has changed.

   Var_Edit_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this widget

   Signals : constant chars_ptr_array :=
     (1 => New_String ("changed"));
   --  The list of signals defined for this widget

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor  : out Variable_Edit;
                      Project : Prj.Tree.Project_Node_Id;
                      Pkg     : Prj.Tree.Project_Node_Id := Empty_Node)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_Int, 2 => GType_None));
   begin
      Editor := new Variable_Edit_Record;
      Initialize (Editor);

      Initialize_Class_Record
        (Editor, Signals, Var_Edit_Class, "VarEditor", Signal_Parameters);

      Editor.Project := Project;
      Editor.Pkg := Pkg;
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Var_Edit : access Variable_Edit_Record'Class;
      Var : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node;
      Scenario_Variable_Only : Boolean)
   is
      use Widget_List;
      Item : Gtk_List_Item;
      Str : String_Id;
      Index : Natural := 0;
      S : chars_ptr;
   begin
      Editor := new New_Var_Edit_Record;
      Editor.Var := Var;
      Editor.Var_Editor := Variable_Edit (Var_Edit);
      Initialize (Editor);
      Set_Variable_Kind (Editor.Single_Value, Prj.Single);
      Set_Visible_Lines (Editor.Single_Value, 1);
      Allow_References (Editor.Enumeration_Value, False);

      --  Force toggle of the button, so that widgets are set sensitive
      --  appropriately.

      Set_Active (Editor.Get_Environment, False);
      Set_Active (Editor.Get_Environment, True);

      --  Fill the list of existing environment variables before we put the
      --  currently referenced variable (in case it doesn't represent an
      --  existing one.
      --  ??? Should be sorted.

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
            Gtk_New (Item, S2 (S2'First .. J - 1));
            Show (Item);
            Add (Get_List (Editor.List_Env_Variables), Item);
            Index := Index + 1;
         end;
      end loop;

      --  If we are editing scenario variables, some of the fields can not
      --  be modified (or the variable would no longer be valid for a scenario.

      if Scenario_Variable_Only then
         Set_Sensitive (Editor.Untyped_List_Variable, False);
         Set_Sensitive (Editor.List_Scrolled, False);
         Set_Sensitive (Editor.Untyped_Single_Variable, False);
         Set_Sensitive (Editor.Single_Value, False);
         Set_Sensitive (Editor.Get_Environment, False);
      end if;

      --  Fill the information for the variable

      if Var /= Empty_Node then
         Set_Title (Editor, "Editing a variable");

         Editor.Name_Was_Changed := True;
         Get_Name_String (Name_Of (Var));
         Set_Text (Editor.Variable_Name,
                   Name_Buffer (Name_Buffer'First .. Name_Len));
         Editor.Name_Was_Changed := False;

         Str := Get_Environment (Var);
         Set_Active (Editor.Get_Environment, Str /= No_String);

         if Str /= No_String then
            String_To_Name_Buffer (Str);
            Set_Text (Get_Entry (Editor.List_Env_Variables),
                      Name_Buffer (Name_Buffer'First .. Name_Len));
         end if;

         --  ??? Should be moved in prj_api
         if Kind_Of (Var) = N_Variable_Declaration then
            if Expression_Kind_Of (Var) = Single then
               Set_Active (Editor.Untyped_Single_Variable, True);
               Display_Expr (Editor.Single_Value, Value_Of (Var));
            else
               Set_Active (Editor.Untyped_List_Variable, True);
               Display_Expr (Editor.List_Value, Value_Of (Var));
            end if;

         else  --  Typed variable
            Set_Active (Editor.Typed_Variable, True);
            Display_Expr (Editor.Enumeration_Value, Type_Values (Var));
         end if;

         Set_Text (Gtk_Label (Get_Child (Editor.Add_Button)), "Update");
      else
         Set_Label (Editor.Name_Frame, "Name");
         Set_Active (Editor.Env_Must_Be_Defined, True);
         Set_Text (Gtk_Label (Get_Child (Editor.Add_Button)), "Add");
      end if;
   end Gtk_New;

   -------------
   -- Changed --
   -------------

   procedure Changed
     (Editor : access Variable_Edit_Record'Class;
      Var    : Project_Node_Id) is
   begin
      Widget_Callback.Emit_By_Name (Editor, "changed", Gint (Var));
   end Changed;

   ------------------
   -- Display_Expr --
   ------------------

   procedure Display_Expr
     (Editable : access Gtk_Text_Record'Class;
      Expr : String_List_Iterator)
   is
      Term, N : Project_Node_Id;
      E : String_List_Iterator := Expr;
      Pos : Gint := Get_Position (Editable);
   begin
      Realize (Editable); --  Make sure that the Value_Editors are realized
      while not Done (E) loop
         if Kind_Of (Data (E)) = N_Literal_String then
            String_To_Name_Buffer (String_Value_Of (Data (E)));
            Insert_Text
              (Editable, Name_Buffer (Name_Buffer'First .. Name_Len), Pos);

         else
            Term := First_Term (Data (E));
            while Term /= Empty_Node loop
               N := Current_Term (Term);

               case Kind_Of (N) is
                  when N_Variable_Reference =>
                     Get_Name_String (Name_Of (N));
                     Insert_Text
                       (Editable,
                        "${" & Name_Buffer (Name_Buffer'First .. Name_Len)
                        & '}',
                        Pos);

                  when N_Literal_String =>
                     String_To_Name_Buffer (String_Value_Of (N));
                     Insert_Text
                       (Editable,
                        Name_Buffer (Name_Buffer'First .. Name_Len),
                        Pos);

                  when others =>
                     Put_Line ("Display_Expr: " & Kind_Of (N)'Img);
                     raise Program_Error;
               end case;

               Term := Next_Term (Term);
            end loop;
         end if;

         E := Next (E);
         if not Done (E) then
            Insert_Text (Editable, "" & ASCII.LF, Pos);
         end if;
      end loop;
      Set_Position (Editable, 0);
   end Display_Expr;

   ------------------
   -- Display_Expr --
   ------------------

   procedure Display_Expr
     (Editable : access Gtk_Entry_Record'Class;
      Expr : String_List_Iterator)
   is
      Term, N : Project_Node_Id;
      E : String_List_Iterator := Expr;
   begin
      Realize (Editable); --  Make sure that the Value_Editors are realized
      while not Done (E) loop
         if Kind_Of (Data (E)) = N_Literal_String then
            String_To_Name_Buffer (String_Value_Of (Data (E)));
            Append_Text
              (Editable, Name_Buffer (Name_Buffer'First .. Name_Len));

         else
            Term := First_Term (Data (E));
            while Term /= Empty_Node loop
               N := Current_Term (Term);

               case Kind_Of (N) is
                  when N_Variable_Reference =>
                     Get_Name_String (Name_Of (N));
                     Append_Text
                       (Editable,
                        "${" & Name_Buffer (Name_Buffer'First .. Name_Len)
                        & '}');

                  when N_Literal_String =>
                     String_To_Name_Buffer (String_Value_Of (N));
                     Append_Text
                       (Editable,
                        Name_Buffer (Name_Buffer'First .. Name_Len));

                  when others =>
                     Put_Line ("Display_Expr: " & Kind_Of (N)'Img);
                     raise Program_Error;
               end case;

               Term := Next_Term (Term);
            end loop;
         end if;

         E := Next (E);
         if not Done (E) then
            Append_Text (Editable, "" & ASCII.LF);
         end if;
      end loop;
   end Display_Expr;

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

   -----------------
   -- Resize_Text --
   -----------------

   procedure Resize_Text_Area
     (Text : access Value_Editor_Record'Class;
      Var : Project_Node_Id;
      Max_Lines : Natural)
   is
      Iter : String_List_Iterator := Value_Of (Var);
      Num_Lines : Natural := 0;
   begin
      while not Done (Iter) loop
         Num_Lines := Num_Lines + 1;
         Iter := Next (Iter);
      end loop;

      Num_Lines := Natural'Min (Num_Lines, Max_Lines);
      Set_Visible_Lines (Text, Num_Lines);
   end Resize_Text_Area;

   -------------------------
   -- Typed_Value_Changed --
   -------------------------

   procedure Typed_Value_Changed
     (GEntry : access Gtk_Widget_Record'Class;
      User   : Editor_Row_Data)
   is
      use type Widget_List.Glist;
      Editor : Variable_Edit renames User.Editor;
      Var    : Project_Node_Id renames Editor.Data (User.Row).Var;
      Str    : String_Id;
      List   : Gtk_List := Get_List (Editor.Data (User.Row).Type_Combo);

   begin
      if Get_Selection (List) /= Widget_List.Null_List then
         Str := External_Reference_Of (Var);
         if Str /= No_String then
            String_To_Name_Buffer (Str);
            Prj.Ext.Add
              (Name_Buffer (Name_Buffer'First .. Name_Len),
               Get_Chars (Get_Entry (Editor.Data (User.Row).Type_Combo)));
         end if;

         Changed (Editor, Var);
      end if;
   end Typed_Value_Changed;

   -----------------
   -- Refresh_Row --
   -----------------

   procedure Refresh_Row
     (Editor : access Variable_Edit_Record'Class;
      Row    : Guint;
      Var    : Project_Node_Id)
   is
      procedure Free is new Unchecked_Deallocation
        (Row_Data_Array, Row_Data_Array_Access);
      Button   : Gtk_Button;
      Data     : Var_Handler_Data;
      Row_Data : Row_Data_Array_Access;
      Expr     : Project_Node_Id;
      Str      : String_Id;

   begin
      if Row > Editor.Num_Rows then
         Resize (Editor.List_Variables, Rows => Row, Columns => 5);
         Editor.Num_Rows := Row;
         Row_Data := new Row_Data_Array (0 .. Editor.Num_Rows);
         if Editor.Data /= null then
            Row_Data (0 .. Editor.Num_Rows - 1) := Editor.Data.all;
            Free (Editor.Data);
         end if;
         Editor.Data := Row_Data;

         Editor.Data (Row).Var := Var;

         --  Insert the widgets
         Gtk_New (Button);
         Add (Button, Create_Pixmap (stock_preferences_xpm, Editor));
         Attach (Editor.List_Variables, Button, 0, 1,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Xoptions => 0, Yoptions => 0);
         Data := (Var => Var, Editor => Variable_Edit (Editor));
         Var_Handler.Connect
           (Button, "clicked",
            Var_Handler.To_Marshaller (Edit_Variable'Access), Data);

         Gtk_New (Button);
         Add (Button, Create_Pixmap (trash_xpm, Editor));
         Attach (Editor.List_Variables, Button, 1, 2,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Xoptions => 0, Yoptions => 0);

         --  Name of the variable
         Gtk_New (Editor.Data (Row).Name_Label, "");
         Set_Alignment (Editor.Data (Row).Name_Label, 0.0, 0.0);
         Attach (Editor.List_Variables, Editor.Data (Row).Name_Label, 2, 3,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);

         --  Environment variables
         Gtk_New (Editor.Data (Row).Env_Label, "");
         Set_Alignment (Editor.Data (Row).Env_Label, 0.0, 0.0);
         Attach (Editor.List_Variables, Editor.Data (Row).Env_Label, 4, 5,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);

         Show_All (Editor.List_Variables);
      end if;

      --  Set the correct values for the widgets

      Get_Name_String (Name_Of (Var));
      Set_Text (Editor.Data (Row).Name_Label,
                Name_Buffer (Name_Buffer'First .. Name_Len));

      --  Note that the type of the variable might have changed since the
      --  last time we did the update, so we have to recreate some of the
      --  widgets now

      if Editor.Data (Row).Type_Combo /= null then
         Destroy (Editor.Data (Row).Type_Combo);
         Editor.Data (Row).Type_Combo := null;
      elsif Editor.Data (Row).Scrolled /= null then
         Destroy (Editor.Data (Row).Scrolled);
         Editor.Data (Row).Scrolled := null;
         Editor.Data (Row).Value_Edit := null;
      elsif Editor.Data (Row).Value_Edit /= null then
         Destroy (Editor.Data (Row).Value_Edit);
         Editor.Data (Row).Value_Edit := null;
      end if;

      if Kind_Of (Var) = N_Typed_Variable_Declaration then
         Gtk_New (Editor.Data (Row).Type_Combo);
         Set_Editable (Get_Entry (Editor.Data (Row).Type_Combo), False);
         Attach (Editor.List_Variables, Editor.Data (Row).Type_Combo, 3, 4,
                 Row - 1, Row, Yoptions => 0);
         Add_Possible_Values
           (Get_List (Editor.Data (Row).Type_Combo),
            String_Type_Of (Var));
         Set_Text (Get_Entry (Editor.Data (Row).Type_Combo), "");

         Str := External_Reference_Of (Var);
         if Str /= No_String then
            String_To_Name_Buffer (Str);
            Str := Prj.Ext.Value_Of (Name_Find);
            if Str /= No_String then
               String_To_Name_Buffer (Str);
               Set_Text
                 (Get_Entry (Editor.Data (Row).Type_Combo),
                  Name_Buffer (Name_Buffer'First .. Name_Len));
            end if;
         end if;

         Show_All (Editor.Data (Row).Type_Combo);
         Editor_Callback.Connect
           (Get_Entry (Editor.Data (Row).Type_Combo),
            "changed",
            Editor_Callback.To_Marshaller (Typed_Value_Changed'Access),
            (Variable_Edit (Editor), Row));

      elsif Expression_Kind_Of (Var) = Prj.List then
         Gtk_New (Editor.Data (Row).Scrolled);
         Set_Policy
           (Editor.Data (Row).Scrolled, Policy_Never, Policy_Automatic);
         Gtk_New (Editor.Data (Row).Value_Edit);
         Add (Editor.Data (Row).Scrolled, Editor.Data (Row).Value_Edit);
         Attach (Editor.List_Variables, Editor.Data (Row).Scrolled, 3, 4,
                 Row - 1, Row, Yoptions => 0);
         Resize_Text_Area (Editor.Data (Row).Value_Edit, Var, 5);
         Display_Expr (Editor.Data (Row).Value_Edit, Value_Of (Var));
         Set_Position (Editor.Data (Row).Value_Edit, 0);
         Show_All (Editor.Data (Row).Scrolled);

      else
         Gtk_New (Editor.Data (Row).Value_Edit);
         Attach (Editor.List_Variables, Editor.Data (Row).Value_Edit, 3, 4,
                 Row - 1, Row, Yoptions => 0);
         Set_Visible_Lines (Editor.Data (Row).Value_Edit, 1);
         Display_Expr (Editor.Data (Row).Value_Edit, Value_Of (Var));
         Show_All (Editor.Data (Row).Value_Edit);
      end if;

      --  The environment variable
      Str := External_Reference_Of (Var);
      if Str /= No_String then
         String_To_Name_Buffer (Str);
         Set_Text (Editor.Data (Row).Env_Label,
                   Name_Buffer (Name_Buffer'First .. Name_Len));
      end if;
   end Refresh_Row;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Editor : access Variable_Edit_Record;
      Var    : Prj.Tree.Project_Node_Id := Empty_Node) is
   begin
      pragma Assert
        (Kind_Of (Var) = N_Typed_Variable_Declaration
         or else Kind_Of (Var) = N_Variable_Declaration);

      if Editor.Data /= null then
         for J in Editor.Data'Range loop
            if Editor.Data (J).Var = Var then
               Refresh_Row (Editor, J, Var);
               return;
            end if;
         end loop;
      end if;
      Refresh_Row (Editor, Editor.Num_Rows + 1, Var);
   end Refresh;

   -------------------
   -- Edit_Variable --
   -------------------

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Var_Handler_Data)
   is
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Data.Editor, Data.Var, Scenario_Variable_Only => True);
      Show_All (Edit);
   end Edit_Variable;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable (Editor : access New_Var_Edit_Record) is
      V : Validity := Valid;
      Value : Value_Editor;
      Parent : Project_Node_Id;
      Expr : Project_Node_Id;
      Button : Message_Dialog_Buttons;
   begin
      if Get_Active (Editor.Typed_Variable) then
         Value := Editor.Enumeration_Value;
      elsif Get_Active (Editor.Untyped_List_Variable) then
         Value := Editor.List_Value;
      else
         Value := Editor.Single_Value;
      end if;

      --  The name of the variable mustn't be empty

      if Get_Text (Editor.Variable_Name) = "" then
         Button := Message_Dialog
           ("The name of the variable must be specified",
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_OK,
            Help_Msg    =>
              "Fill the first field at the top of the variable editor",
            Title       => "Invalid variable description");
         return;
      end if;

      --  For environment variables, the name mustn't be empty. We already
      --  know the default value is legal, since the user cannot enter
      --  anything illegal anyway.

      if Get_Active (Editor.Get_Environment)
        and then Get_Chars (Get_Entry (Editor.List_Env_Variables)) = ""
      then
         Button := Message_Dialog
           ("The environment variable name must be specified",
            Dialog_Type => Gtkada.Dialogs.Error,
            Buttons     => Button_OK,
            Help_Msg    =>
              "Fill the Name field in the ""importing"" section",
            Title       => "Invalid variable description");
         return;
      end if;


      if V = Valid then
         V := Check_Validity (Value);
      end if;

      case V is
         when Valid =>
            Expr := Get_Value (Value, Editor.Var_Editor.Project);

            if Editor.Var = Empty_Node then
               Parent := Editor.Var_Editor.Pkg;
               if Parent = Empty_Node then
                  Parent := Editor.Var_Editor.Project;
               end if;

               if Get_Active (Editor.Typed_Variable) then
                  Editor.Var := Get_Or_Create_Typed_Variable
                    (Parent,
                     Name => Get_Text (Editor.Variable_Name),
                     Typ  => Expr);
               else
                  Editor.Var := Get_Or_Create_Variable
                    (Parent,
                     Name => Get_Text (Editor.Variable_Name),
                     Kind => Expression_Kind_Of (Expr));
               end if;

            else
               declare
                  N : constant String := Get_Text (Editor.Variable_Name);
               begin
                  Get_Name_String (Name_Of (Editor.Var));
                  if Name_Buffer (Name_Buffer'First .. Name_Len) /= N then
                     Name_Len := N'Length;
                     Name_Buffer (Name_Buffer'First .. Name_Len) := N;
                     Set_Name_Of (Editor.Var, Name_Find);
                  end if;
               end;
            end if;

            if Get_Active (Editor.Typed_Variable) then
               --  Should delete previous type
               Get_Name_String (Name_Of (Editor.Var));
               Name_Buffer (Name_Len + 1 .. Name_Len + 5) := "_Type";
               Name_Len := Name_Len + 5;
               Set_Name_Of (Expr, Name_Find);
               Set_Kind_Of (Editor.Var, N_Typed_Variable_Declaration);
               Set_String_Type_Of (Editor.Var, Expr);
            else
               Set_Expression_Kind_Of (Editor.Var, Expression_Kind_Of (Expr));
            end if;

            --  If this is an environment variable, set this correctly.
            if Get_Active (Editor.Get_Environment) then
               if not Get_Active (Editor.Env_Must_Be_Defined) then
                  Set_Value_As_External
                    (Editor.Var,
                     Get_Chars (Get_Entry (Editor.List_Env_Variables)),
                     Get_Chars (Get_Entry (Editor.Default_Env_Variable)));
               else
                  Set_Value_As_External
                    (Editor.Var,
                     Get_Chars (Get_Entry (Editor.List_Env_Variables)), "");
               end if;

            else
               Set_Expression_Of (Editor.Var, Expr);
            end if;

            Refresh (Editor.Var_Editor, Editor.Var);
            Destroy (Editor);

         when others =>
            Put_Line ("Validity = " & V'Img);

      end case;
   end Update_Variable;

   ------------------
   -- Name_Changed --
   ------------------

   procedure Name_Changed (Editor : access New_Var_Edit_Record) is
   begin
      if Editor.Var /= Empty_Node
        and then not Editor.Name_Was_Changed
      then
         Editor.Name_Was_Changed := True;
         Get_Name_String (Name_Of (Editor.Var));

         --  ??? With GtkAda 2.0, the label of the frame simply disappears when
         --  it is changed dynamically like this.
         Set_Label (Editor.Name_Frame,
                    "Name (old name was "
                    & Name_Buffer (Name_Buffer'First .. Name_Len) & ")");
      end if;
   end Name_Changed;

end Variable_Editors;

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

with Gtk.Button;          use Gtk.Button;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.Label;           use Gtk.Label;
with Gtk.Table;           use Gtk.Table;
with Gtk.List;            use Gtk.List;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Editable;        use Gtk.Editable;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Widget;          use Gtk.Widget;

with Prj.Tree;   use Prj.Tree;
with Prj;        use Prj;

with Namet;      use Namet;
with Stringt;    use Stringt;
with Types;      use Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;     use System;

with Prj_API;    use Prj_API;
with Value_Editors; use Value_Editors;

package body Variable_Editors is

   procedure Display_Expr
     (Editable : access Gtk_Editable_Record'Class;
      Expr : String_List_Iterator);
   --  Display Expr in GEntry, with variable references replaced with
   --  '$<name>'.
   --  It also displays the other expressions in the list.
   --  GEntry is not deleted first.

   procedure Add_Possible_Values
     (List : access Gtk_List_Record'Class; Typ : Project_Node_Id);
   --  Add all the possible values for type Typ into the List.

   procedure External_As_String (Ext : Project_Node_Id);
   --  Set in Name_Buffer the name of the external variable referenced by Ext.
   --  Note that this doesn't support complex expressions for the variable
   --  name.

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

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Var_Handler_Data);
   --  Called when editing a variable.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Variable_Edit) is
   begin
      Editor := new Variable_Edit_Record;
      Initialize (Editor);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Var : Prj.Tree.Project_Node_Id :=  Prj.Tree.Empty_Node)
   is
      use Widget_List;
      Item : Gtk_List_Item;
      Str : String_Id;
      Index : Natural := 0;
      S : chars_ptr;
   begin
      Editor := new New_Var_Edit_Record;
      Editor.Var := Var;
      Initialize (Editor);
      Set_Visible_Lines (Editor.Single_Value, 1);

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

      --  Fill the information for the variable

      if Var /= Empty_Node then
         Get_Name_String (Project_Nodes.Table (Var).Name);

         Set_Label (Editor.Name_Frame,
                    "Name (old name was "
                    & Name_Buffer (Name_Buffer'First .. Name_Len) & ")");
         Set_Text (Editor.Variable_Name,
                   Name_Buffer (Name_Buffer'First .. Name_Len));

         Str := Get_Environment (Var);
         Set_Active (Editor.Get_Environment, Str /= No_String);
         Set_Sensitive (Editor.List_Env_Variables, Str /= No_String);

         if Str /= No_String then
            String_To_Name_Buffer (Str);
            Set_Text (Get_Entry (Editor.List_Env_Variables),
                      Name_Buffer (Name_Buffer'First .. Name_Len));
         end if;

         if Project_Nodes.Table (Var).Kind = N_Variable_Declaration then
            if Project_Nodes.Table (Var).Expr_Kind = Single then
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
         Set_Text (Gtk_Label (Get_Child (Editor.Add_Button)), "Add");
      end if;
   end Gtk_New;

   ------------------
   -- Display_Expr --
   ------------------

   procedure Display_Expr
     (Editable : access Gtk_Editable_Record'Class;
      Expr : String_List_Iterator)
   is
      Term, N : Project_Node_Id;
      E : String_List_Iterator := Expr;
      Pos : Gint := Get_Position (Editable);
   begin
      Realize (Editable); --  Make sure that the Value_Editors are realized
      while not Done (E) loop
         if Project_Nodes.Table (Data (E)).Kind = N_Literal_String then
            String_To_Name_Buffer (Project_Nodes.Table (Data (E)).Value);
            Insert_Text
              (Editable, Name_Buffer (Name_Buffer'First .. Name_Len), Pos);

         else
            Term := Project_Nodes.Table (Data (E)).Field1;
            while Term /= Empty_Node loop
               N := Project_Nodes.Table (Term).Field1;

               case Project_Nodes.Table (N).Kind is
                  when N_Variable_Reference =>
                     Get_Name_String (Project_Nodes.Table (N).Name);
                     Insert_Text
                       (Editable,
                        "$" & Name_Buffer (Name_Buffer'First .. Name_Len),
                        Pos);

                  when N_Literal_String =>
                     String_To_Name_Buffer (Project_Nodes.Table (N).Value);
                     Insert_Text
                       (Editable,
                        Name_Buffer (Name_Buffer'First .. Name_Len),
                        Pos);

                  when others =>
                     raise Program_Error;
               end case;

               Term := Project_Nodes.Table (Term).Field2;
               if Term /= Empty_Node then
                  Insert_Text (Editable, " & ", Pos);
               end if;
            end loop;
         end if;

         E := Next (E);
         if not Done (E) then
            Insert_Text (Editable, "" & ASCII.LF, Pos);
         end if;
      end loop;
   end Display_Expr;

   ------------------------
   -- External_As_String --
   ------------------------

   procedure External_As_String (Ext : Project_Node_Id) is
      Expr, Term, N : Project_Node_Id;
   begin
      pragma Assert (Ext /= Empty_Node);
      Expr := Project_Nodes.Table (Ext).Field1;
      pragma Assert (Expr /= Empty_Node);

      Term := Project_Nodes.Table (Expr).Field1;
      pragma Assert (Term /= Empty_Node);
      N := Project_Nodes.Table (Term).Field1;

      pragma Assert (N /= Empty_Node);
      pragma Assert (Project_Nodes.Table (N).Kind = N_Literal_String);

      String_To_Name_Buffer (Project_Nodes.Table (N).Value);
   end External_As_String;

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

   ------------------
   -- Add_Variable --
   ------------------

   procedure Add_Variable
     (Editor : access Variable_Edit_Record'Class;
      Var    : Prj.Tree.Project_Node_Id)
   is
      Button  : Gtk_Button;
      Label   : Gtk_Label;
      Combo   : Gtk_Combo;
      Text    : Value_Editor;
      Data    : Var_Handler_Data;
      Scrolled : Gtk_Scrolled_Window;
   begin
      pragma Assert
        (Project_Nodes.Table (Var).Kind = N_Typed_Variable_Declaration
         or else Project_Nodes.Table (Var).Kind = N_Variable_Declaration);

      Resize (Editor.List_Variables, Rows => Editor.Num_Rows, Columns => 5);
      Editor.Num_Rows := Editor.Num_Rows + 1;

      Gtk_New (Button, "E");
      Attach (Editor.List_Variables, Button, 0, 1,
              Editor.Num_Rows - 1, Editor.Num_Rows,
              Xoptions => 0, Yoptions => 0);
      Data := (Var => Var, Editor => Variable_Edit (Editor));
      Var_Handler.Connect
        (Button, "clicked", Var_Handler.To_Marshaller (Edit_Variable'Access),
         Data);

      Gtk_New (Button, "D");
      Attach (Editor.List_Variables, Button, 1, 2,
              Editor.Num_Rows - 1, Editor.Num_Rows,
              Xoptions => 0, Yoptions => 0);

      Get_Name_String (Project_Nodes.Table (Var).Name);
      Gtk_New (Label, Name_Buffer (Name_Buffer'First .. Name_Len));
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Editor.List_Variables, Label, 2, 3,
              Editor.Num_Rows - 1, Editor.Num_Rows,
              Yoptions => 0);

      if Project_Nodes.Table (Var).Kind = N_Typed_Variable_Declaration then
         Gtk_New (Combo);
         Set_Editable (Get_Entry (Combo), False);
         Attach (Editor.List_Variables, Combo, 3, 4,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);
         Add_Possible_Values
           (Get_List (Combo), Project_Nodes.Table (Var).Field2);
         Delete_Text (Get_Entry (Combo));
         Display_Expr (Get_Entry (Combo), Value_Of (Var));

      elsif Project_Nodes.Table (Var).Expr_Kind = Prj.List then
         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
         Gtk_New (Text);
         Add (Scrolled, Text);
         Attach (Editor.List_Variables, Scrolled, 3, 4,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);
         Resize_Text_Area (Text, Var, 5);
         Display_Expr (Text, Value_Of (Var));
         Set_Position (Text, 0);

      else
         Gtk_New (Text);
         Attach (Editor.List_Variables, Text, 3, 4,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);
         Set_Visible_Lines (Text, 1);
         Display_Expr (Text, Value_Of (Var));
      end if;

      --  The environment variable
      if Project_Nodes.Table (Var).Field1 /= Empty_Node
        and then Project_Nodes.Table (Project_Nodes.Table (Var).Field1).Kind
           = N_External_Value
      then
         External_As_String (Project_Nodes.Table (Var).Field1);
         Gtk_New (Label, Name_Buffer (Name_Buffer'First .. Name_Len));
         Set_Alignment (Label, 0.0, 0.0);
         Attach (Editor.List_Variables, Label, 4, 5,
                 Editor.Num_Rows - 1, Editor.Num_Rows,
                 Yoptions => 0);
      end if;

      Show_All (Editor.List_Variables);
   end Add_Variable;

   -------------------
   -- Edit_Variable --
   -------------------

   procedure Edit_Variable
     (Button : access Gtk_Widget_Record'Class; Data : Var_Handler_Data)
   is
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Data.Var);
      Show_All (Edit);
   end Edit_Variable;

end Variable_Editors;

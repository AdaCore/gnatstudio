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
with Gtk.Alignment;       use Gtk.Alignment;
with Gtk.Button;          use Gtk.Button;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.Label;           use Gtk.Label;
with Gtk.List;            use Gtk.List;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Text;            use Gtk.Text;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Dialogs;      use Gtkada.Dialogs;

with Prj.Tree;   use Prj.Tree;
with Prj;        use Prj;

with Namet;      use Namet;
with Stringt;    use Stringt;
with Types;      use Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;     use System;
with Ada.Text_IO; use Ada.Text_IO;

with Prj_API;       use Prj_API;
with Value_Editors; use Value_Editors;
with Glide_Kernel;  use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;

package body Variable_Editors is

   procedure Display_Expr
     (Editable : access Gtk_Text_Record'Class;
      Expr : String_List_Iterator);
   --  Same as above for a text widget

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out New_Var_Edit;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
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
      Editor.Kernel := Kernel_Handle (Kernel);
      Initialize (Editor);

      Configure (Editor.Enumeration_Value, Kernel);
      Configure (Editor.List_Value, Kernel);
      Configure (Editor.Single_Value, Kernel);

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
         Set_Sensitive (Editor.Get_Environment, False);
         Destroy (Editor.Untyped_List_Variable);
         Destroy (Editor.Untyped_Alignment);
         Destroy (Editor.Untyped_Single_Variable);
         Destroy (Editor.Single_Alignment);
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
            Expr := Get_Value (Value, Get_Project (Editor.Kernel));

            if Editor.Var = Empty_Node then
               Parent := Get_Project (Editor.Kernel);

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

                  --  Note: the order is important below, since some of the
                  --  widgets might not exist if the variable editor is
                  --  configured for scenario variables only.

                  if Get_Active (Editor.Typed_Variable) then
                     Set_Expression_Kind_Of (Editor.Var, Single);
                  elsif Get_Active (Editor.Untyped_List_Variable) then
                     Set_Expression_Kind_Of (Editor.Var, Prj.List);
                  else
                     Set_Expression_Kind_Of (Editor.Var, Single);
                  end if;
               end;
            end if;

            if Get_Active (Editor.Typed_Variable) then
               --  ??? Should delete previous type
               Get_Name_String (Name_Of (Editor.Var));
               Name_Buffer (Name_Len + 1 .. Name_Len + 5) := "_Type";
               Name_Len := Name_Len + 5;
               Set_Name_Of (Expr, Name_Find);
               Set_Kind_Of (Editor.Var, N_Typed_Variable_Declaration);
               Set_String_Type_Of (Editor.Var, Expr);
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

            Recompute_View (Editor.Kernel);
            --  ??? We don't really need to recompute the whole view, since
            --  ??? after all we only added a variable that doesn't have any
            --  ??? influence yet.
            --
            --  ??? We should register that the project is no longer normalized

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

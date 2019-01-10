------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Button;               use Gtk.Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Table;                use Gtk.Table;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;

with Gtk.Cell_Layout;          use Gtk.Cell_Layout;
with Gtkada.Dialogs;           use Gtkada.Dialogs;

package body Build_Configurations.Gtkada.Dialogs is

   Icon_Column       : constant := 0; -- Contains the icon
   Name_Column       : constant := 1; -- Contains the displayed name, as markup
   Desc_Column       : constant := 2;
   Whitespace_Column : constant := 3;

   Column_Types : constant GType_Array :=
     (Icon_Column       => GType_String,
      Name_Column       => GType_String,
      Desc_Column       => GType_String,
      Whitespace_Column => GType_String);

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function

   function New_Target_Name_Is_Valid
     (UI       : access Build_UI_Record'Class;
      Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean;
   --  Return True is Name is a valid new target name for Registry

   procedure Fill_Combo
     (UI    : access Build_UI_Record'Class;
      Combo : Gtk_Combo_Box_Text;
      Cat_E : Gtk_Entry);
   --  Fill Combo and Cat_E from information stored in UI

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      --  ??? Provide implementation
      return Msg;
   end "-";

   -----------------
   -- Information --
   -----------------

   procedure Information
     (UI      : access Build_UI_Record'Class;
      Message : String)
   is
      R : Message_Dialog_Buttons;
      pragma Unreferenced (R);
   begin
      R := Message_Dialog
        (Msg            => Message,
         Dialog_Type    => Information,
         Buttons        => Button_OK,
         Default_Button => Button_OK,
         Help_Msg       => "",
         Title          => "",
         Parent         => Gtk_Window (Get_Toplevel (UI)));
   end Information;

   ------------------
   -- Models_Combo --
   ------------------

   function Models_Combo
     (UI : access Build_UI_Record'Class) return Gtk_Combo_Box_Text
   is
      Combo : Gtk_Combo_Box_Text;
      Model : Gtk_Tree_Store;

      function Get_Or_Create_Category (Cat : String) return Gtk_Tree_Iter;
      --  Return the iter corresponding to category Cat

      ----------------------------
      -- Get_Or_Create_Category --
      ----------------------------

      function Get_Or_Create_Category (Cat : String) return Gtk_Tree_Iter is
         It : Gtk_Tree_Iter;

      begin
         It := Get_Iter_First (Model);

         while It /= Null_Iter loop
            if Get_String (Model, It, Desc_Column) = Cat then
               return It;
            end if;
            Next (Model, It);
         end loop;

         --  We reach this point if no iter was found: create it
         Append (Model, It, Null_Iter);
         Set_And_Clear
           (Model, It, (Desc_Column, Name_Column),
            (As_String  ("<b>" & Cat & "</b>"), As_String  ("")));

         return It;
      end Get_Or_Create_Category;

      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer : Gtk_Cell_Renderer_Text;

      Col  : Gtk_Cell_Layout;
      Iter : Gtk_Tree_Iter;

      use Model_Map;
      C  : Cursor;
      M  : Target_Model_Access;

   begin
      Gtk_New_With_Entry (Combo);
      Gtk_New (Model, Column_Types);

      Set_Model (Combo, +Model);

      Set_Entry_Text_Column (Combo, Name_Column);
      Col := +Combo;

      Gtk_New (Text_Renderer);
      Pack_Start (Col, Text_Renderer, False);
      Add_Attribute (Col, Text_Renderer, "text", Whitespace_Column);

      Gtk_New (Icon_Renderer);
      Pack_Start (Col, Icon_Renderer, False);
      Add_Attribute (Col, Icon_Renderer, "icon-name", Icon_Column);

      Gtk_New (Text_Renderer);
      Pack_Start (Col, Text_Renderer, False);
      Add_Attribute (Col, Text_Renderer, "markup", Desc_Column);

      Reorder (Col, Icon_Renderer, 0);

      --  Fill the model combo

      C := UI.Registry.Models.First;

      while Has_Element (C) loop
         M := Element (C);

         if M.Category = "" then
            Iter := Null_Iter;
         else
            Iter := Get_Or_Create_Category (To_String (M.Category));
         end if;

         Append (Model, Iter, Iter);
         Set_And_Clear
           (Model, Iter,
            (Icon_Column, Name_Column, Desc_Column, Whitespace_Column),
            (As_String  (To_String (M.Icon)),
             As_String  (To_String (M.Name)),
             As_String  (To_String (M.Description)),
             As_String  ("        ")));

         Next (C);
      end loop;

      return Combo;
   end Models_Combo;

   ------------------------------
   -- New_Target_Name_Is_Valid --
   ------------------------------

   function New_Target_Name_Is_Valid
     (UI       : access Build_UI_Record'Class;
      Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean is
   begin
      --  Verify that the name is not empty
      if Name = "" then
         Information
           (UI, -"Note: targets must have a non-empty name.");
         return False;
      end if;

      --  Verify that no target with this name exists
      if Contains (Registry.Targets, Name) then
         Information
           (UI,
            -"A target named """ & To_String (Name) & """ already exists.");
         return False;
      end if;

      return True;
   end New_Target_Name_Is_Valid;

   ----------------
   -- Fill_Combo --
   ----------------

   procedure Fill_Combo
     (UI    : access Build_UI_Record'Class;
      Combo : Gtk_Combo_Box_Text;
      Cat_E : Gtk_Entry)
   is
      function Strip (S : String) return String;
      --  Strip S of pango markup

      -----------
      -- Strip --
      -----------

      function Strip (S : String) return String is
      begin
         return S (S'First + 3 .. S'Last - 4);
      end Strip;

      Model        : Gtk_Tree_Model;
      Iter         : Gtk_Tree_Iter;
      P            : Gtk_Tree_Iter;
      Category_Set : Boolean := False;

   begin
      Get_Selected (Get_Selection (UI.View), Model, Iter);

      if Iter /= Null_Iter then
         P := Parent (Model, Iter);

         if P /= Null_Iter then
            Iter := P;
         end if;

         Set_Text (Cat_E, Strip (Get_String (Model, Iter, Name_Column)));
         Category_Set := True;
      end if;

      Iter := Get_Iter_First (UI.View.Model);

      while Iter /= Null_Iter loop
         if Get_Int (UI.View.Model, Iter, 2) = 0 then
            --  This is to test that the iter is a category iter

            declare
               Str : constant String :=
                       Strip (Get_String (UI.View.Model, Iter, Name_Column));
            begin
               Append_Text (Combo, Str);

               if not Category_Set then
                  Set_Text (Cat_E, Str);
                  Category_Set := True;
               end if;
            end;
         end if;

         Next (UI.View.Model, Iter);
      end loop;
   end Fill_Combo;

   -----------------------
   -- Add_Target_Dialog --
   -----------------------

   procedure Add_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Model     : out Unbounded_String;
      Name      : out Unbounded_String;
      Category  : out Unbounded_String;
      Cancelled : out Boolean)
   is
      Button : Gtk_Button;
      pragma Unreferenced (Button);
      Parent : constant Gtk_Window := Gtk_Window (Get_Toplevel (UI));
      Dialog : Gtk_Dialog;
      Combo  : Gtk_Combo_Box_Text;
      Label  : Gtk_Label;

      Model_E : Gtk_Entry;
      Name_E  : Gtk_Entry;
      Cat_E   : Gtk_Entry;
      Table   : Gtk_Table;

      Hbox    : Gtk_Hbox;

      use Model_Map;
      C : Cursor;

   begin
      Cancelled := False;

      Gtk_New (Dialog => Dialog,
               Title  => -"New target",
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent);
      Set_Transient_For (Dialog, Parent);

      Gtk_New (Table, 3, 2, False);

      --  Add the models combo

      Gtk_New (Label, -"Target model");
      Set_Tooltip_Text
        (Label,
         -"Select the target model");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 1, 2, Expand or Fill, 0, 3, 3);

      Combo := Models_Combo (UI);
      Model_E := Gtk_Entry (Get_Child (Combo));
      Model_E.Set_Name ("new_target-target_model");
      Set_Editable (Model_E, False);
      Set_Tooltip_Text
        (Combo,
         -"Select the target type/model");

      C := UI.Registry.Models.First;
      if Has_Element (C) then
         Set_Text (Model_E, To_String (Element (C).Name));
      end if;

      Attach (Table, Combo, 1, 2, 1, 2, Expand or Fill, 0, 3, 3);

      --  Add the name entry
      Gtk_New (Label, -"Target name");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 0, 1, Expand or Fill, 0, 3, 3);

      Gtk_New (Name_E);
      Set_Text (Name_E, "New target");
      Name_E.Set_Name ("new_target-target_name");
      Select_Region (Name_E, 0);

      Attach (Table, Name_E, 1, 2, 0, 1, Expand or Fill, 0, 3, 3);

      --  Enter some relevant text and select it

      --  Add the category entry

      Gtk_New (Label, -"Target category");
      Set_Tooltip_Text
        (Label,
         -"Type a new category name, or select an existing one");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 2, 3, Expand or Fill, 0, 3, 3);

      Gtk_New_With_Entry (Combo);
      Combo.Set_Entry_Text_Column (0);
      Cat_E := Gtk_Entry (Get_Child (Combo));
      Cat_E.Set_Name ("new_target-target_category");
      Set_Tooltip_Text
        (Combo,
         -"Type a new category name, or select an existing one");

      Fill_Combo (UI, Combo, Cat_E);

      Attach (Table, Combo, 1, 2, 2, 3, Expand or Fill, 0, 3, 3);
      --  ??? make this a combo-box-entry with the already existing categories

      Pack_Start (Get_Content_Area (Dialog), Table, False, False, 3);

      --  Add the buttons

      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      --  Grab the focus on the entry, select the text, and make it activate
      --  the default, so that the user only has to press Enter if he is
      --  happy with the selection.
      Grab_Focus (Name_E);
      Select_Region (Name_E, 0);
      Set_Activates_Default (Name_E, True);

      Show_All (Dialog);

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               Name     := To_Unbounded_String (Get_Text (Name_E));
               Model    := To_Unbounded_String (Get_Text (Model_E));
               Category := To_Unbounded_String (Get_Text (Cat_E));

               if New_Target_Name_Is_Valid (UI, UI.Registry, Name) then
                  Destroy (Dialog);
                  exit;
               end if;

            when others =>
               Cancelled := True;
               Destroy (Dialog);
               exit;
         end case;
      end loop;
   end Add_Target_Dialog;

   -------------------------
   -- Clone_Target_Dialog --
   -------------------------

   procedure Clone_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Target    : Target_Access;
      Name      : out Unbounded_String;
      Category  : out Unbounded_String;
      Cancelled : out Boolean)
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Button;
      pragma Unreferenced (Button);
      Parent : constant Gtk_Window := Gtk_Window (Get_Toplevel (UI));
      Combo  : Gtk_Combo_Box_Text;
      Label  : Gtk_Label;

      Name_E  : Gtk_Entry;
      Cat_E   : Gtk_Entry;
      Table   : Gtk_Table;

      Hbox    : Gtk_Hbox;

   begin
      Cancelled := False;

      Gtk_New (Dialog => Dialog,
               Title  => -"Clone target",
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent);
      Set_Transient_For (Dialog, Parent);

      Gtk_New (Table, 3, 2, False);

      --  Add the name entry
      Gtk_New (Label, -"Target name");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 0, 1, Expand or Fill, 0, 3, 3);

      Gtk_New (Name_E);
      Set_Text (Name_E, (-"Copy of ") & To_String (Target.Name));
      Select_Region (Name_E, 0);

      Attach (Table, Name_E, 1, 2, 0, 1, Expand or Fill, 0, 3, 3);

      --  Enter some relevant text and select it

      --  Add the category entry

      Gtk_New (Label, -"Target category");
      Set_Tooltip_Text
        (Label,
         -"Type a new category name, or select an existing one");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 2, 3, Expand or Fill, 0, 3, 3);

      Gtk_New_With_Entry (Combo);
      Combo.Set_Entry_Text_Column (0);
      Cat_E := Gtk_Entry (Get_Child (Combo));
      Set_Tooltip_Text
        (Combo,
         -"Type a new category name, or select an existing one");

      Fill_Combo (UI, Combo, Cat_E);

      Attach (Table, Combo, 1, 2, 2, 3, Expand or Fill, 0, 3, 3);
      --  ??? make this a combo-box-entry with the already existing categories

      Pack_Start (Get_Content_Area (Dialog), Table, False, False, 3);

      --  Add the buttons

      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      --  Grab the focus on the entry, select the text, and make it activate
      --  the default, so that the user only has to press Enter if he is
      --  happy with the selection.
      Grab_Focus (Name_E);
      Select_Region (Name_E, 0);
      Set_Activates_Default (Name_E, True);

      Show_All (Dialog);

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               Name := To_Unbounded_String (Get_Text (Name_E));
               Category := To_Unbounded_String (Get_Text (Cat_E));

               if New_Target_Name_Is_Valid (UI, UI.Registry, Name) then
                  Destroy (Dialog);
                  exit;
               end if;

            when others =>
               Cancelled := True;
               Destroy (Dialog);
               exit;
         end case;
      end loop;
   end Clone_Target_Dialog;

   -------------------
   -- Yes_No_Dialog --
   -------------------

   function Yes_No_Dialog
     (UI : access Build_UI_Record'Class;
      M  : String) return Boolean
   is
      Buttons : Message_Dialog_Buttons;
   begin
      Buttons := Message_Dialog
        (Msg            => M,
         Dialog_Type    => Information,
         Buttons        => Button_Yes or Button_No,
         Default_Button => Button_No,
         Parent         => Gtk_Window (Get_Toplevel (UI)));

      return (Buttons and Button_Yes) /= 0;
   end Yes_No_Dialog;

   --------------------------
   -- Delete_Target_Dialog --
   --------------------------

   procedure Delete_Target_Dialog
     (UI        : access Build_UI_Record'Class;
      Target    : Target_Access;
      Cancelled : out Boolean)
   is
   begin
      Cancelled := False;

      if Target.Properties.Read_Only then
         Information
           (UI, -"This target is a predefined target, and cannot be removed");
         Cancelled := True;
         return;
      end if;

      Cancelled := not Yes_No_Dialog
        (UI => UI,
         M  => (-"About to suppress target") & ASCII.LF
         & "'" & To_String (Target.Name)
         & "'." & ASCII.LF & "Would you like to continue?");
   end Delete_Target_Dialog;

end Build_Configurations.Gtkada.Dialogs;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;

with Gtk.Button;               use Gtk.Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Table;                use Gtk.Table;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
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

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function

   function New_Target_Name_Is_Valid
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean;
   --  Return True is Name is a valid new target name for Registry

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

   procedure Information (Message : String) is
      R : Message_Dialog_Buttons;
      pragma Unreferenced (R);
   begin
      R := Message_Dialog
        (Msg            => Message,
         Dialog_Type    => Information,
         Buttons        => Button_OK,
         Default_Button => Button_OK,
         Help_Msg       => "",
         Title          => "");
   end Information;

   ------------------
   -- Models_Combo --
   ------------------

   function Models_Combo
     (UI : access Build_UI_Record'Class) return Gtk_Combo_Box_Entry
   is
      Combo     : Gtk_Combo_Box_Entry;
      Model     : Gtk_Tree_Store;

      function Columns return GType_Array;
      --  Used for the models combo

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
         Set (Model, It, Desc_Column, "<b>" & Cat & "</b>");
         Set (Model, It, Name_Column, "");

         return It;
      end Get_Or_Create_Category;

      -------------
      -- Columns --
      -------------

      function Columns return GType_Array is
      begin
         return GType_Array'
           (Icon_Column       => GType_String,
            Name_Column       => GType_String,
            Desc_Column       => GType_String,
            Whitespace_Column => GType_String);
      end Columns;

      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer : Gtk_Cell_Renderer_Text;

      Col  : Gtk_Cell_Layout;
      Iter : Gtk_Tree_Iter;

      use Model_Map;
      C  : Cursor;
      M  : Target_Model_Access;
   begin
      Gtk_New (Combo);
      Gtk_New (Model, Columns);

      Set_Model (Combo, Gtk_Tree_Model (Model));

      Set_Text_Column (Combo, Name_Column);
      Col := Gtk.Combo_Box_Entry."+" (Combo);

      Gtk_New (Text_Renderer);
      Pack_Start (Col, Text_Renderer, False);
      Add_Attribute (Col, Text_Renderer, "text", Whitespace_Column);

      Gtk_New (Icon_Renderer);
      Pack_Start (Col, Icon_Renderer, False);
      Add_Attribute (Col, Icon_Renderer, "stock-id", Icon_Column);

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

         Set (Model, Iter, Name_Column, To_String (M.Name));
         Set (Model, Iter, Icon_Column, To_String (M.Icon));
         Set (Model, Iter, Desc_Column, To_String (M.Description));
         Set (Model, Iter, Whitespace_Column, "        ");

         Next (C);
      end loop;

      return Combo;
   end Models_Combo;

   ------------------------------
   -- New_Target_Name_Is_Valid --
   ------------------------------

   function New_Target_Name_Is_Valid
     (Registry : Build_Config_Registry_Access;
      Name     : Unbounded_String) return Boolean is
   begin
      --  Verify that the name is not empty
      if Name = "" then
         Information (-"Note: targets must have a non-empty name.");
         return False;
      end if;

      --  Verify that no target with this name exists
      if Contains (Registry.Targets, Name) then
         Information
           (-"A target named """ & To_String (Name) & """ already exists.");
         return False;
      end if;

      return True;
   end New_Target_Name_Is_Valid;

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
      Dialog : Gtk_Dialog;
      Combo  : Gtk_Combo_Box_Entry;
      Label  : Gtk_Label;

      Model_E : Gtk_Entry;
      Name_E  : Gtk_Entry;
      Cat_E   : Gtk_Entry;
      Table   : Gtk_Table;

      Hbox    : Gtk_Hbox;
      M       : Gtk_List_Store;
      Iter    : Gtk_Tree_Iter;

      use Model_Map;
      C : Cursor;

   begin
      Cancelled := False;

      Gtk_New (Dialog);
      Set_Transient_For (Dialog, Gtk_Window (Get_Toplevel (UI)));
      Set_Title (Dialog, -"New target");

      Gtk_New (Table, 3, 2, False);

      --  Add the models combo

      Gtk_New (Label, -"Target type");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 1, 2, Expand or Fill, 0, 3, 3);

      Combo := Models_Combo (UI);
      Model_E := Gtk_Entry (Get_Child (Combo));
      Set_Editable (Model_E, False);

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
      Select_Region (Name_E, 0);

      Attach (Table, Name_E, 1, 2, 0, 1, Expand or Fill, 0, 3, 3);

      --  Enter some relevant text and select it

      --  Add the category entry

      Gtk_New (Label, -"Target category");
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 2, 3, Expand or Fill, 0, 3, 3);

      Gtk_New (M, GType_Array'(0 => GType_String));
      Gtk_New_With_Model (Combo, M, 0);
      Cat_E := Gtk_Entry (Get_Child (Combo));

      Iter := Get_Iter_First (UI.View.Model);

      while Iter /= Null_Iter loop
         if Get_Int (UI.View.Model, Iter, 2) = 0 then
            --  This is to test that the iter is a category iter

            declare
               function Strip (S : String) return String;
               --  Strip S of pango markup

               -----------
               -- Strip --
               -----------

               function Strip (S : String) return String is
               begin
                  return S (S'First + 3 .. S'Last - 4);
               end Strip;

               Str : constant String :=
                       Strip (Get_String (UI.View.Model, Iter, Name_Column));
            begin
               Append_Text (Combo, Str);

               if Get_Text (Cat_E) = "" then
                  Set_Text (Cat_E, Str);
               end if;
            end;
         end if;

         Next (UI.View.Model, Iter);
      end loop;

      Attach (Table, Combo, 1, 2, 2, 3, Expand or Fill, 0, 3, 3);
      --  ??? make this a combo-box-entry with the already existing categories

      Pack_Start (Get_Vbox (Dialog), Table, False, False, 3);

      --  Add the buttons

      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);
      Show_All (Dialog);

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               Name     := To_Unbounded_String (Get_Text (Name_E));
               Model    := To_Unbounded_String (Get_Text (Model_E));
               Category := To_Unbounded_String (Get_Text (Cat_E));

               if New_Target_Name_Is_Valid (UI.Registry, Name) then
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
      Combo  : Gtk_Combo_Box_Entry;
      Label  : Gtk_Label;

      Name_E  : Gtk_Entry;
      Cat_E   : Gtk_Entry;
      Table   : Gtk_Table;

      Hbox    : Gtk_Hbox;
      M       : Gtk_List_Store;
      Iter    : Gtk_Tree_Iter;

   begin
      Cancelled := False;

      Gtk_New (Dialog);
      Set_Transient_For (Dialog, Gtk_Window (Get_Toplevel (UI)));
      Set_Title (Dialog, -"Clone target");

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
      Gtk_New_Hbox (Hbox);
      Pack_Start (Hbox, Label, False, False, 0);
      Attach (Table, Hbox, 0, 1, 2, 3, Expand or Fill, 0, 3, 3);

      Gtk_New (M, GType_Array'(0 => GType_String));
      Gtk_New_With_Model (Combo, M, 0);
      Cat_E := Gtk_Entry (Get_Child (Combo));

      Iter := Get_Iter_First (UI.View.Model);

      while Iter /= Null_Iter loop
         if Get_Int (UI.View.Model, Iter, 2) = 0 then
            --  This is to test that the iter is a category iter

            declare
               function Strip (S : String) return String;
               --  Strip S of pango markup

               function Strip (S : String) return String is
               begin
                  return S (S'First + 3 .. S'Last - 4);
               end Strip;

               Str : constant String :=
                 Strip (Get_String (UI.View.Model, Iter, Name_Column));
            begin
               Append_Text (Combo, Str);

               if Get_Text (Cat_E) = "" then
                  Set_Text (Cat_E, Str);
               end if;
            end;
         end if;

         Next (UI.View.Model, Iter);
      end loop;

      Attach (Table, Combo, 1, 2, 2, 3, Expand or Fill, 0, 3, 3);
      --  ??? make this a combo-box-entry with the already existing categories

      Pack_Start (Get_Vbox (Dialog), Table, False, False, 3);

      --  Add the buttons

      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);
      Show_All (Dialog);

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               Name := To_Unbounded_String (Get_Text (Name_E));
               Category := To_Unbounded_String (Get_Text (Cat_E));

               if New_Target_Name_Is_Valid (UI.Registry, Name) then
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
           (-"This target is a predefined target, and cannot be removed");
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

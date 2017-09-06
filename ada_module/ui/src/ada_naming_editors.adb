------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;               use GNAT.Strings;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with System;                     use System;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Combo_Box;              use Gtk.Combo_Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;

with GPR;                        use GPR;
with GPS.Intl;                   use GPS.Intl;
with GUI_Utils;                  use GUI_Utils;
with String_Hash;

package body Ada_Naming_Editors is

   Unit_Column     : constant := 0;
   --  Column where the unit name of the naming exception is stored

   Spec_Column     : constant := 1;
   --  Column where the spec name of the naming exception is stored

   Body_Column     : constant := 2;
   --  Column where the body name of the naming exception is stored

   Editable_Column : constant := 3;
   --  Column indicating if the given row is editable

   Exception_List_Column_Types : constant GType_Array :=
                                   (Unit_Column     => GType_String,
                                    Spec_Column     => GType_String,
                                    Body_Column     => GType_String,
                                    Editable_Column => GType_Boolean);
   --  Column types used for the naming exceptions list tree view

   Gnat_Naming_Scheme   : constant := 0;
   Apex_Naming_Scheme   : constant := 1;
   Dec_Naming_Scheme    : constant := 2;
   Custom_Naming_Scheme : constant := 3;
   --  Indexes of the predefined naming schemes combo choices

   Empty_Unit_Name : constant String := "<unit_name>";
   Empty_Spec_Name : constant String := "<spec_file> [<at index>]";
   Empty_Body_Name : constant String := "<body_file> [<at index>]";
   --  Used to fill newly created rows of the naming exceptions list tree view

   procedure On_Add_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when clicking on the '+' button of the exception list view

   procedure On_Remove_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when clicking on the '-' button of the exception list view

   procedure On_Standard_Scheme_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the standard naming cheme has changed

   procedure Customized
     (Object : access Gtk_Widget_Record'Class);
   --  Called when a specific naming scheme option has changed

   procedure Set_Predefined_Scheme
     (Editor     : access Ada_Naming_Editor_Record'Class;
      Scheme_Num : Natural);
   --  Changes all the fields in the GUI to the specified predefined scheme.
   --  The definition for Scheme_Num depends on the order of the entries in
   --  the combo box.

   procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record'Class;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Project_Type;
      Display_Exceptions : Boolean := True);
   --  Show the project's settings in the editor

   type Naming_Data is record
      Body_Name : GNAT.Strings.String_Access;
      Spec_Name : GNAT.Strings.String_Access;
   end record;
   type Naming_Data_Access is access all Naming_Data;

   procedure Free (Data : in out Naming_Data_Access);
   --  Free the memory occupied by Data

   package Naming_Hash is new String_Hash
     (Data_Type => Naming_Data_Access,
      Free_Data => Free,
      Null_Ptr  => null);
   use Naming_Hash.String_Hash_Table;

   -----------------------------
   -- On_Add_Naming_Exception --
   -----------------------------

   procedure On_Add_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor   : constant Ada_Naming_Editor := Ada_Naming_Editor (Self);
      New_Iter : Gtk_Tree_Iter;
   begin
      Editor.Exception_List_Model.Append (New_Iter, Parent => Null_Iter);

      Editor.Exception_List_Model.Set
        (New_Iter,
         Column => Unit_Column,
         Value  => Empty_Unit_Name);
      Editor.Exception_List_Model.Set
        (New_Iter,
         Column => Spec_Column,
         Value  => Empty_Spec_Name);
      Editor.Exception_List_Model.Set
        (New_Iter,
         Column => Body_Column,
         Value  => Empty_Body_Name);
      Editor.Exception_List_Model.Set
        (New_Iter,
         Column => Editable_Column,
         Value  => True);

      Editor.Exception_List.Scroll_To_Cell
        (Get_Path (Editor.Exception_List_Model, New_Iter),
         Column    => Get_Column (Editor.Exception_List, Unit_Column),
         Use_Align => False,
         Row_Align => 0.0,
         Col_Align => 0.0);
      Editor.Exception_List.Set_Cursor_On_Cell
        (Get_Path (Editor.Exception_List_Model, New_Iter),
         Focus_Column  => Get_Column (Editor.Exception_List, Unit_Column),
         Focus_Cell    => null,
         Start_Editing => True);
   end On_Add_Naming_Exception;

   --------------------------------
   -- On_Remove_Naming_Exception --
   --------------------------------

   procedure On_Remove_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor : constant Ada_Naming_Editor := Ada_Naming_Editor (Self);
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Editor.Exception_List), Model, Iter);

      if Iter /= Null_Iter then
         Editor.Exception_List_Model.Remove (Iter);
      end if;
   end On_Remove_Naming_Exception;

   --------------------------------
   -- On_Standard_Scheme_Changed --
   --------------------------------

   procedure On_Standard_Scheme_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      E     : constant Ada_Naming_Editor := Ada_Naming_Editor (Object);
      Value : Gint;

   begin
      if Get_Active_Iter (E.Standard_Scheme) /= Null_Iter then
         Value := Get_Active (E.Standard_Scheme);

         if Value /= Custom_Naming_Scheme then
            Set_Predefined_Scheme (E, Natural (Value));

            --  Restore the contents of the standard scheme buttons, that has
            --  been changed through callbacks when the changed the contents of
            --  the GUI.

            Set_Active (E.Standard_Scheme, Value);
         end if;
      end if;
   end On_Standard_Scheme_Changed;

   ----------------
   -- Customized --
   ----------------

   procedure Customized
     (Object : access Gtk_Widget_Record'Class)
   is
      E : constant Ada_Naming_Editor := Ada_Naming_Editor (Object);
   begin
      Set_Active (E.Standard_Scheme, Custom_Naming_Scheme);
   end Customized;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Naming_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Naming_Data, Naming_Data_Access);
   begin
      if Data /= null then
         Free (Data.Body_Name);
         Free (Data.Spec_Name);
         Unchecked_Free (Data);
      end if;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Ada_Naming_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := GNATCOLL.Projects.No_Project)
   is
      Idx : Gint := 0;
      Group_Widget : Dialog_Group_Widget;
      Button       : Gtk_Button;
      Col_Number   : Gint;

      procedure Create_Naming_Scheme_Combo
        (Combo      : out Gtk_Combo_Box_Text;
         Label      : String;
         Values     : Unbounded_String_Array;
         Default    : Gint := 0;
         Doc        : String := "";
         With_Entry : Boolean := True;
         Changed_Cb : Widget_Callback.Simple_Handler := Customized'Access);
      --  Create a combo widget with the given Label and using Values as valid
      --  choices.

      --------------------------------
      -- Create_Naming_Scheme_Combo --
      --------------------------------

      procedure Create_Naming_Scheme_Combo
        (Combo      : out Gtk_Combo_Box_Text;
         Label      : String;
         Values     : Unbounded_String_Array;
         Default    : Gint := 0;
         Doc        : String := "";
         With_Entry : Boolean := True;
         Changed_Cb : Widget_Callback.Simple_Handler := Customized'Access) is
      begin
         if With_Entry then
            Gtk_New_With_Entry (Combo);
         else
            Gtk_New (Combo);
         end if;

         Group_Widget.Create_Child
           (Widget    => Combo,
            Label     => Label,
            Doc       => Doc,
            Expand    => False);

         for Value of Values loop
            Combo.Append_Text (To_String (Value));
         end loop;

         Combo.Set_Active (Default);
         Widget_Callback.Object_Connect
           (Combo, Gtk.Combo_Box.Signal_Changed,
            Changed_Cb, Self);
      end Create_Naming_Scheme_Combo;

      procedure Create_Exceptions_List_Column (Name : String);
      --  Create and append a column to the exceptions list tree view

      -----------------------------------
      -- Create_Exceptions_List_Column --
      -----------------------------------

      procedure Create_Exceptions_List_Column (Name : String)
      is
         Render       : Gtk_Cell_Renderer_Text;
         Col          : Gtk_Tree_View_Column;
      begin
         Gtk_New (Col);
         Col_Number := Append_Column (Self.Exception_List, Col);
         Set_Title (Col, Name);
         Gtk_New (Render);
         Pack_Start (Col, Render, False);
         Set_Sort_Column_Id (Col, Col_Number - 1);
         Add_Attribute (Col, Render, "text", Col_Number - 1);
         Add_Attribute (Col, Render, "editable", 3);
         Set_Editable_And_Callback
           (Self.Exception_List_Model, Render, Col_Number - 1);
      end Create_Exceptions_List_Column;

   begin
      Dialog_Utils.Initialize (Self);

      --  Create the 'Scheme' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View => Self,
         Group_Name  => "Scheme");

      --  Create the naming scheme editor combo widget
      Create_Naming_Scheme_Combo
        (Self.Standard_Scheme,
         Label      => "Naming scheme",
         Values     => (1 => To_Unbounded_String ("GNAT default"),
                        2 => To_Unbounded_String ("unit.separate.1.ada"),
                        3 => To_Unbounded_String ("unit__separate_.ada"),
                        4 => To_Unbounded_String ("<custom>")),
         Doc        => "Choose among a list of predefined Ada naming schemes.",
         With_Entry => False,
         Changed_Cb => On_Standard_Scheme_Changed'Access);

      --  Create the 'Details' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View => Self,
         Group_Name  => "Details");

      --  Create the filename casing editor combo widget
      Create_Naming_Scheme_Combo
        (Self.Casing,
         Label  => "Filename casing",
         Values => (1 => Null_Unbounded_String),
         Doc    => "Choose a casing policy for file names.");

      --  Create the dot replacement entry widget
      Create_Naming_Scheme_Combo
        (Self.Dot_Replacement,
         Label      => "Dot replacement",
         Values     => (1 => To_Unbounded_String ("-")),
         Doc        => "Choose the string used to replace the dot in"
         & "unit names.");

      --  Create the combo widgets for the extensions
      Create_Naming_Scheme_Combo
        (Self.Spec_Extension,
         Label  => "Spec extensions",
         Values => (1 => To_Unbounded_String (".ads"),
                    2 => To_Unbounded_String (".1.ada"),
                    3 => To_Unbounded_String ("_.ada")),
         Doc    => "Choose a suffix for file names that contain "
         & "specifications.");
      Create_Naming_Scheme_Combo
        (Self.Body_Extension,
         Label  => "Body extensions",
         Values => (1 => To_Unbounded_String (".adb"),
                    2 => To_Unbounded_String (".2.ada"),
                    3 => To_Unbounded_String (".ada")),
         Doc    => "Choose a suffix for file names that contain bodies.");
      Create_Naming_Scheme_Combo
        (Self.Separate_Extension,
         Label  => "Separate extensions",
         Values => (1 => To_Unbounded_String (".adb"),
                    2 => To_Unbounded_String (".2.ada"),
                    3 => To_Unbounded_String (".ada")),
         Doc    => "Choose a suffix for file names that contain subunits "
         & "(separate bodies).");

      --  Create the 'Exceptions' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Self,
         Group_Name          => "Exceptions",
         Allow_Multi_Columns => False);

      --  Create the list view containing all the naming exceptions
      Self.Exception_List_View := new Dialog_View_With_Button_Box_Record;
      Initialize
        (Self.Exception_List_View,
         Position => Pos_Left);
      Group_Widget.Append_Child
        (Widget    => Self.Exception_List_View,
         Expand    => True,
         Fill      => True);

      --  Create the 'add' and 'remove' buttons
      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-add-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked
        (Call  => On_Add_Naming_Exception'Access,
         Slot  => Self);
      Self.Exception_List_View.Append_Button (Button);

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-remove-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked
        (Call  => On_Remove_Naming_Exception'Access,
         Slot  => Self);
      Self.Exception_List_View.Append_Button (Button);

      --  Create the naming exceptions list tree view
      Gtk_New (Self.Exception_List_Model, Exception_List_Column_Types);
      Gtk_New (Self.Exception_List,
               Self.Exception_List_Model);
      Self.Exception_List_View.Append
        (Widget        => Self.Exception_List,
         Expand        => True,
         Fill          => True);

      Set_Mode
        (Get_Selection (Self.Exception_List),
         Selection_Single);

      --  Create the naming exceptions list tree view columns
      Create_Exceptions_List_Column ("Unit name");
      Create_Exceptions_List_Column ("Spec filename");
      Create_Exceptions_List_Column ("Body filename");

      Self.Standard_Scheme.Set_Sensitive (not Read_Only);
      Self.Casing.Set_Sensitive (not Read_Only);
      Self.Dot_Replacement.Set_Sensitive (not Read_Only);
      Self.Spec_Extension.Set_Sensitive (not Read_Only);
      Self.Body_Extension.Set_Sensitive (not Read_Only);
      Self.Separate_Extension.Set_Sensitive (not Read_Only);
      Self.Exception_List_View.Set_Sensitive (not Read_Only);

      Gtk_List_Store'(-(Self.Casing.Get_Model)).Clear;

      for Casing in Casing_Type loop
         if Casing /= Unknown then
            Self.Casing.Append_Text (-GPR.Image (Casing));

            if Casing = All_Lower_Case then
               Self.Casing.Set_Active (Idx);
            end if;

            Idx := Idx + 1;
         end if;
      end loop;

      Set_Active (Self.Standard_Scheme, Gnat_Naming_Scheme);

      Show_Project_Settings
        (Self, Kernel, Project, Display_Exceptions => True);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self      : not null access Ada_Naming_Editor_Record;
      Languages : GNAT.Strings.String_List) return Boolean
   is
      Visible : constant Boolean := In_List ("ada", Languages);
   begin
      --  When the Ada language is not used in the project yet, set the GNAT
      --  naming scheme as the default one: this will allow to have it by
      --  default if the user selects to incorporate the Ada language in
      --  the project via the Project Properties editor.
      if not Visible
        and then Self.Standard_Scheme.Get_Active = Custom_Naming_Scheme
      then
         Self.Standard_Scheme.Set_Active (Gnat_Naming_Scheme);
      end if;

      return Visible;
   end Is_Visible;

   ---------------------------
   -- Set_Predefined_Scheme --
   ---------------------------

   procedure Set_Predefined_Scheme
     (Editor : access Ada_Naming_Editor_Record'Class;
      Scheme_Num : Natural) is
   begin
      case Scheme_Num is
         when Gnat_Naming_Scheme =>
            --  GNAT Default
            Set_Active_Text
              (Editor.Casing, -GPR.Image (All_Lower_Case));
            Set_Active_Text
              (Editor.Dot_Replacement, Default_Gnat_Dot_Replacement);
            Set_Active_Text
              (Editor.Spec_Extension, Default_Gnat_Spec_Suffix);
            Set_Active_Text
              (Editor.Body_Extension, Default_Gnat_Body_Suffix);
            Set_Active_Text
              (Editor.Separate_Extension, Default_Gnat_Separate_Suffix);

         when Apex_Naming_Scheme =>
            --  APEX Default
            Set_Active_Text (Editor.Casing, -GPR.Image (All_Lower_Case));
            Set_Active_Text (Editor.Dot_Replacement, ".");
            Set_Active_Text (Editor.Spec_Extension, ".1.ada");
            Set_Active_Text (Editor.Body_Extension, ".2.ada");
            Set_Active_Text (Editor.Separate_Extension, ".2.ada");

         when Dec_Naming_Scheme =>
            --  DEC Ada Default
            Set_Active_Text (Editor.Casing, -GPR.Image (All_Lower_Case));
            Set_Active_Text (Editor.Dot_Replacement, "__");
            Set_Active_Text (Editor.Spec_Extension, "_.ada");
            Set_Active_Text (Editor.Body_Extension, ".ada");
            Set_Active_Text (Editor.Separate_Extension, ".ada");

         when others =>
            null;
      end case;
   end Set_Predefined_Scheme;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Editor             : not null access Ada_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Kernel, Languages);
      Changed    : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      Ada_Scheme : constant Boolean :=
                     Get_Active (Editor.Standard_Scheme) = 0;
      Cache      : Naming_Hash.String_Hash_Table.Instance;
      Data       : Naming_Data;

      procedure Update_If_Required
        (Name : Attribute_Pkg_String; Value : String; Index : String);
      --  Update the attribute if necessary

      procedure Update_Exceptions (Attr : Attribute_Pkg_String);
      --  Update the list of exceptions for Attr.
      --  If there is any change, sets Changed to True.

      ------------------------
      -- Update_If_Required --
      ------------------------

      procedure Update_If_Required
        (Name : Attribute_Pkg_String; Value : String; Index : String)
      is
         Modified : Boolean := False;
      begin
         if Project = GNATCOLL.Projects.No_Project then
            Modified := True;

         else
            declare
               Old : constant String :=
                       Project.Attribute_Value
                         (Attribute => Name,
                          Index     => Index);
            begin
               Modified := Value /= Old
                 and then (Old /= "" or else not Ada_Scheme);
            end;
         end if;

         if Modified then
            Changed := True;
            if Ada_Scheme then
               Project.Delete_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Name,
                  Index     => Index);
            else
               Project.Set_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Name,
                  Value     => Value,
                  Index     => Index);
            end if;
         end if;
      end Update_If_Required;

      -----------------------
      -- Update_Exceptions --
      -----------------------

      procedure Update_Exceptions (Attr : Attribute_Pkg_String) is
         Indexes     : GNAT.Strings.String_List :=
           Project.Attribute_Indexes (Attr);
         Data        : Naming_Data_Access;
         Cache_Iter  : Naming_Hash.String_Hash_Table.Cursor;
         Val         : GNAT.Strings.String_Access;
         Last        : Natural;
         Idx         : Integer;

      begin
         --  Modify existing exceptions (and detect whether there have
         --  been any changes).
         --  We reset to null the fields in the cache, so that we can more
         --  easily find the new values later on.

         for Index in Indexes'Range loop
            Data := Get (Cache, Indexes (Index).all);

            if Data /= null then
               if Attr = Spec_Attribute then
                  Val := Data.Spec_Name;
                  Data.Spec_Name := null;  --  modify in place in htable
               else
                  Val := Data.Body_Name;
                  Data.Body_Name := null;
               end if;
            else
               Val := null;
            end if;

            if Val = null then
               Changed := True;  --  We had an exception , no longer have one
               Project.Delete_Attribute (Attr, Index => Indexes (Index).all);

            else
               if Project.Attribute_Value
                 (Attribute => Attr, Index => Indexes (Index).all)
                 /= Val.all
               then
                  Last := Ada.Strings.Fixed.Index (Val.all, " at ");

                  if Last < Val'First then
                     Last := Val'Last + 1;
                     Idx  := 0;
                  else
                     Idx  := Integer'Value (Val (Last + 4 .. Val'Last));
                  end if;

                  Changed := True;  --  Exception has changed
                  Project.Set_Attribute
                    (Attr,
                     Index    => Indexes (Index).all,
                     Value    => Val (Val'First .. Last - 1),
                     At_Index => Idx);
               end if;
            end if;

            Free (Val);
         end loop;

         --  Then add new exceptions

         Get_First (Cache, Cache_Iter);
         loop
            Data := Get_Element (Cache_Iter);
            exit when Data = null;

            if Attr = Spec_Attribute then
               Val := Data.Spec_Name;
            else
               Val := Data.Body_Name;
            end if;

            if Val /= null then
               Changed := True;  --  new exception added

               Last := Ada.Strings.Fixed.Index (Val.all, " at ");

               if Last < Val'First then
                  Last := Val'Last + 1;
                  Idx  := 0;
               else
                  Idx  := Integer'Value (Val (Last + 4 .. Val'Last));
               end if;

               Project.Set_Attribute
                 (Attr,
                  Index    => Get_Key (Cache_Iter),
                  Value    => Val (Val'First .. Last - 1),
                  At_Index => Idx);
            end if;

            Get_Next (Cache, Cache_Iter);
         end loop;

         Free (Indexes);
      end Update_Exceptions;

   begin
      Update_If_Required
        (Spec_Suffix_Attribute,
         Get_Active_Text (Editor.Spec_Extension), "ada");
      Update_If_Required
        (Impl_Suffix_Attribute,
         Get_Active_Text (Editor.Body_Extension), "ada");
      Update_If_Required
        (Separate_Suffix_Attribute,
         Get_Active_Text (Editor.Separate_Extension), "");
      Update_If_Required
        (Casing_Attribute,
         GPR.Image (Casing_Type'Val
                    (Get_Active (Editor.Casing))), "");
      Update_If_Required
        (Dot_Replacement_Attribute,
         Get_Active_Text (Editor.Dot_Replacement), "");

      --  Fill the hash table

      Iter := Get_Iter_First (Editor.Exception_List_Model);

      while Iter /= Null_Iter loop
         declare
            U : constant String :=
                  Get_String (Editor.Exception_List_Model, Iter, 0);
            Spec : constant String :=
                     Get_String (Editor.Exception_List_Model, Iter, 1);
            Bod : constant String :=
                     Get_String (Editor.Exception_List_Model, Iter, 2);
         begin
            if Spec /= "" then
               Data.Spec_Name := new String'(Spec);
            else
               Data.Spec_Name := null;
            end if;

            if Bod /= "" then
               Data.Body_Name := new String'(Bod);
            else
               Data.Body_Name := null;
            end if;

            Set (Cache, U, new Naming_Data'(Data));
         end;

         Next (Editor.Exception_List_Model, Iter);
      end loop;

      --  Update the project if needed

      Changed := Changed or else Project = GNATCOLL.Projects.No_Project;

      --  Computing whether a list has changed is extremely fast now that the
      --  hash table has been created. Much faster than updating the attributes
      --  for nothing.

      Update_Exceptions (Spec_Attribute);
      Update_Exceptions (Body_Attribute);

      Reset (Cache);

      return Changed;
   end Edit_Project;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record'Class;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Project_Type;
      Display_Exceptions : Boolean := True)
   is
      procedure Set_Unit_Spec
        (Model    : System.Address;
         Iter     : Gtk_Tree_Iter;
         Col      : Gint := 0; Unit     : String;
         Col2     : Gint := 1; Spec     : String;
         Col3     : Gint := 3; Editable : Gint := 1);
      pragma Import (C, Set_Unit_Spec, "ada_gtk_tree_store_set_ptr_ptr_int");

      pragma Unreferenced (Kernel);
      Dot_Replacement : constant String :=
                          Project.Attribute_Value
                            (Dot_Replacement_Attribute,
                             Default => Default_Gnat_Dot_Replacement);
      Casing          : constant String :=
                          Project.Attribute_Value
                            (Casing_Attribute,
                             Default => -GPR.Image (All_Lower_Case));
      Separate_Suffix : constant String :=
                          Project.Attribute_Value
                            (Separate_Suffix_Attribute,
                             Default => Default_Gnat_Separate_Suffix);
      Body_Suffix     : constant String :=
                          Project.Attribute_Value
                            (Impl_Suffix_Attribute,
                             Index   => "ada",
                             Default => Default_Gnat_Body_Suffix);
      Spec_Suffix     : constant String :=
                          Project.Attribute_Value
                            (Spec_Suffix_Attribute,
                             Index   => "ada",
                             Default => Default_Gnat_Spec_Suffix);
      Id              : Gint;

   begin
      Set_Active_Text (Editor.Dot_Replacement, Dot_Replacement);
      Set_Active_Text (Editor.Casing, -Casing);
      Set_Active_Text (Editor.Spec_Extension, Spec_Suffix);
      Set_Active_Text (Editor.Body_Extension, Body_Suffix);
      Set_Active_Text (Editor.Separate_Extension, Separate_Suffix);

      Clear (Editor.Exception_List_Model);
      Id := Freeze_Sort (Editor.Exception_List_Model);

      if Display_Exceptions then
         declare
            Specs  : GNAT.Strings.String_List :=
              Project.Attribute_Indexes (Spec_Attribute);
            Bodies  : GNAT.Strings.String_List :=
              Project.Attribute_Indexes (Body_Attribute);

            Iter   : Gtk_Tree_Iter;
         begin
            for S in Specs'Range loop
               Append (Editor.Exception_List_Model, Iter, Null_Iter);
               Set_Unit_Spec
                 (Get_Object (Editor.Exception_List_Model), Iter,
                  Col   => 0,
                  Unit  => Specs (S).all & ASCII.NUL,
                  Col2  => 1,
                  Spec  => Project.Attribute_Value
                    (Spec_Attribute,
                     Index => Specs (S).all) & ASCII.NUL);
            end loop;

            for B in Bodies'Range loop
               --  ??? We could store the name_id for the unit name, to avoid a
               --  string comparison, and save time. However, this won't be
               --  needed if C303-008 is implemented (merging the two exception
               --  lists)
               declare
                  Unit : constant String := Bodies (B).all;
               begin
                  Iter := Get_Iter_First (Editor.Exception_List_Model);
                  while Iter /= Null_Iter loop
                     exit when Get_String
                       (Editor.Exception_List_Model, Iter, 0) = Unit;
                     Next (Editor.Exception_List_Model, Iter);
                  end loop;
               end;

               if Iter = Null_Iter then
                  Append (Editor.Exception_List_Model, Iter, Null_Iter);
                  Set_Unit_Spec
                    (Get_Object (Editor.Exception_List_Model), Iter,
                     Col   => 0,
                     Unit  => Bodies (B).all & ASCII.NUL,
                     Col2  => 2,
                     Spec  => Project.Attribute_Value
                       (Body_Attribute, Index => Bodies (B).all)
                       & ASCII.NUL);

               else
                  Set (Editor.Exception_List_Model, Iter,
                       Column => 2,
                       Value  => Project.Attribute_Value
                         (Body_Attribute, Index => Bodies (B).all)
                         & ASCII.NUL);
               end if;
            end loop;

            Free (Specs);
            Free (Bodies);
         end;
      end if;

      Thaw_Sort (Editor.Exception_List_Model, Id);

      --  GNAT naming scheme ?
      if Get_Iter_First (Editor.Exception_List_Model) /= Null_Iter
        or else Get_Active_Text (Editor.Casing) /=
          -GPR.Image (All_Lower_Case)
      then
         Set_Active (Editor.Standard_Scheme, Custom_Naming_Scheme);

      elsif Get_Active_Text (Editor.Dot_Replacement) =
          Default_Gnat_Dot_Replacement
        and then Get_Active_Text (Editor.Spec_Extension) =
        Default_Gnat_Spec_Suffix
        and then Get_Active_Text (Editor.Body_Extension) =
        Default_Gnat_Body_Suffix
        and then Get_Active_Text (Editor.Separate_Extension) =
        Default_Gnat_Separate_Suffix
      then
         Set_Active (Editor.Standard_Scheme, Gnat_Naming_Scheme);

      elsif Get_Active_Text (Editor.Dot_Replacement) = "."
        and then Get_Active_Text (Editor.Spec_Extension) = ".1.ada"
        and then Get_Active_Text (Editor.Body_Extension) = ".2.ada"
        and then Get_Active_Text (Editor.Separate_Extension) = ".2.ada"
      then
         Set_Active (Editor.Standard_Scheme, Apex_Naming_Scheme);

      elsif Get_Active_Text (Editor.Dot_Replacement) = "__"
        and then Get_Active_Text (Editor.Spec_Extension) = "_.ada"
        and then Get_Active_Text (Editor.Body_Extension) = ".ada"
        and then Get_Active_Text (Editor.Separate_Extension) = ".ada"
      then
         Set_Active (Editor.Standard_Scheme, Dec_Naming_Scheme);

      else
         Set_Active (Editor.Standard_Scheme, Custom_Naming_Scheme);
      end if;
   end Show_Project_Settings;

end Ada_Naming_Editors;

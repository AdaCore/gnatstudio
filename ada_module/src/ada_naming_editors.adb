-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Box;                  use Gtk.Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.List;                 use Gtk.List;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Casing;                   use Casing;
with Prj;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;
with GUI_Utils;                use GUI_Utils;
with Glide_Intl;               use Glide_Intl;
with Projects;                 use Projects;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Projects.Editor;          use Projects, Projects.Editor;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with String_Hash;
with System;                   use System;

package body Ada_Naming_Editors is

   Empty_Unit_Name : constant String := "<unit_name>";
   Empty_Spec_Name : constant String := "<spec_file>";
   Empty_Body_Name : constant String := "<body_file>";

   Default_Gnat_Dot_Replacement : constant String := "-";
   Default_Gnat_Spec_Suffix     : constant String := ".ads";
   Default_Gnat_Body_Suffix     : constant String := ".adb";
   Default_Gnat_Separate_Suffix : constant String := ".adb";
   --  Default settings for the GNAT naming scheme.

   Gnat_Naming_Scheme   : constant := 0;
   Apex_Naming_Scheme   : constant := 1;
   Dec_Naming_Scheme    : constant := 2;
   Custom_Naming_Scheme : constant := 3;

   type Naming_Data is record
      Body_Name : GNAT.OS_Lib.String_Access;
      Spec_Name : GNAT.OS_Lib.String_Access;
   end record;
   No_Data : constant Naming_Data := (null, null);

   procedure Free (Data : in out Naming_Data);
   --  Free the memory occupied by Data

   package Naming_Hash is new String_Hash
     (Data_Type => Naming_Data,
      Free_Data => Free,
      Null_Ptr  => No_Data);
   use Naming_Hash.String_Hash_Table;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Naming_Data) is
   begin
      Free (Data.Body_Name);
      Free (Data.Spec_Name);
   end Free;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Ada_Naming_Editor) is
      Casing_Items : Gtk.Enums.String_List.Glist;
      Size_Group   : Gtk_Size_Group;
   begin
      Editor := new Ada_Naming_Editor_Record;
      Gtk_New (Editor.GUI);

      Set_Width_Chars (Editor.GUI.Unit_Name_Entry, 8);
      Set_Width_Chars (Editor.GUI.Spec_Filename_Entry, 8);
      Set_Width_Chars (Editor.GUI.Body_Filename_Entry, 8);

      Set_Size_Request (Editor.GUI.Exception_List, -1, 170);
      Gtk_New (Size_Group, Both);
      Add_Widget (Size_Group, Editor.GUI.Standard_Scheme);
      Add_Widget (Size_Group, Editor.GUI.Casing);
      Add_Widget (Size_Group, Editor.GUI.Dot_Replacement);
      Add_Widget (Size_Group, Editor.GUI.Spec_Extension);
      Add_Widget (Size_Group, Editor.GUI.Body_Extension);
      Add_Widget (Size_Group, Editor.GUI.Separate_Extension);

      Gtk_New (Size_Group, Both);
      Add_Widget (Size_Group, Editor.GUI.Label_Naming_Scheme);
      Add_Widget (Size_Group, Editor.GUI.Label_Casing);
      Add_Widget (Size_Group, Editor.GUI.Label_Dot_Replacement);
      Add_Widget (Size_Group, Editor.GUI.Label_Spec_Extensions);
      Add_Widget (Size_Group, Editor.GUI.Label_Body_Extensions);
      Add_Widget (Size_Group, Editor.GUI.Label_Separate_Extensions);

      Ref (Editor.GUI.Main_Box);
      Unparent (Editor.GUI.Main_Box);

      Reset_Exception_Fields (Editor.GUI);

      for Casing in Casing_Type loop
         if Casing /= Unknown then
            Gtk.Enums.String_List.Append (Casing_Items, -Prj.Image (Casing));
         end if;
      end loop;

      Set_Popdown_Strings (Editor.GUI.Casing, Casing_Items);
      Set_Text (Get_Entry (Editor.GUI.Casing), -Prj.Image (All_Lower_Case));
      Free_String_List (Casing_Items);

      Select_Item (Get_List (Editor.GUI.Standard_Scheme), Gnat_Naming_Scheme);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Editor : access Ada_Naming_Editor_Record) is
   begin
      Destroy (Editor.GUI);
      --  Editor.GUI.Main_Box is automatically destroyed when its parent
      --  contain (multi-language naming editor) is destroyed.
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Ada_Naming_Editor_Record) return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.GUI.Main_Box);
   end Get_Window;

   ----------------------------
   -- Reset_Exception_Fields --
   ----------------------------

   procedure Reset_Exception_Fields
     (Editor : access Naming_Scheme_Editor_Record'Class;
      Field  : Gtk.GEntry.Gtk_Entry := null) is
   begin
      if Field = null then
         Set_Text (Editor.Unit_Name_Entry, Empty_Unit_Name);
         Set_Text (Editor.Spec_Filename_Entry, Empty_Spec_Name);
         Set_Text (Editor.Body_Filename_Entry, Empty_Body_Name);

      elsif Field = Editor.Unit_Name_Entry then
         Set_Text (Field, Empty_Unit_Name);

      elsif Field = Editor.Spec_Filename_Entry then
         Set_Text (Field, Empty_Spec_Name);

      elsif Field = Editor.Body_Filename_Entry then
         Set_Text (Field, Empty_Body_Name);
      end if;
   end Reset_Exception_Fields;

   ---------------------------
   -- Set_Predefined_Scheme --
   ---------------------------

   procedure Set_Predefined_Scheme
     (Editor : access Naming_Scheme_Editor_Record'Class;
      Scheme_Num : Natural) is
   begin
      case Scheme_Num is
         when Gnat_Naming_Scheme =>
            --  GNAT Default
            Set_Text (Get_Entry (Editor.Casing), -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, Default_Gnat_Dot_Replacement);
            Set_Text (Get_Entry (Editor.Spec_Extension),
                      Default_Gnat_Spec_Suffix);
            Set_Text (Get_Entry (Editor.Body_Extension),
                      Default_Gnat_Body_Suffix);
            Set_Text (Get_Entry (Editor.Separate_Extension),
                      Default_Gnat_Separate_Suffix);

         when Apex_Naming_Scheme =>
            --  APEX Default
            Set_Text (Get_Entry (Editor.Casing),
                      -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, ".");
            Set_Text (Get_Entry (Editor.Spec_Extension), ".1.ada");
            Set_Text (Get_Entry (Editor.Body_Extension), ".2.ada");
            Set_Text (Get_Entry (Editor.Separate_Extension), ".2.ada");

         when Dec_Naming_Scheme =>
            --  DEC Ada Default
            Set_Text (Get_Entry (Editor.Casing), -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, "__");
            Set_Text (Get_Entry (Editor.Spec_Extension), "_.ada");
            Set_Text (Get_Entry (Editor.Body_Extension), ".ada");
            Set_Text (Get_Entry (Editor.Separate_Extension), ".ada");

         when others =>
            null;
      end case;
   end Set_Predefined_Scheme;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor  : access Ada_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean
   is
      Changed  : Boolean := False;
      Iter : Gtk_Tree_Iter;
      Ada_Scheme : constant Boolean :=
        Get_Index_In_List (Editor.GUI.Standard_Scheme) = 0;
      Cache   : Naming_Hash.String_Hash_Table.HTable;
      Data : Naming_Data;
      Cache_Iter : Naming_Hash.String_Hash_Table.Iterator;

      type Array2 is array (Gint'(1) .. 2) of Integer;
      Lengths : Array2 := (others => -1);

      procedure Update_If_Required
        (Name : Attribute_Pkg; Value : String; Index : String);
      --  Update the attribute if necessary

      function List_Changed (List : Associative_Array; Column : Gint)
         return Boolean;
      --  True if the list of elements in List is different from the elements
      --  in column Column of the exceptions list.

      function Rows_Count (Column : Gint) return Natural;
      --  Return the number of non-blank rows in Column

      ------------------------
      -- Update_If_Required --
      ------------------------

      procedure Update_If_Required
        (Name : Attribute_Pkg; Value : String; Index : String)
      is
         Modified : Boolean := False;
      begin
         if Project = No_Project then
            Modified := True;

         else
            declare
               Old : constant String := Get_Attribute_Value
                 (Project        => Project,
                  Attribute      => Name,
                  Index          => Index);
            begin
               Modified := Value /= Old
                 and then (Old /= "" or else not Ada_Scheme);
            end;
         end if;

         if Modified then
            Changed := True;
            if Ada_Scheme then
               Delete_Attribute
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Name,
                  Attribute_Index    => Index);
            else
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          => Name,
                  Value              => Value,
                  Attribute_Index    => Index);
            end if;
         end if;
      end Update_If_Required;

      ----------------
      -- Rows_Count --
      ----------------

      function Rows_Count (Column : Gint) return Natural is
         Length : Natural := 0;
         Iter   : Naming_Hash.String_Hash_Table.Iterator;
         Data   : Naming_Data;
      begin
         Get_First (Cache, Iter);
         loop
            Data := Get_Element (Iter);
            exit when Data = No_Data;
            if (Column = 1 and then Data.Spec_Name /= null)
              or else (Column = 2 and then Data.Body_Name /= null)
            then
               Length := Length + 1;
            end if;

            Get_Next (Cache, Iter);
         end loop;
         return Length;
      end Rows_Count;

      ------------------
      -- List_Changed --
      ------------------

      function List_Changed (List : Associative_Array; Column : Gint)
         return Boolean
      is
         Data : Naming_Data;
      begin
         Lengths (Column) := Rows_Count (Column);

         if List'Length /= Lengths (Column) then
            return True;
         end if;

         for Elem in List'Range loop
            declare
               U     : constant String := Get_String (List (Elem).Index);
               Value : constant String := To_String (List (Elem).Value);
            begin
               Data := Get (Cache, U);
               if Data = No_Data
                 or else (Column = 1
                          and then (Data.Spec_Name = null
                                    or else Data.Spec_Name.all /= Value))
                 or else (Column = 2
                          and then (Data.Body_Name = null
                                    or else Data.Body_Name.all /= Value))
               then
                  return True;
               end if;
            end;
         end loop;

         return False;
      end List_Changed;

   begin
      Update_If_Required
        (Spec_Suffix_Attribute,
         Get_Text (Get_Entry (Editor.GUI.Spec_Extension)), Ada_String);
      Update_If_Required
        (Impl_Suffix_Attribute,
         Get_Text (Get_Entry (Editor.GUI.Body_Extension)), Ada_String);
      Update_If_Required
        (Separate_Suffix_Attribute,
         Get_Text (Get_Entry (Editor.GUI.Separate_Extension)), "");
      Update_If_Required
        (Casing_Attribute,
         Prj.Image (Casing_Type'Val
                    (Get_Index_In_List (Editor.GUI.Casing))), "");
      Update_If_Required
        (Dot_Replacement_Attribute, Get_Text (Editor.GUI.Dot_Replacement), "");

      --  Fill the hash table
      Iter := Get_Iter_First (Editor.GUI.Exception_List_Model);
      while Iter /= Null_Iter loop
         declare
            U : constant String :=
              Get_String (Editor.GUI.Exception_List_Model, Iter, 0);
            Spec : constant String :=
              Get_String (Editor.GUI.Exception_List_Model, Iter, 1);
            Bod : constant String :=
              Get_String (Editor.GUI.Exception_List_Model, Iter, 2);
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

            Set (Cache, U, Data);
         end;

         Next (Editor.GUI.Exception_List_Model, Iter);
      end loop;

      --  Update the project if needed

      Changed := Changed
        or else Project = No_Project;

      --  Computing whether a list has changed is extremely fast now that the
      --  hash table has been created. Much faster than updating the attributes
      --  for nothing.

      for L in Lengths'Range loop
         if (L = 1 and then List_Changed
             (Get_Attribute_Value (Project, Specification_Attribute), L))
           or else (L = 2 and then List_Changed
             (Get_Attribute_Value (Project, Implementation_Attribute), L))
         then
            Changed := True;

            declare
               Values : Associative_Array_Values (1 .. Lengths (L));
               Index  : Natural := Values'First;
            begin
               Get_First (Cache, Cache_Iter);
               loop
                  Data := Get_Element (Cache_Iter);
                  exit when Data = No_Data;

                  if L = 1
                    and then Data.Spec_Name /= null
                  then
                     Values (Index) :=
                       (Index => new String'(Get_Key (Cache_Iter)),
                        Value => Data.Spec_Name);
                     Index := Index + 1;

                  elsif L = 2
                    and then Data.Body_Name /= null
                  then
                     Values (Index) :=
                       (Index => new String'(Get_Key (Cache_Iter)),
                        Value => Data.Body_Name);
                     Index := Index + 1;
                  end if;

                  Get_Next (Cache, Cache_Iter);
               end loop;

               if L = 1 then
                  Set_Attribute_Value_In_Scenario
                    (Project, Scenario_Variables, Specification_Attribute,
                     Values (Values'First .. Index - 1));
               else
                  Set_Attribute_Value_In_Scenario
                    (Project, Scenario_Variables, Implementation_Attribute,
                     Values (Values'First .. Index - 1));
               end if;

               for V in Values'First .. Index - 1 loop
                  Free (Values (V).Index);
               end loop;
            end;
         end if;
      end loop;

      Reset (Cache);

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True)
   is
      procedure Set_Unit_Spec
        (Model : System.Address;
         Iter  : Gtk_Tree_Iter;
         Col   : Gint := 0;
         Unit  : String;
         Col2  : Gint := 1;
         Spec  : String;
         Col3  : Gint := 3;
         Editable : Boolean := True;
         Last  : Gint := -1);
      pragma Import (C, Set_Unit_Spec, "gtk_tree_store_set");

      pragma Unreferenced (Kernel);
      Dot_Replacement : constant String := Get_Attribute_Value
        (Project, Dot_Replacement_Attribute,
         Default => Default_Gnat_Dot_Replacement);
      Casing : constant String := Get_Attribute_Value
        (Project, Casing_Attribute,
         Default => -Prj.Image (All_Lower_Case));
      Separate_Suffix : constant String := Get_Attribute_Value
        (Project, Separate_Suffix_Attribute,
         Default => Default_Gnat_Separate_Suffix);
      Body_Suffix : constant String := Get_Attribute_Value
        (Project, Impl_Suffix_Attribute,
         Index => Ada_String, Default => Default_Gnat_Body_Suffix);
      Spec_Suffix : constant String := Get_Attribute_Value
        (Project, Spec_Suffix_Attribute,
         Index => Ada_String, Default => Default_Gnat_Spec_Suffix);
      Id : Gint;
   begin
      Set_Text (Editor.GUI.Dot_Replacement,                Dot_Replacement);
      Set_Text (Get_Entry (Editor.GUI.Casing),            -Casing);
      Set_Text (Get_Entry (Editor.GUI.Spec_Extension),     Spec_Suffix);
      Set_Text (Get_Entry (Editor.GUI.Body_Extension),     Body_Suffix);
      Set_Text (Get_Entry (Editor.GUI.Separate_Extension), Separate_Suffix);

      Clear (Editor.GUI.Exception_List_Model);
      Id := Freeze_Sort (Editor.GUI.Exception_List_Model);

      if Display_Exceptions then
         declare
            Specs  : constant Associative_Array := Get_Attribute_Value
              (Project, Specification_Attribute);
            Bodies : constant Associative_Array := Get_Attribute_Value
              (Project, Implementation_Attribute);
            Iter : Gtk_Tree_Iter;
         begin
            for S in Specs'Range loop
               Append (Editor.GUI.Exception_List_Model, Iter, Null_Iter);
               Set_Unit_Spec
                 (Get_Object (Editor.GUI.Exception_List_Model), Iter,
                  Col   => 0,
                  Unit  => Get_String (Specs (S).Index) & ASCII.NUL,
                  Col2  => 1,
                  Spec  => To_String (Specs (S).Value) & ASCII.NUL);
            end loop;

            for B in Bodies'Range loop
               --  ??? We could store the name_id for the unit name, to avoid a
               --  string comparison, and save time. However, this won't be
               --  needed if C303-008 is implemented (merging the two exception
               --  lists)
               declare
                  Unit : constant String := Get_String (Bodies (B).Index);
               begin
                  Iter := Get_Iter_First (Editor.GUI.Exception_List_Model);
                  while Iter /= Null_Iter loop
                     exit when Get_String
                       (Editor.GUI.Exception_List_Model, Iter, 0) = Unit;
                     Next (Editor.GUI.Exception_List_Model, Iter);
                  end loop;
               end;

               if Iter = Null_Iter then
                  Append (Editor.GUI.Exception_List_Model, Iter, Null_Iter);
                  Set_Unit_Spec
                    (Get_Object (Editor.GUI.Exception_List_Model), Iter,
                     Col   => 0,
                     Unit  => Get_String (Bodies (B).Index) & ASCII.NUL,
                     Col2  => 2,
                     Spec  => To_String (Bodies (B).Value) & ASCII.NUL);
               else
                  Set (Editor.GUI.Exception_List_Model, Iter,
                       Column => 2,
                       Value  => To_String (Bodies (B).Value));
               end if;
            end loop;
         end;
      end if;

      Thaw_Sort (Editor.GUI.Exception_List_Model, Id);

      --  GNAT naming scheme ?
      if Get_Iter_First (Editor.GUI.Exception_List_Model) /= Null_Iter
        or else Get_Text (Get_Entry (Editor.GUI.Casing)) /=
          -Prj.Image (All_Lower_Case)
      then
         Select_Item
           (Get_List (Editor.GUI.Standard_Scheme), Custom_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) =
          Default_Gnat_Dot_Replacement
        and then Get_Text (Get_Entry (Editor.GUI.Spec_Extension)) =
        Default_Gnat_Spec_Suffix
        and then Get_Text (Get_Entry (Editor.GUI.Body_Extension)) =
        Default_Gnat_Body_Suffix
        and then Get_Text (Get_Entry (Editor.GUI.Separate_Extension)) =
        Default_Gnat_Separate_Suffix
      then
         Select_Item
           (Get_List (Editor.GUI.Standard_Scheme), Gnat_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) = "."
        and then Get_Text (Get_Entry (Editor.GUI.Spec_Extension)) = ".1.ada"
        and then Get_Text (Get_Entry (Editor.GUI.Body_Extension)) = ".2.ada"
        and then Get_Text (Get_Entry (Editor.GUI.Separate_Extension)) =
          ".2.ada"
      then
         Select_Item
           (Get_List (Editor.GUI.Standard_Scheme), Apex_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) = "__"
        and then Get_Text (Get_Entry (Editor.GUI.Spec_Extension)) = "_.ada"
        and then Get_Text (Get_Entry (Editor.GUI.Body_Extension)) = ".ada"
        and then Get_Text (Get_Entry (Editor.GUI.Separate_Extension)) = ".ada"
      then
         Select_Item
           (Get_List (Editor.GUI.Standard_Scheme), Dec_Naming_Scheme);

      else
         Select_Item
           (Get_List (Editor.GUI.Standard_Scheme), Custom_Naming_Scheme);
      end if;
   end Show_Project_Settings;

   -----------------------
   -- Add_New_Exception --
   -----------------------

   procedure Add_New_Exception
     (Editor : access Naming_Scheme_Editor_Record'Class)
   is
      Unit      : constant String := Get_Text (Editor.Unit_Name_Entry);
      Spec_Name : constant String := Get_Text (Editor.Spec_Filename_Entry);
      Body_Name : constant String := Get_Text (Editor.Body_Filename_Entry);
      Iter      : Gtk_Tree_Iter;
      Id        : Gint;

   begin
      if Unit /= Empty_Unit_Name
        and then (Spec_Name /= Empty_Spec_Name
                  or else Body_Name /= Empty_Body_Name)
      then
         --  Check if there is already an entry for this unit

         Iter := Get_Iter_First (Editor.Exception_List_Model);
         while Iter /= Null_Iter loop
            declare
               U : constant String :=
                 Get_String (Editor.Exception_List_Model, Iter, 0);
            begin
               if U = Unit then
                  exit;
               elsif  U > Unit then
                  Iter := Null_Iter;
                  exit;
               end if;
            end;
            Next (Editor.Exception_List_Model, Iter);
         end loop;

         Id := Freeze_Sort (Editor.Exception_List_Model);

         if Iter = Null_Iter then
            Append (Editor.Exception_List_Model, Iter, Null_Iter);
            Set (Editor.Exception_List_Model, Iter,
                 Column => 0,
                 Value  => Unit);
            Set (Editor.Exception_List_Model, Iter,
                 Column => 3,
                 Value  => True);
         end if;

         if Spec_Name /= Empty_Spec_Name then
            Set (Editor.Exception_List_Model, Iter,
                 Column => 1,
                 Value  => Spec_Name);
         end if;

         if Body_Name /= Empty_Body_Name then
            Set (Editor.Exception_List_Model, Iter,
                 Column => 2,
                 Value  => Body_Name);
         end if;

         Thaw_Sort (Editor.Exception_List_Model, Id);

         Scroll_To_Cell
           (Editor.Exception_List,
            Get_Path (Editor.Exception_List_Model, Iter),
            Column => Get_Column (Editor.Exception_List, 0),
            Use_Align => False,
            Row_Align => 0.0,
            Col_Align => 0.0);
         Reset_Exception_Fields (Editor);
      end if;
   end Add_New_Exception;

   ---------------------
   -- Clear_Unit_Name --
   ---------------------

   procedure Clear_Unit_Name
     (Editor : access Naming_Scheme_Editor_Record'Class) is
   begin
      if Get_Text (Editor.Unit_Name_Entry) = Empty_Unit_Name then
         Set_Text (Editor.Unit_Name_Entry, "");
      end if;
   end Clear_Unit_Name;

   ---------------------
   -- Clear_Spec_Name --
   ---------------------

   procedure Clear_Spec_Name
     (Editor : access Naming_Scheme_Editor_Record'Class) is
   begin
      if Get_Text (Editor.Spec_Filename_Entry) = Empty_Spec_Name then
         Set_Text (Editor.Spec_Filename_Entry, "");
      end if;
   end Clear_Spec_Name;

   ---------------------
   -- Clear_Body_Name --
   ---------------------

   procedure Clear_Body_Name
     (Editor : access Naming_Scheme_Editor_Record'Class) is
   begin
      if Get_Text (Editor.Body_Filename_Entry) = Empty_Body_Name then
         Set_Text (Editor.Body_Filename_Entry, "");
      end if;
   end Clear_Body_Name;

end Ada_Naming_Editors;

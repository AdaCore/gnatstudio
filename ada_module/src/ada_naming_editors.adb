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
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Box;                  use Gtk.Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Clist;                use Gtk.Clist;
with Gtk.List;                 use Gtk.List;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtkada.Types;             use Gtkada.Types;
with Casing;                   use Casing;
with Prj;
with Types;                    use Types;
with Snames;                   use Snames;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;
with GUI_Utils;                use GUI_Utils;
with Glide_Intl;               use Glide_Intl;
with Projects;                 use Projects;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Basic_Types;              use Basic_Types;
with Projects.Editor;          use Projects, Projects.Editor;

with Interfaces.C.Strings;     use Interfaces.C.Strings;

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

      Set_Auto_Sort (Editor.GUI.Exception_List, True);
      Reset_Exception_Fields (Editor.GUI);

      for Casing in Casing_Type loop
         if Casing /= Unknown then
            Gtk.Enums.String_List.Append (Casing_Items, -Prj.Image (Casing));
         end if;
      end loop;

      Set_Popdown_Strings (Editor.GUI.Casing, Casing_Items);
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
      Num_Rows : constant Gint := Get_Rows (Editor.GUI.Exception_List);
      Naming   : constant String := Get_String (Name_Naming);
      Changed  : Boolean := False;
      Ada_Scheme : constant Boolean :=
        Get_Index_In_List (Editor.GUI.Standard_Scheme) = 0;

      procedure Update_If_Required
        (Name : Name_Id; Value : String; Index : String);
      --  Update the attribute if necessary

      function List_Changed (List : Associative_Array; Column : Gint)
         return Boolean;
      --  True if the list of elements in List is different from the elements
      --  in column Column of the exceptions list.

      ------------------------
      -- Update_If_Required --
      ------------------------

      procedure Update_If_Required
        (Name : Name_Id; Value : String; Index : String)
      is
         Modified : Boolean := False;
      begin
         if Project = No_Project then
            Modified := True;

         else
            declare
               Old : constant String := Get_Attribute_Value
                 (Project        => Project,
                  Attribute_Name => Get_String (Name),
                  Package_Name   => Naming,
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
                  Pkg_Name           => Naming,
                  Scenario_Variables => Scenario_Variables,
                  Attribute_Name     => Get_String (Name),
                  Attribute_Index    => Index);
            else
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Naming,
                  Scenario_Variables => Scenario_Variables,
                  Attribute_Name     => Get_String (Name),
                  Value              => Value,
                  Attribute_Index    => Index);
            end if;
         end if;
      end Update_If_Required;

      ------------------
      -- List_Changed --
      ------------------

      function List_Changed (List : Associative_Array; Column : Gint)
         return Boolean
      is
         Length : constant Natural := List'Length;
         Old_Names  : Argument_List (1 .. Length);
         Old_Values : Argument_List (1 .. Length);
         Current : Natural := 1;
      begin
         for Elem in List'Range loop
            Old_Names (Current) := new String'(Get_String (List (Elem).Index));
            Old_Values (Current) := new String'(To_String (List (Elem).Value));
            Current := Current + 1;
         end loop;

         for J in 0 .. Num_Rows - 1 loop
            declare
               U : constant String :=
                 Get_Text (Editor.GUI.Exception_List, J, 0);
               Value : constant String :=
                 Get_Text (Editor.GUI.Exception_List, J, Column);
               Found : Boolean := False;
            begin
               if Value /= "" then
                  for Index in Old_Names'Range loop
                     if Old_Names (Index) /= null
                       and then Old_Names (Index).all = U
                     then
                        if Old_Values (Index).all /= Value then
                           Free (Old_Names);
                           Free (Old_Values);
                           return True;
                        end if;

                        Free (Old_Names (Index));
                        Free (Old_Values (Index));
                           Found := True;
                     end if;
                  end loop;

                  if not Found then
                     Free (Old_Names);
                     Free (Old_Values);
                     return True;
                  end if;
               end if;
            end;
         end loop;

         --  If there remains at least one value in the old values, then the
         --  lists are different
         for Index in Old_Names'Range loop
            if Old_Names (Index) /= null then
               Free (Old_Names);
               Free (Old_Values);
               return True;
            end if;
         end loop;

         return False;
      end List_Changed;

   begin
      Update_If_Required
        (Name_Specification_Suffix,
         Get_Text (Get_Entry (Editor.GUI.Spec_Extension)), Ada_String);
      Update_If_Required
        (Name_Implementation_Suffix,
         Get_Text (Get_Entry (Editor.GUI.Body_Extension)), Ada_String);
      Update_If_Required
        (Name_Separate_Suffix,
         Get_Text (Get_Entry (Editor.GUI.Separate_Extension)), "");
      Update_If_Required
        (Name_Casing,
         Prj.Image (Casing_Type'Val
                    (Get_Index_In_List (Editor.GUI.Casing))), "");
      Update_If_Required
        (Name_Dot_Replacement, Get_Text (Editor.GUI.Dot_Replacement), "");

      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenario_Variables,
         Attribute_Name     => Get_String (Name_Specification),
         Attribute_Index    => Any_Attribute);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenario_Variables,
         Attribute_Name     => Get_String (Name_Implementation),
         Attribute_Index    => Any_Attribute);

      Changed := Changed
        or else Project = No_Project
        or else List_Changed
           (Get_Attribute_Value
            (Project, Specification_Attribute, Naming_Package), 1)
        or else List_Changed
           (Get_Attribute_Value
            (Project, Implementation_Attribute, Naming_Package), 2);

      if Changed then
         for J in 0 .. Num_Rows - 1 loop
            declare
               U : constant String :=
                 Get_Text (Editor.GUI.Exception_List, J, 0);
               Spec : constant String :=
                 Get_Text (Editor.GUI.Exception_List, J, 1);
               Bod : constant String :=
                 Get_Text (Editor.GUI.Exception_List, J, 2);
            begin
               if Spec /= "" then
                  Update_Attribute_Value_In_Scenario
                    (Project            => Project,
                     Pkg_Name           => Naming,
                     Scenario_Variables => Scenario_Variables,
                     Attribute_Name     => Get_String (Name_Specification),
                     Value              => Spec,
                     Attribute_Index    => U);
               end if;
               if Bod /= "" then
                  Update_Attribute_Value_In_Scenario
                    (Project            => Project,
                     Pkg_Name           => Naming,
                     Scenario_Variables => Scenario_Variables,
                     Attribute_Name     => Get_String (Name_Implementation),
                     Value              => Bod,
                     Attribute_Index    => U);
               end if;
            end;
         end loop;
      end if;

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
      pragma Unreferenced (Kernel);
      Row   : Gint;
      Dot_Replacement : constant String := Get_Attribute_Value
        (Project, Dot_Replacement_Attribute, Naming_Package,
         Default => Default_Gnat_Dot_Replacement);
      Casing : constant String := Get_Attribute_Value
        (Project, Casing_Attribute, Naming_Package,
         Default => -Prj.Image (All_Lower_Case));
      Separate_Suffix : constant String := Get_Attribute_Value
        (Project, Separate_Suffix_Attribute, Naming_Package,
         Default => Default_Gnat_Separate_Suffix);
      Body_Suffix : constant String := Get_Attribute_Value
        (Project, Impl_Suffix_Attribute, Naming_Package,
         Index => Ada_String, Default => Default_Gnat_Body_Suffix);
      Spec_Suffix : constant String := Get_Attribute_Value
        (Project, Spec_Suffix_Attribute, Naming_Package,
         Index => Ada_String, Default => Default_Gnat_Spec_Suffix);
   begin
      Set_Text (Editor.GUI.Dot_Replacement,                Dot_Replacement);
      Set_Text (Get_Entry (Editor.GUI.Casing),            -Casing);
      Set_Text (Get_Entry (Editor.GUI.Spec_Extension),     Spec_Suffix);
      Set_Text (Get_Entry (Editor.GUI.Body_Extension),     Body_Suffix);
      Set_Text (Get_Entry (Editor.GUI.Separate_Extension), Separate_Suffix);

      Freeze (Editor.GUI.Exception_List);
      Clear (Editor.GUI.Exception_List);

      if Display_Exceptions then
         declare
            Specs  : constant Associative_Array := Get_Attribute_Value
              (Project, Specification_Attribute, Naming_Package);
            Bodies : constant Associative_Array := Get_Attribute_Value
              (Project, Implementation_Attribute, Naming_Package);
         begin
            for S in Specs'Range loop
               Row := Prepend
                 (Editor.GUI.Exception_List,
                  Get_String (Specs (S).Index)
                  + To_String (Specs (S).Value) + "");
            end loop;

            for B in Bodies'Range loop
               Row := Find_First_Row_Matching
                 (Editor.GUI.Exception_List, 0, Get_String (Bodies (B).Index));

               if Row = -1 then
                  Row := Prepend
                    (Editor.GUI.Exception_List,
                     Get_String (Bodies (B).Index)
                     + "" + To_String (Bodies (B).Value));
               else
                  Set_Text (Editor.GUI.Exception_List, Row, 2,
                            To_String (Bodies (B).Value));
               end if;
            end loop;

            Sort (Editor.GUI.Exception_List);
         end;
      end if;

      Thaw (Editor.GUI.Exception_List);

      --  GNAT naming scheme ?
      if Get_Rows (Editor.GUI.Exception_List) /= 0
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
      Text      : Gtkada.Types.Chars_Ptr_Array (0 .. 2);
      Num_Rows  : constant Gint := Get_Rows (Editor.Exception_List);
      Row       : Gint := -1;

   begin
      if Unit /= Empty_Unit_Name then

         --  Check if there is already an entry for this unit

         for J in 0 .. Num_Rows - 1 loop
            declare
               U : constant String :=
                 Get_Text (Editor.Exception_List, J, 0);
            begin
               if U = Unit then
                  Row := J;
               elsif  U > Unit then
                  exit;
               end if;
            end;
         end loop;

         if Spec_Name /= Empty_Spec_Name
           or else Body_Name /= Empty_Body_Name
         then
            if Row = -1 then
               Text (0) := New_String (Unit);
               Text (1) := New_String ("");
               Text (2) := New_String ("");
               Row := Append (Editor.Exception_List, Text);
               Free (Text);
            end if;

            if Spec_Name /= Empty_Spec_Name then
               Set_Text (Editor.Exception_List, Row, 1, Spec_Name);
            end if;

            if Body_Name /= Empty_Body_Name then
               Set_Text (Editor.Exception_List, Row, 2, Body_Name);
            end if;

            Moveto (Editor.Exception_List, Row, 0, 0.0, 0.0);
            Reset_Exception_Fields (Editor);
         end if;

         Grab_Focus (Editor.Unit_Name_Entry);
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

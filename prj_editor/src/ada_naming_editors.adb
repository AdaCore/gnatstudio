-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2002                         --
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
with Prj.Tree;                 use Prj.Tree;
with Prj.Util;                 use Prj.Util;
with Prj_API;                  use Prj_API;
with Prj;                      use Prj;
with Types;                    use Types;
with Namet;                    use Namet;
with Snames;                   use Snames;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;
with GUI_Utils;                use GUI_Utils;
with Glide_Intl;               use Glide_Intl;
with Prj_API;                  use Prj_API;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Project;     use Glide_Kernel.Project;

with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Ada_Naming_Editors is

   Empty_Unit_Name : constant String := "<unit_name>";
   Empty_Spec_Name : constant String := "<spec_file_name>";
   Empty_Body_Name : constant String := "<body_file_name>";

   Default_Gnat_Dot_Replacement : constant String := "-";
   Default_Gnat_Spec_Suffix     : constant String := ".ads";
   Default_Gnat_Body_Suffix     : constant String := ".adb";
   Default_Gnat_Separate_Suffix : constant String := ".adb";
   --  Default settings for the GNAT naming scheme.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Ada_Naming_Editor) is
      Casing_Items : String_List.Glist;
      Size_Group : Gtk_Size_Group;
   begin
      Editor := new Ada_Naming_Editor_Record;
      Initialize (Editor);

      Set_Size_Request (Editor.Exception_List, -1, 170);
      Gtk_New (Size_Group, Both);
      Add_Widget (Size_Group, Editor.Standard_Scheme);
      Add_Widget (Size_Group, Editor.Casing);
      Add_Widget (Size_Group, Editor.Dot_Replacement);
      Add_Widget (Size_Group, Editor.Spec_Extension);
      Add_Widget (Size_Group, Editor.Body_Extension);
      Add_Widget (Size_Group, Editor.Separate_Extension);

      Gtk_New (Size_Group, Both);
      Add_Widget (Size_Group, Editor.Label_Naming_Scheme);
      Add_Widget (Size_Group, Editor.Label_Casing);
      Add_Widget (Size_Group, Editor.Label_Dot_Replacement);
      Add_Widget (Size_Group, Editor.Label_Spec_Extensions);
      Add_Widget (Size_Group, Editor.Label_Body_Extensions);
      Add_Widget (Size_Group, Editor.Label_Separate_Extensions);

      Ref (Editor.Main_Box);
      Unparent (Editor.Main_Box);
      --  ??? When is the memory occupied by Editor itself ever freed ?

      Set_Auto_Sort (Editor.Exception_List, True);
      Reset_Exception_Fields (Editor);

      for Casing in Casing_Type loop
         if Casing /= Unknown then
            String_List.Append (Casing_Items, -Image (Casing));
         end if;
      end loop;

      Set_Popdown_Strings (Editor.Casing, Casing_Items);
      Free_String_List (Casing_Items);

      --  Set the GNAT naming scheme
      Select_Item (Get_List (Editor.Standard_Scheme), 0);
   end Gtk_New;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Ada_Naming_Editor_Record) return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.Main_Box);
   end Get_Window;

   ----------------------------
   -- Reset_Exception_Fields --
   ----------------------------

   procedure Reset_Exception_Fields
     (Editor : access Ada_Naming_Editor_Record'Class;
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
     (Editor     : access Ada_Naming_Editor_Record;
      Scheme_Num : Natural) is
   begin
      case Scheme_Num is
         when 0 =>
            --  GNAT Default
            Set_Text (Get_Entry (Editor.Casing), -Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, Default_Gnat_Dot_Replacement);
            Set_Text (Get_Entry (Editor.Spec_Extension),
                      Default_Gnat_Spec_Suffix);
            Set_Text (Get_Entry (Editor.Body_Extension),
                      Default_Gnat_Body_Suffix);
            Set_Text (Get_Entry (Editor.Separate_Extension),
                      Default_Gnat_Separate_Suffix);

         when 1 =>
            --  APEX Default
            Set_Text (Get_Entry (Editor.Casing), -Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, ".");
            Set_Text (Get_Entry (Editor.Spec_Extension), ".1.ada");
            Set_Text (Get_Entry (Editor.Body_Extension), ".2.ada");
            Set_Text (Get_Entry (Editor.Separate_Extension), ".2.ada");

         when 2 =>
            --  DEC Ada Default
            Set_Text (Get_Entry (Editor.Casing), -Image (All_Lower_Case));
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
      Kernel  : access Kernel_Handle_Record'Class;
      Project : Prj.Tree.Project_Node_Id) return Boolean
   is
      Num_Rows : constant Gint := Get_Rows (Editor.Exception_List);
      Naming   : constant String := Get_Name_String (Name_Naming);
      Scenar   : constant Project_Node_Array := Scenario_Variables (Kernel);
   begin
      --  Eliminate all the Ada-related attributes.
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Specification_Suffix),
         Attribute_Index    => Ada_String);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Implementation_Suffix),
         Attribute_Index    => Ada_String);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Separate_Suffix),
         Attribute_Index    => Ada_String);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Casing),
         Attribute_Index    => Ada_String);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Dot_Replacement),
         Attribute_Index    => Ada_String);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Specification),
         Attribute_Index    => Any_Attribute);
      Delete_Attribute
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Implementation),
         Attribute_Index    => Any_Attribute);


      --  Do nothing for the standard GNAT naming scheme

      if Get_Index_In_List (Editor.Standard_Scheme) /= 0 then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => Naming,
            Scenario_Variables => Scenar,
            Attribute_Name     => Get_Name_String (Name_Specification_Suffix),
            Value              => Get_Text (Get_Entry (Editor.Spec_Extension)),
            Attribute_Index    => Ada_String);
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => Naming,
            Scenario_Variables => Scenar,
            Attribute_Name     => Get_Name_String (Name_Implementation_Suffix),
            Value              => Get_Text (Get_Entry (Editor.Body_Extension)),
            Attribute_Index    => Ada_String);
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => Naming,
            Scenario_Variables => Scenar,
            Attribute_Name     => Get_Name_String (Name_Separate_Suffix),
            Value         => Get_Text (Get_Entry (Editor.Separate_Extension)));
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => Naming,
            Scenario_Variables => Scenar,
            Attribute_Name     => Get_Name_String (Name_Casing),
            Value              => Image
              (Casing_Type'Val (Get_Index_In_List (Editor.Casing))));
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Pkg_Name           => Naming,
            Scenario_Variables => Scenar,
            Attribute_Name     => Get_Name_String (Name_Dot_Replacement),
            Value              => Get_Text (Editor.Dot_Replacement));
      end if;

      for J in 0 .. Num_Rows - 1 loop
         declare
            U : constant String := Get_Text (Editor.Exception_List, J, 0);
            Spec : constant String :=
              Get_Text (Editor.Exception_List, J, 1);
            Bod : constant String :=
              Get_Text (Editor.Exception_List, J, 2);
         begin
            if Spec /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Naming,
                  Scenario_Variables => Scenar,
                  Attribute_Name     => Get_Name_String (Name_Specification),
                  Value              => Spec,
                  Attribute_Index    => U);
            end if;
            if Bod /= "" then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Naming,
                  Scenario_Variables => Scenar,
                  Attribute_Name     => Get_Name_String (Name_Implementation),
                  Value              => Bod,
                  Attribute_Index    => U);
            end if;
         end;
      end loop;

      --  Should return True only of the naming scheme actually changed.
      return True;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record;
      Project_View       : Prj.Project_Id;
      Display_Exceptions : Boolean := True)
   is
      Data  : constant Naming_Data := Prj.Projects.Table (Project_View).Naming;
      Value : Variable_Value;
      Elem  : Array_Element_Id;
      Row   : Gint;
   begin
      if Data.Dot_Replacement /= No_Name then
         Set_Text (Editor.Dot_Replacement,
                   Get_Name_String (Data.Dot_Replacement));
      else
         Set_Text (Editor.Dot_Replacement, Default_Gnat_Dot_Replacement);
      end if;

      Set_Text (Get_Entry (Editor.Casing), -Image (Data.Casing));

      Value := Value_Of
        (Index => Name_Ada, In_Array => Data.Specification_Suffix);
      if Value.Kind = Single and then Value.Value /= No_String then
         Set_Text
           (Get_Entry (Editor.Spec_Extension), Get_String (Value.Value));
      else
         Set_Text
           (Get_Entry (Editor.Spec_Extension),  Default_Gnat_Spec_Suffix);
      end if;

      Value := Value_Of
        (Index    => Name_Ada, In_Array => Data.Implementation_Suffix);
      if Value.Kind = Single and then Value.Value /= No_String then
         Set_Text
           (Get_Entry (Editor.Body_Extension), Get_String (Value.Value));
      else
         Set_Text
           (Get_Entry (Editor.Body_Extension),  Default_Gnat_Body_Suffix);
      end if;

      if Data.Separate_Suffix /= No_Name then
         Set_Text
           (Get_Entry (Editor.Separate_Extension),
            Get_Name_String (Data.Separate_Suffix));
      else
         Set_Text
           (Get_Entry (Editor.Separate_Extension),
            Default_Gnat_Separate_Suffix);
      end if;

      Freeze (Editor.Exception_List);
      Clear (Editor.Exception_List);

      if Display_Exceptions then
         Elem := Data.Specifications;
         while Elem /= No_Array_Element loop
            Value := Prj.Array_Elements.Table (Elem).Value;
            Row := Prepend
              (Editor.Exception_List,
               Get_Name_String (Prj.Array_Elements.Table (Elem).Index)
                 + Get_String (Value.Value) + "");
            Elem := Prj.Array_Elements.Table (Elem).Next;
         end loop;

         Elem := Data.Bodies;
         while Elem /= No_Array_Element loop
            Value := Prj.Array_Elements.Table (Elem).Value;
            Row := Find_First_Row_Matching
              (Editor.Exception_List,
               0,
               Get_Name_String (Prj.Array_Elements.Table (Elem).Index));
            if Row = -1 then
               Row := Prepend
                 (Editor.Exception_List,
                  Get_Name_String (Prj.Array_Elements.Table (Elem).Index)
                    + "" + Get_String (Value.Value));
            else
               Set_Text (Editor.Exception_List, Row, 2,
                         Get_String (Value.Value));
            end if;
            Elem := Prj.Array_Elements.Table (Elem).Next;
         end loop;

         Sort (Editor.Exception_List);
      end if;

      Thaw (Editor.Exception_List);
   end Show_Project_Settings;

   -----------------------
   -- Add_New_Exception --
   -----------------------

   procedure Add_New_Exception (Editor : access Ada_Naming_Editor_Record) is

      Unit : constant String := Get_Text (Editor.Unit_Name_Entry);
      Spec_Name : constant String := Get_Text (Editor.Spec_Filename_Entry);
      Body_Name : constant String := Get_Text (Editor.Body_Filename_Entry);
      Text : Gtkada.Types.Chars_Ptr_Array (0 .. 2);
      Num_Rows : constant Gint := Get_Rows (Editor.Exception_List);
      Row : Gint := -1;
   begin
      if Unit /= Empty_Unit_Name then

         --  Check if there is already an entry for this unit

         for J in 0 .. Num_Rows - 1 loop
            declare
               U : constant String := Get_Text (Editor.Exception_List, J, 0);
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

   procedure Clear_Unit_Name (Editor : access Ada_Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Unit_Name_Entry) = Empty_Unit_Name then
         Set_Text (Editor.Unit_Name_Entry, "");
      end if;
   end Clear_Unit_Name;

   ---------------------
   -- Clear_Spec_Name --
   ---------------------

   procedure Clear_Spec_Name (Editor : access Ada_Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Spec_Filename_Entry) = Empty_Spec_Name then
         Set_Text (Editor.Spec_Filename_Entry, "");
      end if;
   end Clear_Spec_Name;

   ---------------------
   -- Clear_Body_Name --
   ---------------------

   procedure Clear_Body_Name (Editor : access Ada_Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Body_Filename_Entry) = Empty_Body_Name then
         Set_Text (Editor.Body_Filename_Entry, "");
      end if;
   end Clear_Body_Name;

end Ada_Naming_Editors;

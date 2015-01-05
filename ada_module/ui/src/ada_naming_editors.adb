------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with System;                   use System;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;

with Gtk.Box;                  use Gtk.Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;

with Casing;                   use Casing;
with GPS.Intl;                 use GPS.Intl;
with GUI_Utils;                use GUI_Utils;
with Prj;

with String_Hash;

package body Ada_Naming_Editors is

   Empty_Unit_Name : constant String := "<unit_name>";
   Empty_Spec_Name : constant String := "<spec_file> [<at index>]";
   Empty_Body_Name : constant String := "<body_file> [<at index>]";

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Ada_Naming_Editor) is
      Size_Group   : Gtk_Size_Group;
      Idx          : Gint := 0;
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

      Gtk_List_Store'(-(Editor.GUI.Casing.Get_Model)).Clear;

      for Casing in Casing_Type loop
         if Casing /= Unknown then
            Editor.GUI.Casing.Append_Text (-Prj.Image (Casing));

            if Casing = All_Lower_Case then
               Editor.GUI.Casing.Set_Active (Idx);
            end if;

            Idx := Idx + 1;
         end if;
      end loop;

      Set_Active (Editor.GUI.Standard_Scheme, Gnat_Naming_Scheme);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Editor : access Ada_Naming_Editor_Record) is
   begin
      Destroy (Editor.GUI);
      --  Editor.GUI.Main_Box is automatically destroyed when its parent
      --  contain (multi-language naming editor) is destroyed.
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   overriding function Get_Window
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
            Set_Active_Text (Editor.Casing, -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, Default_Gnat_Dot_Replacement);
            Set_Active_Text (Editor.Spec_Extension, Default_Gnat_Spec_Suffix);
            Set_Active_Text (Editor.Body_Extension, Default_Gnat_Body_Suffix);
            Set_Active_Text (Editor.Separate_Extension,
                             Default_Gnat_Separate_Suffix);

         when Apex_Naming_Scheme =>
            --  APEX Default
            Set_Active_Text (Editor.Casing, -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, ".");
            Set_Active_Text (Editor.Spec_Extension, ".1.ada");
            Set_Active_Text (Editor.Body_Extension, ".2.ada");
            Set_Active_Text (Editor.Separate_Extension, ".2.ada");

         when Dec_Naming_Scheme =>
            --  DEC Ada Default
            Set_Active_Text (Editor.Casing, -Prj.Image (All_Lower_Case));
            Set_Text (Editor.Dot_Replacement, "__");
            Set_Active_Text (Editor.Spec_Extension, "_.ada");
            Set_Active_Text (Editor.Body_Extension, ".ada");
            Set_Active_Text (Editor.Separate_Extension, ".ada");

         when others =>
            null;
      end case;
   end Set_Predefined_Scheme;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   overriding function Create_Project_Entry
     (Editor             : access Ada_Naming_Editor_Record;
      Project            : Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages);
      Changed    : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      Ada_Scheme : constant Boolean :=
                     Get_Active (Editor.GUI.Standard_Scheme) = 0;
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
         if Project = No_Project then
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
         Val         : String_Access;
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
         Get_Active_Text (Editor.GUI.Spec_Extension), "ada");
      Update_If_Required
        (Impl_Suffix_Attribute,
         Get_Active_Text (Editor.GUI.Body_Extension), "ada");
      Update_If_Required
        (Separate_Suffix_Attribute,
         Get_Active_Text (Editor.GUI.Separate_Extension), "");
      Update_If_Required
        (Casing_Attribute,
         Prj.Image (Casing_Type'Val
                    (Get_Active (Editor.GUI.Casing))), "");
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

            Set (Cache, U, new Naming_Data'(Data));
         end;

         Next (Editor.GUI.Exception_List_Model, Iter);
      end loop;

      --  Update the project if needed

      Changed := Changed or else Project = No_Project;

      --  Computing whether a list has changed is extremely fast now that the
      --  hash table has been created. Much faster than updating the attributes
      --  for nothing.

      Update_Exceptions (Spec_Attribute);
      Update_Exceptions (Body_Attribute);

      Reset (Cache);

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   overriding procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record;
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
                             Default => -Prj.Image (All_Lower_Case));
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
      Set_Text (Editor.GUI.Dot_Replacement,           Dot_Replacement);
      Set_Active_Text (Editor.GUI.Casing,            -Casing);
      Set_Active_Text (Editor.GUI.Spec_Extension,     Spec_Suffix);
      Set_Active_Text (Editor.GUI.Body_Extension,     Body_Suffix);
      Set_Active_Text (Editor.GUI.Separate_Extension, Separate_Suffix);

      Clear (Editor.GUI.Exception_List_Model);
      Id := Freeze_Sort (Editor.GUI.Exception_List_Model);

      if Display_Exceptions then
         declare
            Specs  : GNAT.Strings.String_List :=
              Project.Attribute_Indexes (Spec_Attribute);
            Bodies  : GNAT.Strings.String_List :=
              Project.Attribute_Indexes (Body_Attribute);

            Iter   : Gtk_Tree_Iter;
         begin
            for S in Specs'Range loop
               Append (Editor.GUI.Exception_List_Model, Iter, Null_Iter);
               Set_Unit_Spec
                 (Get_Object (Editor.GUI.Exception_List_Model), Iter,
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
                     Unit  => Bodies (B).all & ASCII.NUL,
                     Col2  => 2,
                     Spec  => Project.Attribute_Value
                       (Body_Attribute, Index => Bodies (B).all)
                       & ASCII.NUL);

               else
                  Set (Editor.GUI.Exception_List_Model, Iter,
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

      Thaw_Sort (Editor.GUI.Exception_List_Model, Id);

      --  GNAT naming scheme ?
      if Get_Iter_First (Editor.GUI.Exception_List_Model) /= Null_Iter
        or else Get_Active_Text (Editor.GUI.Casing) /=
          -Prj.Image (All_Lower_Case)
      then
         Set_Active (Editor.GUI.Standard_Scheme, Custom_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) =
          Default_Gnat_Dot_Replacement
        and then Get_Active_Text (Editor.GUI.Spec_Extension) =
        Default_Gnat_Spec_Suffix
        and then Get_Active_Text (Editor.GUI.Body_Extension) =
        Default_Gnat_Body_Suffix
        and then Get_Active_Text (Editor.GUI.Separate_Extension) =
        Default_Gnat_Separate_Suffix
      then
         Set_Active (Editor.GUI.Standard_Scheme, Gnat_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) = "."
        and then Get_Active_Text (Editor.GUI.Spec_Extension) = ".1.ada"
        and then Get_Active_Text (Editor.GUI.Body_Extension) = ".2.ada"
        and then Get_Active_Text (Editor.GUI.Separate_Extension) = ".2.ada"
      then
         Set_Active (Editor.GUI.Standard_Scheme, Apex_Naming_Scheme);

      elsif Get_Text (Editor.GUI.Dot_Replacement) = "__"
        and then Get_Active_Text (Editor.GUI.Spec_Extension) = "_.ada"
        and then Get_Active_Text (Editor.GUI.Body_Extension) = ".ada"
        and then Get_Active_Text (Editor.GUI.Separate_Extension) = ".ada"
      then
         Set_Active (Editor.GUI.Standard_Scheme, Dec_Naming_Scheme);

      else
         Set_Active (Editor.GUI.Standard_Scheme, Custom_Naming_Scheme);
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

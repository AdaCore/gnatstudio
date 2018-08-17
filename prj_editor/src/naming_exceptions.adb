------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with GNAT.OS_Lib;              use GNAT.OS_Lib;

with Glib;                     use Glib;
with Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;
with Gtk.Button;               use Gtk.Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;

with Basic_Types;
with GUI_Utils;                use GUI_Utils;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

package body Naming_Exceptions is

   Empty_Filename : constant String := "<filename>";
   --  Used to initialize a new tree view row

   Is_Spec_Column  : constant := 0;
   --  Column used to know if a the row's filename designates a specification
   --  file.

   Filename_Column : constant := 1;
   --  Column where the file name of the naming exception is stored

   Editable_Column : constant := 2;
   --  Column indicating if the row is editable or not

   Is_Spec_Column_Name  : aliased String := "Specification File";
   Filename_Column_Name : aliased String := "Filename";
   --  Column titles

   Column_Types   : constant GType_Array :=
     (Is_Spec_Column    => GType_Boolean,
      Filename_Column   => GType_String,
      Editable_Column   => GType_Boolean);
   --  Column types of the exceptions list tree view

   procedure Add_Naming_Exception
     (Self           : not null access Exceptions_Editor_Record'Class;
      Filename       : String;
      Is_Spec        : Boolean;
      Editable       : Boolean := True;
      Scroll_To_Cell : Boolean := False);
   --  Add a new row in the execptions list tree view

   procedure On_Add_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when clicking on the '+' button of the exception list view

   procedure On_Remove_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when clicking on the '-' button of the exception list view

   --------------------------
   -- Add_Naming_Exception --
   --------------------------

   procedure Add_Naming_Exception
     (Self           : not null access Exceptions_Editor_Record'Class;
      Filename       : String;
      Is_Spec        : Boolean;
      Editable       : Boolean := True;
      Scroll_To_Cell : Boolean := False)
   is
      New_Iter : Gtk_Tree_Iter;
   begin
      Self.Exceptions_List_Model.Append (New_Iter, Parent => Null_Iter);

      Set_And_Clear
        (Self.Exceptions_List_Model,
         Iter   => New_Iter,
         Values =>
           (Filename_Column    => As_String (Filename),
            Is_Spec_Column     => As_Boolean (Is_Spec),
            Editable_Column    => As_Boolean (Editable)));

      if Scroll_To_Cell then
         declare
            Path : constant Gtk_Tree_Path :=
              Get_Path (Self.Exceptions_List_Model, New_Iter);

         begin
            Self.Exceptions_List_Tree.Scroll_To_Cell
              (Path      => Path,
               Column    =>
                 Get_Column (Self.Exceptions_List_Tree, Filename_Column),
               Use_Align => False,
               Row_Align => 0.0,
               Col_Align => 0.0);
            Self.Exceptions_List_Tree.Set_Cursor_On_Cell
              (Path          => Path,
               Focus_Column  => Get_Column
                 (Self.Exceptions_List_Tree, Filename_Column),
               Focus_Cell    => null,
               Start_Editing => True);
            Path_Free (Path);
         end;
      end if;
   end Add_Naming_Exception;

   -----------------------------
   -- On_Add_Naming_Exception --
   -----------------------------

   procedure On_Add_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor   : constant Exceptions_Editor := Exceptions_Editor (Self);
   begin
      Editor.Add_Naming_Exception
        (Filename       => Empty_Filename,
         Is_Spec        => False,
         Scroll_To_Cell => True);
   end On_Add_Naming_Exception;

   --------------------------------
   -- On_Remove_Naming_Exception --
   --------------------------------

   procedure On_Remove_Naming_Exception
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Editor : constant Exceptions_Editor := Exceptions_Editor (Self);
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (Editor.Exceptions_List_Tree), Model, Iter);

      if Iter /= Null_Iter then
         Editor.Exceptions_List_Model.Remove (Iter);
      end if;
   end On_Remove_Naming_Exception;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor   : out Exceptions_Editor;
      Language : String)
   is
      Button     : Gtk_Button;
      Col_Number : Gint with Unreferenced;
   begin
      Editor := new Exceptions_Editor_Record;
      Dialog_Utils.Initialize
        (Editor,
         Position => Pos_Left);
      Editor.Language := To_Unbounded_String (To_Lower (Language));

      --  Create the exceptions list tree view

      Editor.Exceptions_List_Tree := Create_Tree_View
        (Column_Types       => Column_Types,
         Column_Names       =>
           (Is_Spec_Column + 1  => Is_Spec_Column_Name'Unchecked_Access,
            Filename_Column + 1 => Filename_Column_Name'Unchecked_Access),
         Show_Column_Titles => True,
         Editable_Columns   => (Is_Spec_Column  => Editable_Column,
                                Filename_Column => Editable_Column),
         Selection_Mode     => Selection_Single);
      Editor.Exceptions_List_Model := -Get_Model (Editor.Exceptions_List_Tree);

      Editor.Append
        (Widget        => Editor.Exceptions_List_Tree,
         Expand        => True,
         Fill          => True);

      --  Create the 'add' and 'remove' buttons

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-add-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked
        (Call  => On_Add_Naming_Exception'Access,
         Slot  => Editor);
      Editor.Append_Button (Button);

      Gtk_New_From_Icon_Name
        (Button,
         Icon_Name => "gps-remove-symbolic",
         Size      => Icon_Size_Small_Toolbar);
      Button.Set_Relief (Relief_None);
      Button.On_Clicked
        (Call  => On_Remove_Naming_Exception'Access,
         Slot  => Editor);
      Editor.Append_Button (Button);
   end Gtk_New;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor             : access Exceptions_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array)
      return Boolean
   is
      Model     : Gtk_Tree_Store renames Editor.Exceptions_List_Model;
      Language  : constant String := To_String (Editor.Language);

      function Get_Number_Of_Spec_Exceptions return Integer;
      --  Return the number of naming exceptions that are for specification
      --  files in the exceptions list tree view model

      procedure Fetch_Exceptions_From_Model
        (Impls : out Argument_List;
         Specs : out Argument_List);
      --  Fetch the implementation and specification naming exceptions from
      --  the model.

      function Change_Exception_Attr_If_Needed
        (Attr       : Attribute_Pkg_List;
         Exceptions : Argument_List) return Boolean;
      --  Change the given attribute if needed, giving Exceptions as a new
      --  value for it.
      --  Return True if the attribute has been changed.

      -----------------------------------
      -- Get_Number_Of_Spec_Exceptions --
      -----------------------------------

      function Get_Number_Of_Spec_Exceptions return Integer is
         Result : Integer := 0;
         Iter   : Gtk_Tree_Iter := Get_Iter_First
           (Editor.Exceptions_List_Model);
      begin
         while Iter /= Null_Iter loop
            if Get_Boolean (Model, Iter, Is_Spec_Column) then
               Result := Result + 1;
            end if;
            Next (Model, Iter);
         end loop;

         return Result;
      end Get_Number_Of_Spec_Exceptions;

      ---------------------------------
      -- Fetch_Exceptions_From_Model --
      ---------------------------------

      procedure Fetch_Exceptions_From_Model
        (Impls : out Argument_List;
         Specs : out Argument_List)
      is
         Impl_Idx : Integer := Impls'First;
         Spec_Idx : Integer := Specs'First;
         Iter     : Gtk_Tree_Iter := Get_Iter_First
           (Editor.Exceptions_List_Model);
      begin
         while Iter /= Null_Iter loop
            if Get_Boolean (Model, Iter, Is_Spec_Column) then
               Specs (Spec_Idx) := new String'
                 (Get_String (Model, Iter, Filename_Column));
               Spec_Idx := Spec_Idx + 1;
            else
               Impls (Impl_Idx) := new String'
                 (Get_String (Model, Iter, Filename_Column));
               Impl_Idx := Impl_Idx + 1;
            end if;

            Next (Model, Iter);
         end loop;
      end Fetch_Exceptions_From_Model;

      -------------------------------------
      -- Change_Exception_Attr_If_Needed --
      -------------------------------------

      function Change_Exception_Attr_If_Needed
        (Attr       : Attribute_Pkg_List;
         Exceptions : Argument_List) return Boolean
      is
         Need_Change    : Boolean := False;
         Old_Exceptions : String_List_Access := Project.Attribute_Value
           (Attribute => Attr,
            Index     => Language);
      begin
         --  Compare the exceptions list with the one currently set in the
         --  project to know if we should change the project.

         if Old_Exceptions = null then
            Need_Change := Exceptions'Length > 0;
         else
            Need_Change := not Basic_Types.Is_Equal
              (Exceptions, Old_Exceptions.all);
         end if;

         Free (Old_Exceptions);

         if Need_Change then
            if Exceptions'Length > 0 then
               Project.Set_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Attr,
                  Values    => Exceptions,
                  Index     => Language);
            else
               Project.Delete_Attribute
                 (Scenario  => Scenario_Variables,
                  Attribute => Attr,
                  Index     => Language);
            end if;
         end if;

         return Need_Change;
      end Change_Exception_Attr_If_Needed;

      Nb_Rows   : constant Integer :=
                    Integer (N_Children (Editor.Exceptions_List_Model));
      Nb_Specs  : constant Integer := Get_Number_Of_Spec_Exceptions;
      Nb_Impls  : constant Integer := Nb_Rows - Nb_Specs;
      Specs     : Argument_List (1 .. Nb_Specs);
      Impls     : Argument_List (1 .. Nb_Impls);
      Changed   : Boolean := False;
   begin
      --  Get the exceptions from the exceptions list tree view model

      Fetch_Exceptions_From_Model
        (Impls => Impls,
         Specs => Specs);

      if Project = No_Project then
         Changed := True;
      else
         --  Change the corresponding project attributes, if needed

         Changed := Change_Exception_Attr_If_Needed
           (Attr       => Impl_Exception_Attribute,
            Exceptions => Impls);
         Changed := Changed or Change_Exception_Attr_If_Needed
           (Attr       => Spec_Exception_Attribute,
            Exceptions => Specs);
      end if;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Exceptions_Editor_Record;
      Project            : Project_Type)
   is
      procedure Add_Exceptions_From_Project_Attribute
        (Attr : Attribute_Pkg_List);
      --  Add the filename exceptions stored for the given attribute

      -------------------------------------------
      -- Add_Exceptions_From_Project_Attribute --
      -------------------------------------------

      procedure Add_Exceptions_From_Project_Attribute
        (Attr : Attribute_Pkg_List)
      is
         Filenames : String_List_Access := Project.Attribute_Value
              (Attribute => Attr,
               Index     => To_String (Editor.Language));
         Is_Spec   : constant Boolean := Attr = Spec_Exception_Attribute;
      begin
         if Filenames /= null then
            for Filename of Filenames.all loop
               Editor.Add_Naming_Exception
                 (Filename       => Filename.all,
                  Is_Spec        => Is_Spec);
            end loop;

            Free (Filenames);
         end if;
      end Add_Exceptions_From_Project_Attribute;

   begin
      Clear (Editor.Exceptions_List_Model);

      if Project /= No_Project then
         Add_Exceptions_From_Project_Attribute (Impl_Exception_Attribute);
         Add_Exceptions_From_Project_Attribute (Spec_Exception_Attribute);
      end if;
   end Show_Project_Settings;

end Naming_Exceptions;

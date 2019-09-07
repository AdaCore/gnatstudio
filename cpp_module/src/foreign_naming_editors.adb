------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with GNAT.OS_Lib;        use GNAT.OS_Lib;

with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.Widget;         use Gtk.Widget;

with Dialog_Utils;       use Dialog_Utils;
with GPS.Kernel.Project; use GPS.Kernel.Project;

with Naming_Exceptions;  use Naming_Exceptions;

package body Foreign_Naming_Editors is

   ---------------------------
   -- Naming_Editor_Factory --
   ---------------------------

   function Naming_Editor_Factory
     (Kernel : not null access Kernel_Handle_Record'Class;
      Lang   : String) return not null Project_Editor_Page
   is
      pragma Unreferenced (Kernel);
      Result : Foreign_Naming_Editor;
   begin
      Result          := new Foreign_Naming_Editor_Record;
      Result.Language := new String'(Lang);
      return Project_Editor_Page (Result);
   end Naming_Editor_Factory;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self      : not null access Foreign_Naming_Editor_Record;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Read_Only : Boolean;
      Project   : Project_Type := No_Project)
   is
      Display_Exceptions : constant Boolean := True;
      P                  : Project_Type     := Project;
      Group_Widget       : Dialog_Group_Widget;
   begin
      Dialog_Utils.Initialize (Self);

      --  Create the 'Details' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize (Group_Widget, Parent_View => Self, Group_Name => "Details");

      --  Create the header files entry widget
      Gtk_New_With_Entry (Self.Spec_Ext);
      Self.Spec_Ext.Set_Sensitive (not Read_Only);
      Self.Spec_Ext.Set_Entry_Text_Column (0);
      Self.Spec_Ext.Append_Text (".h");
      Self.Spec_Ext.Append_Text (".hh");
      Self.Spec_Ext.Append_Text (".H");
      Self.Spec_Ext.Append_Text (".hpp");

      Group_Widget.Create_Child
        (Widget => Self.Spec_Ext,
         Label  => "Header files",
         Doc    => "Choose a suffix for header file names.",
         Expand => False);

      --  Create the implementation files entry widget
      Gtk_New_With_Entry (Self.Body_Ext);
      Self.Body_Ext.Set_Sensitive (not Read_Only);
      Self.Body_Ext.Set_Entry_Text_Column (0);
      Self.Body_Ext.Append_Text (".c");
      Self.Body_Ext.Append_Text (".cc");
      Self.Body_Ext.Append_Text (".C");
      Self.Body_Ext.Append_Text (".cpp");

      Group_Widget.Create_Child
        (Widget => Self.Body_Ext,
         Label  => "Implementation",
         Doc    => "Choose a suffix for implementation file names.");

      --  Create the 'Exceptions' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Self,
         Group_Name          => "Exceptions",
         Allow_Multi_Columns => False);

      Gtk_New (Self.Exceptions, Self.Language.all);
      Group_Widget.Append_Child
        (Widget => Self.Exceptions,
         Expand => True,
         Fill   => True);

      --  Now show the project's settings

      --  If the project is null, we get the default values from the current
      --  top-level project. It will automatically have the default extensions
      --  set when a project was registered, unless overridden by the user

      if Project = No_Project then
         P := Get_Project (Kernel);
      end if;

      Set_Text
        (Gtk_Entry (Get_Child (Self.Spec_Ext)),
         P.Attribute_Value
         (Spec_Suffix_Attribute, Index => Self.Language.all));
      Set_Text
        (Gtk_Entry (Get_Child (Self.Body_Ext)),
         P.Attribute_Value
         (Impl_Suffix_Attribute, Index => Self.Language.all));

      if Display_Exceptions then
         Show_Project_Settings (Self.Exceptions, Project);
      end if;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Foreign_Naming_Editor_Record) is
   begin
      Free (Self.Language);
   end Destroy;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self      : not null access Foreign_Naming_Editor_Record;
      Languages : GNAT.Strings.String_List) return Boolean
   is
   begin
      return In_List (Self.Language.all, Languages);
   end Is_Visible;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Foreign_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages, Kernel);
      Changed : Boolean;
   begin
      Changed :=
        Create_Project_Entry (Self.Exceptions, Project, Scenario_Variables);

      if Project = No_Project
        or else
          Project.Attribute_Value
          (Attribute => Spec_Suffix_Attribute, Index => Self.Language.all) /=
          Get_Active_Text (Self.Spec_Ext)
      then
         Project.Set_Attribute
         (Scenario                                 =>
            Scenario_Variables, Attribute          =>
            Spec_Suffix_Attribute, Value           =>
            Get_Active_Text (Self.Spec_Ext), Index =>
            Self.Language.all);
         Changed := True;
      end if;

      if Project = No_Project
        or else
          Project.Attribute_Value
          (Attribute => Impl_Suffix_Attribute, Index => Self.Language.all) /=
          Get_Active_Text (Self.Body_Ext)
      then
         Project.Set_Attribute
         (Scenario                                 =>
            Scenario_Variables, Attribute          =>
            Impl_Suffix_Attribute, Value           =>
            Get_Active_Text (Self.Body_Ext), Index =>
            Self.Language.all);
         Changed := True;
      end if;

      return Changed;
   end Edit_Project;

end Foreign_Naming_Editors;

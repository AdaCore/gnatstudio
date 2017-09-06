------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

with Gtk.Box;                 use Gtk.Box;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Label;               use Gtk.Label;
with Gtk.Size_Group;          use Gtk.Size_Group;

with Dialog_Utils;            use Dialog_Utils;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Project;      use GPS.Kernel.Project;

package body Custom_Naming_Editors is

   ----------------------------------
   -- Custom_Naming_Editor_Factory --
   ----------------------------------

   function Custom_Naming_Editor_Factory
     (Kernel : not null access Kernel_Handle_Record'Class;
      Lang   : String) return not null access Project_Editor_Page_Record'Class
   is
      pragma Unreferenced (Kernel);
      Result : Custom_Naming_Editor;
   begin
      Result := new Custom_Naming_Editor_Record;
      Result.Language := new String'(Lang);
      return Result;
   end Custom_Naming_Editor_Factory;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self         : not null access Custom_Naming_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project)
   is
      Extensions : String_List :=
        Get_Registry (Kernel).Environment.Registered_Extensions
           (Self.Language.all);
      Label      : Gtk_Label;
      Box        : Gtk_Box;
      Vbox       : Gtk_Box;
      Group      : Gtk_Size_Group;
      Ent        : Gtk_Entry;
      Frame      : Gtk_Frame;
      P          : Project_Type := Project;

   begin
      Dialog_Utils.Initialize (Self);

      Gtk_New (Group);

      Gtk_New (Frame, -"Details");
      Self.Append (Frame);

      Gtk_New_Vbox (Vbox, Homogeneous => True);
      Frame.Add (Vbox);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Vbox.Pack_Start (Box);
      Gtk_New (Label, -"Spec. Extension:  ");
      Label.Set_Alignment (0.0, 0.5);
      Box.Pack_Start (Label, Expand => False);
      Group.Add_Widget (Label);

      Gtk_New (Self.Spec_Extension);
      Self.Spec_Extension.Set_Sensitive (not Read_Only);
      Self.Spec_Extension.Set_Tooltip_Text
        (-("File extension for specification files. These files are generally"
           & " not to be compiled. Leave this field empty if there are no"
           & " spec files for this language."));
      Box.Pack_Start (Self.Spec_Extension, Expand => True, Fill => True);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Vbox.Pack_Start (Box);
      Gtk_New (Label, -"Impl. Extension:  ");
      Label.Set_Alignment (0.0, 0.5);
      Box.Pack_Start (Label, Expand => False);
      Group.Add_Widget (Label);

      Gtk_New (Self.Impl_Extension);
      Self.Impl_Extension.Set_Sensitive (not Read_Only);
      Self.Impl_Extension.Set_Tooltip_Text
        (-("File extension for implementation files. These files must"
            & " generally be compiled. Leave this field empty if there are no"
            & " implementation files for this language."));
      Box.Pack_Start (Self.Impl_Extension, Expand => True, Fill => True);

      if Extensions'Length /= 0 then
         Gtk_New (Frame, -"Predefined extensions");
         Self.Append (Frame);
         Gtk_New_Vbox (Vbox, Homogeneous => True);
         Frame.Add (Vbox);

         for E in Extensions'Range loop
            Gtk_New_Hbox (Box, Homogeneous => False);
            Vbox.Pack_Start (Box, Expand => False);

            Gtk_New (Label, -"Extension:  ");
            Label.Set_Alignment (0.0, 0.5);
            Box.Pack_Start (Label, Expand => False);
            Group.Add_Widget (Label);

            Gtk_New (Ent);
            Ent.Set_Sensitive (not Read_Only);
            Ent.Set_Text (Extensions (E).all & (-"  (cannot be changed)"));
            Ent.Set_Tooltip_Text
              (-("Extension defined in the GPS configuration files for this"
                 & " language. This extension is shown here for reference"
                 & " purposes, and is used as the default value for the"
                 & " extensions above"));
            Ent.Set_Editable (False);
            Box.Pack_Start (Ent, Expand => True, Fill => True);
         end loop;

         Free (Extensions);
      end if;

      --  ??? These are implementation exceptions, should have support for
      --  spec exceptions
      Gtk_New (Frame, -"Exceptions");
      Self.Append (Frame, Expand => True, Fill => True);

      Gtk_New (Self.Exceptions, Self.Language.all);
      Self.Exceptions.Set_Border_Width (3);
      Frame.Add (Self.Exceptions);

      --  Update project's settings

      if Project = No_Project then
         P := Get_Project (Kernel);
      end if;

      Self.Spec_Extension.Set_Text
        (P.Attribute_Value
           (Spec_Suffix_Attribute, Index => To_Lower (Self.Language.all)));
      Self.Impl_Extension.Set_Text
        (P.Attribute_Value
           (Impl_Suffix_Attribute, Index => To_Lower (Self.Language.all)));

      Show_Project_Settings (Self.Exceptions, Project);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self         : not null access Custom_Naming_Editor_Record;
      Languages    : GNAT.Strings.String_List) return Boolean
   is
   begin
      return In_List (Self.Language.all, Languages);
   end Is_Visible;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : in out Custom_Naming_Editor_Record) is
   begin
      Free (Self.Language);
   end Destroy;

   ------------------
   -- Edit_Project --
   ------------------

   overriding function Edit_Project
     (Self               : not null access Custom_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages, Kernel);
      Changed : Boolean := False;
   begin
      if Project = No_Project
        or else Project.Attribute_Value
          (Attribute => Spec_Suffix_Attribute,
           Index     => To_Lower (Self.Language.all)) /=
              Get_Text (Self.Spec_Extension)
      then
         Project.Set_Attribute
           (Scenario  => Scenario_Variables,
            Attribute => Spec_Suffix_Attribute,
            Value     => Get_Text (Self.Spec_Extension),
            Index     => To_Lower (Self.Language.all));
         Changed := True;
      end if;

      if Project = No_Project
        or else Project.Attribute_Value
          (Attribute => Impl_Suffix_Attribute,
           Index     => To_Lower (Self.Language.all)) /=
        Get_Text (Self.Impl_Extension)
      then
         Project.Set_Attribute
           (Scenario  => Scenario_Variables,
            Attribute => Impl_Suffix_Attribute,
            Value     => Get_Text (Self.Impl_Extension),
            Index     => To_Lower (Self.Language.all));
         Changed := True;
      end if;

      return Changed or
        Create_Project_Entry (Self.Exceptions, Project, Scenario_Variables);
   end Edit_Project;

end Custom_Naming_Editors;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2006, AdaCore              --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Basic_Types;                      use Basic_Types;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Project;               use GPS.Kernel, GPS.Kernel.Project;
with Gtk.Box;                          use Gtk.Box;
with Gtk.Frame;                        use Gtk.Frame;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Size_Group;                   use Gtk.Size_Group;
with Gtk.Tooltips;                     use Gtk.Tooltips;
with Gtk.Widget;                       use Gtk.Widget;
with Projects.Editor;                  use Projects, Projects.Editor;
with Projects.Registry;                use Projects.Registry;
with Naming_Exceptions;                use Naming_Exceptions;

package body Custom_Naming_Editors is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor   : out Custom_Naming_Editor;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String)
   is
      Extensions : Argument_List := Get_Registered_Extensions
        (Get_Registry (Kernel).all, Language);
      Label      : Gtk_Label;
      Box        : Gtk_Box;
      Vbox       : Gtk_Box;
      Group      : Gtk_Size_Group;
      Ent        : Gtk_Entry;
      Frame      : Gtk_Frame;
   begin
      Editor := new Custom_Naming_Editor_Record;
      Editor.Language := new String'(Language);
      Gtk_New_Vbox (Editor.GUI, Homogeneous => False, Spacing => 4);

      Gtk_New (Group);

      Gtk_New (Frame, -"Details");
      Pack_Start (Editor.GUI, Frame, Expand => False);

      Gtk_New_Vbox (Vbox, Homogeneous => True);
      Add (Frame, Vbox);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box);
      Gtk_New (Label, -"Spec. Extension:  ");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Add_Widget (Group, Label);
      Gtk_New (Editor.Spec_Extension);
      Set_Tip
        (Get_Tooltips (Kernel), Editor.Spec_Extension,
         -("File extension for specification files. These files are generally"
           & " not to be compiled. Leave this field empty if there are no"
           & " spec files for this language."));
      Pack_Start (Box, Editor.Spec_Extension, Expand => True, Fill => True);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box);
      Gtk_New (Label, -"Impl. Extension:  ");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_Start (Box, Label, Expand => False);
      Add_Widget (Group, Label);
      Gtk_New (Editor.Impl_Extension);
      Set_Tip
        (Get_Tooltips (Kernel), Editor.Impl_Extension,
         -("File extension for implementation files. These files must"
            & " generally be compiled. Leave this field empty if there are no"
            & " implementation files for this language."));
      Pack_Start (Box, Editor.Impl_Extension, Expand => True, Fill => True);

      if Extensions'Length /= 0 then
         Gtk_New (Frame, -"Predefined extensions");
         Pack_Start (Editor.GUI, Frame, Expand => False);
         Gtk_New_Vbox (Vbox, Homogeneous => True);
         Add (Frame, Vbox);

         for E in Extensions'Range loop
            Gtk_New_Hbox (Box, Homogeneous => False);
            Pack_Start (Vbox, Box, Expand => False);

            Gtk_New (Label, -"Extension:  ");
            Set_Alignment (Label, 0.0, 0.5);
            Pack_Start (Box, Label, Expand => False);
            Add_Widget (Group, Label);

            Gtk_New (Ent);
            Set_Text (Ent, Extensions (E).all & (-"  (cannot be changed)"));
            Set_Tip
              (Get_Tooltips (Kernel), Ent,
               -("Extension defined in the GPS configuration files for this"
                 & " language. This extension is shown here for reference"
                 & " purposes, and is used as the default value for the"
                 & " extensions above"));
            Set_Editable (Ent, False);
            Pack_Start (Box, Ent, Expand => True, Fill => True);
         end loop;

         Free (Extensions);
      end if;

      --  ??? These are implementation exceptions, should have support for
      --  spec exceptions
      Gtk_New (Frame, -"Exceptions");
      Pack_Start (Editor.GUI, Frame, Expand => True, Fill => True);

      Gtk_New (Editor.Exceptions, Language);
      Set_Border_Width (Editor.Exceptions, 3);
      Add (Frame, Editor.Exceptions);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Editor : access Custom_Naming_Editor_Record) is
   begin
      Destroy (Editor.GUI);
      Free (Editor.Language);
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Custom_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.GUI);
   end Get_Window;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor  : access Custom_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Languages    : Argument_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages);
      Changed  : Boolean := False;
   begin
      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Spec_Suffix_Attribute,
           Index          => To_Lower (Editor.Language.all)) /=
        Get_Text (Editor.Spec_Extension)
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Spec_Suffix_Attribute,
            Value              => Get_Text (Editor.Spec_Extension),
            Attribute_Index    => To_Lower (Editor.Language.all));
         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Impl_Suffix_Attribute,
           Index          => To_Lower (Editor.Language.all)) /=
        Get_Text (Editor.Impl_Extension)
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Impl_Suffix_Attribute,
            Value              => Get_Text (Editor.Impl_Extension),
            Attribute_Index    => To_Lower (Editor.Language.all));
         Changed := True;
      end if;

      return Changed or
        Create_Project_Entry (Editor.Exceptions, Project, Scenario_Variables);
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Custom_Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True)
   is
      P : Project_Type := Project;
   begin
      --  If the project is null, we get the default values from the current
      --  top-level project. It will automatically have the default extensions
      --  set when a project was registered, unless overriden by the user

      if Project = No_Project then
         P := Get_Project (Kernel);
      end if;

      Set_Text
        (Editor.Spec_Extension,
         Get_Attribute_Value
            (P, Spec_Suffix_Attribute,
             Index => To_Lower (Editor.Language.all)));
      Set_Text
        (Editor.Impl_Extension,
         Get_Attribute_Value
            (P, Impl_Suffix_Attribute,
             Index => To_Lower (Editor.Language.all)));

      if Display_Exceptions then
         Show_Project_Settings (Editor.Exceptions, Project);
      end if;
   end Show_Project_Settings;

end Custom_Naming_Editors;

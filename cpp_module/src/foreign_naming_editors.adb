-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
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

with Glide_Intl;                       use Glide_Intl;
with Glide_Kernel.Project;             use Glide_Kernel.Project;
with Gtk.Box;                          use Gtk.Box;
with Gtk.Combo;                        use Gtk.Combo;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Frame;                        use Gtk.Frame;
with Gtk.Label;                        use Gtk.Label;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Size_Group;                   use Gtk.Size_Group;
with Gtk.Widget;                       use Gtk.Widget;
with Projects.Editor;                  use Projects, Projects.Editor;
with Naming_Exceptions;                use Naming_Exceptions;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;

package body Foreign_Naming_Editors is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor   : out Foreign_Naming_Editor;
      Language : String)
   is
      Frame     : Gtk_Frame;
      Box, Vbox : Gtk_Box;
      Label     : Gtk_Label;
      Group     : Gtk_Size_Group;
      Items     : Gtk.Enums.String_List.Glist;
   begin
      Editor := new Foreign_Naming_Editor_Record;
      Editor.Language := new String'(Language);

      Gtk_New_Vbox (Editor.GUI, Homogeneous => False);

      Gtk_New (Group);

      Gtk_New (Frame, -"Details");
      Pack_Start (Editor.GUI, Frame, Expand => False);

      Gtk_New_Vbox (Vbox, Homogeneous => True);
      Add (Frame, Vbox);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box, Expand => False);

      Gtk_New (Label, -"Header files:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Group, Label);
      Pack_Start (Box, Label);

      Gtk_New (Editor.Spec_Ext);
      Pack_Start (Box, Editor.Spec_Ext);
      Gtk.Enums.String_List.Append (Items, ".h");
      Gtk.Enums.String_List.Append (Items, ".hh");
      Gtk.Enums.String_List.Append (Items, ".H");
      Gtk.Enums.String_List.Append (Items, ".hpp");
      Set_Popdown_Strings (Editor.Spec_Ext, Items);
      Gtk.Enums.String_List.Free (Items);

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box, Expand => False);

      Gtk_New (Label, -"Implementation:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Group, Label);
      Pack_Start (Box, Label);

      Gtk_New (Editor.Body_Ext);
      Pack_Start (Box, Editor.Body_Ext);
      Gtk.Enums.String_List.Append (Items, ".c");
      Gtk.Enums.String_List.Append (Items, ".cc");
      Gtk.Enums.String_List.Append (Items, ".C");
      Gtk.Enums.String_List.Append (Items, ".cpp");
      Set_Popdown_Strings (Editor.Body_Ext, Items);
      Gtk.Enums.String_List.Free (Items);

      Gtk_New (Frame, -"Exceptions");
      Pack_Start (Editor.GUI, Frame, Expand => True, Fill => True);
      Gtk_New (Editor.Exceptions, Language);
      Set_Border_Width (Editor.Exceptions, 3);
      Add (Frame, Editor.Exceptions);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Editor : access Foreign_Naming_Editor_Record) is
   begin
      Free (Editor.Language);
      Destroy (Editor.GUI);
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.GUI);
   end Get_Window;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor  : access Foreign_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      Changed  : Boolean;

   begin
      Changed := Create_Project_Entry
        (Editor.Exceptions, Project, Scenario_Variables);

      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Spec_Suffix_Attribute,
           Index          => Editor.Language.all) /=
              Get_Text (Get_Entry (Editor.Spec_Ext))
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Spec_Suffix_Attribute,
            Value              => Get_Text (Get_Entry (Editor.Spec_Ext)),
            Attribute_Index    => Editor.Language.all);
         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Impl_Suffix_Attribute,
           Index          => Editor.Language.all) /=
              Get_Text (Get_Entry (Editor.Body_Ext))
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Impl_Suffix_Attribute,
            Value              => Get_Text (Get_Entry (Editor.Body_Ext)),
            Attribute_Index    => Editor.Language.all);
         Changed := True;
      end if;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True)
   is
      P      : Project_Type := Project;
   begin
      --  If the project is null, we get the default values from the current
      --  top-level project. It will automatically have the default extensions
      --  set when a project was registered, unless overriden by the user

      if Project = No_Project then
         P := Get_Project (Kernel);
      end if;

      --  ??? Do we get access to the default extensions correctly ?

      Set_Text
        (Get_Entry (Editor.Spec_Ext),
         Get_Attribute_Value
            (P, Spec_Suffix_Attribute,
             Index => Editor.Language.all));
      Set_Text
        (Get_Entry (Editor.Body_Ext),
         Get_Attribute_Value
            (P, Impl_Suffix_Attribute,
             Index => Editor.Language.all));

      if Display_Exceptions then
         Show_Project_Settings (Editor.Exceptions, Project);
      end if;
   end Show_Project_Settings;

end Foreign_Naming_Editors;

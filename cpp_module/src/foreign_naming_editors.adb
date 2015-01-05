------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

with GNAT.OS_Lib;                      use GNAT.OS_Lib;

with Gtk.Box;                          use Gtk.Box;
with Gtk.Combo_Box_Text;               use Gtk.Combo_Box_Text;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Frame;                        use Gtk.Frame;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Size_Group;                   use Gtk.Size_Group;
with Gtk.Widget;                       use Gtk.Widget;

with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Project;               use GPS.Kernel.Project;

with Naming_Exceptions;                use Naming_Exceptions;

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

      Gtk_New_With_Entry (Editor.Spec_Ext);
      Editor.Spec_Ext.Set_Entry_Text_Column (0);
      Pack_Start (Box, Editor.Spec_Ext);
      Editor.Spec_Ext.Append_Text (".h");
      Editor.Spec_Ext.Append_Text (".hh");
      Editor.Spec_Ext.Append_Text (".H");
      Editor.Spec_Ext.Append_Text (".hpp");

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box, Expand => False);

      Gtk_New (Label, -"Implementation:");
      Set_Alignment (Label, 0.0, 0.5);
      Add_Widget (Group, Label);
      Pack_Start (Box, Label);

      Gtk_New_With_Entry (Editor.Body_Ext);
      Editor.Body_Ext.Set_Entry_Text_Column (0);
      Pack_Start (Box, Editor.Body_Ext);
      Editor.Body_Ext.Append_Text (".c");
      Editor.Body_Ext.Append_Text (".cc");
      Editor.Body_Ext.Append_Text (".C");
      Editor.Body_Ext.Append_Text (".cpp");

      Gtk_New (Frame, -"Exceptions");
      Pack_Start (Editor.GUI, Frame, Expand => True, Fill => True);
      Gtk_New (Editor.Exceptions, Language);
      Set_Border_Width (Editor.Exceptions, 3);
      Add (Frame, Editor.Exceptions);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Editor : access Foreign_Naming_Editor_Record) is
   begin
      Free (Editor.Language);
      Destroy (Editor.GUI);
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   overriding function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.GUI);
   end Get_Window;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   overriding function Create_Project_Entry
     (Editor             : access Foreign_Naming_Editor_Record;
      Project            : Project_Type;
      Languages          : Argument_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      pragma Unreferenced (Languages);
      Changed  : Boolean;

   begin
      Changed := Create_Project_Entry
        (Editor.Exceptions, Project, Scenario_Variables);

      if Project = No_Project
        or else Project.Attribute_Value
          (Attribute      => Spec_Suffix_Attribute,
           Index          => Editor.Language.all) /=
              Get_Active_Text (Editor.Spec_Ext)
      then
         Project.Set_Attribute
           (Scenario  => Scenario_Variables,
            Attribute => Spec_Suffix_Attribute,
            Value     => Get_Active_Text (Editor.Spec_Ext),
            Index     => Editor.Language.all);
         Changed := True;
      end if;

      if Project = No_Project
        or else Project.Attribute_Value
          (Attribute      => Impl_Suffix_Attribute,
           Index          => Editor.Language.all) /=
              Get_Active_Text (Editor.Body_Ext)
      then
         Project.Set_Attribute
           (Scenario  => Scenario_Variables,
            Attribute => Impl_Suffix_Attribute,
            Value     => Get_Active_Text (Editor.Body_Ext),
            Index     => Editor.Language.all);
         Changed := True;
      end if;

      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   overriding procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Project_Type;
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

      --  ??? Do we get access to the default extensions correctly ?

      Set_Text
        (Gtk_Entry (Get_Child (Editor.Spec_Ext)),
         P.Attribute_Value
            (Spec_Suffix_Attribute,
             Index => Editor.Language.all));
      Set_Text
        (Gtk_Entry (Get_Child (Editor.Body_Ext)),
         P.Attribute_Value
            (Impl_Suffix_Attribute,
             Index => Editor.Language.all));

      if Display_Exceptions then
         Show_Project_Settings (Editor.Exceptions, Project);
      end if;
   end Show_Project_Settings;

end Foreign_Naming_Editors;

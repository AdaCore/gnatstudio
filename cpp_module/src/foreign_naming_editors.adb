-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Basic_Types;                      use Basic_Types;
with Foreign_Naming_Scheme_Editor_Pkg; use Foreign_Naming_Scheme_Editor_Pkg;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
with Glib;                             use Glib;
with Glide_Intl;                       use Glide_Intl;
with Glide_Kernel.Project;             use Glide_Kernel.Project;
with Gtk.Box;                          use Gtk.Box;
with Gtk.Clist;                        use Gtk.Clist;
with Gtk.Combo;                        use Gtk.Combo;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Widget;                       use Gtk.Widget;
with Gtkada.Types;                     use Gtkada.Types;
with Interfaces.C.Strings;             use Interfaces.C.Strings;
with Projects.Editor;                  use Projects, Projects.Editor;
with Types;                            use Types;

package body Foreign_Naming_Editors is

   Empty_Filename : constant String := -"<filename>";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor   : out Foreign_Naming_Editor;
      Language : Types.Name_Id) is
   begin
      Editor := new Foreign_Naming_Editor_Record;
      Gtk_New (Editor.GUI);
      Editor.Language := Language;

      Ref (Editor.GUI.Main_Box);
      Unparent (Editor.GUI.Main_Box);

      Set_Auto_Sort (Editor.GUI.Exception_List, True);
      Set_Text (Editor.GUI.Filename_Entry, Empty_Filename);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Editor : access Foreign_Naming_Editor_Record) is
   begin
      Destroy (Editor.GUI);
      --  Editor.GUI.Main_Box is automatically destroyed when its parent
      --  contain (multi-language naming editor) is destroyed.
   end Destroy;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.GUI.Main_Box);
   end Get_Window;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor  : access Foreign_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Scenario_Variable_Array) return Boolean
   is
      Lang     : constant String := Get_String (Editor.Language);
      Num_Rows : constant Gint := Get_Rows (Editor.GUI.Exception_List);
      Bodies   : Argument_List (1 .. Integer (Num_Rows));
      Changed  : Boolean := False;

   begin
      for J in 0 .. Num_Rows - 1 loop
         Bodies (Integer (J + 1)) := new String'
           (Get_Text (Editor.GUI.Exception_List, J, 0));
      end loop;

      if Project = No_Project then
         Changed := True;
      else
         declare
            Old_Exceptions : Argument_List := Get_Attribute_Value
              (Project        => Project,
               Attribute      => Impl_Exception_Attribute,
               Index          => Lang);
         begin
            Changed := not Is_Equal (Bodies, Old_Exceptions);
            Free (Old_Exceptions);
         end;
      end if;

      if Changed then
         if Num_Rows /= 0 then
            Update_Attribute_Value_In_Scenario
              (Project           => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute         => Impl_Exception_Attribute,
               Values            => Bodies,
               Attribute_Index   => Lang);
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Impl_Exception_Attribute,
               Attribute_Index    => Lang);
         end if;

         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Spec_Suffix_Attribute,
           Index          => Lang) /=
        Get_Text (Get_Entry (Editor.GUI.Header_File_Extension))
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Spec_Suffix_Attribute,
            Value  => Get_Text (Get_Entry (Editor.GUI.Header_File_Extension)),
            Attribute_Index    => Lang);
         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Attribute_Value
          (Project        => Project,
           Attribute      => Impl_Suffix_Attribute,
           Index          => Lang) /=
        Get_Text (Get_Entry (Editor.GUI.Implementation_Extension))
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => Scenario_Variables,
            Attribute          => Impl_Suffix_Attribute,
            Value              =>
              Get_Text (Get_Entry (Editor.GUI.Implementation_Extension)),
            Attribute_Index    => Lang);
         Changed := True;
      end if;

      Free (Bodies);

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
      Lang   : constant String := Get_String (Editor.Language);
      Row    : Gint;
      pragma Unreferenced (Row);

      Text   : Gtkada.Types.Chars_Ptr_Array (0 .. 0);
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
        (Get_Entry (Editor.GUI.Header_File_Extension),
         Get_Attribute_Value
            (P, Spec_Suffix_Attribute,
             Index => Get_String (Editor.Language)));
      Set_Text
        (Get_Entry (Editor.GUI.Implementation_Extension),
         Get_Attribute_Value
            (P, Impl_Suffix_Attribute,
             Index => Get_String (Editor.Language)));

      if Project /= No_Project and then Display_Exceptions then
         declare
            Bodies : Argument_List := Get_Attribute_Value
              (Project,
               Attribute      => Impl_Exception_Attribute,
               Index          => Lang);
         begin
            Clear (Editor.GUI.Exception_List);
            for B in Bodies'Range loop
               Text (0) := New_String (Bodies (B).all);
               Row := Append (Editor.GUI.Exception_List, Text);
               Free (Text);
            end loop;

            Free (Bodies);
         end;
      end if;
   end Show_Project_Settings;

end Foreign_Naming_Editors;

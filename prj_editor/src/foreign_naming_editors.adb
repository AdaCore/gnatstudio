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
with Glide_Kernel;                     use Glide_Kernel;
with Gtk.Box;                          use Gtk.Box;
with Gtk.Clist;                        use Gtk.Clist;
with Gtk.Combo;                        use Gtk.Combo;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Widget;                       use Gtk.Widget;
with Gtkada.Types;                     use Gtkada.Types;
with Interfaces.C.Strings;             use Interfaces.C.Strings;
with Namet;                            use Namet;
with Prj;                              use Prj;
with Prj.Util;                         use Prj.Util;
with Prj.Tree;                         use Prj.Tree;
with Prj_API;                          use Prj_API;
with Snames;                           use Snames;
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
      Foreign_Naming_Scheme_Editor_Pkg.Initialize (Editor);
      Editor.Language := Language;

      Ref (Editor.Main_Box);
      Unparent (Editor.Main_Box);
      --  ??? When is the memory occupied by Editor itself ever freed ?

      Set_Auto_Sort (Editor.Exception_List, True);
      Set_Text (Editor.Filename_Entry, Empty_Filename);
   end Gtk_New;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.Main_Box);
   end Get_Window;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   function Create_Project_Entry
     (Editor  : access Foreign_Naming_Editor_Record;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project : Prj.Tree.Project_Node_Id) return Boolean
   is
      Naming   : constant String := Get_Name_String (Name_Naming);
      Scenar   : constant Project_Node_Array := Scenario_Variables (Kernel);
      Lang     : constant String := Get_Name_String (Editor.Language);
      Num_Rows : constant Gint := Get_Rows (Editor.Exception_List);
      Bodies   : Argument_List (1 .. Integer (Num_Rows));
      Changed  : Boolean := False;

   begin
      Update_Attribute_Value_In_Scenario
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Specification_Suffix),
         Value          => Get_Text (Get_Entry (Editor.Header_File_Extension)),
         Attribute_Index    => Lang);
      Update_Attribute_Value_In_Scenario
        (Project            => Project,
         Pkg_Name           => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name     => Get_Name_String (Name_Implementation_Suffix),
         Value       => Get_Text (Get_Entry (Editor.Implementation_Extension)),
         Attribute_Index    => Lang);

      for J in 0 .. Num_Rows - 1 loop
         Bodies (Integer (J + 1)) := new String'
           (Get_Text (Editor.Exception_List, J, 0));
      end loop;

      Update_Attribute_Value_In_Scenario
        (Project           => Project,
         Pkg_Name          => Naming,
         Scenario_Variables => Scenar,
         Attribute_Name    => Get_Name_String (Name_Implementation_Exceptions),
         Values            => Bodies,
         Attribute_Index   => Lang);

      --  ??? Should return True only if the naming scheme actually changed.
      Changed := True;
      return Changed;
   end Create_Project_Entry;

   ---------------------------
   -- Show_Project_Settings --
   ---------------------------

   procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Project_View       : Prj.Project_Id;
      Display_Exceptions : Boolean := True)
   is
      Naming : constant String := Get_Name_String (Name_Naming);
      Lang   : constant String := Get_Name_String (Editor.Language);
      Bodies : Argument_List := Get_Attribute_Value
        (Project_View,
         Attribute_Name => Get_Name_String (Name_Implementation_Exceptions),
         Package_Name   => Naming,
         Index          => Lang);
      Row  : Gint;
      Text : Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      Ext  : Name_Id;
   begin
      --  We directly access the tables in Prj, instead of using
      --  Get_Attribute_Value, so that we also get access to the default
      --  extensions.

      Ext := Value_Of
        (Index => Editor.Language,
         In_Array => Projects.Table
           (Project_View).Naming.Specification_Suffix);
      if Ext /= No_Name then
         Set_Text
           (Get_Entry (Editor.Header_File_Extension), Get_Name_String (Ext));
      else
         Set_Text (Get_Entry (Editor.Header_File_Extension), "");
      end if;

      Ext := Value_Of
        (Index => Editor.Language,
         In_Array => Projects.Table
           (Project_View).Naming.Implementation_Suffix);

      if Ext /= No_Name then
         Set_Text
           (Get_Entry (Editor.Implementation_Extension),
            Get_Name_String (Ext));
      else
         Set_Text (Get_Entry (Editor.Implementation_Extension), "");
      end if;

      if Display_Exceptions then
         Clear (Editor.Exception_List);
         for B in Bodies'Range loop
            Text (0) := New_String (Bodies (B).all);
            Row := Append (Editor.Exception_List, Text);
            Free (Text);
         end loop;
      end if;
      Free (Bodies);
   end Show_Project_Settings;
end Foreign_Naming_Editors;

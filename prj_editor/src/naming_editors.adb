-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                     use Glib;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Box;                  use Gtk.Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Combo;                use Gtk.Combo;
with Gtk.List;                 use Gtk.List;
with Gtk.Clist;                use Gtk.Clist;
with Gtkada.Types;             use Gtkada.Types;
with Prj.Tree;                 use Prj.Tree;
with Prj_API;                  use Prj_API;
with Prj;                      use Prj;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;

with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Naming_Editors is

   Empty_Unit_Name : constant String := "<unit_name>";
   Empty_Spec_Name : constant String := "<spec_file_name>";
   Empty_Body_Name : constant String := "<body_file_name>";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Naming_Editor) is
   begin
      Editor := new Naming_Editor_Record;
      Initialize (Editor);
      Ref (Editor.Main_Box);
      Unparent (Editor.Main_Box);
      Set_Auto_Sort (Editor.Exception_List, True);
      Reset_Exception_Fields (Editor);
   end Gtk_New;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Editor : access Naming_Editor_Record) return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Editor.Main_Box);
   end Get_Window;

   ----------------------------
   -- Reset_Exception_Fields --
   ----------------------------

   procedure Reset_Exception_Fields
     (Editor : access Naming_Editor_Record'Class;
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
     (Editor : access Naming_Editor_Record;
      Scheme_Num : Natural) is
   begin
      case Scheme_Num is
         when 0 =>
            --  GNAT Default
            Set_Text (Get_Entry (Editor.Casing), "lowercase");
            Set_Text (Editor.Dot_Replacement, "-");
            Set_Text (Get_Entry (Editor.Spec_Extension), ".ads");
            Set_Text (Get_Entry (Editor.Body_Extension), ".adb");
            Set_Text (Get_Entry (Editor.Separate_Extension), ".adb");

         when 1 =>
            --  APEX Default
            Set_Text (Get_Entry (Editor.Casing), "lowercase");
            Set_Text (Editor.Dot_Replacement, ".");
            Set_Text (Get_Entry (Editor.Spec_Extension), ".1.ada");
            Set_Text (Get_Entry (Editor.Body_Extension), ".2.ada");
            Set_Text (Get_Entry (Editor.Separate_Extension), ".2.ada");

         when others =>
            null;
      end case;
   end Set_Predefined_Scheme;

   --------------------------
   -- Create_Project_Entry --
   --------------------------

   procedure Create_Project_Entry
     (Editor  : access Naming_Editor_Record;
      Project : Prj.Tree.Project_Node_Id)
   is
      use Widget_List;
      List  : Gtk_List := Get_List (Editor.Standard_Scheme);
      Scheme : Gint := Child_Position (List, Get_Data (Get_Selection (List)));
      Pack : Project_Node_Id := Empty_Node;
      Var : Project_Node_Id;
      Num_Rows : constant Gint := Get_Rows (Editor.Exception_List);
   begin
      --  Do nothing for the standard GNAT naming scheme
      if Scheme /= 0 then
         Pack := Get_Or_Create_Package (Project, "Naming");
         Var := Get_Or_Create_Attribute (Pack, "Casing", Kind => Single);
         Set_Value (Var, Get_Text (Get_Entry (Editor.Casing)));
         Var := Get_Or_Create_Attribute
           (Pack, "Dot_Replacement", Kind => Single);
         Set_Value (Var, Get_Text (Editor.Dot_Replacement));
         Var := Get_Or_Create_Attribute
           (Pack, "Specification_Append", Kind => Single);
         Set_Value (Var, Get_Text (Get_Entry (Editor.Spec_Extension)));
         Var := Get_Or_Create_Attribute (Pack, "Body_Append", Kind => Single);
         Set_Value (Var, Get_Text (Get_Entry (Editor.Body_Extension)));
         Var := Get_Or_Create_Attribute
           (Pack, "Separate_Append", Kind => Single);
         Set_Value (Var, Get_Text (Get_Entry (Editor.Separate_Extension)));
      end if;

      if Num_Rows /= 0 then
         if Pack = Empty_Node then
            Pack := Get_Or_Create_Package (Project, "Naming");
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
                  Var := Get_Or_Create_Attribute
                    (Pack, "Specification", U, Single);
                  Set_Value (Var, Spec);
               end if;
               if Bod /= "" then
                  Var := Get_Or_Create_Attribute
                    (Pack, "Body_Part", U, Single);
                  Set_Value (Var, Bod);
               end if;
            end;
         end loop;

      end if;
   end Create_Project_Entry;

   -----------------------
   -- Add_New_Exception --
   -----------------------

   procedure Add_New_Exception (Editor : access Naming_Editor_Record) is
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

   procedure Clear_Unit_Name (Editor : access Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Unit_Name_Entry) = Empty_Unit_Name then
         Set_Text (Editor.Unit_Name_Entry, "");
      end if;
   end Clear_Unit_Name;

   ---------------------
   -- Clear_Spec_Name --
   ---------------------

   procedure Clear_Spec_Name (Editor : access Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Spec_Filename_Entry) = Empty_Spec_Name then
         Set_Text (Editor.Spec_Filename_Entry, "");
      end if;
   end Clear_Spec_Name;

   ---------------------
   -- Clear_Body_Name --
   ---------------------

   procedure Clear_Body_Name (Editor : access Naming_Editor_Record) is
   begin
      if Get_Text (Editor.Body_Filename_Entry) = Empty_Body_Name then
         Set_Text (Editor.Body_Filename_Entry, "");
      end if;
   end Clear_Body_Name;

end Naming_Editors;


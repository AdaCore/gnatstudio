-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Basic_Types;              use Basic_Types;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Glib.Values;              use Glib.Values;
with Glib;                     use Glib;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Object;               use Gtk.Object;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Language_Handlers;        use Language_Handlers;
with Projects;                 use Projects;
with String_Utils;             use String_Utils;

package body Languages_Lists is

   procedure Changed
     (List   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Emits the "changed" signal

   List_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   List_Signals : constant chars_ptr_array := (1 => New_String ("changed"));

   Language_Column : constant := 0;
   Selected_Column : constant := 1;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List : out Languages_List;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project : Projects.Project_Type)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));
      Languages : Argument_List := Known_Languages
        (Get_Language_Handler (Kernel));
      View : Gtk_Tree_View;
      Col  : Gtk_Tree_View_Column;
      Toggle : Gtk_Cell_Renderer_Toggle;
      Text   : Gtk_Cell_Renderer_Text;
      Col_Number : Gint;
      pragma Unreferenced (Col_Number);
      Iter   : Gtk_Tree_Iter;
      Scrolled : Gtk_Scrolled_Window;
   begin
      List := new Languages_List_Record;
      Initialize (List, -"Languages");
      Set_Border_Width (List, 5);

      Gtk.Object.Initialize_Class_Record
        (List,
         Signals      => List_Signals,
         Class_Record => List_Class_Record,
         Type_Name    => "LanguagesList",
         Parameters   => Signal_Parameters);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (List, Scrolled);

      Gtk_New (List.Languages,
               (Language_Column => GType_String,
                Selected_Column => GType_Boolean));
      Gtk_New (View, List.Languages);
      Set_Headers_Visible (View, False);
      Add (Scrolled, View);

      Gtk_New (Toggle);
      Widget_Callback.Object_Connect (Toggle, "toggled", Changed'Access, List);

      Gtk_New (Text);

      Gtk_New (Col);
      Col_Number := Append_Column (View, Col);
      Pack_Start (Col, Toggle, False);
      Add_Attribute (Col, Toggle, "active", Selected_Column);

      Pack_Start (Col, Text, True);
      Add_Attribute (Col, Text, "text", Language_Column);
      Set_Sort_Column_Id (Col, Language_Column);
      Clicked (Col);

      List.Kernel    := Kernel_Handle (Kernel);

      for L in Languages'Range loop
         declare
            S : String := Languages (L).all;
         begin
            Mixed_Case (S);

            Append (List.Languages, Iter, Null_Iter);
            Set (List.Languages, Iter, Language_Column, S);
            Set (List.Languages, Iter, Selected_Column, S = "Ada");
         end;
      end loop;

      if Project /= No_Project then
         declare
            Project_Languages : Argument_List :=  Get_Languages (Project);
         begin
            Iter := Get_Iter_First (List.Languages);
            while Iter /= Null_Iter loop
               Set (List.Languages, Iter, Selected_Column,
                    Contains
                      (Project_Languages,
                       Get_String (List.Languages, Iter, Language_Column),
                       Case_Sensitive => False));
               Next (List.Languages, Iter);
            end loop;
            Free (Project_Languages);
         end;
      end if;

      Free (Languages);
   end Gtk_New;

   -------------
   -- Changed --
   -------------

   procedure Changed (List : access Languages_List_Record) is
   begin
      Widget_Callback.Emit_By_Name (List, "changed");
   end Changed;

   -------------
   -- Changed --
   -------------

   procedure Changed
     (List : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      L    : constant Languages_List := Languages_List (List);
      Path : constant String := Get_String (Nth (Params, 1));
      Iter : constant Gtk_Tree_Iter :=
        Get_Iter_From_String (L.Languages, Path);
   begin
      Set (L.Languages, Iter, Selected_Column,
           not Get_Boolean (L.Languages, Iter, Selected_Column));
      Changed (L);
   end Changed;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (List : access Languages_List_Record) return GNAT.OS_Lib.Argument_List
   is
      Num_Languages : Natural := 0;
      Iter          : Gtk_Tree_Iter;

   begin
      Iter := Get_Iter_First (List.Languages);
      while Iter /= Null_Iter loop
         if Get_Boolean (List.Languages, Iter, Selected_Column) then
            Num_Languages := Num_Languages + 1;
         end if;
         Next (List.Languages, Iter);
      end loop;

      declare
         New_Languages : Argument_List (1 .. Num_Languages);
      begin
         Num_Languages := New_Languages'First;

         Iter := Get_Iter_First (List.Languages);
         while Iter /= Null_Iter loop
            if Get_Boolean (List.Languages, Iter, Selected_Column) then
               New_Languages (Num_Languages) := new String'
                 (Get_String (List.Languages, Iter, Language_Column));
               Num_Languages := Num_Languages + 1;
            end if;
            Next (List.Languages, Iter);
         end loop;
         return New_Languages;
      end;
   end Get_Languages;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (List : access Languages_List_Record; Language : String)
      return Boolean
   is
      Iter : Gtk_Tree_Iter := Get_Iter_First (List.Languages);
   begin
      while Iter /= Null_Iter loop
         if Case_Insensitive_Equal
           (Get_String (List.Languages, Iter, Language_Column), Language)
         then
            return Get_Boolean (List.Languages, Iter, Selected_Column);
         end if;

         Next (List.Languages, Iter);
      end loop;
      return False;
   end Is_Selected;

end Languages_Lists;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Containers;            use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with GNAT.Strings;              use GNAT.Strings;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;

with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model_Filter;     use Gtk.Tree_Model_Filter;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Glib.Convert;              use Glib.Convert;

with Basic_Types;               use Basic_Types;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Language.Unknown;          use Language.Unknown;
with Language.Icons;            use Language.Icons;
with Language_Handlers;         use Language_Handlers;
with Language_Utils;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;
with Gtk.Target_List;
with Gtk.Dnd;

package body Project_Explorers_Common is

   Me : constant Trace_Handle := Create ("Project_Explorers_Common");

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column          => GType_String,
         File_Column          => Get_Virtual_File_Type,
         Display_Name_Column  => GType_String,
         Node_Type_Column     => GType_Int,
         Line_Column          => GType_Int,
         Column_Column        => GType_Int,
         Entity_Base_Column   => GType_String);
   end Columns_Types;

   --------------------
   -- Stock_For_Node --
   --------------------

   function Stock_For_Node
     (Node : Node_Types; Expanded : Boolean) return String
   is
   begin
      case Node is
         when Project_Node | Extends_Project_Node =>
            if Expanded then
               return "gps-emblem-project-open";
            else
               return "gps-emblem-project-closed";
            end if;

         when Root_Project_Node =>
            if Expanded then
               return "gps-emblem-project-root-open";
            else
               return "gps-emblem-project-root-closed";
            end if;

         when Runtime_Node =>
            if Expanded then
               return "gps-emblem-project-runtime-open";
            else
               return "gps-emblem-project-runtime-closed";
            end if;

         when Modified_Project_Node =>
            if Expanded then
               return "gps-emblem-project-modified-open";
            else
               return "gps-emblem-project-modified-closed";
            end if;

         when Directory_Node =>
            if Expanded then
               return "gps-emblem-directory-open";
            else
               return "gps-emblem-directory-closed";
            end if;

         when Obj_Directory_Node =>
            if Expanded then
               return "gps-emblem-objdir-open";
            else
               return "gps-emblem-objdir-closed";
            end if;

         when Lib_Directory_Node =>
            if Expanded then
               return "gps-emblem-libdir-open";
            else
               return "gps-emblem-libdir-closed";
            end if;

         when Exec_Directory_Node =>
            if Expanded then
               return "gps-emblem-execdir-open";
            else
               return "gps-emblem-execdir-closed";
            end if;

         when File_Node =>
            return "gps-emblem-file-unmodified";

         when Category_Node =>
            return "gps-emblem-category";

         when Dummy_Node | Entity_Node =>
            return "";
      end case;
   end Stock_For_Node;

   -----------------
   -- Append_File --
   -----------------

   procedure Append_File
     (Kernel : Kernel_Handle;
      Model  : Gtk_Tree_Store;
      Base   : Gtk_Tree_Iter;
      File   : GNATCOLL.VFS.Virtual_File;
      Sorted : Boolean := False)
   is
      Iter  : Gtk_Tree_Iter;
      Iter2 : Gtk_Tree_Iter;
      Lang  : Language_Access;
      Done  : Boolean;
   begin
      if Sorted then
         Iter := Children (Model, Base);
         Done := False;

         while Iter /= Null_Iter loop
            Iter2 := Iter;

            if Get_Node_Type (Model, Iter) = File_Node then
               declare
                  Name : constant Filesystem_String :=
                           Get_Base_Name (Model, Iter);
               begin
                  if File.Base_Name < Name then
                     Insert_Before (Model, Iter2, Base, Iter);
                     Iter := Iter2;
                     Done := True;

                     exit;
                  end if;
               end;
            end if;

            Next (Model, Iter);
         end loop;

         if not Done then
            Append (Model, Iter, Base);
         end if;
      else
         Append (Model, Iter, Base);
      end if;

      Set_File (Model, Iter, File_Column, File);
      Model.Set (Iter, Display_Name_Column, Display_Base_Name (File));
      Set_Node_Type (Model, Iter, File_Node, False);

      Lang := Get_Language_From_File (Get_Language_Handler (Kernel), File);

      if Lang /= Unknown_Lang then
         Append_Dummy_Iter (Model, Iter);
      end if;
   end Append_File;

   -----------------------
   -- Append_Dummy_Iter --
   -----------------------

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Base);
      Set_Node_Type (Model, Iter, Dummy_Node, Expanded => False);
   end Append_Dummy_Iter;

   --------------------
   -- Has_Dummy_Iter --
   --------------------

   function Has_Dummy_Iter
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter) return Boolean
   is
      Iter : Gtk_Tree_Iter;
   begin
      Iter := Model.Nth_Child (Parent, 0);
      return Iter /= Null_Iter
        and then Get_Node_Type (Model, Iter) = Dummy_Node;
   end Has_Dummy_Iter;

   -----------------------
   -- Remove_Dummy_Iter --
   -----------------------

   procedure Remove_Dummy_Iter
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
   begin
      Iter := Model.Nth_Child (Parent, 0);
      if Iter /= Null_Iter
        and then Get_Node_Type (Model, Iter) = Dummy_Node
      then
         Model.Remove (Iter);
      end if;
   end Remove_Dummy_Iter;

   --------------------------
   -- Append_Category_Node --
   --------------------------

   function Append_Category_Node
     (Model         : Gtk_Tree_Store;
      File          : GNATCOLL.VFS.Virtual_File;
      Category      : Language_Category;
      Category_Name : GNATCOLL.Symbols.Symbol;
      Parent_Iter   : Gtk_Tree_Iter;
      Sorted        : Boolean) return Gtk_Tree_Iter
   is
      Name    : constant String :=
                  Language.Category_Name (Category, Category_Name);
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter := Null_Iter;

   begin
      if Sorted then
         Sibling := Children (Model, Parent_Iter);
         while Sibling /= Null_Iter
           and then Get_String (Model, Sibling, Display_Name_Column) <= Name
         loop
            Next (Model, Sibling);
         end loop;
      end if;

      if Sibling = Null_Iter then
         Append (Model, N, Parent_Iter);
      else
         Insert_Before (Model, N, Parent_Iter, Sibling);
      end if;

      Set_File (Model, N, File_Column, File);
      Set (Model, N, Display_Name_Column, Locale_To_UTF8 (Name));
      Set (Model, N, Icon_Column,
           Stock_From_Category
             (Is_Declaration => False,
              Visibility     => Visibility_Public,
              Category       => Category));
      Set (Model, N, Node_Type_Column, Gint (Node_Types'Pos (Category_Node)));

      return N;
   end Append_Category_Node;

   --------------------
   -- Entity_Name_Of --
   --------------------

   function Entity_Name_Of
     (Construct          : Construct_Information;
      Show_Profiles      : Boolean;
      Max_Profile_Length : Positive := Positive'Last) return String
   is

      function Escape return String;
      pragma Inline (Escape);
      --  Escape Construct.Name.all as a pango markup string.
      --  The characters which need to be escaped in pango markup language are
      --  '&', '<', '>', '\', and '"'.
      --  The code here assumes that Entity names, in any language, can only
      --  contain '&', '<', '>', or '"'  and that if it does, one of these
      --  characters is necessarily in the first position, for the overloading
      --  of operators such as '<' or '&&', or for a quoted name.

      ------------
      -- Escape --
      ------------

      function Escape return String is
         C : Character;
         Str : constant Cst_String_Access := Get (Construct.Name);
      begin
         if Str.all = "" then
            return "";
         end if;

         C := Str (Str'First);

         if C = '"' or else C = '&' or else C = '<' or else C = '>' then
            return Escape_Text (Str.all);
         else
            return Str.all;
         end if;
      end Escape;

      Name : constant String := Reduce (Escape);

   begin
      if Show_Profiles and then Construct.Profile /= null then
         return Name & " <span foreground=""#A0A0A0"">"
           & Escape_Text (Reduce (Construct.Profile.all, Max_Profile_Length))
           & "</span>";
      else
         return Name;
      end if;
   end Entity_Name_Of;

   --------------------
   -- Entity_Icon_Of --
   --------------------

   function Entity_Icon_Of
     (Construct : Construct_Information) return String is
   begin
      return Stock_From_Category
        (Is_Declaration => Construct.Is_Declaration,
         Visibility     => Construct.Visibility,
         Category       => Construct.Category);
   end Entity_Icon_Of;

   function Entity_Icon_Of
     (Construct : Simple_Construct_Information) return String
   is
   begin
      return Stock_From_Category
        (Is_Declaration => Construct.Is_Declaration,
         Visibility     => Construct.Visibility,
         Category       => Construct.Category);
   end Entity_Icon_Of;

   ------------------------
   -- Append_Entity_Node --
   ------------------------

   function Append_Entity_Node
     (Model       : Gtk_Tree_Store;
      File        : GNATCOLL.VFS.Virtual_File;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter;
      Sorted      : Boolean) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter := Null_Iter;

   begin
      if Sorted then
         Sibling := Children (Model, Parent_Iter);
         while Sibling /= Null_Iter
           and then Get_String (Model, Sibling, Display_Name_Column)
           <= Get (Construct.Name).all
         loop
            Next (Model, Sibling);
         end loop;
      end if;

      if Sibling = Null_Iter then
         Append (Model, N, Parent_Iter);
      else
         Insert_Before (Model, N, Parent_Iter, Sibling);
      end if;

      Set_File (Model, N, File_Column, File);
      Set (Model, N, Display_Name_Column, Entity_Name_Of (Construct, True));
      Set (Model, N, Entity_Base_Column, Reduce (Get (Construct.Name).all));
      Set (Model, N, Icon_Column, Entity_Icon_Of (Construct));
      Set (Model, N, Node_Type_Column, Gint (Node_Types'Pos (Entity_Node)));

      if Construct.Sloc_Entity.Line /= 0 then
         Set (Model, N, Line_Column, Gint (Construct.Sloc_Entity.Line));
         Set (Model, N, Column_Column, Gint (Construct.Sloc_Entity.Column));
      else
         Set (Model, N, Line_Column, Gint (Construct.Sloc_Start.Line));
         Set (Model, N, Column_Column, Gint (Construct.Sloc_Start.Column));
      end if;

      return N;
   end Append_Entity_Node;

   ----------------------
   -- Append_File_Info --
   ----------------------

   procedure Append_File_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Sorted    : Boolean)
   is
      package Iter_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Gtk_Tree_Iter,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");
      use Iter_Map;

      Languages  : constant Language_Handler := Get_Language_Handler (Kernel);
      N          : Gtk_Tree_Iter;
      Iter       : Gtk_Tree_Iter;
      Lang       : Language_Access;
      Constructs : Construct_List;
      Category   : Language_Category;
      Node_Appended : Boolean := False;
      Categories    : Iter_Map.Map;
      pragma Unreferenced (N);

   begin
      --  Remove any previous information for this file

      Remove_Child_Nodes (Model, Parent => Node);

      Lang := Get_Language_From_File (Languages, File_Name);

      if Lang /= null then
         Language_Utils.Parse_File_Constructs
           (Lang, File_Name, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= No_Symbol then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown
                 and then Category /= Cat_Parameter
                 and then Category /= Cat_Field
               then
                  declare
                     Name     : constant String :=
                                  Category_Name (Category,
                                                 Constructs.Current.
                                                   Category_Name);
                     Cursor   : Iter_Map.Cursor;
                     New_Iter : Gtk_Tree_Iter;

                  begin
                     Cursor := Iter_Map.Find (Categories, Name);

                     if Cursor = No_Element then
                        New_Iter :=
                           Append_Category_Node
                             (Model,
                              File_Name,
                              Category      => Category,
                              Category_Name =>
                                Constructs.Current.Category_Name,
                              Parent_Iter   => Node,
                              Sorted        => Sorted);
                        Insert (Categories, Name, New_Iter);

                     else
                        New_Iter := Element (Cursor);
                     end if;

                     N := Append_Entity_Node
                       (Model, File_Name, Constructs.Current.all, New_Iter,
                        Sorted => Sorted);
                  end;

                  Node_Appended := True;
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         --  If no node was appended, add a "no entity" node

         if not Node_Appended then
            Append (Model, Iter, Node);
            Set (Model, Iter, Display_Name_Column,
                 "<span foreground=""#555555"">"
                 & (-"(no entity)")
                 & "</span>");
            Set (Model, Iter, Node_Type_Column,
                 Gint (Node_Types'Pos (Category_Node)));
         end if;

         Free (Constructs);
      else
         Trace (Me, "No known language for " & Display_Full_Name (File_Name));
      end if;
   end Append_File_Info;

   --------------------------
   -- Create_Or_Reuse_Node --
   --------------------------

   function Create_Or_Reuse_Node
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File;
      Add_Dummy : Boolean := False) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Null_Iter;
      T    : Node_Types;
   begin
      if Parent = Null_Iter then
         Iter := Model.Get_Iter_First;
      else
         Iter := Model.Children (Parent);
      end if;

      while Iter /= Null_Iter loop
         T := Get_Node_Type (Model, Iter);
         if (T = Kind
             or else (Kind in Project_Node_Types
                      and then T in Project_Node_Types))
           and then Get_File (Model, Iter, File_Column) = File
         then
            if T /= Kind then
               Set_Node_Type (Model, Iter, Kind, False);
            end if;

            return Iter;
         end if;
         Model.Next (Iter);
      end loop;

      Model.Append (Iter => Iter, Parent => Parent);
      Model.Set (Iter, Display_Name_Column, Name);
      Set_File (Model, Iter, File_Column, File);
      Set_Node_Type (Model, Iter, Kind, False);

      if Add_Dummy then
         Append_Dummy_Iter (Model, Iter);
      end if;

      return Iter;
   end Create_Or_Reuse_Node;

   --------------------------
   -- Create_Or_Reuse_File --
   --------------------------

   procedure Create_Or_Reuse_File
     (Model  : Gtk_Tree_Store;
      Kernel : not null access Kernel_Handle_Record'Class;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File)
   is
      Child : Gtk_Tree_Iter;
      Lang : constant Language_Access := Get_Language_From_File
        (Get_Language_Handler (Kernel), File);
      pragma Unreferenced (Child);
   begin
      Child := Create_Or_Reuse_Node
        (Model  => Model,
         Parent => Dir,
         Kind   => File_Node,
         File   => File,
         Name   => File.Display_Base_Name,
         Add_Dummy => Lang /= null);
   end Create_Or_Reuse_File;

   -------------------------
   -- Append_Runtime_Info --
   -------------------------

   procedure Append_Runtime_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter)
   is
      Dir, It : Gtk_Tree_Iter;
      pragma Unreferenced (It);
      Previous : Virtual_File;
   begin
      if not Has_Dummy_Iter (Model, Node) then
         --  already added
         return;
      end if;

      declare
         Files : constant File_Array :=
           Get_Registry (Kernel).Environment.Predefined_Source_Files;
      begin
         for F in Files'Range loop
            --  minor optimization to reuse previous dir if possible
            if Previous /= Files (F).Dir then
               Previous := Files (F).Dir;
               Dir := Create_Or_Reuse_Node
                 (Model  => Model,
                  Parent => Node,
                  Kind   => Directory_Node,
                  File   => Previous,
                  Name   => Previous.Display_Full_Name);
            end if;

            Append_File
              (Kernel  => Kernel,
               Model   => Model,
               Base    => Dir,
               File    => Files (F));
         end loop;
      end;
   end Append_Runtime_Info;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category
     (Category : Language_Category) return Language_Category is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      if Category in Dependency_Category
        or else Category in Construct_Category
        or else Category = Cat_Representation_Clause
        or else Category = Cat_Local_Variable
      then
         return Cat_Unknown;

         --  All subprograms are grouped together

      elsif Category in Subprogram_Explorer_Category then
         return Cat_Procedure;

      elsif Category in Type_Category then
         return Cat_Type;

      end if;

      return Category;
   end Filter_Category;

   --------------
   -- Dnd_Data --
   --------------

   overriding function Dnd_Data
     (Child : access MDI_Explorer_Child_Record; Copy : Boolean)
      return Gtkada.MDI.MDI_Child
   is
      C : MDI_Child;
   begin
      if Child.Dnd_From_File = GNATCOLL.VFS.No_File then
         --  So that we can move the explorer itself
         return MDI_Child (Child);

      else
         if Copy then
            C := Find_MDI_Child_By_Name
              (Get_MDI (Child.Kernel),
               Display_Full_Name (Child.Dnd_From_File));
         end if;

         if Copy and then C /= null then
            return Dnd_Data (C, Copy => True);
         else
            Open_File_Editor
              (Child.Kernel,
               Child.Dnd_From_File,
               Project => Child.Dnd_From_Project,
               Line    => 0,
               Column  => 0);
         end if;

         return Get_Focus_Child (Get_MDI (Child.Kernel));
      end if;
   end Dnd_Data;

   -------------------------
   -- Child_Drag_Finished --
   -------------------------

   overriding procedure Child_Drag_Finished
     (Child : access MDI_Explorer_Child_Record) is
   begin
      --  So that we can also move the explorer itself
      Child.Dnd_From_File := GNATCOLL.VFS.No_File;
      Child.Dnd_From_Project := GNATCOLL.Projects.No_Project;
   end Child_Drag_Finished;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Kernel    : Kernel_Handle;
      Child     : access MDI_Explorer_Child_Record'Class;
      Tree      : access Gtk_Tree_View_Record'Class;
      Model     : Gtk_Tree_Store;
      Event     : Gdk_Event_Button;
      Add_Dummy : Boolean) return Boolean
   is
      Iter         : Gtk_Tree_Iter;  --  applies to Model
      Path         : Gtk_Tree_Path;
      Filter_Path  : Gtk_Tree_Path;
      Line, Column : Gint;
      Project      : Project_Type;
      File         : Virtual_File;
      Col          : Gtk_Tree_View_Column;
      Rect         : Gdk_Rectangle;
      Cell_X, Cell_Y : Gint;
      Row_Found      : Boolean;
   begin
      if Event.Button = 1 then
         declare
            Filter_M     : constant Gtk_Tree_Model := Get_Model (Tree);
            Filter_Iter  : Gtk_Tree_Iter;  --  applies to Filter_M
         begin
            Filter_Iter := Find_Iter_For_Event (Tree, Event);
            if Filter_Iter = Null_Iter then
               return False;
            end if;

            if Filter_M /= +Model then
               Convert_Iter_To_Child_Iter
                 (Gtk_Tree_Model_Filter'(-Filter_M),
                  Child_Iter  => Iter,
                  Filter_Iter => Filter_Iter);
            else
               Iter := Filter_Iter;
            end if;
         end;

         if Event.The_Type /= Button_Release then
            --  Set cursor to pointed position before open menu, etc
            Path := Get_Path (Model, Iter);
            Set_Cursor (Tree, Path, null, False);
            Path_Free (Path);
         end if;

         case Get_Node_Type (Model, Iter) is
            when Directory_Node_Types
               | Project_Node_Types
               | Category_Node
               | Runtime_Node =>
               Cancel_Child_Drag (Child);

               if Event.The_Type = Gdk_2button_Press then
                  declare
                     Path    : Gtk_Tree_Path;
                     Ignore  : Boolean;
                     pragma Unreferenced (Ignore);
                  begin
                     Path := Get_Path (Model, Iter);

                     if Row_Expanded (Tree, Path) then
                        Ignore := Collapse_Row (Tree, Path);

                     else
                        if Add_Dummy then
                           Append_Dummy_Iter (Model, Iter);
                        end if;
                        Ignore := Expand_Row (Tree, Path, False);
                     end if;

                     Path_Free (Path);
                  end;
               end if;

               return False;

            when File_Node =>
               File    := Get_File (Model, Iter, File_Column);
               Project := Get_Project_From_Node
                 (Model, Kernel, Iter, Importing => False);

               if Event.The_Type = Gdk_2button_Press
                 or else Event.The_Type = Gdk_3button_Press
               then
                  Cancel_Child_Drag (Child);
                  Open_File_Editor
                    (Kernel,
                     File,
                     Project => Project,
                     Line    => 0,
                     Column  => 0);
                  return True;

               elsif Event.The_Type = Button_Press then

                  --  Did the user click on the expander, or on the file name?

                  Get_Path_At_Pos
                    (Tree,
                     Gint (Event.X),
                     Gint (Event.Y),
                     Filter_Path,
                     Col,
                     Cell_X,
                     Cell_Y,
                     Row_Found);
                  Tree.Get_Cell_Area
                    (Path   => Filter_Path,
                     Column => Col,
                     Rect   => Rect);
                  Path_Free (Filter_Path);
                  if Cell_X < Rect.X or else Cell_X > Rect.X + Rect.Width then
                     Cancel_Child_Drag (Child);
                     return False;
                  end if;

                  --  ... he clicked on the file name

                  declare
                     use type Gtk.Target_List.Gtk_Target_List;
                     X : constant Gtk.Target_List.Gtk_Target_List
                       := Gtk.Dnd.Source_Get_Target_List (Tree);
                  begin
                     --  If Tree provides drag&drop source, then use it
                     --  instead of MDI drag&drop
                     if not X.Is_Null then
                        Cancel_Child_Drag (Child);
                        return False;
                     end if;
                  end;

                  --  Drag-and-drop does not work on floating MDI children

                  if Get_State (Child) /= Gtkada.MDI.Floating then
                     Child.Kernel        := Kernel;
                     Child.Dnd_From_File := File;
                     Child.Dnd_From_Project := Project;

                     Child_Drag_Begin
                       (Child, Event,
                        Areas => Central_Only);  --  editors
                  end if;
                  return False;

               else
                  Cancel_Child_Drag (Child);
               end if;

            when Entity_Node =>
               Cancel_Child_Drag (Child);

               if Event.The_Type = Button_Release then
                  Line := Get_Int (Model, Iter, Line_Column);
                  Column := Get_Int (Model, Iter, Column_Column);
                  File   := Get_File (Model, Iter, File_Column);
                  Project := Get_Project_From_Node
                    (Model, Kernel, Iter, Importing => False);

                  Open_File_Editor
                    (Kernel,
                     File,
                     Project => Project,
                     Line    => Natural (Line),
                     Column  => Visible_Column_Type (Column));
               end if;
               return False;

            when Dummy_Node =>
               return False;
         end case;
      end if;

      return False;
   end On_Button_Press;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Kernel : Kernel_Handle;
      Tree   : access Gtk_Tree_View_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      use type Gdk.Types.Gdk_Key_Type;

      Iter         : Gtk_Tree_Iter;
      Line, Column : Gint;
      Model        : Gtk_Tree_Model;
      File         : Virtual_File;
      Project      : Project_Type;

   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);

      if Iter = Null_Iter then
         return False;
      end if;

      if Get_Key_Val (Event) = GDK_Return then
         case Node_Types'Val
           (Integer (Get_Int (Model, Iter, Node_Type_Column))) is

         when File_Node =>
            File    := Get_File (Gtk_Tree_Store'(-Model), Iter, File_Column);
            Project := Get_Project_From_Node
              (Gtk_Tree_Store'(-Model), Kernel, Iter, Importing => False);

            Open_File_Editor
              (Kernel,
               File,
               Project => Project,
               Line    => 0,
               Column  => 0);

         when Entity_Node =>
            Line   := Get_Int (Model, Iter, Line_Column);
            Column := Get_Int (Model, Iter, Column_Column);
            File   := Get_File (Gtk_Tree_Store'(-Model), Iter, File_Column);
            Project := Get_Project_From_Node
              (Gtk_Tree_Store'(-Model), Kernel, Iter, Importing => False);

            Open_File_Editor
              (Kernel,
               File,
               Project => Project,
               Line    => Natural (Line),
               Column  => Visible_Column_Type (Column));

         when others =>
            null;
         end case;
      end if;

      return False;
   end On_Key_Press;

   -------------------
   -- Get_Node_Type --
   -------------------

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types is
   begin
      return
        Node_Types'Val
          (Integer (Get_Int (Model, Node, Node_Type_Column)));
   end Get_Node_Type;

   -------------------
   -- Set_Node_Type --
   -------------------

   procedure Set_Node_Type
     (Model    : Gtk_Tree_Store;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean) is
   begin
      Set (Model, Node, Node_Type_Column, Gint (Node_Types'Pos (N_Type)));

      if N_Type not in Category_Node .. Entity_Node then
         Set (Model, Node, Icon_Column,
              Stock_For_Node (N_Type, Expanded => Expanded));
      end if;
   end Set_Node_Type;

   -------------------
   -- Get_Base_Name --
   -------------------

   function Get_Base_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Filesystem_String is
   begin
      return Get_File (Model, Node, File_Column).Base_Name;
   end Get_Base_Name;

   -----------------------
   -- Get_Absolute_Name --
   -----------------------

   function Get_Absolute_Name
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Virtual_File is
   begin
      return Get_File (Model, Node, File_Column);
   end Get_Absolute_Name;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File is
   begin
      return Get_File (Model, Node, File_Column);
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Virtual_File
   is
      F : constant Virtual_File := Get_File (Model, Node, File_Column);
   begin
      if F = GNATCOLL.VFS.No_File then
         return F;
      end if;

      if Get_Node_Type (Model, Node) = Directory_Node then
         return F;
      else
         return F.Get_Parent;
      end if;
   end Get_Directory_From_Node;

   ---------------------------
   -- Get_Project_From_Node --
   ---------------------------

   function Get_Project_From_Node
     (Model     : Gtk_Tree_Store;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Project_Type
   is
      Parent_Iter : Gtk_Tree_Iter;
      Node_Type   : Node_Types;
      Project     : Project_Type;
   begin
      if Importing then
         Parent_Iter := Parent (Model, Node);

         if Parent_Iter = Null_Iter then
            return Get_Project (Kernel);
         end if;

      else
         Parent_Iter := Node;
      end if;

      while Parent_Iter /= Null_Iter loop
         Node_Type := Get_Node_Type (Model, Parent_Iter);

         exit when Node_Type in Project_Node_Types;

         Parent_Iter := Parent (Model, Parent_Iter);
      end loop;

      if Parent_Iter /= Null_Iter then
         declare
            N : constant Virtual_File :=
              Get_File (Model, Parent_Iter, File_Column);
         begin
            Project := Get_Registry (Kernel).Tree.Project_From_Path (N);
         end;

      else
         --  Should we fall back on Get_Project_From_File ?
         Project := No_Project;
      end if;

      return Project;

   exception
      when E : others =>
         Trace (Me, E);
         return No_Project;
   end Get_Project_From_Node;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Context_Factory
     (Context : in out Selection_Context;
      Kernel  : Kernel_Handle;
      Model   : Gtk_Tree_Store;
      Iter    : Gtk_Tree_Iter)
   is
      function Entity_Base (Name : String) return String;
      --  Return the "basename" for the entity, ie convert "parent.name" to
      --  "name", in the case of Ada parent packages.
      --  ??? Should this be done by the parser itself

      -----------------
      -- Entity_Base --
      -----------------

      function Entity_Base (Name : String) return String is
      begin
         --  ??? Should use standard UTF8 subprogams
         for C in reverse Name'Range loop
            if Name (C) = '.' then
               return Name (C + 1 .. Name'Last);
            end if;
         end loop;
         return Name;
      end Entity_Base;

      Node_Type : Node_Types;
      L         : Integer := 0;

   begin
      if Iter /= Null_Iter then
         Node_Type := Get_Node_Type (Model, Iter);
      else
         return;
      end if;

      if Node_Type = Entity_Node then
         Set_Entity_Information
           (Context       => Context,
            Entity_Name   =>
              Entity_Base (Get_String (Model, Iter, Entity_Base_Column)),
            Entity_Column => Visible_Column_Type
              (Get_Int (Model, Iter, Column_Column)));
         L := Integer (Get_Int (Model, Iter, Line_Column));
      end if;

      if Node_Type in Project_Node_Types then
         Set_File_Information
           (Context           => Context,
            Project           =>
              Get_Project_From_Node (Model, Kernel, Iter, False),
            Importing_Project =>
              Get_Project_From_Node (Model, Kernel, Iter, True));

      else
         Set_File_Information
           (Context      => Context,
            Files        => (1 => Get_File_From_Node (Model, Iter)),
            Project      =>
              Get_Project_From_Node (Model, Kernel, Iter, False),
            Importing_Project =>
              Get_Project_From_Node (Model, Kernel, Iter, True),
            Line         => L);
      end if;
   end Context_Factory;

end Project_Explorers_Common;

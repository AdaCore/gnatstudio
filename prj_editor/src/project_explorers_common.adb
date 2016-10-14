------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;

with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Glib.Convert;              use Glib.Convert;
with Glib.Values;
with Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Target_List;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;

with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GUI_Utils;                 use GUI_Utils;
with Language.Icons;            use Language.Icons;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;

package body Project_Explorers_Common is

   Me : constant Trace_Handle := Create ("Project_Explorers_Common");

   procedure Add_Column_Name
     (Name    : String;
      Columns : in out Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array;
      Last    : in out Gint);
   --  Increase Last and set Name value and column index on Last's position.

   procedure Add_Column_File
     (File    : Virtual_File;
      Columns : in out Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array;
      Last    : in out Gint);
   --  Increase Last and set File value and column index on Last's position.

   procedure Add_Column_Type_Icon
     (Kind     : Node_Types;
      Expanded : Boolean;
      Columns  : in out Glib.Gint_Array;
      Values   : in out Glib.Values.GValue_Array;
      Last     : in out Gint);
   --  Increase Last and set Kind value and column index on Last's position.
   --  If Kind not in Category_Node .. Entity_Node then add data for icon.

   procedure Add_Column_Icon
     (Name     : String;
      Columns  : in out Glib.Gint_Array;
      Values   : in out Glib.Values.GValue_Array;
      Last     : in out Gint);
   --  Increase Last and set Icon value and column index on Last's position.

   procedure Set
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Name     : String;
      Kind     : Node_Types;
      Expanded : Boolean;
      File     : Virtual_File);
   --  Set values of columns

   ---------------------
   -- Add_Column_File --
   ---------------------

   procedure Add_Column_File
     (File    : Virtual_File;
      Columns : in out Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array;
      Last    : in out Gint) is
   begin
      Last := Last + 1;
      Columns (Integer (Last)) := File_Column;
      Glib.Values.Init (Values (Last), Get_Virtual_File_Type);
      Set_File (Values (Last), File);
   end Add_Column_File;

   ---------------------
   -- Add_Column_Icon --
   ---------------------

   procedure Add_Column_Icon
     (Name     : String;
      Columns  : in out Glib.Gint_Array;
      Values   : in out Glib.Values.GValue_Array;
      Last     : in out Gint) is
   begin
      Last := Last + 1;
      Columns (Integer (Last)) := Icon_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);
      Glib.Values.Set_String (Values (Last), Name);
   end Add_Column_Icon;

   ---------------------
   -- Add_Column_Name --
   ---------------------

   procedure Add_Column_Name
     (Name    : String;
      Columns : in out Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array;
      Last    : in out Gint) is
   begin
      Last := Last + 1;
      Columns (Integer (Last)) := Display_Name_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);
      Glib.Values.Set_String (Values (Last), Name);
   end Add_Column_Name;

   --------------------------
   -- Add_Column_Type_Icon --
   --------------------------

   procedure Add_Column_Type_Icon
     (Kind     : Node_Types;
      Expanded : Boolean;
      Columns  : in out Glib.Gint_Array;
      Values   : in out Glib.Values.GValue_Array;
      Last     : in out Gint) is
   begin
      Last := Last + 1;
      Columns (Integer (Last)) := Node_Type_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
      Glib.Values.Set_Int (Values (Last), Gint (Node_Types'Pos (Kind)));
      Add_Column_Icon
        (Stock_For_Node (Kind, Expanded => Expanded),
         Columns, Values, Last);
   end Add_Column_Type_Icon;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column          => GType_String,
         File_Column          => Get_Virtual_File_Type,
         Display_Name_Column  => GType_String,
         Node_Type_Column     => GType_Int);
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
      end case;
   end Stock_For_Node;

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
         Str : constant Cst_String_Access := Get (Construct.Info.Name);
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
      if Show_Profiles and then Construct.Info.Profile /= No_Symbol then
         return Name & " <span foreground=""#A0A0A0"">"
           & Escape_Text
              (Reduce (Get (Construct.Info.Profile).all, Max_Profile_Length))
           & "</span>";
      else
         return Name;
      end if;
   end Entity_Name_Of;

   --------------------
   -- Entity_Icon_Of --
   --------------------

   function Entity_Icon_Of
     (Construct : Construct_Information) return String
     is (Entity_Icon_Of (Construct.Info));

   function Entity_Icon_Of
     (Construct : Simple_Construct_Information) return String
   is
   begin
      return Stock_From_Category
        (Is_Declaration => Construct.Is_Declaration,
         Visibility     => Construct.Visibility,
         Category       => Construct.Category);
   end Entity_Icon_Of;
   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      Model.Append (Iter => Iter, Parent => Parent);
      Set (Model, Iter, Name, Kind, False, File);
      return Iter;
   end Create_Node;

   --------------------------
   -- Create_Or_Reuse_Node --
   --------------------------

   function Create_Or_Reuse_Node
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File) return Gtk_Tree_Iter
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

      return Create_Node (Model, Parent, Kind, Name, File);
   end Create_Or_Reuse_Node;

   --------------------------
   -- Create_Or_Reuse_File --
   --------------------------

   function Create_Or_Reuse_File
     (Model  : Gtk_Tree_Store;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File) return Gtk_Tree_Iter is
   begin
      return Create_Or_Reuse_Node
        (Model  => Model,
         Parent => Dir,
         Kind   => File_Node,
         File   => File,
         Name   => File.Display_Base_Name);
   end Create_Or_Reuse_File;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Model  : Gtk_Tree_Store;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File) return Gtk_Tree_Iter is
   begin
      return Create_Node
        (Model  => Model,
         Parent => Dir,
         Kind   => File_Node,
         File   => File,
         Name   => File.Display_Base_Name);
   end Create_File;

   -------------------------
   -- Append_Runtime_Info --
   -------------------------

   procedure Append_Runtime_Info
     (Kernel    : Kernel_Handle;
      Model     : Gtk_Tree_Store;
      Node      : Gtk_Tree_Iter)
   is
      Dir, Dummy : Gtk_Tree_Iter;
      Previous : Virtual_File;
      Files    : constant File_Array :=
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

         Dummy := Create_File (Model, Dir, Files (F));
      end loop;
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
            Open_File_Action_Hook.Run
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
      Tree      : not null access Tree_View_Record'Class;
      Event     : Gdk_Event_Button) return Boolean
   is
      Iter         : Gtk_Tree_Iter;  --  applies to Model
      Path         : Gtk_Tree_Path;
      Filter_Path  : Gtk_Tree_Path;
      Project      : Project_Type;
      File         : Virtual_File;
      Col          : Gtk_Tree_View_Column;
      Rect         : Gdk_Rectangle;
      Cell_X, Cell_Y : Gint;
      Row_Found      : Boolean;
   begin
      if Event.Button = 1 then
         declare
            Filter_Iter  : Gtk_Tree_Iter;  --  applies to Filter_M
         begin
            Filter_Iter := Find_Iter_For_Event (Tree, Event);
            if Filter_Iter = Null_Iter then
               return False;
            end if;

            Iter := Tree.Convert_To_Store_Iter (Filter_Iter);
         end;

         if Event.The_Type /= Button_Release then
            --  Set cursor to pointed position before open menu, etc
            Path := Tree.Get_Filter_Path_For_Store_Iter (Iter);
            Set_Cursor (Tree, Path, null, False);
            Path_Free (Path);
         end if;

         case Get_Node_Type (Tree.Model, Iter) is
            when Directory_Node_Types
               | Project_Node_Types
               | Runtime_Node =>

               Cancel_Child_Drag (Child);

               if Event.The_Type = Gdk_2button_Press then
                  declare
                     Path    : Gtk_Tree_Path;
                     Ignore  : Boolean;
                     pragma Unreferenced (Ignore);
                  begin
                     Path := Tree.Get_Filter_Path_For_Store_Iter (Iter);

                     if Row_Expanded (Tree, Path) then
                        Ignore := Collapse_Row (Tree, Path);
                     else
                        Ignore := Expand_Row (Tree, Path, False);
                     end if;

                     Path_Free (Path);
                  end;
               end if;
               return False;

            when File_Node =>
               File    := Get_File (Tree.Model, Iter, File_Column);
               Project := Get_Project_From_Node
                 (Tree.Model, Kernel, Iter, Importing => False);

               if Event.The_Type = Gdk_2button_Press
                 or else Event.The_Type = Gdk_3button_Press
               then
                  Cancel_Child_Drag (Child);
                  Open_File_Action_Hook.Run
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
         end case;
      end if;

      return False;
   end On_Button_Press;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Kernel : Kernel_Handle;
      Tree   : not null access Tree_View_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      use type Gdk.Types.Gdk_Key_Type;

      Iter         : Gtk_Tree_Iter;
      Model        : Gtk_Tree_Model;
      File         : Virtual_File;
      Project      : Project_Type;

   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);
      Iter := Tree.Convert_To_Store_Iter (Iter);

      if Iter = Null_Iter then
         return False;
      end if;

      if Get_Key_Val (Event) = GDK_Return then
         case Get_Node_Type (Tree.Model, Iter) is
         when File_Node =>
            File    := Get_File_From_Node (Tree.Model, Iter);
            Project := Get_Project_From_Node
              (Tree.Model, Kernel, Iter, Importing => False);

            Open_File_Action_Hook.Run
              (Kernel,
               File,
               Project => Project,
               Line    => 0,
               Column  => 0);

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
      Expanded : Boolean)
   is
      Columns : Glib.Gint_Array (1 .. 2);
      Values  : Glib.Values.GValue_Array (1 .. 2);
      Last    : Gint := 0;

   begin
      Add_Column_Type_Icon (N_Type, Expanded, Columns, Values, Last);
      Set (Model, Node, Columns (1 .. Integer (Last)), Values (1 .. Last));

      for Index in 1 .. Last loop
         Glib.Values.Unset (Values (Index));
      end loop;
   end Set_Node_Type;

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
      elsif Get_Node_Type (Model, Node) = Directory_Node then
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
      Node_Type : Node_Types;
   begin
      if Iter = Null_Iter then
         return;
      end if;

      Node_Type := Get_Node_Type (Model, Iter);
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
            Line         => 0);
      end if;
   end Context_Factory;

   ---------
   -- Set --
   ---------

   procedure Set
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Name     : String;
      Kind     : Node_Types;
      Expanded : Boolean;
      File     : Virtual_File)
   is
      Columns : Glib.Gint_Array (1 .. 4);
      Values  : Glib.Values.GValue_Array (1 .. 4);
      Last    : Gint := 0;
   begin
      Add_Column_Type_Icon (Kind, Expanded, Columns, Values, Last);
      Add_Column_Name (Name, Columns, Values, Last);
      Add_Column_File (File, Columns, Values, Last);

      Set (Model, Iter, Columns (1 .. Integer (Last)), Values (1 .. Last));

      for Index in 1 .. Last loop
         Glib.Values.Unset (Values (Index));
      end loop;
   end Set;

end Project_Explorers_Common;

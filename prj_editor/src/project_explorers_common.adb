------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Symbols;      use GNATCOLL.Symbols;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS.GtkAda;   use GNATCOLL.VFS.GtkAda;

with Gdk.Drag_Contexts;     use Gdk.Drag_Contexts;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Gdk.Types.Keysyms;     use Gdk.Types.Keysyms;
with Glib.Convert;          use Glib.Convert;
with Gdk.Dnd;               use Gdk.Dnd;
with Gtk.Dnd;               use Gtk.Dnd;
with Gtk.Label;             use Gtk.Label;
with Gtk.Selection_Data;    use Gtk.Selection_Data;
with Gtk.Target_List;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;

with File_Utils;
with GPS.Kernel.Contexts;   use GPS.Kernel.Contexts;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;    use GPS.Kernel.Project;
with GUI_Utils;             use GUI_Utils;
with Language.Icons;        use Language.Icons;
with Projects;              use Projects;
with String_Utils;          use String_Utils;
with URIs;

package body Project_Explorers_Common is

   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.EXPLORERS_COMMON");

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
     (Kind      : Node_Types;
      Expanded  : Boolean;
      Columns   : in out Glib.Gint_Array;
      Values    : in out Glib.Values.GValue_Array;
      Last      : in out Gint;
      Icon_Name : String := "");
   --  Increase Last and set Kind value and column index on Last's position.
   --  If Kind not in Category_Node .. Entity_Node then add data for icon.

   procedure Add_Column_Icon
     (Name     : String;
      Columns  : in out Glib.Gint_Array;
      Values   : in out Glib.Values.GValue_Array;
      Last     : in out Gint);
   --  Increase Last and set Icon value and column index on Last's position.

   procedure Set
     (Model     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Name      : String;
      Kind      : Node_Types;
      Expanded  : Boolean;
      File      : Virtual_File;
      Icon_Name : String := "");
   --  Set values of columns

   function Freeze_Selection
     (Dummy_Selection   : not null access Gtk_Tree_Selection_Record'Class;
      Dummy_Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Dummy_Path        : Gtk.Tree_Model.Gtk_Tree_Path;
      Dummy_Is_Selected : Boolean) return Boolean is (False);

   function Thaw_Selection
     (Dummy_Selection   : not null access Gtk_Tree_Selection_Record'Class;
      Dummy_Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Dummy_Path        : Gtk.Tree_Model.Gtk_Tree_Path;
      Dummy_Is_Selected : Boolean) return Boolean is (True);

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
     (Kind      : Node_Types;
      Expanded  : Boolean;
      Columns   : in out Glib.Gint_Array;
      Values    : in out Glib.Values.GValue_Array;
      Last      : in out Gint;
      Icon_Name : String := "") is
   begin
      Last := Last + 1;
      Columns (Integer (Last)) := Node_Type_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
      Glib.Values.Set_Int (Values (Last), Gint (Node_Types'Pos (Kind)));
      Add_Column_Icon
        ((if Icon_Name /= ""
          then Icon_Name
          else Stock_For_Node (Kind, Expanded => Expanded)),
         Columns, Values, Last);
   end Add_Column_Type_Icon;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column         => GType_String,
         File_Column         => Get_Virtual_File_Type,
         Display_Name_Column => GType_String,
         Node_Type_Column    => GType_Int);
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
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Parent    : Gtk_Tree_Iter;
      Kind      : Node_Types;
      Name      : String;
      File      : Virtual_File;
      Icon_Name : String := "") return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter := Null_Iter;
      M    : constant Gtk_Tree_Store := Self.Model;
   begin
      M.Append (Iter => Iter, Parent => Parent);
      Set (M, Iter, Name, Kind, False, File, Icon_Name);
      return Iter;
   end Create_Node;

   --------------------------
   -- Create_Or_Reuse_Node --
   --------------------------

   function Create_Or_Reuse_Node
     (Self   : not null access Base_Explorer_Tree_Record'Class;
      Parent : Gtk_Tree_Iter;
      Kind   : Node_Types;
      Name   : String;
      File   : Virtual_File) return Gtk_Tree_Iter
   is
      M    : constant Gtk_Tree_Store := Self.Model;
      Iter : Gtk_Tree_Iter := Null_Iter;
      T    : Node_Types;
   begin
      if Parent = Null_Iter then
         Iter := M.Get_Iter_First;
      else
         Iter := M.Children (Parent);
      end if;

      while Iter /= Null_Iter loop
         T := Self.Get_Node_Type (Iter);
         if (T = Kind
             or else (Kind in Project_Node_Types
                      and then T in Project_Node_Types))
           and then Get_File (M, Iter, File_Column) = File
         then
            if T /= Kind then
               Self.Set_Node_Type (Iter, Kind, False);
            end if;

            return Iter;
         end if;
         M.Next (Iter);
      end loop;

      return Create_Node (Self, Parent, Kind, Name, File);
   end Create_Or_Reuse_Node;

   --------------------------
   -- Create_Or_Reuse_File --
   --------------------------

   function Create_Or_Reuse_File
     (Self   : not null access Base_Explorer_Tree_Record'Class;
      Dir    : Gtk_Tree_Iter;
      File   : Virtual_File) return Gtk_Tree_Iter is
   begin
      return Self.Create_Or_Reuse_Node
        (Parent => Dir,
         Kind   => File_Node,
         File   => File,
         Name   => File.Display_Base_Name);
   end Create_Or_Reuse_File;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Dir       : Gtk_Tree_Iter;
      File      : Virtual_File;
      Icon_Name : String := "gps-emblem-file-unmodified") return Gtk_Tree_Iter
   is
   begin
      return Self.Create_Node
        (Parent    => Dir,
         Kind      => File_Node,
         File      => File,
         Name      => File.Display_Base_Name,
         Icon_Name => Icon_Name);
   end Create_File;

   -------------------------
   -- Append_Runtime_Info --
   -------------------------

   procedure Append_Runtime_Info
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Node      : Gtk_Tree_Iter)
   is
      Dir, Dummy : Gtk_Tree_Iter;
      Previous : Virtual_File;
      Files    : constant File_Array :=
        Get_Registry (Self.Kernel).Environment.Predefined_Source_Files;
   begin
      for F in Files'Range loop
         --  minor optimization to reuse previous dir if possible
         if Previous /= Files (F).Dir then
            Previous := Files (F).Dir;
            Dir := Self.Create_Or_Reuse_Node
              (Parent => Node,
               Kind   => Directory_Node,
               File   => Previous,
               Name   => Previous.Display_Full_Name);
         end if;

         Dummy := Self.Create_File (Dir, Files (F));
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
     (Child     : access MDI_Explorer_Child_Record'Class;
      Tree      : not null access Base_Explorer_Tree_Record'Class;
      Event     : Gdk_Event_Button) return Boolean
   is
      Iter           : Gtk_Tree_Iter;  --  applies to Model
      Filter_Path    : Gtk_Tree_Path;
      Project        : Project_Type;
      File           : Virtual_File;
      Col            : Gtk_Tree_View_Column;
      Rect           : Gdk_Rectangle;
      Cell_X, Cell_Y : Gint;
      Row_Found      : Boolean;

   begin
      if Event.Button = 1 then
         declare
            Filter_Iter : Gtk_Tree_Iter;  --  applies to Filter_M
         begin
            Filter_Iter := Find_Iter_For_Event (Tree, Event);
            if Filter_Iter = Null_Iter then
               return False;
            end if;

            Iter := Tree.Convert_To_Store_Iter (Filter_Iter);
         end;

         case Tree.Get_Node_Type (Iter) is
            when Directory_Node_Types
               | Project_Node_Types
               | Runtime_Node =>

               Cancel_Child_Drag (Child);

               if Event.The_Type = Gdk_2button_Press then
                  declare
                     Path  : Gtk_Tree_Path;
                     Dummy : Boolean;
                  begin
                     Path := Tree.Get_Filter_Path_For_Store_Iter (Iter);

                     if Row_Expanded (Tree, Path) then
                        Dummy := Collapse_Row (Tree, Path);
                     else
                        Dummy := Expand_Row (Tree, Path, False);
                     end if;

                     Path_Free (Path);
                  end;
               end if;
               return False;

            when File_Node =>
               File    := Get_File (Tree.Model, Iter, File_Column);
               Project := Tree.Get_Project_From_Node
                 (Iter, Importing => False);

               if Event.The_Type = Gdk_2button_Press
                 or else Event.The_Type = Gdk_3button_Press
               then
                  Cancel_Child_Drag (Child);
                  Open_File_Action_Hook.Run
                    (Tree.Kernel,
                     File,
                     Project => Project,
                     Line    => 0,
                     Column  => 0);
                  return True;

               elsif Event.The_Type = Button_Release then
                  Tree.Get_Selection.Set_Select_Function
                    (Thaw_Selection'Access);

                  Get_Path_At_Pos
                    (Tree,
                     Gint (Event.X),
                     Gint (Event.Y),
                     Filter_Path,
                     Col,
                     Cell_X,
                     Cell_Y,
                     Row_Found);

                  if Tree.Frozen_Selection then
                     Tree.Set_Cursor (Filter_Path, null, False);
                  end if;

                  Tree.Frozen_Selection := False;
                  Path_Free (Filter_Path);

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

                  --  Intercept mouse clicks on selected items so that we can
                  --  drag multiple items without the click selecting only one
                  if Tree.Get_Selection.Path_Is_Selected (Filter_Path) then
                     Tree.Get_Selection.Set_Select_Function
                       (Freeze_Selection'Access);
                     Tree.Frozen_Selection := True;
                  end if;

                  Path_Free (Filter_Path);
                  if Cell_X < Rect.X or else Cell_X > Rect.X + Rect.Width then
                     Cancel_Child_Drag (Child);
                     return False;
                  end if;

                  --  ... he clicked on the file name

                  declare
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
                     Child.Kernel        := Tree.Kernel;
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
     (Tree   : not null access Base_Explorer_Tree_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      use type Gdk.Types.Gdk_Key_Type;
      use Gtk_Tree_Path_List;

      List    : Gtk_Tree_Path_List.Glist;
      G_Iter  : Gtk_Tree_Path_List.Glist;
      Path    : Gtk_Tree_Path;
      Iter    : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Model;
      File    : Virtual_File;
      Project : Project_Type;
   begin
      if Get_Key_Val (Event) /= GDK_Return then
         return False;
      end if;

      Tree.Get_Selection.Get_Selected_Rows (Model, List);
      G_Iter := Gtk_Tree_Path_List.Last (List);

      while G_Iter /= Null_List loop
         Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
         Iter := Gtk.Tree_Model.Get_Iter (Model, Path);
         Iter := Tree.Convert_To_Store_Iter (Iter);

         if Iter /= Null_Iter then
            case Tree.Get_Node_Type (Iter) is
            when File_Node =>
               File := Tree.Get_File_From_Node (Iter);
               Project := Tree.Get_Project_From_Node
                 (Iter, Importing => False);

               Open_File_Action_Hook.Run
                 (Tree.Kernel,
                  File,
                  Project => Project,
                  Line    => 0,
                  Column  => 0);

            when others =>
               null;
            end case;
         end if;

         G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
      end loop;

      Free_Path_List (List);
      return False;
   end On_Key_Press;

   -------------------
   -- Get_Node_Type --
   -------------------

   function Get_Node_Type
     (Model : Gtk_Tree_Store;
      Node  : Gtk_Tree_Iter) return Node_Types is
   begin
      return Node_Types'Val
        (Integer (Get_Int (Model, Node, Node_Type_Column)));
   end Get_Node_Type;

   -------------------
   -- Set_Node_Type --
   -------------------

   procedure Set_Node_Type
     (Self     : not null access Base_Explorer_Tree_Record'Class;
      Node     : Gtk_Tree_Iter;
      N_Type   : Node_Types;
      Expanded : Boolean)
   is
      Columns : Glib.Gint_Array (1 .. 2);
      Values  : Glib.Values.GValue_Array (1 .. 2);
      Last    : Gint := 0;

   begin
      Add_Column_Type_Icon (N_Type, Expanded, Columns, Values, Last);
      Set
        (Self.Model, Node, Columns (1 .. Integer (Last)), Values (1 .. Last));

      for Index in 1 .. Last loop
         Glib.Values.Unset (Values (Index));
      end loop;
   end Set_Node_Type;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Self  : not null access Base_Explorer_Tree_Record'Class;
      Node  : Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File is
   begin
      return Get_File (Self.Model, Node, File_Column);
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Self  : not null access Base_Explorer_Tree_Record'Class;
      Node  : Gtk_Tree_Iter) return Virtual_File
   is
      F : constant Virtual_File := Get_File (Self.Model, Node, File_Column);
   begin
      if F = GNATCOLL.VFS.No_File then
         return F;
      elsif Self.Get_Node_Type (Node) = Directory_Node then
         return F;
      else
         return F.Get_Parent;
      end if;
   end Get_Directory_From_Node;

   ---------------------------
   -- Get_Project_From_Node --
   ---------------------------

   function Get_Project_From_Node
     (Self      : not null access Base_Explorer_Tree_Record'Class;
      Node      : Gtk_Tree_Iter;
      Importing : Boolean) return Project_Type
   is
      M           : constant Gtk_Tree_Store := Self.Model;
      Parent_Iter : Gtk_Tree_Iter;
      Node_Type   : Node_Types;
      Project     : Project_Type;
   begin
      if Importing then
         Parent_Iter := Parent (M, Node);

         if Parent_Iter = Null_Iter then
            return Get_Project (Self.Kernel);
         end if;

      else
         Parent_Iter := Node;
      end if;

      while Parent_Iter /= Null_Iter loop
         Node_Type := Self.Get_Node_Type (Parent_Iter);
         exit when Node_Type in Project_Node_Types;

         Parent_Iter := Parent (M, Parent_Iter);
      end loop;

      if Parent_Iter /= Null_Iter then
         declare
            N : constant Virtual_File :=
              Get_File (M, Parent_Iter, File_Column);
         begin
            Project := Get_Registry (Self.Kernel).Tree.Project_From_Path (N);
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
     (Self    : not null access Base_Explorer_Tree_Record'Class;
      Context : in out Selection_Context)
   is
      use Gtk_Tree_Path_List;

      List              : Gtk_Tree_Path_List.Glist;
      G_Iter            : Gtk_Tree_Path_List.Glist;
      Path              : Gtk_Tree_Path;
      Iter              : Gtk_Tree_Iter;
      Model             : Gtk_Tree_Model;
      File              : Virtual_File;
      Files             : File_Array_Access :=
        new File_Array'(Empty_File_Array);
      Project           : Project_Type;
      Importing_Project : Project_Type;
      Node_Type         : Node_Types;
   begin
      Self.Get_Selection.Get_Selected_Rows (Model, List);
      G_Iter := Gtk_Tree_Path_List.Last (List);

      while G_Iter /= Null_List loop
         Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
         Iter := Gtk.Tree_Model.Get_Iter (Model, Path);
         Iter := Self.Convert_To_Store_Iter (Iter);

         if Iter /= Null_Iter then
            Node_Type := Self.Get_Node_Type (Iter);

            Project := Self.Get_Project_From_Node (Iter, False);
            Importing_Project := Self.Get_Project_From_Node (Iter, True);

            if Node_Type not in Project_Node_Types then
               File := Self.Get_File_From_Node (Iter);
               GNATCOLL.VFS.Append (Files, File);
            end if;
         end if;

         G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
      end loop;

      Free_Path_List (List);

      Set_File_Information
        (Context           => Context,
         Files             => Files.all,
         Project           => Project,
         Importing_Project => Importing_Project,
         Line              => 0);
      Unchecked_Free (Files);
   end Context_Factory;

   ---------
   -- Set --
   ---------

   procedure Set
     (Model     : Gtk_Tree_Store;
      Iter      : Gtk_Tree_Iter;
      Name      : String;
      Kind      : Node_Types;
      Expanded  : Boolean;
      File      : Virtual_File;
      Icon_Name : String := "")
   is
      Columns : Glib.Gint_Array (1 .. 4);
      Values  : Glib.Values.GValue_Array (1 .. 4);
      Last    : Gint := 0;
   begin
      Add_Column_Type_Icon (Kind, Expanded, Columns, Values, Last, Icon_Name);
      Add_Column_Name (Name, Columns, Values, Last);
      Add_Column_File (File, Columns, Values, Last);

      Set (Model, Iter, Columns (1 .. Integer (Last)), Values (1 .. Last));

      for Index in 1 .. Last loop
         Glib.Values.Unset (Values (Index));
      end loop;
   end Set;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Self     : not null access Explorer_Tooltip_Handler;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Filter_Iter, Iter  : Gtk_Tree_Iter;
      Node_Type          : Node_Types;
      File               : Virtual_File;
      Area               : Gdk_Rectangle;
      Label              : Gtk_Label;
   begin
      Initialize_Tooltips (Self.Tree, X, Y, Area, Filter_Iter);
      Iter := Self.Tree.Convert_To_Store_Iter (Filter_Iter);

      if Iter /= Null_Iter then
         Self.Set_Tip_Area (Area);
         Node_Type := Self.Tree.Get_Node_Type (Iter);

         case Node_Type is
            when Project_Node_Types =>
               --  Project or extended project full pathname
               File := Get_File (Self.Tree.Model, Iter, File_Column);
               Gtk_New (Label, File.Display_Full_Name);

            when Directory_Node_Types =>
               Gtk_New
                 (Label,
                  Get_Tooltip_For_Directory
                    (Kernel    => Self.Tree.Kernel,
                     Directory => Get_File
                       (Self.Tree.Model, Iter, File_Column),
                     Project   => Self.Tree.Get_Project_From_Node
                       (Iter, Importing => False)));
               Label.Set_Use_Markup (True);

            when File_Node =>
               Gtk_New
                 (Label,
                  Get_Tooltip_For_File
                    (Kernel    => Self.Tree.Kernel,
                     File      => Self.Tree.Get_File_From_Node (Iter),
                     Project   => Self.Tree.Get_Project_From_Node
                       (Iter, Importing => False)));
               Label.Set_Use_Markup (True);

            when others =>
               null;
         end case;
      end if;

      return Gtk_Widget (Label);
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self          : On_VCS_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      Files         : GPS.Kernel.File_Sets.Set;
      Props         : VCS_File_Properties)
   is
      pragma Unreferenced (Kernel);

      function On_Node
        (M     : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean;
      --  A file might occur at multiple places.
      --  Even when using a Flat view, it could belong to multiple aggregated
      --  projects (and possibly different VCS engine for each)

      -------------
      -- On_Node --
      -------------

      function On_Node
        (M     : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter) return Boolean
      is
         pragma Unreferenced (M, Path);
      begin
         if Self.Tree.Get_Node_Type (Iter) = File_Node
           and then Files.Contains (Self.Tree.Get_File_From_Node (Iter))
         then
            Self.Tree.Model.Set
              (Iter, Icon_Column,
               UTF8_String'(To_String
                 (Vcs.Get_Display (Props.Status).Icon_Name)));
         end if;
         return False;  --  continue traversing
      end On_Node;

   begin
      --  Need to find all places where this file is displayed in the tree.
      --  ??? Should we cache this => for now no, since there are only a
      --  limited number of files that change status (and this hook is only
      --  run on actual status change).

      Self.Tree.Model.Foreach (On_Node'Unrestricted_Access);
   end Execute;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
      (Self   : not null access Base_Explorer_Tree_Record'Class;
       Row    : Gtk_Tree_Iter) return Node_Id
   is
      P     : constant Gtk_Tree_Path := Self.Model.Get_Path (Row);
      Depth : constant Gint := Get_Depth (P);
   begin
      Path_Free (P);
      return (File  => Self.Get_File_From_Node (Row),
              Depth => Integer (Depth));
   end Get_Id;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Node_Id) return Ada.Containers.Hash_Type is
      use Ada.Containers;
   begin
      return (GNATCOLL.VFS.Full_Name_Hash (Self.File) + Hash_Type (Self.Depth))
         mod Hash_Type'Last;
   end Hash;

   -------------------
   -- Drag_Data_Get --
   -------------------

   procedure Drag_Data_Get
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is

      Tree        : constant Base_Explorer_Tree := Base_Explorer_Tree (Object);
      Model       : Gtk_Tree_Model;
      M           : constant Gtk_Tree_Store := Tree.Model;
      List        : Gtk_Tree_Path_List.Glist;
      G_Iter      : Gtk_Tree_Path_List.Glist;
      Path        : Gtk_Tree_Path;
      Iter        : Gtk_Tree_Iter;
      File        : Virtual_File;
      Data_String : Unbounded_String;
      Data        : constant Gtk.Selection_Data.Gtk_Selection_Data :=
        From_Object (Get_Address (Nth (Args, 2)));
      use Gtk_Tree_Path_List;

   begin
      Tree.Get_Selection.Get_Selected_Rows (Model, List);
      G_Iter := Gtk_Tree_Path_List.Last (List);

      while G_Iter /= Null_List loop
         Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
         Iter := Gtk.Tree_Model.Get_Iter (Model, Path);
         Iter := Tree.Convert_To_Store_Iter (Iter);

         if Iter /= Null_Iter then
            case Tree.Get_Node_Type (Iter) is
            when File_Node =>
               File := Get_File (M, Iter, File_Column);
               begin
                  if not File.Is_Readable then
                     Kernel.Get_Messages_Window.Insert_Error
                       ("File """ & (+(File.Base_Name)) &
                          """ is not readable" & ASCII.LF);
                  else
                     Append (Data_String,
                             URIs.Conversions.From_File (+File.Full_Name)
                             & ASCII.LF);
                  end if;

               exception
                  when others =>
                     null;
               end;

            when others =>
               null;
            end case;
         end if;

         G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
      end loop;

      Free_Path_List (List);
      Gtk.Selection_Data.Selection_Data_Set
        (Data, Gtk.Selection_Data.Get_Target (Data), 8,
         To_String (Data_String));
   end Drag_Data_Get;

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      Tree    : constant Base_Explorer_Tree := Base_Explorer_Tree (Object);
      Model   : constant Gtk_Tree_Store := Tree.Model;
      Context : constant Drag_Context :=
                  Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint := Get_Int (Nth (Args, 2));
      Y       : constant Gint := Get_Int (Nth (Args, 3));
      Data    : constant Gtk_Selection_Data :=
                  From_Object (Get_Address (Nth (Args, 4)));
      Time    : constant Guint32 := Guint32 (Get_Uint (Nth (Args, 6)));
      Action  : constant Drag_Action := Get_Selected_Action (Context);
      Iter    : Gtk_Tree_Iter;
      Success : Boolean;
      Refresh : Boolean := False;  -- Do we have at least a success

      procedure Fail (Msg : String);

      ----------
      -- Fail --
      ----------

      procedure Fail (Msg : String) is
      begin
         Kernel.Get_Messages_Window.Insert_Error (Msg & ASCII.LF);
      end Fail;

   begin
      --  Retrieve the destination directory iter
      declare
         Path      : Gtk_Tree_Path;
         Buffer_X  : Gint;
         Buffer_Y  : Gint;
         Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      begin
         Get_Path_At_Pos
           (Tree, X, Y,
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Success);

         if not Success or Path = Null_Gtk_Tree_Path then
            Iter := Null_Iter;
         else
            Iter := Get_Iter (Model, Path);
            Path_Free (Path);
         end if;
      end;

      if Get_Source_Widget (Context) /= Object then
         --  Forward requests from other applications/widgets to common handler
         GPS.Kernel.Modules.UI.Drag_Data_Received (Object, Args, Kernel);
      elsif Iter /= Null_Iter
        and then Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
        and then
          (Action = Action_Copy
           or else Action = Action_Move
           or else Action = Action_Any)
      then
         declare
            Source  : Virtual_File;
            Target  : Virtual_File;
            Node    : constant Virtual_File :=
                        Get_File (Model, Iter, File_Column);
            Dir     : constant Virtual_File := Node.Dir;
            Sources : constant File_Array_Access :=
                        File_Utils.URL_List_To_Files
                          (Get_Data_As_String (Data));
            Src_Dir : Virtual_File;
         begin
            if Sources = null then
               Success := False;
            else
               for Ix in Sources'Range loop
                  Source := Sources (Ix);
                  Target := Dir.Create_From_Dir (Source.Base_Name);

                  if not Dir.Is_Writable then
                     Fail ("Target directory " & (+(Dir.Full_Name)) &
                             " is not writable");
                     Gtk.Dnd.Finish
                       (Context,
                        Success => False,
                        Del     => False,
                        Time    => Time);
                     return;
                  end if;

                  if Source = Target then
                     Success := False;

                  elsif Action = Action_Move
                    or else Action = Action_Any
                  then
                     if not Source.Is_Writable then
                        Fail ("Source " & (+(Source.Base_Name)) &
                                " is not writable");
                     end if;

                     Src_Dir := Sources (Ix).Get_Parent;
                     if Src_Dir = No_File then
                        Fail ("Source directory is unavailable");

                     elsif not Src_Dir.Is_Writable then
                        Fail ("Source directory " & (+(Src_Dir.Full_Name)) &
                                " is not writable");
                     end if;

                     Source.Rename (Target, Success);
                     Refresh := Refresh or else Success;

                     if Success then
                        File_Renamed_Hook.Run (Kernel, Source, Target);
                     end if;
                  else
                     Source.Copy (Target.Full_Name, Success);

                     if Success then
                        File_Saved_Hook.Run (Kernel, Target);
                     end if;
                  end if;
               end loop;
            end if;

            Gtk.Dnd.Finish
              (Context,
               Success => Success,
               Del     => Success and (Action = Action_Move),
               Time    => Time);

            if Refresh then
               Reload_Project_If_Needed (Kernel);
               Recompute_View (Kernel);
            end if;
         end;
      else
         Gtk.Dnd.Finish
           (Context, Success => False, Del => False, Time => Time);
      end if;
   end Drag_Data_Received;

end Project_Explorers_Common;

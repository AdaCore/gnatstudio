--  Runtime dependencies
with Interfaces.C.Strings;
with Text_IO;              use Text_IO;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

--  Dependencies on GtkAda
with Glib;                 use Glib;
with Gdk.Bitmap;           use Gdk.Bitmap;
with Gdk.Color;            use Gdk.Color;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Gtk.Ctree;            use Gtk.Ctree;
with Gtkada.Types;         use Gtkada.Types;
with Gtk.Enums;            use Gtk.Enums;

--  Dependencies on GNAT sources
with Prj;                  use Prj;
with Namet;                use Namet;
with Stringt;              use Stringt;
with Prj.Tree;             use Prj.Tree;
with Prj.Env;              use Prj.Env;
with Types;                use Types;

package body Project_Trees is

   Absolute_Directories : constant Boolean := False;
   --  <preference> True if directires should be displayed as absolute names,
   --  False if they should be relative to the current directory set by the
   --  user.

   Show_Directories : constant Boolean := True;
   --  <preference> Whether directories should be displayed in the tree.
   --  If False, only the projects are shown.


   Number_Of_Columns : constant := 1;
   --  Number of columns in the ctree.

   subtype Tree_Chars_Ptr_Array is Chars_Ptr_Array (1 .. Number_Of_Columns);

   mini_folder_xpm   : aliased Chars_Ptr_Array (0 .. 0);
   mini_ofolder_xpm  : aliased Chars_Ptr_Array (0 .. 0);

   pragma Import (C, mini_folder_xpm, "mini_folder_xpm");
   pragma Import (C, mini_ofolder_xpm, "mini_ofolder_xpm");

   type Node_Types is (Project_Node, Directory_Node);
   --  ??? Would be nice if this was extensible (for instance to include symbol
   --  browsing directly in the tree)

   type User_Data (Node_Type : Node_Types) is record
      case Node_Type is
         when Project_Node =>
            Project : Project_Id;

         when Directory_Node =>
            Directory : String_Id;
      end case;
   end record;

   package Project_Row_Data is new Gtk.Ctree.Row_Data (User_Data);
   use Project_Row_Data;

   function Create_Line_Text (Column1 : String) return Tree_Chars_Ptr_Array;
   --  Create an array of strings suitable for display in the ctree.
   --  Always use this function instead of creating the array yourself, since
   --  this checks that there are as many elements in the array as columns in
   --  the tree

   procedure Add_Node
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node;
      Project : Project_Id);
   --  Add a new project (and its dependencies) in the tree.
   --  Node is the parent of the project in the tree.

   function Get_Parent_Project
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node) return Project_Id;
   --  Return the name of the project that Node belongs to. Note that if Node
   --  is directly associated with a projet, we return the importing project,
   --  note the one associated with Node.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree        : out Project_Tree;
      Columns     : in     Gint;
      Tree_Column : in     Gint := 0) is
   begin
      Tree := new Project_Tree_Record;
      Gtk.Ctree.Initialize (Tree, Number_Of_Columns, Tree_Column);

      Create_From_Xpm_D
        (Tree.Folder_Open_Pixmap,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Tree.Folder_Open_Mask,
         Transparent => Null_Color,
         Data        => mini_ofolder_xpm);
      Create_From_Xpm_D
        (Tree.Folder_Pixmap,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Tree.Folder_Mask,
         Transparent => Null_Color,
         Data        => mini_folder_xpm);

      Set_Line_Style (Tree, Ctree_Lines_Solid);
   end Gtk_New;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node;
      Project : Project_Id)
   is
      N : Gtk_Ctree_Node;
      N2 : Gtk_Ctree_Node := null;
      --  Prj_List : Project_Node_Id := First_With_Clause_Of (Project);
      Prj_List : Project_List := Projects.Table (Project).Imported_Projects;
      Dir : String_List_Id;
      Current_Dir : constant String := String (Get_Current_Dir);
   begin
      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Node,
         Sibling       => null,
         Text          => Create_Line_Text
           (Get_Name_String (Projects.Table (Project).Name)),
         --  (Get_Name_String (Name_Of (Project))),
         Spacing       => 5,
         Pixmap_Closed => Tree.Folder_Pixmap,
         Mask_Closed   => Tree.Folder_Mask,
         Pixmap_Opened => Tree.Folder_Open_Pixmap,
         Mask_Opened   => Tree.Folder_Open_Mask,
         Is_Leaf       => False,
         Expanded      => Node = null);
      Node_Set_Row_Data (Tree, N, (Project_Node, Project));

      --  Should have a special handling for the modified project files, since
      --  some of its settings are overrided.

      --  if Projects.Table (Project).Modifies /= No_Project then
      --     Add_Node (Tree, N, Projects.Table (Project).Modifies);
      --  end if;

      while Prj_List /= Empty_Project_List loop
         --  Add_Node (Tree, N, Project_Node_Of (Prj_List));
         --  Prj_List := Next_With_Clause_Of (Prj_List);
         Add_Node (Tree, N, Project_Lists.Table (Prj_List).Project);
         Prj_List := Project_Lists.Table (Prj_List).Next;
      end loop;

      --  Insert the directories in the project file
      if Show_Directories then
         Dir := Projects.Table (Project).Source_Dirs;
         while Dir /= Nil_String loop
            String_To_Name_Buffer (String_Elements.Table (Dir).Value);

            if Absolute_Directories
              and then not Is_Absolute_Path (Name_Buffer (1 .. Name_Len))
            then
               Name_Buffer
                 (Current_Dir'Length + 1 .. Current_Dir'Length + Name_Len) :=
                 Name_Buffer (1 .. Name_Len);
               Name_Buffer (1 .. Current_Dir'Length) := Current_Dir;
               Name_Len := Current_Dir'Length + Name_Len;
            end if;

            N2 := Insert_Node
              (Ctree         => Tree,
               Parent        => N,
               Sibling       => N2,
               Text          => Create_Line_Text (Name_Buffer (1 .. Name_Len)),
               Spacing       => 5,
               Pixmap_Closed => Null_Pixmap,
               Mask_Closed   => Null_Bitmap,
               Pixmap_Opened => Null_Pixmap,
               Mask_Opened   => Null_Bitmap,
               Is_Leaf       => True,
               Expanded      => True);
            Node_Set_Row_Data
              (Tree, N2, (Directory_Node, String_Elements.Table (Dir).Value));
            Dir := String_Elements.Table (Dir).Next;
         end loop;
      end if;
   end Add_Node;

   ----------
   -- Load --
   ----------

   procedure Load
     (Tree         : access Project_Tree_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id) is
   begin
      pragma Assert
        (Project_View /= No_Project, "Must specify a project view");
      Tree.Project := Project;
      Tree.Current_View := Project_View;
      Freeze (Tree);
      Clear (Tree);
      Add_Node (Tree, null, Project_View);
      Thaw (Tree);
   end Load;

   ----------------------
   -- Create_Line_Text --
   ----------------------

   function Create_Line_Text (Column1 : String) return Tree_Chars_Ptr_Array is
   begin
      return (1 => Interfaces.C.Strings.New_String (Column1));
   end Create_Line_Text;

   ------------------------
   -- Get_Parent_Project --
   ------------------------

   function Get_Parent_Project
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node) return Project_Id
   is
      Parent : Gtk_Ctree_Node;
   begin
      Parent := Row_Get_Parent (Node_Get_Row (Node));
      while Node_Get_Row_Data (Tree, Parent).Node_Type
        /= Project_Node
      loop
         Parent := Row_Get_Parent (Node_Get_Row (Parent));
      end loop;

      return Node_Get_Row_Data (Tree, Parent).Project;
   end Get_Parent_Project;

   --------------------------
   -- Get_Selected_Project --
   --------------------------

   function Get_Selected_Project (Tree : access Project_Tree_Record)
      return Project_Id
   is
      use type Node_List.Glist;
      Selection : Node_List.Glist := Get_Selection (Tree);
   begin
      if Selection /= Node_List.Null_List then
         declare
            N : constant Gtk_Ctree_Node := Node_List.Get_Data (Selection);
            User : constant User_Data := Node_Get_Row_Data (Tree, N);
         begin
            case User.Node_Type is
               when Project_Node =>
                  return User.Project;

               when Directory_Node =>
                  return Get_Parent_Project (Tree, N);
            end case;
         end;
      end if;
      return No_Project;
   end Get_Selected_Project;

   ------------------------------
   -- Get_Selected_Directories --
   ------------------------------

   function Get_Selected_Directories
     (Tree    : access Project_Tree_Record;
      Project : Prj.Project_Id) return Name_Id_Array
   is
      use type Node_List.Glist;
      Selection : Node_List.Glist := Get_Selection (Tree);
   begin
      if Selection /= Node_List.Null_List then
         declare
            N : constant Gtk_Ctree_Node := Node_List.Get_Data (Selection);
            User : constant User_Data   := Node_Get_Row_Data (Tree, N);
         begin
            case User.Node_Type is
               when Project_Node =>
                  return Name_Id_Array' (1 .. 0 => No_Name);

               when Directory_Node =>
                  if Get_Parent_Project (Tree, N) /= Project then
                     return Name_Id_Array' (1 .. 0 => No_Name);
                  else
                     String_To_Name_Buffer (User.Directory);
                     return Name_Id_Array' (1 .. 1 => Name_Find);
                  end if;
            end case;
         end;
      end if;
      return Name_Id_Array' (1 .. 0 => No_Name);
   end Get_Selected_Directories;

end Project_Trees;

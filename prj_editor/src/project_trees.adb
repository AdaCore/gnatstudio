--  Runtime dependencies
with Interfaces.C.Strings;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

--  Dependencies on GtkAda
with Gdk.Bitmap;           use Gdk.Bitmap;
with Gdk.Color;            use Gdk.Color;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Glib;                 use Glib;
with Gtk.Arguments;        use Gtk.Arguments;
with Gtk.Ctree;            use Gtk.Ctree;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.Types;         use Gtkada.Types;

--  Dependencies on GNAT sources
with Prj;                  use Prj;
with Namet;                use Namet;
with Stringt;              use Stringt;
with Prj.Tree;             use Prj.Tree;
with Types;                use Types;

with Prj_API;              use Prj_API;

package body Project_Trees is

   Absolute_Directories : constant Boolean := False;
   --  <preference> True if directories should be displayed as absolute names,
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
            Name    : Name_Id;
            --  We do not keep a pointer to the project_id itself, since this
            --  becomes obsolete as soon as a new project_view is parsed. On
            --  the other hand, the Name_Id is always the same, thus making it
            --  possible to relate nodes from the old tree and nodes from the
            --  new one.

            Up_To_Date : Boolean := False;
            --  Indicates whether the children of this node (imported projects,
            --  directories,...) have already been parsed and added to the
            --  tree. If this is False, then when the node is open, any child
            --  should be removed and the new children should be computed.

         when Directory_Node =>
            Directory : String_Id;
            --  The name of the directory associated with that node
      end case;
   end record;
   --  Information kept with each node in the tree.

   package Project_Row_Data is new Gtk.Ctree.Row_Data (User_Data);
   use Project_Row_Data;

   function Create_Line_Text (Column1 : String) return Tree_Chars_Ptr_Array;
   --  Create an array of strings suitable for display in the ctree.
   --  Always use this function instead of creating the array yourself, since
   --  this checks that there are as many elements in the array as columns in
   --  the tree

   function Add_Project_Node
     (Tree         : access Project_Tree_Record'Class;
      Project      : Project_Id;
      Parent_Node  : Gtk_Ctree_Node := null) return Gtk_Ctree_Node;
   --  Add a new project (and its dependencies) in the tree.
   --  Parent_Node is the parent of the project in the tree. If this is null,
   --  the new node is added at the root level of the tree.
   --  The new node is initially closed, and its contents will only be
   --  initialized when the node is opened by the user.

   procedure Add_Node_Contents
     (Tree         : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node);
   --  Add the imported projects and directories to Project_Node, if they are
   --  not already inserted in the tree.

   procedure Add_Node_Directories
     (Tree : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Project_View : Project_Id);
   --  Add the directories belong to Project_Node (found in Project_View).
   --  Note that this is done unconditionnaly (don't test if they are already
   --  there), and is already done by Add_Node_Contents.

   procedure Add_Dummy_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Add a dummy, invisible, child to Node. This is used to force Tree to
   --  display an expansion box besides Node. The actual children of Node will
   --  be computed on demand when the user expands Node.

   procedure Expand_Tree_Cb
     (Tree : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   function Get_Parent_Project
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node) return Project_Id;
   --  Return the name of the project that Node belongs to. Note that if Node
   --  is directly associated with a projet, we return the importing project,
   --  note the one associated with Node.

   function Get_Selected_Project_Node (Tree : access Project_Tree_Record)
      return Gtk_Ctree_Node;
   --  Return the node that contains the selected directory (or, if the user
   --  selected a project directly, it returns the node of that project itself)

   procedure Update_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Refresh the contents of the Node after the Project_View has
   --  changed. This means that possibly the list of directories has
   --  changed. However, the hierarchy of projects can not change, nor the list
   --  of modified projects

   procedure Select_Directory
     (Tree         : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Directory    : String := "");
   --  Select a specific project, and (if not "") a specific directory
   --  in that project

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

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Widget_Callback.Connect (Tree, "tree_expand", Expand_Tree_Cb'Access);

      --  So that the horizontal scrollbars work correctly.
      Set_Column_Auto_Resize (Tree, 0, True);
   end Gtk_New;

   --------------------
   -- Add_Dummy_Node --
   --------------------

   procedure Add_Dummy_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      N : Gtk_Ctree_Node;
   begin
      --  Add a dummy node
      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Node,
         Sibling       => null,
         Text          => Create_Line_Text ("dummy"),
         Spacing       => 5,
         Pixmap_Closed => Null_Pixmap,
         Mask_Closed   => Null_Bitmap,
         Pixmap_Opened => Null_Pixmap,
         Mask_Opened   => Null_Bitmap,
         Is_Leaf       => True,
         Expanded      => True);
   end Add_Dummy_Node;

   ----------------------
   -- Add_Project_Node --
   ----------------------

   function Add_Project_Node
     (Tree         : access Project_Tree_Record'Class;
      Project      : Project_Id;
      Parent_Node  : Gtk_Ctree_Node := null) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean :=
        Projects.Table (Project).Imported_Projects = Empty_Project_List
        and then (not Show_Directories
                  or else Projects.Table (Project).Source_Dirs = Nil_String);
   begin
      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Create_Line_Text
           (Get_Name_String (Projects.Table (Project).Name)),
         Spacing       => 5,
         Pixmap_Closed => Tree.Folder_Pixmap,
         Mask_Closed   => Tree.Folder_Mask,
         Pixmap_Opened => Tree.Folder_Open_Pixmap,
         Mask_Opened   => Tree.Folder_Open_Mask,
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      Node_Set_Row_Data
        (Tree, N,
         (Project_Node, Projects.Table (Project).Name, Up_To_Date => False));

      if not Is_Leaf then
         Add_Dummy_Node (Tree, N);
      end if;

      return N;
   end Add_Project_Node;

   ----------------------
   -- Select_Directory --
   ----------------------

   procedure Select_Directory
     (Tree         : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Directory    : String := "")
   is
      N : Gtk_Ctree_Node :=
        Row_Get_Children (Node_Get_Row (Project_Node));
   begin
      if Directory = "" then
         Gtk_Select (Tree, Project_Node);

      else
         while N /= null loop
            declare
               D : constant User_Data := Node_Get_Row_Data (Tree, N);
            begin
               if D.Node_Type = Directory_Node then
                  String_To_Name_Buffer (D.Directory);
                  if Name_Buffer (1 .. Name_Len) = Directory then
                     Gtk_Select (Tree, N);
                     return;
                  end if;
               end if;
            end;
            N := Row_Get_Sibling (Node_Get_Row (N));
         end loop;
      end if;
   end Select_Directory;

   -----------------
   -- Update_Node --
   -----------------

   procedure Update_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      Data  : User_Data := Node_Get_Row_Data (Tree, Node);
      N, N2 : Gtk_Ctree_Node;

   begin
      case Data.Node_Type is

         when Project_Node =>
            --  Nodes that are not expanded should remain that way. Their
            --  contents will be computed only when the user tries to expand
            --  them.
            --  However, if the contents has been computed before, we need to
            --  clean things up.

            if not Row_Get_Expanded (Node_Get_Row (Node)) then
               if Data.Up_To_Date then
                  Data.Up_To_Date := False;
                  Node_Set_Row_Data (Tree, Node, Data);

                  N := Row_Get_Children (Node_Get_Row (Node));
                  while N /= null loop
                     N2 := Row_Get_Sibling (Node_Get_Row (N));
                     Remove_Node (Tree, N);
                     N := N2;
                  end loop;

                  Add_Dummy_Node (Tree, Node);
               end if;

            --  Else the node was expanded, we need to replace its contents
            --  with the new one.

            else

               --  First remove old directories

               N := Row_Get_Children (Node_Get_Row (Node));
               while N /= null loop
                  N2 := Row_Get_Sibling (Node_Get_Row (N));

                  if Node_Get_Row_Data (Tree, N).Node_Type = Project_Node then
                     Update_Node (Tree, N);
                  else
                     Remove_Node (Tree, N);
                  end if;

                  N := N2;
               end loop;

               --  Then put the new ones
               Add_Node_Directories
                 (Tree, Node, Get_Project_View_From_Name (Data.Name));
            end if;

         when Directory_Node =>
            null;

      end case;
   end Update_Node;

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Tree : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Node : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
   begin
      Add_Node_Contents (Project_Tree (Tree), Node);
   end Expand_Tree_Cb;

   --------------------------
   -- Add_Node_Directories --
   --------------------------

   procedure Add_Node_Directories
     (Tree : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Project_View : Project_Id)
   is
      N           : Gtk_Ctree_Node := null;
      Dir         : String_List_Id;
      Current_Dir : constant String := String (Get_Current_Dir);
   begin
      --  Insert the directories in the project file
      if Show_Directories then

         --  ??? need a special icon for the object directory

         Dir := Projects.Table (Project_View).Source_Dirs;
         while Dir /= Nil_String loop
            String_To_Name_Buffer (String_Elements.Table (Dir).Value);

            if Absolute_Directories
              and then not Is_Absolute_Path (Name_Buffer (1 .. Name_Len))
            then
               Name_Buffer
                 (Current_Dir'Length + 1 .. Current_Dir'Length + Name_Len)
                 := Name_Buffer (1 .. Name_Len);
               Name_Buffer (1 .. Current_Dir'Length) := Current_Dir;
               Name_Len := Current_Dir'Length + Name_Len;
            end if;

            N := Insert_Node
              (Ctree         => Tree,
               Parent        => Project_Node,
               Sibling       => N,
               Text          =>
                 Create_Line_Text (Name_Buffer (1 .. Name_Len)),
               Spacing       => 5,
               Pixmap_Closed => Null_Pixmap,
               Mask_Closed   => Null_Bitmap,
               Pixmap_Opened => Null_Pixmap,
               Mask_Opened   => Null_Bitmap,
               Is_Leaf       => True,
               Expanded      => True);
            Node_Set_Row_Data
              (Tree, N,
               (Directory_Node, String_Elements.Table (Dir).Value));
            Dir := String_Elements.Table (Dir).Next;
         end loop;
      end if;
   end Add_Node_Directories;

   -----------------------
   -- Add_Node_Contents --
   -----------------------

   procedure Add_Node_Contents
     (Tree         : access Project_Tree_Record'Class;
      Project_Node : Gtk_Ctree_Node)
   is
      Project     : Project_Id;
      Data        : User_Data := Node_Get_Row_Data (Tree, Project_Node);
      Prj_List    : Project_List;
      N           : Gtk_Ctree_Node := null;
   begin
      --  If the node is not already up-to-date

      if not Data.Up_To_Date then

         Project := Get_Project_View_From_Name (Data.Name);

         --  Remove the dummy node
         Remove_Node (Tree, Row_Get_Children (Node_Get_Row (Project_Node)));


         --  ??? Should have a special handling for the modified project files,
         --  ??? since some of its settings are overriden.

         --  if Projects.Table (Project).Modifies /= No_Project then
         --     Add_Node (Tree, N, Projects.Table (Project).Modifies);
         --  end if;

         --  Imported projects

         Prj_List := Projects.Table (Project).Imported_Projects;
         while Prj_List /= Empty_Project_List loop
            N := Add_Project_Node
              (Tree, Project_Lists.Table (Prj_List).Project, Project_Node);
            Prj_List := Project_Lists.Table (Prj_List).Next;
         end loop;

         Add_Node_Directories (Tree, Project_Node, Project);

         Data.Up_To_Date := True;
         Node_Set_Row_Data (Tree, Project_Node, Data);
      end if;
   end Add_Node_Contents;

   ----------
   -- Load --
   ----------

   procedure Load
     (Tree         : access Project_Tree_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id)
   is
      Selected_P   : Gtk_Ctree_Node := null;
      Selected_Dir : String_Id := No_String;
   begin
      pragma Assert
        (Project /= Empty_Node and then Project_View /= No_Project);
      Tree.Current_View := Project_View;

      Freeze (Tree);

      --  If we are displaying a new view of the tree that was there before, we
      --  want to keep the project nodes, and most important their open/close
      --  status, so as to minimize the changes the user sees.

      if Tree.Project = Project then

         --  Save the selection, so that we can restore it later
         Selected_P := Get_Selected_Project_Node (Tree);
         if Selected_P /= null then
            declare
               U : User_Data := Node_Get_Row_Data
                 (Tree, Node_List.Get_Data (Get_Selection (Tree)));
            begin
               if U.Node_Type = Directory_Node then
                  Selected_Dir := U.Directory;
               end if;
            end;
         end if;

         String_To_Name_Buffer (Selected_Dir);
         declare
            D : constant String := Name_Buffer (Name_Buffer'First .. Name_Len);
         begin
            Update_Node (Tree, Node_Nth (Tree, 0));

            --  Restore the selection. Note that this also resets the project
            --  view clist, with the contents of all the files.

            if Selected_P /= null then
               Select_Directory (Tree, Selected_P, D);
            end if;
         end;

      else
         Clear (Tree);
         Tree.Project := Project;
         Expand
           (Tree, Add_Project_Node (Tree, Project_View, Parent_Node => null));
      end if;

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

      return
        Get_Project_View_From_Name (Node_Get_Row_Data (Tree, Parent).Name);
   end Get_Parent_Project;

   -------------------------------
   -- Get_Selected_Project_Node --
   -------------------------------

   function Get_Selected_Project_Node (Tree : access Project_Tree_Record)
      return Gtk_Ctree_Node
   is
      use type Node_List.Glist;
      Selection : Node_List.Glist := Get_Selection (Tree);
      N : Gtk_Ctree_Node;
   begin
      if Selection /= Node_List.Null_List then
         N := Node_List.Get_Data (Selection);
         while N /= null loop
            if Node_Get_Row_Data (Tree, N).Node_Type = Project_Node then
               return N;
            end if;

            N := Row_Get_Parent (Node_Get_Row (N));
         end loop;
      end if;
      return null;
   end Get_Selected_Project_Node;

   --------------------------
   -- Get_Selected_Project --
   --------------------------

   function Get_Selected_Project (Tree : access Project_Tree_Record)
      return Project_Id
   is
      N : Gtk_Ctree_Node := Get_Selected_Project_Node (Tree);
   begin
      if N /= null then
         return Get_Project_View_From_Name
           (Node_Get_Row_Data (Tree, N). Name);
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

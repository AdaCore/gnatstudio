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

with Interfaces.C.Strings;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Unchecked_Conversion;
with System;

with Gdk.Bitmap;           use Gdk.Bitmap;
with Gdk.Color;            use Gdk.Color;
with Gdk.Event;            use Gdk.Event;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gtk.Arguments;        use Gtk.Arguments;
with Gtk.Ctree;            use Gtk.Ctree;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.Types;         use Gtkada.Types;

with Prj;                  use Prj;
with Namet;                use Namet;
with Stringt;              use Stringt;
with Types;                use Types;

with Prj_API;              use Prj_API;
with Pixmaps_IDE;          use Pixmaps_IDE;
with Pixmaps_Prj;          use Pixmaps_Prj;
with Language;             use Language;
with Language.Ada; use Language.Ada;
with Language.C;   use Language.C;
with Language.Cpp; use Language.Cpp;
with Basic_Types;  use Basic_Types;
with String_Utils; use String_Utils;
with Glide_Kernel; use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Editor;  use Glide_Kernel.Editor;
with Variable_Editors; use Variable_Editors;
with GUI_Utils;    use GUI_Utils;

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

   subtype String_Access is Basic_Types.String_Access;

   function To_Chars_Ptr is new Unchecked_Conversion
     (System.Address, Interfaces.C.Strings.chars_ptr);

   subtype Tree_Chars_Ptr_Array is Chars_Ptr_Array (1 .. Number_Of_Columns);

   type User_Data (Node_Type : Node_Types) is record
      Up_To_Date : Boolean := False;
      --  Indicates whether the children of this node (imported projects,
      --  directories,...) have already been parsed and added to the tree. If
      --  this is False, then when the node is open, any child should be
      --  removed and the new children should be computed.

      case Node_Type is
         when Project_Node | Modified_Project_Node =>
            Name    : Name_Id;
            --  We do not keep a pointer to the project_id itself, since this
            --  becomes obsolete as soon as a new project_view is parsed. On
            --  the other hand, the Name_Id is always the same, thus making it
            --  possible to relate nodes from the old tree and nodes from the
            --  new one.

         when Directory_Node =>
            Directory : String_Id;
            --  The name of the directory associated with that node

         when Obj_Directory_Node =>
            null;

         when File_Node =>
            File : String_Id;

         when Category_Node =>
            Subprogram_Spec : Boolean;

         when Entity_Node =>
            Sloc_Start, Sloc_End : Source_Location;

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

   ------------------
   -- Adding nodes --
   ------------------

   function Add_Project_Node
     (Tree         : access Project_Tree_Record'Class;
      Project      : Project_Id;
      Parent_Node  : Gtk_Ctree_Node := null;
      Modified_Project : Boolean := False) return Gtk_Ctree_Node;
   --  Add a new project node in the tree.
   --  Parent_Node is the parent of the project in the tree. If this is null,
   --  the new node is added at the root level of the tree.
   --  The new node is initially closed, and its contents will only be
   --  initialized when the node is opened by the user.

   function Add_Directory_Node
     (Tree             : access Project_Tree_Record'Class;
      Directory        : String;
      Parent_Node      : Gtk_Ctree_Node := null;
      Current_Dir      : String;
      Directory_String : String_Id := No_String;
      Object_Directory : Boolean := False) return Gtk_Ctree_Node;
   --  Add a new directory node in the tree, for Directory.
   --  Current_Dir is used to resolve Directory to an absolute directory if
   --  required.  Directory_String should be specified for source directories
   --  only, and is not required for object directories.

   function Add_File_Node
     (Tree             : access Project_Tree_Record'Class;
      File             : String_Id;
      Parent_Node      : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new file node in the tree, for File

   function Add_Category_Node
     (Tree             : access Project_Tree_Record'Class;
      Category_Name    : String;
      Is_Specification : Boolean;
      Parent_Node      : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new category node in the tree, for Category_Name

   function Add_Entity_Node
     (Tree : access Project_Tree_Record'Class;
      Construct : Construct_Information;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new entity node in the tree, for Entity_Name

   ---------------------
   -- Expanding nodes --
   ---------------------

   procedure Expand_Project_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data);
   --  Expand a project node, ie add children for all the imported projects,
   --  the directories, ...

   procedure Expand_Directory_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data);
   --  Expand a directory node, ie add children for all the files and
   --  subirectories.

   procedure Expand_File_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data);
   --  Expand a file node, ie add children for all the entities defined in the
   --  file.

   procedure Expand_Tree_Cb
     (Tree : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   --------------------
   -- Updating nodes --
   --------------------

   procedure Update_Project_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Recompute the directories for the project.

   procedure Update_Directory_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Recompute the files for the directory. This procedure tries to keep the
   --  existing files if they are in the project view, so as to keep the
   --  expanded status

   ----------------------------
   -- Retrieving information --
   ----------------------------

   function Get_Parent_Project
     (Tree    : access Project_Tree_Record'Class;
      Node    : Gtk_Ctree_Node) return Project_Id;
   --  Return the name of the project that Node belongs to. Note that if Node
   --  is directly associated with a projet, we return the importing project,
   --  note the one associated with Node.

   function Has_Entries (Directory : String) return Boolean;
   --  Return True if Directory contains some subdirectories or files.

   function File_In_Directory
     (Directory : String_Id; File : String_Id) return Boolean;
   --  Return True if File was found in Directory

   procedure Add_Dummy_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Add a dummy, invisible, child to Node. This is used to force Tree to
   --  display an expansion box besides Node. The actual children of Node will
   --  be computed on demand when the user expands Node.

   function Get_File_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String;
   --  Return the name of the file containing Node (or, in case Node is an
   --  Entity_Node, the name of the file that contains the entity).
   --  The full name, including directory, is returned.

   function Get_Directory_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String;
   --  Return the name of the directory to which Node belongs. This returns the
   --  full directory name, relative to the project.
   --  The return strings always ends with a directory separator.

   function Category_Name (Category : Language_Category) return String;
   --  Return the name of the node for Category

   function Get_Selected_Project_Node (Tree : access Project_Tree_Record'Class)
      return Gtk_Ctree_Node;
   --  Return the node that contains the selected directory (or, if the user
   --  selected a project directly, it returns the node of that project itself)

   function Get_File_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String_Id;
   --  Return the file associated with Node (ie the file that contains the
   --  entity for an Entity_Node), or file itself for a File_Node.
   --  No_String is returned for a Directory_Node or Project_Node

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

   procedure Refresh (Kernel : access GObject_Record'Class; Tree : GObject);
   --  Refresh the contents of the tree after the project view has changed.
   --  This procedure tries to keep as many things as possible in the current
   --  state (expanded nodes,...)

   procedure Project_Changed
     (Kernel : access GObject_Record'Class; Tree : GObject);
   --  Called when the project as changed, as opposed to the project view.
   --  This means we need to start up with a completely new tree, no need to
   --  try to keep the current one.

   procedure Node_Selected
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node);
   --  Called when a node is selected.
   --  It provides the standard behavior when an entity is selected (open the
   --  appropriate source editor).

   function Button_Press
     (Tree : access Gtk_Widget_Record'Class; Args : Gtk_Args) return Boolean;
   --  Callback for the "button_press" event

   function Filter_Category (Category : Language_Category)
      return Language_Category;
   --  Return the category to use when an entity is Category.
   --  This is used to group subprograms (procedures and functions together),
   --  or remove unwanted categories (in which case Cat_Unknown is returned).

   function Tree_Contextual_Menu
     (Tree : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Gtk_Menu;
   --  Return the contextual menu to be used for the tree.

   procedure On_Add_Variable (Tree : access Gtk_Widget_Record'Class);
   --  Callback for the "Add variable" contextual menu item

   ---------------------
   -- On_Add_Variable --
   ---------------------

   procedure On_Add_Variable (Tree : access Gtk_Widget_Record'Class) is
      T : Project_Tree := Project_Tree (Tree);
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, T.Kernel, Scenario_Variable_Only => True);
      Show_All (Edit);
   end On_Add_Variable;

   --------------------------
   -- Tree_Contextual_Menu --
   --------------------------

   function Tree_Contextual_Menu
     (Tree : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Gtk_Menu
   is
      pragma Warnings (Off, Event);
      T : Project_Tree := Project_Tree (Tree);
      Item : Gtk_Menu_Item;
   begin
      if T.Contextual_Menu /= null then
         Destroy (T.Contextual_Menu);
      end if;

      Gtk_New (T.Contextual_Menu);

      Gtk_New (Item, Label => "Add Directory");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      Gtk_New (Item, Label => "Change Object Directory");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      Gtk_New (Item, Label => "Remove Directory");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      Gtk_New (Item, Label => "");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      Gtk_New (Item, Label => "Add Variable");
      Append (T.Contextual_Menu, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (On_Add_Variable'Access), T);

      Gtk_New (Item, Label => "Edit Default Switches");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      Gtk_New (Item, Label => "Edit Switches for <file>");
      Set_Sensitive (Item, False);
      Append (T.Contextual_Menu, Item);

      return T.Contextual_Menu;
   end Tree_Contextual_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree        : out Project_Tree;
      Kernel      : access Kernel_Handle_Record'Class)
   is
      procedure Create_Pixmaps
        (Node_Type : Node_Types; Open, Close : Chars_Ptr_Array);
      --  Create the four pixmaps and masks associated with a specific node
      --  type.

      --------------------
      -- Create_Pixmaps --
      --------------------

      procedure Create_Pixmaps
        (Node_Type : Node_Types; Open, Close : Chars_Ptr_Array) is
      begin
         Create_From_Xpm_D
           (Tree.Open_Pixmaps (Node_Type), null, Get_System,
            Tree.Open_Masks (Node_Type), Null_Color, Open);
         Create_From_Xpm_D
           (Tree.Close_Pixmaps (Node_Type), null, Get_System,
            Tree.Close_Masks (Node_Type), Null_Color, Close);
      end Create_Pixmaps;

   begin
      Tree := new Project_Tree_Record;
      Gtk.Ctree.Initialize (Tree, 1, 0);

      Register_Contextual_Menu (Tree, Tree_Contextual_Menu'Access);

      Create_Pixmaps (Project_Node, project_xpm, project_closed_xpm);
      Create_Pixmaps
        (Modified_Project_Node, project_modified_xpm, project_modified_xpm);
      Create_Pixmaps (Directory_Node, mini_ofolder_xpm, mini_folder_xpm);
      Create_Pixmaps
        (Obj_Directory_Node, mini_folder_object_xpm, mini_folder_object_xpm);
      Create_Pixmaps (File_Node, mini_page_xpm, mini_page_xpm);
      Create_Pixmaps (Category_Node, var_xpm, var_xpm);

      Set_Line_Style (Tree, Ctree_Lines_Solid);

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Widget_Callback.Connect (Tree, "tree_expand", Expand_Tree_Cb'Access);

      --  So that the horizontal scrollbars work correctly.
      Set_Column_Auto_Resize (Tree, 0, True);

      --  Automatic update of the tree when the project changes
      Tree.Kernel := Kernel_Handle (Kernel);

      Object_User_Callback.Connect
        (Kernel, "project_view_changed",
         Object_User_Callback.To_Marshaller (Refresh'Access),
         GObject (Tree));
      Object_User_Callback.Connect
        (Kernel, "project_changed",
         Object_User_Callback.To_Marshaller (Project_Changed'Access),
         GObject (Tree));

      Gtkada.Handlers.Return_Callback.Connect
        (Tree, "button_press_event", Button_Press'Access);

      --  Update the tree with the current project
      Refresh (Kernel, GObject (Tree));
   end Gtk_New;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed
     (Kernel : access GObject_Record'Class; Tree : GObject)
   is
      T : Project_Tree := Project_Tree (Tree);
   begin
      --  Destroy all the items in the tree.
      --  The next call to refresh via the "project_view_changed" signal will
      --  completely restore the tree.
      Freeze (T);
      Remove_Node (T, null);
      Thaw (T);
   end Project_Changed;

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
     (Tree             : access Project_Tree_Record'Class;
      Project          : Project_Id;
      Parent_Node      : Gtk_Ctree_Node := null;
      Modified_Project : Boolean := False) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean :=
        Projects.Table (Project).Imported_Projects = Empty_Project_List
        and then (not Show_Directories
                  or else Projects.Table (Project).Source_Dirs = Nil_String);
      Node_Type : Node_Types := Project_Node;
   begin
      if Modified_Project then
         Node_Type := Modified_Project_Node;
      end if;

      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Create_Line_Text
           (Get_Name_String (Projects.Table (Project).Name)),
         Spacing       => 5,
         Pixmap_Closed => Tree.Close_Pixmaps (Node_Type),
         Mask_Closed   => Tree.Close_Masks (Node_Type),
         Pixmap_Opened => Tree.Open_Pixmaps (Node_Type),
         Mask_Opened   => Tree.Open_Masks (Node_Type),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      if Node_Type = Project_Node then
         Node_Set_Row_Data
           (Tree, N,
            (Node_Type  => Project_Node,
             Name       => Projects.Table (Project).Name,
             Up_To_Date => False));

      elsif Node_Type = Modified_Project_Node then
         Node_Set_Row_Data
           (Tree, N,
            (Node_Type  => Modified_Project_Node,
             Name       => Projects.Table (Project).Name,
             Up_To_Date => False));
      end if;

      if not Is_Leaf then
         Add_Dummy_Node (Tree, N);
      end if;
      return N;
   end Add_Project_Node;

   ------------------------
   -- Add_Directory_Node --
   ------------------------

   function Add_Directory_Node
     (Tree             : access Project_Tree_Record'Class;
      Directory        : String;
      Parent_Node      : Gtk_Ctree_Node := null;
      Current_Dir      : String;
      Directory_String : String_Id := No_String;
      Object_Directory : Boolean := False) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : Boolean;
      Node_Type : Node_Types := Directory_Node;
      Start_Index : Natural := 1;
      Buffer : String (1 .. Current_Dir'Length + Directory'Length);
      Buffer_Len : Natural;
   begin
      if Object_Directory then
         Node_Type := Obj_Directory_Node;
      end if;

      --  Compute the absolute directory
      if not Is_Absolute_Path (Directory) then
         Buffer (1 .. Current_Dir'Length) := Current_Dir;
         Buffer
           (Current_Dir'Length + 1 .. Current_Dir'Length + Directory'Length) :=
           Directory;
         Buffer_Len := Current_Dir'Length + Directory'Length;

         if not Absolute_Directories then
            Start_Index := Current_Dir'Length + 1;
         end if;
      else
         Buffer_Len := Directory'Length;
         Buffer (1 .. Buffer_Len) := Directory;
      end if;

      Is_Leaf := Node_Type = Obj_Directory_Node
        or else not Has_Entries (Buffer (1 .. Buffer_Len));


      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          =>
           Create_Line_Text (Buffer (Start_Index .. Buffer_Len)),
         Spacing       => 5,
         Pixmap_Closed => Tree.Close_Pixmaps (Node_Type),
         Mask_Closed   => Tree.Close_Masks (Node_Type),
         Pixmap_Opened => Tree.Open_Pixmaps (Node_Type),
         Mask_Opened   => Tree.Open_Masks (Node_Type),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      if Object_Directory then
         Node_Set_Row_Data
           (Tree, N, (Obj_Directory_Node, Up_To_Date => False));
      else
         Node_Set_Row_Data
           (Tree, N,
            (Directory_Node,
             Directory => Directory_String,
             Up_To_Date => False));
      end if;

      if not Is_Leaf then
         Add_Dummy_Node (Tree, N);
      end if;
      return N;
   end Add_Directory_Node;

   -------------------
   -- Add_File_Node --
   -------------------

   function Add_File_Node
     (Tree             : access Project_Tree_Record'Class;
      File             : String_Id;
      Parent_Node      : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean := False;
   begin
      String_To_Name_Buffer (File);

      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Create_Line_Text (Name_Buffer (1 .. Name_Len)),
         Spacing       => 5,
         Pixmap_Closed => Tree.Close_Pixmaps (File_Node),
         Mask_Closed   => Tree.Close_Masks (File_Node),
         Pixmap_Opened => Tree.Open_Pixmaps (File_Node),
         Mask_Opened   => Tree.Open_Masks (File_Node),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      Node_Set_Row_Data
        (Tree, N, (File_Node, File => File, Up_To_Date => False));

      if not Is_Leaf then
         Add_Dummy_Node (Tree, N);
      end if;
      return N;
   end Add_File_Node;

   -----------------------
   -- Add_Category_Node --
   -----------------------

   function Add_Category_Node
     (Tree             : access Project_Tree_Record'Class;
      Category_Name    : String;
      Is_Specification : Boolean;
      Parent_Node      : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean := False;
   begin
      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Create_Line_Text (Category_Name),
         Spacing       => 5,
         Pixmap_Closed => Tree.Close_Pixmaps (Category_Node),
         Mask_Closed   => Tree.Close_Masks (Category_Node),
         Pixmap_Opened => Tree.Open_Pixmaps (Category_Node),
         Mask_Opened   => Tree.Open_Masks (Category_Node),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      Node_Set_Row_Data
        (Tree, N, (Category_Node,
                   Subprogram_Spec => Is_Specification,
                   Up_To_Date => True));
      return N;
   end Add_Category_Node;

   ---------------------
   -- Add_Entity_Node --
   ---------------------

   function Add_Entity_Node
     (Tree : access Project_Tree_Record'Class;
      Construct : Construct_Information;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean := True;
      Text : Tree_Chars_Ptr_Array;
   begin
      if Construct.Is_Declaration then
         if Construct.Profile /= null then
            Text := Create_Line_Text
              (Construct.Name.all & " (spec) " &
               Reduce (Construct.Profile.all));
         else
            Text := Create_Line_Text (Construct.Name.all & " (spec)");
         end if;

      elsif Construct.Profile /= null then
         Text := Create_Line_Text
           (Construct.Name.all & " " & Reduce (Construct.Profile.all));
      else
         Text := Create_Line_Text (Construct.Name.all);
      end if;

      N := Insert_Node
        (Ctree         => Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => 5,
         Pixmap_Closed => Tree.Close_Pixmaps (Entity_Node),
         Mask_Closed   => Tree.Close_Masks (Entity_Node),
         Pixmap_Opened => Tree.Open_Pixmaps (Entity_Node),
         Mask_Opened   => Tree.Open_Masks (Entity_Node),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);

      Node_Set_Row_Data
        (Tree, N, (Entity_Node,
                   Sloc_Start => Construct.Sloc_Start,
                   Sloc_End   => Construct.Sloc_End,
                   Up_To_Date => True));
      return N;
   end Add_Entity_Node;

   -------------------------
   -- Expand_Project_Node --
   -------------------------

   procedure Expand_Project_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data)
   is
      Prj_List    : Project_List;
      Project     : Project_Id := Get_Project_View_From_Name (Data.Name);
      N           : Gtk_Ctree_Node := null;
      Dir         : String_List_Id;
      Current_Dir : constant String := String (Get_Current_Dir);

   begin
      --  The modified project, if any, is always first

      if Projects.Table (Project).Modifies /= No_Project then
         N := Add_Project_Node
           (Tree, Projects.Table (Project).Modifies, Node, True);
      end if;

      --  Imported projects

      Prj_List := Projects.Table (Project).Imported_Projects;
      while Prj_List /= Empty_Project_List loop
         N := Add_Project_Node
           (Tree, Project_Lists.Table (Prj_List).Project, Node);
         Prj_List := Project_Lists.Table (Prj_List).Next;
      end loop;

      --  Source directories
      --  ??? Should show only first-level directories

      Dir := Projects.Table (Project).Source_Dirs;
      while Dir /= Nil_String loop
         String_To_Name_Buffer (String_Elements.Table (Dir).Value);
         N := Add_Directory_Node
           (Tree             => Tree,
            Directory        => Name_Buffer (1 .. Name_Len),
            Parent_Node      => Node,
            Current_Dir      => Current_Dir,
            Directory_String => String_Elements.Table (Dir).Value);
         Dir := String_Elements.Table (Dir).Next;
      end loop;

      --  Object directory

      N := Add_Directory_Node
        (Tree             => Tree,
         Directory        =>
           Get_Name_String (Projects.Table (Project).Object_Directory),
         Parent_Node      => Node,
         Current_Dir      => Current_Dir,
         Object_Directory => True);
   end Expand_Project_Node;

   ---------------------------
   -- Expand_Directory_Node --
   ---------------------------

   procedure Expand_Directory_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data)
   is
      Project_View : Project_Id := Get_Parent_Project (Tree, Node);
      Src : String_List_Id := Projects.Table (Project_View).Sources;
      N : Gtk_Ctree_Node;

   begin
      --  Subdirectories
      --  ???

      --  Files

      while Src /= Nil_String loop
         if File_In_Directory
           (Data.Directory, String_Elements.Table (Src).Value)
         then
            N := Add_File_Node
              (Tree        => Tree,
               File        => String_Elements.Table (Src).Value,
               Parent_Node => Node);
         end if;
         Src := String_Elements.Table (Src).Next;
      end loop;
   end Expand_Directory_Node;

   ----------------------
   -- Expand_File_Node --
   ----------------------

   procedure Expand_File_Node
     (Tree : access Project_Tree_Record'Class;
      Node : Gtk_Ctree_Node;
      Data : User_Data)
   is
      File_Name  : constant String := Get_File_From_Node (Tree, Node);
      Name       : constant String := File_Name & ASCII.NUL;
      Buffer     : String_Access;
      N          : Gtk_Ctree_Node;
      F          : File_Descriptor;
      Lang       : Language_Access;
      Indent, Next_Indent : Natural;
      Constructs : Construct_List;
      Length     : Natural;
      Category   : Language_Category;

      type Ctree_Node_Array is array (Language_Category'Range)
        of Gtk_Ctree_Node;
      Categories : Ctree_Node_Array := (others => null);

   begin
      F := Open_Read (Name, Binary);

      if F = Invalid_FD then
         return;
      end if;

      Buffer := new String (1 .. Integer (File_Length (F)));
      Length := Read (F, Buffer.all'Address, Buffer'Length);
      Close (F);

      Lang := Get_Language_From_File (File_Name);

      if Lang /= null then
         Parse_Constructs
           (Lang,
            To_Chars_Ptr (Buffer.all'Address),
            Buffer_Length => Length,
            Result        => Constructs,
            Indent        => Indent,
            Next_Indent   => Next_Indent);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown then
                  if Categories (Category) = null then
                     Categories (Category) := Add_Category_Node
                       (Tree,
                        Category_Name    => Category_Name (Category),
                        Is_Specification => False,
                        Parent_Node      => Node);
                  end if;

                  N := Add_Entity_Node
                    (Tree, Constructs.Current.all, Categories (Category));
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         Free (Constructs);
      end if;

      Free (Buffer);
   end Expand_File_Node;

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Tree : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      T        : Project_Tree := Project_Tree (Tree);
      Node     : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Data     : User_Data := Node_Get_Row_Data (T, Node);
   begin
      --  If the node is not already up-to-date

      if not Data.Up_To_Date then
         Freeze (T);

         --  Remove the dummy node, and report that the node is up-to-date
         Remove_Node (T, Row_Get_Children (Node_Get_Row (Node)));
         Data.Up_To_Date := True;
         Node_Set_Row_Data (T, Node, Data);

         case Data.Node_Type is
            when Project_Node =>
               Expand_Project_Node (Project_Tree (T), Node, Data);

            when Modified_Project_Node =>
               null;

            when Directory_Node =>
               Expand_Directory_Node (Project_Tree (T), Node, Data);

            when Obj_Directory_Node =>
               null;

            when File_Node =>
               Expand_File_Node (Project_Tree (T), Node, Data);

            when Category_Node | Entity_Node =>
               --  Work was already done when the file node was open
               null;

         end case;

         Sort_Recursive (T, Node);
         Thaw (T);
      end if;
   end Expand_Tree_Cb;

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Entries (Directory : String) return Boolean is
      D    : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
   begin
      Open (D, Directory);
      loop
         Read (D, File, Last);
         exit when Last = 0;

         --  and then Is_Directory (Absolute_Dir & File (File'First .. Last))
         --  ??? Should check in the project itself, not on the physical drive.
         if File (File'First .. Last) /= "."
           and then File (File'First .. Last) /= ".."
         then
            Close (D);
            return True;
         end if;
      end loop;
      Close (D);
      return False;

   exception
      when Directory_Error =>
         --  The directory couldn't be open, probably because of permissions.
         return False;
   end Has_Entries;

   -----------------------
   -- File_In_Directory --
   -----------------------

   function File_In_Directory
     (Directory : String_Id; File : String_Id) return Boolean
   is
      D    : Dir_Type;
      File_Name : String (1 .. 255);
      Last : Natural;
   begin
      String_To_Name_Buffer (Directory);
      Open (D, Name_Buffer (1 .. Name_Len));
      String_To_Name_Buffer (File);

      loop
         Read (D, File_Name, Last);
         exit when Last = 0;

         if Last = Name_Len
           and then File_Name (1 .. Last) = Name_Buffer (1 .. Name_Len)
         then
            Close (D);
            return True;
         end if;
      end loop;

      Close (D);
      return False;

   exception
      when Directory_Error =>
         --  The directory couldn't be open, probably because of permissions.
         return False;
   end File_In_Directory;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String
   is
      N : Gtk_Ctree_Node := Node;
   begin
      while N /= null
        and then Node_Get_Row_Data (Tree, N).Node_Type /= File_Node
      loop
         N := Row_Get_Parent (Node_Get_Row (N));
      end loop;

      if N = null then
         return "";
      else
         String_To_Name_Buffer (Node_Get_Row_Data (Tree, N).File);
         declare
            Name : constant String := Name_Buffer (1 .. Name_Len);
         begin
            return
              Get_Directory_From_Node (Tree, Row_Get_Parent (Node_Get_Row (N)))
              & Name;
         end;
      end if;
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String
   is
      N : Gtk_Ctree_Node := Node;
   begin
      while N /= null loop
         declare
            User : constant User_Data := Node_Get_Row_Data (Tree, N);
         begin
            exit when User.Node_Type = Directory_Node
              or else User.Node_Type = Obj_Directory_Node;
         end;

         N := Row_Get_Parent (Node_Get_Row (N));
      end loop;

      if N = null then
         return "";
      else
         String_To_Name_Buffer (Node_Get_Row_Data (Tree, N).Directory);
         declare
            Name : constant String := Name_Buffer (1 .. Name_Len);
            Last : Natural := Name'Last;
         begin
            if Name'Length > 2
              and then Name (Last - 1 .. Last) = Directory_Separator & "."
            then
               Last := Last - 1;
            end if;

            if Name (Last) /= Directory_Separator then
               return
                 Get_Directory_From_Node
                 (Tree, Row_Get_Parent (Node_Get_Row (N)))
                 & Name & Directory_Separator;

            else
               return
                 Get_Directory_From_Node
                 (Tree, Row_Get_Parent (Node_Get_Row (N)))
                 & Name (Name'First .. Last);
            end if;
         end;
      end if;
   end Get_Directory_From_Node;

   -------------------
   -- Category_Name --
   -------------------

   function Category_Name (Category : Language_Category) return String is
   begin
      if Category = Cat_Procedure then
         return "subprogram";

      else
         declare
            S : String := Language_Category'Image (Category);
         begin
            Lower_Case (S);

            --  Skip the "Cat_" partx
            return S (S'First + 4 .. S'Last);
         end;
      end if;
   end Category_Name;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category (Category : Language_Category)
      return Language_Category is
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

   -------------------------
   -- Update_Project_Node --
   -------------------------

   procedure Update_Project_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      Count : Natural := 0;
      Src   : String_List_Id;
      Index : Natural;
      N, N2 : Gtk_Ctree_Node;
      Current_Dir : constant String := String (Get_Current_Dir);
   begin
      --  The goal here is to keep the directories if their current state
      --  (expanded or not), while doing the update.

      --  Count the number of subdirectories

      Src := Projects.Table (Get_Project_View (Tree.Kernel)).Source_Dirs;
      while Src /= Nil_String loop
         Count := Count + 1;
         Src := String_Elements.Table (Src).Next;
      end loop;

      declare
         Sources : array (1 .. Count) of String_Id;
      begin
         --  Store the directories
         Index := Sources'First;
         Src := Projects.Table (Get_Project_View (Tree.Kernel)).Source_Dirs;
         while Src /= Nil_String loop
            Sources (Index) := String_Elements.Table (Src).Value;
            Index := Index + 1;
            Src := String_Elements.Table (Src).Next;
         end loop;

         --  Remove from the tree all the directories that are no longer in the
         --  project

         N := Row_Get_Children (Node_Get_Row (Node));
         while N /= null loop
            N2 := Row_Get_Sibling (Node_Get_Row (N));

            declare
               User : constant User_Data := Node_Get_Row_Data (Tree, N);
            begin
               if User.Node_Type = Directory_Node then
                  Index := Sources'First;
                  while Index <= Sources'Last loop
                     if Sources (Index) /= No_String
                       and then String_Equal (Sources (Index), User.Directory)
                     then
                        Sources (Index) := No_String;
                        exit;
                     end if;
                     Index := Index + 1;
                  end loop;

                  if Index > Sources'Last then
                     Remove_Node (Tree, N);
                  else
                     Update_Node (Tree, N);
                  end if;
               end if;
            end;
            N := N2;
         end loop;

         --  Then add all the new directories

         for J in Sources'Range loop
            if Sources (J) /= No_String then
               String_To_Name_Buffer (Sources (J));
               N := Add_Directory_Node
                 (Tree             => Tree,
                  Directory        => Name_Buffer (1 .. Name_Len),
                  Parent_Node      => Node,
                  Current_Dir      => Current_Dir,
                  Directory_String => Sources (J),
                  Object_Directory => False);
            end if;
         end loop;
      end;
   end Update_Project_Node;

   ---------------------------
   -- Update_Directory_Node --
   ---------------------------

   procedure Update_Directory_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      Count : Natural := 0;
      Src   : String_List_Id;
      Index : Natural;
      N, N2 : Gtk_Ctree_Node;
   begin
      --  The goal here is to keep the files and subdirectories if their
      --  current state (expanded or not), while doing the update.

      --  Count the number of subdirectories

      Src := Projects.Table (Get_Project_View (Tree.Kernel)).Sources;
      while Src /= Nil_String loop
         Count := Count + 1;
         Src := String_Elements.Table (Src).Next;
      end loop;

      declare
         Sources : array (1 .. Count) of String_Id;
      begin
         --  Store the source files
         Index := Sources'First;
         Src := Projects.Table (Get_Project_View (Tree.Kernel)).Sources;
         while Src /= Nil_String loop
            Sources (Index) := String_Elements.Table (Src).Value;
            String_To_Name_Buffer (Sources (Index));
            Index := Index + 1;
            Src := String_Elements.Table (Src).Next;
         end loop;

         --  Remove from the tree all the directories that are no longer in the
         --  project

         N := Row_Get_Children (Node_Get_Row (Node));
         while N /= null loop
            N2 := Row_Get_Sibling (Node_Get_Row (N));

            declare
               User : constant User_Data := Node_Get_Row_Data (Tree, N);
            begin
               if User.Node_Type = File_Node then
                  Index := Sources'First;
                  while Index <= Sources'Last loop
                     if Sources (Index) /= No_String
                       and then String_Equal (Sources (Index), User.File)
                     then
                        Sources (Index) := No_String;
                        exit;
                     end if;
                     Index := Index + 1;
                  end loop;

                  if Index > Sources'Last then
                     Remove_Node (Tree, N);
                  end if;
               end if;
            end;
            N := N2;
         end loop;

         --  Then add all the new directories

         for J in Sources'Range loop
            if Sources (J) /= No_String then
               N := Add_File_Node
                 (Tree             => Tree,
                  File             => Sources (J),
                  Parent_Node      => Node);
            end if;
         end loop;
      end;
   end Update_Directory_Node;

   -----------------
   -- Update_Node --
   -----------------

   procedure Update_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      Data  : User_Data := Node_Get_Row_Data (Tree, Node);
      N, N2 : Gtk_Ctree_Node;

   begin
      --  If the information about the node hasn't been computed before,
      --  then we don't need to do anything. This will be done when the
      --  node is actually expanded by the user

      if not Data.Up_To_Date then
         return;
      end if;

      --  Likewise, if a node is not expanded, we simply remove all underlying
      --  information
      --  ??? This means that when the user will open them, he won't find the
      --  ??? status that was there before.

      case Data.Node_Type is
         when Project_Node | Modified_Project_Node
           | Directory_Node | Obj_Directory_Node | File_Node
           =>
            if not Row_Get_Expanded (Node_Get_Row (Node)) then
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

         when others =>
            null;
      end case;

      case Data.Node_Type is
         when Project_Node   => Update_Project_Node (Tree, Node);
         when Directory_Node => Update_Directory_Node (Tree, Node);

         when others =>
            null;
      end case;
   end Update_Node;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Kernel : access GObject_Record'Class; Tree : GObject)
   is
      T : Project_Tree := Project_Tree (Tree);
      Selected_P   : Gtk_Ctree_Node := null;
      Selected_Dir : String_Access := null;
   begin
      --  No project view => Clean up the tree
      if Get_Project_View (T.Kernel) = No_Project then
         Remove_Node (T, null);
         return;
      end if;

      Freeze (T);

      --  If the tree is empty, this simply means we never created it, so we
      --  need to do it now

      if Node_Nth (T, 0) = null then
         Expand (T, Add_Project_Node (T, Get_Project_View (T.Kernel)));


      --  If we are displaying a new view of the tree that was there before, we
      --  want to keep the project nodes, and most important their open/close
      --  status, so as to minimize the changes the user sees.

      else
         --  Save the selection, so that we can restore it later
         Selected_P := Get_Selected_Project_Node (T);
         if Selected_P /= null then
            declare
               U : User_Data := Node_Get_Row_Data
                 (T, Node_List.Get_Data (Get_Selection (T)));
            begin
               if U.Node_Type = Directory_Node then
                  String_To_Name_Buffer (U.Directory);
                  Selected_Dir := new String'
                    (Name_Buffer (Name_Buffer'First .. Name_Len));
               end if;
            end;
         end if;

         Update_Node (T, Node_Nth (T, 0));
         Sort_Recursive (T);

         --  Restore the selection. Note that this also resets the project
         --  view clist, with the contents of all the files.

         if Selected_P /= null then
            if Selected_Dir /= null then
               Select_Directory (T, Selected_P, Selected_Dir.all);
               Free (Selected_Dir);
            else
               Select_Directory (T, Selected_P);
            end if;
         end if;
      end if;

      Thaw (T);
   end Refresh;

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

   function Get_Selected_Project_Node (Tree : access Project_Tree_Record'Class)
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

   ----------------------------
   -- Get_Selected_Directory --
   ----------------------------

   function Get_Selected_Directory
     (Tree    : access Project_Tree_Record;
      Project_Filter : Prj.Project_Id := Prj.No_Project) return String_Id
   is
      use type Node_List.Glist;
      Selection : Node_List.Glist := Get_Selection (Tree);
      N : Gtk_Ctree_Node;
   begin
      if Selection /= Node_List.Null_List then
         N := Node_List.Get_Data (Selection);

         --  Loop until we get to a directory or project
         while N /= null loop
            declare
               User : constant User_Data := Node_Get_Row_Data (Tree, N);
            begin
               case User.Node_Type is
                  when Project_Node =>
                     return No_String;

                  when Directory_Node =>
                     if Project_Filter /= No_Project
                       and then Get_Parent_Project (Tree, N) /= Project_Filter
                     then
                        return No_String;
                     else
                        return User.Directory;
                     end if;

                  when others =>
                     null;
               end case;
            end;

            N := Row_Get_Parent (Node_Get_Row (N));
         end loop;
      end if;
      return No_String;
   end Get_Selected_Directory;

   -----------------------
   -- Get_Selected_File --
   -----------------------

   function Get_Selected_File
     (Tree    : access Project_Tree_Record) return Types.String_Id
   is
      use type Node_List.Glist;
      Selection : Node_List.Glist := Get_Selection (Tree);
   begin
      if Selection /= Node_List.Null_List then
         return Get_File_From_Node (Tree, Node_List.Get_Data (Selection));
      end if;
      return No_String;
   end Get_Selected_File;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
      return String_Id
   is
      N : Gtk_Ctree_Node := Node;
   begin
      --  Loop until we get to a file
      while N /= null loop
         declare
            User : constant User_Data := Node_Get_Row_Data (Tree, N);
         begin
            case User.Node_Type is
               when File_Node =>
                  return User.File;

               when Project_Node
                 | Directory_Node
                 | Modified_Project_Node
                 | Obj_Directory_Node =>
                  return No_String;

               when others =>
                  null;
            end case;
         end;
         N := Row_Get_Parent (Node_Get_Row (N));
      end loop;
      return No_String;
   end Get_File_From_Node;

   -------------------
   -- Node_Selected --
   -------------------

   procedure Node_Selected
     (Tree : access Project_Tree_Record'Class; Node : Gtk_Ctree_Node)
   is
      use type Node_List.Glist;

      File : constant String_Id := Get_File_From_Node (Tree, Node);
      N    : Gtk_Ctree_Node := Node;
      User : constant User_Data := Node_Get_Row_Data (Tree, N);

   begin
      case User.Node_Type is
         when Entity_Node =>
            String_To_Name_Buffer (File);

            declare
               File_S : constant String := Name_Buffer (1 .. Name_Len);
               Dir_S  : constant String := Get_Directory_From_Node (Tree, N);

            begin
               Go_To (Tree.Kernel,
                      Dir_S & File_S,
                      User.Sloc_Start.Line, User.Sloc_Start.Column);
            end;

         when File_Node =>
            String_To_Name_Buffer (File);
            declare
               File_S : constant String := Name_Buffer (1 .. Name_Len);
               Dir_S  : constant String :=
                 Get_Directory_From_Node (Tree, N);
            begin
               Open_File (Tree.Kernel, Dir_S & File_S);
            end;

         when others =>
            null;

      end case;
   end Node_Selected;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Tree : access Gtk_Widget_Record'Class; Args : Gtk_Args) return Boolean
   is
      use Row_List;

      T        : constant Project_Tree := Project_Tree (Tree);
      Event    : constant Gdk_Event := To_Event (Args, 1);
      Row      : Gint;
      Column   : Gint;
      Is_Valid : Boolean;
      Node     : Gtk_Ctree_Node;

   begin
      Get_Selection_Info
        (T, Gint (Get_X (Event)), Gint (Get_Y (Event)), Row, Column, Is_Valid);

      if not Is_Valid then
         return False;
      end if;

      if Get_Button (Event) = 1 then
         Node := Node_Nth (T, Guint (Row));

         declare
            use type Node_List.Glist;
            User : constant User_Data := Node_Get_Row_Data (T, Node);
         begin
            --  Select the node only on double click if this is a file, on
            --  simple click otherwise.

            case User.Node_Type is
               when File_Node =>
                  if Get_Event_Type (Event) = Gdk_2button_Press then
                     Node_Selected (T, Node);

                     --  Stop the propagation of the event, otherwise the
                     --  node will also be opened, which is confusing.

                     return True;
                  end if;

               when others =>
                  Node_Selected (T, Node);
            end case;
         end;
      end if;

      return False;
   end Button_Press;

begin
   --  ??? Temporary, this will be done in Glide itself.
   Add_File_Extensions (Ada_Lang, ".ads;.adb;.ada;.a;.dg");
   Add_File_Extensions (C_Lang,   ".c;.h");
   Add_File_Extensions (Cpp_Lang, ".cc;.cpp;.C;.hh;.H");
end Project_Trees;

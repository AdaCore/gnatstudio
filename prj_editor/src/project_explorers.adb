-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glide_Kernel;            use Glide_Kernel;
with Scenario_Views;          use Scenario_Views;
with Vsearch_Ext;             use Vsearch_Ext;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Interfaces.C.Strings;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;            use GNAT.Case_Util;
with Ada.Exceptions;            use Ada.Exceptions;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib.Xml_Int;             use Glib.Xml_Int;

with Gdk.Bitmap;               use Gdk.Bitmap;
with Gdk.Color;                use Gdk.Color;
with Gdk.Event;                use Gdk.Event;
with Gdk.Pixmap;               use Gdk.Pixmap;
with Gdk.Pixbuf;               use Gdk.Pixbuf;

with Gtk.Enums;                use Gtk.Enums;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Clist;                use Gtk.Clist;
with Gtk.Ctree;                use Gtk.Ctree;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtk.Label;                use Gtk.Label;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.Types;             use Gtkada.Types;
with Gtkada.MDI;               use Gtkada.MDI;

with Prj;                      use Prj;
with Namet;                    use Namet;
with Stringt;                  use Stringt;

with Types;                    use Types;

with Prj_API;                  use Prj_API;
with Pixmaps_IDE;              use Pixmaps_IDE;
with Pixmaps_Prj;              use Pixmaps_Prj;
with Language;                 use Language;
with Language.Unknown;         use Language.Unknown;
with Basic_Types;              use Basic_Types;
with String_Utils;             use String_Utils;
with String_List_Utils;        use String_List_Utils;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Intl;               use Glide_Intl;
with Language_Handlers.Glide;  use Language_Handlers.Glide;
with Traces;                   use Traces;
with Vsearch_Ext;              use Vsearch_Ext;
with Find_Utils;               use Find_Utils;

with Unchecked_Deallocation;
with System;
with Unchecked_Conversion;

package body Project_Explorers is

   Me : constant Debug_Handle := Create ("Project_Explorers");

   Show_Relative_Paths : constant Boolean := True;
   --  <preference> If True, only relative paths are show in the project view,
   --  otherwise the full path is displayed.

   Normalized_Directories : constant Boolean := True;
   --  <preference> True if directories should be fully normalized, eg links
   --  should be resolved. False if the explorer should display the name as
   --  given in the project file"

   Projects_Before_Directories : constant Boolean := False;
   --  <preference> True if the projects should be displayed, when sorted,
   --  before the directories in the project view.

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column          : constant := 0;
   Base_Name_Column     : constant := 1;
   Absolute_Name_Column : constant := 2;
   Node_Type_Column     : constant := 3;
   User_Data_Column     : constant := 4;

   Number_Of_Columns    : constant := 1;
   --  Number of columns in the ctree.

   Ctree_Spacing        : constant := 5;
   --  The spacing used for the ctree nodes.

   Explorer_Module_ID   : Module_ID := null;
   --  Id for the explorer module

   -----------------
   -- Local types --
   -----------------

   subtype String_Access is Basic_Types.String_Access;

   type Append_Directory_Idle_Data is record
      Explorer  : Project_Explorer;
      Norm_Dest : String_Access;
      Norm_Dir  : String_Access;
      D         : GNAT.Directory_Operations.Dir_Type;
      Depth     : Integer := 0;
      Base      : Gtk_Tree_Iter;
      Dirs      : String_List_Utils.String_List.List;
      Files     : String_List_Utils.String_List.List;
      Idle      : Boolean := False;
   end record;

   procedure Free is
      new Unchecked_Deallocation (Append_Directory_Idle_Data,
                                  Append_Directory_Idle_Data_Access);

   subtype Tree_Chars_Ptr_Array is Chars_Ptr_Array (1 .. Number_Of_Columns);

   type User_Data (Node_Type : Real_Node_Types; Name_Length : Natural) is
   record
      Up_To_Date : Boolean := False;
      --  Indicates whether the children of this node (imported projects,
      --  directories,...) have already been parsed and added to the tree. If
      --  this is False, then when the node is open, any child should be
      --  removed and the new children should be computed.

      case Node_Type is
         when Project_Node | Extends_Project_Node =>
            Name    : Name_Id;
            --  We do not keep a pointer to the project_id itself, since this
            --  becomes obsolete as soon as a new project_view is parsed. On
            --  the other hand, the Name_Id is always the same, thus making it
            --  possible to relate nodes from the old tree and nodes from the
            --  new one.

         when Directory_Node | Obj_Directory_Node =>
            Directory : String_Id;
            --  The name of the directory associated with that node
            --  ??? The String_Id might be reset if we ever decide to reset the
            --  tables. We should keep a Name_Id instead.

         when File_Node =>
            null;

         when Category_Node =>
            Category : Language_Category;

         when Entity_Node =>
            Entity_Name : String (1 .. Name_Length);
            Sloc_Start, Sloc_Entity, Sloc_End : Source_Location;

      end case;
   end record;
   --  Information kept with each node in the tree.

   type User_Data_Access is access User_Data;
   procedure Free is new Unchecked_Deallocation (User_Data, User_Data_Access);
   function To_User_Data is new
     Unchecked_Conversion (System.Address, User_Data_Access);

   package Project_Row_Data is new Gtk.Ctree.Row_Data (User_Data);
   use Project_Row_Data;

   ---------------
   -- Searching --
   ---------------

   type Explorer_Search_Context is new Search_Context with record
      Current : Gtk_Ctree_Node;
      Include_Entities : Boolean;
   end record;
   type Explorer_Search_Context_Access is access all Explorer_Search_Context;

   type Explorer_Search_Extra_Record is new Gtk_Frame_Record with record
      Include_Entities : Gtk_Check_Button;
   end record;
   type Explorer_Search_Extra is access all Explorer_Search_Extra_Record'Class;

   function Explorer_Search_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Create a new search context for the explorer

   function Search
     (Context         : access Explorer_Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search the next occurrence in the explorer

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   function Create_Line_Text (Column1 : String) return Tree_Chars_Ptr_Array;
   --  Create an array of strings suitable for display in the ctree.
   --  Always use this function instead of creating the array yourself, since
   --  this checks that there are as many elements in the array as columns in
   --  the tree.
   --  The caller is responsible for freeing the memory associated with the
   --  returned value.

   function Parse_Path
     (Path : String) return String_List_Utils.String_List.List;
   --  Parse a path string and return a list of all directories in it.

   function Greatest_Common_Path
     (L : String_List_Utils.String_List.List) return String;
   --  Return the greatest common path to a list of directories.

   ------------------
   -- Adding nodes --
   ------------------

   function Add_Project_Node
     (Explorer     : access Project_Explorer_Record'Class;
      Project      : Project_Id;
      Parent_Node  : Gtk_Ctree_Node := null;
      Modified_Project : Boolean := False) return Gtk_Ctree_Node;
   --  Add a new project node in the tree.
   --  Parent_Node is the parent of the project in the tree. If this is null,
   --  the new node is added at the root level of the tree.
   --  The new node is initially closed, and its contents will only be
   --  initialized when the node is opened by the user.

   function Add_Directory_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Directory        : String;
      Parent_Node      : Gtk_Ctree_Node := null;
      Project          : Project_Id;
      Directory_String : String_Id;
      Files_In_Project : String_Array_Access;
      Object_Directory : Boolean := False) return Gtk_Ctree_Node;
   --  Add a new directory node in the tree, for Directory.
   --  Directory is expected to be an absolute path name.
   --  Directory_String should be specified for source directories only, and is
   --  not required for object directories.
   --  Project is the project to which the directory belongs

   function Add_File_Node
     (Explorer    : access Project_Explorer_Record'Class;
      File        : String;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new file node in the tree, for File

   function Add_Category_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Category    : Language_Category;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new category node in the tree, for Category_Name

   function Add_Entity_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Construct   : Construct_Information;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new entity node in the tree, for Entity_Name

   function File_Append_Category_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add a category node in the file view.

   function File_Append_Entity_Node
     (Explorer    : access Project_Explorer_Record'Class;
      File        : String;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add an entity node in the file view.

   procedure File_Append_File_Info
     (Explorer  : access Project_Explorer_Record'Class;
      Node      : Gtk_Tree_Iter;
      File_Name : String);
   --  Add info to a file node in the file view.

   procedure File_Append_Dummy_Iter
     (Explorer : access Project_Explorer_Record'Class;
      Base     : Gtk_Tree_Iter);
   --  Add an empty item to an iter in the file view.

   procedure File_Append_File
     (Explorer  : access Project_Explorer_Record'Class;
      Base      : Gtk_Tree_Iter;
      File      : String);
   --  Append a file node to Base in the file view.
   --  File must be an absolute file name.

   procedure File_Append_Directory
     (Explorer      : access Project_Explorer_Record'Class;
      Dir           : String;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : String  := "";
      Idle          : Boolean := False);
   --  Add to the file view the directory Dir, at node given by Iter.
   --  If Append_To_Dir is not "", and is a sub-directory of Dir, then
   --  the path is expanded recursively all the way to Append_To_Dir.

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  ???
   --  Called by File_Append_Directory.

   procedure Free_Children
     (T    : Project_Explorer;
      Iter : Gtk_Tree_Iter);
   --  Free all the children of iter Iter in the file view.

   procedure Compute_Children
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node);
   --  Compute the children of Node, if they haven't been computed yet.

   ---------------------
   -- Expanding nodes --
   ---------------------

   procedure Expand_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node;
      Data     : User_Data);
   --  Expand a project node, ie add children for all the imported projects,
   --  the directories, ...

   procedure Expand_Directory_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node;
      Data     : User_Data);
   --  Expand a directory node, ie add children for all the files and
   --  subirectories.

   procedure Expand_File_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node);
   --  Expand a file node, ie add children for all the entities defined in the
   --  file.

   procedure Expand_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues);
   --  Called every time a node is expanded in the file view.
   --  It is responsible for automatically adding the children of the current
   --  node if they are not there already.

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues);
   --  Called every time a node is collapsed in the file view.

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view.

   procedure File_Remove_Idle_Calls
     (Explorer : Project_Explorer);
   --  Remove the idle calls for filling the file view.

   --------------------
   -- Updating nodes --
   --------------------

   procedure Update_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files : String_Array_Access;
      Node : Gtk_Ctree_Node);
   --  Recompute the directories for the project.

   procedure Update_Directory_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files_In_Project : String_Array_Access;
      Node     : Gtk_Ctree_Node;
      Data     : User_Data);
   --  Recompute the files for the directory. This procedure tries to keep the
   --  existing files if they are in the project view, so as to keep the
   --  expanded status
   --  Data must be the user data associated with Node

   ----------------------------
   -- Retrieving information --
   ----------------------------

   function Get_Project_From_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node) return Project_Id;
   --  Return the name of the project that Node belongs to. Note that if Node
   --  is directly associated with a projet, we return the importing project,
   --  note the one associated with Node.

   procedure Add_Dummy_Node
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node);
   --  Add a dummy, invisible, child to Node. This is used to force Tree to
   --  display an expansion box besides Node. The actual children of Node will
   --  be computed on demand when the user expands Node.

   function Get_File_From_Node
     (Explorer  : access Project_Explorer_Record'Class;
      Node      : Gtk_Ctree_Node;
      Full_Path : Boolean := False) return String;
   --  Return the name of the file containing Node (or, in case Node is an
   --  Entity_Node, the name of the file that contains the entity).
   --  The full name, including directory, is returned if Full_Path is True.

   function Get_Directory_From_Node
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node)
      return String;
   --  Return the name of the directory to which Node belongs. This returns the
   --  full directory name, relative to the project.
   --  The return strings always ends with a directory separator.

   function Category_Name (Category : Language_Category) return String;
   --  Return the name of the node for Category

   function Get_Selected_Project_Node
     (Explorer : access Project_Explorer_Record'Class) return Gtk_Ctree_Node;
   --  Return the node that contains the selected directory (or, if the user
   --  selected a project directly, it returns the node of that project itself)

   procedure Update_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Node             : Gtk_Ctree_Node;
      Files_In_Project : String_Array_Access;
      Force_Expanded   : Boolean := False);
   --  Refresh the contents of the Node after the Project_View has
   --  changed. This means that possibly the list of directories has
   --  changed. However, the hierarchy of projects can not change, nor the list
   --  of modified projects.
   --  Files_In_Project should contain the list of sources in the project to
   --  which Node belongs. If it is null, it will be computed automatically.
   --
   --  If Force_Expanded is true, then the node is considered as currently
   --  expanded.

   procedure Select_Directory
     (Explorer     : access Project_Explorer_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Directory    : String := "");
   --  Select a specific project, and (if not "") a specific directory
   --  in that project

   function Has_Entries
     (Project   : Project_Id;
      Directory : String;
      Files     : String_Array_Access) return Boolean;
   --  Return True if Directory contains any file among Files.

   procedure Refresh
     (Kernel : access GObject_Record'Class; Explorer : GObject);
   --  Refresh the contents of the tree after the project view has changed.
   --  This procedure tries to keep as many things as possible in the current
   --  state (expanded nodes,...)

   procedure Project_Changed
     (Kernel : access GObject_Record'Class; Explorer : GObject);
   --  Called when the project as changed, as opposed to the project view.
   --  This means we need to start up with a completely new tree, no need to
   --  try to keep the current one.

   procedure Node_Selected
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node);
   --  Called when a node is selected.
   --  It provides the standard behavior when an entity is selected (open the
   --  appropriate source editor).

   function Button_Press_Release
     (Explorer : access Gtk_Widget_Record'Class; Args : Gtk_Args)
      return Boolean;
   --  Callback for the "button_press" event

   function Find_Iter_For_Event
     (Explorer : access Project_Explorer_Record'Class;
      Event    : Gdk_Event)
     return Gtk_Tree_Iter;
   --  Get the iter in the file view under the cursor corresponding to Event,
   --  if any.

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the file view.

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for the "button_press" event on the file view.

   function Filter_Category
     (Category : Language_Category) return Language_Category;
   --  Return the category to use when an entity is Category.
   --  This is used to group subprograms (procedures and functions together),
   --  or remove unwanted categories (in which case Cat_Unknown is returned).

   function Explorer_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access;
   --  Return the context to use for the contextual menu.
   --  It is also used to return the context for
   --  Glide_Kernel.Get_Current_Context, and thus can be called with a null
   --  event or a null menu.

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create the Glide_Kernel.Get_Current_Context.

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one.

   procedure Child_Selected
     (Explorer : access Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a new child is selected in the MDI. This makes sure
   --  that the selected not in the explorer doesn't reflect false information.

   procedure On_Parse_Xref (Explorer : access Gtk_Widget_Record'Class);
   --  Parse all the LI information contained in the object directory of the
   --  current selection.

   function Project_View_Compare
     (Tree : access Gtk_Clist_Record'Class;
      Row1 : Gtk_Clist_Row;
      Row2 : Gtk_Clist_Row) return Gint;
   --  The ordering function used for the project view.

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column               => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column      => GType_String,
         Base_Name_Column          => GType_String,
         Node_Type_Column          => GType_Int,
         User_Data_Column          => GType_Pointer);
   end Columns_Types;

   -------------------------------
   -- File_Append_Category_Node --
   -------------------------------

   function File_Append_Category_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
      Name    : constant String := Category_Name (Category);
   begin
      --  ??? code duplication from Add_Category_Node
      Sibling := Children (Explorer.File_Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Explorer.File_Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Explorer.File_Model, Sibling, Base_Name_Column)
           <= Name
         loop
            Next (Explorer.File_Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Explorer.File_Model, N, Parent_Iter);
         else
            Insert_Before (Explorer.File_Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Explorer.File_Model, N, Absolute_Name_Column, "");
      Set (Explorer.File_Model, N, Base_Name_Column, Name);
      Set (Explorer.File_Model, N, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (Category_Node)));
      Set (Explorer.File_Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Category_Node)));
      return N;
   end File_Append_Category_Node;

   -----------------------------
   -- File_Append_Entity_Node --
   -----------------------------

   function File_Append_Entity_Node
     (Explorer    : access Project_Explorer_Record'Class;
      File        : String;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
      User    : User_Data_Access;
      Val     : GValue;
   begin
      Sibling := Children (Explorer.File_Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Explorer.File_Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Explorer.File_Model, Sibling, Base_Name_Column)
           <= Construct.Name.all
         loop
            Next (Explorer.File_Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Explorer.File_Model, N, Parent_Iter);
         else
            Insert_Before (Explorer.File_Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Explorer.File_Model, N, Absolute_Name_Column, File);

      if Construct.Is_Declaration then
         if Construct.Profile /= null then
            Set (Explorer.File_Model, N, Base_Name_Column,
                 Construct.Name.all & " (spec) " &
                 Reduce (Construct.Profile.all));
         else
            Set (Explorer.File_Model, N, Base_Name_Column,
                 Construct.Name.all & " (spec)");

         end if;

      elsif Construct.Profile /= null then
         Set (Explorer.File_Model, N, Base_Name_Column,
              Construct.Name.all & " " & Reduce (Construct.Profile.all));
      else
         Set (Explorer.File_Model, N, Base_Name_Column,
              Construct.Name.all);
      end if;

      Set (Explorer.File_Model, N, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (Entity_Node)));
      Set (Explorer.File_Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Entity_Node)));

      User := new User_Data'(Node_Type   => Entity_Node,
                             Name_Length => Construct.Name'Length,
                             Entity_Name => Construct.Name.all,
                             Sloc_Start  => Construct.Sloc_Start,
                             Sloc_Entity => Construct.Sloc_Entity,
                             Sloc_End    => Construct.Sloc_End,
                             Up_To_Date  => True);
      Init (Val, GType_Pointer);
      Set_Address (Val, User.all'Address);
      Set_Value (Explorer.File_Model, N, User_Data_Column, Val);
      Unset (Val);
      return N;
   end File_Append_Entity_Node;

   ---------------------------
   -- File_Append_File_Info --
   ---------------------------

   procedure File_Append_File_Info
     (Explorer  : access Project_Explorer_Record'Class;
      Node      : Gtk_Tree_Iter;
      File_Name : String)
   is
      Buffer     : String_Access;
      N          : Gtk_Tree_Iter;
      F          : File_Descriptor;
      Lang       : Language_Access;
      Constructs : Construct_List;
      Length     : Natural;
      Category   : Language_Category;

      type Gtk_Tree_Iter_Array is array (Language_Category'Range)
        of Gtk_Tree_Iter;
      Categories : Gtk_Tree_Iter_Array := (others => Null_Iter);

   begin
      Push_State (Explorer.Kernel, Busy);
      --  ??? code duplication from Expand_File_Node.

      F := Open_Read (File_Name, Binary);

      if F = Invalid_FD then
         return;
      end if;

      Buffer := new String (1 .. Integer (File_Length (F)));
      Length := Read (F, Buffer.all'Address, Buffer'Length);
      Close (F);

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel)),
         File_Name);

      if Lang /= Unknown_Lang then
         Parse_Constructs (Lang, Buffer (1 .. Length), Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown then
                  if Categories (Category) = Null_Iter then
                     Categories (Category) := File_Append_Category_Node
                       (Explorer,
                        Category    => Category,
                        Parent_Iter => Node);
                  end if;

                  N := File_Append_Entity_Node
                    (Explorer, File_Name,
                     Constructs.Current.all, Categories (Category));
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         Free (Constructs);
      end if;

      Free (Buffer);
      Pop_State (Explorer.Kernel);
   end File_Append_File_Info;

   ----------------------------
   -- File_Append_Dummy_Iter --
   ----------------------------

   procedure File_Append_Dummy_Iter
     (Explorer : access Project_Explorer_Record'Class;
      Base     : Gtk_Tree_Iter)
   is
      Iter      : Gtk_Tree_Iter;
   begin
      Append (Explorer.File_Model, Iter, Base);
   end File_Append_Dummy_Iter;

   ----------------------
   -- File_Append_File --
   ----------------------

   procedure File_Append_File
     (Explorer : access Project_Explorer_Record'Class;
      Base     : Gtk_Tree_Iter;
      File     : String)
   is
      Iter : Gtk_Tree_Iter;
      Lang : Language_Access;

   begin
      Append (Explorer.File_Model, Iter, Base);

      Set (Explorer.File_Model, Iter, Absolute_Name_Column, File);

      Set (Explorer.File_Model, Iter, Base_Name_Column, Base_Name (File));

      Set (Explorer.File_Model, Iter, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (File_Node)));

      Set (Explorer.File_Model, Iter, Node_Type_Column,
           Gint (Node_Types'Pos (File_Node)));

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel)),
         File);

      if Lang /= Unknown_Lang then
         File_Append_Dummy_Iter (Explorer, Iter);
      end if;
   end File_Append_File;

   --------------------
   -- Read_Directory --
   --------------------

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean
   is
      File       : String (1 .. 1024);
      Last       : Natural;
      Path_Found : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      New_D      : Append_Directory_Idle_Data_Access;

      use String_List_Utils.String_List;

   begin
      --  If we are appending at the base, create a node indicating the
      --  absolute path to the directory.

      if D.Base = Null_Iter then
         Append (D.Explorer.File_Model, Iter, D.Base);

         --   ??? if D.Norm_Dir.all = Directory_Separator then
         --      check for drive letters.

         Set (D.Explorer.File_Model, Iter, Absolute_Name_Column,
              D.Norm_Dir.all);
         Set (D.Explorer.File_Model, Iter, Base_Name_Column,
              D.Norm_Dir.all);
         Set (D.Explorer.File_Model, Iter, Node_Type_Column,
              Gint (Node_Types'Pos (Directory_Node)));
         Set (D.Explorer.File_Model, Iter, Icon_Column,
              C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));
         D.Base := Iter;

         return Read_Directory (D);
      end if;

      Read (D.D, File, Last);

      if D.Depth >= 0 and then Last /= 0 then
         if not (Last = 1 and then File (1) = '.')
           and then not (Last = 2 and then File (1 .. 2) = "..")
         then
            if Is_Directory (D.Norm_Dir.all & File (File'First .. Last)) then
               Append (D.Dirs, File (File'First .. Last));
            else
               Append (D.Files, File (File'First .. Last));
            end if;

            if D.Depth = 0 then
               D.Depth := -1;
            end if;
         end if;

         return True;
      end if;

      Close (D.D);

      if D.Idle then
         Pop_State (D.Explorer.Kernel);
         Push_State (D.Explorer.Kernel, Busy);
      end if;

      Sort (D.Dirs);
      Sort (D.Files);

      while not Is_Empty (D.Dirs) loop
         declare
            Dir : constant String := Head (D.Dirs);
         begin
            Append (D.Explorer.File_Model, Iter, D.Base);
            Set (D.Explorer.File_Model, Iter, Absolute_Name_Column,
                 D.Norm_Dir.all & Dir & Directory_Separator);
            Set (D.Explorer.File_Model, Iter, Base_Name_Column, Dir);
            Set (D.Explorer.File_Model, Iter, Node_Type_Column,
                 Gint (Node_Types'Pos (Directory_Node)));

            if D.Depth = 0 then
               exit;
            end if;

            --  Are we on the path to the target directory ?

            if not Path_Found
              and then D.Norm_Dir.all'Length + Dir'Length
              <= D.Norm_Dest.all'Length
              and then D.Norm_Dest.all
              (D.Norm_Dest.all'First
               .. D.Norm_Dest.all'First
                  + D.Norm_Dir.all'Length + Dir'Length - 1)
              = D.Norm_Dir.all & Dir
            then
               Path_Found := True;

               declare
                  Success   : Boolean;
                  Path      : Gtk_Tree_Path;
                  Expanding : constant Boolean := D.Explorer.Expanding;
               begin
                  Path := Get_Path (D.Explorer.File_Model, D.Base);

                  D.Explorer.Expanding := True;
                  Success := Expand_Row (D.Explorer.File_Tree, Path, False);
                  D.Explorer.Expanding := Expanding;

                  Set (D.Explorer.File_Model, D.Base, Icon_Column,
                       C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));

                  Path_Free (Path);
               end;

               --  Are we on the target directory ?

               if D.Norm_Dest.all = D.Norm_Dir.all & Dir
                 & Directory_Separator
               then
                  declare
                     Success   : Boolean;
                     Path      : Gtk_Tree_Path;
                     Expanding : constant Boolean := D.Explorer.Expanding;
                  begin
                     Path := Get_Path (D.Explorer.File_Model, Iter);

                     File_Append_Directory
                       (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                        Iter, D.Depth, D.Norm_Dest.all,
                        False);

                     D.Explorer.Expanding := True;
                     Success := Expand_Row (D.Explorer.File_Tree, Path, False);
                     D.Explorer.Expanding := Expanding;

                     Set (D.Explorer.File_Model, Iter, Icon_Column,
                          C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));

                     Scroll_To_Cell
                       (D.Explorer.File_Tree,
                        Path, null, True,
                        0.1, 0.1);

                     Path_Free (Path);
                  end;

               else
                  File_Append_Directory
                    (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                     Iter, D.Depth, D.Norm_Dest.all, D.Idle);
               end if;

            else
               File_Append_Dummy_Iter (D.Explorer, Iter);

               Set (D.Explorer.File_Model, Iter, Icon_Column,
                    C_Proxy (D.Explorer.Close_Pixbufs (Directory_Node)));
            end if;

            Next (D.Dirs);
         end;
      end loop;

      while not Is_Empty (D.Files) loop
         File_Append_File
           (D.Explorer, D.Base, D.Norm_Dir.all & Head (D.Files));
         Next (D.Files);
      end loop;

      Free (D.Norm_Dir);
      Free (D.Norm_Dest);

      Pop_State (D.Explorer.Kernel);

      New_D := D;
      Free (New_D);

      return False;

   exception
      when Directory_Error =>
         --  The directory couldn't be open, probably because of permissions.

         New_D := D;
         Free (New_D);
         return False;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Read_Directory;

   ---------------------------
   -- File_Append_Directory --
   ---------------------------

   procedure File_Append_Directory
     (Explorer      : access Project_Explorer_Record'Class;
      Dir           : String;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : String  := "";
      Idle          : Boolean := False)
   is
      D : Append_Directory_Idle_Data_Access := new Append_Directory_Idle_Data;
      --  ??? where is this freed ?

      Timeout_Id : Timeout_Handler_Id;

   begin
      begin
         Open (D.D, Dir);
      exception
         when Directory_Error =>
            Free (D);
            return;
      end;

      D.Norm_Dest := new String'(Normalize_Pathname (Append_To_Dir));
      D.Norm_Dir  := new String'(Normalize_Pathname (Dir));
      D.Depth     := Depth;
      D.Base      := Base;
      D.Explorer  := Project_Explorer (Explorer);
      D.Idle      := Idle;

      if Idle then
         Push_State (Explorer.Kernel, Processing);
      else
         Push_State (Explorer.Kernel, Busy);
      end if;

      if Idle then
         Timeout_Id :=
           File_Append_Directory_Timeout.Add (1, Read_Directory'Access, D);
         Timeout_Id_List.Append (Explorer.Fill_Timeout_Ids, Timeout_Id);
      else
         loop
            exit when not Read_Directory (D);
         end loop;
      end if;
   end File_Append_Directory;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   --------------------------
   -- Project_View_Compare --
   --------------------------

   function Project_View_Compare
     (Tree : access Gtk_Clist_Record'Class;
      Row1 : Gtk_Clist_Row;
      Row2 : Gtk_Clist_Row) return Gint
   is
      T  : constant Gtk_Ctree := Gtk_Ctree (Tree);
      N1 : constant Gtk_Ctree_Node := Find_Node_Ptr (T, Gtk_Ctree_Row (Row1));
      N2 : constant Gtk_Ctree_Node := Find_Node_Ptr (T, Gtk_Ctree_Row (Row2));
      D1 : constant User_Data := Node_Get_Row_Data (T, N1);
      D2 : constant User_Data := Node_Get_Row_Data (T, N2);
      Is_Project1 : constant Boolean :=
        D1.Node_Type in Project_Node .. Extends_Project_Node;
      Is_Project2 : constant Boolean :=
        D2.Node_Type in Project_Node .. Extends_Project_Node;
      Is_Dir1 : constant Boolean :=
        D1.Node_Type in Directory_Node .. Obj_Directory_Node;
      Is_Dir2 : constant Boolean :=
        D2.Node_Type in Directory_Node .. Obj_Directory_Node;

      Text1 : constant String := Node_Get_Text (T, N1, 0);
      Text2 : constant String := Node_Get_Text (T, N2, 0);

   begin
      --  At least one of the nodes is a project

      if Is_Project1 then
         if Is_Project2 then
            if Text1 < Text2 then
               return -1;
            elsif Text1 = Text2 then
               return 0;
            else
               return 1;
            end if;
         elsif Projects_Before_Directories then
            return -1;
         else
            return 1;
         end if;

      elsif Is_Project2 then
         if Projects_Before_Directories then
            return 1;
         else
            return -1;
         end if;
      end if;

      --  None of the nodes is a project, is at least one a directory ?

      if Is_Dir1 then
         if Is_Dir2 then
            --  Object directories come last
            if D1.Node_Type = Obj_Directory_Node then
               return 1;
            elsif D2.Node_Type = Obj_Directory_Node then
               return -1;
            end if;

            if Text1 < Text2 then
               return -1;
            elsif Text1 = Text2 then
               return 0;
            else
               return 1;
            end if;
         else
            return 1;
         end if;
      elsif Is_Dir2 then
         return -1;
      end if;

      --  None of the nodes is a directory. the two nodes necessarily have the
      --  same type, so we just compare the strings

      if Text1 < Text2 then
         return -1;
      elsif Text1 = Text2 then
         return 0;
      else
         return 1;
      end if;
   end Project_View_Compare;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Explorer := new Project_Explorer_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class)
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
           (Explorer.Open_Pixmaps (Node_Type),
            Get_Window (Get_Main_Window (Kernel)),
            Explorer.Open_Masks (Node_Type), Null_Color, Open);
         Create_From_Xpm_D
           (Explorer.Close_Pixmaps (Node_Type),
            Get_Window (Get_Main_Window (Kernel)),
            Explorer.Close_Masks (Node_Type), Null_Color, Close);

         Explorer.Open_Pixbufs (Node_Type) := Gdk_New_From_Xpm_Data (Open);
         Explorer.Close_Pixbufs (Node_Type) := Gdk_New_From_Xpm_Data (Close);
      end Create_Pixmaps;

      Scrolled : Gtk_Scrolled_Window;
      Label    : Gtk_Label;

   begin
      Initialize_Vbox (Explorer, Homogeneous => False);
      Explorer.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Explorer.Search, Kernel_Handle (Kernel));
      Ref (Explorer.Search.Vbox_Search);
      Remove (Explorer.Search, Explorer.Search.Vbox_Search);
      Pack_Start
        (Explorer, Explorer.Search.Vbox_Search,
         Fill => True, Expand => False);
      Unref (Explorer.Search.Vbox_Search);

      Gtk_New (Explorer.Scenario, Kernel);
      Pack_Start (Explorer, Explorer.Scenario, Fill => True, Expand => False);

      Gtk_New (Explorer.Notebook);
      Set_Tab_Pos (Explorer.Notebook, Pos_Bottom);
      Pack_Start (Explorer, Explorer.Notebook, Fill => True, Expand => True);

      Gtk_New (Scrolled);
      Gtk_New (Label, -"Project View");
      Append_Page (Explorer.Notebook, Scrolled, Label);

      Gtk_New (Explorer.Tree, Number_Of_Columns, 0);
      Add (Scrolled, Explorer.Tree);

      Set_Compare_Func (Explorer.Tree, Project_View_Compare'Access);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.Tree,
         Object          => Explorer,
         ID              => Explorer_Module_ID,
         Context_Func    => Explorer_Context_Factory'Access);

      Create_Pixmaps (Project_Node, project_xpm, project_closed_xpm);
      Create_Pixmaps
        (Modified_Project_Node,
         project_modified_xpm,
         project_modified_closed_xpm);
      Create_Pixmaps
        (Extends_Project_Node, project_ext_xpm, project_ext_closed_xpm);
      Create_Pixmaps (Directory_Node, mini_ofolder_xpm, mini_folder_xpm);
      Create_Pixmaps
        (Obj_Directory_Node, mini_folder_object_xpm, mini_folder_object_xpm);
      Create_Pixmaps (File_Node, mini_page_xpm, mini_page_xpm);
      Create_Pixmaps (Category_Node, var_xpm, var_xpm);

      --  Create the Tree View for the files view.

      Gtk_New (Explorer.File_Model, Columns_Types);
      Gtk_New (Explorer.File_Tree, Explorer.File_Model);
      Set_Headers_Visible (Explorer.File_Tree, False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.File_Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (File_Button_Press'Access),
         Explorer,
         After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Get_Selection (Explorer.File_Tree),
         "changed",
         Gtkada.Handlers.Widget_Callback.To_Marshaller
           (File_Selection_Changed'Access),
         Explorer,
         After => True);

      Gtk_New (Scrolled);
      Gtk_New (Label, -"File View");
      Append_Page (Explorer.Notebook, Scrolled, Label);

      Add (Scrolled, Explorer.File_Tree);
      Set_Column_Types (Explorer.File_Tree);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.File_Tree,
         Object          => Explorer,
         ID              => Explorer_Module_ID,
         Context_Func    => Explorer_Context_Factory'Access);

      --  ??? The following block is duplicated in Refresh.
      if Get_Pref (Kernel, File_View_Shows_Only_Project) then
         declare
            Inc : String_List_Utils.String_List.List;
            Obj : String_List_Utils.String_List.List;
         begin
            Inc := Parse_Path
              (Include_Path (Get_Project_View (Kernel), True));
            Obj := Parse_Path
              (Object_Path (Get_Project_View (Kernel), True));
            String_List_Utils.String_List.Concat (Inc, Obj);
            File_Append_Directory
              (Explorer,
               Greatest_Common_Path (Inc),
               Null_Iter, 1, Get_Current_Dir, True);
            String_List_Utils.String_List.Free (Inc);
         end;
      else
         File_Append_Directory
           (Explorer, (1 => Directory_Separator),
            Null_Iter, 1, Get_Current_Dir, True);
      end if;

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "row_expanded",
         File_Tree_Expand_Row_Cb'Access, Explorer, False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "row_collapsed",
         File_Tree_Collapse_Row_Cb'Access, Explorer, False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "destroy",
         On_File_Destroy'Access, Explorer, False);

      Set_Line_Style (Explorer.Tree, Ctree_Lines_Solid);

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Explorer.Expand_Id := Widget_Callback.Object_Connect
        (Explorer.Tree, "tree_expand", Expand_Tree_Cb'Access, Explorer);
      Widget_Callback.Object_Connect
        (Explorer.Tree, "tree_select_row",
         Tree_Select_Row_Cb'Access, Explorer);

      --  So that the horizontal scrollbars work correctly.
      Set_Column_Auto_Resize (Explorer.Tree, 0, True);

      --  Automatic update of the tree when the project changes
      Object_User_Callback.Connect
        (Kernel, "project_view_changed",
         Object_User_Callback.To_Marshaller (Refresh'Access),
         GObject (Explorer));
      Object_User_Callback.Connect
        (Kernel, "project_changed",
         Object_User_Callback.To_Marshaller (Project_Changed'Access),
         GObject (Explorer));

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree, "button_release_event",
         Button_Press_Release'Access, Explorer);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree, "button_press_event",
         Button_Press_Release'Access, Explorer);

      --  The explorer (project view) is automatically refreshed when the
      --  project view is changed.

      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_selected",
         Child_Selected'Unrestricted_Access, Explorer);
   end Initialize;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
     (Explorer : access Gtk_Widget_Record'Class; Args : GValues)
   is
      E     : constant Project_Explorer := Project_Explorer (Explorer);
      Child : constant MDI_Child := MDI_Child (To_Object (Args, 1));
   begin
      if Child = null
        or else not (Get_Widget (Child).all in Project_Explorer_Record'Class)
      then
         Unselect_Recursive (E.Tree);
      end if;
   end Child_Selected;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget
   is
      Explorer : Project_Explorer;
   begin
      if Node.Tag.all = "Project_Explorer" then
         Gtk_New (Explorer, User);
         Refresh (User, GObject (Explorer));
         return Gtk_Widget (Explorer);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Project_Explorer_Record'Class then
         N := new Node;
         N.Tag := new String'("Project_Explorer");
         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------------
   -- On_Parse_Xref --
   -------------------

   procedure On_Parse_Xref (Explorer : access Gtk_Widget_Record'Class) is
      E    : constant Project_Explorer := Project_Explorer (Explorer);
      Node : constant Gtk_Ctree_Node :=
        Node_List.Get_Data (Get_Selection (E.Tree));
      Data : constant User_Data := Node_Get_Row_Data (E.Tree, Node);
   begin
      pragma Assert (Data.Node_Type = Obj_Directory_Node);
      Push_State (E.Kernel, Busy);
      Parse_All_LI_Information
        (E.Kernel, Normalize_Pathname (Get_String (Data.Directory)));
      Pop_State (E.Kernel);

   exception
      when Exc : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (Exc));
         Pop_State (E.Kernel);
   end On_Parse_Xref;

   ------------------------------
   -- Explorer_Context_Factory --
   ------------------------------

   function Explorer_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget);

      T       : constant Project_Explorer := Project_Explorer (Object);
      Context : Selection_Context_Access;
      Item    : Gtk_Menu_Item;

   begin
      if Get_Current_Page (T.Notebook) = 0 then
         declare
            use type Node_List.Glist;

            Row, Column       : Gint;
            Is_Valid          : Boolean;
            Node, Parent      : Gtk_Ctree_Node;
            Importing_Project : Project_Id := No_Project;

         begin
            if Event /= null then
               Get_Selection_Info
                 (T.Tree, Gint (Get_X (Event)), Gint (Get_Y (Event)),
                  Row, Column, Is_Valid);

               if not Is_Valid then
                  return null;
               end if;

               Node := Node_Nth (T.Tree, Guint (Row));
               Gtk_Select (T.Tree, Node);

            else
               if Get_Selection (T.Tree) /= Node_List.Null_List then
                  Node := Node_List.Get_Data (Get_Selection (T.Tree));
               else
                  return null;
               end if;
            end if;

            Parent := Row_Get_Parent (Node_Get_Row (Node));

            declare
               Data : constant User_Data := Node_Get_Row_Data (T.Tree, Node);
            begin
               if Data.Node_Type = Entity_Node then
                  Context := new Entity_Selection_Context;
               else
                  Context := new File_Selection_Context;
               end if;

               if Data.Node_Type = Entity_Node then
                  Set_File_Information
                    (Context   => File_Selection_Context_Access (Context),
                     Directory    => Get_Directory_From_Node (T, Node),
                     File_Name    => Get_File_From_Node (T, Node));
                  Set_Entity_Information
                    (Context     => Entity_Selection_Context_Access (Context),
                     Entity_Name => Data.Entity_Name,
                     Category    => Node_Get_Row_Data
                       (T.Tree, Parent).Category,
                     Line        => Data.Sloc_Entity.Line,
                     Column      => Data.Sloc_Entity.Column);

               else
                  if Parent /= null then
                     Importing_Project := Get_Project_From_Node (T, Parent);
                  end if;

                  Set_File_Information
                    (Context   => File_Selection_Context_Access (Context),
                     Directory    => Get_Directory_From_Node (T, Node),
                     File_Name    => Get_File_From_Node (T, Node),
                     Project_View => Get_Project_From_Node (T, Node),
                     Importing_Project => Importing_Project);
               end if;

               if Data.Node_Type = Obj_Directory_Node
                 and then Menu /= null
               then
                  Gtk_New (Item, -"Parse all xref information");
                  Add (Menu, Item);
                  Widget_Callback.Object_Connect
                    (Item, "activate",
                     Widget_Callback.To_Marshaller (On_Parse_Xref'Access),
                     T);
               end if;
            end;
         end;

      else
         declare
            Iter      : Gtk_Tree_Iter;
            File      : String_Access := null;
            Node_Type : Node_Types;

         begin
            Iter := Find_Iter_For_Event (T, Event);

            if Iter /= Null_Iter then
               Node_Type := Node_Types'Val
                 (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

               case Node_Type is
                  when Directory_Node | File_Node =>
                     File := new String'
                       (Get_String (T.File_Model, Iter, Absolute_Name_Column));
                     Context := new File_Selection_Context;

                  when Entity_Node =>
                     Context := new Entity_Selection_Context;

                  when others =>
                     null;

               end case;
            end if;

            if File /= null then
               Set_File_Information
                 (Context   => File_Selection_Context_Access (Context),
                  Directory => Dir_Name (File.all),
                  File_Name => Base_Name (File.all));
               Free (File);
            end if;
         end;
      end if;

      return Context;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
         return Context;
   end Explorer_Context_Factory;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls (Explorer : Project_Explorer) is
   begin
      while not Timeout_Id_List.Is_Empty (Explorer.Fill_Timeout_Ids) loop
         Pop_State (Explorer.Kernel);
         Timeout_Remove (Timeout_Id_List.Head (Explorer.Fill_Timeout_Ids));
         Timeout_Id_List.Next (Explorer.Fill_Timeout_Ids);
      end loop;
   end File_Remove_Idle_Calls;

   ---------------------
   -- On_File_Destroy --
   ---------------------

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params);
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      File_Remove_Idle_Calls (T);
   end On_File_Destroy;

   -------------------------------
   -- File_Tree_Collapse_Row_Cb --
   -------------------------------

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T    : constant Project_Explorer := Project_Explorer (Explorer);
      Path : constant Gtk_Tree_Path :=
        Gtk_Tree_Path (Get_Proxy (Nth (Values, 2)));
      Iter : Gtk_Tree_Iter;

   begin
      Iter := Get_Iter (T.File_Model, Path);

      if Iter /= Null_Iter then
         declare
            Iter_Name : constant String :=
              Get_String (T.File_Model, Iter, Absolute_Name_Column);

         begin
            if Is_Directory (Iter_Name) then
               Set (T.File_Model, Iter, Icon_Column,
                    C_Proxy (T.Close_Pixbufs (Directory_Node)));
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end File_Tree_Collapse_Row_Cb;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Project_Explorer;
      Iter : Gtk_Tree_Iter)
   is
      Child_Iter : constant Gtk_Tree_Iter := Children (T.File_Model, Iter);
      Current    : Gtk_Tree_Iter;
      Val        : GValue;
      User       : User_Data_Access;

   begin
      if Has_Child (T.File_Model, Iter) then
         Current := Child_Iter;

         while Current /= Null_Iter loop
            --  ??? There might be a problem here: we must free children
            --  recursively, this frees only one level.

            if Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)))
              = Entity_Node
            then
               Get_Value (T.File_Model, Current, User_Data_Column, Val);
               User := To_User_Data (Get_Address (Val));
               Free (User);
            end if;

            Remove (T.File_Model, Current);
            Current := Children (T.File_Model, Iter);
         end loop;
      end if;
   end Free_Children;

   -----------------------------
   -- File_Tree_Expand_Row_Cb --
   -----------------------------

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T       : Project_Explorer := Project_Explorer (Explorer);
      Path    : constant Gtk_Tree_Path :=
        Gtk_Tree_Path (Get_Proxy (Nth (Values, 2)));
      Iter    : Gtk_Tree_Iter;
      Success : Boolean;

   begin
      if T.Expanding then
         return;
      end if;

      Iter := Get_Iter (T.File_Model, Path);

      if Iter /= Null_Iter then
         T.Expanding := True;

         declare
            Iter_Name : constant String :=
              Get_String (T.File_Model, Iter, Absolute_Name_Column);
            N_Type : constant Real_Node_Types := Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

         begin
            case N_Type is
               when Directory_Node =>
                  Free_Children (T, Iter);
                  File_Append_Directory (T, Iter_Name, Iter, 1);
                  Set (T.File_Model, Iter, Icon_Column,
                       C_Proxy (T.Open_Pixbufs (Directory_Node)));

               when File_Node =>
                  Free_Children (T, Iter);
                  File_Append_File_Info (T, Iter, Iter_Name);

               when Extends_Project_Node =>
                  null;

               when Category_Node | Entity_Node =>
                  null;

               when Project_Node =>
                  null;

               when Obj_Directory_Node =>
                  null;

            end case;
         end;

         Success := Expand_Row (T.File_Tree, Path, False);
         T.Expanding := False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end File_Tree_Expand_Row_Cb;

   ------------------------
   -- Tree_Select_Row_Cb --
   ------------------------

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      T       : constant Project_Explorer := Project_Explorer (Explorer);
      Node    : constant Gtk_Ctree_Node :=
        Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Context : File_Selection_Context_Access;

   begin
      Context := new File_Selection_Context;
      Set_Context_Information (Context, T.Kernel, Explorer_Module_ID);
      Set_File_Information
        (Context,
         Directory    => Get_Directory_From_Node (T, Node),
         File_Name    => Get_File_From_Node (T, Node),
         Project_View => Get_Project_From_Node (T, Node));
      Context_Changed (T.Kernel, Selection_Context_Access (Context));
      Free (Selection_Context_Access (Context));

   exception
      when E : others =>
         Free (Selection_Context_Access (Context));
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end Tree_Select_Row_Cb;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed
     (Kernel : access GObject_Record'Class; Explorer : GObject)
   is
      pragma Unreferenced (Kernel);
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      --  Destroy all the items in the tree.
      --  The next call to refresh via the "project_view_changed" signal will
      --  completely restore the tree.
      Freeze (T.Tree);
      Remove_Node (T.Tree, null);
      Thaw (T.Tree);
   end Project_Changed;

   --------------------
   -- Add_Dummy_Node --
   --------------------

   procedure Add_Dummy_Node
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node)
   is
      N    : Gtk_Ctree_Node;
      Text : Tree_Chars_Ptr_Array := Create_Line_Text ("");
   begin
      --  Add a dummy node
      N := Insert_Node
        (Ctree         => Explorer.Tree,
         Parent        => Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Null_Pixmap,
         Mask_Closed   => Null_Bitmap,
         Pixmap_Opened => Null_Pixmap,
         Mask_Opened   => Null_Bitmap,
         Is_Leaf       => True,
         Expanded      => True);
      Node_Set_Row_Data
        (Explorer.Tree, N,
         (Node_Type   => Obj_Directory_Node,
          Name_Length => 0,
          Directory   => No_String,
          Up_To_Date  => False));
      Free (Text);
   end Add_Dummy_Node;

   ----------------------
   -- Add_Project_Node --
   ----------------------

   function Add_Project_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Project          : Project_Id;
      Parent_Node      : Gtk_Ctree_Node := null;
      Modified_Project : Boolean := False) return Gtk_Ctree_Node
   is
      N         : Gtk_Ctree_Node;
      Is_Leaf   : constant Boolean :=
        Projects.Table (Project).Imported_Projects = Empty_Project_List
        and then (not Get_Pref (Explorer.Kernel, Show_Directories)
                  or else Projects.Table (Project).Source_Dirs = Nil_String);
      Node_Type : Node_Types := Project_Node;
      Text      : Tree_Chars_Ptr_Array;

   begin
      if Modified_Project then
         Node_Type := Extends_Project_Node;

      elsif Project_Modified
        (Explorer.Kernel, Get_Project_From_View (Project))
      then
         Node_Type := Modified_Project_Node;
      end if;

      Text := Create_Line_Text
        (Get_String (Projects.Table (Project).Name));
      N := Insert_Node
        (Ctree         => Explorer.Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Explorer.Close_Pixmaps (Node_Type),
         Mask_Closed   => Explorer.Close_Masks (Node_Type),
         Pixmap_Opened => Explorer.Open_Pixmaps (Node_Type),
         Mask_Opened   => Explorer.Open_Masks (Node_Type),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);
      Free (Text);

      if Node_Type = Project_Node
        or else Node_Type = Modified_Project_Node
      then
         Node_Set_Row_Data
           (Explorer.Tree, N,
            (Node_Type   => Project_Node,
             Name_Length => 0,
             Name        => Projects.Table (Project).Name,
             Up_To_Date  => False));

      elsif Node_Type = Extends_Project_Node then
         Node_Set_Row_Data
           (Explorer.Tree, N,
            (Node_Type   => Extends_Project_Node,
             Name_Length => 0,
             Name        => Projects.Table (Project).Name,
             Up_To_Date  => False));
      end if;

      if not Is_Leaf then
         Add_Dummy_Node (Explorer, N);
      end if;

      return N;
   end Add_Project_Node;

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Entries
     (Project   : Project_Id;
      Directory : String;
      Files     : String_Array_Access) return Boolean
   is
      Dir : constant String := Name_As_Directory (Normalize_Pathname
        (Directory, Dir_Name (Project_Path (Project)),
         Resolve_Links => False));
   begin
      if Files /= null then
         --  We check in the project itself whether there are some files in the
         --  directory.

         for F in Files'Range loop
            if Dir_Name (Files (F).all) = Dir then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Has_Entries;

   ------------------------
   -- Add_Directory_Node --
   ------------------------

   function Add_Directory_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Directory        : String;
      Parent_Node      : Gtk_Ctree_Node := null;
      Project          : Project_Id;
      Directory_String : String_Id;
      Files_In_Project : String_Array_Access;
      Object_Directory : Boolean := False) return Gtk_Ctree_Node
   is
      N         : Gtk_Ctree_Node;
      Is_Leaf   : Boolean;
      Node_Type : Node_Types := Directory_Node;
      Text      : Tree_Chars_Ptr_Array;
      Node_Text : String_Access;

   begin
      pragma Assert (Object_Directory or else Directory_String /= No_String);

      if Object_Directory then
         Node_Type := Obj_Directory_Node;
      end if;

      --  Compute the absolute directory

      if Normalized_Directories then
         Node_Text := new String'
           (Name_As_Directory (Normalize_Pathname (Directory)));
      else
         Node_Text := new String'(Name_As_Directory (Directory));
      end if;

      if Show_Relative_Paths then
         Text := Create_Line_Text (Relative_Path_Name
            (Node_Text.all, Dir_Name (Project_Path (Project))));
      else
         Text := Create_Line_Text (Node_Text.all);
      end if;

      Is_Leaf := Node_Type = Obj_Directory_Node
        or else not Has_Entries (Project, Directory, Files_In_Project);

      N := Insert_Node
        (Ctree         => Explorer.Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Explorer.Close_Pixmaps (Node_Type),
         Mask_Closed   => Explorer.Close_Masks (Node_Type),
         Pixmap_Opened => Explorer.Open_Pixmaps (Node_Type),
         Mask_Opened   => Explorer.Open_Masks (Node_Type),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);
      Free (Node_Text);
      Free (Text);

      if Object_Directory then
         Node_Set_Row_Data
           (Explorer.Tree, N,
            (Node_Type   => Obj_Directory_Node,
             Name_Length => 0,
             Directory   => Directory_String,
             Up_To_Date  => False));
      else
         Node_Set_Row_Data
           (Explorer.Tree, N,
            (Node_Type   => Directory_Node,
             Name_Length => 0,
             Directory   => Directory_String,
             Up_To_Date  => False));
      end if;

      if not Is_Leaf then
         Add_Dummy_Node (Explorer, N);
      end if;

      return N;
   end Add_Directory_Node;

   -------------------
   -- Add_File_Node --
   -------------------

   function Add_File_Node
     (Explorer    : access Project_Explorer_Record'Class;
      File        : String;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N       : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean := False;
      Text    : Tree_Chars_Ptr_Array;

   begin
      Text := Create_Line_Text (File);
      N := Insert_Node
        (Ctree         => Explorer.Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Explorer.Close_Pixmaps (File_Node),
         Mask_Closed   => Explorer.Close_Masks (File_Node),
         Pixmap_Opened => Explorer.Open_Pixmaps (File_Node),
         Mask_Opened   => Explorer.Open_Masks (File_Node),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);
      Free (Text);

      Node_Set_Row_Data
        (Explorer.Tree, N, (Node_Type   => File_Node,
                            Name_Length => 0,
                            Up_To_Date => False));

      if not Is_Leaf then
         Add_Dummy_Node (Explorer, N);
      end if;

      return N;
   end Add_File_Node;

   -----------------------
   -- Add_Category_Node --
   -----------------------

   function Add_Category_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Category    : Language_Category;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N       : Gtk_Ctree_Node;
      Is_Leaf : constant Boolean := False;
      Text    : Tree_Chars_Ptr_Array;

   begin
      Text := Create_Line_Text (Category_Name (Category));
      N := Insert_Node
        (Ctree         => Explorer.Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Explorer.Close_Pixmaps (Category_Node),
         Mask_Closed   => Explorer.Close_Masks (Category_Node),
         Pixmap_Opened => Explorer.Open_Pixmaps (Category_Node),
         Mask_Opened   => Explorer.Open_Masks (Category_Node),
         Is_Leaf       => Is_Leaf,
         Expanded      => False);
      Free (Text);

      Node_Set_Row_Data
        (Explorer.Tree, N,
         (Node_Type   => Category_Node,
          Name_Length => 0,
          Category    => Category,
          Up_To_Date  => True));
      return N;
   end Add_Category_Node;

   ---------------------
   -- Add_Entity_Node --
   ---------------------

   function Add_Entity_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Construct   : Construct_Information;
      Parent_Node : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N       : Gtk_Ctree_Node;
      Text    : Tree_Chars_Ptr_Array;

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
        (Ctree         => Explorer.Tree,
         Parent        => Parent_Node,
         Sibling       => null,
         Text          => Text,
         Spacing       => Ctree_Spacing,
         Pixmap_Closed => Explorer.Close_Pixmaps (Entity_Node),
         Mask_Closed   => Explorer.Close_Masks (Entity_Node),
         Pixmap_Opened => Explorer.Open_Pixmaps (Entity_Node),
         Mask_Opened   => Explorer.Open_Masks (Entity_Node),
         Is_Leaf       => True,
         Expanded      => False);
      Free (Text);

      Node_Set_Row_Data
        (Explorer.Tree, N,
         (Node_Type   => Entity_Node,
          Name_Length => Construct.Name'Length,
          Entity_Name => Construct.Name.all,
          Sloc_Start  => Construct.Sloc_Start,
          Sloc_Entity => Construct.Sloc_Entity,
          Sloc_End    => Construct.Sloc_End,
          Up_To_Date  => True));
      return N;
   end Add_Entity_Node;

   -------------------------
   -- Expand_Project_Node --
   -------------------------

   procedure Expand_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node;
      Data     : User_Data)
   is
      Prj_List    : Project_List;
      Project     : constant Project_Id :=
        Get_Project_View_From_Name (Data.Name);
      N           : Gtk_Ctree_Node := null;
      Dir         : String_List_Id;
      Files       : String_Array_Access := Get_Source_Files
        (Project, Recursive => False, Full_Path => True, Normalized => True);

   begin
      Push_State (Explorer.Kernel, Busy);
      Freeze (Explorer.Tree);
      --  The modified project, if any, is always first

      if Projects.Table (Project).Extends /= No_Project then
         N := Add_Project_Node
           (Explorer, Projects.Table (Project).Extends, Node, True);
      end if;

      --  Imported projects

      Prj_List := Projects.Table (Project).Imported_Projects;
      while Prj_List /= Empty_Project_List loop
         N := Add_Project_Node
           (Explorer, Project_Lists.Table (Prj_List).Project, Node);
         Prj_List := Project_Lists.Table (Prj_List).Next;
      end loop;

      if Get_Pref (Explorer.Kernel, Show_Directories) then
         --  Source directories
         --  ??? Should show only first-level directories

         Dir := Projects.Table (Project).Source_Dirs;
         while Dir /= Nil_String loop
            String_To_Name_Buffer (String_Elements.Table (Dir).Value);
            N := Add_Directory_Node
              (Explorer         => Explorer,
               Directory        => Name_Buffer (1 .. Name_Len),
               Project          => Project,
               Parent_Node      => Node,
               Files_In_Project => Files,
               Directory_String => String_Elements.Table (Dir).Value);
            Dir := String_Elements.Table (Dir).Next;
         end loop;

         --  Object directory

         Start_String;
         Store_String_Chars
           (Get_String (Projects.Table (Project).Object_Directory));

         N := Add_Directory_Node
           (Explorer         => Explorer,
            Directory        =>
              Get_String (Projects.Table (Project).Object_Directory),
            Project          => Project,
            Parent_Node      => Node,
            Directory_String => End_String,
            Files_In_Project => null,
            Object_Directory => True);
      end if;

      Free (Files);
      Thaw (Explorer.Tree);
      Pop_State (Explorer.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Free (Files);
         Thaw (Explorer.Tree);
         Pop_State (Explorer.Kernel);
   end Expand_Project_Node;

   ---------------------------
   -- Expand_Directory_Node --
   ---------------------------

   procedure Expand_Directory_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node;
      Data     : User_Data)
   is
      Project_View : constant Project_Id :=
        Get_Project_From_Node (Explorer, Node);
      Src : String_List_Id;
      N : Gtk_Ctree_Node;
      Dir : constant String :=
        Name_As_Directory (Get_String (Data.Directory));

   begin
      Push_State (Explorer.Kernel, Busy);
      Freeze (Explorer.Tree);
      Src := Projects.Table (Project_View).Sources;
      while Src /= Nil_String loop
         if Is_Regular_File
           (Dir & Get_String (String_Elements.Table (Src).Value))
         then
            N := Add_File_Node
              (Explorer    => Explorer,
               File        => Get_String (String_Elements.Table (Src).Value),
               Parent_Node => Node);
         end if;
         Src := String_Elements.Table (Src).Next;
      end loop;
      Thaw (Explorer.Tree);
      Pop_State (Explorer.Kernel);
   end Expand_Directory_Node;

   ----------------------
   -- Expand_File_Node --
   ----------------------

   procedure Expand_File_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node)
   is
      File_Name  : constant String :=
        Get_File_From_Node (Explorer, Node, Full_Path => True);
      Buffer     : String_Access;
      N          : Gtk_Ctree_Node;
      F          : File_Descriptor;
      Lang       : Language_Access;
      Constructs : Construct_List;
      Length     : Natural;
      Category   : Language_Category;

      type Ctree_Node_Array is array (Language_Category'Range)
        of Gtk_Ctree_Node;
      Categories : Ctree_Node_Array := (others => null);

   begin
      F := Open_Read (File_Name, Binary);

      if F = Invalid_FD then
         return;
      end if;

      Push_State (Explorer.Kernel, Busy);
      Freeze (Explorer.Tree);

      Buffer := new String (1 .. Integer (File_Length (F)));
      Length := Read (F, Buffer.all'Address, Buffer'Length);
      Close (F);

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel)),
         File_Name);

      if Lang /= null then
         Parse_Constructs (Lang, Buffer (1 .. Length), Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown then
                  if Categories (Category) = null then
                     Categories (Category) := Add_Category_Node
                       (Explorer,
                        Category    => Category,
                        Parent_Node => Node);
                  end if;

                  N := Add_Entity_Node
                    (Explorer, Constructs.Current.all, Categories (Category));
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         Free (Constructs);
      end if;

      Free (Buffer);
      Thaw (Explorer.Tree);
      Pop_State (Explorer.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Free (Buffer);
         Thaw (Explorer.Tree);
         Pop_State (Explorer.Kernel);
   end Expand_File_Node;

   ----------------------
   -- Compute_Children --
   ----------------------

   procedure Compute_Children
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node)
   is
      Data : User_Data := Node_Get_Row_Data (Explorer.Tree, Node);
   begin
      --  If the node is not already up-to-date

      Freeze (Explorer.Tree);

      if not Data.Up_To_Date then
         --  Remove the dummy node, and report that the node is up-to-date
         Remove_Node (Explorer.Tree, Row_Get_Children (Node_Get_Row (Node)));
         Data.Up_To_Date := True;
         Node_Set_Row_Data (Explorer.Tree, Node, Data);

         case Data.Node_Type is
            when Project_Node =>
               Expand_Project_Node (Explorer, Node, Data);

            when Extends_Project_Node =>
               null;

            when Directory_Node =>
               Expand_Directory_Node (Explorer, Node, Data);

            when Obj_Directory_Node =>
               null;

            when File_Node =>
               Expand_File_Node (Explorer, Node);

            when Category_Node | Entity_Node =>
               --  Work was already done when the file node was open
               null;

         end case;

         Sort_Recursive (Explorer.Tree, Node);

      else
         Update_Node (Explorer, Node, null, Force_Expanded => True);
      end if;

      Thaw (Explorer.Tree);
   end Compute_Children;

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Node     : constant Gtk_Ctree_Node :=
        Gtk_Ctree_Node (To_C_Proxy (Args, 1));
   begin
      Compute_Children (Project_Explorer (Explorer), Node);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Expand_Tree_Cb;

   ------------------------
   -- Get_File_From_Node --
   ------------------------

   function Get_File_From_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node : Gtk_Ctree_Node;
      Full_Path : Boolean := False)
      return String
   is
      N : Gtk_Ctree_Node := Node;
   begin
      while N /= null
        and then Node_Get_Row_Data (Explorer.Tree, N).Node_Type /= File_Node
      loop
         N := Row_Get_Parent (Node_Get_Row (N));
      end loop;

      if N = null then
         return "";
      elsif Full_Path then
         return Get_Directory_From_Node
           (Explorer, Row_Get_Parent (Node_Get_Row (N)))
           & Node_Get_Text (Explorer.Tree, N, 0);
      else
         return Node_Get_Text (Explorer.Tree, N, 0);
      end if;
   end Get_File_From_Node;

   -----------------------------
   -- Get_Directory_From_Node --
   -----------------------------

   function Get_Directory_From_Node
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node)
     return String
   is
      N : Gtk_Ctree_Node := Node;
   begin
      while N /= null loop
         declare
            User : constant User_Data := Node_Get_Row_Data (Explorer.Tree, N);
         begin
            exit when User.Node_Type = Directory_Node
              or else User.Node_Type = Obj_Directory_Node;
         end;

         N := Row_Get_Parent (Node_Get_Row (N));
      end loop;

      if N = null then
         return "";
      else
         String_To_Name_Buffer
           (Node_Get_Row_Data (Explorer.Tree, N).Directory);
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
                 (Explorer, Row_Get_Parent (Node_Get_Row (N)))
                 & Name & Directory_Separator;

            else
               return
                 Get_Directory_From_Node
                 (Explorer, Row_Get_Parent (Node_Get_Row (N)))
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
         return -"subprogram";

      else
         declare
            S : String := Language_Category'Image (Category);
         begin
            To_Lower (S);

            --  Skip the "Cat_" part
            return S (S'First + 4 .. S'Last);
         end;
      end if;
   end Category_Name;

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

   ----------------------
   -- Select_Directory --
   ----------------------

   procedure Select_Directory
     (Explorer     : access Project_Explorer_Record'Class;
      Project_Node : Gtk_Ctree_Node;
      Directory    : String := "")
   is
      N : Gtk_Ctree_Node :=
        Row_Get_Children (Node_Get_Row (Project_Node));
   begin
      if Directory = "" then
         Gtk_Select (Explorer.Tree, Project_Node);

      else
         while N /= null loop
            declare
               D : constant User_Data := Node_Get_Row_Data (Explorer.Tree, N);
            begin
               if D.Node_Type = Directory_Node then
                  String_To_Name_Buffer (D.Directory);
                  if Name_Buffer (1 .. Name_Len) = Directory then
                     Gtk_Select (Explorer.Tree, N);
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
     (Explorer : access Project_Explorer_Record'Class;
      Files    : String_Array_Access;
      Node     : Gtk_Ctree_Node)
   is
      function Imported_Projects (Prj : Project_Id) return Project_Id_Array;
      --  Return the list of imported projects, as an array

      function Is_Same_Directory (Dir1, Dir2 : String_Id) return Boolean;
      --  Compare the two directories. The first one is first normalized, the
      --  second one is assumed to be normalized already

      -----------------------
      -- Imported_Projects --
      -----------------------

      function Imported_Projects (Prj : Project_Id) return Project_Id_Array is
         Count : Natural := 0;
         Import : Project_List := Projects.Table (Prj).Imported_Projects;
      begin
         while Import /= Empty_Project_List loop
            Count := Count + 1;
            Import := Project_Lists.Table (Import).Next;
         end loop;

         declare
            Imported : Project_Id_Array (1 .. Count);
         begin
            Count := Imported'First;
            Import := Projects.Table (Prj).Imported_Projects;
            while Import /= Empty_Project_List loop
               Imported (Count) := Project_Lists.Table (Import).Project;
               Count := Count + 1;
               Import := Project_Lists.Table (Import).Next;
            end loop;
            return Imported;
         end;
      end Imported_Projects;

      -----------------------
      -- Is_Same_Directory --
      -----------------------

      function Is_Same_Directory (Dir1, Dir2 : String_Id) return Boolean is
         D1 : constant String := Name_As_Directory
           (Normalize_Pathname (Get_String (Dir1), Resolve_Links => False));
         D2 : constant String := Get_String (Dir2);
      begin
         return D1 = D2;
      end Is_Same_Directory;


      Index : Natural;
      N, N2, Tmp : Gtk_Ctree_Node;
      Project : constant Project_Id := Get_Project_From_Node (Explorer, Node);
      Sources : String_Id_Array := Source_Dirs (Project);
      Imported : Project_Id_Array := Imported_Projects (Project);

   begin
      --  The goal here is to keep the directories if their current state
      --  (expanded or not), while doing the update.

      --  Remove from the tree all the directories that are no longer in the
      --  project

      N := Row_Get_Children (Node_Get_Row (Node));
      while N /= null loop
         N2 := Row_Get_Sibling (Node_Get_Row (N));

         declare
            User : constant User_Data :=
              Node_Get_Row_Data (Explorer.Tree, N);
            Prj  : Project_Id;
            Obj  : String_Id;
         begin
            case User.Node_Type is
               when Directory_Node =>
                  Index := Sources'First;
                  while Index <= Sources'Last loop
                     if Sources (Index) /= No_String
                       and then
                       (String_Equal (Sources (Index), User.Directory)
                        or else Is_Same_Directory
                        (Sources (Index), User.Directory))
                     then
                        Sources (Index) := No_String;
                        exit;
                     end if;
                     Index := Index + 1;
                  end loop;

                  if Index > Sources'Last then
                     Remove_Node (Explorer.Tree, N);
                  else
                     Update_Node (Explorer, N, Files);
                  end if;

               when Obj_Directory_Node =>
                  Prj := Get_Project_From_Node (Explorer, N);
                  Remove_Node (Explorer.Tree, N);
                  Start_String;
                  Store_String_Chars
                    (Get_String (Projects.Table
                                   (Project).Object_Directory));
                  Obj := End_String;
                  Tmp := Add_Directory_Node
                    (Explorer,
                     Directory        => Get_String
                       (Projects.Table (Prj).Object_Directory),
                     Project          => Project,
                     Parent_Node      => Node,
                     Directory_String => Obj,
                     Files_In_Project => null,
                     Object_Directory => True);

               when Project_Node =>
                  --  The list of imported project files cannot change with
                  --  the scenario, so there is nothing to be done here
                  declare
                     Prj_Name : constant String := Get_String (User.Name);
                  begin
                     Index := Imported'First;
                     while Index <= Imported'Last loop
                        if Imported (Index) /= No_Project
                          and then Project_Name (Imported (Index)) = Prj_Name
                        then
                           Imported (Index) := No_Project;
                           exit;
                        end if;
                        Index := Index + 1;
                     end loop;

                     if Index > Imported'Last then
                        if Explorer.Old_Selection = N then
                           Explorer.Old_Selection := Row_Get_Parent
                             (Node_Get_Row (Explorer.Old_Selection));
                        end if;
                        Remove_Node (Explorer.Tree, N);

                     else
                        Update_Node (Explorer, N, Files_In_Project => null);
                     end if;
                  end;

               when others =>
                  --  No other node type is possible
                  null;
            end case;
         end;
         N := N2;
      end loop;

      --  Then add all imported projects
      --  Since they are not expanded initially, we do not need to update their
      --  contents.
      for J in Imported'Range loop
         if Imported (J) /= No_Project then
            N := Add_Project_Node
              (Explorer, Project => Imported (J),  Parent_Node => Node);
         end if;
      end loop;

      --  Then add all the new directories

      for J in Sources'Range loop
         if Sources (J) /= No_String then
            String_To_Name_Buffer (Sources (J));
            N := Add_Directory_Node
              (Explorer         => Explorer,
               Directory        => Name_Buffer (1 .. Name_Len),
               Parent_Node      => Node,
               Project          => Project,
               Directory_String => Sources (J),
               Files_In_Project => Files,
               Object_Directory => False);
         end if;
      end loop;
   end Update_Project_Node;

   ---------------------------
   -- Update_Directory_Node --
   ---------------------------

   procedure Update_Directory_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Files_In_Project : String_Array_Access;
      Node             : Gtk_Ctree_Node;
      Data             : User_Data)
   is
      Index : Natural;
      N, N2 : Gtk_Ctree_Node;
      Dir : constant String := Name_As_Directory (Normalize_Pathname
         (Get_String (Data.Directory), Resolve_Links => False));

      type Boolean_Array is array (Files_In_Project'Range) of Boolean;
      New_File : Boolean_Array := (others => True);
      Expanded : constant Boolean := Row_Get_Expanded (Node_Get_Row (Node));

   begin
      --  The goal here is to keep the files and subdirectories if their
      --  current state (expanded or not), while doing the update.

      --  Remove from the tree all the files that are no longer in the
      --  project

      N := Row_Get_Children (Node_Get_Row (Node));
      while N /= null loop
         N2 := Row_Get_Sibling (Node_Get_Row (N));

         declare
            User : constant User_Data :=
              Node_Get_Row_Data (Explorer.Tree, N);
            F : constant String := Node_Get_Text (Explorer.Tree, N, 0);
         begin
            if User.Node_Type = File_Node then
               Index := Files_In_Project'First;
               while Index <= Files_In_Project'Last loop
                  if New_File (Index)
                    and then Base_Name (Files_In_Project (Index).all) = F
                  then
                     New_File (Index) := False;
                     exit;
                  end if;
                  Index := Index + 1;
               end loop;

               if Index > Files_In_Project'Last then
                  Remove_Node (Explorer.Tree, N);
               end if;
            end if;
         end;
         N := N2;
      end loop;

      --  Then add all the new files

      for J in Files_In_Project'Range loop
         if New_File (J)
           and then Dir_Name (Files_In_Project (J).all) = Dir
         then
            N := Add_File_Node
              (Explorer         => Explorer,
               File             => Base_Name (Files_In_Project (J).all),
               Parent_Node      => Node);
         end if;
      end loop;

      --  In case we have first deleted all sources files before adding the
      --  others, the expansion status might have changed, so we need to
      --  restore it.

      if Expanded then
         Gtk.Handlers.Handler_Block (Explorer.Tree, Explorer.Expand_Id);
         Gtk.Ctree.Expand (Explorer.Tree, Node);
         Gtk.Handlers.Handler_Unblock (Explorer.Tree, Explorer.Expand_Id);
      end if;
   end Update_Directory_Node;

   -----------------
   -- Update_Node --
   -----------------

   procedure Update_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Node             : Gtk_Ctree_Node;
      Files_In_Project : String_Array_Access;
      Force_Expanded   : Boolean := False)
   is
      Data   : User_Data := Node_Get_Row_Data (Explorer.Tree, Node);
      N, N2  : Gtk_Ctree_Node;
      N_Type : Node_Types;
      Prj    : constant Project_Id := Get_Project_From_Node (Explorer, Node);
      Files  : String_Array_Access := Files_In_Project;
      Expanded : Boolean := Row_Get_Expanded (Node_Get_Row (Node));

   begin
      if Files = null then
         Files := Get_Source_Files
           (Prj, Recursive => False, Full_Path => True,
            Normalized => Normalized_Directories);
      end if;

      --  If the information about the node hasn't been computed before,
      --  then we don't need to do anything. This will be done when the
      --  node is actually expanded by the user

      --  We need to recompute if a node now has children, when it didn't
      --  before.
      if Row_Get_Children (Node_Get_Row (Node)) = null then
         declare
            Str : constant String := Node_Get_Text (Explorer.Tree, Node, 0);
         begin
            if (Data.Node_Type = Directory_Node
                  and then Has_Entries (Prj, Str, Files))
              or else
              (Data.Node_Type = Project_Node
               and then
               (Projects.Table (Prj).Imported_Projects /= Empty_Project_List
                or else
                (Get_Pref (Explorer.Kernel, Show_Directories)
                 and then Projects.Table (Prj).Source_Dirs /= Nil_String)))
            then
               Set_Node_Info
                 (Ctree         => Explorer.Tree,
                  Node          => Node,
                  Text          => Str,
                  Spacing       => Ctree_Spacing,
                  Pixmap_Closed => Explorer.Close_Pixmaps (Data.Node_Type),
                  Mask_Closed   => Explorer.Close_Masks   (Data.Node_Type),
                  Pixmap_Opened => Explorer.Open_Pixmaps  (Data.Node_Type),
                  Mask_Opened   => Explorer.Open_Masks    (Data.Node_Type),
                  Is_Leaf       => False,
                  Expanded      => False);
               Add_Dummy_Node (Explorer, Node);
            end if;
            Expanded := True;
         end;
      end if;

      if Data.Up_To_Date then
         --  Likewise, if a node is not expanded, we simply remove all
         --  underlying information.

         if Force_Expanded or else Expanded then
            case Data.Node_Type is
               when Project_Node   =>
                  Update_Project_Node (Explorer, Files, Node);
               when Directory_Node =>
                  Update_Directory_Node (Explorer, Files, Node, Data);
               when others         => null;
            end case;

         else
            Data.Up_To_Date := False;
            Node_Set_Row_Data (Explorer.Tree, Node, Data);

            N := Row_Get_Children (Node_Get_Row (Node));
            while N /= null loop
               N2 := Row_Get_Sibling (Node_Get_Row (N));
               Remove_Node (Explorer.Tree, N);
               N := N2;
            end loop;

            Add_Dummy_Node (Explorer, Node);
         end if;
      end if;

      --  Has to be done whether the node is expanded or not, and whether it is
      --  up-to-date or not, as long as its icon is visible.
      --  Change the icons to reflect the modified state of the project
      if Data.Node_Type = Project_Node then
         if Project_Modified
           (Explorer.Kernel,
            Get_Project_From_View (Get_Project_From_Node (Explorer, Node)))
         then
            N_Type := Modified_Project_Node;
         else
            N_Type := Project_Node;
         end if;

         Set_Node_Info
           (Ctree         => Explorer.Tree,
            Node          => Node,
            Text          => Node_Get_Text (Explorer.Tree, Node, 0),
            Spacing       => Ctree_Spacing,
            Pixmap_Closed => Explorer.Close_Pixmaps (N_Type),
            Mask_Closed   => Explorer.Close_Masks (N_Type),
            Pixmap_Opened => Explorer.Open_Pixmaps (N_Type),
            Mask_Opened   => Explorer.Open_Masks (N_Type),
            Is_Leaf       => Row_Get_Is_Leaf (Node_Get_Row (Node)),
            Expanded      => Expanded);
      end if;

      if Files_In_Project = null then
         Free (Files);
      end if;
   end Update_Node;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Kernel : access GObject_Record'Class; Explorer : GObject)
   is
      K : constant Kernel_Handle := Kernel_Handle (Kernel);
      T : Project_Explorer := Project_Explorer (Explorer);
      Selected_Dir : String_Access := null;
   begin
      T.Old_Selection := null;

      --  No project view => Clean up the tree
      if Get_Project_View (T.Kernel) = No_Project then
         Remove_Node (T.Tree, null);

         --  ??? must free memory associated with entities !
         return;
      end if;

      Clear (T.File_Model);
      File_Remove_Idle_Calls (T);

      if Get_Pref (K, File_View_Shows_Only_Project) then
         declare
            Inc : String_List_Utils.String_List.List;
            Obj : String_List_Utils.String_List.List;
         begin
            Inc := Parse_Path
              (Include_Path (Get_Project_View (T.Kernel), True));
            Obj := Parse_Path
              (Object_Path (Get_Project_View (T.Kernel), True));
            String_List_Utils.String_List.Concat (Inc, Obj);
            File_Append_Directory
              (T,
               Greatest_Common_Path (Inc),
               Null_Iter, 1, Get_Current_Dir, True);
            String_List_Utils.String_List.Free (Inc);
         end;
      else
         File_Append_Directory
           (T, (1 => Directory_Separator),
            Null_Iter, 1, Get_Current_Dir, True);
      end if;

      Freeze (T.Tree);

      --  If the tree is empty, this simply means we never created it, so we
      --  need to do it now

      if Node_Nth (T.Tree, 0) = null then
         Gtk.Ctree.Expand
           (T.Tree, Add_Project_Node (T, Get_Project_View (T.Kernel)));

      --  If we are displaying a new view of the tree that was there before, we
      --  want to keep the project nodes, and most important their open/close
      --  status, so as to minimize the changes the user sees.

      else
         --  Save the selection, so that we can restore it later

         T.Old_Selection := Get_Selected_Project_Node (T);

         if T.Old_Selection /= null then
            declare
               U : constant User_Data := Node_Get_Row_Data
                 (T.Tree, Node_List.Get_Data (Get_Selection (T.Tree)));
            begin
               if U.Node_Type = Directory_Node then
                  String_To_Name_Buffer (U.Directory);
                  Selected_Dir := new String'
                    (Name_Buffer (Name_Buffer'First .. Name_Len));
               end if;
            end;
         end if;

         Update_Node (T, Node_Nth (T.Tree, 0), Files_In_Project => null);
         Sort_Recursive (T.Tree);

         --  Restore the selection. Note that this also resets the project
         --  view clist, with the contents of all the files.

         if T.Old_Selection /= null then
            if Selected_Dir /= null then
               Select_Directory (T, T.Old_Selection, Selected_Dir.all);
               Free (Selected_Dir);
            else
               Select_Directory (T, T.Old_Selection);
            end if;
         end if;
      end if;

      Thaw (T.Tree);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Thaw (T.Tree);
   end Refresh;

   ----------------------
   -- Create_Line_Text --
   ----------------------

   function Create_Line_Text (Column1 : String) return Tree_Chars_Ptr_Array is
   begin
      return (1 => Interfaces.C.Strings.New_String (Column1));
   end Create_Line_Text;

   ---------------------------
   -- Get_Project_From_Node --
   ---------------------------

   function Get_Project_From_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Ctree_Node) return Project_Id
   is
      Parent : Gtk_Ctree_Node := Node;
   begin
      while Node_Get_Row_Data (Explorer.Tree, Parent).Node_Type
        /= Project_Node
      loop
         Parent := Row_Get_Parent (Node_Get_Row (Parent));
      end loop;

      return
        Get_Project_View_From_Name
        (Node_Get_Row_Data (Explorer.Tree, Parent).Name);
   end Get_Project_From_Node;

   -------------------------------
   -- Get_Selected_Project_Node --
   -------------------------------

   function Get_Selected_Project_Node
     (Explorer : access Project_Explorer_Record'Class) return Gtk_Ctree_Node
   is
      use type Node_List.Glist;
      Selection : constant Node_List.Glist := Get_Selection (Explorer.Tree);
      N : Gtk_Ctree_Node;
   begin
      if Selection /= Node_List.Null_List then
         N := Node_List.Get_Data (Selection);
         while N /= null loop
            if Node_Get_Row_Data (Explorer.Tree, N).Node_Type =
              Project_Node
            then
               return N;
            end if;

            N := Row_Get_Parent (Node_Get_Row (N));
         end loop;
      end if;
      return null;
   end Get_Selected_Project_Node;

   -------------------
   -- Node_Selected --
   -------------------

   procedure Node_Selected
     (Explorer : access Project_Explorer_Record'Class; Node : Gtk_Ctree_Node)
   is
      File : constant String :=
        Get_File_From_Node (Explorer, Node, Full_Path => True);
      User : constant User_Data := Node_Get_Row_Data (Explorer.Tree, Node);

   begin
      case User.Node_Type is
         when Entity_Node =>
            Open_File_Editor
              (Explorer.Kernel,
               File,
               Line   => User.Sloc_Entity.Line,
               Column => User.Sloc_Entity.Column);

         when File_Node =>
            Open_File_Editor (Explorer.Kernel, File);

         when others =>
            null;

      end case;
   end Node_Selected;

   ----------------------------
   -- File_Selection_Changed --
   ----------------------------

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class)
   is
      T        : constant Project_Explorer := Project_Explorer (Explorer);
      Context  : Selection_Context_Access;
   begin
      Context := Explorer_Context_Factory (T.Kernel, T, T, null, null);

      if Context /= null then
         Set_Context_Information (Context, T.Kernel, Explorer_Module_ID);
         Context_Changed (T.Kernel, Context);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Selection_Changed;

   -------------------------
   -- Find_Iter_For_Event --
   -------------------------

   function Find_Iter_For_Event
     (Explorer : access Project_Explorer_Record'Class;
      Event    : Gdk_Event) return Gtk_Tree_Iter
   is
      X         : Gdouble;
      Y         : Gdouble;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;
      Iter      : Gtk_Tree_Iter := Null_Iter;
      Model     : Gtk_Tree_Model;
   begin
      if Event /= null then
         X := Get_X (Event);
         Y := Get_Y (Event);
         Path := Gtk_New;
         Get_Path_At_Pos
           (Explorer.File_Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path = null then
            return Iter;
         end if;

         Select_Path (Get_Selection (Explorer.File_Tree), Path);
         Iter := Get_Iter (Explorer.File_Model, Path);
         Path_Free (Path);
      else
         Get_Selected (Get_Selection (Explorer.File_Tree),
                       Model,
                       Iter);
      end if;

      return Iter;
   end Find_Iter_For_Event;

   -----------------------
   -- File_Button_Press --
   -----------------------

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T    : constant Project_Explorer := Project_Explorer (Explorer);
      Iter : Gtk_Tree_Iter;
   begin
      if Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (T, Event);

         if Iter /= Null_Iter then
            case Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column))) is

               when Directory_Node =>
                  return False;

               when File_Node =>
                  if (Get_Event_Type (Event) = Gdk_2button_Press
                      or else Get_Event_Type (Event) = Gdk_3button_Press)
                  then
                     Open_File_Editor
                       (T.Kernel,
                        Get_String (T.File_Model, Iter, Absolute_Name_Column));
                     return True;
                  end if;

               when Entity_Node =>

                  declare
                     Val  : GValue;
                     User : User_Data_Access;
                  begin
                     --  ??? the following two lines are due to a possible
                     --  mapping error in GtkAda: I need to call "Unset" on
                     --  Val before calling Get_Value below, otherwise I get
                     --  a critical error saying "cannot init val because it
                     --  was initialized before with value null"... and I need
                     --  to call Init before Unset otherwise I get a similar
                     --  message when calling Unset

                     Init (Val, GType_Pointer);
                     Unset (Val);

                     Get_Value (T.File_Model, Iter, User_Data_Column, Val);
                     User := To_User_Data (Get_Address (Val));

                     Open_File_Editor
                       (T.Kernel,
                        Get_String (T.File_Model, Iter, Absolute_Name_Column),
                        Line   => User.Sloc_Entity.Line,
                        Column => User.Sloc_Entity.Column);

                     Unset (Val);
                  end;

                  return False;

               when others =>
                  return False;
            end case;

         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end File_Button_Press;

   --------------------------
   -- Button_Press_Release --
   --------------------------

   function Button_Press_Release
     (Explorer : access Gtk_Widget_Record'Class; Args : Gtk_Args)
      return Boolean
   is
      use Gtk.Ctree.Row_List;
      T        : constant Project_Explorer := Project_Explorer (Explorer);
      Event    : constant Gdk_Event := To_Event (Args, 1);
      Row      : Gint;
      Column   : Gint;
      Is_Valid : Boolean;
      Node     : Gtk_Ctree_Node;

   begin
      Get_Selection_Info
        (T.Tree, Gint (Get_X (Event)), Gint (Get_Y (Event)),
         Row, Column, Is_Valid);

      if not Is_Valid then
         return False;
      end if;

      if Get_Button (Event) = 1 then
         Node := Node_Nth (T.Tree, Guint (Row));

         declare
            use type Node_List.Glist;
            User : constant User_Data := Node_Get_Row_Data (T.Tree, Node);
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
                  if Get_Event_Type (Event) = Button_Release then
                     Node_Selected (T, Node);
                  end if;
            end case;
         end;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press_Release;

   ----------------------
   -- On_Open_Explorer --
   ----------------------

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : Project_Explorer;
      Child    : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Record'Tag);

      if Child = null then
         Gtk_New (Explorer, Kernel);
         Refresh (Kernel, GObject (Explorer));
         Child := Put (Get_MDI (Kernel), Explorer);
         Set_Title (Child, -"Project Explorer");
         Set_Dock_Side (Child, Left);
         Dock_Child (Child);
      else
         Raise_Child (Child);
         Set_Focus_Child (Get_MDI (Kernel), Child);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Explorer;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access is
   begin
      return Explorer_Context_Factory (Kernel, Child, Child, null, null);
   end Default_Factory;

   -----------------------------
   -- Explorer_Search_Factory --
   -----------------------------

   function Explorer_Search_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Kernel, All_Occurences, Extra_Information);
      Context : Explorer_Search_Context_Access;

   begin
      Context := new Explorer_Search_Context;
      Context.Include_Entities := False;
      return Search_Context_Access (Context);
   end Explorer_Search_Factory;

   ------------
   -- Search --
   ------------

   function Search
     (Context         : access Explorer_Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      pragma Unreferenced (Search_Backward);
      C        : constant Explorer_Search_Context_Access :=
        Explorer_Search_Context_Access (Context);
      Explorer : Project_Explorer;

      procedure Next;
      --  Move to the next node in the tree

      ----------
      -- Next --
      ----------

      procedure Next is
         Tmp : Gtk_Ctree_Node;
      begin
         if not Row_Get_Is_Leaf (Node_Get_Row (C.Current)) then
            if C.Include_Entities
              or else Node_Get_Row_Data
                (Explorer.Tree, C.Current).Node_Type /= File_Node
            then
               Compute_Children (Explorer, C.Current);
               Tmp := Row_Get_Children (Node_Get_Row (C.Current));

               if Tmp /= null then
                  C.Current := Tmp;
                  return;
               end if;
            end if;
         end if;

         loop
            Tmp := Row_Get_Sibling (Node_Get_Row (C.Current));
            exit when Tmp /= null;

            C.Current := Row_Get_Parent (Node_Get_Row (C.Current));
            if C.Current = null then
               return;
            end if;
         end loop;

         C.Current := Tmp;
      end Next;

      Child : MDI_Child;
      Tmp   : Gtk_Ctree_Node;

   begin
      --  ??? Problem: this algorithm will analyze the same files multiple
      --  times if they appear multiple times in the explorer. This might not
      --  be necessary if every time we expand a file we duplicate the entity
      --  information for all the occurences of this file.

      --  Make sure the explorer is visible, and get a handle on it

      On_Open_Explorer (Get_MDI (Kernel), Kernel_Handle (Kernel));
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Record'Tag);
      Explorer := Project_Explorer (Get_Widget (Child));

      --  Find the next matching node

      if C.Current = null then
         C.Current := Node_Nth (Explorer.Tree, 0);
      else
         Next;
      end if;

      Gtk.Handlers.Handler_Block (Explorer.Tree, Explorer.Expand_Id);

      while C.Current /= null loop
         if Match (C, Node_Get_Text (Explorer.Tree, C.Current, 0)) /= -1 then
            Gtk_Select (Explorer.Tree, C.Current);

            if not Is_Viewable (Explorer.Tree, C.Current) then
               Tmp := Row_Get_Parent (Node_Get_Row (C.Current));
               while Tmp /= null loop
                  Gtk.Ctree.Expand (Explorer.Tree, Tmp);
                  Tmp := Row_Get_Parent (Node_Get_Row (Tmp));
               end loop;
            end if;

            if Node_Is_Visible (Explorer.Tree, C.Current) /=
              Visibility_Full
            then
               Node_Moveto (Explorer.Tree, C.Current, 0, 0.5, 0.0);
            end if;

            Gtk.Handlers.Handler_Unblock (Explorer.Tree, Explorer.Expand_Id);
            return True;
         end if;
         Next;
      end loop;

      Gtk.Handlers.Handler_Unblock (Explorer.Tree, Explorer.Expand_Id);
      return False;
   end Search;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Enable_Entity_Search : constant Boolean := False;
      Project : constant String := '/' & (-"Project");
      N       : Node_Ptr;
      Extra   : Explorer_Search_Extra;
      Box     : Gtk_Box;
      Widget  : Gtk_Widget;

   begin
      Register_Module
        (Module                  => Explorer_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Explorer_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null,
         MDI_Child_Tag           => Project_Explorer_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Add a project explorer to the default desktop.
      N := new Node;
      N.Tag := new String'("Project_Explorer");

      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         300, 600,
         "Project Explorer", "Project Explorer",
         Docked, Left,
         True);

      Register_Menu
        (Kernel, Project, -"Explorer", "", On_Open_Explorer'Access);
      Vsearch_Ext.Register_Default_Search (Kernel);

      if Enable_Entity_Search then
         Extra := new Explorer_Search_Extra_Record;
         Gtk.Frame.Initialize (Extra);

         Gtk_New_Vbox (Box, Homogeneous => False);
         Add (Extra, Box);

         Gtk_New (Extra.Include_Entities, -"search entities (might be slow)");
         Pack_Start (Box, Extra.Include_Entities);
         Set_Active (Extra.Include_Entities, False);
         Kernel_Callback.Connect
           (Extra.Include_Entities, "toggled",
            Kernel_Callback.To_Marshaller (Reset_Search'Access),
            Kernel_Handle (Kernel));
         Widget := Gtk_Widget (Extra);
      end if;

      declare
         Name : constant String := -"Project explorer";
      begin
         Register_Search_Function
           (Kernel            => Kernel,
            Data => (Length            => Name'Length,
                     Label             => Name,
                     Factory           => Explorer_Search_Factory'Access,
                     Extra_Information => Widget,
                     Mask              => All_Options and not Supports_Replace
                       and not Search_Backward and not All_Occurences));
      end;
   end Register_Module;

   ----------------
   -- Parse_Path --
   ----------------

   function Parse_Path
     (Path : String) return String_List_Utils.String_List.List
   is
      First : Integer;
      Index : Integer;

      use String_List_Utils.String_List;
      Result : String_List_Utils.String_List.List;

   begin
      First := Path'First;
      Index := First + 1;

      while Index <= Path'Last loop
         if Path (Index) = Path_Separator then
            Append (Result, Path (First .. Index - 1));
            Index := Index + 1;
            First := Index;
         end if;

         Index := Index + 1;
      end loop;

      if First /= Path'Last then
         Append (Result, Path (First .. Path'Last));
      end if;

      return Result;
   end Parse_Path;

   --------------------------
   -- Greatest_Common_Path --
   --------------------------

   function Greatest_Common_Path
     (L : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;

      N : List_Node;
   begin
      if Is_Empty (L) then
         return "";
      end if;

      N := First (L);

      declare
         Greatest_Prefix        : constant String  := Data (N);
         Greatest_Prefix_First  : constant Natural := Greatest_Prefix'First;
         Greatest_Prefix_Length : Natural := Greatest_Prefix'Length;
      begin
         N := Next (N);

         while N /= Null_Node loop
            declare
               Challenger : constant String  := Data (N);
               First      : constant Natural := Challenger'First;
               Index      : Natural := 0;
               Length     : constant Natural := Challenger'Length;
            begin
               while Index < Greatest_Prefix_Length
                 and then Index < Length
                 and then Challenger (First + Index)
                 = Greatest_Prefix (Greatest_Prefix_First + Index)
               loop
                  Index := Index + 1;
               end loop;

               Greatest_Prefix_Length := Index;
            end;

            if Greatest_Prefix_Length <= 1 then
               exit;
            end if;

            N := Next (N);
         end loop;

         if Greatest_Prefix_Length = 0 then
            return (1 => Directory_Separator);
         end if;

         while Greatest_Prefix (Greatest_Prefix'First
                                + Greatest_Prefix_Length - 1)
           /= Directory_Separator
         loop
            Greatest_Prefix_Length := Greatest_Prefix_Length - 1;
         end loop;

         return Greatest_Prefix
           (Greatest_Prefix_First
            .. Greatest_Prefix_First + Greatest_Prefix_Length - 1);
      end;
   end Greatest_Common_Path;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Gtk.Main.Timeout_Handler_Id) is
      pragma Unreferenced (D);
   begin
      null;
   end Free;

end Project_Explorers;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                            AdaCore                                --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Exceptions;            use Ada.Exceptions;

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;
with Gdk.Pixmap;                use Gdk.Pixmap;

with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Tree_View;          use Gtkada.Tree_View;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;

with Namet;                     use Namet;

with Types;                     use Types;

with Basic_Types;
with Projects;                  use Projects;
with Language;                  use Language;
with String_Utils;              use String_Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with Language_Handlers;         use Language_Handlers;
with Traces;                    use Traces;
with Find_Utils;                use Find_Utils;
with File_Utils;                use File_Utils;
with GUI_Utils;                 use GUI_Utils;
with String_List_Utils;
with Histories;                 use Histories;
with VFS;                       use VFS;
with Tooltips;
with Commands.Interactive;      use Commands, Commands.Interactive;

with Entities;
with String_Hash;

with Project_Explorers_Common;  use Project_Explorers_Common;

package body Project_Explorers is

   Me : constant Debug_Handle := Create ("Project_Explorers");

   type Explorer_Module_Record is new Module_ID_Record with null record;
   Explorer_Module_ID : Module_ID := null;
   --  Id for the explorer module

   Explorers_Tooltips : constant Debug_Handle :=
     Create ("Explorers.Tooltips", Off);

   Show_Absolute_Paths : constant History_Key :=
     "explorer-show-absolute-paths";
   Show_Flat_View : constant History_Key :=
     "explorer-show-flat-view";

   Projects_Before_Directories : constant Boolean := False;
   --  <preference> True if the projects should be displayed, when sorted,
   --  before the directories in the project view.

   procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget);
   --  See inherited documentation

   ---------------
   -- Searching --
   ---------------

   type Search_Status is new Integer;
   --  Values stored in the String_Status hash table:
   --    - n: the entry or one of its children matches. n is the number of
   --         children that potentially matches (ie that have an entry set to
   --         n or -1
   --    - 0: the node doesn't match and neither do its children.
   --    - -1: the entry hasn't been examined yet

   Search_Match : constant Search_Status := 1;
   No_Match     : constant Search_Status := 0;
   Unknown      : constant Search_Status := -1;

   procedure Nop (X : in out Search_Status);
   --  Do nothing, required for instantiation of string_boolean_hash

   package String_Status_Hash is new String_Hash
     (Data_Type => Search_Status,
      Free_Data => Nop,
      Null_Ptr  => No_Match);
   use String_Status_Hash;
   use String_Status_Hash.String_Hash_Table;

   type Explorer_Search_Context is new Search_Context with record
      Current             : Gtk_Tree_Iter := Null_Iter;
      Include_Entities    : Boolean;
      Include_Projects    : Boolean;
      Include_Directories : Boolean;
      Include_Files       : Boolean;

      Matches             : String_Status_Hash.String_Hash_Table.HTable;
      --  The search is performed on the internal Ada structures first, and for
      --  each matching project, directory or file, an entry is made in this
      --  table (set to true). This then speeds up the traversing of the tree
      --  to find the matching entities.
   end record;
   type Explorer_Search_Context_Access is access all Explorer_Search_Context;

   procedure Free (Context : in out Explorer_Search_Context);
   --  Free the memory allocated for Context

   type Explorer_Search_Extra_Record is new Gtk_Frame_Record with record
      Include_Entities    : Gtk_Check_Button;
      Include_Projects    : Gtk_Check_Button;
      Include_Directories : Gtk_Check_Button;
      Include_Files       : Gtk_Check_Button;
   end record;
   type Explorer_Search_Extra is access all Explorer_Search_Extra_Record'Class;

   function Explorer_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Create a new search context for the explorer

   function Explorer_Search_Factory
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Include_Projects : Boolean;
      Include_Files    : Boolean)
      return Search_Context_Access;
   --  Create a new search context for the explorer. Only one occurence is
   --  searched, and only in Projects or Files, depending on the parameters.

   procedure Search
     (Context         : access Explorer_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           :  out Boolean;
      Continue        : out Boolean);
   --  Search the next occurrence in the explorer

   --------------
   -- Tooltips --
   --------------

   type Explorer_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Explorer : Project_Explorer_Access;
   end record;
   type Explorer_Tooltips_Access is access all Explorer_Tooltips'Class;
   procedure Draw
     (Tooltip : access Explorer_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentatoin

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   ------------------
   -- Adding nodes --
   ------------------

   procedure Add_Or_Update_Flat_View_Root_Node
     (Explorer : access Project_Explorer_Record'Class);
   --  Add the root node for the flat view.

   function Add_Project_Node
     (Explorer     : access Project_Explorer_Record'Class;
      Project      : Project_Type;
      Parent_Node  : Gtk_Tree_Iter := Null_Iter;
      Name_Suffix  : String := "") return Gtk_Tree_Iter;
   --  Add a new project node in the tree.
   --  Name_Suffix is added to the node's name in addition to the project name.
   --  Parent_Node is the parent of the project in the tree. If this is null,
   --  the new node is added at the root level of the tree.
   --  The new node is initially closed, and its contents will only be
   --  initialized when the node is opened by the user.

   procedure Compute_Children
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter);
   --  Compute the children of Node, if they haven't been computed yet.

   procedure Add_Object_Directories
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter;
      Project  : Project_Type);
   --  Add the object and exec directory nodes for Node. They are added
   --  unconditionally

   procedure Set_Directory_Node_Attributes
     (Explorer  : access Project_Explorer_Record'Class;
      Directory : String;
      Node      : Gtk_Tree_Iter;
      Project   : Project_Type;
      Node_Type : Directory_Node_Types);
   --  Set the attributes in tree model for a directory node

   procedure Add_Files_In_Directory
     (Explorer         : access Project_Explorer_Record'Class;
      Directory        : String;
      Files_In_Project : File_Array_Access;
      Node             : Gtk_Tree_Iter);
   --  Add the files that belong to the directory

   ---------------------
   -- Expanding nodes --
   ---------------------

   procedure Expand_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter);
   --  Expand a project node, ie add children for all the imported projects,
   --  the directories, ...

   procedure Expand_File_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter);
   --  Expand a file node, ie add children for all the entities defined in the
   --  file.

   procedure Expand_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Collapse_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a node is collapsed

   function Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   --------------------
   -- Updating nodes --
   --------------------

   procedure Update_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files    : File_Array_Access;
      Node     : Gtk_Tree_Iter);
   --  Recompute the directories for the project.

   procedure Update_Directory_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Files_In_Project : File_Array_Access;
      Node             : Gtk_Tree_Iter);
   --  Recompute the files for the directory. This procedure tries to keep the
   --  existing files if they are in the project view, so as to keep the
   --  expanded status
   --  Data must be the user data associated with Node

   procedure Update_Directory_Node_Text
     (Explorer : access Project_Explorer_Record'Class;
      Project  : Project_Type;
      Node     : Gtk_Tree_Iter);
   --  Set the text to display for this directory node

   procedure Update_Absolute_Paths
     (Explorer : access Gtk_Widget_Record'Class);
   --  Update the text for all directory nodes in the tree, mostly after the
   --  "show absolute path" setting has changed.

   procedure Update_Flat_View
     (Explorer : access Gtk_Widget_Record'Class);
   --  Update the tree when "show flat view" setting has changed.

   ----------------------------
   -- Retrieving information --
   ----------------------------

   procedure Update_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Node             : Gtk_Tree_Iter;
      Files_In_Project : File_Array_Access;
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

   procedure Refresh
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Refresh the contents of the tree after the project view has changed.
   --  This procedure tries to keep as many things as possible in the current
   --  state (expanded nodes,...)

   type Refresh_Hook_Record is new Function_No_Args with record
      Explorer : Project_Explorer;
   end record;
   type Refresh_Hook is access all Refresh_Hook_Record'Class;
   procedure Execute
     (Hook   : Refresh_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   type Project_Changed_Hook_Record is new Function_No_Args with record
      Explorer : Project_Explorer;
   end record;
   type Project_Hook is access all Project_Changed_Hook_Record'Class;
   procedure Execute
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project as changed, as opposed to the project view.
   --  This means we need to start up with a completely new tree, no need to
   --  try to keep the current one.

   procedure Jump_To_Node
     (Explorer    : Project_Explorer;
      Target_Node : Gtk_Tree_Iter);
   --  Select Target_Node, and make sure it is visible on the screen

   procedure Explorer_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu);
   --  Return the context to use for the contextual menu.
   --  It is also used to return the context for
   --  GPS.Kernel.Get_Current_Context, and thus can be called with a null
   --  event or a null menu.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one.

   function Get_Or_Create_Project_View
     (Kernel : access Kernel_Handle_Record'Class) return Project_Explorer;
   --  Make sure a project view exists, and raise it.

   procedure Child_Selected
     (Explorer : access Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a new child is selected in the MDI. This makes sure
   --  that the selected node in the explorer doesn't reflect false information

   procedure On_Parse_Xref (Explorer : access Gtk_Widget_Record'Class);
   --  Parse all the LI information contained in the selected project.

   function Get_Imported_Projects
     (Project         : Project_Type;
      Direct_Only     : Boolean := True;
      Include_Project : Boolean := False)
      return Project_Type_Array;
   --  Return the list of imported projects as an array.
   --  If Include_Project is False, then Project itself will not be included in
   --  the returned array

   --------------
   -- Commands --
   --------------

   type Locate_File_In_Explorer_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Locate_File_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Locate_Project_In_Explorer_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Locate_Project_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -------------
   -- Filters --
   -------------

   type Project_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type Directory_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type File_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type Entity_Node_Filter_Record is new Action_Filter_Record
      with null record;
   function Filter_Matches_Primitive
     (Context : access Project_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   function Filter_Matches_Primitive
     (Context : access Directory_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   function Filter_Matches_Primitive
     (Context : access File_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   function Filter_Matches_Primitive
     (Context : access Entity_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Context : access Project_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
        and then Has_Project_Information (Ctxt)
        and then not Has_Directory_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Context : access Directory_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
        and then Has_Directory_Information (Ctxt)
        and then not Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Context : access File_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
        and then Has_File_Information (Ctxt)
        and then not Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Context : access Entity_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
        and then Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Project_Explorer;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Explorer := new Project_Explorer_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      --  If expanding/collapsing, don't handle  button clicks
      if T.Expanding then
         T.Expanding := False;
         return False;
      else
         return On_Button_Press
           (T.Kernel,
            MDI_Explorer_Child (Find_MDI_Child (Get_MDI (T.Kernel), T)),
            T.Tree, T.Tree.Model, Event, True);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   ------------------------
   -- Tree_Select_Row_Cb --
   ------------------------

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues)
   is
      pragma Unreferenced (Args);
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      Context_Changed (T.Kernel);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end Tree_Select_Row_Cb;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Label    : Gtk_Label;
      H1       : Refresh_Hook;
      H2       : Project_Hook;
      Tooltip  : Explorer_Tooltips_Access;

   begin
      Initialize_Vbox (Explorer, Homogeneous => False);
      Explorer.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Gtk_New (Label, -"Project View");
      Pack_Start (Explorer, Scrolled, Fill => True, Expand => True);

      Init_Graphics;
      Gtk_New (Explorer.Tree, Columns_Types);
      Set_Headers_Visible (Explorer.Tree, False);
      Set_Column_Types (Gtk_Tree_View (Explorer.Tree));

      Add (Scrolled, Explorer.Tree);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.Tree,
         Object          => Explorer,
         ID              => Explorer_Module_ID,
         Context_Func    => Explorer_Context_Factory'Access);

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Explorer.Expand_Id := Widget_Callback.Object_Connect
        (Explorer.Tree, "row_expanded", Expand_Tree_Cb'Access, Explorer);
      Widget_Callback.Object_Connect
        (Explorer.Tree, "row_collapsed", Collapse_Tree_Cb'Access, Explorer);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         "button_release_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);

      Widget_Callback.Object_Connect
        (Get_Selection (Explorer.Tree), "changed",
         Tree_Select_Row_Cb'Access, Explorer, After => True);

      --  Automatic update of the tree when the project changes
      H1 := new Refresh_Hook_Record'
        (Function_No_Args with Explorer => Project_Explorer (Explorer));
      Add_Hook
        (Kernel, Project_View_Changed_Hook, H1,
         Name => "explorer.project_view_changed", Watch => GObject (Explorer));

      H2 := new Project_Changed_Hook_Record'
        (Function_No_Args with Explorer => Project_Explorer (Explorer));
      Add_Hook
        (Kernel, Project_Changed_Hook, H2,
         Name => "explorer.project_changed", Watch => GObject (Explorer));

      --  The explorer (project view) is automatically refreshed when the
      --  project view is changed.

      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), "child_selected",
         Child_Selected'Access, Explorer, After => True);

      Gtk.Dnd.Dest_Set
        (Explorer.Tree, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree, "drag_data_received",
         Drag_Data_Received'Access, Kernel_Handle (Kernel));

      --  Initialize tooltips

      Tooltip := new Explorer_Tooltips;
      Tooltip.Explorer := Project_Explorer_Access (Explorer);
      Set_Tooltip (Tooltip, Explorer.Tree, 250);
   end Initialize;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected
     (Explorer : access Gtk_Widget_Record'Class; Args : GValues)
   is
      E     : constant Project_Explorer := Project_Explorer (Explorer);
      Child : constant MDI_Child := MDI_Child (To_Object (Args, 1));
      Model : Gtk_Tree_Model;
      Node  : Gtk_Tree_Iter;
   begin
      Get_Selected (Get_Selection (E.Tree), Model, Node);

      if Node = Null_Iter
        or else Child = null
        or else (Get_Title (Child) = " ")
        or else (Get_Title (Child) =
                   Full_Name (Get_File_From_Node (E.Tree.Model, Node)).all)
      then
         return;
      end if;

      if Child = null
        or else not (Get_Widget (Child).all in Project_Explorer_Record'Class)
      then
         Unselect_All (Get_Selection (E.Tree));
      end if;
   end Child_Selected;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Explorer : Project_Explorer;
   begin
      if Node.Tag.all = "Project_Explorer_Project" then
         Explorer := Get_Or_Create_Project_View (User);
         return Find_MDI_Child (Get_MDI (User), Explorer);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Project_Explorer_Record'Class then
         N := new Node;
         N.Tag := new String'("Project_Explorer_Project");
         return N;
      end if;

      return null;
   end Save_Desktop;

   -------------------
   -- On_Parse_Xref --
   -------------------

   procedure On_Parse_Xref (Explorer : access Gtk_Widget_Record'Class) is
      E     : constant Project_Explorer := Project_Explorer (Explorer);
      Node  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

   begin
      Get_Selected (Get_Selection (E.Tree), Model, Node);

      Push_State (E.Kernel, Busy);
      Parse_All_LI_Information
        (E.Kernel,
         Project => Get_Project_From_Node
           (E.Tree.Model, E.Kernel, Node, False),
         Recursive => False);
      Pop_State (E.Kernel);

   exception
      when Exc : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (Exc));
         Pop_State (E.Kernel);
   end On_Parse_Xref;

   ------------------------------
   -- Explorer_Context_Factory --
   ------------------------------

   procedure Explorer_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget, Object);

      --  "Object" is also the explorer, but this way we make sure the current
      --  context is that of the explorer (since it will have the MDI focus)
      T         : constant Project_Explorer :=
                    Get_Or_Create_Project_View (Kernel);
      Iter      : constant Gtk_Tree_Iter :=
                    Find_Iter_For_Event (T.Tree, T.Tree.Model, Event);
      Item      : Gtk_Menu_Item;
      Check     : Gtk_Check_Menu_Item;
      Path      : Gtk_Tree_Path;
      Node_Type : Node_Types;
   begin
      if Iter /= Null_Iter then
         Path := Get_Path (T.Tree.Model, Iter);
         if not Path_Is_Selected (Get_Selection (T.Tree), Path) then
            Set_Cursor (T.Tree, Path, null, False);
         end if;
         Path_Free (Path);
         Node_Type := Get_Node_Type (T.Tree.Model, Iter);
      else
         return;
      end if;

      Project_Explorers_Common.Context_Factory
        (Context, Kernel_Handle (Kernel), T.Tree, T.Tree.Model, Event, Menu);

      if Menu /= null then
         Gtk_New (Check, Label => -"Show absolute paths");
         Associate
           (Get_History (Kernel).all, Show_Absolute_Paths, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled", Update_Absolute_Paths'Access,
            Slot_Object => T);

         Gtk_New (Check, Label => -"Show flat view");
         Associate
           (Get_History (Kernel).all, Show_Flat_View, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled", Update_Flat_View'Access,
            Slot_Object => T);
      end if;

      if (Node_Type = Project_Node
          or else Node_Type = Extends_Project_Node)
        and then Menu /= null
      then
         Gtk_New (Item, -"Parse all xref information");
         Add (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate", On_Parse_Xref'Access, T);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end Explorer_Context_Factory;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      --  Destroy all the items in the tree.
      --  The next call to refresh via the "project_view_changed" signal will
      --  completely restore the tree.

      Clear (Hook.Explorer.Tree.Model);
   end Execute;

   ----------------------
   -- Add_Project_Node --
   ----------------------

   function Add_Project_Node
     (Explorer    : access Project_Explorer_Record'Class;
      Project     : Project_Type;
      Parent_Node : Gtk_Tree_Iter := Null_Iter;
      Name_Suffix : String := "") return Gtk_Tree_Iter
   is
      N         : Gtk_Tree_Iter;
      Ref       : Gtk_Tree_Iter := Null_Iter;
      Is_Leaf   : constant Boolean :=
        not Has_Imported_Projects (Project)
        and then Get_Attribute_Value (Project, Obj_Dir_Attribute) = ""
        and then Source_Dirs (Project)'Length = 0;
      Node_Type : Node_Types := Project_Node;
      Node_Text : constant String := Project_Name (Project);

   begin
      if Project = No_Project then
         return Null_Iter;
      end if;

      if Extending_Project (Project) /= No_Project then
         Node_Type := Extends_Project_Node;

      elsif Project_Modified (Project) then
         Node_Type := Modified_Project_Node;
      end if;

      if Parent_Node /= Null_Iter then
         Ref := Children (Explorer.Tree.Model, Parent_Node);

         --  Insert the nodes sorted.

         while Ref /= Null_Iter
           and then
             (Get_Node_Type (Explorer.Tree.Model, Ref) /= Project_Node
              or else Get_String (Explorer.Tree.Model, Ref, Base_Name_Column)
                < Node_Text)
         loop
            Next (Explorer.Tree.Model, Ref);
         end loop;
      end if;

      if Ref = Null_Iter then
         Append (Explorer.Tree.Model, N, Parent_Node);
      else
         Insert_Before (Explorer.Tree.Model, N, Parent_Node, Ref);
      end if;

      Set (Explorer.Tree.Model, N, Absolute_Name_Column,
           Full_Name (Project_Path (Project)).all);

      if Extending_Project (Project) /= No_Project then
         --  ??? We could use a different icon instead
         Set (Explorer.Tree.Model, N, Base_Name_Column,
              Locale_To_UTF8 (Node_Text) & " (extended)" & Name_Suffix);
      else
         Set (Explorer.Tree.Model, N, Base_Name_Column,
              Locale_To_UTF8 (Node_Text & Name_Suffix));
      end if;

      Set_Node_Type (Explorer.Tree.Model, N, Node_Type, False);

      Set (Explorer.Tree.Model, N, Project_Column,
           Gint (Name_Id'(Project_Name (Project))));

      if not Is_Leaf then
         Append_Dummy_Iter (Explorer.Tree.Model, N);
      end if;

      return N;
   end Add_Project_Node;

   --------------------------------
   -- Update_Directory_Node_Text --
   --------------------------------

   procedure Update_Directory_Node_Text
     (Explorer : access Project_Explorer_Record'Class;
      Project  : Project_Type;
      Node     : Gtk_Tree_Iter)
   is
      Node_Text : constant String :=
                    Get_String
                      (Explorer.Tree.Model, Node, Absolute_Name_Column);
   begin
      if Get_History
        (Get_History (Explorer.Kernel).all, Show_Absolute_Paths)
      then
         Set (Explorer.Tree.Model, Node, Base_Name_Column, Node_Text);
      else
         Set (Explorer.Tree.Model, Node, Base_Name_Column,
              Relative_Path_Name
                (Node_Text, Dir_Name (Project_Path (Project)).all));
      end if;
   end Update_Directory_Node_Text;

   ---------------------------
   -- Update_Absolute_Paths --
   ---------------------------

   procedure Update_Absolute_Paths
     (Explorer : access Gtk_Widget_Record'Class)
   is
      Exp : constant Project_Explorer := Project_Explorer (Explorer);
      procedure Process_Node (Iter : Gtk_Tree_Iter; Project : Project_Type);
      --  Recursively process node

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Iter : Gtk_Tree_Iter; Project : Project_Type) is
         It : Gtk_Tree_Iter := Children (Exp.Tree.Model, Iter);
      begin
         while It /= Null_Iter loop
            case Get_Node_Type (Exp.Tree.Model, It) is
               when Project_Node | Extends_Project_Node =>
                  Process_Node
                    (It, Get_Project_From_Node
                     (Exp.Tree.Model, Exp.Kernel, It, False));

               when Directory_Node
                 | Obj_Directory_Node
                 | Exec_Directory_Node =>
                  Update_Directory_Node_Text (Exp, Project, It);

               when others =>
                  null;
            end case;

            Next (Exp.Tree.Model, It);
         end loop;
      end Process_Node;

      Iter : Gtk_Tree_Iter := Get_Iter_First (Exp.Tree.Model);
   begin
      --  There can be multiple toplevel nodes in the case of the flat view
      while Iter /= Null_Iter loop
         Process_Node (Iter, Get_Project (Exp.Kernel));
         Next (Exp.Tree.Model, Iter);
      end loop;
   end Update_Absolute_Paths;

   ----------------------
   -- Update_Flat_View --
   ----------------------

   procedure Update_Flat_View
     (Explorer : access Gtk_Widget_Record'Class)
   is
      Tree : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      Clear (Tree.Tree.Model);
      Refresh (Explorer);
   end Update_Flat_View;

   -----------------------------------
   -- Set_Directory_Node_Attributes --
   -----------------------------------

   procedure Set_Directory_Node_Attributes
     (Explorer  : access Project_Explorer_Record'Class;
      Directory : String;
      Node      : Gtk_Tree_Iter;
      Project   : Project_Type;
      Node_Type : Directory_Node_Types) is
   begin
      Set (Explorer.Tree.Model, Node, Absolute_Name_Column,
           Locale_To_UTF8 (Name_As_Directory (Directory)));

      Update_Directory_Node_Text (Explorer, Project, Node);

      Set_Node_Type (Explorer.Tree.Model, Node, Node_Type, False);
      Set (Explorer.Tree.Model, Node, Up_To_Date_Column, False);
   end Set_Directory_Node_Attributes;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Explorer_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Font       : Pango_Font_Description;
      Window     : Gdk.Window.Gdk_Window;
      New_Window : Gdk_Window;
      Mask       : Gdk_Modifier_Type;

      X, Y       : Gint;
      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean := False;
      Par, Iter  : Gtk_Tree_Iter;
      Node_Type  : Node_Types;

      Text       : String_Access;
   begin
      Pixmap := null;
      Area   := (0, 0, 0, 0);

      Window := Get_Bin_Window (Tooltip.Explorer.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      --  ??? By default, return immediately (Row_Found = False) since tooltips
      --  in the explorer are too intrusive: they will appear on top of other
      --  windows, and contextual menus.

      if Active (Explorers_Tooltips) then
         Get_Path_At_Pos
           (Tooltip.Explorer.Tree, X, Y, Path,
            Column, Cell_X, Cell_Y, Row_Found);
      end if;

      if not Row_Found then
         return;

      else
         --  Now check that the cursor is over a text

         Iter := Get_Iter (Tooltip.Explorer.Tree.Model, Path);

         declare
            Str     : constant String := Get_String
              (Tooltip.Explorer.Tree.Model, Iter, Base_Name_Column);
            S_Icon  : constant Gint := 15; -- size used for the icon
            S_Level : constant Gint := 12; -- size used for each indent level
            --  ??? S_Icon and S_Level have been computed experimentally. It is
            --  maybe possible to get the proper values from the Tree_View.
            Layout  : Pango_Layout;
            Width   : Gint;
            Height  : Gint;
         begin
            Font := Get_Pref (Default_Font);
            Layout := Create_Pango_Layout (Tooltip.Explorer, Str);
            Set_Font_Description (Layout, Font);
            Get_Pixel_Size (Layout, Width, Height);

            if Cell_X > S_Icon + (Get_Depth (Path) * S_Level) + Width + 10 then
               return;
            end if;
         end;
      end if;

      Get_Cell_Area (Tooltip.Explorer.Tree, Path, Column, Area);

      Path_Free (Path);

      Node_Type := Get_Node_Type (Tooltip.Explorer.Tree.Model, Iter);

      case Node_Type is
         when Project_Node | Extends_Project_Node =>
            --  Project or extended project full pathname
            Text := new String'
              (Get_String
                 (Tooltip.Explorer.Tree.Model, Iter, Absolute_Name_Column));

         when Directory_Node | Obj_Directory_Node | Exec_Directory_Node =>
            --  Directroy full pathname and project name
            --  Get parent node which is the project name
            Par := Parent (Tooltip.Explorer.Tree.Model, Iter);

            Text := new String'
              (Get_String
                 (Tooltip.Explorer.Tree.Model, Iter, Absolute_Name_Column) &
               ASCII.LF &
               (-"in project ") &
               Get_String
                 (Tooltip.Explorer.Tree.Model, Par, Base_Name_Column));

         when File_Node =>
            --  Base filename and Project name
            --  Get grand-parent node which is the project node
            Par := Parent
              (Tooltip.Explorer.Tree.Model,
               Parent (Tooltip.Explorer.Tree.Model, Iter));

            Text := new String'
              (Get_String (Tooltip.Explorer.Tree.Model, Iter, Base_Name_Column)
               & ASCII.LF &
               (-"in project ") &
               Get_String
                 (Tooltip.Explorer.Tree.Model, Par, Base_Name_Column));

         when Entity_Node =>
            --  Entity (parameters) declared at Filename:line
            --  Get grand-parent node which is the filename node
            Par := Parent
              (Tooltip.Explorer.Tree.Model,
               Parent (Tooltip.Explorer.Tree.Model, Iter));

            Text := new String'
              (Get_String (Tooltip.Explorer.Tree.Model, Iter, Base_Name_Column)
               & ASCII.LF &
               (-"declared at ") &
               Get_String (Tooltip.Explorer.Tree.Model, Par, Base_Name_Column)
               & ':' &
               Image (Integer
                 (Get_Int (Tooltip.Explorer.Tree.Model, Iter, Line_Column))));

         when others =>
            null;
      end case;

      if Text /= null then
         Create_Pixmap_From_Text
           (Text.all,
            Font, Get_Pref (Tooltip_Color),
            Tooltip.Explorer.Tree,
            Pixmap);
         Free (Text);
      end if;
   end Draw;

   -------------------------
   -- Expand_Project_Node --
   -------------------------

   procedure Expand_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter)
   is
      Project : constant Project_Type := Get_Project_From_Node
        (Explorer.Tree.Model, Explorer.Kernel, Node, False);
      Files   : File_Array_Access :=
                  Get_Source_Files (Project, Recursive => False);
   begin
      Update_Project_Node (Explorer, Files, Node);
      Unchecked_Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Explorer.Kernel);
   end Expand_Project_Node;

   ----------------------------
   -- Add_Object_Directories --
   ----------------------------

   procedure Add_Object_Directories
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter;
      Project  : Project_Type)
   is
      Obj  : constant String :=
               Name_As_Directory (Object_Path (Project, False));
      Exec : constant String := Executables_Directory (Project);

      function Create_Object_Dir (Node : Gtk_Tree_Iter) return Gtk_Tree_Iter;
      --  Create a node for display of the object directory

      -----------------------
      -- Create_Object_Dir --
      -----------------------

      function Create_Object_Dir (Node : Gtk_Tree_Iter) return Gtk_Tree_Iter is
         N   : Gtk_Tree_Iter;
         Ref : Gtk_Tree_Iter := Children (Explorer.Tree.Model, Node);
      begin
         while Ref /= Null_Iter loop
            if Get_Node_Type (Explorer.Tree.Model, Ref) /= Directory_Node then
               Insert_Before (Explorer.Tree.Model, N, Node, Ref);
               return N;
            end if;

            Next (Explorer.Tree.Model, Ref);
         end loop;

         Append (Explorer.Tree.Model, N, Node);
         return N;
      end Create_Object_Dir;

   begin
      if Obj /= "" then
         Set_Directory_Node_Attributes
           (Explorer  => Explorer,
            Directory => Obj,
            Node      => Create_Object_Dir (Node),
            Project   => Project,
            Node_Type => Obj_Directory_Node);
      end if;

      if Exec /= "" and then Exec /= Obj then
         Set_Directory_Node_Attributes
           (Explorer  => Explorer,
            Directory => Exec,
            Node      => Create_Object_Dir (Node),
            Project   => Project,
            Node_Type => Exec_Directory_Node);
      end if;
   end Add_Object_Directories;

   ----------------------
   -- Expand_File_Node --
   ----------------------

   procedure Expand_File_Node
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter)
   is
      File_Name : constant Virtual_File :=
        Get_File_From_Node (Explorer.Tree.Model, Node);
   begin
      Append_File_Info (Explorer.Kernel, Explorer.Tree.Model, Node, File_Name);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Expand_File_Node;

   ----------------------
   -- Compute_Children --
   ----------------------

   procedure Compute_Children
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter)
   is
      type Boolean_Categories is array (Language_Category) of Boolean;
      Expansion : Boolean_Categories;
      N, File   : Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
      Dummy     : Boolean;
      pragma Unreferenced (Dummy);

   begin
      --  If the node is not already up-to-date

      if not Is_Up_To_Date (Explorer.Tree.Model, Node) then
         --  Remove the dummy node, and report that the node is up-to-date

         N := Children (Explorer.Tree.Model, Node);
         Remove (Explorer.Tree.Model, N);

         Set (Explorer.Tree.Model, Node, Up_To_Date_Column, True);

         case Get_Node_Type (Explorer.Tree.Model, Node) is
            when Project_Node | Modified_Project_Node =>
               Expand_Project_Node (Explorer, Node);

            when Extends_Project_Node =>
               Expand_Project_Node (Explorer, Node);

            when Directory_Node | Obj_Directory_Node | Exec_Directory_Node =>
               null;

            when File_Node =>
               Expand_File_Node (Explorer, Node);

            when Category_Node =>
               --  If this is not up-to-date, we need to recompute at the
               --  file_node level, but keep the other category nodes in their
               --  expanded state.

               --  Memorize the current expansion state for the categories

               Expansion := (others => False);

               File := Parent (Explorer.Tree.Model, Node);
               N := Children (Explorer.Tree.Model, File);

               while N /= Null_Iter loop
                  Path := Get_Path (Explorer.Tree.Model, N);
                  Expansion (Get_Category_Type (Explorer.Tree.Model, N)) :=
                    Row_Expanded (Explorer.Tree, Path);
                  Path_Free (Path);
                  Next (Explorer.Tree.Model, N);
               end loop;

               --  Recompute the file node (closing and expanding it again will
               --  generate the appropriate callbacks, and this is taken care
               --  of automatically)

               Path := Get_Path (Explorer.Tree.Model, File);
               Dummy := Collapse_Row (Explorer.Tree, Path);
               Compute_Children (Explorer, File);
               Dummy := Expand_Row (Explorer.Tree, Path, False);
               Path_Free (Path);

               --  Restore the state of the categories

               N := Children (Explorer.Tree.Model, File);

               while N /= Null_Iter loop
                  if Expansion
                    (Get_Category_Type (Explorer.Tree.Model, N))
                  then
                     Path := Get_Path (Explorer.Tree.Model, N);
                     Dummy := Expand_Row (Explorer.Tree, Path, False);
                     Path_Free (Path);
                  end if;

                  Next (Explorer.Tree.Model, N);
               end loop;

            when Entity_Node =>
               --   These are leaf nodes, so don't have children
               null;
         end case;

      else
         Update_Node (Explorer, Node, null, Force_Expanded => True);
      end if;
   end Compute_Children;

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues)
   is
      Margin    : constant Gint := 30;
      T         : constant Project_Explorer := Project_Explorer (Explorer);
      Path      : constant Gtk_Tree_Path :=
                    Gtk_Tree_Path (Get_Proxy (Nth (Args, 2)));
      Node      : constant Gtk_Tree_Iter := Get_Iter (T.Tree.Model, Path);
      Success   : Boolean;
      pragma Unreferenced (Success);
      Area_Rect : Gdk_Rectangle;
      Path2     : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
   begin
      if T.Expanding then
         return;
      end if;

      T.Expanding := True;

      Compute_Children (T, Node);

      Success := Expand_Row (T.Tree, Path, False);

      Set_Node_Type
        (T.Tree.Model,
         Node,
         Get_Node_Type (T.Tree.Model, Node),
         True);

      if Realized_Is_Set (T.Tree) then
         Iter := Children (T.Tree.Model, Node);

         if Iter /= Null_Iter then
            Path2 := Get_Path (T.Tree.Model, Iter);
            Get_Cell_Area (T.Tree, Path2, Get_Column (T.Tree, 0), Area_Rect);
            if Area_Rect.Y > Get_Allocation_Height (T.Tree) - Margin then
               Scroll_To_Cell
                 (T.Tree,
                  Path, null, True,
                  0.1, 0.1);
            end if;
            Path_Free (Path2);
         end if;
      end if;

      --  Set back the cursor to the parent node.
      Set_Cursor (T.Tree, Path, null, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Expand_Tree_Cb;

   ----------------------
   -- Collapse_Tree_Cb --
   ----------------------

   procedure Collapse_Tree_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues)
   is
      E    : constant Project_Explorer := Project_Explorer (Explorer);
      Path : constant Gtk_Tree_Path :=
               Gtk_Tree_Path (Get_Proxy (Nth (Args, 2)));
      Node : constant Gtk_Tree_Iter := Get_Iter (E.Tree.Model, Path);

   begin
      --  Set expanding state to prevent automatic drag & drop start
      E.Expanding := True;

      --  Redraw the pixmap.

      Set_Node_Type
        (E.Tree.Model,
         Node,
         Get_Node_Type (E.Tree.Model, Node),
         False);
   end Collapse_Tree_Cb;

   ---------------------------
   -- Get_Imported_Projects --
   ---------------------------

   function Get_Imported_Projects
     (Project         : Project_Type;
      Direct_Only     : Boolean := True;
      Include_Project : Boolean := False)
      return Project_Type_Array
   is
      Count : Natural := 0;
      Iter  : Imported_Project_Iterator := Start
        (Project, Recursive => True, Direct_Only => Direct_Only);
   begin
      while Current (Iter) /= No_Project loop
         Count := Count + 1;
         Next (Iter);
      end loop;

      declare
         Imported : Project_Type_Array (1 .. Count);
         Index    : Natural := Imported'First;
      begin
         Iter := Start
           (Project, Recursive => True, Direct_Only => Direct_Only);
         while Current (Iter) /= No_Project loop
            --  In some cases, we get the project itself in the list of its
            --  dependencies (???). For instance, doing a search in the
            --  explorer if we didn't have this test would duplicate the
            --  project node.
            if Include_Project or else Current (Iter) /= Project then
               Imported (Index) := Current (Iter);
               Index := Index + 1;
            end if;
            Next (Iter);
         end loop;

         return Imported (Imported'First .. Index - 1);
      end;
   end Get_Imported_Projects;

   ----------------------------
   -- Add_Files_In_Directory --
   ----------------------------

   procedure Add_Files_In_Directory
     (Explorer         : access Project_Explorer_Record'Class;
      Directory        : String;
      Files_In_Project : File_Array_Access;
      Node             : Gtk_Tree_Iter)
   is
      Files : File_Array (Files_In_Project'Range);
      Index : Integer := Files'First;
   begin
      for S in Files_In_Project'Range loop
         if File_Equal (Dir_Name (Files_In_Project (S)).all, Directory) then
            Files (Index) := Files_In_Project (S);
            Index := Index + 1;
         end if;
      end loop;

      Sort (Files (Files'First .. Index - 1));

      for S in Files'First .. Index - 1 loop
         Append_File
           (Kernel => Explorer.Kernel,
            Model  => Explorer.Tree.Model,
            File   => Files (S),
            Base   => Node);
      end loop;

      Set (Explorer.Tree.Model, Node, Up_To_Date_Column, True);
   end Add_Files_In_Directory;

   -------------------------
   -- Update_Project_Node --
   -------------------------

   procedure Update_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files    : File_Array_Access;
      Node     : Gtk_Tree_Iter)
   is
      procedure Add_Projects;
      --  Add the subprojects that weren't present for the previous view

      procedure Add_Remaining_Source_Dirs;
      --  Add all remaining source directories

      N, N2 : Gtk_Tree_Iter;
      use String_List_Utils.String_List;
      Index    : Natural;
      Prj      : Project_Type;
      Dirs     : String_List_Utils.String_List.List;
      Dir_Node : String_List_Utils.String_List.List_Node;
      Project  : constant Project_Type := Get_Project_From_Node
        (Explorer.Tree.Model, Explorer.Kernel, Node, False);
      Sources  : constant Name_Id_Array := Source_Dirs (Project);
      Imported : Project_Type_Array := Get_Imported_Projects (Project);

      ------------------
      -- Add_Projects --
      ------------------

      procedure Add_Projects is
         N : Gtk_Tree_Iter;
         pragma Unreferenced (N);
      begin
         if not Get_History
           (Get_History (Explorer.Kernel).all, Show_Flat_View)
         then
            for P in Imported'Range loop
               if Imported (P) /= No_Project then
                  N := Add_Project_Node
                    (Explorer, Project => Imported (P), Parent_Node => Node);
               end if;
            end loop;
         end if;
      end Add_Projects;

      -------------------------------
      -- Add_Remaining_Source_Dirs --
      -------------------------------

      procedure Add_Remaining_Source_Dirs is
      begin
         while Dir_Node /= Null_Node loop
            Insert_Before
              (Explorer.Tree.Model,
               Iter    => N2,
               Parent  => Node,
               Sibling => N);
            Set_Directory_Node_Attributes
              (Explorer  => Explorer,
               Directory => Data (Dir_Node),
               Node      => N2,
               Project   => Project,
               Node_Type => Directory_Node);
            Add_Files_In_Directory
              (Explorer         => Explorer,
               Directory        => Data (Dir_Node),
               Files_In_Project => Files,
               Node             => N2);
            Dir_Node := Next (Dir_Node);
         end loop;
      end Add_Remaining_Source_Dirs;

   begin
      --  The goal here is to keep the directories if their current state
      --  (expanded or not), while doing the update.

      for J in Sources'Range loop
         if Sources (J) /= No_Name then
            Get_Name_String (Sources (J));
            String_List_Utils.String_List.Append
              (Dirs, Name_As_Directory (Name_Buffer (1 .. Name_Len)));
         end if;
      end loop;

      if Filenames_Are_Case_Sensitive then
         String_List_Utils.Sort (Dirs);
      else
         String_List_Utils.Sort_Case_Insensitive (Dirs);
      end if;

      Dir_Node := First (Dirs);
      N        := Children (Explorer.Tree.Model, Node);

      while N /= Null_Iter loop
         if Get_Node_Type (Explorer.Tree.Model, N) /= Directory_Node then
            Add_Remaining_Source_Dirs;
         end if;

         N2 := N;
         Next (Explorer.Tree.Model, N2);

         case Get_Node_Type (Explorer.Tree.Model, N) is
            when Directory_Node =>
               declare
                  Dir  : constant String :=
                    Get_Directory_From_Node (Explorer.Tree.Model, N);
               begin
                  --  If the directory is no longer part of the sources, remove
                  --  it.
                  --  ??? Should be case insensitive compare

                  if Dir_Node = Null_Node or else Dir < Data (Dir_Node) then
                     Remove (Explorer.Tree.Model, N);
                     N := N2;

                  --  Directory still exists in the new view
                  elsif File_Equal (Dir, Data (Dir_Node)) then
                     Update_Directory_Node_Text (Explorer, Project, N);
                     Update_Node (Explorer, N, Files);
                     N := N2;
                     Dir_Node := Next (Dir_Node);

                  --  New directory added to the project view
                  else
                     Insert_Before
                       (Explorer.Tree.Model,
                        Iter    => N2,
                        Parent  => Node,
                        Sibling => N);
                     Set_Directory_Node_Attributes
                       (Explorer  => Explorer,
                        Directory => Data (Dir_Node),
                        Node      => N2,
                        Project   => Project,
                        Node_Type => Directory_Node);
                     Add_Files_In_Directory
                       (Explorer         => Explorer,
                        Directory        => Data (Dir_Node),
                        Files_In_Project => Files,
                        Node             => N2);
                     Dir_Node := Next (Dir_Node);
                  end if;
               end;

            when Obj_Directory_Node | Exec_Directory_Node =>
               Remove (Explorer.Tree.Model, N);
               N := N2;

            when Project_Node
              | Modified_Project_Node
              | Extends_Project_Node =>
               --  The list of imported projects could change if another
               --  dependency was added, so we need to check for this case
               --  as well.

               Prj := Get_Project_From_Node
                 (Explorer.Tree.Model, Explorer.Kernel, N, False);
               if Prj /= No_Project then
                  Index := Imported'First;
                  while Index <= Imported'Last loop
                     if Imported (Index) = Prj then
                        Imported (Index) := No_Project;
                        exit;
                     end if;
                     Index := Index + 1;
                  end loop;
               else
                  Index := Natural'Last;
               end if;

               if Index > Imported'Last then
                  Remove (Explorer.Tree.Model, N);
               else
                  Update_Node (Explorer, N, Files_In_Project => null);
               end if;

               N := N2;

            when others =>
               --  No other node type is possible
               N := N2;
         end case;
      end loop;

      Add_Remaining_Source_Dirs;
      String_List_Utils.String_List.Free (Dirs);

      if Projects_Before_Directories then
         Add_Projects;
         Add_Object_Directories (Explorer, Node, Project);
      else
         Add_Object_Directories (Explorer, Node, Project);
         Add_Projects;
      end if;
   end Update_Project_Node;

   ---------------------------
   -- Update_Directory_Node --
   ---------------------------

   procedure Update_Directory_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Files_In_Project : File_Array_Access;
      Node             : Gtk_Tree_Iter)
   is
      Index : Natural;
      N, N2 : Gtk_Tree_Iter;
      Dir   : constant String := Name_As_Directory
        (Normalize_Pathname
           (Get_Directory_From_Node (Explorer.Tree.Model, Node),
            Resolve_Links => False));

      type Boolean_Array is array (Files_In_Project'Range) of Boolean;
      New_File : Boolean_Array := (others => True);
      Expanded : constant Boolean := Get_Expanded (Explorer.Tree, Node);
      Path     : Gtk_Tree_Path;
      Dummy    : Boolean;
      pragma Unreferenced (Dummy);

   begin
      --  The goal here is to keep the files and subdirectories if their
      --  current state (expanded or not), while doing the update.

      --  Remove from the tree all the files that are no longer in the
      --  project

      N := Children (Explorer.Tree.Model, Node);
      while N /= Null_Iter loop
         N2 := N;
         Next (Explorer.Tree.Model, N2);

         declare
            F : constant String := Get_Base_Name (Explorer.Tree.Model, N);
         begin
            if Get_Node_Type (Explorer.Tree.Model, N) = File_Node then
               Index := Files_In_Project'First;

               while Index <= Files_In_Project'Last loop
                  if New_File (Index)
                    and then Full_Name (Files_In_Project (Index)).all = Dir & F
                  then
                     New_File (Index) := False;
                     exit;
                  end if;
                  Index := Index + 1;
               end loop;

               if Index > Files_In_Project'Last then
                  Remove (Explorer.Tree.Model, N);
               end if;
            end if;
         end;
         N := N2;
      end loop;

      --  Then add all the new files

      for J in Files_In_Project'Range loop
         if New_File (J)
           and then File_Equal (Dir_Name (Files_In_Project (J)).all, Dir)
         then
            Append_File
              (Explorer.Kernel,
               Explorer.Tree.Model,
               Node,
               Files_In_Project (J));
         end if;
      end loop;

      --  In case we have first deleted all sources files before adding the
      --  others, the expansion status might have changed, so we need to
      --  restore it.

      if Expanded then
         Gtk.Handlers.Handler_Block (Explorer.Tree, Explorer.Expand_Id);
         Path := Get_Path (Explorer.Tree.Model, Node);
         Dummy := Expand_Row (Explorer.Tree, Path, False);
         Path_Free (Path);
         Gtk.Handlers.Handler_Unblock (Explorer.Tree, Explorer.Expand_Id);
      end if;
   end Update_Directory_Node;

   -----------------
   -- Update_Node --
   -----------------

   procedure Update_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Node             : Gtk_Tree_Iter;
      Files_In_Project : File_Array_Access;
      Force_Expanded   : Boolean := False)
   is
      N, N2     : Gtk_Tree_Iter;
      Node_Type : constant Node_Types :=
                    Get_Node_Type (Explorer.Tree.Model, Node);
      N_Type    : Node_Types;
      Prj       : constant Project_Type := Get_Project_From_Node
        (Explorer.Tree.Model, Explorer.Kernel, Node, False);
      Files     : File_Array_Access := Files_In_Project;
      Expanded  : constant Boolean := Get_Expanded (Explorer.Tree, Node);
   begin
      if Files = null then
         Files := Get_Source_Files (Prj, Recursive => False);
      end if;

      --  If the information about the node hasn't been computed before,
      --  then we don't need to do anything. This will be done when the
      --  node is actually expanded by the user

      if Is_Up_To_Date (Explorer.Tree.Model, Node) then
         --  Likewise, if a node is not expanded, we simply remove all
         --  underlying information.
         --  Directory_Node contents is computed along with the project node,
         --  so we mustn't remove its contents

         if Force_Expanded
           or else Expanded
           or else Node_Type = Directory_Node
         then
            case Node_Type is
               when Project_Node | Modified_Project_Node =>
                  Update_Project_Node (Explorer, Files, Node);
               when Directory_Node =>
                  Update_Directory_Node (Explorer, Files, Node);
               when others         => null;
            end case;

         else
            Set_Up_To_Date (Explorer.Tree.Model, Node, False);

            N := Children (Explorer.Tree.Model, Node);

            while N /= Null_Iter loop
               N2 := N;
               Next (Explorer.Tree.Model, N);
               Remove (Explorer.Tree.Model, N2);
            end loop;

            Append_Dummy_Iter (Explorer.Tree.Model, Node);
         end if;
      end if;

      --  Has to be done whether the node is expanded or not, and whether it is
      --  up-to-date or not, as long as its icon is visible.
      --  Change the icons to reflect the modified state of the project
      if Node_Type = Project_Node
        or else Node_Type = Modified_Project_Node
      then
         if Project_Modified
           (Get_Project_From_Node
              (Explorer.Tree.Model, Explorer.Kernel, Node, False))
         then
            N_Type := Modified_Project_Node;
         else
            N_Type := Project_Node;
         end if;

         Set_Node_Type (Explorer.Tree.Model, Node, N_Type, Expanded);
      end if;

      if Files_In_Project = null then
         Unchecked_Free (Files);
      end if;
   end Update_Node;

   ---------------------------------------
   -- Add_Or_Update_Flat_View_Root_Node --
   ---------------------------------------

   procedure Add_Or_Update_Flat_View_Root_Node
     (Explorer : access Project_Explorer_Record'Class)
   is
      Iter2, Iter3 : Gtk_Tree_Iter;
      Imported     : Project_Type_Array := Get_Imported_Projects
        (Get_Project (Explorer.Kernel),
         Direct_Only     => False,
         Include_Project => True);
      Name         : Name_Id;
      Found        : Boolean;
      Id           : Gint;
      pragma Unreferenced (Id);
   begin
      Iter2 := Get_Iter_First (Explorer.Tree.Model);
      while Iter2 /= Null_Iter loop
         Name := Name_Id
           (Get_Int (Explorer.Tree.Model, Iter2, Project_Column));
         Found := False;

         for Im in Imported'Range loop
            if Imported (Im) /= No_Project
              and then Project_Name (Imported (Im)) = Name
            then
               Imported (Im) := No_Project;
               Found := True;
            end if;
         end loop;

         if not Found then
            Iter_Copy (Source => Iter2, Dest => Iter3);
            Next (Explorer.Tree.Model, Iter2);
            Remove (Explorer.Tree.Model, Iter3);
         else
            Update_Node (Explorer, Iter2, null);
            Next (Explorer.Tree.Model, Iter2);
         end if;
      end loop;

      for Im in Imported'Range loop
         if Imported (Im) = Get_Project (Explorer.Kernel) then
            Iter2 := Add_Project_Node (Explorer, Imported (Im), Null_Iter,
                                       Name_Suffix => " (root project)");
         elsif Imported (Im) /= No_Project then
            Iter2 := Add_Project_Node (Explorer, Imported (Im), Null_Iter);
         end if;
      end loop;

      Thaw_Sort (Explorer.Tree.Model, Base_Name_Column);
      Id := Freeze_Sort (Explorer.Tree.Model);
   end Add_Or_Update_Flat_View_Root_Node;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Refresh_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Refresh (Hook.Explorer);
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class) is
      T     : constant Project_Explorer := Project_Explorer (Explorer);
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
      Path  : Gtk_Tree_Path;
      pragma Unreferenced (Dummy);

   begin
      --  No project view => Clean up the tree
      if Get_Project (T.Kernel) = No_Project then
         Clear (T.Tree.Model);

         --  ??? must free memory associated with entities !
         return;
      end if;

      --  For a flat view

      if Get_History (Get_History (T.Kernel).all, Show_Flat_View) then
         Add_Or_Update_Flat_View_Root_Node (T);

      --  If the tree is empty, this simply means we never created it, so we
      --  need to do it now

      elsif Get_Iter_First (T.Tree.Model) = Null_Iter then
         Iter := Add_Project_Node (T, Get_Project (T.Kernel));
         Path := Get_Path (T.Tree.Model, Iter);
         Dummy := Expand_Row (T.Tree, Path, False);
         Path_Free (Path);

      --  If we are displaying a new view of the tree that was there before, we
      --  want to keep the project nodes, and most important their open/close
      --  status, so as to minimize the changes the user sees.

      else
         Update_Node
           (T, Get_Iter_First (T.Tree.Model), Files_In_Project => null);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Refresh;

   --------------------------------
   -- Get_Or_Create_Project_View --
   --------------------------------

   function Get_Or_Create_Project_View
     (Kernel : access Kernel_Handle_Record'Class) return Project_Explorer
   is
      Explorer : Project_Explorer;
      Child    : MDI_Child;
      C2       : MDI_Explorer_Child;
   begin
      Child := MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Record'Tag));

      if Child = null then
         Gtk_New (Explorer, Kernel);
         Refresh (Explorer);

         C2 := new MDI_Explorer_Child_Record;
         Initialize (C2, Explorer,
                     Default_Width  => 215,
                     Default_Height => 600,
                     Group          => Group_View,
                     Module         => Explorer_Module_ID);
         Set_Title (C2, -"Project View",  -"Project View");
         Put (Get_MDI (Kernel), C2, Initial_Position => Position_Left);

         Set_Focus_Child (C2);
         Raise_Child (C2);
         return Explorer;
      else
         Raise_Child (Child);
         Set_Focus_Child (Get_MDI (Kernel), Child);
         return Project_Explorer (Get_Widget (Child));
      end if;
   end Get_Or_Create_Project_View;

   ----------------------
   -- On_Open_Explorer --
   ----------------------

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Explorer : Project_Explorer;
      pragma Unreferenced (Widget, Explorer);

   begin
      Explorer := Get_Or_Create_Project_View (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Open_Explorer;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget) is
   begin
      Explorer_Context_Factory
        (Context, Get_Kernel (Module.all), Child, Child, null, null);
   end Default_Context_Factory;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Explorer_Search_Context) is
   begin
      Reset (Context.Matches);
   end Free;

   -----------------------------
   -- Explorer_Search_Factory --
   -----------------------------

   function Explorer_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Kernel, All_Occurences);
      Context : Explorer_Search_Context_Access;

   begin
      Assert (Me, Extra_Information /= null,
              "No extra information widget specified");

      Context := new Explorer_Search_Context;

      Context.Include_Projects := Get_Active
        (Explorer_Search_Extra (Extra_Information).Include_Projects);
      Context.Include_Directories := Get_Active
        (Explorer_Search_Extra (Extra_Information).Include_Directories);
      Context.Include_Files := Get_Active
        (Explorer_Search_Extra (Extra_Information).Include_Files);
      Context.Include_Entities := Get_Active
        (Explorer_Search_Extra (Extra_Information).Include_Entities);

      --  If we have no context, nothing to do
      if not (Context.Include_Projects
              or else Context.Include_Directories
              or else Context.Include_Files
              or else Context.Include_Entities)
      then
         Free (Search_Context_Access (Context));
         return null;
      end if;

      Reset (Context.Matches);
      return Search_Context_Access (Context);
   end Explorer_Search_Factory;

   -----------------------------
   -- Explorer_Search_Factory --
   -----------------------------

   function Explorer_Search_Factory
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Include_Projects : Boolean;
      Include_Files    : Boolean)
      return Search_Context_Access
   is
      pragma Unreferenced (Kernel);
      Context : Explorer_Search_Context_Access;

   begin
      Context := new Explorer_Search_Context;

      Context.Include_Projects    := Include_Projects;
      Context.Include_Directories := False;
      Context.Include_Files       := Include_Files;
      Context.Include_Entities    := False;

      Reset (Context.Matches);
      return Search_Context_Access (Context);
   end Explorer_Search_Factory;

   ------------
   -- Search --
   ------------

   procedure Search
     (Context         : access Explorer_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean)
   is
      pragma Unreferenced (Search_Backward, Give_Focus, Continue);
      C        : constant Explorer_Search_Context_Access :=
        Explorer_Search_Context_Access (Context);
      Explorer : constant Project_Explorer :=
        Get_Or_Create_Project_View (Kernel);

      procedure Initialize_Parser;
      --  Compute all the matching files and mark them in the htable.

      function Next return Gtk_Tree_Iter;
      --  Return the next matching node

      procedure Next_Or_Child
        (Name        : String;
         Start       : Gtk_Tree_Iter;
         Check_Match : Boolean;
         Result      : out Gtk_Tree_Iter;
         Finish      : out Boolean);
      pragma Inline (Next_Or_Child);
      --  Move to the next node, starting from a project or directory node by
      --  name Name.
      --  If Check_Match is false, then this subprogram doesn't test if the
      --  node matches context.

      procedure Next_File_Node
        (Start  : Gtk_Tree_Iter;
         Result : out Gtk_Tree_Iter;
         Finish : out Boolean);
      pragma Inline (Next_File_Node);
      --  Move to the next node, starting from a file node

      function Check_Entities (File : VFS.Virtual_File) return Boolean;
      pragma Inline (Check_Entities);
      --  Check if File contains any entity matching C.
      --  Return True if there is a match

      procedure Mark_File_And_Projects
        (File           : Virtual_File;
         Project_Marked : Boolean;
         Project        : Project_Type;
         Mark_File      : Search_Status;
         Increment      : Search_Status);
      pragma Inline (Mark_File_And_Projects);
      --  Mark the file Full_Name/Base as matching, as well as the project it
      --  belongs to and all its importing projects.
      --  Increment is added to the reference count for all directories and
      --  importing projects (should be 1 if the file is added, -1 if the file
      --  is removed)

      -------------------
      -- Next_Or_Child --
      -------------------

      procedure Next_Or_Child
        (Name        : String;
         Start       : Gtk_Tree_Iter;
         Check_Match : Boolean;
         Result      : out Gtk_Tree_Iter;
         Finish      : out Boolean) is
      begin
         if Check_Match
           and then Start /= C.Current and then Match (C, Name) /= -1
         then
            Result := Start;
            Finish := True;

         elsif Get (C.Matches, Name) /= No_Match then
            Compute_Children (Explorer, Start);
            Result := Children (Explorer.Tree.Model, Start);
            Finish := False;

         else
            Result := Start;
            Next (Explorer.Tree.Model, Result);
            Finish := False;
         end if;
      end Next_Or_Child;

      --------------------
      -- Next_File_Node --
      --------------------

      procedure Next_File_Node
        (Start  : Gtk_Tree_Iter;
         Result : out Gtk_Tree_Iter;
         Finish : out Boolean)
      is
         N      : aliased constant String :=
                    Get_Base_Name (Explorer.Tree.Model, Start);
         Status : Search_Status;
      begin
         Status := Get (C.Matches, N);
         if C.Include_Entities then
            --  The file was already parsed, and we know it matched
            if Status >= Search_Match then
               Compute_Children (Explorer, Start);
               Result := Children (Explorer.Tree.Model, Start);
               Finish := False;
               return;

            --  The file was never parsed
            elsif Status = Unknown then
               if Check_Entities
                 (Create (Full_Filename => Get_Directory_From_Node
                            (Explorer.Tree.Model, Start) & N))
               then
                  Set (C.Matches, N, Search_Match);
                  Compute_Children (Explorer, Start);
                  Result := Children (Explorer.Tree.Model, Start);
                  Finish := False;
                  return;
               else
                  --  Decrease the count for importing directories and
                  --  projects, so that if no file belonging to them is
                  --  referenced any more, we simply don't parse them

                  Mark_File_And_Projects
                    (File => Create
                       (Full_Filename => Get_Directory_From_Node
                          (Explorer.Tree.Model, Start) & N),
                     Project_Marked => False,
                     Project        => Get_Project_From_Node
                       (Explorer.Tree.Model, Explorer.Kernel, Start, False),
                     Mark_File      => No_Match,
                     Increment      => -1);
               end if;
            end if;

         elsif Status /= No_Match then
            --  Do not return the initial node
            if Context.Include_Files and then C.Current /= Start then
               Result := Start;
               Finish := True;
               return;
            end if;
         end if;

         --  The file doesn't match

         Result := Start;
         Next (Explorer.Tree.Model, Result);
         Finish := False;
      end Next_File_Node;

      ----------
      -- Next --
      ----------

      function Next return Gtk_Tree_Iter is
         Start_Node : Gtk_Tree_Iter := C.Current;
         Tmp        : Gtk_Tree_Iter;
         Finish     : Boolean;

         function First_Word (Str : String) return String;
         --  Return the first word in Str. This is required since the model
         --  of the explorer stores the arguments of the subprograms as well,
         --  and no match would be found otherwise

         ----------------
         -- First_Word --
         ----------------

         function First_Word (Str : String) return String is
         begin
            for J in Str'Range loop
               if Str (J) = ' ' then
                  return Str (Str'First .. J - 1);
               end if;
            end loop;
            return Str;
         end First_Word;

      begin
         while Start_Node /= Null_Iter loop
            begin
               case Get_Node_Type (Explorer.Tree.Model, Start_Node) is
                  when Project_Node
                       | Extends_Project_Node
                       | Modified_Project_Node =>
                     Next_Or_Child
                       (Project_Name
                          (Get_Project_From_Node
                             (Explorer.Tree.Model, Kernel, Start_Node, False)),
                        Start_Node,
                        Context.Include_Projects, Tmp, Finish);

                     if Finish then
                        return Tmp;
                     end if;

                  when Directory_Node =>
                     Next_Or_Child
                       (Get_Directory_From_Node
                          (Explorer.Tree.Model, Start_Node),
                        Start_Node,
                        Context.Include_Directories, Tmp, Finish);
                     if Finish and then Context.Include_Directories then
                        return Tmp;
                     end if;

                  when Obj_Directory_Node | Exec_Directory_Node =>
                     Tmp := Start_Node;
                     Next (Explorer.Tree.Model, Tmp);

                  when File_Node =>
                     Next_File_Node (Start_Node, Tmp, Finish);
                     if Finish and then Context.Include_Files then
                        return Tmp;
                     end if;

                  when Category_Node =>
                     Tmp := Children (Explorer.Tree.Model, Start_Node);

                  when Entity_Node =>
                     if C.Current /= Start_Node
                       and then Get
                         (C.Matches,
                          First_Word
                            (Get_Base_Name (Explorer.Tree.Model, Start_Node)))
                          /= No_Match
                     then
                        return Start_Node;
                     else
                        Tmp := Start_Node;
                        Next (Explorer.Tree.Model, Tmp);
                     end if;
               end case;

               while Tmp = Null_Iter loop
                  Start_Node := Parent (Explorer.Tree.Model, Start_Node);
                  exit when Start_Node = Null_Iter;

                  Tmp := Start_Node;
                  Next (Explorer.Tree.Model, Tmp);
               end loop;

               Start_Node := Tmp;
            end;
         end loop;
         return Null_Iter;
      end Next;

      --------------------
      -- Check_Entities --
      --------------------

      function Check_Entities (File : VFS.Virtual_File) return Boolean is
         use type Entities.LI_Handler;
         Languages  : constant Language_Handler :=
                        Language_Handler (Get_Language_Handler (Kernel));
         Handler    : constant Entities.LI_Handler :=
                        Get_LI_Handler_From_File (Languages, File);
         Constructs : Construct_List;
         Status     : Boolean := False;

         use type Basic_Types.String_Access;

      begin
         if Handler = null then
            return False;
         end if;

         Entities.Parse_File_Constructs
           (Handler, Languages, File, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Filter_Category (Constructs.Current.Category) /= Cat_Unknown
              and then Constructs.Current.Name /= null
              and then Match (C, Constructs.Current.Name.all) /= -1
            then
               Status := True;

               if Get (C.Matches, Constructs.Current.Name.all) /=
                 Search_Match
               then
                  Set (C.Matches, Constructs.Current.Name.all, Search_Match);
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         Free (Constructs);
         return Status;
      end Check_Entities;

      ----------------------------
      -- Mark_File_And_Projects --
      ----------------------------

      procedure Mark_File_And_Projects
        (File           : Virtual_File;
         Project_Marked : Boolean;
         Project        : Project_Type;
         Mark_File      : Search_Status;
         Increment      : Search_Status)
      is
         Dir  : constant String := Dir_Name (File).all;
         Iter : Imported_Project_Iterator;

      begin
         Set (C.Matches, Base_Name (File), Mark_File);

         --  Mark the number of entries in the directory, so that if a file
         --  doesn't match we can decrease it later, and finally no longer
         --  examine the directory
         if Get (C.Matches, Dir) /= No_Match then
            Set (C.Matches, Dir, Get (C.Matches, Dir) + Increment);
         elsif Increment > 0 then
            Set (C.Matches, Dir, 1);
         end if;

         if not Project_Marked then
            --  Mark the current project and all its importing
            --  projects as matching

            declare
               N : constant String := Project_Name (Project);
            begin
               Set (C.Matches, N, Get (C.Matches, N) + Increment);
            end;

            Iter := Find_All_Projects_Importing
              (Project      => Project);

            while Current (Iter) /= No_Project loop
               declare
                  N : constant String := Project_Name (Current (Iter));
               begin
                  Set (C.Matches, N, Get (C.Matches, N) + Increment);
               end;

               Next (Iter);
            end loop;
         end if;
      end Mark_File_And_Projects;

      -----------------------
      -- Initialize_Parser --
      -----------------------

      procedure Initialize_Parser is
         Iter : Imported_Project_Iterator := Start
           (Get_Project (Kernel), Recursive => True);
         Project_Marked : Boolean := False;
      begin
         while Current (Iter) /= No_Project loop

            if Match (C, Project_Name (Current (Iter))) /= -1 then
               Project_Marked := False;
               Mark_File_And_Projects
                 (File           => Project_Path (Current (Iter)),
                  Project_Marked => Project_Marked,
                  Project        => Current (Iter),
                  Mark_File      => Unknown,
                  Increment      => 1);
            end if;

            declare
               Sources : File_Array_Access := Get_Source_Files
                 (Project    => Current (Iter),
                  Recursive  => False);
            begin
               Project_Marked := False;
               for S in Sources'Range loop
                  declare
                     Base : constant String := Base_Name (Sources (S));
                  begin
                     if Match (C, Base) /= -1 then
                        Mark_File_And_Projects
                          (File           => Sources (S),
                           Project_Marked => Project_Marked,
                           Project        => Current (Iter),
                           Mark_File      => Search_Match,
                           Increment      => 1);
                        Project_Marked  := True;
                     end if;

                     if not Project_Marked and then C.Include_Entities then
                        Mark_File_And_Projects
                          (File           => Sources (S),
                           Project_Marked => Project_Marked,
                           Project        => Current (Iter),
                           Mark_File      => Unknown,
                           Increment      => 1);
                        --  Do not change Project_Marked, since we want the
                        --  total count for directories and projects to be the
                        --  total number of files in them.
                        --  ??? Could be more efficient
                     end if;
                  end;
               end loop;

               VFS.Unchecked_Free (Sources);
            end;

            Next (Iter);
         end loop;
      end Initialize_Parser;

   begin
      --  We need to freeze and block the handlers to speed up the display of
      --  the node on the screen.
      Gtk.Handlers.Handler_Block (Explorer.Tree, Explorer.Expand_Id);

      if C.Current = Null_Iter then
         Initialize_Parser;
         C.Current := Get_Iter_First (Explorer.Tree.Model);
      end if;

      C.Current := Next;

      if C.Current /= Null_Iter then
         Jump_To_Node (Explorer, C.Current);
      end if;

      Gtk.Handlers.Handler_Unblock (Explorer.Tree, Explorer.Expand_Id);

      Found := C.Current /= Null_Iter;
   end Search;

   --------------------
   --  Jump_To_Node  --
   --------------------

   procedure Jump_To_Node
     (Explorer    : Project_Explorer;
      Target_Node : Gtk_Tree_Iter)
   is
      Path   : Gtk_Tree_Path;
      Parent : Gtk_Tree_Path;
      Expand : Boolean;

      procedure Expand_Recursive (The_Path : Gtk_Tree_Path);
      --  Expand Path and all parents of Path that are not expanded.

      procedure Expand_Recursive (The_Path : Gtk_Tree_Path) is
         Parent : constant Gtk_Tree_Path := Copy (The_Path);
         Dummy  : Boolean;
      begin
         Dummy := Up (Parent);

         if Dummy then
            if not Row_Expanded (Explorer.Tree, Parent) then
               Expand_Recursive (Parent);
            end if;
         end if;

         Path_Free (Parent);
         Dummy := Expand_Row (Explorer.Tree, The_Path, False);
      end Expand_Recursive;

   begin
      Grab_Focus (Explorer.Tree);

      Path := Get_Path (Explorer.Tree.Model, Target_Node);
      Parent := Copy (Path);
      Expand := Up (Parent);

      if Expand then
         Expand_Recursive (Parent);
      end if;

      Path_Free (Parent);
      Set_Cursor (Explorer.Tree, Path, null, False);

      Scroll_To_Cell (Explorer.Tree, Path, null, True, 0.1, 0.1);

      Path_Free (Path);
   end Jump_To_Node;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Locate_File_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      C        : Search_Context_Access;
      Found    : Boolean;
      Continue : Boolean;
   begin
      C := Explorer_Search_Factory
        (Kernel,
         Include_Projects => False,
         Include_Files    => True);
      --  ??? Should we work directly with a Virtual_File, so that we
      --  are sure to match the right file, not necessarily a file with
      --  the same base name in an extending project...
      Set_Context
        (Context  => C,
         Look_For =>
           "^" & Base_Name (File_Information (Context.Context)) & "$",
         Options  => (Case_Sensitive => Filenames_Are_Case_Sensitive,
                      Whole_Word     => True,
                      Regexp         => True));

      Search
        (C, Kernel,
         Search_Backward => False,
         Give_Focus      => True,
         Found           => Found,
         Continue        => Continue);

      if not Found then
         Insert (Kernel,
                 -"File not found in the explorer: "
                 & Base_Name (File_Information (Context.Context)),
                 Mode => GPS.Kernel.Console.Error);
      end if;

      Free (C);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Locate_Project_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      C        : Search_Context_Access;
      Found    : Boolean;
      Continue : Boolean;
   begin
      C := Explorer_Search_Factory
        (Kernel,
         Include_Projects => True,
         Include_Files    => False);
      Set_Context
        (Context  => C,
         Look_For => Project_Name (Project_Information (Context.Context)),
         Options  => (Case_Sensitive => Filenames_Are_Case_Sensitive,
                      Whole_Word     => True,
                      Regexp         => False));

      Search
        (C, Kernel,
         Search_Backward => False,
         Give_Focus      => True,
         Found           => Found,
         Continue        => Continue);

      if not Found then
         Insert (Kernel,
                 -"Project not found in the explorer: "
                 & Project_Name (Project_Information (Context.Context)));
      end if;

      Free (C);
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
      Tools   : constant String := '/' & (-"Tools") & '/' & (-"Views");
      Extra   : Explorer_Search_Extra;
      Box     : Gtk_Box;

      Project_Node_Filter : constant Action_Filter :=
        new Project_Node_Filter_Record;
      Directory_Node_Filter : constant Action_Filter :=
        new Directory_Node_Filter_Record;
      File_Node_Filter : constant Action_Filter :=
        new File_Node_Filter_Record;
      Entity_Node_Filter : constant Action_Filter :=
        new Entity_Node_Filter_Record;
      Command : Interactive_Command_Access;

   begin
      Explorer_Module_ID := new Explorer_Module_Record;
      Register_Module
        (Module                  => Explorer_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Explorer_Module_Name,
         Priority                => GPS.Kernel.Modules.Default_Priority);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Locate_File_In_Explorer_Command;
      Register_Contextual_Menu
        (Kernel, "Locate file in explorer",
         Action => Command,
         Filter => Action_Filter
           (Lookup_Filter (Kernel, "In project")
            and not Create (Module => Explorer_Module_Name)),
         Label  => "Locate in explorer: %f");

      Command := new Locate_Project_In_Explorer_Command;
      Register_Contextual_Menu
        (Kernel, "Locate project in explorer",
         Action => Command,
         Filter => Action_Filter
           (Lookup_Filter (Kernel, "Project only")
            and not Create (Module => Explorer_Module_Name)),
         Label  => "Locate in explorer: %p");

      Register_Menu
        (Kernel, Project, -"Project _View", "", On_Open_Explorer'Access);
      Register_Menu
        (Kernel, Tools, -"_Project", "", On_Open_Explorer'Access,
         Ref_Item => -"Remote");

      Extra := new Explorer_Search_Extra_Record;
      Gtk.Frame.Initialize (Extra, -"Scope");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Extra, Box);

      Gtk_New (Extra.Include_Projects, -"Projects");
      Pack_Start (Box, Extra.Include_Projects);
      Set_Active (Extra.Include_Projects, True);
      Kernel_Callback.Connect
        (Extra.Include_Projects, "toggled",
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Directories, -"Directories");
      Pack_Start (Box, Extra.Include_Directories);
      Set_Active (Extra.Include_Directories, True);
      Kernel_Callback.Connect
        (Extra.Include_Directories, "toggled",
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Files, -"Files");
      Pack_Start (Box, Extra.Include_Files);
      Set_Active (Extra.Include_Files, True);
      Kernel_Callback.Connect
        (Extra.Include_Files, "toggled",
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Entities, -"Entities (might be slow)");
      Pack_Start (Box, Extra.Include_Entities);
      Set_Active (Extra.Include_Entities, False);
      Kernel_Callback.Connect
        (Extra.Include_Entities, "toggled",
         Reset_Search'Access, Kernel_Handle (Kernel));

      Register_Filter
        (Kernel,
         Filter => Project_Node_Filter,
         Name   => "Explorer_Project_Node");
      Register_Filter
        (Kernel,
         Filter => Directory_Node_Filter,
         Name   => "Explorer_Directory_Node");
      Register_Filter
        (Kernel,
         Filter => File_Node_Filter,
         Name   => "Explorer_File_Node");
      Register_Filter
        (Kernel,
         Filter => Entity_Node_Filter,
         Name   => "Explorer_Entity_Node");

      declare
         Name : constant String := -"Project explorer";
      begin
         Find_Utils.Register_Search_Function
           (Kernel            => Kernel,
            Data => (Length            => Name'Length,
                     Label             => Name,
                     Factory           => Explorer_Search_Factory'Access,
                     Extra_Information => Gtk_Widget (Extra),
                     Id             => Abstract_Module_ID (Explorer_Module_ID),
                     Mask              => All_Options and not Supports_Replace
                       and not Search_Backward and not All_Occurrences));
      end;
   end Register_Module;

   ---------
   -- Nop --
   ---------

   procedure Nop (X : in out Search_Status) is
      pragma Unreferenced (X);
   begin
      null;
   end Nop;

end Project_Explorers;

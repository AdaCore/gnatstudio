------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNATCOLL.JSON;             use GNATCOLL.JSON;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Gdk;                       use Gdk;
with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;

with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;     use Gtk.Tree_Model_Filter;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Sortable;         use Gtk.Tree_Sortable;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Tree_View;          use Gtkada.Tree_View;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Pango.Layout;              use Pango.Layout;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;             use Generic_Views;
with Histories;                 use Histories;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;

with GPS.Intl;                  use GPS.Intl;
with GPS.Properties;            use GPS.Properties;
with GPS.Search;                use GPS.Search;
with GPS.VCS;                   use GPS.VCS;

with GUI_Utils;                 use GUI_Utils;
with Projects;                  use Projects;
with Project_Explorers_Common;  use Project_Explorers_Common;
with String_List_Utils;
with Tooltips;
with Filter_Panels;             use Filter_Panels;

package body Project_Explorers is
   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.PRJ_VIEW");

   Show_Absolute_Paths         : Boolean_Preference;
   Show_Basenames              : Boolean_Preference;
   Show_Flat_View              : Boolean_Preference;
   Show_Directories            : Boolean_Preference;
   Show_Object_Dirs            : Boolean_Preference;
   Show_Empty_Dirs             : Boolean_Preference;
   Projects_Before_Directories : Boolean_Preference;
   Show_Runtime                : Boolean_Preference;
   Preserve_Nodes_State        : Boolean_Preference;

   Toggle_Absolute_Path_Name : constant String :=
     "Explorer toggle absolute paths";
   Toggle_Absolute_Path_Tip : constant String :=
     "Toggle the display of absolute paths or just base names in the"
     & " project explorer";

   package Boolean_User_Data is new Glib.Object.User_Data (Boolean);
   User_Data_Projects_Before_Directories : constant String :=
     "gps-prj-before-dirs";
   --  local cache of the history key, for use in Sort_Func

   -------------
   --  Filter --
   -------------

   type Project_View_Config is record
      Hidden_Files_Pattern : Unbounded_String;
      Initialized          : Boolean := False;
      Flat_View            : Boolean := False;
      Show_Directories     : Boolean := False;
      Show_Hidden_Files    : Boolean := False;
      Show_Object_Dirs     : Boolean := False;
      Show_Empty_Dirs      : Boolean := False;
      Show_Runtime         : Boolean := False;
      Projects_Before_Dirs : Boolean := False;
   end record;
   --  The current config. This is used to detect whether a refresh is needed
   --  when preferences change.

   package Filter_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => "=");
   use Filter_Sets;

   type Explorer_Filter is record
      Config  : Project_View_Config;

      Pattern : GPS.Search.Search_Pattern_Access;
      --  The pattern on which we filter.

      Visible : Filter_Sets.Set;
      --  A cache of the filter. We do not manipulate the gtk model directly,
      --  because it does not contain everything in general (the contents of
      --  nodes is added dynamically).
   end record;

   procedure Set_Pattern
     (Self    : in out Explorer_Filter;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pattern : Search_Pattern_Access);
   --  Change the pattern and update the cache

   ----------------------
   -- Custom tree view --
   ----------------------

   type Explorer_Tree_View_Record is new Base_Explorer_Tree_Record with record
      User_Filter : Explorer_Filter;
   end record;
   type Explorer_Tree_View is access all Explorer_Tree_View_Record'Class;

   overriding function Is_Visible
     (Self : not null access Explorer_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   overriding procedure Add_Children
     (Self       : not null access Explorer_Tree_View_Record;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);

   ---------------------------------
   -- The project explorer widget --
   ---------------------------------

   type Project_Explorer_Record is new Generic_Views.View_Record with record
      Tree        : Explorer_Tree_View;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Is_New_Root : Boolean := True;
      --  When the project changed a new root node is created and must be
      --  expanded during the first refresh
   end record;
   overriding procedure Create_Menu
     (View    : not null access Project_Explorer_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Create_Toolbar
     (View    : not null access Project_Explorer_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Project_Explorer_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access);

   function On_Focus_Changed
     (Explorer : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Called when the filter gains or loses focus

   function Initialize
     (Explorer : access Project_Explorer_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create a new explorer, and return the focus widget.

   procedure On_Explorer_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the Project_Explorer_Record is being destroyed

   procedure Store_Expanded_Nodes
     (Self : access Project_Explorer_Record'Class);
   --  Stores expanded project nodes

   type Explorer_Child_Record is
      new MDI_Explorer_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package Explorer_Views is new Generic_Views.Simple_Views
     (Module_Name        => Explorer_Module_Name,
      View_Name          => "Project",
      Formal_View_Record => Project_Explorer_Record,
      Formal_MDI_Child   => Explorer_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Position           => Position_Left,
      Initialize         => Initialize);
   use Explorer_Views;
   subtype Project_Explorer is Explorer_Views.View_Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   type Toggle_Absolute_Path_Command is
      new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Toggle_Absolute_Path_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;

   type Collapse_All_Projects_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Collapse_All_Projects_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;

   package File_Node_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Gtk_Tree_Iter,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => "=");

   type Directory_Info is record
      Directory : Virtual_File;
      Kind      : Node_Types;
   end record;
   function "<" (D1, D2 : Directory_Info) return Boolean;
   package Files_List is new Ada.Containers.Doubly_Linked_Lists (Virtual_File);
   package Dirs_Files_Hash is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type        => Directory_Info,
      Element_Type    => Files_List.List,
      "="             => Files_List."=");
   use Files_List, Dirs_Files_Hash;

   procedure For_Each_File_Node
     (Model    : Gtk_Tree_Store;
      Parent   : Gtk_Tree_Iter;
      Callback : not null access procedure (It : in out Gtk_Tree_Iter));
   --  For each file node representing a direct source of Parent (does not
   --  look into nested project nodes). Callback can freely modify It, or
   --  the model.

   function Find_Project_Node
     (Self    : not null access Project_Explorer_Record'Class;
      Project : Project_Type) return Gtk_Tree_Iter;
   --  Find the first node matching the project

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Explorer : Project_Explorer;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Used to sort nodes in the explorer

   function Compute_Project_Node_Type
      (Self     : not null access Explorer_Tree_View_Record'Class;
       Project  : Project_Type) return Node_Types;
   --  The node type to use for a project

   procedure Set_Column_Types
     (Self : not null access Project_Explorer_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   ---------------------
   -- Expanding nodes --
   ---------------------

   function Directory_Node_Text
     (Show_Abs_Paths : Boolean;
      Show_Base      : Boolean;
      Project        : Project_Type;
      Dir            : Virtual_File) return String;
   --  Return the text to use for a directory node

   procedure Expand_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Collapse_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Called every time a node is collapsed

   procedure Refresh_Project_Node
     (Self      : not null access Explorer_Tree_View_Record'Class;
      Node      : Gtk_Tree_Iter;
      Flat_View : Boolean);
   --  Insert the children nodes for the project (directories, imported
   --  projects,...)
   --  Node is associated with Project. Both can be null when in flat view
   --  mode.

   function Button_Press
     (Explorer : access GObject_Record'Class;
      Event    : Gdk_Event_Button) return Boolean;
   --  Called every time a row is clicked
   --  ??? It is actually called twice in that case: a first time when the
   --  mouse button is pressed and a second time when it is released.

   function Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Calledback on a key press

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a new row is selected

   --------------------
   -- Updating nodes --
   --------------------

   procedure Update_Absolute_Paths
     (Explorer : access Gtk_Widget_Record'Class);
   --  Update the text for all directory nodes in the tree, mostly after the
   --  "show absolute path" setting has changed.

   ----------------------------
   -- Retrieving information --
   ----------------------------

   procedure Refresh
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Refresh the contents of the tree after the project view has changed.
   --  This procedure tries to keep as many things as possible in the current
   --  state (expanded nodes,...)

   type On_Refresh is new Simple_Hooks_Function with record
      Explorer : Project_Explorer;
   end record;
   overriding procedure Execute
     (Self   : On_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   procedure Jump_To_Node
     (Explorer    : Project_Explorer;
      Target_Node : Gtk_Tree_Iter);
   --  Select Target_Node, and make sure it is visible on the screen

   type On_Project_Changed is new Simple_Hooks_Function with record
      Explorer : Project_Explorer;
   end record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project changes. Expand the root node at that time

   type On_Project_Changing is new File_Hooks_Function with record
      Explorer : Project_Explorer;
   end record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when closing the project. Stores nodes expanding state

   --------------
   -- Commands --
   --------------

   type Locate_File_In_Explorer_Command (Focus : Boolean)
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Locate_File_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Locate_Project_In_Explorer_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Locate_Project_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -------------
   -- Filters --
   -------------

   type Project_View_Filter_Record is new Action_Filter_Record
      with null record;
   type Project_Toolbar_Record is new Action_Filter_Record
      with null record;
   type Project_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type Directory_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type File_Node_Filter_Record is new Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Project_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   overriding function Filter_Matches_Primitive
     (Context : access Project_Toolbar_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   overriding function Filter_Matches_Primitive
     (Context : access Project_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   overriding function Filter_Matches_Primitive
     (Context : access Directory_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   overriding function Filter_Matches_Primitive
     (Context : access File_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   ----------------
   -- Properties --
   ----------------

   type Expanded_Nodes_Property_Record is
     new Property_Record with record
      Paths : String_List_Utils.String_List.Vector;
   end record;
   type Expanded_Nodes_Property is access all Expanded_Nodes_Property_Record;

   overriding procedure Save
     (Property : access Expanded_Nodes_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Property : in out Expanded_Nodes_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value);

   -------------------------------
   -- Compute_Project_Node_Type --
   -------------------------------

   function Compute_Project_Node_Type
     (Self     : not null access Explorer_Tree_View_Record'Class;
      Project  : Project_Type) return Node_Types is
   begin
      if Project.Modified then
         return Modified_Project_Node;
      elsif Project = Get_Project (Self.Kernel) then
         return Root_Project_Node;
      elsif Extending_Project (Project) /= No_Project then
         return Extends_Project_Node;
      else
         return Project_Node;
      end if;
   end Compute_Project_Node_Type;

   ---------
   -- "<" --
   ---------

   function "<" (D1, D2 : Directory_Info) return Boolean is
   begin
      if D1.Kind < D2.Kind then
         return True;
      elsif D1.Kind = D2.Kind then
         return D1.Directory < D2.Directory;
      else
         return False;
      end if;
   end "<";

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Project_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Views.Get_Module;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Project_Toolbar_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Ctxt);
      View   : constant Project_Explorer :=
        Explorer_Views.Get_Or_Create_View (Kernel, Focus => False);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Views.Get_Module
        and then View.Get_Filter.Get_Focus_Child /= null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Project_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Views.Get_Module
        and then Has_Project_Information (Ctxt)
        and then not Has_Directory_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Directory_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Views.Get_Module
        and then Has_Directory_Information (Ctxt)
        and then not Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access File_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Views.Get_Module
        and then Has_File_Information (Ctxt)
        and then not Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Self : not null access Project_Explorer_Record'Class)
   is
      Tree        : constant Gtk_Tree_View := Gtk_Tree_View (Self.Tree);
      Col         : Gtk_Tree_View_Column;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Self.Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Self.Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "icon-name", Icon_Column);
      Add_Attribute (Col, Self.Text_Rend, "markup", Display_Name_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Explorer : access GObject_Record'Class;
      Event    : Gdk_Event_Button) return Boolean
   is
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      return On_Button_Press
        (MDI_Explorer_Child (Explorer_Views.Child_From_View (T)),
         T.Tree, Event);
   end Button_Press;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean is
   begin
      return On_Key_Press (Project_Explorer (Explorer).Tree, Event);
   end Key_Press;

   ------------------------
   -- Tree_Select_Row_Cb --
   ------------------------

   procedure Tree_Select_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class; Args : GValues)
   is
      pragma Unreferenced (Args);
      T : constant Project_Explorer := Project_Explorer (Explorer);
      Child : constant GPS_MDI_Child := Explorer_Views.Child_From_View (T);
   begin
      --  Might be null during a call to Add_Children
      if Child /= null
         and then MDI_Child (Child) = Get_MDI (T.Kernel).Get_Focus_Child
      then
         T.Kernel.Context_Changed (Child.Build_Context);
      end if;
   end Tree_Select_Row_Cb;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Explorer : access Project_Explorer_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Tooltip  : Explorer_Tooltip_Handler_Access;
      Scrolled : Gtk_Scrolled_Window;
      Hook     : Preferences_Hooks_Function_Access;

   begin
      Initialize_Vbox (Explorer, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Explorer.Pack_Start (Scrolled, Expand => True, Fill => True);

      Explorer.Tree := new Explorer_Tree_View_Record;
      Explorer.Tree.Kernel := Explorer.Kernel;
      Explorer.Tree.Initialize
        (Column_Types     => Columns_Types,
         Capability_Type  => Filtered,
         Set_Visible_Func => True);
      Explorer.Tree.Set_Propagate_Filtered_Status (False);
      Set_Mode (Explorer.Tree.Get_Selection, Selection_Multiple);
      Set_Headers_Visible (Explorer.Tree, False);
      Explorer.Tree.Set_Enable_Search (False);
      Set_Column_Types (Explorer);

      Set_Name (Explorer.Tree, "Project Explorer Tree");  --  For testsuite

      Scrolled.Add (Explorer.Tree);

      Setup_Contextual_Menu
        (Kernel          => Explorer.Kernel,
         Event_On_Widget => Explorer.Tree);

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (Expand_Row_Cb'Access),
         Explorer);
      Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Collapsed,
         Widget_Callback.To_Marshaller (Collapse_Row_Cb'Access),
         Explorer);

      Explorer.Tree.On_Button_Release_Event (Button_Press'Access, Explorer);
      Explorer.Tree.On_Button_Press_Event (Button_Press'Access, Explorer);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Key_Press'Access),
         Slot_Object => Explorer,
         After       => False);

      Widget_Callback.Object_Connect
        (Get_Selection (Explorer.Tree), Signal_Changed,
         Tree_Select_Row_Cb'Access, Explorer, After => True);

      --  Automatic update of the tree when the project changes
      Project_View_Changed_Hook.Add
        (Obj   =>
            new On_Refresh'
           (Hook_Function with Explorer => Project_Explorer (Explorer)),
         Watch => Explorer);

      --  Store nodes expanding state
      Project_Changing_Hook.Add
        (Obj   =>
            new On_Project_Changing'
           (Hook_Function with Explorer => Project_Explorer (Explorer)),
         Watch => Explorer);

      --  Automatically expand the root node when the project changes
      --   and restore nodes expanding
      Project_Changed_Hook.Add
        (Obj   =>
            new On_Project_Changed'
           (Hook_Function with Explorer => Project_Explorer (Explorer)),
         Watch => Explorer);

      --  Set the DnD handlers

      Gtk.Dnd.Dest_Set
        (Explorer.Tree, Dest_No_Default, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree,
         Signal_Drag_Data_Received,
         Project_Explorers_Common.Drag_Data_Received'Access,
         Explorer.Kernel);
      Explorer.Tree.Enable_Model_Drag_Source
        (Gdk.Types.Button1_Mask, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree, Signal_Drag_Data_Get,
         Drag_Data_Get'Access, Explorer.Kernel);

      --  Sorting is not alphabetic: directories come first, then files. Use
      --  a custom sort function

      Set_Sort_Func
        (+Explorer.Tree.Model,
         Display_Name_Column,
         Sort_Func      => Sort_Func'Access);
      Set_Sort_Column_Id
        (+Explorer.Tree.Model, Display_Name_Column, Sort_Ascending);

      Tooltip := new Explorer_Tooltip_Handler;
      Tooltip.Tree := Explorer.Tree;
      Tooltip.Associate_To_Widget (Explorer.Tree);

      Hook :=
        new On_Pref_Changed'
          (Hook_Function with Explorer => Project_Explorer (Explorer));
      Preferences_Changed_Hook.Add (Obj => Hook, Watch => Explorer);
      Hook.Execute (Explorer.Kernel, null);   --  also calls Refresh

      Vcs_File_Status_Changed_Hook.Add
        (new On_VCS_Status_Changed'
           (Vcs_File_Status_Hooks_Function with Tree => Explorer.Tree),
         Watch => Explorer);

      Explorer.On_Destroy (On_Explorer_Destroy'Access);

      return Gtk.Widget.Gtk_Widget (Explorer.Tree);
   end Initialize;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      A_Before_B : Gint := -1;
      B_Before_A : Gint := 1;
      M          : constant Gtk_Tree_Store := -Model;
      A_Type     : constant Node_Types :=
                     Get_Node_Type (M, A);
      B_Type     : constant Node_Types :=
                     Get_Node_Type (M, B);
      Order      : Gtk_Sort_Type;
      Column     : Gint;

      function Alphabetical return Gint;
      --  Compare the two nodes alphabetically
      --  ??? Should take into account the sorting order

      ------------------
      -- Alphabetical --
      ------------------

      function Alphabetical return Gint is
         A_Name : constant String := To_Lower (Get_String (Model, A, Column));
         B_Name : constant String := To_Lower (Get_String (Model, B, Column));
      begin
         if A_Name < B_Name then
            return A_Before_B;
         elsif A_Name = B_Name then
            case A_Type is   --  same as B_Type
               when Project_Node_Types | Directory_Node_Types | File_Node =>

                  if Get_File (Model, A, File_Column) <
                    Get_File (Model, B, File_Column)
                  then
                     return A_Before_B;
                  else
                     return B_Before_A;
                  end if;

               when others =>
                  return A_Before_B;
            end case;
         else
            return B_Before_A;
         end if;
      end Alphabetical;

      Projects_Before_Directories : constant Boolean :=
        Boolean_User_Data.Get (M, User_Data_Projects_Before_Directories);

   begin
      Get_Sort_Column_Id (M, Column, Order);
      if Order = Sort_Descending then
         A_Before_B := 1;
         B_Before_A := -1;
      end if;

      --  Subprojects first

      case A_Type is
         when Project_Node_Types =>
            case B_Type is
               when Project_Node_Types =>
                  return Alphabetical;

               when Runtime_Node =>
                  return A_Before_B;

               when others =>
                  if Projects_Before_Directories then
                     return A_Before_B;
                  else
                     return B_Before_A;
                  end if;
            end case;

         when Directory_Node =>
            case B_Type is
               when Project_Node_Types =>
                  if Projects_Before_Directories then
                     return B_Before_A;
                  else
                     return A_Before_B;
                  end if;

               when Directory_Node =>
                  return Alphabetical;

               when others =>
                  return A_Before_B;
            end case;

         when Obj_Directory_Node | Lib_Directory_Node =>
            case B_Type is
               when Project_Node_Types =>
                  if Projects_Before_Directories then
                     return B_Before_A;
                  else
                     return A_Before_B;
                  end if;

               when Directory_Node =>
                  return B_Before_A;

               when Obj_Directory_Node | Lib_Directory_Node =>
                  return Alphabetical;

               when Runtime_Node | Exec_Directory_Node =>
                  return A_Before_B;

               when others =>
                  return B_Before_A;
            end case;

         when Exec_Directory_Node =>
            case B_Type is
               when Project_Node_Types =>
                  if Projects_Before_Directories then
                     return B_Before_A;
                  else
                     return A_Before_B;
                  end if;

               when Directory_Node | Obj_Directory_Node | Lib_Directory_Node =>
                  return B_Before_A;

               when Exec_Directory_Node =>
                  return Alphabetical;

               when Runtime_Node =>
                  return A_Before_B;

               when others =>
                  return B_Before_A;
            end case;

         when Runtime_Node =>
            return B_Before_A;

         when File_Node =>
            case B_Type is
               when Project_Node_Types =>
                  if Projects_Before_Directories then
                     return B_Before_A;
                  else
                     return A_Before_B;
                  end if;

               when Obj_Directory_Node | Lib_Directory_Node =>
                  return A_Before_B;

               when others =>
                  return Alphabetical;
            end case;
      end case;
   end Sort_Func;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      Config : Project_View_Config;
   begin
      if Self.Explorer = null then
         return;
      end if;

      Set_Font_And_Colors
        (Self.Explorer.Tree, Fixed_Font => True, Pref => Pref);

      if Pref = null
        or else Pref = Preference (Show_Ellipsis)
      then
         Set_Property
           (Self.Explorer.Text_Rend,
            Gtk.Cell_Renderer_Text.Ellipsize_Property,
            (if Show_Ellipsis.Get_Pref
             then Ellipsize_Middle else Ellipsize_None));
         Self.Explorer.Tree.Queue_Resize;
         Self.Explorer.Tree.Queue_Draw;
      end if;

      Config :=
        (Initialized          => True,
         Flat_View            => Show_Flat_View.Get_Pref,
         Show_Directories     => Show_Directories.Get_Pref,
         Show_Hidden_Files    => Show_Hidden_Files.Get_Pref,
         Show_Object_Dirs     => Show_Object_Dirs.Get_Pref,
         Show_Empty_Dirs      => Show_Empty_Dirs.Get_Pref,
         Show_Runtime         => Show_Runtime.Get_Pref,
         Projects_Before_Dirs => Projects_Before_Directories.Get_Pref,
         Hidden_Files_Pattern =>
           To_Unbounded_String (Hidden_Files_Pattern.Get_Pref));

      if Config /= Self.Explorer.Tree.User_Filter.Config then
         Self.Explorer.Tree.User_Filter.Config := Config;
         Refresh (Self.Explorer);
      end if;

      if Pref = null
        or else Pref = Preference (Show_Absolute_Paths)
        or else Pref = Preference (Show_Basenames)
      then
         Update_Absolute_Paths (Self.Explorer);
      end if;
   end Execute;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Project_Explorer_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "project_view",
         Tooltip     => -"Filter the contents of the project view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Project Explorer Filter");
      --  Recompute the filters when the filter focus state changes,
      --  it's needed to properly receive the backspace key
      Return_Callback.Object_Connect
        (View.Get_Filter.Get_Focus_Widget,
         Signal_Focus_In_Event,
         Return_Callback.To_Marshaller (On_Focus_Changed'Access),
         Slot_Object => View);
      Return_Callback.Object_Connect
        (View.Get_Filter.Get_Focus_Widget,
         Signal_Focus_Out_Event,
         Return_Callback.To_Marshaller (On_Focus_Changed'Access),
         Slot_Object => View);
   end Create_Toolbar;

   -------------------------
   -- On_Explorer_Destroy --
   -------------------------

   procedure On_Explorer_Destroy (Self : access Gtk_Widget_Record'Class) is
   begin
      Store_Expanded_Nodes (Project_Explorer (Self));
   end On_Explorer_Destroy;

   ----------------------
   -- On_Focus_Changed --
   ----------------------

   function On_Focus_Changed
     (Explorer : access Gtk_Widget_Record'Class)
      return Boolean
   is
   begin
      Refresh_Context (Project_Explorer (Explorer).Kernel);
      return False;
   end On_Focus_Changed;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View : not null access Project_Explorer_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Show_Absolute_Paths);
      Append_Menu (Menu, K, Show_Basenames);
      Append_Menu (Menu, K, Show_Ellipsis);
      Menu.Append (Gtk_Menu_Item_New);
      Append_Menu (Menu, K, Show_Flat_View);
      Append_Menu (Menu, K, Show_Directories);
      Append_Menu (Menu, K, Show_Hidden_Files);
      Append_Menu (Menu, K, Show_Object_Dirs);
      Append_Menu (Menu, K, Show_Empty_Dirs);
      Append_Menu (Menu, K, Projects_Before_Directories);
      Menu.Append (Gtk_Menu_Item_New);
      Append_Menu (Menu, K, Show_Runtime);
      Append_Menu (Menu, K, Preserve_Nodes_State);
   end Create_Menu;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self : not null access Explorer_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      File : Virtual_File;
   begin
      case Self.Get_Node_Type (Iter) is
         when Project_Node_Types | File_Node =>
            if Self.User_Filter.Pattern = null then
               return True;
            else
               File := Self.Get_File_From_Node (Iter);
               return Self.User_Filter.Visible.Contains (File);
            end if;

         when Directory_Node_Types =>
            if not Self.User_Filter.Config.Show_Empty_Dirs
              and then not Has_Child (Self.Model, Iter)
            then
               return False;
            elsif Self.User_Filter.Pattern = null then
               return True;
            else
               File := Self.Get_File_From_Node (Iter);
               return Self.User_Filter.Visible.Contains (File);
            end if;

         when Runtime_Node =>
            return True;
      end case;
   end Is_Visible;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Self    : in out Explorer_Filter;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pattern : Search_Pattern_Access)
   is
      Show_Abs_Paths : constant Boolean := Show_Absolute_Paths.Get_Pref;
      Show_Base      : constant Boolean := Show_Basenames.Get_Pref;
      Flat_View      : constant Boolean := Self.Config.Flat_View;

      procedure Mark_Project_And_Parents_Visible (P : Project_Type);
      --  mark the given project node and all its parents as visible

      procedure Mark_Project_And_Parents_Visible (P : Project_Type) is
         It : Project_Iterator;
      begin
         --  Unless already marked, nothing more to do
         if not Self.Visible.Contains (P.Project_Path) then
            Self.Visible.Include (P.Project_Path);

            if not Flat_View then
               It := P.Find_All_Projects_Importing
                 (Include_Self => False, Direct_Only => False);
               while Current (It) /= No_Project loop
                  Mark_Project_And_Parents_Visible (Current (It));
                  Next (It);
               end loop;
            end if;
         end if;
      end Mark_Project_And_Parents_Visible;

      PIter       : Project_Iterator;
      P           : Project_Type;
      Files       : File_Array_Access;
      Found       : Boolean;
      Prj_Visible : Boolean;  --  has the project already been marked visible
   begin
      GPS.Search.Free (Self.Pattern);
      Self.Pattern := Pattern;

      Self.Visible.Clear;

      if Pattern = null then
         --  No filter applied, make all visible
         return;
      end if;

      PIter := Get_Project (Kernel).Start
        (Direct_Only      => False,
         Include_Extended => True);
      while Current (PIter) /= No_Project loop
         P := Current (PIter);

         if Self.Pattern.Start (P.Name) /= GPS.Search.No_Match then
            Prj_Visible := True;
            Mark_Project_And_Parents_Visible (P);
         else
            Prj_Visible := False;
         end if;

         Files := P.Source_Files (Recursive => False);
         for F in Files'Range loop
            if Show_Base then
               Found := Self.Pattern.Start
                 (Files (F).Display_Base_Name) /= GPS.Search.No_Match;
            elsif Show_Abs_Paths then
               Found := Self.Pattern.Start
                 (Files (F).Display_Full_Name) /= GPS.Search.No_Match;
            else
               --  ??? Should be looking at the relative name
               Found := Self.Pattern.Start
                 (Files (F).Display_Base_Name) /= GPS.Search.No_Match;
            end if;

            if Found then
               if not Prj_Visible then
                  Prj_Visible := True;
                  Mark_Project_And_Parents_Visible (P);
               end if;

               Self.Visible.Include (Files (F).Dir);
               Self.Visible.Include (Files (F));
            end if;
         end loop;
         Unchecked_Free (Files);

         Next (PIter);
      end loop;
   end Set_Pattern;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Project_Explorer_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access) is
   begin
      Trace (Me, "Filter Changed, refiltering");
      Set_Pattern (Self.Tree.User_Filter, Self.Kernel, Pattern);
      Self.Tree.Refilter;
      Trace (Me, "Filter Changed, refiltered");

   exception
      when E : others =>
         Trace (Me, E);
   end Filter_Changed;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      T           : constant Project_Explorer :=
        Explorer_Views.View_From_Child (Self);
      Filter_Iter : constant Gtk_Tree_Iter :=
        Find_Iter_For_Event (T.Tree, Event);
      Filter_Path : Gtk_Tree_Path;
      Context : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);  --  inherited
   begin
      if Filter_Iter = Null_Iter then
         return Context;
      end if;

      Filter_Path := Get_Path (T.Tree.Get_Model, Filter_Iter);
      if not Path_Is_Selected (Get_Selection (T.Tree), Filter_Path) then
         Set_Cursor (T.Tree, Filter_Path, null, False);
      end if;
      Path_Free (Filter_Path);
      T.Tree.Context_Factory (Context);
      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Toggle_Absolute_Path_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Set_Pref (Show_Absolute_Paths, K.Get_Preferences,
                not Show_Absolute_Paths.Get_Pref);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Collapse_All_Projects_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      View     : constant Project_Explorer :=
                   Explorer_Views.Get_Or_Create_View
                     (Get_Kernel (Context.Context),
                      Focus => False);
      Iter     : Gtk_Tree_Iter := Get_Iter_First (View.Tree.Model);
      Sort     : constant Gint := Freeze_Sort (View.Tree.Model);

      procedure Recurse (It : Gtk_Tree_Iter);
      --  Close all project nodes recursively

      procedure Recurse (It : Gtk_Tree_Iter) is
         Child_It : Gtk_Tree_Iter;
         P        : Gtk_Tree_Path;
         Success  : Boolean;
         pragma Unreferenced (Success);
      begin
         case View.Tree.Get_Node_Type (It) is
            when Project_Node_Types
               | Runtime_Node
               | Directory_Node_Types =>

               P := View.Tree.Model.Get_Path (It);
               Success := View.Tree.Collapse_Row (P);
               Path_Free (P);

               Child_It := Children (View.Tree.Model, It);
               while Child_It /= Null_Iter loop
                  Recurse (Child_It);
                  Next (View.Tree.Model, Child_It);
               end loop;

            when others =>
               null;
         end case;
      end Recurse;

   begin
      while Iter /= Null_Iter loop
         Recurse (Iter);
         Next (View.Tree.Model, Iter);
      end loop;

      Thaw_Sort (View.Tree.Model, Sort);
      return Commands.Success;
   end Execute;

   ---------------------------
   -- Update_Absolute_Paths --
   ---------------------------

   procedure Update_Absolute_Paths
     (Explorer : access Gtk_Widget_Record'Class)
   is
      Exp : constant Project_Explorer := Project_Explorer (Explorer);
      Show_Abs_Paths : constant Boolean := Show_Absolute_Paths.Get_Pref;
      Show_Base      : constant Boolean := Show_Basenames.Get_Pref;

      procedure Process_Node (Iter : Gtk_Tree_Iter; Project : Project_Type);
      --  Recursively process node

      ------------------
      -- Process_Node --
      ------------------

      procedure Process_Node (Iter : Gtk_Tree_Iter; Project : Project_Type) is
         It  : Gtk_Tree_Iter := Children (Exp.Tree.Model, Iter);
         Prj : Project_Type  := Project;
      begin
         case Exp.Tree.Get_Node_Type (Iter) is
            when Project_Node_Types =>
               Prj := Exp.Tree.Get_Project_From_Node (Iter, False);
            when Directory_Node_Types | File_Node | Runtime_Node =>
               null;
         end case;

         while It /= Null_Iter loop
            case Exp.Tree.Get_Node_Type (It) is
               when Project_Node_Types | Runtime_Node =>
                  Process_Node (It, No_Project);

               when Directory_Node_Types =>
                  --  ??? When in a runtime mode, the path is not relative to
                  --  any project, so we are always displaying the full path.

                  Exp.Tree.Model.Set (It, Display_Name_Column,
                       Directory_Node_Text
                         (Show_Abs_Paths => Show_Abs_Paths,
                          Show_Base      => Show_Base,
                          Project        => Prj,
                          Dir            => Get_File
                            (Exp.Tree.Model, It, File_Column)));

               when others =>
                  null;
            end case;

            Next (Exp.Tree.Model, It);
         end loop;
      end Process_Node;

      Iter : Gtk_Tree_Iter := Get_Iter_First (Exp.Tree.Model);
      Sort : constant Gint := Freeze_Sort (Exp.Tree.Model);
   begin
      while Iter /= Null_Iter loop
         Process_Node (Iter, Get_Project (Exp.Kernel));
         Next (Exp.Tree.Model, Iter);
      end loop;

      Thaw_Sort (Exp.Tree.Model, Sort);
   end Update_Absolute_Paths;

   ------------------
   -- Add_Children --
   ------------------

   overriding procedure Add_Children
     (Self       : not null access Explorer_Tree_View_Record;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      N_Type : constant Node_Types := Self.Get_Node_Type (Store_Iter);
   begin
      case N_Type is
         when Project_Node_Types =>
            declare
               --  This has no effect when Add_Children is called from
               --  Refresh, since we are already detached.
               Dummy : constant Explorer_Expansion.Detached_Model :=
                  Explorer_Expansion.Detach_Model_From_View (Self);
            begin
               Refresh_Project_Node
                 (Self, Store_Iter,
                  Flat_View => Self.User_Filter.Config.Flat_View);
               Self.Refilter;
            end;

         when Runtime_Node =>
            --  Following does nothing if info is aleeady there
            declare
               Dummy : constant Explorer_Expansion.Detached_Model :=
                  Explorer_Expansion.Detach_Model_From_View (Self);
            begin
               Self.Append_Runtime_Info (Store_Iter);
            end;

         when File_Node | Directory_Node_Types =>
            null;   --  nothing to do
      end case;
   end Add_Children;

   -------------------
   -- Expand_Row_Cb --
   -------------------

   procedure Expand_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      T      : constant Project_Explorer := Project_Explorer (Explorer);
      Iter   : Gtk_Tree_Iter;
      N_Type : Node_Types;
   begin
      if Filter_Iter /= Null_Iter then
         Iter   := T.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
         N_Type := T.Tree.Get_Node_Type (Iter);
         T.Tree.Set_Node_Type (Iter, N_Type, Expanded => True);
      end if;
   end Expand_Row_Cb;

   ---------------------
   -- Collapse_Row_Cb --
   ---------------------

   procedure Collapse_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      E : constant Project_Explorer := Project_Explorer (Explorer);
      Iter   : Gtk_Tree_Iter;
      N_Type : Node_Types;
   begin
      if Filter_Iter /= Null_Iter then
         Iter := E.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
         N_Type := E.Tree.Get_Node_Type (Iter);
         E.Tree.Set_Node_Type (Iter, N_Type, Expanded => False);
      end if;
   end Collapse_Row_Cb;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Refresh;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Refresh (Self.Explorer);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Self.Explorer.Is_New_Root := True;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Kernel, File);
   begin
      Store_Expanded_Nodes (Self.Explorer);
   end Execute;

   --------------------------
   -- Store_Expanded_Nodes --
   --------------------------

   procedure Store_Expanded_Nodes
     (Self : access Project_Explorer_Record'Class)
   is
      P     : Expanded_Nodes_Property;
      Model : Gtk_Tree_Store;

      procedure Process (Iter : Gtk_Tree_Iter);
      --  Checks whether a node is expanded and store it if true

      -------------
      -- Process --
      -------------

      procedure Process (Iter : Gtk_Tree_Iter) is
         Path : Gtk_Tree_Path;
      begin
         if Model.Has_Child (Iter) then
            Path := Self.Tree.Get_Filter_Path_For_Store_Iter (Iter);
            if Self.Tree.Row_Expanded (Path) then
               P.Paths.Append (Model.Get_String_From_Iter (Iter));

               for Idx in 1 .. Model.N_Children (Iter) loop
                  Process (Model.Nth_Child (Iter, Idx - 1));
               end loop;
            end if;
            Path_Free (Path);
         end if;
      end Process;

      Iter : Gtk_Tree_Iter;

   begin
      if not Preserve_Nodes_State.Get_Pref then
         return;
      end if;

      Trace (Me, "Store expanded nodes");

      P     := new Expanded_Nodes_Property_Record;
      Model := Self.Tree.Model;
      Iter  := Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         Process (Iter);
         Model.Next (Iter);
      end loop;

      Set_Property
        (Kernel     => Self.Kernel,
         Project    => Get_Project (Self.Kernel),
         Name       => "project_view_nodes",
         Property   => Property_Access (P),
         Persistent => True);
   end Store_Expanded_Nodes;

   -------------------------
   -- Directory_Node_Text --
   -------------------------

   function Directory_Node_Text
     (Show_Abs_Paths : Boolean;
      Show_Base      : Boolean;
      Project        : Project_Type;
      Dir            : Virtual_File) return String is
   begin
      if Show_Base then
         return +(Dir.Base_Dir_Name);

      elsif Show_Abs_Paths then
         return Dir.Display_Full_Name;

      else
         declare
            Rel : constant String :=
              +Relative_Path (Dir, Project.Project_Path.Dir);
         begin
            --  If there is in common is '/', we just use a full path
            --  instead, that looks better, especially for runtime files
            if Starts_With (Rel, "..") then
               declare
                  --  Workaround against an early finalization problem
                  --  (O506-030).
                  GCP : constant Virtual_File := Greatest_Common_Path
                    ((Dir, Project.Project_Path.Dir));
               begin
                  if GCP.Full_Name.all = "/" then
                     return Dir.Display_Full_Name;
                  end if;
               end;
            end if;

            if Rel = "" then
               return "";
            elsif Rel (Rel'Last) = '/' or else Rel (Rel'Last) = '\' then
               return Rel (Rel'First .. Rel'Last - 1);
            else
               return Rel;
            end if;
         end;
      end if;
   end Directory_Node_Text;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class) is
      T      : constant Project_Explorer := Project_Explorer (Explorer);
      --  store old filter
      Filter : constant GPS.Search.Search_Pattern_Access :=
        T.Tree.User_Filter.Pattern;
   begin
      --  clear filter to avoid filtering during reloading
      T.Tree.User_Filter.Pattern := null;
      T.Tree.User_Filter.Visible.Clear;

      --  Cache the value for use in Sort_Func
      Boolean_User_Data.Set
        (T.Tree.Model,
         T.Tree.User_Filter.Config.Projects_Before_Dirs,
         User_Data_Projects_Before_Directories);

      declare
         Dummy : constant Explorer_Expansion.Detached_Model :=
            Explorer_Expansion.Detach_Model_From_View (T.Tree);
      begin
         T.Tree.Model.Clear;

         if Get_Project (T.Kernel) = No_Project then
            return;
         end if;

         Refresh_Project_Node
           (Self      => T.Tree,
            Node      => Null_Iter,
            Flat_View => T.Tree.User_Filter.Config.Flat_View);

         --  Add children for the root project. We can't simply expand the
         --  node, since the view is detached (and reattaching would detach
         --  it again to add the children, which is inefficient since we
         --  need to restore the expansion status every time)

         T.Tree.Add_Row_Children (T.Tree.Model.Get_Iter_First);

         --  Refilter only if needed
         if not T.Tree.User_Filter.Config.Show_Empty_Dirs
            or else Filter /= null
         then
            Trace (Me, "Refilter");
            if Filter /= null then
               Set_Pattern (T.Tree.User_Filter, T.Kernel, Filter);
            end if;
            T.Tree.Refilter;
            Trace (Me, "Done Refilter");
         end if;
      end;

      if T.Is_New_Root then
         T.Is_New_Root := False;
         --  Expand the node for the root project. Its contents
         --  has already been added, so this operation is fast.
         declare
            Path : Gtk_Tree_Path;

            procedure Expand;
            procedure Expand
            is
               Success : Boolean with Unreferenced;
            begin
               Success := Expand_Row (T.Tree, Path, False);
               Path_Free (Path);
            end Expand;

            use String_List_Utils.String_List;

            Property : Expanded_Nodes_Property_Record;
            Found    : Boolean;

         begin
            Path := T.Tree.Get_Filter_Path_For_Store_Iter
              (T.Tree.Model.Get_Iter_First);
            Expand;

            Trace (Me, "Restore expanded nodes");

            Get_Property
              (Property,
               Get_Project (T.Kernel),
               Name => "project_view_nodes",
               Found => Found);

            if Found then
               for Item of Property.Paths loop
                  Path := T.Tree.Get_Filter_Path_For_Store_Iter
                    (T.Tree.Model.Get_Iter_From_String (Item));
                  Expand;
               end loop;
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Refresh;

   -----------------------
   -- Find_Project_Node --
   -----------------------

   package Files_Set is new Ada.Containers.Ordered_Sets (Virtual_File);
   package Projects_Vectors is
     new Ada.Containers.Vectors (Positive, Project_Type);
   --  Used in Find_Project_Node and declared at package level
   --  to avoid side effects

   function Find_Project_Node
     (Self    : not null access Project_Explorer_Record'Class;
      Project : Project_Type) return Gtk_Tree_Iter
   is
      Flat_View : constant Boolean := Self.Tree.User_Filter.Config.Flat_View;
      Node      : Gtk_Tree_Iter;
      P         : Project_Type;

      Processed : Files_Set.Set;
      Way       : Projects_Vectors.Vector;
      Found     : Boolean := False;

      procedure Build_Way
        (Cur_Project  : Project_Type;
         Goal_Project : Project_Type;
         Way          : in out Projects_Vectors.Vector;
         Found        : in out Boolean);
      --  Build a way from the project Cur_Project and through its children
      --  until it finds the project Goal_Project

      procedure Search_Project_Node
        (Way  : in out Projects_Vectors.Vector;
         Node : in out Gtk_Tree_Iter);
      --  Add children for node and call itself recursively
      --  to add children for all projects in "way" list

      -------------------------
      -- Search_Project_Node --
      -------------------------

      procedure Search_Project_Node
        (Way  : in out Projects_Vectors.Vector;
         Node : in out Gtk_Tree_Iter)
      is
         Path : Gtk_Tree_Path;
      begin
         --  Iterate over childs to find project from way

         while Node /= Null_Iter loop
            P := Self.Tree.Get_Project_From_Node (Node, Importing => False);

            if P = Way.First_Element then
               --  Found the head of the list Way, suppress it
               Way.Delete_First;

               --  Move to the children of Node of the project P
               Path := Self.Tree.Model.Get_Path (Node);
               Self.Tree.Add_Row_Children (Node);
               Node := Self.Tree.Model.Get_Iter (Path);
               Path_Free (Path);

               if not Way.Is_Empty then
                  --  Continue to follow Way
                  Node := Self.Tree.Model.Children (Node);
                  Search_Project_Node (Way, Node);
               end if;

               exit;
            end if;

            Self.Tree.Model.Next (Node);
         end loop;
      end Search_Project_Node;

      ---------------
      -- Build_Way --
      ---------------

      procedure Build_Way
        (Cur_Project  : Project_Type;
         Goal_Project : Project_Type;
         Way          : in out Projects_Vectors.Vector;
         Found        : in out Boolean)
      is
         Iterator       : Project_Iterator;
         Nested_Project : Project_Type;
      begin
         if Processed.Contains (Cur_Project.Project_Path) then
            --  This project is already tested
            return;
         end if;

         Processed.Insert (Cur_Project.Project_Path);

         if Cur_Project = Goal_Project then
            Found := True;
            return;
         end if;

         Iterator := Cur_Project.Start
           (Recursive   => True,
            Direct_Only => True);

         --  Iterate over nested projects
         loop
            Nested_Project := Current (Iterator);
            if Nested_Project = No_Project then
               --  No more project, return "not found"
               return;
            end if;

            --  Add current project to way and test it (with nested)
            Way.Append (Nested_Project);
            Build_Way (Nested_Project, Goal_Project, Way, Found);
            if Found then
               return;
            end if;

            --  Last project does not contains needed,
            --  delete from way and try next
            Way.Delete_Last;
            Next (Iterator);
         end loop;

      end Build_Way;

   begin
      if Project = No_Project then
         return Null_Iter;
      end if;

      if Flat_View then
         Node := Self.Tree.Model.Get_Iter_First;
         while Node /= Null_Iter loop
            P := Self.Tree.Get_Project_From_Node (Node, Importing => False);
            if P = Project then
               return Node;
            end if;

            Self.Tree.Model.Next (Node);
         end loop;

         return Null_Iter;

      else
         --  The Way begins with the Root project
         Way.Append (Self.Kernel.Get_Project_Tree.Root_Project);
         Build_Way
           (Self.Kernel.Get_Project_Tree.Root_Project, Project, Way, Found);

         if Found then
            --  Way contains projects chain,
            --  we need to add all children nodes for this way

            Self.Tree.Add_Row_Children (Self.Tree.Model.Get_Iter_First);
            Node := Self.Tree.Model.Get_Iter_First;

            if not Way.Is_Empty then
               --  We have to follow the way
               Search_Project_Node (Way, Node);
            end if;
         end if;

         return Node;
      end if;
   end Find_Project_Node;

   ------------------------
   -- For_Each_File_Node --
   ------------------------

   procedure For_Each_File_Node
     (Model    : Gtk_Tree_Store;
      Parent   : Gtk_Tree_Iter;
      Callback : not null access procedure (It : in out Gtk_Tree_Iter))
   is
      It, Current : Gtk_Tree_Iter;
   begin
      It := Model.Children (Parent);
      while It /= Null_Iter loop
         Current := It;
         Model.Next (It);
         case Get_Node_Type (Model, Current) is
            when File_Node      => Callback (Current);
            when Directory_Node =>
               For_Each_File_Node (Model, Current, Callback);
            when others         => null;
         end case;
      end loop;
   end For_Each_File_Node;

   --------------------------
   -- Refresh_Project_Node --
   --------------------------

   procedure Refresh_Project_Node
     (Self      : not null access Explorer_Tree_View_Record'Class;
      Node      : Gtk_Tree_Iter;
      Flat_View : Boolean)
   is
      function Create_Or_Reuse_Project
        (P : Project_Type) return Gtk_Tree_Iter;
      function Create_Or_Reuse_Directory
        (Dir : Directory_Info; Parent : Gtk_Tree_Iter) return Gtk_Tree_Iter;
      --  Create a new project node, or reuse one if it exists

      procedure Create_Or_Reuse_Runtime;
      --  Create a new runtime node, or reuse one if it exists.

      procedure Add_File (Parent : Gtk_Tree_Iter; File : Virtual_File);
      --  Add file node

      Show_Abs_Paths : constant Boolean := Show_Absolute_Paths.Get_Pref;
      Show_Base      : constant Boolean := Show_Basenames.Get_Pref;
      Show_Obj_Dirs  : constant Boolean :=
         Self.User_Filter.Config.Show_Object_Dirs;
      Show_Dirs      : constant Boolean :=
         Self.User_Filter.Config.Show_Directories;

      Child   : Gtk_Tree_Iter;
      Files   : File_Array_Access;
      Project : Project_Type;
      Dirs    : Dirs_Files_Hash.Map;
      VCS     : Abstract_VCS_Engine_Access;

      -----------------------------
      -- Create_Or_Reuse_Project --
      -----------------------------

      function Create_Or_Reuse_Project
        (P : Project_Type) return Gtk_Tree_Iter
      is
         Child : Gtk_Tree_Iter;
      begin
         Child := Self.Create_Or_Reuse_Node
           (Parent => Node,
            Kind   => Compute_Project_Node_Type (Self, P),
            File   => P.Project_Path,
            Name   => P.Name
               & (if Flat_View and then P = Get_Project (Self.Kernel)
                  then " (root project)"
                  elsif P.Extending_Project /= No_Project
                  then " (extended)"
                  else ""));
         Set_File (Self.Model, Child, File_Column, P.Project_Path);
         Self.Set_Might_Have_Children (Child);
         return Child;
      end Create_Or_Reuse_Project;

      -------------------------------
      -- Create_Or_Reuse_Directory --
      -------------------------------

      function Create_Or_Reuse_Directory
        (Dir : Directory_Info; Parent : Gtk_Tree_Iter) return Gtk_Tree_Iter
      is
      begin
         return Self.Create_Or_Reuse_Node
           (Parent => Parent,
            Kind   => Dir.Kind,
            File   => Dir.Directory,
            Name   => Directory_Node_Text
              (Show_Abs_Paths, Show_Base, Project, Dir.Directory));
      end Create_Or_Reuse_Directory;

      -----------------------------
      -- Create_Or_Reuse_Runtime --
      -----------------------------

      procedure Create_Or_Reuse_Runtime is
      begin
         if Self.User_Filter.Config.Show_Runtime then
            Child := Self.Create_Or_Reuse_Node
              (Parent => Null_Iter,  --  always at toplevel
               Kind   => Runtime_Node,
               File   => No_File,
               Name   => "runtime");
            Self.Set_Might_Have_Children (Child);
         end if;
      end Create_Or_Reuse_Runtime;

      --------------
      -- Add_File --
      --------------

      procedure Add_File (Parent : Gtk_Tree_Iter; File : Virtual_File) is
         Dummy : Gtk_Tree_Iter;
      begin
         Dummy := Create_File
           (Self, Parent, File,
            Icon_Name => To_String
              (VCS.Get_Display
                   (VCS.File_Properties_From_Cache (File).Status).Icon_Name));
      end Add_File;

   begin
      Increase_Indent (Me, "Refresh project node");
      if Node = Null_Iter then
         if Flat_View then
            declare
               Iter : Project_Iterator := Get_Project (Self.Kernel).Start
                 (Direct_Only      => False,
                  Include_Extended => True);
            begin
               while Current (Iter) /= No_Project loop
                  Child := Create_Or_Reuse_Project (Current (Iter));
                  Next (Iter);
               end loop;
            end;
         else
            --  Create and expand the node for the root project
            Child := Create_Or_Reuse_Project (Get_Project (Self.Kernel));
         end if;

         Create_Or_Reuse_Runtime;
         Decrease_Indent (Me, "done Refresh project done after root");
         return;
      end if;

      Project := Self.Get_Project_From_Node (Node, Importing => False);

      --  Compute (in background) VCS status for files, if not done yet

      VCS := Self.Kernel.VCS.Get_VCS (Project);
      VCS.Ensure_Status_For_Project (Project);

      --  Insert non-expanded nodes for imported projects

      if not Flat_View then
         declare
            Iter : Project_Iterator := Project.Start
              (Direct_Only => True, Include_Extended => True);
         begin
            while Current (Iter) /= No_Project loop
               if Current (Iter) /= Project then
                  Child := Create_Or_Reuse_Project (Current (Iter));
                  Self.Set_Might_Have_Children (Child);
               end if;

               Next (Iter);
            end loop;
         end;
      end if;

      --  Prepare list of directories

      if Show_Obj_Dirs then
         Dirs.Include
           ((Project.Object_Dir, Obj_Directory_Node), Files_List.Empty_List);

         if Project.Executables_Directory /= Project.Object_Dir then
            Dirs.Include
              ((Project.Executables_Directory, Exec_Directory_Node),
               Files_List.Empty_List);
         end if;

         if Project.Library_Directory /= Project.Object_Dir then
            Dirs.Include
              ((Project.Library_Directory, Lib_Directory_Node),
               Files_List.Empty_List);
         end if;
      end if;

      Files := Project.Source_Files (Recursive => False);

      if Show_Dirs then
         for Dir of Project.Source_Dirs loop
            Dirs.Include ((Dir, Directory_Node), Files_List.Empty_List);
         end loop;

         --  Prepare list of files

         for F of Files.all loop
            Dirs ((F.Dir, Directory_Node)).Append (F);
         end loop;

         --  Now insert directories and files (including object directories).
         --  This operation might take several seconds on very large projects
         --  (10_000 files in the same directory for instance). Would be nice
         --  to split that into small chunks eventually.

         declare
            Dir         : Dirs_Files_Hash.Cursor := Dirs.First;
            Previous    : Directory_Info := (No_File, Runtime_Node);
            Dummy       : Gtk_Tree_Iter;
         begin
            while Has_Element (Dir) loop
               if not Self.Kernel.Is_Hidden (Key (Dir).Directory) then
                  --  minor optimization, reuse dir if same as previous file
                  if Key (Dir) /= Previous then
                     Previous := (Key (Dir).Directory, Key (Dir).Kind);
                     Child := Create_Or_Reuse_Directory (Key (Dir), Node);
                  end if;

                  if Show_Dirs then
                     for F of Dirs (Dir) loop
                        if not Self.Kernel.Is_Hidden (F) then
                           Add_File (Child, F);
                        end if;
                     end loop;
                  end if;
               end if;

               Next (Dir);
            end loop;
         end;

      else
         for F of Files.all loop
            if not Self.Kernel.Is_Hidden (F) then
               Add_File (Node, F);
            end if;
         end loop;
      end if;

      Unchecked_Free (Files);
      Decrease_Indent (Me, "done Refresh project");
   end Refresh_Project_Node;

   ------------------
   -- Jump_To_Node --
   ------------------

   procedure Jump_To_Node
     (Explorer    : Project_Explorer;
      Target_Node : Gtk_Tree_Iter)
   is
      Path   : Gtk_Tree_Path;
      Parent : Gtk_Tree_Path;
      Filter_Path : Gtk_Tree_Path;

      procedure Expand_Recursive (Filter_Path : Gtk_Tree_Path);
      --  Expand Path and all parents of Path that are not expanded

      ----------------------
      -- Expand_Recursive --
      ----------------------

      procedure Expand_Recursive (Filter_Path : Gtk_Tree_Path) is
         Parent : constant Gtk_Tree_Path := Copy (Filter_Path);
         Dummy  : Boolean;
         pragma Warnings (Off, Dummy);
      begin
         Dummy := Up (Parent);

         if Dummy then
            if not Row_Expanded (Explorer.Tree, Parent) then
               Expand_Recursive (Parent);
            end if;
         end if;

         Path_Free (Parent);
         Dummy := Expand_Row (Explorer.Tree, Filter_Path, False);
      end Expand_Recursive;

   begin
      Path := Get_Path (Explorer.Tree.Model, Target_Node);
      Filter_Path := Explorer.Tree.Filter.Convert_Child_Path_To_Path (Path);
      Parent := Copy (Filter_Path);
      if Up (Parent) then
         Expand_Recursive (Parent);
      end if;
      Path_Free (Parent);

      Set_Cursor (Explorer.Tree, Filter_Path, null, False);
      Scroll_To_Cell (Explorer.Tree, Filter_Path, null, True, 0.5, 0.0);

      Path_Free (Path);
      Path_Free (Filter_Path);
   end Jump_To_Node;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Locate_File_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      File     : constant Virtual_File  := File_Information (Context.Context);
      S        : File_Info_Set;
      View     : constant Project_Explorer :=
        Explorer_Views.Get_Or_Create_View (Kernel, Focus => Command.Focus);
      Node     : Gtk_Tree_Iter;
      Success  : Boolean := False;
      --  Needed to store the result of Expand_Row
      Useless  : Boolean;
      pragma Unreferenced (Useless);
      Filter_Path, Path : Gtk_Tree_Path;

      procedure Select_If_Searched (C : in out Gtk_Tree_Iter);
      procedure Select_If_Searched (C : in out Gtk_Tree_Iter) is
      begin
         if not Success then
            if View.Tree.Get_File_From_Node (C) = File then
               Jump_To_Node (View, C);
               Success := True;
            end if;
         end if;
      end Select_If_Searched;

   begin
      --  If the target node is not visible due to the current filter
      --  setting, clear the filter before jumping.

      if View.Tree.User_Filter.Pattern /= null
        and then not View.Tree.User_Filter.Visible.Contains (File)
      then
         View.Set_Filter ("");
         View.Tree.Refilter;

         --  We use the "execute_again" mechanism here because the filter is
         --  applied not immediately but in an idle callback.
         return Execute_Again;
      end if;

      S := Get_Registry (Kernel).Tree.Info_Set (File);
      Node := Find_Project_Node
        (View, File_Info (S.First_Element).Project);

      --  Flat View need to expand the project node to compute its files
      Path := View.Tree.Model.Get_Path (Node);
      Filter_Path := View.Tree.Filter.Convert_Child_Path_To_Path (Path);
      Path_Free (Path);
      Useless := View.Tree.Expand_Row (Filter_Path, False);
      Path_Free (Filter_Path);

      if Node /= Null_Iter then
         For_Each_File_Node (View.Tree.Model, Node, Select_If_Searched'Access);
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Locate_Project_In_Explorer_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      View     : constant Project_Explorer :=
        Explorer_Views.Get_Or_Create_View (Kernel, Focus => True);
      Node     : Gtk_Tree_Iter;
   begin
      Node := Find_Project_Node (View, Project_Information (Context.Context));
      if Node /= Null_Iter then
         Jump_To_Node (View, Node);
      end if;

      return Commands.Success;
   end Execute;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Expanded_Nodes_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      use String_List_Utils.String_List;

      Values : JSON_Array;
      C      : String_List_Utils.String_List.Cursor := Property.Paths.First;
   begin
      while Has_Element (C) loop
         Append (Values, Create (Element (C)));
         Next (C);
      end loop;

      Value.Set_Field ("paths", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Expanded_Nodes_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      Values : JSON_Array;

   begin
      Values := Value.Get ("paths");
      for Index in 1 .. Length (Values) loop
         Property.Paths.Append (Get (Values, Index).Get);
      end loop;
   end Load;
   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Project_View_Filter    : constant Action_Filter :=
                                new Project_View_Filter_Record;
      Project_Toolbar_Filter : constant Action_Filter :=
                                new Project_Toolbar_Record;
      Project_Node_Filter    : constant Action_Filter :=
                                new Project_Node_Filter_Record;
      Directory_Node_Filter  : constant Action_Filter :=
                                new Directory_Node_Filter_Record;
      File_Node_Filter       : constant Action_Filter :=
                                new File_Node_Filter_Record;
   begin
      Explorer_Views.Register_Module (Kernel => Kernel);

      Show_Flat_View := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-flat-view", False,
         Label => -"Show flat view");

      Show_Absolute_Paths := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-absolute-paths", False,
         Label => -"Show absolute paths",
         Doc   =>
           -("Show absolute path names for directories, from the root of" &
             " the disk. If unset, names are displayed relative to" &
             " the location of the project file." & ASCII.LF &
             "This option has no effect if you select Show Basenames."));

      Show_Empty_Dirs := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-empty-directories", True,
         Label => -"Show empty directories");

      Projects_Before_Directories :=
        Kernel.Get_Preferences.Create_Invisible_Pref
          ("explorer-show-projects-first", False,
           Label => -"Projects before directories",
           Doc =>
             -("Show imported projects before directories."));

      Show_Object_Dirs := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-object-dirs", True,
         Label => -"Show object directories");

      Show_Runtime := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-runtime", False,
         Label => "Show runtime files");

      Show_Directories := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-directories", True,
         Label => "Group by directories",
         Doc   => -("If False, files are shown directly below the projects,"
           & " otherwise they are grouped by categories"));

      Show_Basenames := Kernel.Get_Preferences.Create_Invisible_Pref
        ("explorer-show-basenames", False,
         Label => "Show basenames",
         Doc   =>
           -("If True, only the base name of directories is displayed." &
             " If the name is /some/long/path, then only 'path' will be" &
             " visible."));

      Preserve_Nodes_State :=
        Kernel.Get_Preferences.Create_Invisible_Pref
          ("explorer-preserve-nodes-state", False,
           Label => -"Preserve nodes state",
           Doc =>
             -("Preserve the expanded nodes between GPS sessions."));

      Register_Action
        (Kernel, "Locate file in explorer",
         Command      => new Locate_File_In_Explorer_Command (Focus => True),
         Description  => "Locate selected file in project view",
         Filter       => Lookup_Filter (Kernel, "File")
         and not Create (Module => Explorer_Module_Name),
         Category     => -"Projects",
         For_Learning => True);

      Register_Action
        (Kernel, "Locate file in explorer (no focus)",
         Command      => new Locate_File_In_Explorer_Command (Focus => False),
         Description  =>
           -("Locate selected file in project view."
           & " Don't give the focus to the Project view."),
         Filter       => Lookup_Filter (Kernel, "File")
         and not Create (Module => Explorer_Module_Name),
         Category     => -"Projects",
         For_Learning => False);

      Register_Action
        (Kernel, "Locate project in explorer",
         Command      => new Locate_Project_In_Explorer_Command,
         Description  => "Locate selected project in project view",
         Filter       => Lookup_Filter (Kernel, "Project only")
         and not Create (Module => Explorer_Module_Name),
         Category     => -"Projects",
         For_Learning => True);

      Register_Action
        (Kernel, Toggle_Absolute_Path_Name,
         Command     => new Toggle_Absolute_Path_Command,
         Description => Toggle_Absolute_Path_Tip,
         Category    => -"Project Explorer");

      Register_Action
        (Kernel, "Project view: collapse all projects",
         new Collapse_All_Projects_Command,
         Description => "Collapse all project nodes in the Project view",
         Category    => -"Project Explorer",
         Icon_Name   => "gps-collapse-all-symbolic");

      Register_Filter
        (Kernel,
         Filter => Project_View_Filter,
         Name   => "Explorer_View");
      Register_Filter
        (Kernel,
         Filter => Project_Toolbar_Filter,
         Name   => "Explorer_Toolbar_Filter");
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
   end Register_Module;

end Project_Explorers;

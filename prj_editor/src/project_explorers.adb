------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

with Glib;                      use Glib;
with Glib.Main;                 use Glib.Main;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Cairo;                     use Cairo;

with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;

with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Tree_Sortable;         use Gtk.Tree_Sortable;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Tree_View;          use Gtkada.Tree_View;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Entities;
with Find_Utils;                use Find_Utils;
with Histories;                 use Histories;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Projects;                  use Projects;
with Project_Explorers_Common;  use Project_Explorers_Common;
with Remote;                    use Remote;
with String_Hash;
with String_Utils;              use String_Utils;
with Tooltips;
with Traces;                    use Traces;
with XML_Utils;                 use XML_Utils;
with Vsearch;                   use Vsearch;

package body Project_Explorers is

   Me : constant Debug_Handle := Create ("Project_Explorers");

   type Explorer_Module_Record is new Module_ID_Record with null record;
   Explorer_Module_ID : Module_ID := null;
   --  Id for the explorer module

   Explorers_Tooltips  : constant Debug_Handle :=
                          Create ("Explorers.Tooltips", GNATCOLL.Traces.Off);

   Show_Absolute_Paths : constant History_Key :=
                           "explorer-show-absolute-paths";
   Show_Flat_View      : constant History_Key :=
                           "explorer-show-flat-view";
   Show_Hidden_Dirs    : constant History_Key :=
                           "explorer-show-hidden-directories";

   Projects_Before_Directories : constant Boolean := False;
   --  <preference> True if the projects should be displayed, when sorted,
   --  before the directories in the project view.

   function Hash (Key : Filesystem_String) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);

   package Filename_Node_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Filesystem_String,
      Element_Type    => Gtk_Tree_Iter,
      Hash            => Hash,
      Equivalent_Keys => "=");
   use Filename_Node_Hash;

   package File_Node_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Gtk_Tree_Iter,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => "=");
   use File_Node_Hash;

   overriding procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   type Explorer_Path is record
      Explorer : Project_Explorer;
      Path     : Gtk_Tree_Path;
   end record;

   package Scroll_Idle is new Glib.Main.Generic_Sources (Explorer_Path);

   function Idle_Scroll_To (Data : Explorer_Path) return Boolean;
   --  Utility function to scroll to Data.Path

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

   package Project_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (String, Ada.Strings.Hash, "=");

   Projects : Project_Sets.Set;
   --  Cache for project passed through search

   procedure Nop (X : in out Search_Status) is null;
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

      Matches             : String_Status_Hash.String_Hash_Table.Instance;
      --  The search is performed on the internal Ada structures first, and for
      --  each matching project, directory or file, an entry is made in this
      --  table (set to true). This then speeds up the traversing of the tree
      --  to find the matching entities.
      --  Key is
      --    Base_Name for File and Project
      --    Display_Full_Name for directories
   end record;
   type Explorer_Search_Context_Access is access all Explorer_Search_Context;

   overriding function Context_Look_In
     (Self : Explorer_Search_Context) return String;
   overriding procedure Free (Context : in out Explorer_Search_Context);
   --  Free the memory allocated for Context

   type Explorer_Search_Extra_Record is new Gtk_Box_Record with record
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

   overriding procedure Search
     (Context         : access Explorer_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean);
   --  Search the next occurrence in the explorer

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function Sort_Func
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Used to sort nodes in the explorer

   --------------
   -- Tooltips --
   --------------

   type Explorer_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Explorer : Project_Explorer_Access;
   end record;
   type Explorer_Tooltips_Access is access all Explorer_Tooltips'Class;
   overriding procedure Draw
     (Tooltip : access Explorer_Tooltips;
      Pixmap  : out Cairo_Surface;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentatoin

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view

   ------------------
   -- Adding nodes --
   ------------------

   procedure Add_Or_Update_Flat_View_Root_Node
     (Explorer : access Project_Explorer_Record'Class);
   --  Add the root node for the flat view

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
   --  Compute the children of Node, if they haven't been computed yet

   procedure Add_Object_Directories
     (Explorer : access Project_Explorer_Record'Class;
      Node     : Gtk_Tree_Iter;
      Project  : Project_Type);
   --  Add the object and exec directory nodes for Node. They are added
   --  unconditionally.

   procedure Set_Directory_Node_Attributes
     (Explorer  : access Project_Explorer_Record'Class;
      Directory : Virtual_File;
      Node      : Gtk_Tree_Iter;
      Project   : Project_Type;
      Node_Type : Directory_Node_Types);
   --  Set the attributes in tree model for a directory node

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

   procedure Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is collapsed

   function Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
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

   procedure Update_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files    : File_Array;
      Node     : Gtk_Tree_Iter);
   --  Recompute the directories for the project

   procedure Update_Directory_Node_Text
     (Explorer : access Project_Explorer_Record'Class;
      Project  : Project_Type;
      Node     : Gtk_Tree_Iter);
   --  Set the text to display for this directory node

   procedure Update_Absolute_Paths
     (Explorer : access Gtk_Widget_Record'Class);
   --  Update the text for all directory nodes in the tree, mostly after the
   --  "show absolute path" setting has changed.

   procedure Update_View
     (Explorer : access Gtk_Widget_Record'Class);
   --  Update the tree when "show flat view" or "show hidden directories"
   --  setting have changed.

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
   overriding procedure Execute
     (Hook   : Refresh_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed

   type Project_Changed_Hook_Record is new Function_No_Args with record
      Explorer : Project_Explorer;
   end record;
   type Project_Hook is access all Project_Changed_Hook_Record'Class;
   overriding procedure Execute
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
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one

   function Get_Or_Create_Project_View
     (Kernel       : access Kernel_Handle_Record'Class;
      Raise_Window : Boolean) return Project_Explorer;
   --  Make sure a project view exists, and raise it if Raise_Window is True

   procedure Child_Selected
     (Explorer : access Gtk_Widget_Record'Class; Args : GValues);
   --  Called every time a new child is selected in the MDI. This makes sure
   --  that the selected node in the explorer doesn't reflect false information

   procedure On_Parse_Xref (Explorer : access Gtk_Widget_Record'Class);
   --  Parse all the LI information contained in the selected project

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
   type Project_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type Directory_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type File_Node_Filter_Record is new Action_Filter_Record
      with null record;
   type Entity_Node_Filter_Record is new Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Project_View_Filter_Record;
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
   overriding function Filter_Matches_Primitive
     (Context : access Entity_Node_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Project_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID;
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
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
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
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
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
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Module_ID
        and then Has_File_Information (Ctxt)
        and then not Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
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
      Add_Attribute (Col, Text_Rend, "markup", Display_Name_Column);
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
            T.Tree, T.Tree.Model, Event, False);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      return On_Key_Press (T.Kernel, T.Tree, Event);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Key_Press;

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
      when E : others => Trace (Me, E);
   end Tree_Select_Row_Cb;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      H1       : Refresh_Hook;
      H2       : Project_Hook;
      Tooltip  : Explorer_Tooltips_Access;

   begin
      Initialize_Vbox (Explorer, Homogeneous => False);
      Explorer.Kernel := Kernel_Handle (Kernel);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Explorer, Scrolled, Fill => True, Expand => True);

      Init_Graphics (Gtk_Widget (Explorer));
      Gtk_New (Explorer.Tree, Columns_Types);
      Set_Headers_Visible (Explorer.Tree, False);
      Set_Column_Types (Gtk_Tree_View (Explorer.Tree));

      Set_Name (Explorer.Tree, "Project Explorer Tree");  --  For testsuite

      Add (Scrolled, Explorer.Tree);
      Set_Font_And_Colors (Explorer.Tree, Fixed_Font => True);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.Tree,
         Object          => Explorer,
         ID              => Explorer_Module_ID,
         Context_Func    => Explorer_Context_Factory'Access);

      --  The contents of the nodes is computed on demand. We need to be aware
      --  when the user has changed the visibility status of a node.

      Explorer.Expand_Id := Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (Expand_Row_Cb'Access),
         Explorer);
      Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Collapsed,
         Widget_Callback.To_Marshaller (Collapse_Row_Cb'Access),
         Explorer);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Button_Release_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);

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

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "project_Explorer.preferences_changed",
                Watch => GObject (Explorer));

      --  The explorer (project view) is automatically refreshed when the
      --  project view is changed.

      Widget_Callback.Object_Connect
        (Get_MDI (Kernel), Signal_Child_Selected,
         Child_Selected'Access, Explorer, After => True);

      Gtk.Dnd.Dest_Set
        (Explorer.Tree, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree, Signal_Drag_Data_Received,
         Drag_Data_Received'Access, Kernel_Handle (Kernel));

      --  Sorting is now alphabetic: directories come first, then files. Use
      --  a custom sort function

      Set_Sort_Func
        (+Explorer.Tree.Model,
         Display_Name_Column,
         Sort_Func      => Sort_Func'Access);
      Set_Sort_Column_Id
        (+Explorer.Tree.Model, Display_Name_Column, Sort_Ascending);

      --  Initialize tooltips

      Tooltip := new Explorer_Tooltips;
      Tooltip.Explorer := Project_Explorer_Access (Explorer);
      Set_Tooltip (Tooltip, Explorer.Tree, 250);
   end Initialize;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      A_Before_B : Gint := -1;
      B_Before_A : Gint := 1;
      A_Type     : constant Node_Types :=
                     Get_Node_Type (Gtk_Tree_Store (Model), A);
      B_Type     : constant Node_Types :=
                     Get_Node_Type (Gtk_Tree_Store (Model), B);
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
         else
            return B_Before_A;
         end if;
      end Alphabetical;

   begin
      Get_Sort_Column_Id (+Gtk_Tree_Store (Model), Column, Order);
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

         when Obj_Directory_Node =>
            case B_Type is
               when Project_Node_Types =>
                  if Projects_Before_Directories then
                     return B_Before_A;
                  else
                     return A_Before_B;
                  end if;

               when Directory_Node =>
                  return B_Before_A;

               when Obj_Directory_Node =>
                  return Alphabetical;

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

               when Directory_Node | Obj_Directory_Node =>
                  return B_Before_A;

               when Exec_Directory_Node =>
                  return Alphabetical;

               when others =>
                  return B_Before_A;
            end case;

         when others =>
            if B_Type = A_Type then
               return Alphabetical;
            else
               return B_Before_A;
            end if;
      end case;
   end Sort_Func;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Child    : constant MDI_Child :=
                   Find_MDI_Child_By_Tag
                     (Get_MDI (Kernel), Project_Explorer_Record'Tag);
      Explorer : Project_Explorer_Access;
   begin
      if Child /= null then
         Explorer := Project_Explorer_Access (Get_Widget (Child));
         Set_Font_And_Colors (Explorer.Tree, Fixed_Font => True);
      end if;
   end Preferences_Changed;

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
                   Display_Full_Name (Get_File_From_Node (E.Tree.Model, Node)))
      then
         return;
      end if;

      if not (Get_Widget (Child).all in Project_Explorer_Record'Class) then
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
         Explorer := Get_Or_Create_Project_View (User, Raise_Window => False);
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
         Trace (Me, Exc);
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
                    Get_Or_Create_Project_View (Kernel, Raise_Window => False);
      Iter      : constant Gtk_Tree_Iter :=
                    Find_Iter_For_Event (T.Tree, T.Tree.Model, Event);
      Item      : Gtk_Menu_Item;
      Sep       : Gtk_Separator_Menu_Item;
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
         Associate (Get_History (Kernel).all, Show_Absolute_Paths, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Update_Absolute_Paths'Access,
            Slot_Object => T);

         Gtk_New (Check, Label => -"Show flat view");
         Associate (Get_History (Kernel).all, Show_Flat_View, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Update_View'Access,
            Slot_Object => T);

         Gtk_New (Check, Label => -"Show hidden directories");
         Create_New_Boolean_Key_If_Necessary
           (Get_History (Kernel).all,
            Key           => Show_Hidden_Dirs,
            Default_Value => True);
         Associate (Get_History (Kernel).all, Show_Hidden_Dirs, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Update_View'Access,
            Slot_Object => T);

         Gtk_New (Sep);
         Append (Menu, Sep);
      end if;

      if Node_Type in Project_Node_Types
        and then Menu /= null
      then
         Gtk_New (Item, -"Parse all xref information");
         Add (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, Signal_Activate, On_Parse_Xref'Access, T);
      end if;

   exception
      when E : others => Trace (Me, E);
   end Explorer_Context_Factory;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
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
      Is_Leaf   : constant Boolean :=
                    not Has_Imported_Projects (Project)
                    and then Project.Attribute_Value
                      (Obj_Dir_Attribute) = ""
                    and then Source_Dirs (Project)'Length = 0;
      Node_Text : constant String := Project.Name;
      Node_Type : Node_Types := Project_Node;
      N         : Gtk_Tree_Iter;
      Ref       : Gtk_Tree_Iter := Null_Iter;

   begin
      if Project = No_Project then
         return Null_Iter;
      end if;

      if Extending_Project (Project) /= No_Project then
         Node_Type := Extends_Project_Node;

      elsif Project.Modified then
         Node_Type := Modified_Project_Node;
      end if;

      if Parent_Node /= Null_Iter then
         Ref := Children (Explorer.Tree.Model, Parent_Node);

         --  Insert the nodes sorted

         while Ref /= Null_Iter
           and then
             (Get_Node_Type (Explorer.Tree.Model, Ref) /= Project_Node
               or else Get_String (Explorer.Tree.Model, Ref,
                                    Display_Name_Column)
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

      Set_File (Explorer.Tree.Model, N, File_Column, Project_Path (Project));

      if Extending_Project (Project) /= No_Project then
         --  ??? We could use a different icon instead
         Set (Explorer.Tree.Model, N, Display_Name_Column,
              Display_Full_Name (Create (+Node_Text))
              & " (extended)" & Name_Suffix);
      else
         Set (Explorer.Tree.Model, N, Display_Name_Column,
              Display_Full_Name (Create (+(Node_Text & Name_Suffix))));
      end if;

      Set_Node_Type (Explorer.Tree.Model, N, Node_Type, False);

      Set (Explorer.Tree.Model, N, Project_Column, Project.Name);

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
      File : constant Virtual_File :=
               Get_File (Explorer.Tree.Model, Node, File_Column);
   begin
      if Get_History
        (Get_History (Explorer.Kernel).all, Show_Absolute_Paths)
      then
         Set (Explorer.Tree.Model, Node, Display_Name_Column,
              File.Display_Full_Name);
      else
         declare
            Rel_Path : constant String :=
                         +Relative_Path (File, Project_Directory (Project));
         begin
            if Rel_Path (Rel_Path'Last) = '/'
              or else Rel_Path (Rel_Path'Last) = '\'
            then
               Set
                 (Explorer.Tree.Model, Node,
                  Display_Name_Column,
                  Rel_Path (Rel_Path'First .. Rel_Path'Last - 1));
            else
               Set
                 (Explorer.Tree.Model, Node,
                  Display_Name_Column, Rel_Path);
            end if;
         end;
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
         It   : Gtk_Tree_Iter := Children (Exp.Tree.Model, Iter);
         Prj  : Project_Type := Project;
      begin
         case Get_Node_Type (Exp.Tree.Model, Iter) is
            when Project_Node_Types =>
               Prj := Get_Project_From_Node
                 (Exp.Tree.Model, Exp.Kernel, Iter, False);

            when Directory_Node_Types
               | File_Node | Category_Node | Entity_Node =>
               null;
         end case;

         while It /= Null_Iter loop
            case Get_Node_Type (Exp.Tree.Model, It) is
               when Project_Node_Types =>
                  Process_Node (It, No_Project);

               when Directory_Node_Types =>
                  Update_Directory_Node_Text (Exp, Prj, It);

               when others =>
                  null;
            end case;

            Next (Exp.Tree.Model, It);
         end loop;
      end Process_Node;

      Iter : Gtk_Tree_Iter := Get_Iter_First (Exp.Tree.Model);
      Sort : constant Gint := Freeze_Sort (Exp.Tree.Model);
   begin
      --  There can be multiple toplevel nodes in the case of the flat view
      while Iter /= Null_Iter loop
         Process_Node (Iter, Get_Project (Exp.Kernel));
         Next (Exp.Tree.Model, Iter);
      end loop;

      Thaw_Sort (Exp.Tree.Model, Sort);
   end Update_Absolute_Paths;

   -----------------
   -- Update_View --
   -----------------

   procedure Update_View
     (Explorer : access Gtk_Widget_Record'Class)
   is
      Tree : constant Project_Explorer := Project_Explorer (Explorer);
   begin
      Clear (Tree.Tree.Model);
      Refresh (Explorer);
   end Update_View;

   -----------------------------------
   -- Set_Directory_Node_Attributes --
   -----------------------------------

   procedure Set_Directory_Node_Attributes
     (Explorer  : access Project_Explorer_Record'Class;
      Directory : Virtual_File;
      Node      : Gtk_Tree_Iter;
      Project   : Project_Type;
      Node_Type : Directory_Node_Types) is
   begin
      Ensure_Directory (Directory);
      Set_File (Explorer.Tree.Model, Node, File_Column, Directory);

      Update_Directory_Node_Text (Explorer, Project, Node);

      Set_Node_Type (Explorer.Tree.Model, Node, Node_Type, False);
      Set (Explorer.Tree.Model, Node, Up_To_Date_Column, False);
   end Set_Directory_Node_Attributes;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Tooltip : access Explorer_Tooltips;
      Pixmap  : out Cairo_Surface;
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
      File       : Virtual_File;

      Text       : String_Access;
   begin
      Pixmap := Null_Surface;
      Area   := (0, 0, 0, 0);

      if not Active (Explorers_Tooltips) then
         return;
      end if;

      Window := Get_Bin_Window (Tooltip.Explorer.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Explorer.Tree, X, Y, Path,
         Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return;

      else
         --  Now check that the cursor is over a text

         Iter := Get_Iter (Tooltip.Explorer.Tree.Model, Path);

         declare
            Str     : constant String :=
                        Get_String
                          (Tooltip.Explorer.Tree.Model,
                           Iter, Display_Name_Column);
            S_Icon  : constant Gint := 15; -- size used for the icon
            S_Level : constant Gint := 12; -- size used for each indent level
            --  ??? S_Icon and S_Level have been computed experimentally. It is
            --  maybe possible to get the proper values from the Tree_View.
            Layout  : Pango_Layout;
            Width   : Gint;
            Height  : Gint;
         begin
            Font := Default_Font.Get_Pref_Font;
            Layout := Create_Pango_Layout (Tooltip.Explorer, "");
            Set_Markup (Layout, Str);
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
         when Project_Node_Types =>
            --  Project or extended project full pathname
            File := Get_File (Tooltip.Explorer.Tree.Model, Iter, File_Column);
            Text := new String'(File.Display_Full_Name);

         when Directory_Node_Types =>
            --  Directroy full pathname and project name
            --  Get parent node which is the project name
            Par := Parent (Tooltip.Explorer.Tree.Model, Iter);

            File := Get_File (Tooltip.Explorer.Tree.Model, Iter, File_Column);
            Text := new String'
              (File.Display_Full_Name
               & ASCII.LF &
               (-"in project ") &
               Get_String
                 (Tooltip.Explorer.Tree.Model, Par, Display_Name_Column));

         when File_Node =>
            --  Base filename and Project name
            --  Get grand-parent node which is the project node
            Par := Parent
              (Tooltip.Explorer.Tree.Model,
               Parent (Tooltip.Explorer.Tree.Model, Iter));

            Text := new String'
              (Get_String
                 (Tooltip.Explorer.Tree.Model, Iter,
                  Display_Name_Column)
               & ASCII.LF &
               (-"in project ") &
               Get_String
                 (Tooltip.Explorer.Tree.Model, Par, Display_Name_Column));

         when Entity_Node =>
            --  Entity (parameters) declared at Filename:line
            --  Get grand-parent node which is the filename node
            Par := Parent
              (Tooltip.Explorer.Tree.Model,
               Parent (Tooltip.Explorer.Tree.Model, Iter));

            Text := new String'
              (Get_String
                 (Tooltip.Explorer.Tree.Model, Iter, Display_Name_Column)
               & ASCII.LF &
               (-"declared at ") &
               Get_String (Tooltip.Explorer.Tree.Model, Par,
                 Display_Name_Column)
               & ':' &
               Image (Integer
                 (Get_Int (Tooltip.Explorer.Tree.Model, Iter, Line_Column))));

         when others =>
            null;
      end case;

      if Text /= null then
         Create_Pixmap_From_Text
           (Text.all,
            Font, Tooltip_Color.Get_Pref,
            Tooltip.Explorer.Tree,
            Pixmap,
            Use_Markup => True);
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
      Project : constant Project_Type :=
                  Get_Project_From_Node
                    (Explorer.Tree.Model, Explorer.Kernel, Node, False);
      Files   : File_Array_Access := Project.Source_Files (Recursive => False);
   begin
      Update_Project_Node (Explorer, Files.all, Node);
      Unchecked_Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
      Obj  : constant Virtual_File := Project.Object_Dir;
      Exec : constant Virtual_File := Executables_Directory (Project);

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
      if Obj /= GNATCOLL.VFS.No_File then
         Set_Directory_Node_Attributes
           (Explorer  => Explorer,
            Directory => Obj,
            Node      => Create_Object_Dir (Node),
            Project   => Project,
            Node_Type => Obj_Directory_Node);
      end if;

      if Exec /= GNATCOLL.VFS.No_File and then Exec /= Obj then
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
      when E : others => Trace (Exception_Handle, E);
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
      Sort_Col  : constant Gint := Freeze_Sort (Explorer.Tree.Model);

   begin
      --  If the node is not already up-to-date
      if not Is_Up_To_Date (Explorer.Tree.Model, Node) then
         --  Remove the dummy node, and report that the node is up-to-date

         N := Children (Explorer.Tree.Model, Node);
         if N /= Null_Iter then
            Remove (Explorer.Tree.Model, N);
         end if;

         Set (Explorer.Tree.Model, Node, Up_To_Date_Column, True);

         case Get_Node_Type (Explorer.Tree.Model, Node) is
            when Project_Node_Types =>
               Expand_Project_Node (Explorer, Node);

            when Directory_Node_Types =>
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

               --  ??? Expanding should already recompile the children...
               --               Compute_Children (Explorer, File);

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

      Thaw_Sort (Explorer.Tree.Model, Sort_Col);
   end Compute_Children;

   --------------------
   -- Idle_Scroll_To --
   --------------------

   function Idle_Scroll_To (Data : Explorer_Path) return Boolean is
   begin
      Scroll_To_Cell
        (Data.Explorer.Tree,
         Data.Path, null, True,
         0.1, 0.1);
      Path_Free (Data.Path);
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Idle_Scroll_To;

   -------------------
   -- Expand_Row_Cb --
   -------------------

   procedure Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      Margin    : constant Gint := 30;
      T         : constant Project_Explorer := Project_Explorer (Explorer);
      Area_Rect : Gdk_Rectangle;
      Path2     : Gtk_Tree_Path;
      Iter2     : Gtk_Tree_Iter;

      Success   : Boolean;
      pragma Unreferenced (Success);
      Dummy     : G_Source_Id;
      pragma Unreferenced (Dummy);

   begin
      if T.Expanding then
         return;
      end if;

      T.Expanding := True;

      Compute_Children (T, Iter);

      Success := Expand_Row (T.Tree, Path, False);

      --  Gtk+ Hack. Here, in Compute_Children, we have probably removed the
      --  prelight, either when re-sorting the tree, or when deleting rows
      --  (the dummy iter, or project-related iters).
      --  In order to re-prelight the expander, fake a motion event.
      --  This is a copy of a hack done in gtktreeview.c
      --  (gtk_tree_view_real_collapse_row)
      declare
         Event  : Gdk_Event;
         Child_X, Child_Y, X, Y  : Gint;
         Mask   : Gdk_Modifier_Type;
         Dummy_W : Gdk_Window;
         Child  : Gdk_Window;
         Parent : Gdk_Window;
         Dummy  : Boolean;
         pragma Unreferenced (Dummy);
         use Gdk;
      begin
         Child := Get_Bin_Window (T.Tree);
         Parent := Get_Parent (Child);
         Get_Pointer (Parent, X, Y, Mask, Dummy_W);
         if Dummy_W = Child then
            Allocate (Event      => Event,
                      Event_Type => Motion_Notify,
                      Window     => Get_Window (T.Tree));
            Get_Position (Child, Child_X, Child_Y);
            Set_Window (Event, Child);
            Set_Time (Event, 0);
            Set_X (Event, Gdouble (X - Child_X));
            Set_Y (Event, Gdouble (Y - Child_Y));
            Dummy := Return_Callback.Emit_By_Name
              (T.Tree, Signal_Motion_Notify_Event, Event);
         end if;
      end;

      Set_Node_Type
        (T.Tree.Model,
         Iter,
         Get_Node_Type (T.Tree.Model, Iter),
         True);

      if Realized_Is_Set (T.Tree) then
         Iter2 := Children (T.Tree.Model, Iter);

         if Iter2 /= Null_Iter then
            Path2 := Get_Path (T.Tree.Model, Iter2);
            Get_Cell_Area (T.Tree, Path2, Get_Column (T.Tree, 0), Area_Rect);
            if Area_Rect.Y > Get_Allocation_Height (T.Tree) - Margin then
               Dummy :=
                 Scroll_Idle.Idle_Add
                   (Idle_Scroll_To'Access, (T, Copy (Path)));
            end if;
            Path_Free (Path2);
         end if;
      end if;

      T.Expanding := False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         T.Expanding := False;
   end Expand_Row_Cb;

   ---------------------
   -- Collapse_Row_Cb --
   ---------------------

   procedure Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);

      E : constant Project_Explorer := Project_Explorer (Explorer);

   begin
      --  Redraw the pixmap

      Set_Node_Type
        (E.Tree.Model,
         Iter,
         Get_Node_Type (E.Tree.Model, Iter),
         False);
   end Collapse_Row_Cb;

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
      Iter  : Project_Iterator :=
        Project.Start (Recursive => True, Direct_Only => Direct_Only);
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

   -------------------------
   -- Update_Project_Node --
   -------------------------

   procedure Update_Project_Node
     (Explorer : access Project_Explorer_Record'Class;
      Files    : File_Array;
      Node     : Gtk_Tree_Iter)
   is
      function Is_Hidden (Dir : Virtual_File) return Boolean;
      --  Return true if Dir contains an hidden directory (a directory starting
      --  with a dot).

      Project   : constant Project_Type :=
                    Get_Project_From_Node
                      (Explorer.Tree.Model, Explorer.Kernel, Node, False);

      Dirs      : constant File_Array := Project.Source_Dirs;

      N, N2, N3 : Gtk_Tree_Iter;
      Index     : Natural;
      Prj       : Project_Type;
      Imported  : Project_Type_Array := Get_Imported_Projects (Project);

      ---------------
      -- Is_Hidden --
      ---------------

      function Is_Hidden (Dir : Virtual_File) return Boolean is

         Root : constant Virtual_File := Get_Root (Dir);
         D    : Virtual_File := Dir;

      begin
         while D /= GNATCOLL.VFS.No_File
           and then D /= Root
         loop
            if Is_Hidden (Explorer.Kernel, D.Base_Dir_Name) then
               return True;
            end if;

            D := D.Get_Parent;
         end loop;

         return False;
      end Is_Hidden;

      S_Dirs   : File_Node_Hash.Map;
      Old_Dirs : File_Node_Hash.Map;
      S_Files  : Filename_Node_Hash.Map;
      D_Cursor : File_Node_Hash.Cursor;
      S_Cursor : Filename_Node_Hash.Cursor;

   begin
      --  Depending on whether we used trusted mode or not, some extra
      --  directories might be displayed in addition to the explicit ones
      --  mentioned in the project. This would be the case if some of them are
      --  links but the user chose the trusted mode anyway. This is acceptable,
      --  since that is an explicit choice from the user.

      --  Find all existing source dirs and files. Directory names read from
      --  the tree always end with a directory separator.

      N := Children (Explorer.Tree.Model, Node);

      while N /= Null_Iter loop
         Iter_Copy (Source => N, Dest => N2);
         Next (Explorer.Tree.Model, N2);

         case Get_Node_Type (Explorer.Tree.Model, N) is
            when Directory_Node =>
               Include
                 (Old_Dirs, Get_Absolute_Name (Explorer.Tree.Model,  N), N);

               N3 := Children (Explorer.Tree.Model, N);
               while N3 /= Null_Iter loop
                  Include
                    (S_Files,
                     Full_Name
                       (Get_Absolute_Name (Explorer.Tree.Model, N3)).all,
                     N3);
                  Next (Explorer.Tree.Model, N3);
               end loop;

            when Obj_Directory_Node | Exec_Directory_Node =>
               --  Remove it, we'll put it back anyway
               Remove (Explorer.Tree.Model, N);

            when Project_Node_Types =>
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
               elsif Get_Expanded (Explorer.Tree, N) then
                  Update_Node (Explorer, N, Files_In_Project => null);
               end if;

            when others =>
               null;

         end case;

         Iter_Copy (Source => N2, Dest => N);
      end loop;

      --  Now add each file (and missing source dirs when needed). The
      --  directories are created based on the needs of the files, not on the
      --  source dirs attribute (this is done later for the directories that
      --  contain no file) so that when a project is created through the
      --  debugger for instance it doesn't have to include the runtime dirs
      --  and other directories that might contain source files.

      for F in Files'Range loop
         declare
            Dir : constant Virtual_File := Get_Parent (Files (F));
         begin
            D_Cursor := Find (S_Dirs, Dir);

            if D_Cursor = File_Node_Hash.No_Element then
               --  Was this directory already displayed in the tree ?

               D_Cursor := Find (Old_Dirs, Dir);

               if D_Cursor = File_Node_Hash.No_Element then
                  --  No, create it

                  if Get_History
                    (Get_History (Explorer.Kernel).all, Show_Hidden_Dirs)
                    or else not Is_Hidden (Dir)
                  then
                     Append
                       (Explorer.Tree.Model,
                        Iter    => N,
                        Parent  => Node);
                     Set_Directory_Node_Attributes
                       (Explorer  => Explorer,
                        Directory => Dir,
                        Node      => N,
                        Project   => Project,
                        Node_Type => Directory_Node);
                     Set (Explorer.Tree.Model, N, Up_To_Date_Column, True);
                     Include (S_Dirs, Dir, N);
                  else
                     N := Null_Iter;
                  end if;

               else
                  --  Consider the directory as new, ie we need to keep it for
                  --  the new representation of tree
                  N := Element (D_Cursor);
                  Set (Explorer.Tree.Model, N, Up_To_Date_Column, True);
                  Include (S_Dirs, Dir, N);
                  Delete (Old_Dirs, D_Cursor);
               end if;

            else
               N := Element (D_Cursor);
            end if;
         end;

         S_Cursor := Find (S_Files, Full_Name (Files (F)).all);

         if S_Cursor /= Filename_Node_Hash.No_Element then
            --  The file was already present in the tree, preserve it if it has
            --  the same parent directory

            N2 := Element (S_Cursor);
            if Parent (Explorer.Tree.Model, N2) /= N then
               --  Do not remove from the tree immediately, since we could end
               --  up with an empty directory node, which would thus become
               --  unexpanded.
               --  Add the file at the new location

               if N /= Null_Iter then
                  Append_File
                    (Explorer.Kernel, Explorer.Tree.Model, N, Files (F));
               end if;

            else
               --  Remove the file from the htable, so that we do not delete
               --  the node later
               Delete (S_Files, S_Cursor);
            end if;

         elsif N /= Null_Iter then
            Append_File
              (Explorer.Kernel, Explorer.Tree.Model, N, Files (F));
         end if;
      end loop;

      --  Remove file nodes that no longer correspond to existing files

      S_Cursor := First (S_Files);
      while Has_Element (S_Cursor) loop
         N := Element (S_Cursor);
         Remove (Explorer.Tree.Model, N);
         Next (S_Cursor);
      end loop;

      --  Remove directory nodes that no longer correspond to valid dirs.
      --  These are the empty directories, since we only create directories
      --  when we have files.

      N := Children (Explorer.Tree.Model, Node);
      while N /= Null_Iter loop
         Iter_Copy (Source => N, Dest => N2);
         Next (Explorer.Tree.Model, N);

         if Get_Node_Type (Explorer.Tree.Model, N2) = Directory_Node then
            if Find (Old_Dirs, Get_Absolute_Name (Explorer.Tree.Model, N2)) /=
              File_Node_Hash.No_Element
            then
               Remove (Explorer.Tree.Model, N2);
            end if;
         end if;
      end loop;

      --  Add those source directories that contain no file

      for D in Dirs'Range loop
         declare
            Dir : constant Virtual_File := Dirs (D);
         begin
            Ensure_Directory (Dir);

            if Find (S_Dirs, Dir) = File_Node_Hash.No_Element then
               if Get_History
                 (Get_History (Explorer.Kernel).all, Show_Hidden_Dirs)
                 or else not Is_Hidden (Dir)
               then
                  Append
                    (Explorer.Tree.Model,
                     Iter    => N,
                     Parent  => Node);
                  Set_Directory_Node_Attributes
                    (Explorer  => Explorer,
                     Directory => Dir,
                     Node      => N,
                     Project   => Project,
                     Node_Type => Directory_Node);
                  Set (Explorer.Tree.Model, N, Up_To_Date_Column, True);
               end if;
            end if;
         end;
      end loop;

      Add_Object_Directories (Explorer, Node, Project);

      --  Add project nodes

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
   end Update_Project_Node;

   -----------------
   -- Update_Node --
   -----------------

   procedure Update_Node
     (Explorer         : access Project_Explorer_Record'Class;
      Node             : Gtk_Tree_Iter;
      Files_In_Project : File_Array_Access;
      Force_Expanded   : Boolean := False)
   is
      Sort_Col  : constant Gint := Freeze_Sort (Explorer.Tree.Model);

      N, N2     : Gtk_Tree_Iter;
      Node_Type : constant Node_Types :=
                    Get_Node_Type (Explorer.Tree.Model, Node);
      N_Type    : Node_Types;
      Prj       : constant Project_Type :=
                    Get_Project_From_Node
                      (Explorer.Tree.Model, Explorer.Kernel, Node, False);
      Expanded  : constant Boolean := Get_Expanded (Explorer.Tree, Node);
   begin
      --  If the information about the node hasn't been computed before,
      --  then we don't need to do anything. This will be done when the
      --  node is actually expanded by the user.

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
               when Project_Node_Types =>
                  declare
                     Files : File_Array_Access := Files_In_Project;

                  begin
                     if Prj /= No_Project then
                        if Files_In_Project = null then
                           Files := Prj.Source_Files (Recursive => False);
                        end if;

                        Update_Project_Node (Explorer, Files.all, Node);

                        if Files_In_Project = null then
                           Unchecked_Free (Files);
                        end if;
                     end if;
                  end;

               when others => null;
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
      --  Change the icons to reflect the modified state of the project.
      if Node_Type in Project_Node_Types then
         if Prj /= No_Project and then Prj.Modified then
            N_Type := Modified_Project_Node;
         else
            N_Type := Project_Node;
         end if;

         Set_Node_Type (Explorer.Tree.Model, Node, N_Type, Expanded);
      end if;

      Thaw_Sort (Explorer.Tree.Model, Sort_Col);
   end Update_Node;

   ---------------------------------------
   -- Add_Or_Update_Flat_View_Root_Node --
   ---------------------------------------

   procedure Add_Or_Update_Flat_View_Root_Node
     (Explorer : access Project_Explorer_Record'Class)
   is
      Iter2, Iter3 : Gtk_Tree_Iter;
      Imported     : Project_Type_Array :=
                       Get_Imported_Projects
                         (Get_Project (Explorer.Kernel),
                          Direct_Only     => False,
                          Include_Project => True);
      Found        : Boolean;
      Id           : constant Gint := Freeze_Sort (Explorer.Tree.Model);

   begin
      Iter2 := Get_Iter_First (Explorer.Tree.Model);

      while Iter2 /= Null_Iter loop
         declare
            Name : constant String :=
              Get_String (Explorer.Tree.Model, Iter2, Project_Column);
         begin
            Found := False;

            for Im in Imported'Range loop
               if Imported (Im) /= No_Project
                 and then Imported (Im).Name = Name
               then
                  Imported (Im) := No_Project;
                  Found := True;
                  exit;
               end if;
            end loop;
         end;

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
         if Imported (Im) = No_Project then
            --  Already updated
            null;
         elsif Imported (Im) = Get_Project (Explorer.Kernel) then
            Iter2 := Add_Project_Node (Explorer, Imported (Im), Null_Iter,
                                       Name_Suffix => " (root project)");
         elsif Imported (Im) /= No_Project then
            Iter2 := Add_Project_Node (Explorer, Imported (Im), Null_Iter);
         end if;
      end loop;

      Thaw_Sort (Explorer.Tree.Model, Id);
   end Add_Or_Update_Flat_View_Root_Node;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
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
      --  need to do it now.

      elsif Get_Iter_First (T.Tree.Model) = Null_Iter then
         Iter := Add_Project_Node (T, Get_Project (T.Kernel));
         Path := Get_Path (T.Tree.Model, Iter);
         Dummy := Collapse_Row (T.Tree, Path);
         Compute_Children (T, Iter);

         --  Expand, but we do not need to recompute the children, since this
         --  was just done.
         Gtk.Handlers.Handler_Block (T.Tree, T.Expand_Id);
         Dummy := Expand_Row (T.Tree, Path, False);
         Gtk.Handlers.Handler_Unblock (T.Tree, T.Expand_Id);

         Path_Free (Path);

      --  If we are displaying a new view of the tree that was there before, we
      --  want to keep the project nodes, and most important their open/close
      --  status, so as to minimize the changes the user sees.

      else
         declare
            Path_Start, Path_End : Gtk_Tree_Path;
            Success : Boolean;
         begin

            --  Memorize the visible path before updating

            T.Tree.Get_Visible_Range (Path_Start, Path_End, Success);

            --  Update the tree

            Update_Node
              (T, Get_Iter_First (T.Tree.Model), Files_In_Project => null);

            --  Scroll to the initial location after updating

            if Success then
               T.Tree.Scroll_To_Cell
                 (Path      => Path_Start,
                  Column    => null,
                  Use_Align => True,
                  Row_Align => 0.0,
                  Col_Align => 0.0);
               Path_Free (Path_Start);
               Path_Free (Path_End);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Refresh;

   --------------------------------
   -- Get_Or_Create_Project_View --
   --------------------------------

   function Get_Or_Create_Project_View
     (Kernel       : access Kernel_Handle_Record'Class;
      Raise_Window : Boolean) return Project_Explorer
   is
      Explorer : Project_Explorer;
      Child    : MDI_Child;
      C2       : MDI_Explorer_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Record'Tag);

      if Child = null then
         Gtk_New (Explorer, Kernel);
         Refresh (Explorer);

         C2 := new MDI_Explorer_Child_Record;
         Initialize (C2, Explorer,
                     Default_Width  => 215,
                     Default_Height => 600,
                     Focus_Widget   => Gtk_Widget (Explorer.Tree),
                     Group          => Group_View,
                     Module         => Explorer_Module_ID);
         Set_Title (C2, -"Project",  -"Project");
         Put (Get_MDI (Kernel), C2, Initial_Position => Position_Left);

         Set_Focus_Child (C2);
         Raise_Child (C2);
         return Explorer;

      else
         if Raise_Window then
            Raise_Child (Child);
         end if;

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
      Explorer := Get_Or_Create_Project_View (Kernel, Raise_Window => True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Explorer;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject) is
   begin
      Explorer_Context_Factory
        (Context, Get_Kernel (Module.all),
         Gtk_Widget (Child), Child, null, null);
   end Default_Context_Factory;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Context : in out Explorer_Search_Context) is
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

   overriding procedure Search
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
                   Get_Or_Create_Project_View (Kernel, Raise_Window => True);

      Full_Name_For_Dirs : constant Boolean := Get_History
        (Get_History (Explorer.Kernel).all, Show_Absolute_Paths);
      --  Use full name of directory in search

      function Directory_Name (Dir : Virtual_File) return String;
      --  Return directory name for search.
      --  It returns Base_Name or Full_Name depending on Full_Name_For_Dirs

      procedure Initialize_Parser;
      --  Compute all the matching files and mark them in the htable

      function Next return Gtk_Tree_Iter;
      --  Return the next matching node

      procedure Next_Or_Child
        (Name           : String;
         Key            : String;
         Start          : Gtk_Tree_Iter;
         Check_Match    : Boolean;
         Check_Projects : Boolean;
         Result         : out Gtk_Tree_Iter;
         Finish         : out Boolean);
      pragma Inline (Next_Or_Child);
      --  Move to the next node, starting from a project or directory node by
      --  name Name and key Key.
      --  Key may differ from Name for directories, where Key is Full_Name,
      --  but Name could be Base_Name.
      --  If Check_Match is false, then this subprogram doesn't test if the
      --  node's Name matches context.
      --  If Check_Projects is true, then this subprogram maintain set of
      --  projects and process children nodes only for first occurrence of the
      --  project.

      procedure Next_File_Node
        (Start  : Gtk_Tree_Iter;
         Result : out Gtk_Tree_Iter;
         Finish : out Boolean);
      pragma Inline (Next_File_Node);
      --  Move to the next node, starting from a file node

      function Check_Entities (File : Virtual_File) return Boolean;
      pragma Inline (Check_Entities);
      --  Check if File contains any entity matching C.
      --  Return True if there is a match.

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

      --------------------
      -- Directory_Name --
      --------------------

      function Directory_Name (Dir : Virtual_File) return String is
      begin
         if Full_Name_For_Dirs then
            return Dir.Display_Full_Name;
         else
            return +Dir.Base_Dir_Name;
         end if;
      end Directory_Name;

      -------------------
      -- Next_Or_Child --
      -------------------

      procedure Next_Or_Child
        (Name           : String;
         Key            : String;
         Start          : Gtk_Tree_Iter;
         Check_Match    : Boolean;
         Check_Projects : Boolean;
         Result         : out Gtk_Tree_Iter;
         Finish         : out Boolean) is
      begin
         Finish := False;

         if Check_Match
           and then Start /= C.Current and then Match (C, Name) /= -1
         then
            Result := Start;
            Finish := True;

         elsif Get (C.Matches, Key) /= No_Match then
            if Check_Projects then
               declare
                  Project_Name : constant String :=
                    Explorer.Tree.Model.Get_String (Start, Project_Column);

               begin
                  if Projects.Contains (Project_Name) then
                     Result := Start;
                     Explorer.Tree.Model.Next (Result);

                  else
                     Projects.Insert (Project_Name);
                     Compute_Children (Explorer, Start);
                     Result := Children (Explorer.Tree.Model, Start);
                  end if;
               end;

            else
               Compute_Children (Explorer, Start);
               Result := Children (Explorer.Tree.Model, Start);
            end if;

         else
            Result := Start;
            Next (Explorer.Tree.Model, Result);
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
         N      : aliased constant Filesystem_String :=
                    Get_Base_Name (Explorer.Tree.Model, Start);
         Status : Search_Status;
      begin
         Status := Get (C.Matches, +N);
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
                 (Create_From_Dir
                    (Get_Directory_From_Node (Explorer.Tree.Model, Start), N))
               then
                  Set (C.Matches, +N, Search_Match);
                  Compute_Children (Explorer, Start);
                  Result := Children (Explorer.Tree.Model, Start);
                  Finish := False;
                  return;
               else
                  --  Decrease the count for importing directories and
                  --  projects, so that if no file belonging to them is
                  --  referenced any more, we simply don't parse them

                  Mark_File_And_Projects
                    (File => Create_From_Dir
                       (Get_Directory_From_Node (Explorer.Tree.Model, Start),
                        N),
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
                  when Project_Node_Types =>
                     declare
                        Name : constant String :=
                          Get_Project_From_Node
                            (Explorer.Tree.Model, Kernel, Start_Node, False)
                            .Name;
                     begin
                        Next_Or_Child
                          (Name           => Name,
                           Key            => Name,
                           Start          => Start_Node,
                           Check_Match    => Context.Include_Projects,
                           Check_Projects => True,
                           Result         => Tmp,
                           Finish         => Finish);

                        if Finish then
                           return Tmp;
                        end if;
                     end;

                  when Directory_Node =>
                     declare
                        Dir : constant Virtual_File := Get_Directory_From_Node
                          (Explorer.Tree.Model, Start_Node);
                     begin
                        Next_Or_Child
                          (Name           => Directory_Name (Dir),
                           Key            => Display_Full_Name (Dir),
                           Start          => Start_Node,
                           Check_Match    => Context.Include_Directories,
                           Check_Projects => False,
                           Result         => Tmp,
                           Finish         => Finish);

                        if Finish and then Context.Include_Directories then
                           return Tmp;
                        end if;
                     end;

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
                            (+Get_Base_Name (Explorer.Tree.Model, Start_Node)))
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

      function Check_Entities (File : Virtual_File) return Boolean is
         use type Entities.LI_Handler;
         Languages  : constant Language_Handler :=
                        Get_Language_Handler (Kernel);
         Handler    : constant Entities.LI_Handler :=
                        Get_LI_Handler_From_File (Languages, File);
         Constructs : Construct_List;
         Status     : Boolean := False;

      begin
         if Handler = null then
            return False;
         end if;

         Entities.Parse_File_Constructs (Handler, Languages, File, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Filter_Category (Constructs.Current.Category) /= Cat_Unknown
              and then Constructs.Current.Name /= No_Symbol
              and then Match (C, Get (Constructs.Current.Name).all) /= -1
            then
               Status := True;

               if Get (C.Matches, Get (Constructs.Current.Name).all) /=
                 Search_Match
               then
                  Set (C.Matches, Get (Constructs.Current.Name).all,
                       Search_Match);
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
         Parent : constant Virtual_File := Dir (File);
         Dir    : constant String := Display_Full_Name (Parent);
         Iter   : Project_Iterator;

      begin
         if File.Is_Directory then
            --  Use full name of directories to keep them unique
            Set (C.Matches, Display_Full_Name (File), Mark_File);
            --  Don't mark parent directory, because project view doesn't
            --  place directories inside directory
         else
            Set (C.Matches, +Base_Name (File), Mark_File);

            --  Mark the number of entries in the directory, so that if a file
            --  doesn't match we can decrease it later, and finally no longer
            --  examine the directory
            if Get (C.Matches, Dir) /= No_Match then
               Set (C.Matches, Dir, Get (C.Matches, Dir) + Increment);
            elsif Increment > 0 then
               Set (C.Matches, Dir, 1);
            end if;
         end if;

         if not Project_Marked then
            --  Mark the current project and all its importing projects as
            --  matching.

            declare
               N : constant String := Project.Name;
            begin
               Set (C.Matches, N, Get (C.Matches, N) + Increment);
            end;

            Iter := Find_All_Projects_Importing
              (Project      => Project);

            while Current (Iter) /= No_Project loop
               declare
                  N : constant String := Current (Iter).Name;
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
         Iter : Project_Iterator := Start
           (Get_Project (Kernel), Recursive => True);
         Project_Marked : Boolean := False;
      begin
         Projects.Clear;

         while Current (Iter) /= No_Project loop
            Project_Marked := False;

            if Match (C, Current (Iter).Name) /= -1 then
               Mark_File_And_Projects
                 (File           => Project_Path (Current (Iter)),
                  Project_Marked => Project_Marked,
                  Project        => Current (Iter),
                  Mark_File      => Unknown,
                  Increment      => 1);
            end if;

            if Context.Include_Directories then
               declare
                  Sources : constant File_Array := Current (Iter).Source_Dirs;
               begin
                  for S in Sources'Range loop
                     declare
                        Name : constant String := Directory_Name (Sources (S));
                     begin
                        if Match (C, Name) /= -1 then
                           Mark_File_And_Projects
                             (File           => Sources (S),
                              Project_Marked => Project_Marked,
                              Project        => Current (Iter),
                              Mark_File      => Search_Match,
                              Increment      => 1);
                           Project_Marked  := True;
                        end if;
                     end;
                  end loop;
               end;
            end if;

            declare
               Sources : File_Array_Access := Current (Iter).Source_Files;
            begin
               for S in Sources'Range loop
                  declare
                     Base : constant String := Display_Base_Name (Sources (S));
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

               GNATCOLL.VFS.Unchecked_Free (Sources);
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
      --  Expand Path and all parents of Path that are not expanded

      ----------------------
      -- Expand_Recursive --
      ----------------------

      procedure Expand_Recursive (The_Path : Gtk_Tree_Path) is
         Parent : constant Gtk_Tree_Path := Copy (The_Path);
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

   overriding function Execute
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
           "^" & (+Base_Name (File_Information (Context.Context))) & "$",
         Options  =>
           (Case_Sensitive => Is_Case_Sensitive (Get_Nickname (Build_Server)),
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
                 & Display_Base_Name (File_Information (Context.Context)),
                 Mode => GPS.Kernel.Console.Error);
      end if;

      Free (C);
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
         Look_For => Project_Information (Context.Context).Name,
         Options  =>
           (Case_Sensitive => Is_Case_Sensitive (Get_Nickname (Build_Server)),
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
                 & Project_Information (Context.Context).Name);
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

      Project_View_Filter   : constant Action_Filter :=
                                new Project_View_Filter_Record;
      Project_Node_Filter   : constant Action_Filter :=
                                new Project_Node_Filter_Record;
      Directory_Node_Filter : constant Action_Filter :=
                                new Directory_Node_Filter_Record;
      File_Node_Filter      : constant Action_Filter :=
                                new File_Node_Filter_Record;
      Entity_Node_Filter    : constant Action_Filter :=
                                new Entity_Node_Filter_Record;
      Command               : Interactive_Command_Access;

   begin
      Explorer_Module_ID := new Explorer_Module_Record;
      Register_Module
        (Module      => Explorer_Module_ID,
         Kernel      => Kernel,
         Module_Name => Explorer_Module_Name,
         Priority    => GPS.Kernel.Modules.Default_Priority);

      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Locate_File_In_Explorer_Command;
      Register_Contextual_Menu
        (Kernel, "Locate file in explorer",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "In project")
                     and not Create (Module => Explorer_Module_Name),
         Label  => "Locate in Project View: %f");

      Command := new Locate_Project_In_Explorer_Command;
      Register_Contextual_Menu
        (Kernel, "Locate project in explorer",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "Project only")
                     and not Create (Module => Explorer_Module_Name),
         Label  => "Locate in Project View: %p");

      Register_Menu
        (Kernel, Project, -"Project _View", "", On_Open_Explorer'Access);
      Register_Menu
        (Kernel, Tools, -"_Project", "", On_Open_Explorer'Access,
         Ref_Item => -"Remote");

      Extra := new Explorer_Search_Extra_Record;
      Gtk.Box.Initialize_Vbox (Extra);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Extra, Box);

      Gtk_New (Extra.Include_Projects, -"Projects");
      Pack_Start (Box, Extra.Include_Projects);
      Set_Active (Extra.Include_Projects, True);
      Kernel_Callback.Connect
        (Extra.Include_Projects, Signal_Toggled,
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Directories, -"Directories");
      Pack_Start (Box, Extra.Include_Directories);
      Set_Active (Extra.Include_Directories, True);
      Kernel_Callback.Connect
        (Extra.Include_Directories, Signal_Toggled,
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Files, -"Files");
      Pack_Start (Box, Extra.Include_Files);
      Set_Active (Extra.Include_Files, True);
      Kernel_Callback.Connect
        (Extra.Include_Files, Signal_Toggled,
         Reset_Search'Access, Kernel_Handle (Kernel));

      Gtk_New (Extra.Include_Entities, -"Entities (might be slow)");
      Pack_Start (Box, Extra.Include_Entities);
      Set_Active (Extra.Include_Entities, False);
      Kernel_Callback.Connect
        (Extra.Include_Entities, Signal_Toggled,
         Reset_Search'Access, Kernel_Handle (Kernel));

      Register_Filter
        (Kernel,
         Filter => Project_View_Filter,
         Name   => "Explorer_View");
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

      Register_Search_Function
        (Kernel            => Kernel,
         Label             => -"Project View",
         Factory           => Explorer_Search_Factory'Access,
         Extra_Information => Extra,
         Id                => Explorer_Module_ID,
         Mask              => All_Options and not Supports_Replace
         and not Search_Backward and not All_Occurrences);
   end Register_Module;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Filesystem_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (+Key);
   end Hash;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Explorer_Search_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"project explorer";
   end Context_Look_In;

end Project_Explorers;

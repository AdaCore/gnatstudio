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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Glib;                      use Glib;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk;                       use Gdk;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Clist;                 use Gtk.Clist;
with Gtk.Ctree;                 use Gtk.Ctree;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;
with Pixmaps_IDE;               use Pixmaps_IDE;

with Glide_Intl;                use Glide_Intl;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with File_Utils;                use File_Utils;
with Traces;                    use Traces;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

package body Directory_Tree is

   Drives_String : constant String := "Drives";
   --  String used on systems that support the notion of drives (e.g Windows).

   Me : constant Debug_Handle := Create ("Directory_Tree");

   package Boolean_Data is new Gtk.Ctree.Row_Data (Boolean);

   type Idle_Data is record
      Tree        : Dir_Tree;
      Dir         : String_Access;
      Node        : Gtk_Ctree_Node;
      Index       : Natural;
      Busy_Cursor : Gdk_Window;
   end record;
   type Idle_Data_Access is access Idle_Data;
   procedure Free is new
     Ada.Unchecked_Deallocation (Idle_Data, Idle_Data_Access);

   package Dir_Idle is new Gtk.Main.Idle (Idle_Data_Access);

   package Widget_Menus is new GUI_Utils.User_Contextual_Menus
     (User_Data => Directory_Selector);
   --  Used to register contextual menus with a user data.

   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import
     (C, Get_File_Names_Case_Sensitive,
      "__gnat_get_file_names_case_sensitive");

   Case_Sensitive_File_Name : constant Boolean :=
     Get_File_Names_Case_Sensitive = 1;

   function Add_Directory_Node
     (Tree               : access Dir_Tree_Record'Class;
      Dir                : String;
      Parent             : Gtk_Ctree_Node;
      Num_Subdirectories : Integer) return Gtk_Ctree_Node;
   --  Add a new node in tree to reference Dir. The new node is created as
   --  a child of Parent (or at the root of the tree if Parent is null).
   --  Num_Subdirectories should be different from 0 if Dir contains
   --  subdirectories.

   function Directory
     (Tree     : access Dir_Tree_Record'Class;
      N        : Gtk_Ctree_Node;
      Absolute : Boolean := False) return String;
   --  Return the directory associated with node. This doesn't include the
   --  absolute path to the directory, unless Absolute is True.

   function Find_Node
     (Tree : access Dir_Tree_Record'Class;
      Directory : String) return Gtk_Ctree_Node;
   --  Return the node for Directory, or null if not found.

   procedure Add_Sub_Directories
     (Tree : access Dir_Tree_Record'Class;
      N    : Gtk_Ctree_Node);
   --  Check on the disk what the subdirectories of Directory (N) are, and
   --  add them to the tree.
   --  Nothing is done if the subdirectories of N have already been parsed

   procedure Create_Directory_Cb
     (W : access Gtk_Widget_Record'Class);
   --  Create a new subdirectory of the selected directory.

   procedure Expand_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  Called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Collapse_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  called every time a node is collapsed. The children of the node are
   --  deleted, so that the contents is parsed again next time (in case the
   --  user has manually created directories)

   procedure Add_Directory
     (Selector  : access Directory_Selector_Record'Class;
      Dir       : String;
      Recursive : Boolean);
   --  Add Dir in the tree to the list of source directories associated with
   --  the project. If recursive is True, then all the subdirectories are also
   --  added.
   --  This procedure assumes that Dir have a trailing directory separator.

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the 'Add directory recursive' contextual menu.

   procedure Add_Single_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the down button in the directory selector widget
   --  The addition is not recursive.
   --  ??? This could be merged with the above procedure if Object_Connect
   --  could use a User_Data parameter.

   procedure Remove_Directory
     (Selector : access Directory_Selector_Record'Class; Recursive : Boolean);
   --  Remove the currently selected directory.
   --  If recursive is True, then all the subdirectories are also removed

   procedure Remove_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for 'Remove directory recursive' contextual menu.

   procedure Remove_Single_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Remove the currently selected directory in the selection list. This
   --  doesn't remove children of the directory.

   function Find_Directory_In_Selection
     (Selector : access Directory_Selector_Record'Class; Name : String)
      return Gint;
   --  -1 if Name is not a source directory for the project defined in Wiz.
   --  Otherwise, the index of Name in the list is returned.

   function Tree_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu;
   --  Return the contextual menu to use when the user clicks in the directory
   --  tree.

   function List_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu;
   --  Return the contextual menu to use when the user clicks in the list of
   --  selected directories

   function Single_List_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu;
   --  Return the contextual menu to use for a single directory selector.

   function Select_Directory_Idle (Data : Idle_Data_Access) return Boolean;
   --  Select a new directory in an idle loop (so that we can properly display
   --  the busy cursor, and the tree is not frozen graphically for very long).

   function Filter
     (Tree : access Dir_Tree_Record'Class; Dir_Name : String) return Boolean;
   --  Should return True if Dir_Name should be displayed in the tree.
   --  Dir_Name is the last part of the full path, ie shouldn't include its
   --  parent's name.

   procedure On_Destroy (Tree : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Tree : out Dir_Tree; Root : String) is
   begin
      Tree := new Dir_Tree_Record;
      Directory_Tree.Initialize (Tree, Root);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tree : access Dir_Tree_Record'Class; Root : String)
   is
      N : Gtk_Ctree_Node;
   begin
      Gtk.Ctree.Initialize (Tree, Columns => 1);

      Create_From_Xpm_D
        (Tree.Ofolder_Pix,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Tree.Ofolder_Mask,
         Transparent => Null_Color,
         Data        => mini_ofolder_xpm);
      Create_From_Xpm_D
        (Tree.Folder_Pix,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Tree.Folder_Mask,
         Transparent => Null_Color,
         Data        => mini_folder_xpm);

      Tree.Idle := 0;

      N := Add_Directory_Node
        (Tree, Root, null, Subdirectories_Count (Root));
      Widget_Callback.Connect (Tree, "tree_expand", Expand_Tree_Cb'Access);
      Widget_Callback.Connect (Tree, "tree_collapse", Collapse_Tree_Cb'Access);
      Widget_Callback.Connect
        (Tree, "destroy", Widget_Callback.To_Marshaller (On_Destroy'Access));

      --  ??? This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      Set_Column_Auto_Resize (Tree, 0, True);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Tree : access Gtk_Widget_Record'Class) is
   begin
      if Dir_Tree (Tree).Idle /= 0 then
         Idle_Remove (Dir_Tree (Tree).Idle);
      end if;
   end On_Destroy;

   ---------------
   -- Directory --
   ---------------

   function Directory
     (Tree     : access Dir_Tree_Record'Class;
      N        : Gtk_Ctree_Node;
      Absolute : Boolean := False) return String
   is
      S : constant String := Node_Get_Text (Tree, N, 0);
   begin
      if Absolute
        and then Row_Get_Parent (Node_Get_Row (N)) /= null
      then
         return Directory (Tree, Row_Get_Parent (Node_Get_Row (N)), Absolute)
           & S;
      else
         if S = -Drives_String then
            return "";
         else
            return S;
         end if;
      end if;
   end Directory;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Tree : access Dir_Tree_Record'Class;
      Directory : String) return Gtk_Ctree_Node
   is
      Current : Gtk_Ctree_Node := Node_Nth (Tree, 0);
      Children : Gtk_Ctree_Node;
      First, Last : Integer;
   begin
      --  Skip the first '/'
      First := Directory'First + 1;
      Last := First;
      while Last <= Directory'Last
        and then Directory (Last) /= '/'
        and then Directory (Last) /= Directory_Separator
      loop
         Last := Last + 1;
      end loop;

      Children := Row_Get_Children (Node_Get_Row (Current));
      while Children /= null loop
         if Node_Get_Text (Tree, Children, 0) = Directory (First .. Last) then
            Current := Children;
            Children := Row_Get_Children (Node_Get_Row (Current));
            First := Last + 1;
            Last := First;
            while Last <= Directory'Last
              and then Directory (Last) /= '/'
              and then Directory (Last) /= Directory_Separator
            loop
               Last := Last + 1;
            end loop;

            if Last > Directory'Last then
               return Current;
            end if;
         else
            Children := Row_Get_Sibling (Node_Get_Row (Children));
         end if;
      end loop;

      return null;
   end Find_Node;

   ------------
   -- Filter --
   ------------

   function Filter
     (Tree : access Dir_Tree_Record'Class; Dir_Name : String) return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Dir_Name /= "."
        and then Dir_Name /= ".."
        and then Dir_Name /= "CVS";
   end Filter;

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Node : constant Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Win  : constant Gdk_Window := Get_Window (Widget);
   begin
      if Win /= null then
         Set_Busy_Cursor (Win, True, True);
      end if;

      Add_Sub_Directories (Dir_Tree (Widget), Node);

      if Win /= null then
         Set_Busy_Cursor (Win, False);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Expand_Tree_Cb;

   ----------------------
   -- Collapse_Tree_Cb --
   ----------------------

   procedure Collapse_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Tree    : constant Dir_Tree := Dir_Tree (Widget);
      Node    : constant Gtk_Ctree_Node :=
        Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Current : Gtk_Ctree_Node;
      Sibling : Gtk_Ctree_Node;

   begin
      --  It might happen that this signal is emitted with Node = null, for
      --  instance if we are expanding a directory that has no children. In
      --  this case, either Node or Current will be null.

      if Node = null then
         return;
      end if;

      Current := Row_Get_Children (Node_Get_Row (Node));

      if Current = null
        or else Node_Get_Text (Tree, Node, 0) = -Drives_String
      then
         return;
      end if;

      Freeze (Tree);

      --  Leave only one child, so as to keep the "[+]" visible

      loop
         Sibling := Row_Get_Sibling (Node_Get_Row (Current));

         exit when Sibling = null;

         Remove_Node (Tree, Current);
         Current := Sibling;
      end loop;

      Boolean_Data.Node_Set_Row_Data (Tree, Node, False);
      Thaw (Tree);

   exception
      when E : others =>
         Thaw (Tree);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Collapse_Tree_Cb;

   -------------------------
   -- Add_Sub_Directories --
   -------------------------

   procedure Add_Sub_Directories
     (Tree : access Dir_Tree_Record'Class;
      N    : Gtk_Ctree_Node)
   is
      D       : Dir_Type;
      File    : String (1 .. 1024);
      Last    : Natural;
      N2      : Gtk_Ctree_Node;
      Num_Dir : Natural := 0;
      Num_Subdirectories : Integer;
      Has_Dummy_Node : Boolean;

   begin
      --  Have we already parsed the subdirectories ?

      if Boolean_Data.Node_Get_Row_Data (Tree, N) then
         return;
      end if;

      Freeze (Tree);

      Has_Dummy_Node := Row_Get_Children (Node_Get_Row (N)) /= null;

      declare
         Absolute : constant String := Directory (Tree, N, Absolute => True);
      begin
         begin
            Open (D, Absolute);

            loop
               Read (D, File, Last);

               exit when Last = 0;

               Num_Subdirectories :=
                 Subdirectories_Count (Absolute & File (File'First .. Last));

               if Num_Subdirectories /= -1
                 and then Filter (Tree, File (File'First .. Last))
               then
                  Num_Dir := Num_Dir + 1;
                  N2 := Add_Directory_Node
                    (Tree,
                     File (File'First .. Last) & Directory_Separator,
                     N,
                     Num_Subdirectories);
               end if;
            end loop;

            Close (D);

         exception
            when Directory_Error =>
               null;
         end;
      end;

      --  Remove the dummy node inserted when this node was created
      --  This can be done only after there are some other children, or the
      --  parent node is collapsed, thus causing another dummy node to be
      --  inserted.

      if Has_Dummy_Node then
         Remove_Node (Tree, Row_Get_Children (Node_Get_Row (N)));
         Boolean_Data.Node_Set_Row_Data (Tree, N, True);
      end if;

      if Num_Dir /= 0 then
         Sort_Node (Tree, N);
      end if;

      Thaw (Tree);
   end Add_Sub_Directories;

   ------------------------
   -- Add_Directory_Node --
   ------------------------

   function Add_Directory_Node
     (Tree               : access Dir_Tree_Record'Class;
      Dir                : String;
      Parent             : Gtk_Ctree_Node;
      Num_Subdirectories : Integer) return Gtk_Ctree_Node
   is
      N, N2     : Gtk_Ctree_Node;
      Buffer    : aliased String (1 .. 1024);
      Last, Len : Integer;

      function Insert_Directory_Node
        (Parent   : Gtk_Ctree_Node;
         Dir      : String;
         Expanded : Boolean := False) return Gtk_Ctree_Node;
      --  Insert a directory node in Tree given a parent and a sibling node.
      --  Dir is the name of the directory to insert.

      procedure Add_Dummy_Node (N : Gtk_Ctree_Node);
      --  Add a dummy node in Tree for a given node N.

      function Insert_Directory_Node
        (Parent   : Gtk_Ctree_Node;
         Dir      : String;
         Expanded : Boolean := False) return Gtk_Ctree_Node
      is
         Strings : Gtkada.Types.Chars_Ptr_Array (1 .. 1);
         Node    : Gtk_Ctree_Node;
      begin
         Strings (1) := New_String (Dir);
         Node := Insert_Node
           (Tree,
            Parent        => Parent,
            Sibling       => null,
            Text          => Strings,
            Spacing       => 5,
            Pixmap_Closed => Tree.Folder_Pix,
            Mask_Closed   => Tree.Folder_Mask,
            Pixmap_Opened => Tree.Ofolder_Pix,
            Mask_Opened   => Tree.Ofolder_Mask,
            Is_Leaf       => False,
            Expanded      => Expanded);
         Free (Strings);
         Boolean_Data.Node_Set_Row_Data (Tree, Node, False);
         return Node;
      end Insert_Directory_Node;

      procedure Add_Dummy_Node (N : Gtk_Ctree_Node) is
         Strings    : Gtkada.Types.Chars_Ptr_Array (1 .. 1);
         Dummy_Node : Gtk_Ctree_Node;
      begin
         Strings (1) := New_String (".");
         Dummy_Node := Insert_Node
           (Tree,
            Parent        => N,
            Sibling       => null,
            Text          => Strings,
            Spacing       => 5,
            Pixmap_Closed => null,
            Mask_Closed   => null,
            Pixmap_Opened => null,
            Mask_Opened   => null,
            Is_Leaf       => True,
            Expanded      => False);
         Free (Strings);
      end Add_Dummy_Node;

   begin
      --  Always create a node for directories, in case the user wants to add
      --  some extra information (files, ...) later on

      if Dir = (1 => Directory_Separator) then
         Get_Logical_Drive_Strings (Buffer, Len);

         if Len /= 0 then
            N := Insert_Directory_Node (Parent, -Drives_String, True);
            Boolean_Data.Node_Set_Row_Data (Tree, N, True);
            Last := 1;

            for J in 1 .. Len loop
               if Buffer (J) = ASCII.NUL then
                  N2 := Insert_Directory_Node (N, Buffer (Last .. J - 1));
                  Add_Dummy_Node (N2);
                  Last := J + 1;
               end if;
            end loop;

            return N;
         end if;
      end if;

      N := Insert_Directory_Node (Parent, Dir);

      --  Add a dummy node so that it is possible to expand dynamically a
      --  directory

      if Num_Subdirectories > 0 then
         Add_Dummy_Node (N);
      end if;

      return N;
   end Add_Directory_Node;

   -----------------
   -- Show_Parent --
   -----------------

   procedure Show_Parent (Tree : access Dir_Tree_Record) is
      N : Gtk_Ctree_Node;
   begin
      N := Row_Get_Parent
        (Node_Get_Row (Node_List.Get_Data (Get_Selection (Tree))));
      if N /= null then
         Gtk_Select (Tree, N);
         Node_Moveto (Tree, N, 0, 0.1, 0.2);
      end if;
   end Show_Parent;

   --------------------
   -- Show_Directory --
   --------------------

   procedure Show_Directory
     (Tree           : access Dir_Tree_Record;
      Dir            : String;
      Busy_Cursor_On : Gdk.Window.Gdk_Window := null)
   is
      Root     : constant Gtk_Ctree_Node := Node_Nth (Tree, 0);
      Root_Dir : constant String := Directory (Tree, Root, False);
      D        : constant String := Name_As_Directory (Dir);

   begin
      if Dir'Length < Root_Dir'Length
        or else D (D'First .. D'First + Root_Dir'Length - 1) /= Root_Dir
      then
         return;
      end if;

      if Busy_Cursor_On /= null then
         Set_Busy_Cursor (Busy_Cursor_On, True);
      end if;

      Tree.Idle := Dir_Idle.Add
        (Select_Directory_Idle'Access,
         new Idle_Data' (Tree        => Dir_Tree (Tree),
                         Dir         => new String' (D),
                         Node        => Node_Nth (Tree, 0),
                         Index       => D'First + Root_Dir'Length - 1,
                         Busy_Cursor => Busy_Cursor_On));
   end Show_Directory;

   ---------------------------
   -- Select_Directory_Idle --
   ---------------------------

   function Select_Directory_Idle (Data : Idle_Data_Access) return Boolean is
      Dir_End : Natural := Data.Index + 1;
      Tmp     : Gtk_Ctree_Node;
      D       : Idle_Data_Access := Data;

   begin
      --  Parse the directory name

      while Dir_End <= Data.Dir'Last
        and then Data.Dir (Dir_End) /= '/'
        and then Data.Dir (Dir_End) /= GNAT.OS_Lib.Directory_Separator
      loop
         Dir_End := Dir_End + 1;
      end loop;

      if Dir_End > Data.Dir'Last then
         Dir_End := Data.Dir'Last;
      end if;

      --  Find the node for the directory

      Expand (Data.Tree, Data.Node);

      Tmp := Row_Get_Children (Node_Get_Row (Data.Node));

      while Tmp /= null loop
         if Case_Sensitive_File_Name then
            if Node_Get_Text (Data.Tree, Tmp, 0) =
              Data.Dir (Data.Index + 1 .. Dir_End)
            then
               exit;
            end if;

         elsif To_Lower (Node_Get_Text (Data.Tree, Tmp, 0)) =
           To_Lower (Data.Dir (Data.Index + 1 .. Dir_End))
         then
            exit;
         end if;

         Tmp := Row_Get_Sibling (Node_Get_Row (Tmp));
      end loop;

      if Tmp = null
        or else Dir_End >= Data.Dir'Last
      then
         if Tmp = null then
            Tmp := Data.Node;
         end if;

         Gtk_Select (Data.Tree, Tmp);
         Node_Moveto (Data.Tree, Tmp, 0, 0.1, 0.2);

         if Data.Busy_Cursor /= null then
            Set_Busy_Cursor (Data.Busy_Cursor, False);
         end if;

         --  Force a recomputation of the scrollbars, since otherwise they
         --  won't appear until after the user has explicitly resized the
         --  widget.
         Queue_Resize (D.Tree);

         D.Tree.Idle := 0;
         Free (D.Dir);
         Free (D);

         return False;

      else
         Data.Node := Tmp;
         Data.Index := Dir_End;
         return True;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Select_Directory_Idle;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Tree : access Dir_Tree_Record) return String is
      use type Node_List.Glist;
   begin
      if Get_Selection (Tree) = Node_List.Null_List then
         return "";
      else
         return Directory
           (Tree, Node_List.Get_Data (Get_Selection (Tree)), Absolute => True);
      end if;
   end Get_Selection;

   ---------------------------------
   -- Find_Directory_In_Selection --
   ---------------------------------

   function Find_Directory_In_Selection
     (Selector : access Directory_Selector_Record'Class; Name : String)
      return Gint
   is
      Num_Rows : constant Gint := Get_Rows (Selector.List);
   begin
      --  Check if the directory is already there

      for J in 0 .. Num_Rows - 1 loop
         if Get_Text (Selector.List, J, 0) = Name then
            return J;
         end if;
      end loop;

      return -1;
   end Find_Directory_In_Selection;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Selector  : access Directory_Selector_Record'Class;
      Dir       : String;
      Recursive : Boolean)
   is
      Row     : Gint;
      Handle  : Dir_Type;
      File    : String (1 .. 1024);
      Last    : Natural;
      Strings : Gtkada.Types.Chars_Ptr_Array (1 .. 1);

   begin
      pragma Assert
        (Selector.List /= null, "Not a multiple-directory selector");

      Row := Find_Directory_In_Selection (Selector, Dir);

      if Row = -1 then
         Strings (1) := New_String (Dir);
         Row := Append (Selector.List, Strings);
         Free (Strings);
      end if;

      Select_Row (Selector.List, Row, -1);

      if Recursive then
         Open (Handle, Dir);

         loop
            Read (Handle, File, Last);

            exit when Last = 0;

            if Is_Directory (Dir & File (1 .. Last))
              and then Filter (Selector.Directory, File (1 .. Last))
            then
               Add_Directory
                 (Selector, Name_As_Directory (Dir & File (1 .. Last)), True);
            end if;
         end loop;

         Close (Handle);
      end if;

   exception
      when Directory_Error =>
         null;
   end Add_Directory;

   ----------------------
   -- Add_Directory_Cb --
   ----------------------

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
      Dir      : constant String    := Get_Selection (Selector.Directory);

   begin
      pragma Assert
        (Selector.List /= null, "Not a multiple-directory selector");

      if Dir /= "" then
         Freeze (Selector.List);
         Unselect_All (Selector.List);
         Add_Directory (Selector, Dir, Recursive => True);
         Sort (Selector.List);
         Thaw (Selector.List);

         --  Show the first selected item
         Moveto (Selector.List,
                 Gint_List.Get_Data (Get_Selection (Selector.Directory)),
                 0, 0.0, 0.2);
      end if;

   exception
      when E : others =>
         Thaw (Selector.List);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Add_Directory_Cb;

   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory
     (Selector : access Directory_Selector_Record'Class; Recursive : Boolean)
   is
      use type Gint_List.Glist;

      List    : Gint_List.Glist := Get_Selection (Selector.List);
      Next    : Gint_List.Glist;
      Num     : constant Guint := Gint_List.Length (List);
      Strings : Gtkada.Types.Chars_Ptr_Array (1 .. 1);

   begin
      --  Add the directories recursively to the selection (we can't remove
      --  them right away, since this would cancel the current selection and
      --  thus we wouldn't be able to remove all the user-selected ones).

      if Recursive then
         for J in 1 .. Num loop
            declare
               Row : constant Gint := Gint_List.Get_Data (List);
               Dir : constant String := Get_Text (Selector.List, Row, 0);

            begin
               for J in 0 .. Get_Rows (Selector.List) - 1 loop
                  declare
                     N : constant String := Get_Text (Selector.List, J, 0);
                  begin
                     if N'Length > Dir'Length
                       and then N (N'First .. N'First + Dir'Length - 1) = Dir
                     then
                        Select_Row (Selector.List, J, -1);
                     end if;
                  end;
               end loop;
            end;

            List := Gint_List.Next (List);
         end loop;
      end if;

      --  Now remove the whole selection

      List := Get_Selection (Selector.List);

      while List /= Gint_List.Null_List loop
         Next := Gint_List.Next (List);
         Remove (Selector.List, Gint_List.Get_Data (List));
         List := Next;
      end loop;

      --  Workaround for a possible bug in gtk+: when all the rows in the
      --  clist are removed with the loop above, we get a SEGV,
      --  unless we do the following ???

      Strings (1) := New_String ("");
      Remove (Selector.List, Append (Selector.List, Strings));
      Free (Strings);
   end Remove_Directory;

   -------------------------
   -- Remove_Directory_Cb --
   -------------------------

   procedure Remove_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
   begin
      Freeze (Selector.List);
      Remove_Directory (Selector, Recursive => True);
      Thaw (Selector.List);

   exception
      when E : others =>
         Thaw (Selector.List);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Remove_Directory_Cb;

   -----------------------------
   -- Add_Single_Directory_Cb --
   -----------------------------

   procedure Add_Single_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
   begin
      Add_Directory (Selector, Get_Selection (Selector.Directory), False);
      Sort (Selector.List);
   end Add_Single_Directory_Cb;

   --------------------------
   -- Tree_Contextual_Menu --
   --------------------------

   function Tree_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      Item         : Gtk_Menu_Item;
      Selected_Row : Gint;
      Selected_Col : Gint;
      Is_Valid     : Boolean;
      Menu         : Gtk_Menu;

   begin
      Get_Selection_Info
        (Selector.Directory,
         Gint (Get_X (Event)), Gint (Get_Y (Event)),
         Selected_Row, Selected_Col, Is_Valid);

      if Is_Valid then
         Select_Row (Selector.Directory, Selected_Row, Selected_Col);

         Gtk_New (Menu);
         Gtk_New (Item, -"Add directory recursive");
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Add_Directory_Cb'Access),
            Selector);
         Append (Menu, Item);

         Gtk_New (Item, -"Add directory");
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller
            (Add_Single_Directory_Cb'Access),
            Selector);
         Append (Menu, Item);

         Gtk_New (Item, -"Create new subdirectory");
         Append (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Create_Directory_Cb'Access),
            Selector);

         return Menu;
      end if;

      return null;
   end Tree_Contextual_Menu;

   --------------------------------
   -- Remove_Single_Directory_Cb --
   --------------------------------

   procedure Remove_Single_Directory_Cb
     (W : access Gtk_Widget_Record'Class) is
   begin
      Remove_Directory (Directory_Selector (W), Recursive => False);
   end Remove_Single_Directory_Cb;

   --------------------------
   -- List_Contextual_Menu --
   --------------------------

   function List_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      use type Gint_List.Glist;

      Item        : Gtk_Menu_Item;
      Is_Valid    : constant Boolean :=
        Get_Selection (Selector.List) /= Gint_List.Null_List;
      Menu        : Gtk_Menu;
      Row, Column : Gint;
      Valid       : Boolean;

   begin
      Get_Selection_Info
        (Selector.Directory,
         Gint (Get_X (Event)),
         Gint (Get_Y (Event)),
         Row, Column, Valid);

      if not Valid then
         return null;
      end if;

      Select_Row (Selector.Directory, Row, Column);

      Gtk_New (Menu);
      Gtk_New (Item, -"Remove directory recursive");
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Remove_Directory_Cb'Access),
         Selector);
      Set_Sensitive (Item, Is_Valid);
      Append (Menu, Item);

      Gtk_New (Item, -"Remove directory");
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller
         (Remove_Single_Directory_Cb'Access),
         Selector);
      Set_Sensitive (Item, Is_Valid);
      Append (Menu, Item);

      return Menu;
   end List_Contextual_Menu;

   -------------------------
   -- Create_Directory_Cb --
   -------------------------

   procedure Create_Directory_Cb
     (W : access Gtk_Widget_Record'Class)
   is
      Selector : constant Directory_Selector := Directory_Selector (W);
      Current_Dir : constant String := Get_Selection (Selector.Directory);
      Dialog : Gtk_Dialog;
      Label : Gtk_Label;
      Ent : Gtk_Entry;
      Widget : Gtk_Widget;
      N, Parent : Gtk_Ctree_Node;
   begin
      Gtk_New (Dialog,
               Title  => -"Create directory",
               Parent => Gtk_Window (Get_Toplevel (W)),
               Flags  => Modal or Destroy_With_Parent);

      Gtk_New (Label, -"Directory Name:");
      Pack_Start (Get_Vbox (Dialog), Label, Expand => False, Fill => True);

      Gtk_New (Ent, Max => 1024);
      Set_Width_Chars (Ent, 30);
      Set_Text (Ent, Current_Dir);
      Pack_Start (Get_Vbox (Dialog), Ent, Expand => True, Fill => True);

      Widget := Add_Button (Dialog, -"Create", Gtk_Response_OK);
      Widget := Add_Button (Dialog, -"Cancel", Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            D : constant String := Name_As_Directory (Get_Text (Ent));
            Parent_D : constant String := Dir_Name
              (D (D'First .. D'Last - 1));
            Base_D : constant String :=
              Name_As_Directory (Base_Name (D (D'First .. D'Last - 1)));

         begin
            --  Make sure the parent is expanded first. This will read the
            --  currently existing directories, and will avoid duplicating the
            --  new one, which would happen if we did if afterwards.
            Parent := Find_Node (Selector.Directory, Parent_D);
            if Parent /= null then
               Expand (Selector.Directory, Parent);
            end if;

            --  ??? Should we create the intermediate directories.
            Make_Dir (Get_Text (Ent));

            --  Refresh the tree to show the new directory
            if Parent /= null then
               N := Add_Directory_Node (Selector.Directory, Base_D, Parent, 0);
               Sort_Node (Selector.Directory, Parent);
               Node_Moveto (Selector.Directory, N, 0, 0.5, 0.5);
            end if;

         exception
            when Directory_Error =>
               Trace (Me, "Cannot create directory " & Get_Text (Ent));
               --  Insert (Kernel, "Cannot create directory" & Get_Text (Ent));
         end;
      end if;

      Destroy (Dialog);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Create_Directory_Cb;

   ---------------------------------
   -- Single_List_Contextual_Menu --
   ---------------------------------

   function Single_List_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      --  pragma Unreferenced (Event);
      Menu : Gtk_Menu;
      Item : Gtk_Menu_Item;
      Row, Column : Gint;
      Valid : Boolean;

   begin
      Get_Selection_Info
        (Selector.Directory,
         Gint (Get_X (Event)),
         Gint (Get_Y (Event)),
         Row, Column, Valid);

      if not Valid then
         return null;
      end if;

      Select_Row (Selector.Directory, Row, Column);

      Gtk_New (Menu);
      Gtk_New (Item, -"Create new subdirectory");
      Append (Menu, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Create_Directory_Cb'Access),
         Selector);

      return Menu;
   end Single_List_Contextual_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector             : out Directory_Selector;
      Initial_Directory    : String;
      Root_Directory       : String := (1 => GNAT.OS_Lib.Directory_Separator);
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection) is
   begin
      Selector := new Directory_Selector_Record;
      Initialize
        (Selector, Initial_Directory, Root_Directory,
         Multiple_Directories, Busy_Cursor_On, Initial_Selection);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Selector             : access Directory_Selector_Record'Class;
      Initial_Directory    : String;
      Root_Directory       : String := (1 => GNAT.OS_Lib.Directory_Separator);
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection)
   is
      Scrolled  : Gtk_Scrolled_Window;
      Bbox      : Gtk_Hbutton_Box;
      Button    : Gtk_Button;
      Arrow     : Gtk_Arrow;
      Vbox      : Gtk_Box;

   begin
      Initialize_Vpaned (Selector);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack1 (Selector, Scrolled, Resize => True);

      Gtk_New (Selector.Directory, Root_Directory);
      Add (Scrolled, Selector.Directory);

      Show_Directory (Selector.Directory, Initial_Directory, Busy_Cursor_On);

      if Multiple_Directories then
         Widget_Menus.Register_Contextual_Menu
           (Selector.Directory, Directory_Selector (Selector),
            Tree_Contextual_Menu'Access);

         Gtk_New_Vbox (Vbox, Homogeneous => False);
         Pack2 (Selector, Vbox, Resize => False);

         Gtk_New (Bbox);
         Set_Layout (Bbox, Buttonbox_Spread);
         Pack_Start (Vbox, Bbox, Expand => False, Fill => False);

         Gtk_New (Button);
         Gtk_New (Arrow, Arrow_Down, Shadow_In);
         Add (Button, Arrow);
         Pack_Start (Bbox, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Add_Single_Directory_Cb'Access),
            Selector);

         Gtk_New (Button);
         Gtk_New (Arrow, Arrow_Up, Shadow_In);
         Add (Button, Arrow);
         Pack_Start (Bbox, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Remove_Single_Directory_Cb'Access),
            Selector);

         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Vbox, Scrolled, Expand => True, Fill => True);

         Gtk_New (Selector.List, Columns => 1);
         Add (Scrolled, Selector.List);
         Set_Selection_Mode (Selector.List, Selection_Multiple);
         Widget_Menus.Register_Contextual_Menu
           (Selector.List, Directory_Selector (Selector),
            List_Contextual_Menu'Access);

         for J in Initial_Selection'Range loop
            Insert (Selector.List, -1,
                    (1 => New_String (Normalize_Pathname
                                      (Initial_Selection (J).all,
                                       Resolve_Links => False))));
         end loop;

         --  ??? This is a workaround for a horizontal scrollbar problem: When
         --  the clist is put in a scrolled window, and if this is not called,
         --  the scrollbar does not allow us to scroll as far right as
         --  possible...
         Set_Column_Auto_Resize (Selector.List, 0, True);

      else
         Widget_Menus.Register_Contextual_Menu
           (Selector.Directory, Directory_Selector (Selector),
            Single_List_Contextual_Menu'Access);
      end if;
   end Initialize;

   --------------------------
   -- Get_Single_Selection --
   --------------------------

   function Get_Single_Selection
     (Selector  : access Directory_Selector_Record'Class) return String
   is
      use type Gint_List.Glist;

      List : Gint_List.Glist;

   begin
      --  A single directory selector ?

      if Selector.List = null then
         return Get_Selection (Selector.Directory);
      else
         List := Get_Selection (Selector.List);

         if List /= Gint_List.Null_List then
            return Get_Text (Selector.List, Gint_List.Get_Data (List), 0);
         end if;

         return "";
      end if;
   end Get_Single_Selection;

   ----------------------------
   -- Get_Multiple_Selection --
   ----------------------------

   function Get_Multiple_Selection
     (Selector : access Directory_Selector_Record'Class)
      return GNAT.OS_Lib.Argument_List is
   begin
      --  A single directory selector ?

      if Selector.List = null then
         return (1 => new String' (Get_Selection (Selector.Directory)));
      else
         declare
            Length : constant Gint := Get_Rows (Selector.List);
            Args   : Argument_List (1 .. Natural (Length));

         begin
            for A in Args'Range loop
               Args (A) := new String'
                 (Get_Text (Selector.List, Gint (A) - 1, 0));
            end loop;

            return Args;
         end;
      end if;
   end Get_Multiple_Selection;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Selector : access Directory_Selector_Record)
      return Dir_Tree is
   begin
      return Selector.Directory;
   end Get_Tree;

end Directory_Tree;

-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;
with Pixmaps_IDE;               use Pixmaps_IDE;

with Glide_Intl;                use Glide_Intl;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with Unchecked_Deallocation;

package body Directory_Tree is

   --  Each node is associated with a single boolean that indicates whether
   --  the subdirectories where parsed or not.

   type My_Dialog_Record is new Gtk_Dialog_Record with record
      Cancelled : Boolean := False;
   end record;

   package Boolean_Data is new Gtk.Ctree.Row_Data (Boolean);

   type Idle_Data is record
      Tree        : Dir_Tree;
      Dir         : String_Access;
      Node        : Gtk_Ctree_Node;
      Index       : Natural;
      Busy_Cursor : Gdk_Window;
   end record;
   type Idle_Data_Access is access Idle_Data;
   procedure Free is new Unchecked_Deallocation (Idle_Data, Idle_Data_Access);

   package Dir_Idle is new Gtk.Main.Idle (Idle_Data_Access);

   package Widget_Menus is new GUI_Utils.User_Contextual_Menus
     (User_Data => Directory_Selector);
   --  Used to register contextual menus with a user data.

   function Add_Directory_Node
     (Tree       : access Dir_Tree_Record'Class;
      Parent_Dir : String;
      Dir        : String;
      Parent     : Gtk_Ctree_Node) return Gtk_Ctree_Node;
   --  Add a new node in tree to reference Dir. The new node is created as
   --  a child of Parent (or at the root of the tree if Parent is null).

   function Directory
     (Tree      : access Dir_Tree_Record'Class;
      N        : Gtk_Ctree_Node;
      Absolute : Boolean := False) return String;
   --  Return the directory associated with node. This doesn't include the
   --  absolute path to the directory, unless Absolute is True.

   procedure Add_Sub_Directories
     (Tree : access Dir_Tree_Record'Class;
      N    : Gtk_Ctree_Node);
   --  Check on the disk what the subdirectories of Directory (N) are, and
   --  add them to the tree.
   --  Nothing is done if the subdirectories of N have already been parsed

   function Has_Subdirectories
     (Tree : access Dir_Tree_Record'Class; Absolute_Dir : String)
      return Boolean;
   --  Return True if Absolute_Dir contains subdirectories.
   --  Absolute_Dir must end with a directory separator.

   procedure Expand_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

   procedure Collapse_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  called every time a node is collapsed. The children of the node are
   --  deleted, so that the contents is parsed again next time (in case the
   --  user has manually created directories)

   procedure Add_Directory
     (Selector : access Directory_Selector_Record'Class;
      Dir : String;
      Recursive : Boolean);
   --  Add Dir in the tree to the list of source directories associated with
   --  the project.  If recursive is True, then all the subdirectories are also
   --  added.

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the down button in the directory selector widget

   procedure Add_Single_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the up button in the source directory selection.
   --  The addition is not recursive.
   --  ??? This could be merged with the above procedure if Object_Connect
   --  could use a User_Data parameter.

   procedure Remove_Directory
     (Selector : access Directory_Selector_Record'Class; Recursive : Boolean);
   --  Remove the currently selected directory.
   --  If recursive is True, then all the subdirectories are also removed

   procedure Remove_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the up button in the source directory selection

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

      N := Add_Directory_Node (Tree, "", Root, null);
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
      Absolute : Boolean := False) return String is
   begin
      --  ??? Not very efficient, since we use the secondary stack. Oh well...
      if Absolute
        and then Row_Get_Parent (Node_Get_Row (N)) /= null
      then
         return Directory (Tree, Row_Get_Parent (Node_Get_Row (N)), Absolute)
           & Node_Get_Text (Tree, N, 0);
      else
         return Node_Get_Text (Tree, N, 0);
      end if;
   end Directory;

   ------------------------
   -- Has_Subdirectories --
   ------------------------

   function Has_Subdirectories
     (Tree : access Dir_Tree_Record'Class; Absolute_Dir : String)
      return Boolean
   is
      D    : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
   begin
      Open (D, Absolute_Dir);

      loop
         Read (D, File, Last);
         exit when Last = 0;

         if Is_Directory  (Absolute_Dir & File (File'First .. Last))
           and then Filter (Tree, File (File'First .. Last))
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
   end Has_Subdirectories;

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
      Node : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Win  : Gdk_Window := Get_Window (Widget);
   begin
      if Win /= null then
         Set_Busy_Cursor (Win, True, True);
      end if;

      Add_Sub_Directories (Dir_Tree (Widget), Node);

      if Win /= null then
         Set_Busy_Cursor (Win, False);
      end if;
   end Expand_Tree_Cb;

   ----------------------
   -- Collapse_Tree_Cb --
   ----------------------

   procedure Collapse_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Tree : Dir_Tree := Dir_Tree (Widget);
      Node : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Tmp  : Gtk_Ctree_Node;
      Tmp2 : Gtk_Ctree_Node;

   begin
      --  It might happen (because of a problem somewhere else in this widget),
      --  that this signal is emitted with Node = null, for instance if we are
      --  expanding a directory that in fact has no children. This test is used
      --  to prevent a crash in that case.
      if Node = null then
         return;
      end if;

      Tmp := Row_Get_Children (Node_Get_Row (Node));
      Freeze (Tree);

      --  Leave only one child, so as to keep the "[+]" visible

      loop
         Tmp2 := Row_Get_Sibling (Node_Get_Row (Tmp));

         exit when Tmp2 = null;

         Remove_Node (Tree, Tmp);
         Tmp := Tmp2;
      end loop;

      Boolean_Data.Node_Set_Row_Data (Tree, Node, False);
      Thaw (Tree);
   end Collapse_Tree_Cb;

   -------------------------
   -- Add_Sub_Directories --
   -------------------------

   procedure Add_Sub_Directories
     (Tree : access Dir_Tree_Record'Class;
      N    : Gtk_Ctree_Node)
   is
      D    : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
      N2   : Gtk_Ctree_Node;

   begin
      --  Have we already parsed the subdirectories ?

      if Boolean_Data.Node_Get_Row_Data (Tree, N) then
         return;
      end if;

      Freeze (Tree);

      declare
         Absolute : constant String := Directory (Tree, N, Absolute => True);
      begin
         Open (D, Absolute);
         loop
            Read (D, File, Last);
            exit when Last = 0;

            if Is_Directory (Absolute & File (File'First .. Last))
              and then Filter (Tree, File (File'First .. Last))
            then
               N2 := Add_Directory_Node
                 (Tree,
                  Absolute,
                  File (File'First .. Last) & Directory_Separator,
                  N);
            end if;
         end loop;
         Close (D);
      end;

      --  Remove the dummy node inserted when this node was created
      --  This can be done only after there are some other children, or the
      --  parent node is collapsed, thus causing another dummy node to be
      --  inserted.

      Remove_Node (Tree, Row_Get_Children (Node_Get_Row (N)));

      Boolean_Data.Node_Set_Row_Data (Tree, N, True);
      Sort_Node (Tree, N);
      Thaw (Tree);
   end Add_Sub_Directories;

   ------------------------
   -- Add_Directory_Node --
   ------------------------

   function Add_Directory_Node
     (Tree       : access Dir_Tree_Record'Class;
      Parent_Dir : String;
      Dir        : String;
      Parent     : Gtk_Ctree_Node) return Gtk_Ctree_Node
   is
      N, N2 : Gtk_Ctree_Node;
   begin
      --  Always create a node for directories, in case the user wants to add
      --  some extra information (files, ...) later on

      N := Insert_Node
        (Tree,
         Parent        => Parent,
         Sibling       => null,
         Text          => Null_Array + Dir,
         Spacing       => 5,
         Pixmap_Closed => Tree.Folder_Pix,
         Mask_Closed   => Tree.Folder_Mask,
         Pixmap_Opened => Tree.Ofolder_Pix,
         Mask_Opened   => Tree.Ofolder_Mask,
         Is_Leaf       => False,
         Expanded      => False);
      Boolean_Data.Node_Set_Row_Data (Tree, N, False);

      --  Should add a dummy node
      if Has_Subdirectories (Tree, Parent_Dir & Dir) then
         N2 := Insert_Node
           (Tree,
            Parent        => N,
            Sibling       => null,
            Text          => Null_Array + ".",
            Spacing       => 5,
            Pixmap_Closed => null,
            Mask_Closed   => null,
            Pixmap_Opened => null,
            Mask_Opened   => null,
            Is_Leaf       => True,
            Expanded      => False);
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
      D : constant String := Name_As_Directory (Dir);
   begin
      if Busy_Cursor_On /= null then
         Set_Busy_Cursor (Busy_Cursor_On, True);
      end if;

      if Dir'Length < Root_Dir'Length
        or else D (D'First .. D'First + Root_Dir'Length - 1) /= Root_Dir
      then
         return;
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
      Tmp : Gtk_Ctree_Node;
      D : Idle_Data_Access := Data;
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
         exit when Node_Get_Text (Data.Tree, Tmp, 0) =
           Data.Dir (Data.Index + 1 .. Dir_End);
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

         D.Tree.Idle := 0;
         Free (D.Dir);
         Free (D);
         return False;

      else
         Data.Node := Tmp;
         Data.Index := Dir_End;
         return True;
      end if;
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
      Row    : Gint;
      Handle : Dir_Type;
      File   : String (1 .. 1024);
      Last   : Natural;

   begin
      pragma Assert
        (Selector.List /= null, "Not a multiple-directory selector");

      Row := Find_Directory_In_Selection (Selector, Dir);

      if Row = -1 then
         Row := Append (Selector.List, Null_Array + Dir);
      end if;

      Select_Row (Selector.List, Row, -1);

      if Recursive then
         Open (Handle, Dir);

         loop
            Read (Handle, File, Last);

            exit when Last = 0;

            if Is_Directory (Dir & File (File'First .. Last))
              and then Filter (Selector.Directory, File (File'First .. Last))
            then
               Add_Directory (Selector, Dir & File (1 .. Last), True);
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
      Selector : Directory_Selector := Directory_Selector (W);
      Dir : constant String := Get_Selection (Selector.Directory);
   begin
      pragma Assert
        (Selector.List /= null, "Not a multiple-directory selector");
      if Dir /= "" then
         Freeze (Selector.List);
         Unselect_All (Selector.List);
         Add_Directory (Selector, Dir, True);
         Sort (Selector.List);
         Thaw (Selector.List);

         --  Show the first selected item
         Moveto (Selector.List,
                 Gint_List.Get_Data (Get_Selection (Selector.Directory)),
                 0, 0.0, 0.2);
      end if;
   end Add_Directory_Cb;

   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory
     (Selector : access Directory_Selector_Record'Class; Recursive : Boolean)
   is
      use type Gint_List.Glist;

      List : Gint_List.Glist := Get_Selection (Selector.List);
      Next : Gint_List.Glist;
      Num  : Guint := Gint_List.Length (List);

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
      --  clist are removed with the loop above, we get a STORAGE_ERROR,
      --  unless we do the following

      Remove (Selector.List, Append (Selector.List, Null_Array + ""));
   end Remove_Directory;

   -------------------------
   -- Remove_Directory_Cb --
   -------------------------

   procedure Remove_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : Directory_Selector := Directory_Selector (W);
   begin
      Freeze (Selector.List);
      Remove_Directory (Selector, Recursive => True);
      Thaw (Selector.List);
   end Remove_Directory_Cb;

   -----------------------------
   -- Add_Single_Directory_Cb --
   -----------------------------

   procedure Add_Single_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : Directory_Selector := Directory_Selector (W);
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

   begin
      if Selector.Tree_Contextual_Menu /= null then
         Destroy (Selector.Tree_Contextual_Menu);
      end if;

      Get_Selection_Info
        (Selector.Directory,
         Gint (Get_X (Event)), Gint (Get_Y (Event)),
         Selected_Row, Selected_Col, Is_Valid);

      if Is_Valid then
         Select_Row (Selector.Directory, Selected_Row, Selected_Col);

         Gtk_New (Selector.Tree_Contextual_Menu);
         Gtk_New (Item, -"Add directory recursive");
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Add_Directory_Cb'Access),
            Selector);
         Append (Selector.Tree_Contextual_Menu, Item);

         Gtk_New (Item, -"Add directory");
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller
            (Add_Single_Directory_Cb'Access),
            Selector);
         Append (Selector.Tree_Contextual_Menu, Item);

         return Selector.Tree_Contextual_Menu;
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
      pragma Unreferenced (Event);

      use type Gint_List.Glist;

      Item     : Gtk_Menu_Item;
      Is_Valid : constant Boolean :=
        Get_Selection (Selector.List) /= Gint_List.Null_List;

   begin
      if Selector.List_Contextual_Menu /= null then
         Destroy (Selector.List_Contextual_Menu);
      end if;

      Gtk_New (Selector.List_Contextual_Menu);
      Gtk_New (Item, -"Remove directory recursive");
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Remove_Directory_Cb'Access),
         Selector);
      Set_Sensitive (Item, Is_Valid);
      Append (Selector.List_Contextual_Menu, Item);

      Gtk_New (Item, -"Remove directory");
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller
         (Remove_Single_Directory_Cb'Access),
         Selector);
      Set_Sensitive (Item, Is_Valid);
      Append (Selector.List_Contextual_Menu, Item);

      return Selector.List_Contextual_Menu;
   end List_Contextual_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector             : out Directory_Selector;
      Initial_Directory    : String;
      Root_Directory       : String := "/";
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
      Root_Directory       : String := "/";
      Multiple_Directories : Boolean := False;
      Busy_Cursor_On       : Gdk.Window.Gdk_Window := null;
      Initial_Selection    : GNAT.OS_Lib.Argument_List := No_Selection)
   is
      Scrolled  : Gtk_Scrolled_Window;
      Bbox      : Gtk_Hbutton_Box;
      Button    : Gtk_Button;
      Arrow     : Gtk_Arrow;

   begin
      Initialize_Vbox (Selector, Homogeneous => False, Spacing => 0);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Selector, Scrolled, Expand => True, Fill => True);

      Gtk_New (Selector.Directory, Root_Directory);
      Add (Scrolled, Selector.Directory);

      Show_Directory (Selector.Directory, Initial_Directory, Busy_Cursor_On);

      if Multiple_Directories then
         Widget_Menus.Register_Contextual_Menu
           (Selector.Directory, Directory_Selector (Selector),
            Tree_Contextual_Menu'Access);

         Gtk_New (Bbox);
         Set_Layout (Bbox, Buttonbox_Spread);
         Pack_Start (Selector, Bbox, Expand => False, Fill => False);

         Gtk_New (Button);
         Gtk_New (Arrow, Arrow_Down, Shadow_In);
         Add (Button, Arrow);
         Pack_Start (Bbox, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Add_Directory_Cb'Access),
            Selector);

         Gtk_New (Button);
         Gtk_New (Arrow, Arrow_Up, Shadow_In);
         Add (Button, Arrow);
         Pack_Start (Bbox, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, "clicked",
            Widget_Callback.To_Marshaller (Remove_Directory_Cb'Access),
            Selector);

         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Selector, Scrolled, Expand => True, Fill => True);

         Gtk_New (Selector.List, Columns => 1);
         Add (Scrolled, Selector.List);
         Set_Selection_Mode (Selector.List, Selection_Multiple);
         Widget_Menus.Register_Contextual_Menu
           (Selector.List, Directory_Selector (Selector),
            List_Contextual_Menu'Access);

         for J in Initial_Selection'Range loop
            Insert (Selector.List, -1,
                    (1 => New_String (Initial_Selection (J).all)));
         end loop;

         --  ??? This is a workaround for a horizontal scrollbar problem: When
         --  the clist is put in a scrolled window, and if this is not called,
         --  the scrollbar does not allow us to scroll as far right as
         --  possible...
         Set_Column_Auto_Resize (Selector.List, 0, True);
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

   ---------
   -- Run --
   ---------

   function Run
     (Selector : access Directory_Selector_Record'Class;
      Title    : String;
      Parent   : access Gtk.Window.Gtk_Window_Record'Class)
      return Gtk.Dialog.Gtk_Response_Type
   is
      Dialog   : Gtk_Dialog;
      Button   : Gtk_Widget;
      Response : Gtk_Response_Type;

   begin
      Gtk_New (Dialog,
               Title => Title,
               Parent => Parent,
               Flags => Modal or Destroy_With_Parent);
      Set_Default_Size (Dialog, 640, 480);
      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Pack_Start (Get_Vbox (Dialog), Selector, Fill => True, Expand => True);
      Show_All (Dialog);

      Response := Run (Dialog);
      Ref (Selector);
      Remove (Get_Vbox (Dialog), Selector);
      Destroy (Dialog);

      return Response;
   end Run;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Selector : access Directory_Selector_Record)
      return Dir_Tree is
   begin
      return Selector.Directory;
   end Get_Tree;

end Directory_Tree;

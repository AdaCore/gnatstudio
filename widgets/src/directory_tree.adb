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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Glib;                      use Glib;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixmap;                use Gdk.Pixmap;
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
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;
with Pixmaps_IDE;               use Pixmaps_IDE;

with GUI_Utils;                 use GUI_Utils;

package body Directory_Tree is

   --  Each node is associated with a single boolean that indicates whether
   --  the subdirectories where parsed or not.

   type My_Dialog_Record is new Gtk_Dialog_Record with record
      Cancelled : Boolean := False;
   end record;

   package Boolean_Data is new Gtk.Ctree.Row_Data (Boolean);

   package Widget_Menus is new GUI_Utils.User_Contextual_Menus
     (User_Data => Directory_Selector);
   --  Used to register contextual menus with a user data.

   function Find_In_Tree
     (Tree             : access Dir_Tree_Record'Class;
      Dir              : String;
      Add_If_Necessary : Boolean := False) return Gtk_Ctree_Node;
   --  Return the node matching Dir in the tree.
   --  Dir is the absolute path to the directory. If the node is not already
   --  in the tree, it is added if Add_If_Necessary is True.

   procedure Realized (Tree : access Gtk_Widget_Record'Class);

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

   function Has_Subdirectories (Absolute_Dir : String) return Boolean;
   --  Return True if Absolute_Dir contains subdirectories.
   --  Absolute_Dir must end with a directory separator.

   procedure Expand_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  called every time a node is expanded. It is responsible for
   --  automatically adding the children of the current node if they are not
   --  there already.

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

   procedure Ok_Clicked_Cb (Dialog : access Gtk_Widget_Record'Class);
   procedure Cancel_Clicked_Cb (Dialog : access Gtk_Widget_Record'Class);
   --  Callback for the buttons in the high-level dialogs

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

   procedure Initialize (Tree : access Dir_Tree_Record'Class; Root : String) is
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

      Add_Directory (Tree, Root);
      Widget_Callback.Connect (Tree, "tree_expand", Expand_Tree_Cb'Access);

      --  ??? This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      Set_Column_Auto_Resize (Tree, 0, True);

      Widget_Callback.Connect
        (Tree, "realize", Widget_Callback.To_Marshaller (Realized'Access));
   end Initialize;

   --------------
   -- Realized --
   --------------

   procedure Realized (Tree : access Gtk_Widget_Record'Class) is
      T : Dir_Tree := Dir_Tree (Tree);
   begin
      Show_Directory (T, Get_Current_Dir);
   end Realized;

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

   function Has_Subdirectories (Absolute_Dir : String) return Boolean is
      D    : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
   begin
      Open (D, Absolute_Dir);
      loop
         Read (D, File, Last);
         exit when Last = 0;

         if File (File'First .. Last) /= "."
           and then File (File'First .. Last) /= ".."
           and then Is_Directory  (Absolute_Dir & File (File'First .. Last))
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

   --------------------
   -- Expand_Tree_Cb --
   --------------------

   procedure Expand_Tree_Cb
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      Node : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
   begin
      Add_Sub_Directories (Dir_Tree (Widget), Node);
   end Expand_Tree_Cb;

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

      --  Remove the dummy node inserted when this node was created
      Remove_Node (Tree, Row_Get_Children (Node_Get_Row (N)));

      declare
         Absolute : constant String := Directory (Tree, N, Absolute => True);
      begin
         Open (D, Absolute);
         loop
            Read (D, File, Last);
            exit when Last = 0;

            if File (File'First .. Last) /= "."
              and then File (File'First .. Last) /= ".."
              and then Is_Directory (Absolute & File (File'First .. Last))
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

      Boolean_Data.Node_Set_Row_Data (Tree, N, True);
      Sort_Recursive (Tree, N);
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
      if Has_Subdirectories (Parent_Dir & Dir) then
         N2 := Insert_Node
           (Tree,
            Parent        => N,
            Sibling       => null,
            Text          => Null_Array + "",
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

   ------------------
   -- Find_In_Tree --
   ------------------

   function Find_In_Tree
     (Tree             : access Dir_Tree_Record'Class;
      Dir              : String;
      Add_If_Necessary : Boolean := False) return Gtk_Ctree_Node
   is
      use type Gtk.Ctree.Row_List.Glist;
      Dir_Start : Positive := Dir'First;
      Dir_End   : Natural;
      Current, Tmp : Gtk_Ctree_Node;

   begin
      pragma Assert
        (Dir (Dir'Last) = '/' or else Dir (Dir'Last) = Directory_Separator);

      if Get_Row_List (Tree) /= Gtk.Ctree.Row_List.Null_List then
         Current := Find_Node_Ptr
           (Tree, Gtk.Ctree.Row_List.Get_Data (Get_Row_List (Tree)));
      end if;

      Freeze (Tree);

      while Dir_Start <= Dir'Last loop
         Dir_End := Dir_Start;

         --  Parse the directory name
         while Dir_End <= Dir'Last
           and then Dir (Dir_End) /= '/'
           and then Dir (Dir_End) /= GNAT.OS_Lib.Directory_Separator
         loop
            Dir_End := Dir_End + 1;
         end loop;

         --  Include the directory separator in the name
         if Dir_End <= Dir'Last then
            Dir_End := Dir_End + 1;
         end if;

         --  Find the node for the directory
         Tmp := Current;
         while Tmp /= null loop
            exit when Directory (Tree, Tmp) = Dir (Dir_Start .. Dir_End - 1);
            Tmp := Row_Get_Sibling (Node_Get_Row (Tmp));
         end loop;

         --  No parent found => Add a new node if necessary
         if Tmp = null then
            if Add_If_Necessary then
               if Current /= null then
                  Current := Row_Get_Parent (Node_Get_Row (Current));
               end if;
               Current := Add_Directory_Node
                 (Tree, "", Dir (Dir_Start .. Dir_End - 1), Current);
               Sort (Tree);
            else
               Thaw (Tree);
               return null;
            end if;
         else
            Current := Tmp;
         end if;

         Dir_Start := Dir_End;

         --  Prepare the next iteration (get the children of the directory)
         if Dir_Start <= Dir'Last then
            Add_Sub_Directories (Tree, Current);
            Current := Row_Get_Children (Node_Get_Row (Current));
         end if;
      end loop;

      Thaw (Tree);
      return Current;
   end Find_In_Tree;

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

   procedure Show_Directory (Tree : access Dir_Tree_Record; Dir : String) is
      N : Gtk_Ctree_Node := Find_In_Tree (Tree, Dir);
      N2 : Gtk_Ctree_Node := N;
   begin
      if not Is_Viewable (Tree, N) then
         loop
            N := Row_Get_Parent (Node_Get_Row (N));
            exit when N = null;
            Expand (Tree, N);
         end loop;
      end if;

      Gtk_Select (Tree, N2);
      Node_Moveto (Tree, N2, 0, 0.1, 0.2);
   end Show_Directory;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory (Tree : access Dir_Tree_Record; Dir : String) is
      N : Gtk_Ctree_Node;
   begin
      N := Find_In_Tree (Tree, Dir, Add_If_Necessary => True);
   end Add_Directory;

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
     (Selector : access Directory_Selector_Record'Class;
      Dir : String;
      Recursive : Boolean)
   is
      Row : Gint;
      Handle : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
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

            --  ??? Should share filter with Directory_Tree
            if File (File'First .. Last) /= "."
              and then File (File'First .. Last) /= ".."
              and then Is_Directory (Dir & File (File'First .. Last))
            then
               Add_Directory
                 (Selector,
                  Dir & File (1 .. Last) & Directory_Separator,
                  True);
            end if;
         end loop;

         Close (Handle);
      end if;
   end Add_Directory;

   ----------------------
   -- Add_Directory_Cb --
   ----------------------

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : Directory_Selector := Directory_Selector (W);
   begin
      pragma Assert
        (Selector.List /= null, "Not a multiple-directory selector");
      Freeze (Selector.List);
      Unselect_All (Selector.List);
      Add_Directory (Selector, Get_Selection (Selector.Directory), True);
      Sort (Selector.List);
      Thaw (Selector.List);

      --  Show the first selected item
      Moveto (Selector.List,
              Gint_List.Get_Data (Get_Selection (Selector.Directory)),
              0, 0.0, 0.2);
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

   procedure Add_Single_Directory_Cb
     (W : access Gtk_Widget_Record'Class)
   is
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
      Item     : Gtk_Menu_Item;
      Selected_Row, Selected_Col : Gint;
      Is_Valid : Boolean;

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
         Gtk_New (Item, "Add directory recursive");
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Add_Directory_Cb'Access),
            Selector);
         Append (Selector.Tree_Contextual_Menu, Item);

         Gtk_New (Item, "Add directory");
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
      Event  : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      use type Gint_List.Glist;
      Item : Gtk_Menu_Item;
      Is_Valid : constant Boolean :=
        Get_Selection (Selector.List) /= Gint_List.Null_List;

   begin
      if Selector.List_Contextual_Menu /= null then
         Destroy (Selector.List_Contextual_Menu);
      end if;

      Gtk_New (Selector.List_Contextual_Menu);
      Gtk_New (Item, "Remove directory recursive");
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Remove_Directory_Cb'Access),
         Selector);
      Set_Sensitive (Item, Is_Valid);
      Append (Selector.List_Contextual_Menu, Item);

      Gtk_New (Item, "Remove directory");
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
      Multiple_Directories : Boolean := False) is
   begin
      Selector := new Directory_Selector_Record;
      Initialize
        (Selector, Initial_Directory, Root_Directory, Multiple_Directories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Selector             : access Directory_Selector_Record'Class;
      Initial_Directory    : String;
      Root_Directory       : String := "/";
      Multiple_Directories : Boolean := False)
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

      Show_Directory (Selector.Directory, Initial_Directory);

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
         Set_Selection_Mode (Selector.List, Selection_Extended);
         Widget_Menus.Register_Contextual_Menu
           (Selector.List, Directory_Selector (Selector),
            List_Contextual_Menu'Access);

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
      return GNAT.OS_Lib.Argument_List
   is
      use type Gint_List.Glist;
      List : Gint_List.Glist;
   begin
      --  A single directory selector ?
      if Selector.List = null then
         return (1 => new String' (Get_Selection (Selector.Directory)));

      else
         List := Get_Selection (Selector.List);
         declare
            Length : constant Integer := Integer (Gint_List.Length (List));
            Args : Argument_List (1 .. Length);
         begin
            for A in Args'Range loop
               Args (A) := new String'
                 (Get_Text (Selector.List, Gint_List.Get_Data (List), 0));
               List := Gint_List.Next (List);
            end loop;
            return Args;
         end;
      end if;
   end Get_Multiple_Selection;

   -------------------
   -- Ok_Clicked_Cb --
   -------------------

   procedure Ok_Clicked_Cb (Dialog : access Gtk_Widget_Record'Class) is
   begin
      My_Dialog_Record (Dialog.all).Cancelled := False;
      Main_Quit;
   end Ok_Clicked_Cb;

   -----------------------
   -- Cancel_Clicked_Cb --
   -----------------------

   procedure Cancel_Clicked_Cb (Dialog : access Gtk_Widget_Record'Class) is
   begin
      My_Dialog_Record (Dialog.all).Cancelled := True;
      Main_Quit;
   end Cancel_Clicked_Cb;

   --------------------------------------
   -- Single_Directory_Selector_Dialog --
   --------------------------------------

   function Single_Directory_Selector_Dialog (Initial_Directory : String)
      return String
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Button;
      Selector : Directory_Selector;
   begin
      Dialog := new My_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog);
      Set_Title (Dialog, "Select directory");
      Set_Default_Size (Dialog, 600, 480);

      Gtk_New (Selector, Initial_Directory, Multiple_Directories => False);
      Pack_Start (Get_Vbox (Dialog), Selector, Fill => True, Expand => True);

      Gtk_New (Button, "OK");
      Pack_Start
        (Get_Action_Area (Dialog), Button, Fill => False, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Ok_Clicked_Cb'Access), Dialog);

      Gtk_New (Button, "Cancel");
      Pack_Start
        (Get_Action_Area (Dialog), Button, Fill => False, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Cancel_Clicked_Cb'Access), Dialog);

      Show_All (Dialog);
      Main;

      if My_Dialog_Record (Dialog.all).Cancelled then
         Destroy (Dialog);
         return "";
      else
         declare
            Name : constant String := Get_Single_Selection (Selector);
         begin
            Destroy (Dialog);
            return Name;
         end;
      end if;
   end Single_Directory_Selector_Dialog;

   ------------------------------------------
   -- Multiple_Directories_Selector_Dialog --
   ------------------------------------------

   function Multiple_Directories_Selector_Dialog
     (Initial_Directory : String) return GNAT.OS_Lib.Argument_List
   is
      Dialog : Gtk_Dialog;
      Button : Gtk_Button;
      Selector : Directory_Selector;
   begin
      Dialog := new My_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog);
      Set_Title (Dialog, "Select multiple directories");
      Set_Default_Size (Dialog, 600, 480);

      Gtk_New (Selector, Initial_Directory, Multiple_Directories => True);
      Pack_Start (Get_Vbox (Dialog), Selector, Fill => True, Expand => True);

      Gtk_New (Button, "OK");
      Pack_Start
        (Get_Action_Area (Dialog), Button, Fill => False, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Ok_Clicked_Cb'Access), Dialog);

      Gtk_New (Button, "Cancel");
      Pack_Start
        (Get_Action_Area (Dialog), Button, Fill => False, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Cancel_Clicked_Cb'Access), Dialog);

      Show_All (Dialog);
      Main;

      if My_Dialog_Record (Dialog.all).Cancelled then
         Destroy (Dialog);
         return (1 .. 2 => null);
      else
         declare
            List : constant Argument_List := Get_Multiple_Selection (Selector);
         begin
            Destroy (Dialog);
            return List;
         end;
      end if;
   end Multiple_Directories_Selector_Dialog;

end Directory_Tree;


with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Glib;                      use Glib;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Ctree;                 use Gtk.Ctree;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;

package body Directory_Tree is

   folder_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, folder_xpm, "mini_folder_xpm");

   ofolder_xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, ofolder_xpm, "mini_ofolder_xpm");

   --  Each node is associated with a single boolean that indicates whether
   --  the subdirectories where parsed or not.

   package Boolean_Data is new Row_Data (Boolean);

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
         Data        => ofolder_xpm);
      Create_From_Xpm_D
        (Tree.Folder_Pix,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Tree.Folder_Mask,
         Transparent => Null_Color,
         Data        => folder_xpm);

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
      use type Row_List.Glist;
      Dir_Start : Positive := Dir'First;
      Dir_End   : Natural;
      Current, Tmp : Gtk_Ctree_Node;

   begin
      pragma Assert
        (Dir (Dir'Last) = '/' or else Dir (Dir'Last) = Directory_Separator);

      if Get_Row_List (Tree) /= Row_List.Null_List then
         Current :=
           Find_Node_Ptr (Tree, Row_List.Get_Data (Get_Row_List (Tree)));
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

   --------------------
   -- Show_Directory --
   --------------------

   procedure Show_Directory (Tree : access Dir_Tree_Record; Dir : String) is
      N : Gtk_Ctree_Node := Find_In_Tree (Tree, Dir);
      N2 : Gtk_Ctree_Node := N;
--        Pos : Gfloat;
   begin
      if not Is_Viewable (Tree, N) then
         loop
            N := Row_Get_Parent (Node_Get_Row (N));
            exit when N = null;
            Expand (Tree, N);
         end loop;
      end if;

      Gtk_Select (Tree, N2);

      --  Scroll to make the directory visible

--        Pos := 0.0;
--        pragma Assert (Get_Vadjustment (Tree) /= null);
--        while Pos <= Get_Upper (Get_Vadjustment (Tree))
--          and then Node_Is_Visible (Tree, N2) = Visibility_None
--        loop
--           Put_Line ("No match at"
--                     & Get_Value (Get_Vadjustment (Tree))'Img
--                     & "  Visible="
--                     & Node_Is_Visible (Tree, N2)'Img);
--           Set_Value (Get_Vadjustment (Tree), Pos);
--           Pos := Pos + Get_Page_Size (Get_Vadjustment (Tree)) / 2.0;
--        end loop;

      --  Moveto (Tree, 30, 0, 0.0, 0.2);
      Node_Moveto (Tree, N2, 0, 0.1, 0.2);
--        if Node_Is_Visible (Tree, N2) /= Visibility_Full then
--           Node_Moveto (Tree, N2, 0, 0.1, 0.2);
--        end if;
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

end Directory_Tree;

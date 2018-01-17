------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;

with Cairo;                     use Cairo;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Gdk;                       use Gdk;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gdk.Event;                 use Gdk.Event;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GUI_Utils;                 use GUI_Utils;
with OS_Utils;                  use OS_Utils;
with UTF8_Utils;                use UTF8_Utils;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

package body Directory_Tree is
   Me : constant Trace_Handle := Create ("Directory_Tree");

   Icon_Column      : constant := 0;
   Base_Name_Column : constant := 1;
   File_Column      : constant := 2;

   Open_Directory_Node : constant String :=
      "gps-emblem-directory-open";
   Closed_Directory_Node : constant String :=
      "gps-emblem-directory-closed";

   package Widget_Menus is new GUI_Utils.User_Contextual_Menus
     (User_Data => Directory_Selector);
   --  Used to register contextual menus with a user data

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because GNATCOLL.VFS.GtkAda.Virtual_File_Type is not constant.

   procedure Set_Columns
     (Model : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      Icon  : String;
      Base  : String;
      File  : Virtual_File);
   --  Set model's values.

   procedure Set_Columns
     (Model : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      Base  : String;
      File  : Virtual_File);
   --  Set model's values.

   package Dir_List is new Ada.Containers.Vectors (Positive, Virtual_File);
   use Dir_List;

   type Append_Directory_Idle_Data is record
      Explorer      : Dir_Tree;
      Norm_Dest     : Virtual_File;
      Norm_Dir      : Virtual_File;
      Depth         : Integer := 0;
      Base          : Gtk_Tree_Iter;
      Dirs          : Dir_List.Vector;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True;
   end record;

   procedure Free (Data : in out Append_Directory_Idle_Data_Access);

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view

   procedure File_Append_Directory
     (Explorer      : access Dir_Tree_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : Virtual_File  := No_File;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True);
   --  Add to the file view the directory Dir, at node given by Iter.
   --  If Append_To_Dir is not "", and is a sub-directory of Dir, then
   --  the path is expanded recursively all the way to Append_To_Dir.

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is expanded in the file view.
   --  It is responsible for automatically adding the children of the current
   --  node if they are not there already.

   function Expose_Event_Cb
     (Explorer : access Gtk_Widget_Record'Class;
      Cr       : Cairo_Context) return Boolean;
   --  Scroll the explorer to the current directory

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is collapsed in the file view

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view

   procedure File_Remove_Idle_Calls
     (Explorer : access Dir_Tree_Record'Class);
   --  Remove the idle calls for filling the file view

   function On_Button_Press
     (Tree      : access Gtk_Tree_View_Record'Class;
      Model     : Gtk_Tree_Store;
      Event     : Gdk_Event;
      Add_Dummy : Boolean) return Boolean;
   --  If the Event is a button click, expand the node or jump to the
   --  location accordingly, and return whether the event should be propagated.
   --  If Add_Dummy is true, a dummy node will be added to nodes collapsed
   --  by this call.

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the file view

   procedure Free_Children
     (T    : Dir_Tree;
      Iter : Gtk_Tree_Iter);
   --  Free all the children of iter Iter in the file view

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  Atomic function to read a directory using the information in D.
   --  Called by File_Append_Directory.

   procedure Refresh
     (Explorer : access Dir_Tree_Record'Class;
      Dir      : Virtual_File);
   --  Refresh the contents of the explorer.
   --  Show directory Dir.

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter);
   --  Append an empty iter to Base

   procedure Create_Directory_Cb
     (W : access Gtk_Widget_Record'Class);
   --  Create a new subdirectory of the selected directory

   procedure Add_Directory
     (Selector  : access Directory_Selector_Record'Class;
      Dir       : Virtual_File;
      Recursive : Boolean);
   --  Add Dir in the tree to the list of source directories associated with
   --  the project. If recursive is True, then all the subdirectories are also
   --  added.
   --  This procedure assumes that Dir have a trailing directory separator.

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the 'Add directory recursive' contextual menu

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
   --  Callback for 'Remove directory recursive' contextual menu

   procedure Remove_Single_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Remove the currently selected directory in the selection list. This
   --  doesn't remove children of the directory.

   function Find_Directory_In_Selection
     (Selector : access Directory_Selector_Record'Class;
      File     : Virtual_File)
      return Gtk_Tree_Iter;
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
   --  Return the contextual menu to use for a single directory selector

   function Get_First_Selected
     (Selector : Directory_Selector) return Gtk_Tree_Iter;
   --  Return the first selected item, or Null_Iter;

   function Filter
     (Tree     : access Dir_Tree_Record'Class;
      Dir_Name : String) return Boolean;
   --  Should return True if Dir_Name should be displayed in the tree.
   --  Dir_Name is the last part of the full path, ie shouldn't include its
   --  parent's name.

   procedure On_Tree_Select_Row
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a change in the directory selection

   procedure On_Tree_Size_Allocate
     (Object : access GObject_Record'Class;
      Params : GValues);
   --  Callback for a change in the directory tree size

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Append_Directory_Idle_Data_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Append_Directory_Idle_Data, Append_Directory_Idle_Data_Access);
   begin
      Data.Dirs.Clear;
      Unchecked_Free (Data);
   end Free;

   ---------------------------
   -- On_Tree_Size_Allocate --
   ---------------------------

   procedure On_Tree_Size_Allocate
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
      Tree  : constant Dir_Tree := Dir_Tree (Object);
      Path  : Gtk_Tree_Path;
      Col   : Gtk_Tree_View_Column;

   begin
      Get_Cursor (Tree.File_Tree, Path, Col);
      Scroll_To_Cell (Tree.File_Tree, Path, null, True, 0.1, 0.1);
      Path_Free (Path);
   end On_Tree_Size_Allocate;

   ------------------------
   -- On_Tree_Select_Row --
   ------------------------

   procedure On_Tree_Select_Row
     (Object : access GObject_Record'Class;
      Params : GValues)
   is
      Tree  : constant Dir_Tree := Dir_Tree (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      pragma Unreferenced (Params);
   begin
      Get_Selected (Get_Selection (Tree.File_Tree), Model, Iter);

      if Iter /= Null_Iter then
         Tree.Current_Dir := Get_File (Tree.File_Model, Iter, File_Column);
      end if;
   end On_Tree_Select_Row;

   ------------
   -- Filter --
   ------------

   function Filter
     (Tree : access Dir_Tree_Record'Class; Dir_Name : String)
      return Boolean
   is
      pragma Unreferenced (Tree);
   begin
      return Dir_Name /= "."
        and then Dir_Name /= ".."
        and then Dir_Name /= "CVS";
   end Filter;

   -----------------
   -- Show_Parent --
   -----------------

   procedure Show_Parent (Tree : access Dir_Tree_Record) is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
   begin
      Get_Selected (Get_Selection (Tree.File_Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Iter := Parent (Tree.File_Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (Tree.File_Model, Iter);
      Scroll_To_Cell (Tree.File_Tree, Path, null, True, 0.1, 0.1);
      Set_Cursor (Tree.File_Tree, Path, null, False);
      Path_Free (Path);
   end Show_Parent;

   --------------------
   -- Show_Directory --
   --------------------

   procedure Show_Directory
     (Tree           : access Dir_Tree_Record;
      Dir            : GNATCOLL.VFS.Virtual_File;
      Busy_Cursor_On : Gdk.Gdk_Window := null)
   is
      Parent   : Gtk_Tree_Iter;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path;
      Curr_Dir : Virtual_File;
      Success  : Boolean;
      Ignore   : Boolean;
      pragma Unreferenced (Busy_Cursor_On, Ignore);

   begin
      --  Find the first non-expanded iter

      Parent := Get_Iter_First (Tree.File_Model);
      if Parent /= Null_Iter then
         Curr_Dir := Get_File (Tree.File_Model, Parent, File_Column);

         --  we changed the remote host. rebuild whole tree
         if Get_Root (Curr_Dir) /= Get_Root (Dir) then
            Refresh (Tree, Dir);
            Parent := Get_Iter_First (Tree.File_Model);
         end if;
      end if;

      Iter := Parent;

      while Iter /= Null_Iter loop
         Path := Get_Path (Tree.File_Model, Iter);

         Curr_Dir := Get_File (Tree.File_Model, Iter, File_Column);

         if Curr_Dir = Dir then
            Scroll_To_Cell (Tree.File_Tree, Path, null, True, 0.1, 0.1);
            Set_Cursor (Tree.File_Tree, Path, null, False);

            --  Expand the selected directory
            if not Row_Expanded (Tree.File_Tree, Path) then
               Ignore := Expand_Row (Tree.File_Tree, Path, False);
            end if;

            Path_Free (Path);

            return;
         end if;

         if Is_Parent (Curr_Dir, Dir) then
            if Row_Expanded (Tree.File_Tree, Path) then
               Iter := Children (Tree.File_Model, Iter);
            else
               Success := Expand_Row (Tree.File_Tree, Path, False);

               if Success then
                  Iter := Children (Tree.File_Model, Iter);
               else
                  Set_Cursor (Tree.File_Tree, Path, null, False);
                  Next (Tree.File_Model, Iter);
               end if;
            end if;
         else
            Next (Tree.File_Model, Iter);
         end if;

         Path_Free (Path);
      end loop;

      Tree.Current_Dir := Dir;
   end Show_Directory;

   ---------------------------------
   -- Find_Directory_In_Selection --
   ---------------------------------

   function Find_Directory_In_Selection
     (Selector : access Directory_Selector_Record'Class;
      File     : Virtual_File)
      return Gtk_Tree_Iter
   is
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_First (Selector.List_Model);

      while Iter /= Null_Iter loop
         if Get_File (Selector.List_Model, Iter, File_Column) = File then
            return Iter;
         end if;

         Next (Selector.List_Model, Iter);
      end loop;

      return Null_Iter;
   end Find_Directory_In_Selection;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Selector  : access Directory_Selector_Record'Class;
      Dir       : Virtual_File;
      Recursive : Boolean)
   is
      Row     : Gtk_Tree_Iter;

   begin
      Row := Find_Directory_In_Selection (Selector, Dir);

      if Row = Null_Iter then
         Append (Selector.List_Model, Row, Null_Iter);
         Set_Columns
           (Selector.List_Model, Row,
            Unknown_To_UTF8 (+Base_Dir_Name (Dir)), Dir);
      end if;

      if Recursive then
         declare
            Files : constant File_Array_Access := Read_Dir (Dir, Dirs_Only);
         begin
            for F in Files'Range loop
               if Is_Directory (Files (F))
                 and then not Is_Symbolic_Link (Files (F))
                 and then Filter (Selector.Directory,
                                  +Full_Name (Files (F)))
               then
                  Add_Directory (Selector, Files (F), True);
               end if;
            end loop;
         end;
      end if;

   exception
      when VFS_Directory_Error =>
         null;
   end Add_Directory;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Tree : access Dir_Tree_Record)
     return GNATCOLL.VFS.Virtual_File is
   begin
      if Tree.Current_Dir = No_File then
         return No_File;
      else
         return Tree.Current_Dir;
      end if;
   end Get_Selection;

   ------------------------
   -- Get_Tree_Selection --
   ------------------------

   function Get_Tree_Selection
     (Tree : access Dir_Tree_Record) return Gtk_Tree_Selection is
   begin
      return Gtk.Tree_View.Get_Selection (Tree.File_Tree);
   end Get_Tree_Selection;

   ----------------------
   -- Add_Directory_Cb --
   ----------------------

   procedure Add_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
      Dir      : constant Virtual_File := Get_Selection (Selector.Directory);
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path;
   begin
      if Dir /= No_File then
         Unselect_All (Get_Selection (Selector.List_Tree));
         Add_Directory (Selector, Dir, Recursive => True);

         --  Show the first selected item
         Iter := Get_First_Selected (Selector);

         if Iter /= Null_Iter then
            Path := Get_Path (Selector.List_Model, Iter);
            Scroll_To_Cell (Selector.List_Tree, Path, null, True, 0.1, 0.1);
            Path_Free (Path);
         end if;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Add_Directory_Cb;

   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory
     (Selector : access Directory_Selector_Record'Class; Recursive : Boolean)
   is
      Iter : Gtk_Tree_Iter;
   begin
      loop
         Iter := Get_First_Selected (Directory_Selector (Selector));

         exit when Iter = Null_Iter;

         if Recursive then
            declare
               Dir   : Virtual_File;
            begin
               Dir := Get_File (Selector.List_Model, Iter, File_Column);

               Iter := Get_Iter_First (Selector.List_Model);

               while Iter /= Null_Iter loop
                  declare
                     Iter_Dir    : Virtual_File;
                     Delete_Iter : Gtk_Tree_Iter;
                  begin
                     Iter_Dir := Get_File
                       (Selector.List_Model, Iter, File_Column);

                     if Is_Parent (Dir, Iter_Dir) then
                        Delete_Iter := Iter;
                        Next (Selector.List_Model, Iter);
                        Remove (Selector.List_Model, Delete_Iter);
                     else
                        Next (Selector.List_Model, Iter);
                     end if;
                  end;
               end loop;
            end;
         else
            Remove (Selector.List_Model, Iter);
         end if;
      end loop;
   end Remove_Directory;

   -------------------------
   -- Remove_Directory_Cb --
   -------------------------

   procedure Remove_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
   begin
      Remove_Directory (Selector, Recursive => True);

   exception
      when E : others => Trace (Me, E);
   end Remove_Directory_Cb;

   -----------------------------
   -- Add_Single_Directory_Cb --
   -----------------------------

   procedure Add_Single_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Selector : constant Directory_Selector := Directory_Selector (W);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
   begin
      Unselect_All (Get_Selection (Selector.List_Tree));
      Add_Directory (Selector, Get_Selection (Selector.Directory), False);
      --  Sort (Selector.List);

      --  Show the first selected item
      Iter := Get_First_Selected (Selector);

      if Iter /= Null_Iter then
         Path := Get_Path (Selector.List_Model, Iter);
         Scroll_To_Cell (Selector.List_Tree, Path, null, True, 0.1, 0.1);
         Path_Free (Path);
      end if;
   end Add_Single_Directory_Cb;

   --------------------------
   -- Tree_Contextual_Menu --
   --------------------------

   function Tree_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      Item         : Gtk_Menu_Item;
      Is_Valid     : Boolean := False;
      Menu         : Gtk_Menu;

      Path         : Gtk_Tree_Path;
      Column       : Gtk_Tree_View_Column;
      Cell_X, Cell_Y : Gint;

   begin
      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         Get_Path_At_Pos
           (Selector.Directory.File_Tree,
            Gint (Event.Button.X), Gint (Event.Button.Y),
            Path, Column, Cell_X, Cell_Y, Is_Valid);
      end if;

      if Is_Valid then
         Set_Cursor (Selector.Directory.File_Tree, Path, null, False);

         Gtk_New (Menu);
         Gtk_New (Item, "Add directory recursive");
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Add_Directory_Cb'Access, Selector);
         Append (Menu, Item);

         Gtk_New (Item, "Add directory");
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Add_Single_Directory_Cb'Access, Selector);
         Append (Menu, Item);

         Gtk_New (Item, "Create new subdirectory");
         Append (Menu, Item);
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Create_Directory_Cb'Access, Selector);

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
      Item        : Gtk_Menu_Item;
      Is_Valid    : Boolean := False;
      Menu        : Gtk_Menu;

      Path           : Gtk_Tree_Path;
      Column         : Gtk_Tree_View_Column;
      Cell_X, Cell_Y : Gint;
   begin
      if Get_Event_Type (Event) in Button_Press .. Button_Release then
         Get_Path_At_Pos
           (Selector.List_Tree,
            Gint (Event.Button.X), Gint (Event.Button.Y),
            Path, Column, Cell_X, Cell_Y, Is_Valid);
      end if;

      if Is_Valid then
         if not Path_Is_Selected
           (Get_Selection (Selector.List_Tree), Path)
         then
            Set_Cursor (Selector.List_Tree, Path, null, False);
         end if;

         Gtk_New (Menu);
         Gtk_New (Item, "Remove directory recursive");
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Remove_Directory_Cb'Access, Selector);
         Append (Menu, Item);

         Gtk_New (Item, "Remove directory");
         Widget_Callback.Object_Connect
           (Item, Gtk.Menu_Item.Signal_Activate,
            Remove_Single_Directory_Cb'Access, Selector);
         Append (Menu, Item);
      end if;

      return Menu;
   end List_Contextual_Menu;

   -------------------------
   -- Create_Directory_Cb --
   -------------------------

   procedure Create_Directory_Cb
     (W : access Gtk_Widget_Record'Class)
   is
      Selector    : constant Directory_Selector := Directory_Selector (W);
      Current_Dir : constant Virtual_File :=
                     Get_Selection (Selector.Directory);
      Dialog      : Gtk_Dialog;
      Label       : Gtk_Label;
      Ent         : Gtk_Entry;
      Ignore      : Gtk_Widget;
      pragma Unreferenced (Ignore);

   begin
      Gtk_New (Dialog,
               Title  => "Create directory",
               Parent => Gtk_Window (Get_Toplevel (W)),
               Flags  => Modal or Destroy_With_Parent);

      Gtk_New (Label, "Directory Name:");
      Pack_Start
        (Get_Content_Area (Dialog), Label, Expand => False, Fill => True);

      Gtk_New (Ent);
      Ent.Set_Max_Length (1024);
      Set_Width_Chars (Ent, 30);
      Set_Text (Ent, Display_Full_Name (Current_Dir));

      Pack_Start
        (Get_Content_Area (Dialog), Ent, Expand => True, Fill => True);

      Ignore := Add_Button (Dialog, "Create", Gtk_Response_OK);
      Ignore := Add_Button (Dialog, "Cancel", Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Iter  : Gtk_Tree_Iter;
            Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;

            Ignore : Boolean;
            pragma Unreferenced (Ignore);
         begin
            Get_Selected
              (Get_Selection (Selector.Directory.File_Tree), Model, Iter);

            Path := Get_Path (Selector.Directory.File_Model, Iter);

            Make_Dir_Recursive
              (Create_From_UTF8 (Get_Text (Ent)));

            Ignore := Collapse_Row (Selector.Directory.File_Tree, Path);
            Ignore := Expand_Row (Selector.Directory.File_Tree, Path, False);

            Path_Free (Path);

         exception
            when Directory_Error =>
               Trace (Me, "Cannot create directory " & Get_Text (Ent));
         end;
      end if;

      Destroy (Dialog);

   exception
      when E : others => Trace (Me, E);
   end Create_Directory_Cb;

   ---------------------------------
   -- Single_List_Contextual_Menu --
   ---------------------------------

   function Single_List_Contextual_Menu
     (Selector : Directory_Selector;
      Event    : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      pragma Unreferenced (Event);
      Menu : Gtk_Menu;
      Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);
      Gtk_New (Item, "Create new subdirectory");
      Append (Menu, Item);
      Widget_Callback.Object_Connect
        (Item, Gtk.Menu_Item.Signal_Activate,
         Create_Directory_Cb'Access, Selector);

      return Menu;
   end Single_List_Contextual_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Selector             : out Directory_Selector;
      Initial_Directory    : Virtual_File;
      Multiple_Directories : Boolean := False;
      Initial_Selection    : File_Array := No_Selection) is
   begin
      Selector := new Directory_Selector_Record;
      Initialize
        (Selector, Initial_Directory, Multiple_Directories, Initial_Selection);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Selector             : access Directory_Selector_Record'Class;
      Initial_Directory    : Virtual_File;
      Multiple_Directories : Boolean := False;
      Initial_Selection    : File_Array := No_Selection)
   is
      Bbox     : Gtk_Hbutton_Box;
      Button   : Gtk_Button;
      Scrolled : Gtk_Scrolled_Window;
      Arrow    : Gtk_Arrow;
      Vbox     : Gtk_Box;
      Iter     : Gtk_Tree_Iter;

   begin
      Initialize_Vpaned (Selector);

      Gtk_New (Selector.Directory, Initial_Directory);
      Pack1 (Selector, Selector.Directory, Resize => True);

      if Multiple_Directories then
         Widget_Menus.Register_Contextual_Menu
           (Selector.Directory.File_Tree,
            Directory_Selector (Selector),
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
           (Button, Gtk.Button.Signal_Clicked,
            Add_Single_Directory_Cb'Access, Selector);

         Gtk_New (Button);
         Gtk_New (Arrow,
                  Arrow_Up, Shadow_In);
         Add (Button, Arrow);
         Pack_Start (Bbox, Button, Expand => False, Fill => False);
         Widget_Callback.Object_Connect
           (Button, Gtk.Button.Signal_Clicked,
            Remove_Single_Directory_Cb'Access, Selector);

         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Vbox, Scrolled, Expand => True, Fill => True);

         Gtk_New (Selector.List_Model, Columns_Types);
         Gtk_New (Selector.List_Tree, Selector.List_Model);
         Unref (Selector.List_Model);
         Set_Headers_Visible (Selector.List_Tree, False);
         Set_Column_Types (Selector.List_Tree);
         Add (Scrolled, Selector.List_Tree);

         Set_Mode (Get_Selection (Selector.List_Tree), Selection_Multiple);
         Widget_Menus.Register_Contextual_Menu
           (Selector.List_Tree, Directory_Selector (Selector),
            List_Contextual_Menu'Access);

         for J in Initial_Selection'Range loop
            Append (Selector.List_Model, Iter, Null_Iter);

            Ensure_Directory (Initial_Selection (J));
            Set_Columns
              (Selector.List_Model, Iter,
               Initial_Selection (J).Display_Base_Dir_Name,
               Initial_Selection (J));
         end loop;

      else
         Widget_Menus.Register_Contextual_Menu
           (Selector.Directory, Directory_Selector (Selector),
            Single_List_Contextual_Menu'Access);
      end if;
   end Initialize;

   ------------------------
   -- Get_First_Selected --
   ------------------------

   function Get_First_Selected
     (Selector : Directory_Selector) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_First (Selector.List_Model);

      while Iter /= Null_Iter loop
         if Iter_Is_Selected (Get_Selection (Selector.List_Tree), Iter) then
            return Iter;
         end if;

         Next (Selector.List_Model, Iter);
      end loop;

      return Null_Iter;
   end Get_First_Selected;

   --------------------------
   -- Get_Single_Selection --
   --------------------------

   function Get_Single_Selection
     (Selector  : access Directory_Selector_Record'Class) return Virtual_File
   is
      Iter  : Gtk_Tree_Iter;
   begin
      Iter := Get_First_Selected (Directory_Selector (Selector));

      if Iter /= Null_Iter then
         return Get_File (Selector.List_Model, Iter, File_Column);
      end if;

      return No_File;
   end Get_Single_Selection;

   ----------------------------
   -- Get_Multiple_Selection --
   ----------------------------

   function Get_Multiple_Selection
     (Selector : access Directory_Selector_Record'Class)
      return GNATCOLL.VFS.File_Array
   is
      Iter   : Gtk_Tree_Iter;
      Length : Integer := 0;
   begin
      Iter := Get_Iter_First (Selector.List_Model);

      while Iter /= Null_Iter loop
         Length := Length + 1;
         Next (Selector.List_Model, Iter);
      end loop;

      declare
         Args    : File_Array (1 .. Natural (Length));
         Current : Integer := 1;
      begin
         Iter := Get_Iter_First (Selector.List_Model);

         while Iter /= Null_Iter loop
            Args (Current) :=
              Get_File (Selector.List_Model, Iter, File_Column);
            Current := Current + 1;
            Next (Selector.List_Model, Iter);
         end loop;

         return Args;
      end;
   end Get_Multiple_Selection;

   -----------------------
   -- Append_Dummy_Iter --
   -----------------------

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter)
   is
      Iter : Gtk_Tree_Iter;
   begin
      Append (Model, Iter, Base);
   end Append_Dummy_Iter;

   --------------------
   -- Read_Directory --
   --------------------

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean
   is
      Path_Found : Boolean := False;
      Iter       : Gtk_Tree_Iter;

   begin
      --  If we are appending at the base, create a node indicating the
      --  absolute path to the directory.

      if D.Base = Null_Iter then
         Append (D.Explorer.File_Model, Iter, D.Base);

         if D.Physical_Read then
            Set_Columns
              (D.Explorer.File_Model, Iter,
               Icon => Open_Directory_Node,
               Base => Display_Base_Dir_Name (D.Norm_Dir),
               File => D.Norm_Dir);

            D.Base := Iter;
            return Read_Directory (D);

         else
            Append_Dummy_Iter (D.Explorer.File_Model, Iter);
            Set_Columns
              (D.Explorer.File_Model, Iter,
               Icon => Closed_Directory_Node,
               Base => Display_Base_Dir_Name (D.Norm_Dir),
               File => D.Norm_Dir);
            return False;
         end if;

      else
         declare
            Path    : constant Gtk_Tree_Path :=
              Get_Path (D.Explorer.File_Model, D.Base);
            Success : Boolean;
            Expanding : constant Boolean := D.Explorer.Expanding;
            pragma Unreferenced (Success);
         begin
            D.Explorer.Expanding := True;
            Success := Expand_Row
              (D.Explorer.File_Tree,
               Path, False);
            D.Explorer.Expanding := Expanding;
            Path_Free (Path);
         end;
      end if;

      declare
         Files : File_Array_Access := Read_Dir (D.Norm_Dir, Dirs_Only);
      begin
         Sort (Files.all);
         D.Depth := D.Depth + 1;
         for F in Files'Range loop
            if D.Depth >= 0 and then Files (F) /= No_File then
               --  if directory but not ./ and not ../
               if Is_Directory (Files (F))
                 and then Files (F) /= D.Norm_Dir
                 and then not Is_Parent (Files (F), D.Norm_Dir)
               then
                  Append (D.Dirs, Files (F));
               end if;
            end if;
         end loop;

         Unchecked_Free (Files);
      end;

      if Is_Empty (D.Dirs) then
         D.Explorer.File_Model.Set
           (D.Base, Icon_Column, Closed_Directory_Node);
      end if;

      while not Is_Empty (D.Dirs) loop
         declare
            Dir   : constant Virtual_File := D.Dirs.First_Element;
         begin
            Append (D.Explorer.File_Model, Iter, D.Base);

            Set_Columns
              (D.Explorer.File_Model, Iter,
               Base => Display_Base_Dir_Name (Dir),
               File => Dir);

            if D.Depth = 0 then
               exit;
            end if;

            --  Are we on the path to the target directory ?

            if not Path_Found
              and then Is_Parent (Dir, D.Norm_Dest)
            then
               Path_Found := True;

               declare
                  Success   : Boolean;
                  pragma Unreferenced (Success);

                  Path      : Gtk_Tree_Path;
                  Expanding : constant Boolean := D.Explorer.Expanding;
               begin
                  Path := Get_Path (D.Explorer.File_Model, D.Base);

                  D.Explorer.Expanding := True;
                  Success := Expand_Row (D.Explorer.File_Tree, Path, False);
                  D.Explorer.Expanding := Expanding;

                  D.Explorer.File_Model.Set
                    (D.Base, Icon_Column, Open_Directory_Node);

                  Path_Free (Path);
               end;

               --  Are we on the target directory ?

               if D.Norm_Dest = Dir then
                  declare
                     Success   : Boolean;
                     pragma Unreferenced (Success);

                     Expanding : constant Boolean := D.Explorer.Expanding;
                  begin
                     if D.Explorer.Path /= Null_Gtk_Tree_Path then
                        Path_Free (D.Explorer.Path);
                     end if;

                     D.Explorer.Path := Get_Path (D.Explorer.File_Model, Iter);

                     File_Append_Directory
                       (D.Explorer, Dir, Iter, D.Depth, D.Norm_Dest, False);

                     D.Explorer.Expanding := True;
                     Success := Expand_Row
                       (D.Explorer.File_Tree,
                        D.Explorer.Path, False);
                     D.Explorer.Expanding := Expanding;

                     D.Explorer.File_Model.Set
                       (Iter, Icon_Column, Open_Directory_Node);

                     D.Explorer.Scroll_To_Directory := True;

                     D.Explorer.Realize_Cb_Id :=
                       Gtkada.Handlers.Return_Callback.Object_Connect
                         (D.Explorer.File_Tree, Signal_Draw,
                          Gtkada.Handlers.Return_Callback.To_Marshaller
                            (Expose_Event_Cb'Access), D.Explorer, True);
                  end;

               else
                  File_Append_Directory
                    (D.Explorer, Dir, Iter, D.Depth, D.Norm_Dest, D.Idle);
               end if;

            else
               Append_Dummy_Iter (D.Explorer.File_Model, Iter);

               D.Explorer.File_Model.Set
                 (Iter, Icon_Column, Closed_Directory_Node);
            end if;

            --  Frees first element in the list
            D.Dirs.Delete_First;
         end;
      end loop;

      return False;

   exception
      when VFS_Directory_Error =>
         --  The directory couldn't be open, probably because of permissions
         return False;

      when E : others =>
         Trace (Me, E);
         return False;
   end Read_Directory;

   ---------------------------
   -- File_Append_Directory --
   ---------------------------

   procedure File_Append_Directory
     (Explorer      : access Dir_Tree_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : Virtual_File := No_File;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True)
   is
      D : Append_Directory_Idle_Data_Access := new Append_Directory_Idle_Data;
      --  D is freed when Read_Directory ends (i.e. returns False)

      Timeout_Id : Glib.Main.G_Source_Id;

   begin
      Ensure_Directory (Dir);
      Ensure_Directory (Append_To_Dir);
      D.Norm_Dir      := Dir;
      D.Norm_Dest     := Append_To_Dir;
      D.Depth         := Depth;
      D.Base          := Base;
      D.Explorer      := Dir_Tree (Explorer);
      D.Idle          := Idle;
      D.Physical_Read := Physical_Read;

      if Idle then
         --  Do not append the first item in an idle loop.
         --  Necessary for preserving order in drive names.

         if Read_Directory (D) then
            Timeout_Id := File_Append_Directory_Timeout.Timeout_Add
              (1, Read_Directory'Access, D, Notify => Free'Access);
            Timeout_Id_List.Append (Explorer.Fill_Timeout_Ids, Timeout_Id);
         else
            Free (D);
         end if;
      else
         loop
            exit when not Read_Directory (D);
         end loop;
         Free (D);
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
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "icon-name", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Model : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      Icon  : String;
      Base  : String;
      File  : Virtual_File) is
   begin
      Set_And_Clear
        (Model, Iter,
         (Icon_Column, Base_Name_Column, File_Column),
         (As_String (Icon),
          As_String (Base),
          As_File   (File)));
   end Set_Columns;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Model : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
      Base  : String;
      File  : Virtual_File) is
   begin
      Set_And_Clear
        (Model, Iter,
         (Base_Name_Column, File_Column),
         (As_String (Base),
          As_File   (File)));
   end Set_Columns;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column      => GType_String,
         File_Column      => GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type,
         Base_Name_Column => GType_String);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree    : out Dir_Tree;
      Initial : Virtual_File := No_File) is
   begin
      Tree := new Dir_Tree_Record;
      Directory_Tree.Initialize (Tree, Initial);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tree    : access Dir_Tree_Record'Class;
      Initial : Virtual_File)
   is
   begin
      Gtk.Scrolled_Window.Initialize (Tree);
      Set_Policy (Tree, Policy_Automatic, Policy_Automatic);

      Gtk_New (Tree.File_Model, Columns_Types);
      Gtk_New (Tree.File_Tree, Tree.File_Model);

      --  The model should be destroyed as soon as the tree view is destroyed
      Unref (Tree.File_Model);

      Add (Tree, Tree.File_Tree);

      Set_Headers_Visible (Tree.File_Tree, False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Tree.File_Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (File_Button_Press'Access),
         Slot_Object => Tree,
         After       => False);

      Set_Column_Types (Tree.File_Tree);

      Widget_Callback.Object_Connect
        (Tree.File_Tree, Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (File_Tree_Expand_Row_Cb'Access),
         Tree,
         False);

      Widget_Callback.Object_Connect
        (Tree.File_Tree,
         Signal_Row_Collapsed,
         Widget_Callback.To_Marshaller (File_Tree_Collapse_Row_Cb'Access),
         Tree,
         False);

      Widget_Callback.Object_Connect
        (Tree.File_Tree, Signal_Destroy,
         On_File_Destroy'Access, Tree, False);

      declare
         Initial_Dir : Virtual_File;
      begin
         if Initial = No_File then
            Initial_Dir := Get_Current_Dir;
         else
            Initial_Dir := Initial;
         end if;

         --  ??? Root_Dir is not taken into account yet
         Refresh (Tree, Initial_Dir);

         Tree.Current_Dir := Initial_Dir;
      end;

      Object_Callback.Object_Connect
        (Get_Selection (Tree.File_Tree),
         Signal_Changed,
         On_Tree_Select_Row'Access,
         Slot_Object => Tree,
         After => True);
   end Initialize;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Dir_Tree_Record'Class) is
   begin
      while not Explorer.Fill_Timeout_Ids.Is_Empty loop
         Glib.Main.Remove (Explorer.Fill_Timeout_Ids.First_Element);
         Explorer.Fill_Timeout_Ids.Delete_First;
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
      E : constant Dir_Tree := Dir_Tree (Explorer);
   begin
      if E.Path /= Null_Gtk_Tree_Path then
         Path_Free (E.Path);
      end if;

      File_Remove_Idle_Calls (E);

      Clear (E.File_Model);
   end On_File_Destroy;

   -------------------------------
   -- File_Tree_Collapse_Row_Cb --
   -------------------------------

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);

      T         : constant Dir_Tree := Dir_Tree (Explorer);
      Iter_File : Virtual_File;

   begin
      Iter_File := Get_File (T.File_Model, Iter, File_Column);

      if Is_Directory (Iter_File) then
         T.File_Model.Set (Iter, Icon_Column, Closed_Directory_Node);
      end if;

   exception
      when E : others => Trace (Me, E);
   end File_Tree_Collapse_Row_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Explorer : access Gtk_Widget_Record'Class;
      Cr       : Cairo_Context) return Boolean
   is
      pragma Unreferenced (Cr);
      T : constant Dir_Tree := Dir_Tree (Explorer);

   begin
      Disconnect (T.File_Tree, T.Realize_Cb_Id);

      if T.Scroll_To_Directory then
         T.Scroll_To_Directory := False;
         Scroll_To_Cell (T.File_Tree, T.Path, null, True, 0.1, 0.1);

         Select_Path (Get_Selection (T.File_Tree), T.Path);
         Set_Cursor (T.File_Tree, T.Path, null, False);

         --  Under Windows, scrolling while processing the expose event does
         --  not invalidate the event, therefore we need to force a redraw on
         --  the widget, in order to avoid having a garbled tree view.
         Queue_Draw (T.File_Tree);
      end if;

      --  When the tree is resized, try to scroll the currently selected
      --  item onscreen.

      Object_Callback.Object_Connect
        (T.File_Tree,
         Signal_Size_Allocate,
         On_Tree_Size_Allocate'Access,
         Slot_Object => T,
         After => True);

      return True;

   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Expose_Event_Cb;

   -----------------------------
   -- File_Tree_Expand_Row_Cb --
   -----------------------------

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      T      : constant Dir_Tree := Dir_Tree (Explorer);
      Ignore : Boolean;
      pragma Unreferenced (Ignore);

   begin
      if T.Expanding then
         return;
      end if;

      T.Expanding := True;

      declare
         Iter_File : Virtual_File;
      begin
         Iter_File := Get_File (T.File_Model, Iter, File_Column);

         Free_Children (T, Iter);
         T.File_Model.Set (Iter, Icon_Column, Open_Directory_Node);
         File_Append_Directory (T, Iter_File, Iter, 1);
      end;

      Ignore := Expand_Row (T.File_Tree, Path, False);
      Set_Cursor (T.File_Tree, Path, null, False);

      Scroll_To_Cell
        (T.File_Tree,
         Path, null, True,
         0.1, 0.1);

      T.Expanding := False;

   exception
      when E : others => Trace (Me, E);
   end File_Tree_Expand_Row_Cb;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Tree      : access Gtk_Tree_View_Record'Class;
      Model     : Gtk_Tree_Store;
      Event     : Gdk_Event;
      Add_Dummy : Boolean) return Boolean
   is
      Iter         : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (Tree, Event);

      if Iter /= Null_Iter then
         if Get_Event_Type (Event) = Gdk_2button_Press then
            declare
               Path   : Gtk_Tree_Path;
               Ignore : Boolean;
               pragma Unreferenced (Ignore);
            begin
               Path := Get_Path (Model, Iter);

               if Row_Expanded (Tree, Path) then
                  Ignore := Collapse_Row (Tree, Path);
               else
                  if Add_Dummy then
                     Append_Dummy_Iter (Model, Iter);
                  end if;

                  Ignore := Expand_Row (Tree, Path, False);
               end if;

               Path_Free (Path);
            end;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Button_Press;

   -----------------------
   -- File_Button_Press --
   -----------------------

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T    : constant Dir_Tree :=
        Dir_Tree (Explorer);
   begin
      return On_Button_Press
        (T.File_Tree, T.File_Model, Event, True);

   exception
      when E : others => Trace (Me, E);
         return False;
   end File_Button_Press;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Explorer : access Dir_Tree_Record'Class;
      Dir      : Virtual_File)
   is
      Dir_Inserted : Boolean := False;
      Drives       : File_Array_Access :=
                       Get_Logical_Drives (Dir.Get_Host);

   begin
      File_Remove_Idle_Calls (Explorer);

      Clear (Explorer.File_Model);

      if Drives'Length = 0 then
         File_Append_Directory
           (Explorer, Get_Root (Dir), Null_Iter, 1, Dir, True);
         Dir_Inserted := True;

      else
         for J in Drives'Range loop
            if Is_Parent (Drives (J), Dir) then
               File_Append_Directory
                 (Explorer, Drives (J), Null_Iter, 1, Dir, True);
               Dir_Inserted := True;

            else
               File_Append_Directory
                 (Explorer, Drives (J), Null_Iter, 0, No_File, False, False);
            end if;
         end loop;
      end if;

      Unchecked_Free (Drives);

      if not Dir_Inserted then
         File_Append_Directory
           (Explorer, Get_Root (Dir),
            Null_Iter, 0, No_File, False, False);
      end if;

   exception
      when E : others => Trace (Me, E);
   end Refresh;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Dir_Tree;
      Iter : Gtk_Tree_Iter) is
   begin
      Remove_Child_Nodes (T.File_Model, Iter);
   end Free_Children;

end Directory_Tree;

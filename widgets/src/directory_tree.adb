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
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Gdk;                       use Gdk;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Arrow;                 use Gtk.Arrow;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Clist;                 use Gtk.Clist;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gdk.Event;                 use Gdk.Event;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Hbutton_Box;           use Gtk.Hbutton_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main;                  use Gtk.Main;
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
with Gtkada.Types;              use Gtkada.Types;
with Pixmaps_IDE;               use Pixmaps_IDE;

with Glide_Intl;                use Glide_Intl;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with String_List_Utils;         use String_List_Utils;
with File_Utils;                use File_Utils;
with Basic_Types;               use Basic_Types;
with Traces;                    use Traces;

with Unchecked_Deallocation;
with Generic_List;

package body Directory_Tree is

   Icon_Column          : constant := 0;
   Base_Name_Column     : constant := 1;
   Absolute_Name_Column : constant := 2;
   Node_Type_Column     : constant := 3;
   User_Data_Column     : constant := 4;
   Line_Column          : constant := 5;
   Column_Column        : constant := 6;
   Project_Column       : constant := 7;
   Category_Column      : constant := 8;
   Up_To_Date_Column    : constant := 9;
   Entity_Base_Column   : constant := 10;

   --------------
   -- Graphics --
   --------------

   type Node_Types is
     (Directory_Node);
   --  The kind of nodes one might find in the tree

   type Pixbuf_Array is array (Node_Types) of Gdk.Pixbuf.Gdk_Pixbuf;

   Open_Pixbufs  : Pixbuf_Array;
   Close_Pixbufs : Pixbuf_Array;

   procedure Init_Graphics;
   --  Initialize the pixbufs.

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics is
   begin
      --  If initialization has already been done, exit.
      if Open_Pixbufs (Directory_Node) /= null then
         return;
      end if;

      Open_Pixbufs (Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_ofolder_xpm);
      Close_Pixbufs (Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_xpm);

   end Init_Graphics;

   Me : constant Debug_Handle := Create ("Directory_Tree");

   package Widget_Menus is new GUI_Utils.User_Contextual_Menus
     (User_Data => Directory_Selector);
   --  Used to register contextual menus with a user data.

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   type Append_Directory_Idle_Data is record
      Explorer      : Dir_Tree;
      Norm_Dest     : Basic_Types.String_Access;
      Norm_Dir      : Basic_Types.String_Access;
      D             : GNAT.Directory_Operations.Dir_Type;
      Depth         : Integer := 0;
      Base          : Gtk_Tree_Iter;
      Dirs          : String_List_Utils.String_List.List;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True;
   end record;

   procedure Free is
     new Unchecked_Deallocation (Append_Directory_Idle_Data,
                                 Append_Directory_Idle_Data_Access);

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure File_Append_Directory
     (Explorer      : access Dir_Tree_Record'Class;
      Dir           : String;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : String  := "";
      Idle          : Boolean := False;
      Physical_Read : Boolean := True);
   --  Add to the file view the directory Dir, at node given by Iter.
   --  If Append_To_Dir is not "", and is a sub-directory of Dir, then
   --  the path is expanded recursively all the way to Append_To_Dir.

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues);
   --  Called every time a node is expanded in the file view.
   --  It is responsible for automatically adding the children of the current
   --  node if they are not there already.

   function Expose_Event_Cb
     (Explorer : access Glib.Object.GObject_Record'Class;
      Values   : GValues) return Boolean;
   --  Scroll the explorer to the current directory.

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues);
   --  Called every time a node is collapsed in the file view.

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view.

   procedure File_Remove_Idle_Calls
     (Explorer : access Dir_Tree_Record'Class);
   --  Remove the idle calls for filling the file view.

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
   --  Callback for the "button_press" event on the file view.

   procedure Free_Children
     (T    : Dir_Tree;
      Iter : Gtk_Tree_Iter);
   --  Free all the children of iter Iter in the file view.

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  Atomic function to read a directory using the information in D.
   --  Called by File_Append_Directory.

   procedure Refresh
     (Explorer : access Dir_Tree_Record'Class);
   --  Refresh the contents of the explorer.
   pragma Unreferenced (Refresh);

   procedure Append_Dummy_Iter
     (Model : Gtk_Tree_Store;
      Base  : Gtk_Tree_Iter);
   --  Append an empty iter to Base.

   procedure Create_Directory_Cb
     (W : access Gtk_Widget_Record'Class);
   --  Create a new subdirectory of the selected directory.

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

   function Filter
     (Tree     : access Dir_Tree_Record'Class;
      Dir_Name : String) return Boolean;
   --  Should return True if Dir_Name should be displayed in the tree.
   --  Dir_Name is the last part of the full path, ie shouldn't include its
   --  parent's name.

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

      Select_Iter (Get_Selection (Tree.File_Tree), Iter);

      Path := Get_Path (Tree.File_Model, Iter);
      Scroll_To_Cell (Tree.File_Tree, Path, null, True, 0.1, 0.1);
      Path_Free (Path);
   end Show_Parent;

   --------------------
   -- Show_Directory --
   --------------------

   procedure Show_Directory
     (Tree           : access Dir_Tree_Record;
      Dir            : String;
      Busy_Cursor_On : Gdk.Window.Gdk_Window := null)
   is
      D        : constant String := Name_As_Directory (Dir);
      Parent   : Gtk_Tree_Iter;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path;
      pragma Unreferenced (Busy_Cursor_On);
   begin
      --  Find the first non-expanded iter

      Parent := Get_Iter_First (Tree.File_Model);

      Iter := Parent;

      while Iter /= Null_Iter loop
         Path := Get_Path (Tree.File_Model, Iter);

         declare
            Curr_Dir : constant String := Get_String
              (Tree.File_Model, Iter, Absolute_Name_Column);
         begin
            if Curr_Dir = D then
               Select_Iter (Get_Selection (Tree.File_Tree), Iter);
               Scroll_To_Cell (Tree.File_Tree, Path, null, True, 0.1, 0.1);
               Path_Free (Path);

               return;
            end if;

            if Curr_Dir'Length < D'Length
              and then D (D'First .. D'First + Curr_Dir'Length - 1) = Curr_Dir
            then
               if Row_Expanded (Tree.File_Tree, Path) then
                  Iter := Children (Tree.File_Model, Iter);
               else
                  File_Append_Directory
                    (Tree, Curr_Dir, Iter, 1, D, True, True);
                  Path_Free (Path);
                  return;
               end if;
            else
               Next (Tree.File_Model, Iter);
            end if;
         end;

         Path_Free (Path);
      end loop;
   end Show_Directory;

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

            declare
               Dir_Name : constant String := Dir & File (1 .. Last);
            begin
               if Is_Directory (Dir_Name)
                 and then not Is_Symbolic_Link (Dir_Name)
                 and then Filter (Selector.Directory, File (1 .. Last))
               then
                  Add_Directory (Selector, Name_As_Directory (Dir_Name), True);
               end if;
            end;
         end loop;

         Close (Handle);
      end if;

   exception
      when Directory_Error =>
         null;
   end Add_Directory;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Tree : access Dir_Tree_Record) return String
   is
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Tree.File_Tree), Model, Iter);

      if Iter = Null_Iter then
         return "";
      else
         return Get_String
           (Tree.File_Model, Iter, Absolute_Name_Column);
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
      Dir      : constant String  := Get_Selection (Selector.Directory);
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
                 Gint_List.Get_Data (Get_Selection (Selector.List)),
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
      Unselect_All (Selector.List);
      Add_Directory (Selector, Get_Selection (Selector.Directory), False);
      Sort (Selector.List);

      --  Show the first selected item
      Moveto (Selector.List,
              Gint_List.Get_Data (Get_Selection (Selector.List)),
              0, 0.2, 0.0);
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
            Gint (Get_X (Event)), Gint (Get_Y (Event)),
            Path, Column, Cell_X, Cell_Y, Is_Valid);
      end if;

      if Is_Valid then
         Select_Path (Get_Selection (Selector.Directory.File_Tree), Path);

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
      pragma Unreferenced (Event);
      use type Gint_List.Glist;

      Item        : Gtk_Menu_Item;
      Is_Valid    : constant Boolean :=
        Get_Selection (Selector.List) /= Gint_List.Null_List;
      Menu        : Gtk_Menu;
   begin
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
      Selector    : constant Directory_Selector := Directory_Selector (W);
      Current_Dir : constant String := Get_Selection (Selector.Directory);
      Dialog      : Gtk_Dialog;
      Label       : Gtk_Label;
      Ent         : Gtk_Entry;
      Widget      : Gtk_Widget;
      pragma Unreferenced (Widget);

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
            Iter  : Gtk_Tree_Iter;
            Model : Gtk_Tree_Model;
            Path  : Gtk_Tree_Path;

            Success : Boolean;
            pragma Unreferenced (Success);
         begin
            Get_Selected
              (Get_Selection (Selector.Directory.File_Tree), Model, Iter);

            Path := Get_Path (Selector.Directory.File_Model, Iter);

            --  ??? Should we create the intermediate directories.
            Make_Dir (Get_Text (Ent));

            Success := Collapse_Row (Selector.Directory.File_Tree, Path);
            Success := Expand_Row (Selector.Directory.File_Tree, Path, False);

            Path_Free (Path);

         exception
            when Directory_Error =>
               Trace (Me, "Cannot create directory " & Get_Text (Ent));
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
      pragma Unreferenced (Event);
      Menu : Gtk_Menu;
      Item : Gtk_Menu_Item;
   begin
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
      pragma Unreferenced (Busy_Cursor_On);
      Scrolled : Gtk_Scrolled_Window;
      Bbox     : Gtk_Hbutton_Box;
      Button   : Gtk_Button;
      Arrow    : Gtk_Arrow;
      Vbox     : Gtk_Box;

   begin
      Initialize_Vpaned (Selector);

      Gtk_New (Selector.Directory, Root_Directory, Initial_Directory);
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
         Set_Size_Request (Selector.List, -1, 150);
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
         return (1 => new String'(Get_Selection (Selector.Directory)));
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

         Set (D.Explorer.File_Model, Iter, Absolute_Name_Column,
              Locale_To_UTF8 (D.Norm_Dir.all));
         Set (D.Explorer.File_Model, Iter, Base_Name_Column,
              Locale_To_UTF8 (D.Norm_Dir.all));
         Set (D.Explorer.File_Model, Iter, Node_Type_Column,
              Gint (Node_Types'Pos (Directory_Node)));

         if D.Physical_Read then
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 Glib.C_Proxy (Open_Pixbufs (Directory_Node)));
            D.Base := Iter;

            return Read_Directory (D);

         else
            Append_Dummy_Iter (D.Explorer.File_Model, Iter);
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 Glib.C_Proxy (Close_Pixbufs (Directory_Node)));
            New_D := D;
            Free (New_D);

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

      Read (D.D, File, Last);

      if D.Depth >= 0 and then Last /= 0 then
         if not (Last = 1 and then File (1) = '.')
           and then not (Last = 2 and then File (1 .. 2) = "..")
         then
            if Is_Directory (D.Norm_Dir.all & File (File'First .. Last)) then
               Append (D.Dirs, File (File'First .. Last));
            end if;

            if D.Depth = 0 then
               D.Depth := -1;
            end if;
         end if;

         return True;
      end if;

      Close (D.D);

      if Filenames_Are_Case_Sensitive then
         Sort (D.Dirs);
      else
         Sort_Case_Insensitive (D.Dirs);
      end if;

      if Is_Empty (D.Dirs) then
         Set (D.Explorer.File_Model, D.Base, Icon_Column,
              Glib.C_Proxy (Close_Pixbufs (Directory_Node)));
      end if;

      while not Is_Empty (D.Dirs) loop
         declare
            Dir : constant String := Head (D.Dirs);
         begin
            Append (D.Explorer.File_Model, Iter, D.Base);
            Set (D.Explorer.File_Model, Iter, Absolute_Name_Column,
                 Locale_To_UTF8 (D.Norm_Dir.all & Dir & Directory_Separator));
            Set (D.Explorer.File_Model, Iter, Base_Name_Column,
                 Locale_To_UTF8 (Dir));
            Set (D.Explorer.File_Model, Iter, Node_Type_Column,
                 Gint (Node_Types'Pos (Directory_Node)));

            if D.Depth = 0 then
               exit;
            end if;

            --  Are we on the path to the target directory ?

            if not Path_Found
              and then D.Norm_Dir'Length + Dir'Length <= D.Norm_Dest'Length
                and then
                  ((Filenames_Are_Case_Sensitive
                    and then (D.Norm_Dest
                               (D.Norm_Dest'First
                                .. D.Norm_Dest'First
                                  + D.Norm_Dir'Length + Dir'Length - 1)
                                   = D.Norm_Dir.all & Dir))
                   or else
                     (not Filenames_Are_Case_Sensitive
                      and then Case_Insensitive_Equal
                        (D.Norm_Dest.all
                           (D.Norm_Dest.all'First
                              .. D.Norm_Dest.all'First
                                + D.Norm_Dir.all'Length
                                  + Dir'Length - 1),
                         D.Norm_Dir.all & Dir)))
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

                  Set (D.Explorer.File_Model, D.Base, Icon_Column,
                       Glib.C_Proxy (Open_Pixbufs (Directory_Node)));

                  Path_Free (Path);
               end;

               --  Are we on the target directory ?

               if D.Norm_Dest.all = D.Norm_Dir.all & Dir
                  & Directory_Separator
               then
                  declare
                     Success   : Boolean;
                     pragma Unreferenced (Success);

                     Expanding : constant Boolean := D.Explorer.Expanding;
                  begin
                     D.Explorer.Path := Get_Path (D.Explorer.File_Model, Iter);

                     File_Append_Directory
                       (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                        Iter, D.Depth, D.Norm_Dest.all,
                        False);

                     D.Explorer.Expanding := True;
                     Success := Expand_Row
                       (D.Explorer.File_Tree,
                        D.Explorer.Path, False);
                     D.Explorer.Expanding := Expanding;

                     Set (D.Explorer.File_Model, Iter, Icon_Column,
                          Glib.C_Proxy (Open_Pixbufs (Directory_Node)));
                     D.Explorer.Scroll_To_Directory := True;
                     D.Explorer.Realize_Cb_Id :=
                       Gtkada.Handlers.Object_Return_Callback.Object_Connect
                         (D.Explorer.File_Tree, "expose_event",
                          Expose_Event_Cb'Access, D.Explorer, True);
                  end;

               else
                  File_Append_Directory
                    (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                     Iter, D.Depth, D.Norm_Dest.all, D.Idle);
               end if;

            else
               Append_Dummy_Iter (D.Explorer.File_Model, Iter);

               Set (D.Explorer.File_Model, Iter, Icon_Column,
                    Glib.C_Proxy (Close_Pixbufs (Directory_Node)));
            end if;

            Next (D.Dirs);
         end;
      end loop;

      Free (D.Norm_Dir);
      Free (D.Norm_Dest);

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
     (Explorer      : access Dir_Tree_Record'Class;
      Dir           : String;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : String  := "";
      Idle          : Boolean := False;
      Physical_Read : Boolean := True)
   is
      D : Append_Directory_Idle_Data_Access := new Append_Directory_Idle_Data;
      --  D is freed when Read_Directory ends (i.e. returns False)

      Timeout_Id : Timeout_Handler_Id;

   begin
      if Physical_Read then
         begin
            Open (D.D, Dir);
         exception
            when Directory_Error =>
               Free (D);
               return;
         end;

         D.Norm_Dir := new String'(Normalize_Pathname (Dir));

      else
         D.Norm_Dir := new String'(Normalize_Pathname (Dir));
      end if;

      D.Norm_Dest     := new String'(Normalize_Pathname (Append_To_Dir));
      D.Depth         := Depth;
      D.Base          := Base;
      D.Explorer      := Dir_Tree (Explorer);
      D.Idle          := Idle;
      D.Physical_Read := Physical_Read;

      if Idle then
         --  Do not append the first item in an idle loop.
         --  Necessary for preserving order in drive names.

         if Read_Directory (D) then
            Timeout_Id :=
              File_Append_Directory_Timeout.Add (1, Read_Directory'Access, D);
            Timeout_Id_List.Append (Explorer.Fill_Timeout_Ids, Timeout_Id);
         end if;
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

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column          => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column => GType_String,
         Base_Name_Column     => GType_String,
         Node_Type_Column     => GType_Int,
         User_Data_Column     => GType_Pointer,
         Line_Column          => GType_Int,
         Column_Column        => GType_Int,
         Project_Column       => GType_Int,
         Category_Column      => GType_Int,
         Up_To_Date_Column    => GType_Boolean,
         Entity_Base_Column   => GType_String);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree    : out Dir_Tree;
      Root    : String;
      Initial : String := "") is
   begin
      Tree := new Dir_Tree_Record;
      Directory_Tree.Initialize (Tree, Root, Initial);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tree    : access Dir_Tree_Record'Class;
      Root    : String;
      Initial : String)
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
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (File_Button_Press'Access),
         Slot_Object => Tree,
         After       => False);

      Set_Column_Types (Tree.File_Tree);

      Init_Graphics;

      Widget_Callback.Object_Connect
        (Tree.File_Tree, "row_expanded",
         File_Tree_Expand_Row_Cb'Access, Tree, False);

      Widget_Callback.Object_Connect
        (Tree.File_Tree, "row_collapsed",
         File_Tree_Collapse_Row_Cb'Access, Tree, False);

      Widget_Callback.Object_Connect
        (Tree.File_Tree, "destroy",
         On_File_Destroy'Access, Tree, False);

      Set_Size_Request
        (Tree,
         400,
         400);

      declare
         Root_Dir    : GNAT.OS_Lib.String_Access;
         Initial_Dir : GNAT.OS_Lib.String_Access;
      begin
         if Root = "" then
            Root_Dir := new String'("" & Directory_Separator);
         else
            Root_Dir := new String'(Root);
         end if;

         if Initial = "" then
            Initial_Dir := new String'(Get_Current_Dir);
         else
            Initial_Dir := new String'(Initial);
         end if;

         File_Append_Directory
           (Tree,
            Root_Dir.all,
            Null_Iter,
            1,
            Initial_Dir.all);

         Free (Root_Dir);
         Free (Initial_Dir);
      end;
   end Initialize;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Dir_Tree_Record'Class) is
   begin
      while not Timeout_Id_List.Is_Empty (Explorer.Fill_Timeout_Ids) loop
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
      E : constant Dir_Tree :=
        Dir_Tree (Explorer);
   begin
      File_Remove_Idle_Calls (E);
   end On_File_Destroy;

   -------------------------------
   -- File_Tree_Collapse_Row_Cb --
   -------------------------------

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T    : constant Dir_Tree :=
        Dir_Tree (Explorer);
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
                    Glib.C_Proxy (Close_Pixbufs (Directory_Node)));
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end File_Tree_Collapse_Row_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Explorer : access Glib.Object.GObject_Record'Class;
      Values   : GValues) return Boolean
   is
      pragma Unreferenced (Values);
      T       : Dir_Tree := Dir_Tree (Explorer);

   begin
      if T.Scroll_To_Directory then
         Scroll_To_Cell
           (T.File_Tree,
            T.Path, null, True,
            0.1, 0.1);
         Disconnect (T.File_Tree, T.Realize_Cb_Id);
         T.Scroll_To_Directory := False;
      end if;

      return True;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
         return True;
   end Expose_Event_Cb;

   -----------------------------
   -- File_Tree_Expand_Row_Cb --
   -----------------------------

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T       : Dir_Tree := Dir_Tree (Explorer);
      Path    : constant Gtk_Tree_Path :=
        Gtk_Tree_Path (Get_Proxy (Nth (Values, 2)));
      Iter    : Gtk_Tree_Iter;
      Success : Boolean;
      pragma Unreferenced (Success);

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
            N_Type : constant Node_Types := Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

         begin
            case N_Type is
               when Directory_Node =>
                  Free_Children (T, Iter);
                  Set (T.File_Model, Iter, Icon_Column,
                       Glib.C_Proxy (Open_Pixbufs (Directory_Node)));
                  File_Append_Directory (T, Iter_Name, Iter, 1);

            end case;
         end;

         Success := Expand_Row (T.File_Tree, Path, False);
         Scroll_To_Cell
           (T.File_Tree,
            Path, null, True,
            0.1, 0.1);

         T.Expanding := False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
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
      if Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (Tree, Model, Event);

         if Iter /= Null_Iter then
            Select_Iter (Get_Selection (Tree), Iter);
            case Node_Types'Val
                 (Integer (Get_Int (Model, Iter, Node_Type_Column))) is

               when Directory_Node =>
                  if Get_Event_Type (Event) = Gdk_2button_Press then
                     declare
                        Path    : Gtk_Tree_Path;
                        Success : Boolean;
                        pragma Unreferenced (Success);
                     begin
                        Path := Get_Path (Model, Iter);

                        if Row_Expanded (Tree, Path) then
                           Success := Collapse_Row (Tree, Path);
                        else
                           if Add_Dummy then
                              Append_Dummy_Iter (Model, Iter);
                           end if;

                           Success := Expand_Row
                             (Tree, Path, False);
                        end if;

                        Path_Free (Path);
                     end;
                  end if;

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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end File_Button_Press;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Gtk.Main.Timeout_Handler_Id) is
      pragma Unreferenced (D);
   begin
      null;
   end Free;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Explorer : access Dir_Tree_Record'Class)
   is
      Buffer       : aliased String (1 .. 1024);
      Last, Len    : Integer;
      Cur_Dir      : constant String := Get_Current_Dir;
      Dir_Inserted : Boolean := False;

   begin
      Clear (Explorer.File_Model);
      File_Remove_Idle_Calls (Explorer);

      Get_Logical_Drive_Strings (Buffer, Len);

      if Len = 0 then
         File_Append_Directory
           (Explorer, (1 => Directory_Separator),
            Null_Iter, 1, Cur_Dir, True);

      else
         Last := 1;

         for J in 1 .. Len loop
            if Buffer (J) = ASCII.NUL then
               if File_Equal
                 (Buffer (Last .. J - 1),
                  Cur_Dir (Cur_Dir'First ..
                       Cur_Dir'First + J - Last - 1))
               then
                  File_Append_Directory
                    (Explorer, Buffer (Last .. J - 1),
                     Null_Iter, 1, Cur_Dir, True);
                  Dir_Inserted := True;

               else
                  File_Append_Directory
                    (Explorer, Buffer (Last .. J - 1),
                     Null_Iter, 0, "", False, False);
               end if;

               Last := J + 1;
            end if;
         end loop;

         if not Dir_Inserted then
            declare
               J : Natural := Cur_Dir'First;
            begin
               while J < Cur_Dir'Last
                 and then Cur_Dir (J) /= Directory_Separator
               loop
                  J := J + 1;
               end loop;

               File_Append_Directory
                 (Explorer, Cur_Dir (Cur_Dir'First .. J),
                  Null_Iter, 1, Cur_Dir, True);
            end;
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Refresh;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Dir_Tree;
      Iter : Gtk_Tree_Iter)
   is
      Current : Gtk_Tree_Iter := Children (T.File_Model, Iter);
   begin
      if Has_Child (T.File_Model, Iter) then
         while Current /= Null_Iter loop
            Remove (T.File_Model, Current);
            Current := Children (T.File_Model, Iter);
         end loop;
      end if;
   end Free_Children;

end Directory_Tree;

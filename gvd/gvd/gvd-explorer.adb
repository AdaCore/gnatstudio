-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                  use Glib;
with Gdk.Bitmap;            use Gdk.Bitmap;
with Gdk.Color;             use Gdk.Color;
with Gdk.Pixmap;            use Gdk.Pixmap;
with Gdk.Types;             use Gdk.Types;
with Gdk.Event;             use Gdk.Event;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Ctree;             use Gtk.Ctree;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Style;             use Gtk.Style;
with Gtk.Tooltips;          use Gtk.Tooltips;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Types;          use Gtkada.Types;
with Gtkada.Handlers;       use Gtkada.Handlers;

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with Language;              use Language;
with Debugger;              use Debugger;

with GVD.Code_Editors;      use GVD.Code_Editors;
with GVD.Menus;             use GVD.Menus;
with GVD.Pixmaps;           use GVD.Pixmaps;
with GVD.Preferences;       use GVD.Preferences;
with GVD.Process;           use GVD.Process;
with GVD.Source_Editors;    use GVD.Source_Editors;
with GVD.Strings;           use GVD.Strings;
with GVD.Types;             use GVD.Types;
with Odd_Intl;              use Odd_Intl;

package body GVD.Explorer is

   type Node_Data (Length : Integer) is record
      Extension    : String (1 .. Length);
      --  Extension associated with the node, or full file name

      Is_File_Node : Boolean := False;
      Computed     : Boolean := False;
      Dummy_Node   : Gtk_Ctree_Node := null;
   end record;
   type Node_Data_Access is access Node_Data;
   package Row_Data_Pkg is new Row_Data (Node_Data_Access);

   type Internal_Category is record
      Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
      Mask    : Gdk.Bitmap.Gdk_Bitmap;
      Node    : Gtk_Ctree_Node;
   end record;
   type Internal_Categories is
     array (Category_Index range <>) of Internal_Category;

   package Tree_Cb is new Gtk.Handlers.Callback (Explorer_Record);

   package Row_Data_Explorer is new Gtk.Ctree.Row_Data (Position_Type);

   procedure First_Handler (Explorer : access Explorer_Record'Class);
   --  Callback handler for Ctree signals.

   procedure Expand_Explorer_Tree
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  Compute the contents of a file if needed. This is not done
   --  systematically, so as to speed things up.

   function Button_Press_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle button press events in the Explorer.

   procedure Display_Shared (Explorer : access Explorer_Record'Class);
   --  Load and display the files found in shared libraries.

   procedure Delete_Not_Found (Explorer : access Explorer_Record'Class);
   --  Delete from the tree all the files that can not be found on the PATH.
   --  These are files that we won't be able to display in the code_editor,
   --  and that can't be explored anyway.

   procedure Show_Current_File (Explorer : access Explorer_Record'Class);
   --  Show and select the node that matches the current source file.

   function Explorer_Contextual_Menu
     (Explorer : access Explorer_Record'Class)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) the contextual menu for an explorer widget.

   function Convert (Explorer : access Explorer_Record'Class)
     return Debugger_Process_Tab;
   --  Return the Process_Tab associated with an explorer.

   Explorer_Contextual_Menu_Name : constant String := "gvd_debugger_context";
   --  String used to store the explorer menu in the user data of the explorer.

   function Find_Node_From_File
     (Explorer  : access Explorer_Record'Class;
      File_Name : String)
     return Gtk_Ctree_Node;
   --  Return the Node that contains the file File_Name, or null if there is
   --  no such node.

   procedure Show_System_Files (Explorer : access Explorer_Record'Class);
   --  Toggle the display of system files

   procedure Clear_Explorer (Explorer : access Explorer_Record'Class);
   --  Remove all the files from the explorer.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer    : out Explorer_Access;
      Code_Editor : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Color : Gdk.Color.Gdk_Color;
   begin
      Explorer := new Explorer_Record;
      Initialize (Explorer, Columns => 1);

      Add_Events (Explorer, Button_Press_Mask or Button_Release_Mask);

      Explorer.Code_Editor := Gtk_Widget (Code_Editor);

      Color := Get_Pref (File_Name_Bg_Color);
      Explorer.Current_File_Style := Copy (Get_Style (Code_Editor));
      Set_Base (Explorer.Current_File_Style, State_Normal, Color);
      Set_Base (Explorer.Current_File_Style, State_Selected, Color);
      Set_Foreground (Explorer.Current_File_Style, State_Active,
                      Black (Get_System));

      Explorer.File_Name_Style := Copy (Get_Style (Code_Editor));

      Widget_Callback.Connect
        (Explorer, "tree_expand", Expand_Explorer_Tree'Access);
      Tree_Cb.Connect
        (Explorer, "tree_select_row",
         Tree_Cb.To_Marshaller (First_Handler'Access));
      Gtkada.Handlers.Return_Callback.Connect
        (Explorer, "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press_Cb'Access));

      Create_From_Xpm_D
        (Explorer.Folder_Open_Pixmap,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Explorer.Folder_Open_Mask,
         Transparent => Null_Color,
         Data        => mini_ofolder_xpm);
      Create_From_Xpm_D
        (Explorer.Folder_Pixmap,
         Window      => null,
         Colormap    => Get_System,
         Mask        => Explorer.Folder_Mask,
         Transparent => Null_Color,
         Data        => mini_folder_xpm);

      Explorer.Explorer_Root := Insert_Node
        (Explorer,
         Parent        => null,
         Sibling       => null,
         Text          => Null_Array + (-"list of files"),
         Spacing       => 5,
         Pixmap_Closed => Explorer.Folder_Pixmap,
         Mask_Closed   => Explorer.Folder_Mask,
         Pixmap_Opened => Explorer.Folder_Open_Pixmap,
         Mask_Opened   => Explorer.Folder_Open_Mask,
         Is_Leaf       => False,
         Expanded      => False);
      Show_All (Explorer);

      --  See Expand_Explorer_Tree for more explanation on the horizontal
      --  scrolling problem that is not solved by the setting below ???
      --  Set_Indent (Explorer, 20);
      --  Set_Spacing (Explorer, 0);
      --  Set_Expander_Style (Explorer, Ctree_Expander_None);
   end Gtk_New;

   --------------------
   -- Clear_Explorer --
   --------------------

   procedure Clear_Explorer (Explorer : access Explorer_Record'Class) is
   begin
      Freeze (Explorer);

      Clear (Explorer);
      Explorer.Current_File_Node := null;
      Explorer.Current_Line := 1;
      Explorer.Explorer_Root := Insert_Node
        (Explorer,
         Parent        => null,
         Sibling       => null,
         Text          => Null_Array + (-"list of files"),
         Spacing       => 5,
         Pixmap_Closed => Explorer.Folder_Pixmap,
         Mask_Closed   => Explorer.Folder_Mask,
         Pixmap_Opened => Explorer.Folder_Open_Pixmap,
         Mask_Opened   => Explorer.Folder_Open_Mask,
         Is_Leaf       => False,
         Expanded      => False);

      Thaw (Explorer);
   end Clear_Explorer;

   -------------------
   -- First_Handler --
   -------------------

   procedure First_Handler
     (Explorer : access Explorer_Record'Class)
   is
      Node      : Gtk_Ctree_Node := Node_List.Get_Data
        (Node_List.First (Get_Selection (Explorer)));
      File_Node : Gtk_Ctree_Node;
      Data      : Node_Data_Access;
      Tab       : Debugger_Process_Tab := Convert (Explorer);
      Lang      : Language_Access;
      Line      : Natural := 1;

   begin
      --  ???  Should set Data.Line to the current line for the current
      --  selection, so that when the user selects the same file again, we
      --  show him the line he was on.

      --  If an entity was referenced, load the file and display the
      --  correct line.

      if Row_Get_Is_Leaf (Node_Get_Row (Node)) then
         File_Node := Row_Get_Parent
           (Node_Get_Row (Row_Get_Parent (Node_Get_Row (Node))));
         Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, File_Node);
         Lang := Get_Language_From_File (Data.Extension);
         Set_Current_Language
           (Code_Editor (Explorer.Code_Editor), Lang);
         Load_File (Code_Editor (Explorer.Code_Editor),
                    Find_File (Tab.Debugger, Data.Extension),
                    Set_Current => False);

         if File_Node = Explorer.Current_File_Node then
            Set_Line (Code_Editor (Explorer.Code_Editor),
                      Explorer.Current_Line, Set_Current => True);
         end if;

         Highlight_Word (Get_Source (Code_Editor (Explorer.Code_Editor)),
                         Row_Data_Explorer.Node_Get_Row_Data (Explorer, Node));

      --  Else if a file was selected

      else
         Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);

         if Data.Is_File_Node then
            Lang := Get_Language_From_File (Data.Extension);
            Set_Current_Language
              (Code_Editor (Explorer.Code_Editor), Lang);

            if Node = Explorer.Current_File_Node then
               Line := Explorer.Current_Line;
            end if;

            Load_File (Code_Editor (Explorer.Code_Editor),
                       Find_File (Tab.Debugger, Data.Extension),
                       Set_Current => False);

            if Line /= 1 then
               Set_Line (Code_Editor (Explorer.Code_Editor), Line,
                         Set_Current => True);
            else
               Set_Line (Code_Editor (Explorer.Code_Editor), Line,
                         Set_Current => False);
            end if;
         end if;
      end if;

   exception
      --  Raise when the node was not associated with some data (for nodes
      --  associated with entity categories)
      when Gtkada.Types.Data_Error =>
         null;
   end First_Handler;

   -------------
   -- Explore --
   -------------

   procedure Explore
     (Tree      : access Explorer_Record;
      Root      : Gtk.Ctree.Gtk_Ctree_Node;
      Window    : access Gtk_Widget_Record'Class;
      Buffer    : String;
      Lang      : Language.Language_Access;
      File_Name : String)
   is
      Matches            : Match_Array (0 .. 10);

      Categories         : constant Explorer_Categories :=
        Explorer_Regexps (Lang);
      Internal_Cat       : Internal_Categories (Categories'Range);

      First              : Natural;
      Node               : Gtk_Ctree_Node;

   begin
      Freeze (Tree);

      --  Create all required icons

      for C in Categories'Range loop
         Create_From_Xpm_D
           (Internal_Cat (C).Pixmap,
            Get_Window (Window),
            Internal_Cat (C).Mask,
            Null_Color,
            Categories (C).Icon.all);
      end loop;

      --  For each category, parse the file

      for C in Categories'Range loop
         if Categories (C).Make_Entry /= null then
            Node := null;
            First := Buffer'First;

            loop
               Match (Categories (C).Regexp.all,
                      Buffer (First .. Buffer'Last),
                      Matches);

               exit when Matches (0) = No_Match;

               declare
                  Cat : aliased Category_Index := C;
                  S   : String := Categories (C).Make_Entry
                    (Buffer, Matches, Cat'Access);

               begin
                  --  Create the parent node for the category, if needed.

                  if Internal_Cat (Cat).Node = null then
                     Internal_Cat (Cat).Node := Insert_Node
                       (Tree, Root, null,
                        Null_Array + Categories (Cat).Name.all,
                        5, Tree.Folder_Pixmap, Tree.Folder_Mask,
                        Tree.Folder_Open_Pixmap, Tree.Folder_Open_Mask,
                        False, False);
                  end if;

                  Node := Insert_Node
                    (Tree, Internal_Cat (Cat).Node, null,
                     Null_Array + S,
                     5,
                     Internal_Cat (Cat).Pixmap, Internal_Cat (Cat).Mask,
                     null, null,
                     True, False);
               end;

               Row_Data_Explorer.Node_Set_Row_Data
                 (Tree, Node, Position_Type
                  (Matches (Categories (C).Position_Index).First));
               First := Matches (0).Last;
            end loop;
         end if;
      end loop;

      --  Free all icons

      for C in Categories'Range loop
         if Internal_Cat (C).Node /= null then
            Sort_Recursive (Tree, Internal_Cat (C).Node);
         end if;

         Gdk.Pixmap.Unref (Internal_Cat (C).Pixmap);
         Gdk.Bitmap.Unref (Internal_Cat (C).Mask);
      end loop;

      Thaw (Tree);
   end Explore;

   --------------------------
   -- Expand_Explorer_Tree --
   --------------------------

   procedure Expand_Explorer_Tree
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Explorer    : Explorer_Access := Explorer_Access (Widget);
      Node        : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
      Data        : Node_Data_Access;

   begin
      Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);

      if Data.Is_File_Node
        and then not Data.Computed
      then
         Remove_Node (Explorer, Data.Dummy_Node);
         Data.Computed := True;

         --  Read the size of the file
         declare
            F         : File_Descriptor;
            Length    : Positive;
            Tab       : Debugger_Process_Tab := Convert (Explorer);
            Full_Name : String := Find_File (Tab.Debugger, Data.Extension);
            Name      : aliased String := Full_Name & ASCII.NUL;

         begin
            F := Open_Read (Name'Address, Binary);

            if F /= Invalid_FD then
               Length := Positive (File_Length (F));

               --  Allocate the buffer
               --  and strip the ^Ms from the string
               declare
                  S    : String (1 .. Length);
                  Lang : Language_Access;
               begin
                  --  Should use GVD.Files.Load_File instead to take
                  --  advantage of caches and remote capabilities ???

                  Length := Read (F, S'Address, Length);

                  --  Need to read the file independently from the
                  --  code_editor.
                  Lang := Get_Language_From_File (Full_Name);

                  if Lang /= null then
                     if Need_To_Strip_Control_M then
                        Explore
                          (Explorer, Node, Explorer,
                           Strip_Control_M (S), Lang, Full_Name);

                     else
                        Explore (Explorer, Node, Explorer, S, Lang, Full_Name);
                     end if;

                  else
                     Node := Insert_Node
                       (Explorer,
                        Parent        => Node,
                        Sibling       => null,
                        Text          => Null_Array + (-"Unknown Language"),
                        Spacing       => 5,
                        Pixmap_Closed => Null_Pixmap,
                        Mask_Closed   => Null_Bitmap,
                        Pixmap_Opened => Null_Pixmap,
                        Mask_Opened   => Null_Bitmap,
                        Is_Leaf       => True,
                        Expanded      => False);
                     Node_Set_Selectable (Explorer, Node, False);
                  end if;
               end;
               Close (F);

            --  File not found
            else
               Node := Insert_Node
                 (Explorer,
                  Parent        => Node,
                  Sibling       => null,
                  Text          => Null_Array + (-"<file not found>"),
                  Spacing       => 5,
                  Pixmap_Closed => Null_Pixmap,
                  Mask_Closed   => Null_Bitmap,
                  Pixmap_Opened => Null_Pixmap,
                  Mask_Opened   => Null_Bitmap,
                  Is_Leaf       => True,
                  Expanded      => False);
               Node_Set_Selectable (Explorer, Node, False);
            end if;
         end;

         Show_All (Explorer);
      end if;

      --  This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      --  ??? Note that this is too expensive to call it during tree creating
      --  when the tree contains lots of items since a complete tree traversal
      --  is done for each item added. Instead, set the resize attribute
      --  only after the first expand.

      Set_Column_Auto_Resize (Explorer, 0, True);

   exception

      --  Raise when the node was not associated with some data (for nodes
      --  associated with entities)
      when Gtkada.Types.Data_Error =>
         null;
   end Expand_Explorer_Tree;

   -------------------
   -- Add_File_Node --
   -------------------

   procedure Add_File_Node
     (Tree      : access Explorer_Record;
      File_Name : String)
   is
      use type Row_List.Glist;
      Extension       : String := File_Extension (File_Name);
      Extension_Nodes : Row_List.Glist;
      Row_Found       : Boolean := False;
      Node            : Gtk_Ctree_Node;
      Data            : Node_Data_Access;
      Extension_Node  : Gtk_Ctree_Node;

   begin
      if Tree.Explorer_Root /= null then
         Remove_Node (Tree, Tree.Explorer_Root);
         Tree.Explorer_Root := null;
      end if;

      --  First find the parent row for the new node

      Extension_Nodes := Get_Row_List (Tree);

      if Extension_Nodes /= Row_List.Null_List then
         Extension_Node :=
           Find_Node_Ptr (Tree, Row_List.Get_Data (Extension_Nodes));

         while Extension_Node /= null loop
            Data := Row_Data_Pkg.Node_Get_Row_Data (Tree, Extension_Node);

            if Data.Extension = Extension then
               Row_Found := True;
               exit;
            end if;

            Extension_Node :=
              Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      if not Row_Found then
         if Extension /= "" then
            Extension_Node := Insert_Node
              (Tree,
               Parent  => Tree.Explorer_Root,
               Sibling => null,
               Text    => Null_Array + (Extension & " files"),
               Spacing => 5,
               Pixmap_Closed => Tree.Folder_Pixmap,
               Mask_Closed   => Tree.Folder_Mask,
               Pixmap_Opened => Tree.Folder_Open_Pixmap,
               Mask_Opened   => Tree.Folder_Open_Mask,
               Is_Leaf       => False,
               Expanded      => False);
         else
            Extension_Node := Insert_Node
              (Tree,
               Parent  => Tree.Explorer_Root,
               Sibling => null,
               Text    => Null_Array + (-"No extension files"),
               Spacing => 5,
               Pixmap_Closed => Tree.Folder_Pixmap,
               Mask_Closed   => Tree.Folder_Mask,
               Pixmap_Opened => Tree.Folder_Open_Pixmap,
               Mask_Opened   => Tree.Folder_Open_Mask,
               Is_Leaf       => False,
               Expanded      => False);
         end if;

         Data := new Node_Data'(Length       => Extension'Length,
                                Extension    => Extension,
                                Is_File_Node => False,
                                Computed     => False,
                                Dummy_Node   => null);
         Row_Data_Pkg.Node_Set_Row_Data (Tree, Extension_Node, Data);
      end if;

      --  Then insert the node (and indicate that the node has not been
      --  explored yet).

      Node := Insert_Node
        (Tree,
         Parent        => Extension_Node,
         Sibling       => null,
         Text          => Null_Array + Base_File_Name (File_Name),
         Spacing       => 5,
         Pixmap_Closed => Tree.Folder_Pixmap,
         Mask_Closed   => Tree.Folder_Mask,
         Pixmap_Opened => Tree.Folder_Open_Pixmap,
         Mask_Opened   => Tree.Folder_Open_Mask,
         Is_Leaf       => False,
         Expanded      => False);
      Data := new Node_Data'(Length       => File_Name'Length,
                             Extension    => File_Name,
                             Is_File_Node => True,
                             Computed     => False,
                             Dummy_Node   => null);

      --  Insert a dummy node so that the ctree displays a [+] button on
      --  the left side.

      Data.Dummy_Node := Insert_Node
        (Tree,
         Parent        => Node,
         Sibling       => null,
         Text          => Null_Array + "",
         Spacing       => 5,
         Pixmap_Closed => Null_Pixmap,
         Mask_Closed   => Null_Bitmap,
         Pixmap_Opened => Null_Pixmap,
         Mask_Opened   => Null_Bitmap,
         Is_Leaf       => True,
         Expanded      => True);
      Row_Data_Pkg.Node_Set_Row_Data (Tree, Node, Data);
   end Add_File_Node;

   -----------------------
   -- Add_List_Of_Files --
   -----------------------

   procedure Add_List_Of_Files
     (Tree : access Explorer_Record;
      List : GVD.Types.String_Array) is
   begin
      Freeze (Tree);

      for File in List'Range loop
         if List (File) /= null then
            Add_File_Node (Tree, List (File).all);
         end if;
      end loop;

      Sort_Recursive (Tree, Tree.Explorer_Root);
      Thaw (Tree);
      Show_All (Tree);
   end Add_List_Of_Files;

   -------------------------
   -- Find_Node_From_File --
   -------------------------

   function Find_Node_From_File
     (Explorer  : access Explorer_Record'Class;
      File_Name : String) return Gtk_Ctree_Node
   is
      use type Row_List.Glist;
      Base_Name       : String := Base_File_Name (File_Name);
      Extension       : String := File_Extension (File_Name);
      Extension_Nodes : Row_List.Glist := Get_Row_List (Explorer);
      Extension_Node  : Gtk_Ctree_Node;
      Data            : Node_Data_Access;
      Row_Found       : Boolean := False;
      Node            : Gtk_Ctree_Node;

   begin
      --  Find the extension node for the current file
      if Explorer.Explorer_Root = null
        and then Extension_Nodes /= Row_List.Null_List
      then
         Extension_Node :=
           Find_Node_Ptr (Explorer, Row_List.Get_Data (Extension_Nodes));

         while Extension_Node /= null loop
            Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Extension_Node);

            if Data.Extension = Extension then
               Row_Found := True;
               exit;
            end if;

            Extension_Node := Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      --  No extension node found.
      if not Row_Found then
         return null;
      end if;

      --  Find the node
      Row_Found := False;
      Node := Row_Get_Children (Node_Get_Row (Extension_Node));

      while Node /= null loop
         Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);

         if Data.Extension = Base_Name then
            return Node;
         end if;

         Node := Row_Get_Sibling (Node_Get_Row (Node));
      end loop;

      return null;
   end Find_Node_From_File;

   ----------------------
   -- Set_Current_File --
   ----------------------

   procedure Set_Current_File
     (Tree      : access Explorer_Record;
      File_Name : String)
   is
      use type Row_List.Glist;
      Node : Gtk_Ctree_Node :=
        Find_Node_From_File (Tree, File_Name);

   begin
      --  Get rid of the highlight on the previous current file

      if Tree.Current_File_Node /= null then
         Node_Set_Row_Style (Tree, Tree.Current_File_Node, Null_Style);
         Tree.Current_File_Node := null;
      end if;

      if Node = null then
         return;
      end if;

      Tree.Current_File_Node := Node;
      Node_Set_Row_Style
        (Tree, Tree.Current_File_Node, Tree.Current_File_Style);

      --  Expand all the parents to make the node visible
      if not Is_Viewable (Tree, Tree.Current_File_Node) then
         Expand (Tree, Row_Get_Parent (Node_Get_Row (Tree.Current_File_Node)));
      end if;

      --  Scroll to make the node visible (??? This does not work, since the
      --  scrollbar is always displayed at the bottom, even when the current
      --  file is at the top).
--   if Node_Is_Visible (Tree, Tree.Current_File_Node) /= Visibility_Full then
--      Node_Moveto (Tree, Tree.Current_File_Node, 0, 0.5, 0.0);
--   end if;
   end Set_Current_File;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Explorer : access Explorer_Record'Class)
   is
      Tab  : constant Debugger_Process_Tab := Convert (Explorer);
      List : GVD.Types.String_Array := Source_Files_List (Tab.Debugger);

   begin
      Set_Column_Auto_Resize (Explorer, 0, False);
      Clear_Explorer (Explorer);
      Add_List_Of_Files (Explorer, List);
      Set_Column_Auto_Resize (Explorer, 0, True);
      GVD.Types.Free (List);
   end On_Executable_Changed;

   ---------------------
   -- Button_Press_Cb --
   ---------------------

   function Button_Press_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      Explorer : constant Explorer_Access := Explorer_Access (Widget);
      Menu    : Gtk_Menu;
   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         Menu := Explorer_Contextual_Menu (Explorer);
         Popup (Menu,
                Button        => Gdk.Event.Get_Button (Event),
                Activate_Time => Gdk.Event.Get_Time (Event));
         Emit_Stop_By_Name (Explorer, "button_press_event");
         return True;
      end if;

      return False;
   end Button_Press_Cb;

   ------------------------------
   -- Explorer_Contextual_Menu --
   ------------------------------

   function Explorer_Contextual_Menu
     (Explorer : access Explorer_Record'Class)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;

      --  Check : Gtk_Check_Menu_Item;
      Mitem : Gtk_Menu_Item;
      Tips  : Gtk_Tooltips;
      Process : Debugger_Process_Tab := Convert (Explorer);

   begin
      begin
         Menu := Menu_User_Data.Get (Explorer, Explorer_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error =>
            null;
      end;

      Gtk_New (Menu);
      Gtk_New (Tips);
      Ref (Tips);

      --  ???  Currently use a standard item, since it is not possible to
      --  hide nodes of the tree, only to remove them. Thus showing again the
      --  system files would require to reparse and recreate the list of
      --  files.
      Gtk_New (Mitem, Label => -"Hide System Files");
      --  Set_Always_Show_Toggle (Check, True);
      --  Set_Active (Check, False);
      Tree_Cb.Object_Connect
        (Mitem, "activate",
         Tree_Cb.To_Marshaller (Show_System_Files'Access),
         Explorer);
      Append (Menu, Mitem);
      --  Set_Tip (Tips, Check, -"Foo", "");

      Gtk_New (Mitem, Label => -"Display Files In Shared Libraries");
      Set_Sensitive (Mitem, Is_Started (Process.Debugger));
      Tree_Cb.Object_Connect
        (Mitem, "activate",
         Tree_Cb.To_Marshaller (Display_Shared'Access),
         Explorer);
      Append (Menu, Mitem);
      --  Set_Tip (Tips, Mitem,
      --     -"Activated only when the executable has started", "");

      Gtk_New (Mitem, Label => -"Delete Files Not Found");
      Tree_Cb.Object_Connect
        (Mitem, "activate",
         Tree_Cb.To_Marshaller (Delete_Not_Found'Access),
         Explorer);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show Current File");
      Tree_Cb.Object_Connect
        (Mitem, "activate",
         Tree_Cb.To_Marshaller (Show_Current_File'Access),
         Explorer);
      Append (Menu, Mitem);

      Show_All (Menu);
      Menu_User_Data.Set (Explorer, Menu, Explorer_Contextual_Menu_Name);

      Enable (Tips);
      return Menu;
   end Explorer_Contextual_Menu;

   --------------------
   -- Display_Shared --
   --------------------

   procedure Display_Shared (Explorer : access Explorer_Record'Class) is
      Process : Debugger_Process_Tab := Convert (Explorer);
      Data    : Node_Data_Access := null;
      Current : GVD.Types.String_Access;

   begin
      --  ??? Should be protected in case the debugger is currently busy

      Set_Busy_Cursor (Process, True);
      Freeze (Explorer);

      if Explorer.Current_File_Node /= null then
         Data := Row_Data_Pkg.Node_Get_Row_Data
           (Explorer, Explorer.Current_File_Node);
         Current := new String' (Data.Extension);
         Explorer.Current_File_Node := null;
      end if;

      Clear (Explorer);
      GVD.Explorer.On_Executable_Changed (Explorer);

      if Current /= null then
         Set_Current_File (Explorer, Current.all);
         Free (Current);
      end if;

      Thaw (Explorer);
      Set_Busy_Cursor (Process, False);
   end Display_Shared;

   -------------
   -- Convert --
   -------------

   function Convert (Explorer : access Explorer_Record'Class)
      return Debugger_Process_Tab is
   begin
      return Debugger_Process_Tab
        (Get_Process (Code_Editor (Explorer.Code_Editor)));
   end Convert;

   ----------------------
   -- Delete_Not_Found --
   ----------------------

   procedure Delete_Not_Found (Explorer : access Explorer_Record'Class) is
      use type Row_List.Glist;
      Extension_Nodes : Row_List.Glist := Get_Row_List (Explorer);
      Extension_Node  : Gtk_Ctree_Node;
      Data            : Node_Data_Access;
      Node            : Gtk_Ctree_Node;
      Next            : Gtk_Ctree_Node;
      Process         : Debugger_Process_Tab := Convert (Explorer);

   begin
      Freeze (Explorer);
      Set_Busy_Cursor (Process, True);

      --  Find the extension node for the current file

      if Extension_Nodes /= Row_List.Null_List then
         Extension_Node :=
           Find_Node_Ptr (Explorer, Row_List.Get_Data (Extension_Nodes));

         while Extension_Node /= null loop

            Node := Row_Get_Children (Node_Get_Row (Extension_Node));

            while Node /= null loop
               Next := Row_Get_Sibling (Node_Get_Row (Node));
               Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);

               --  ??? Should handle remote files as well
               if not Is_Regular_File
                 (Find_File (Process.Debugger, Data.Extension))
               then
                  Remove_Node (Explorer, Node);
               end if;

               Node := Next;
            end loop;

            Extension_Node := Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      Set_Busy_Cursor (Process, False);
      Thaw (Explorer);
   end Delete_Not_Found;

   ----------------------
   -- Set_Current_Line --
   ----------------------

   procedure Set_Current_Line
     (Tree : access Explorer_Record; Line : Natural) is
   begin
      Tree.Current_Line := Line;
   end Set_Current_Line;

   ----------------------
   -- Get_Current_Line --
   ----------------------

   function Get_Current_Line (Tree : access Explorer_Record) return Natural is
   begin
      return Tree.Current_Line;
   end Get_Current_Line;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File (Tree : access Explorer_Record) return String is
      Data : Node_Data_Access;
   begin
      if Tree.Current_File_Node = null then
         return "";
      end if;

      Data := Row_Data_Pkg.Node_Get_Row_Data (Tree, Tree.Current_File_Node);

      if Data = null then
         return "";
      else
         return Data.Extension;
      end if;
   end Get_Current_File;

   -----------------------
   -- Show_System_Files --
   -----------------------

   procedure Show_System_Files (Explorer : access Explorer_Record'Class) is
      use type Row_List.Glist;
      Extension_Nodes : Row_List.Glist := Get_Row_List (Explorer);
      Extension_Node  : Gtk_Ctree_Node;
      Data            : Node_Data_Access;
      Node            : Gtk_Ctree_Node;
      Next            : Gtk_Ctree_Node;
      Process         : Debugger_Process_Tab := Convert (Explorer);
      Lang            : Language_Access;

   begin
      Freeze (Explorer);
      Set_Busy_Cursor (Process, True);

      --  Find the extension node for the current file

      if Extension_Nodes /= Row_List.Null_List then
         Extension_Node :=
           Find_Node_Ptr (Explorer, Row_List.Get_Data (Extension_Nodes));

         while Extension_Node /= null loop
            Node := Row_Get_Children (Node_Get_Row (Extension_Node));

            while Node /= null loop
               Next := Row_Get_Sibling (Node_Get_Row (Node));
               Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);

               Lang := Get_Language_From_File (Data.Extension);

               if Lang /= null
                 and then Is_System_File (Lang, Data.Extension)
               then
                  Remove_Node (Explorer, Node);
               end if;

               Node := Next;
            end loop;

            Extension_Node := Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      Set_Busy_Cursor (Process, False);
      Thaw (Explorer);
   end Show_System_Files;

   -----------------------
   -- Show_Current_File --
   -----------------------

   procedure Show_Current_File (Explorer : access Explorer_Record'Class) is
   begin
      if Explorer.Current_File_Node /= null then
         if not Is_Viewable (Explorer, Explorer.Current_File_Node) then
            Expand
              (Explorer,
               Row_Get_Parent (Node_Get_Row (Explorer.Current_File_Node)));
         end if;

         Gtk_Select (Explorer, Explorer.Current_File_Node);

         --  Scroll the explorer to make the file visible
         if Node_Is_Visible (Explorer, Explorer.Current_File_Node)
           /= Visibility_Full
         then
            Node_Moveto (Explorer, Explorer.Current_File_Node, 0, 0.5, 0.0);
         end if;
      end if;
   end Show_Current_File;

end GVD.Explorer;

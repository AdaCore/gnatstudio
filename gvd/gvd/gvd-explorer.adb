-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gdk.Bitmap;            use Gdk.Bitmap;
with Gdk.Color;             use Gdk.Color;
with Gdk.Pixmap;            use Gdk.Pixmap;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtkada.Types;          use Gtkada.Types;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Odd.Pixmaps;           use Odd.Pixmaps;
with GNAT.Regpat;           use GNAT.Regpat;
with Language;              use Language;
with Odd.Strings;           use Odd.Strings;
with Gtk.Arguments;         use Gtk.Arguments;
with Gtk.Ctree;             use Gtk.Ctree;
with Gtk.Style;             use Gtk.Style;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Enums;             use Gtk.Enums;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Odd.Code_Editors;      use Odd.Code_Editors;
with Debugger.Gdb.Ada;      use Debugger.Gdb.Ada;
with Odd.Types;             use Odd.Types;
with Odd.Process;           use Odd.Process;
with Debugger;              use Debugger;
with Odd_Intl;              use Odd_Intl;

with Ada.Text_IO; use Ada.Text_IO;

package body Odd.Explorer is

   File_Name_Bg_Color : constant String := "#BEBEBE";
   --  Color used for the background of the file name in the editor (grey).


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
   type Internal_Categories is array (Category_Index range <>)
     of Internal_Category;

   package Tree_Cb is new
     Gtk.Handlers.Callback (Explorer_Record);

   package Row_Data_Explorer is new Gtk.Ctree.Row_Data (Position_Type);

   procedure First_Handler
     (Explorer : access Explorer_Record'Class);
   --  Callback handler for Ctree signals.

   function Get_Pos (Buffer : String; Index : Positive) return Position_Type;
   --  Return the line and column corresponding to Index in Buffer.

   procedure Expand_Explorer_Tree
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  Compute the contents of a file if needed. This is not done
   --  systematically, so as to speed things up.

   procedure On_Executable_Changed
     (Explorer : access Gtk_Widget_Record'Class);
   --  Called when the executable associated with the explorer has changed.

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
      Explorer.Code_Editor := Gtk_Widget (Code_Editor);

      Color := Parse (File_Name_Bg_Color);
      Alloc (Get_System, Color);
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

      Widget_Callback.Object_Connect
        (Get_Process (Odd.Code_Editors.Code_Editor (Code_Editor)),
         "executable_changed",
         Widget_Callback.To_Marshaller (On_Executable_Changed'Access),
         Explorer);

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
         Text          => Null_Array + "list of files",
         Spacing       => 5,
         Pixmap_Closed => Explorer.Folder_Pixmap,
         Mask_Closed   => Explorer.Folder_Mask,
         Pixmap_Opened => Explorer.Folder_Open_Pixmap,
         Mask_Opened   => Explorer.Folder_Open_Mask,
         Is_Leaf       => False,
         Expanded      => True);
      Show_All (Explorer);

      --  This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      Set_Column_Auto_Resize (Explorer, 0, True);

--       Set_Indent (Explorer, 20);
--       Set_Spacing (Explorer, 0);
--       Set_Expander_Style (Explorer, Ctree_Expander_None);
   end Gtk_New;

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos (Buffer : String; Index : Positive) return Position_Type is
      Result     : Position_Type;
      Line_Start : Positive := 1;
   begin
      Result.Line := 0;
      Result.Index := Index;

      for J in Buffer'First .. Index loop
         if Buffer (J) = ASCII.LF then
            Result.Line := Result.Line + 1;
            Line_Start := J;
         end if;
      end loop;

      Result.Column := Index - Line_Start;
      return Result;
   end Get_Pos;

   -------------------
   -- First_Handler --
   -------------------

   procedure First_Handler
     (Explorer : access Explorer_Record'Class)
   is
      Node : Gtk_Ctree_Node := Node_List.Get_Data
        (Node_List.First (Get_Selection (Explorer)));
      File_Node : Gtk_Ctree_Node;
      Data : Node_Data_Access;
      Tab  : Debugger_Process_Tab := Debugger_Process_Tab
        (Get_Process (Code_Editor (Explorer.Code_Editor)));
   begin
      --  If an entity was referenced, load the file and display the
      --  correct line.

      if Row_Get_Is_Leaf (Node_Get_Row (Node)) then
         File_Node := Row_Get_Parent
           (Node_Get_Row (Row_Get_Parent (Node_Get_Row (Node))));
         Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, File_Node);
         Load_File (Code_Editor (Explorer.Code_Editor),
                    Find_File (Tab.Debugger, Data.Extension));
         Highlight_Word (Code_Editor (Explorer.Code_Editor),
                         Row_Data_Explorer.Node_Get_Row_Data (Explorer, Node));

      --  Else if a file was selected

      else
         Data := Row_Data_Pkg.Node_Get_Row_Data (Explorer, Node);
         if Data.Is_File_Node then
            Load_File (Code_Editor (Explorer.Code_Editor),
                       Find_File (Tab.Debugger, Data.Extension));
            --  Set_Line (Code_Editor (Explorer.Code_Editor), 1);
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
                  S : String := Categories (C).Make_Entry
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
                 (Tree, Node, Get_Pos (Buffer,
                  Matches (Categories (C).Position_Index).First));
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
            Tab         : Debugger_Process_Tab := Debugger_Process_Tab
              (Get_Process (Code_Editor (Explorer.Code_Editor)));
            Full_Name : String := Find_File
              (Tab.Debugger, Data.Extension);
            Name      : aliased String := Full_Name & ASCII.NUL;
         begin
            F := Open_Read (Name'Address, Text);

            if F /= Invalid_FD then
               Length := Positive (File_Length (F));

               --  Allocate the buffer
               --  and strip the ^Ms from the string
               declare
                  S : String (1 .. Length);
               begin
                  Length := Read (F, S'Address, Length);

                  --  Need to read the file independently from the
                  --  code_editor.
                  --  ??? Fix: Correctly detect the language.
                  Explore
                    (Explorer, Node, Explorer, S, new Gdb_Ada_Language,
                     Full_Name);
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

            Extension_Node := Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      if not Row_Found then
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
         Expanded      => False);
      Row_Data_Pkg.Node_Set_Row_Data (Tree, Node, Data);
   end Add_File_Node;

   -----------------------
   -- Add_List_Of_Files --
   -----------------------

   procedure Add_List_Of_Files
     (Tree : access Explorer_Record;
      List : Odd.Types.String_Array) is
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

   ----------------------
   -- Set_Current_File --
   ----------------------

   procedure Set_Current_File
     (Tree : access Explorer_Record;
      File_Name : String)
   is
      use type Row_List.Glist;
      Base_Name       : String := Base_File_Name (File_Name);
      Extension       : String := File_Extension (File_Name);
      Extension_Nodes : Row_List.Glist := Get_Row_List (Tree);
      Extension_Node  : Gtk_Ctree_Node;
      Data            : Node_Data_Access;
      Row_Found       : Boolean := False;
      Node            : Gtk_Ctree_Node;

   begin
      --  Get rid of the highlight on the previous current file
      if Tree.Current_File_Node /= null then
         Node_Set_Row_Style (Tree, Tree.Current_File_Node, Null_Style);
         Tree.Current_File_Node := null;
      end if;

      --  Find the extension node for the current file
      if Extension_Nodes /= Row_List.Null_List then
         Extension_Node :=
           Find_Node_Ptr (Tree, Row_List.Get_Data (Extension_Nodes));
         while Extension_Node /= null loop
            Data := Row_Data_Pkg.Node_Get_Row_Data (Tree, Extension_Node);
            if Data.Extension = Extension then
               Row_Found := True;
               exit;
            end if;
            Extension_Node := Row_Get_Sibling (Node_Get_Row (Extension_Node));
         end loop;
      end if;

      --  No extension node found.
      if not Row_Found then
         return;
      end if;

      --  Find the node
      Row_Found := False;
      Node := Row_Get_Children (Node_Get_Row (Extension_Node));
      while Node /= null loop
         Data := Row_Data_Pkg.Node_Get_Row_Data (Tree, Node);
         if Data.Extension = Base_Name then
            Row_Found := True;
            exit;
         end if;
         Node := Row_Get_Sibling (Node_Get_Row (Node));
      end loop;

      if not Row_Found then
         return;
      end if;

      Tree.Current_File_Node := Node;
      Node_Set_Row_Style
        (Tree, Tree.Current_File_Node, Tree.Current_File_Style);

      --  Expand all the parents to make the node visible
      if not Is_Viewable (Tree, Tree.Current_File_Node) then
         Expand (Tree, Row_Get_Parent (Node_Get_Row (Tree.Current_File_Node)));
      end if;

      --  Scroll to make the node visible
      if Node_Is_Visible (Tree, Tree.Current_File_Node) /= Visibility_Full then
         Node_Moveto (Tree, Tree.Current_File_Node, 0, 0.5, 0.0);
      end if;

--       Gtk_Select (Tree, Tree.Current_File_Node);
   end Set_Current_File;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Explorer : access Gtk_Widget_Record'Class)
   is
      Exp         : Explorer_Access := Explorer_Access (Explorer);
      Tab         : Debugger_Process_Tab := Debugger_Process_Tab
        (Get_Process (Code_Editor (Exp.Code_Editor)));
      List : Odd.Types.String_Array := Source_Files_List (Tab.Debugger);
   begin
      Add_List_Of_Files (Exp, List);
      Odd.Types.Free (List);
   end On_Executable_Changed;

end Odd.Explorer;

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
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gdk.Event;                 use Gdk.Event;
with Gtk.Main;                  use Gtk.Main;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Unchecked_Deallocation;

with Basic_Types;              use Basic_Types;
with Generic_List;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Intl;               use Glide_Intl;
with Language.Unknown;         use Language.Unknown;
with Language;                 use Language;
with Language_Handlers.Glide;  use Language_Handlers.Glide;
with Pixmaps_IDE;              use Pixmaps_IDE;
with Pixmaps_Prj;              use Pixmaps_Prj;
with Project_Explorers;        use Project_Explorers;
with Prj_API;                  use Prj_API;
with String_List_Utils;        use String_List_Utils;
with String_Utils;             use String_Utils;
with File_Utils;               use File_Utils;
with Traces;                   use Traces;
with Src_Info;

package body Project_Explorers_Files is

   Me : constant Debug_Handle := Create ("Project_Files");

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Explorer_Files_Module_Id   : Glide_Kernel.Module_ID := null;

   Icon_Column          : constant := 0;
   Base_Name_Column     : constant := 1;
   Absolute_Name_Column : constant := 2;
   Node_Type_Column     : constant := 3;
   User_Data_Column     : constant := 4;
   Line_Column          : constant := 5;
   Column_Column        : constant := 6;

   type Append_Directory_Idle_Data is record
      Explorer      : Project_Explorer_Files;
      Norm_Dest     : Basic_Types.String_Access;
      Norm_Dir      : Basic_Types.String_Access;
      D             : GNAT.Directory_Operations.Dir_Type;
      Depth         : Integer := 0;
      Base          : Gtk_Tree_Iter;
      Dirs          : String_List_Utils.String_List.List;
      Files         : String_List_Utils.String_List.List;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True;
   end record;

   procedure Free is
     new Unchecked_Deallocation (Append_Directory_Idle_Data,
                                 Append_Directory_Idle_Data_Access);

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   function Parse_Path
     (Path : String) return String_List_Utils.String_List.List;
   --  Parse a path string and return a list of all directories in it.

   function File_Append_Category_Node
     (Explorer    : access Project_Explorer_Files_Record'Class;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add a category node in the file view.

   function File_Append_Entity_Node
     (Explorer    : access Project_Explorer_Files_Record'Class;
      File        : String;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Add an entity node in the file view.

   procedure File_Append_File_Info
     (Explorer  : access Project_Explorer_Files_Record'Class;
      Node      : Gtk_Tree_Iter;
      File_Name : String);
   --  Add info to a file node in the file view.

   procedure File_Append_Dummy_Iter
     (Explorer : access Project_Explorer_Files_Record'Class;
      Base     : Gtk_Tree_Iter);
   --  Add an empty item to an iter in the file view.

   procedure File_Append_File
     (Explorer  : access Project_Explorer_Files_Record'Class;
      Base      : Gtk_Tree_Iter;
      File      : String);
   --  Append a file node to Base in the file view.
   --  File must be an absolute file name.

   procedure File_Append_Directory
     (Explorer      : access Project_Explorer_Files_Record'Class;
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

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues);
   --  Called every time a node is collapsed in the file view.

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view.

   procedure File_Remove_Idle_Calls
     (Explorer : access Project_Explorer_Files_Record'Class);
   --  Remove the idle calls for filling the file view.

   function Find_Iter_For_Event
     (Explorer : access Project_Explorer_Files_Record'Class;
      Event    : Gdk_Event)
      return Gtk_Tree_Iter;
   --  Get the iter in the file view under the cursor corresponding to Event,
   --  if any.

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the file view.

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for the "button_press" event on the file view.

   procedure Free_Children
     (T    : Project_Explorer_Files;
      Iter : Gtk_Tree_Iter);
   --  Free all the children of iter Iter in the file view.

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  ???
   --  Called by File_Append_Directory.

   function Explorer_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access;
   --  ??? Unused for now while the files explorer is not a separate module.
   --  Return the context to use for the contextual menu.
   --  It is also used to return the context for
   --  Glide_Kernel.Get_Current_Context, and thus can be called with a null
   --  event or a null menu.

   function Greatest_Common_Path
     (L : String_List_Utils.String_List.List) return String;
   --  Return the greatest common path to a list of directories.

   procedure Refresh
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Explorer : access Project_Explorer_Files_Record'Class);
   --  Refresh the contents of the explorer.

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Open_Explorer
     (Widget       : access GObject_Record'Class;
      Kernel       : Kernel_Handle);
   --  Raise the existing explorer, or open a new one.

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
         Column_Column        => GType_Int);
   end Columns_Types;

   -------------------------------
   -- File_Append_Category_Node --
   -------------------------------

   function File_Append_Category_Node
     (Explorer    : access Project_Explorer_Files_Record'Class;
      Category    : Language_Category;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
      Name    : constant String := Category_Name (Category);
   begin
      --  ??? code duplication from Add_Category_Node
      Sibling := Children (Explorer.File_Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Explorer.File_Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Explorer.File_Model, Sibling, Base_Name_Column)
           <= Name
         loop
            Next (Explorer.File_Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Explorer.File_Model, N, Parent_Iter);
         else
            Insert_Before (Explorer.File_Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Explorer.File_Model, N, Absolute_Name_Column, "");
      Set (Explorer.File_Model, N, Base_Name_Column, Name);
      Set (Explorer.File_Model, N, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (Category_Node)));
      Set (Explorer.File_Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Category_Node)));

      return N;
   end File_Append_Category_Node;

   -----------------------------
   -- File_Append_Entity_Node --
   -----------------------------

   function File_Append_Entity_Node
     (Explorer    : access Project_Explorer_Files_Record'Class;
      File        : String;
      Construct   : Construct_Information;
      Parent_Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      N       : Gtk_Tree_Iter;
      Sibling : Gtk_Tree_Iter;
   begin
      Sibling := Children (Explorer.File_Model, Parent_Iter);

      if Sibling = Null_Iter then
         Append (Explorer.File_Model, N, Parent_Iter);
      else
         while Sibling /= Null_Iter
           and then Get_String (Explorer.File_Model, Sibling, Base_Name_Column)
           <= Construct.Name.all
         loop
            Next (Explorer.File_Model, Sibling);
         end loop;

         if Sibling = Null_Iter then
            Append (Explorer.File_Model, N, Parent_Iter);
         else
            Insert_Before (Explorer.File_Model, N, Parent_Iter, Sibling);
         end if;
      end if;

      Set (Explorer.File_Model, N, Absolute_Name_Column, File);

      if Construct.Is_Declaration then
         if Construct.Profile /= null then
            Set (Explorer.File_Model, N, Base_Name_Column,
                 Construct.Name.all & " (spec) " &
                 Reduce (Construct.Profile.all));
         else
            Set (Explorer.File_Model, N, Base_Name_Column,
                 Construct.Name.all & " (spec)");

         end if;

      elsif Construct.Profile /= null then
         Set (Explorer.File_Model, N, Base_Name_Column,
              Construct.Name.all & " " & Reduce (Construct.Profile.all));
      else
         Set (Explorer.File_Model, N, Base_Name_Column,
              Construct.Name.all);
      end if;

      Set (Explorer.File_Model, N, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (Entity_Node)));
      Set (Explorer.File_Model, N, Node_Type_Column,
           Gint (Node_Types'Pos (Entity_Node)));

      Set (Explorer.File_Model, N,
           Line_Column, Gint (Construct.Sloc_Entity.Line));
      Set (Explorer.File_Model, N,
           Column_Column, Gint (Construct.Sloc_Entity.Line));
      return N;
   end File_Append_Entity_Node;

   ---------------------------
   -- File_Append_File_Info --
   ---------------------------

   procedure File_Append_File_Info
     (Explorer  : access Project_Explorer_Files_Record'Class;
      Node      : Gtk_Tree_Iter;
      File_Name : String)
   is
      use Src_Info;

      N          : Gtk_Tree_Iter;
      pragma Unreferenced (N);

      Lang       : Language_Access;
      Constructs : Construct_List;
      Category   : Language_Category;

      type Gtk_Tree_Iter_Array is array (Language_Category'Range)
        of Gtk_Tree_Iter;
      Categories : Gtk_Tree_Iter_Array := (others => Null_Iter);
      Languages  : constant Glide_Language_Handler :=
        Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel));
      Handler    : constant Src_Info.LI_Handler :=
        Get_LI_Handler_From_File (Languages, File_Name);

   begin
      Push_State (Explorer.Kernel, Busy);

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel)),
         File_Name);

      if Lang /= null then
         Parse_File_Constructs
           (Handler, Get_Project_View (Explorer.Kernel),
            Languages, File_Name, Constructs);

         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Category := Filter_Category (Constructs.Current.Category);

               if Category /= Cat_Unknown then
                  if Categories (Category) = Null_Iter then
                     Categories (Category) := File_Append_Category_Node
                       (Explorer,
                        Category    => Category,
                        Parent_Iter => Node);
                  end if;

                  N := File_Append_Entity_Node
                    (Explorer, File_Name,
                     Constructs.Current.all, Categories (Category));
               end if;
            end if;

            Constructs.Current := Constructs.Current.Next;
         end loop;

         Free (Constructs);
      end if;

      Pop_State (Explorer.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Free (Constructs);
         Pop_State (Explorer.Kernel);
   end File_Append_File_Info;

   ----------------------------
   -- File_Append_Dummy_Iter --
   ----------------------------

   procedure File_Append_Dummy_Iter
     (Explorer : access Project_Explorer_Files_Record'Class;
      Base     : Gtk_Tree_Iter)
   is
      Iter      : Gtk_Tree_Iter;
   begin
      Append (Explorer.File_Model, Iter, Base);
   end File_Append_Dummy_Iter;

   ----------------------
   -- File_Append_File --
   ----------------------

   procedure File_Append_File
     (Explorer : access Project_Explorer_Files_Record'Class;
      Base     : Gtk_Tree_Iter;
      File     : String)
   is
      Iter : Gtk_Tree_Iter;
      Lang : Language_Access;

   begin
      Append (Explorer.File_Model, Iter, Base);

      Set (Explorer.File_Model, Iter, Absolute_Name_Column, File);

      Set (Explorer.File_Model, Iter, Base_Name_Column, Base_Name (File));

      Set (Explorer.File_Model, Iter, Icon_Column,
           C_Proxy (Explorer.Close_Pixbufs (File_Node)));
      Set (Explorer.File_Model, Iter, Node_Type_Column,
           Gint (Node_Types'Pos (File_Node)));

      Lang := Get_Language_From_File
        (Glide_Language_Handler (Get_Language_Handler (Explorer.Kernel)),
         File);

      if Lang /= Unknown_Lang then
         File_Append_Dummy_Iter (Explorer, Iter);
      end if;
   end File_Append_File;

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
              D.Norm_Dir.all);
         Set (D.Explorer.File_Model, Iter, Base_Name_Column,
              D.Norm_Dir.all);
         Set (D.Explorer.File_Model, Iter, Node_Type_Column,
              Gint (Node_Types'Pos (Directory_Node)));

         if D.Physical_Read then
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));
            D.Base := Iter;

            return Read_Directory (D);

         else
            File_Append_Dummy_Iter (D.Explorer, Iter);
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 C_Proxy (D.Explorer.Close_Pixbufs (Directory_Node)));
            Pop_State (D.Explorer.Kernel);
            New_D := D;
            Free (New_D);

            return False;
         end if;
      end if;

      Read (D.D, File, Last);

      if D.Depth >= 0 and then Last /= 0 then
         if not (Last = 1 and then File (1) = '.')
           and then not (Last = 2 and then File (1 .. 2) = "..")
         then
            if Is_Directory (D.Norm_Dir.all & File (File'First .. Last)) then
               Append (D.Dirs, File (File'First .. Last));
            else
               Append (D.Files, File (File'First .. Last));
            end if;

            if D.Depth = 0 then
               D.Depth := -1;
            end if;
         end if;

         return True;
      end if;

      Close (D.D);

      if D.Idle then
         Pop_State (D.Explorer.Kernel);
         Push_State (D.Explorer.Kernel, Busy);
      end if;

      if Filenames_Are_Case_Sensitive then
         Sort (D.Dirs);
         Sort (D.Files);
      else
         Sort_Case_Insensitive (D.Dirs);
         Sort_Case_Insensitive (D.Files);
      end if;

      while not Is_Empty (D.Dirs) loop
         declare
            Dir : constant String := Head (D.Dirs);
         begin
            Append (D.Explorer.File_Model, Iter, D.Base);
            Set (D.Explorer.File_Model, Iter, Absolute_Name_Column,
                 D.Norm_Dir.all & Dir & Directory_Separator);
            Set (D.Explorer.File_Model, Iter, Base_Name_Column, Dir);
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
                       C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));

                  Path_Free (Path);
               end;

               --  Are we on the target directory ?

               if D.Norm_Dest.all = D.Norm_Dir.all & Dir
                 & Directory_Separator
               then
                  declare
                     Success   : Boolean;
                     pragma Unreferenced (Success);

                     Path      : Gtk_Tree_Path;
                     Expanding : constant Boolean := D.Explorer.Expanding;
                  begin
                     Path := Get_Path (D.Explorer.File_Model, Iter);

                     File_Append_Directory
                       (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                        Iter, D.Depth, D.Norm_Dest.all,
                        False);

                     D.Explorer.Expanding := True;
                     Success := Expand_Row (D.Explorer.File_Tree, Path, False);
                     D.Explorer.Expanding := Expanding;

                     Set (D.Explorer.File_Model, Iter, Icon_Column,
                          C_Proxy (D.Explorer.Open_Pixbufs (Directory_Node)));

                     Scroll_To_Cell
                       (D.Explorer.File_Tree,
                        Path, null, True,
                        0.1, 0.1);

                     Path_Free (Path);
                  end;

               else
                  File_Append_Directory
                    (D.Explorer, D.Norm_Dir.all & Dir & Directory_Separator,
                     Iter, D.Depth, D.Norm_Dest.all, D.Idle);
               end if;

            else
               File_Append_Dummy_Iter (D.Explorer, Iter);

               Set (D.Explorer.File_Model, Iter, Icon_Column,
                    C_Proxy (D.Explorer.Close_Pixbufs (Directory_Node)));
            end if;

            Next (D.Dirs);
         end;
      end loop;

      while not Is_Empty (D.Files) loop
         File_Append_File
           (D.Explorer, D.Base, D.Norm_Dir.all & Head (D.Files));
         Next (D.Files);
      end loop;

      Free (D.Norm_Dir);
      Free (D.Norm_Dest);

      Pop_State (D.Explorer.Kernel);

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
     (Explorer      : access Project_Explorer_Files_Record'Class;
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
      D.Explorer      := Project_Explorer_Files (Explorer);
      D.Idle          := Idle;
      D.Physical_Read := Physical_Read;

      if Idle then
         Push_State (Explorer.Kernel, Processing);
      else
         Push_State (Explorer.Kernel, Busy);
      end if;

      if Idle then
         Timeout_Id :=
           File_Append_Directory_Timeout.Add (1, Read_Directory'Access, D);
         Timeout_Id_List.Append (Explorer.Fill_Timeout_Ids, Timeout_Id);
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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Project_Explorer_Files;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Explorer := new Project_Explorer_Files_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Files_Record'Class;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Explorer);
      Set_Policy (Explorer, Policy_Automatic, Policy_Automatic);

      Gtk_New (Explorer.File_Model, Columns_Types);
      Gtk_New (Explorer.File_Tree, Explorer.File_Model);

      --  The model should be destroyed as soon as the tree view is destroyed
      Unref (Explorer.File_Model);

      Explorer.Kernel := Kernel_Handle (Kernel);

      Add (Explorer, Explorer.File_Tree);

      Set_Headers_Visible (Explorer.File_Tree, False);

      Return_Callback.Object_Connect
        (Explorer.File_Tree,
         "button_press_event",
         Return_Callback.To_Marshaller (File_Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);

      Widget_Callback.Object_Connect
        (Get_Selection (Explorer.File_Tree),
         "changed",
         Widget_Callback.To_Marshaller (File_Selection_Changed'Access),
         Slot_Object => Explorer,
         After       => True);

      Set_Column_Types (Explorer.File_Tree);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.File_Tree,
         Object          => Explorer,
         ID              => Explorer_Module_ID,
         Context_Func    => Explorer_Context_Factory'Access);

      Explorer.Open_Pixbufs (Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_xpm);
      Explorer.Close_Pixbufs (Project_Node) :=
        Gdk_New_From_Xpm_Data (project_closed_xpm);
      Explorer.Open_Pixbufs (Modified_Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_modified_xpm);
      Explorer.Close_Pixbufs (Modified_Project_Node) :=
        Gdk_New_From_Xpm_Data (project_modified_closed_xpm);
      Explorer.Open_Pixbufs (Extends_Project_Node)  :=
        Gdk_New_From_Xpm_Data (project_ext_xpm);
      Explorer.Close_Pixbufs (Extends_Project_Node) :=
        Gdk_New_From_Xpm_Data (project_ext_closed_xpm);
      Explorer.Open_Pixbufs (Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_ofolder_xpm);
      Explorer.Close_Pixbufs (Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_xpm);
      Explorer.Open_Pixbufs (Obj_Directory_Node)  :=
        Gdk_New_From_Xpm_Data (mini_folder_object_xpm);
      Explorer.Close_Pixbufs (Obj_Directory_Node) :=
        Gdk_New_From_Xpm_Data (mini_folder_object_xpm);
      Explorer.Open_Pixbufs (File_Node)  :=
        Gdk_New_From_Xpm_Data (mini_page_xpm);
      Explorer.Close_Pixbufs (File_Node) :=
        Gdk_New_From_Xpm_Data (mini_page_xpm);
      Explorer.Open_Pixbufs (Category_Node)  :=
        Gdk_New_From_Xpm_Data (var_xpm);
      Explorer.Close_Pixbufs (Category_Node) :=
        Gdk_New_From_Xpm_Data (var_xpm);

      Refresh (Kernel, Explorer);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "row_expanded",
         File_Tree_Expand_Row_Cb'Access, Explorer, False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "row_collapsed",
         File_Tree_Collapse_Row_Cb'Access, Explorer, False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, "destroy",
         On_File_Destroy'Access, Explorer, False);
   end Initialize;

   ------------------------------
   -- Explorer_Context_Factory --
   ------------------------------

   function Explorer_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Menu);

      T         : constant Project_Explorer_Files :=
        Project_Explorer_Files (Object);
      Context   : Selection_Context_Access;
      Iter      : constant Gtk_Tree_Iter := Find_Iter_For_Event (T, Event);
      File      : GNAT.OS_Lib.String_Access := null;
      Node_Type : Node_Types;
   begin
      if Iter /= Null_Iter then
         Node_Type := Node_Types'Val
           (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

         case Node_Type is
            when Directory_Node | File_Node =>
               File := new String'
                 (Get_String (T.File_Model, Iter, Absolute_Name_Column));
               Context := new File_Selection_Context;

            when Entity_Node =>
               Context := new Entity_Selection_Context;

            when others =>
               null;

         end case;
      end if;

      if File /= null then
         Set_File_Information
           (Context   => File_Selection_Context_Access (Context),
            Directory => Dir_Name (File.all),
            File_Name => Base_Name (File.all));
         Free (File);
      end if;

      return Context;
   end Explorer_Context_Factory;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Project_Explorer_Files_Record'Class) is
   begin
      while not Timeout_Id_List.Is_Empty (Explorer.Fill_Timeout_Ids) loop
         Pop_State (Explorer.Kernel);
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
      E : constant Project_Explorer_Files :=
        Project_Explorer_Files (Explorer);
   begin
      File_Remove_Idle_Calls (E);

      for P in E.Open_Pixbufs'Range loop
         if E.Open_Pixbufs (P) /= null then
            Unref (E.Open_Pixbufs (P));
         end if;
      end loop;

      for P in E.Close_Pixbufs'Range loop
         if E.Close_Pixbufs (P) /= null then
            Unref (E.Close_Pixbufs (P));
         end if;
      end loop;
   end On_File_Destroy;

   -------------------------------
   -- File_Tree_Collapse_Row_Cb --
   -------------------------------

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T    : constant Project_Explorer_Files :=
        Project_Explorer_Files (Explorer);
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
                    C_Proxy (T.Close_Pixbufs (Directory_Node)));
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end File_Tree_Collapse_Row_Cb;

   -----------------------------
   -- File_Tree_Expand_Row_Cb --
   -----------------------------

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Values   : GValues)
   is
      T       : Project_Explorer_Files := Project_Explorer_Files (Explorer);
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
            N_Type : constant Real_Node_Types := Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

         begin
            case N_Type is
               when Directory_Node =>
                  Free_Children (T, Iter);
                  File_Append_Directory (T, Iter_Name, Iter, 1);
                  Set (T.File_Model, Iter, Icon_Column,
                       C_Proxy (T.Open_Pixbufs (Directory_Node)));

               when File_Node =>
                  Free_Children (T, Iter);
                  File_Append_File_Info (T, Iter, Iter_Name);

               when Project_Node | Extends_Project_Node =>
                  null;

               when Category_Node | Entity_Node =>
                  null;

               when Obj_Directory_Node =>
                  null;
            end case;
         end;

         Success := Expand_Row (T.File_Tree, Path, False);
         T.Expanding := False;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Message (E));
   end File_Tree_Expand_Row_Cb;

   ----------------------------
   -- File_Selection_Changed --
   ----------------------------

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class)
   is
      T        : constant Project_Explorer_Files :=
        Project_Explorer_Files (Explorer);
      Context  : Selection_Context_Access;
   begin
      Context := Explorer_Context_Factory (T.Kernel, T, T, null, null);

      if Context /= null then
         Set_Context_Information (Context, T.Kernel, Explorer_Module_ID);
         Context_Changed (T.Kernel, Context);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Selection_Changed;

   -------------------------
   -- Find_Iter_For_Event --
   -------------------------

   function Find_Iter_For_Event
     (Explorer : access Project_Explorer_Files_Record'Class;
      Event    : Gdk_Event) return Gtk_Tree_Iter
   is
      X         : Gdouble;
      Y         : Gdouble;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;
      Iter      : Gtk_Tree_Iter := Null_Iter;
      Model     : Gtk_Tree_Model;
   begin
      if Event /= null then
         X := Get_X (Event);
         Y := Get_Y (Event);
         Path := Gtk_New;
         Get_Path_At_Pos
           (Explorer.File_Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path = null then
            return Iter;
         end if;

         Select_Path (Get_Selection (Explorer.File_Tree), Path);
         Iter := Get_Iter (Explorer.File_Model, Path);
         Path_Free (Path);
      else
         Get_Selected (Get_Selection (Explorer.File_Tree),
                       Model,
                       Iter);
      end if;

      return Iter;
   end Find_Iter_For_Event;

   -----------------------
   -- File_Button_Press --
   -----------------------

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T    : constant Project_Explorer_Files :=
        Project_Explorer_Files (Explorer);
      Iter : Gtk_Tree_Iter;
      Line, Column : Gint;
   begin
      if Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (T, Event);

         if Iter /= Null_Iter then
            case Node_Types'Val
              (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column))) is

               when Directory_Node =>
                  return False;

               when File_Node =>
                  if (Get_Event_Type (Event) = Gdk_2button_Press
                      or else Get_Event_Type (Event) = Gdk_3button_Press)
                  then
                     Open_File_Editor
                       (T.Kernel,
                        Get_String (T.File_Model, Iter, Absolute_Name_Column));
                     return True;
                  end if;

               when Entity_Node =>
                  Line := Get_Int (T.File_Model, Iter, Line_Column);
                  Column := Get_Int (T.File_Model, Iter, Column_Column);

                  Open_File_Editor
                    (T.Kernel,
                     Get_String (T.File_Model, Iter, Absolute_Name_Column),
                     Line   => Natural (Line),
                     Column => Natural (Column));
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
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Explorer : access Project_Explorer_Files_Record'Class)
   is
      Buffer       : aliased String (1 .. 1024);
      Last, Len    : Integer;
      Cur_Dir      : constant String := Get_Current_Dir;
      Dir_Inserted : Boolean := False;

   begin
      Clear (Explorer.File_Model);
      File_Remove_Idle_Calls (Explorer);

      if Get_Pref (Kernel, File_View_Shows_Only_Project) then
         declare
            Inc : String_List_Utils.String_List.List;
            Obj : String_List_Utils.String_List.List;
         begin
            Inc := Parse_Path
              (Include_Path (Get_Project_View (Kernel), True));
            Obj := Parse_Path
              (Object_Path (Get_Project_View (Kernel), True));
            String_List_Utils.String_List.Concat (Inc, Obj);
            File_Append_Directory
              (Explorer,
               Greatest_Common_Path (Inc),
               Null_Iter, 1, Get_Current_Dir, True);
            String_List_Utils.String_List.Free (Inc);
         end;
      else
         Get_Logical_Drive_Strings (Buffer, Len);

         if Len = 0 then
            File_Append_Directory
              (Explorer, (1 => Directory_Separator),
               Null_Iter, 1, Cur_Dir, True);

         else
            Last := 1;

            for J in 1 .. Len loop
               if Buffer (J) = ASCII.NUL then
                  if (Filenames_Are_Case_Sensitive
                      and then
                        Buffer (Last .. J - 1) =
                        Cur_Dir (Cur_Dir'First
                                   .. Cur_Dir'First + J - Last - 1))
                    or else
                      (not Filenames_Are_Case_Sensitive
                       and then
                         Case_Insensitive_Equal
                           (Buffer (Last .. J - 1),
                            Cur_Dir (Cur_Dir'First
                                       .. Cur_Dir'First + J - Last - 1)))
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
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Refresh;

   ----------------
   -- Parse_Path --
   ----------------

   function Parse_Path
     (Path : String) return String_List_Utils.String_List.List
   is
      First : Integer;
      Index : Integer;

      use String_List_Utils.String_List;
      Result : String_List_Utils.String_List.List;

   begin
      First := Path'First;
      Index := First + 1;

      while Index <= Path'Last loop
         if Path (Index) = Path_Separator then
            Append (Result, Path (First .. Index - 1));
            Index := Index + 1;
            First := Index;
         end if;

         Index := Index + 1;
      end loop;

      if First /= Path'Last then
         Append (Result, Path (First .. Path'Last));
      end if;

      return Result;
   end Parse_Path;

   --------------------------
   -- Greatest_Common_Path --
   --------------------------

   function Greatest_Common_Path
     (L : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;

      N : List_Node;
   begin
      if Is_Empty (L) then
         return "";
      end if;

      N := First (L);

      declare
         Greatest_Prefix        : constant String  := Data (N);
         Greatest_Prefix_First  : constant Natural := Greatest_Prefix'First;
         Greatest_Prefix_Length : Natural := Greatest_Prefix'Length;
      begin
         N := Next (N);

         while N /= Null_Node loop
            declare
               Challenger : constant String  := Data (N);
               First      : constant Natural := Challenger'First;
               Index      : Natural := 0;
               Length     : constant Natural := Challenger'Length;
            begin
               while Index < Greatest_Prefix_Length
                 and then Index < Length
                 and then
                   ((Filenames_Are_Case_Sensitive
                     and then Challenger (First + Index)
                       = Greatest_Prefix (Greatest_Prefix_First + Index))
                    or else
                      (not Filenames_Are_Case_Sensitive
                       and then To_Lower (Challenger (First + Index))
                         = To_Lower (Greatest_Prefix
                                       (Greatest_Prefix_First + Index))))
               loop
                  Index := Index + 1;
               end loop;

               Greatest_Prefix_Length := Index;
            end;

            if Greatest_Prefix_Length <= 1 then
               exit;
            end if;

            N := Next (N);
         end loop;

         if Greatest_Prefix_Length = 0 then
            return (1 => Directory_Separator);
         end if;

         while Greatest_Prefix (Greatest_Prefix'First
                                + Greatest_Prefix_Length - 1)
           /= Directory_Separator
         loop
            Greatest_Prefix_Length := Greatest_Prefix_Length - 1;
         end loop;

         return Greatest_Prefix
           (Greatest_Prefix_First
            .. Greatest_Prefix_First + Greatest_Prefix_Length - 1);
      end;
   end Greatest_Common_Path;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category
     (Category : Language_Category) return Language_Category is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      if Category in Dependency_Category
        or else Category in Construct_Category
        or else Category = Cat_Representation_Clause
        or else Category = Cat_Local_Variable
      then
         return Cat_Unknown;

         --  All subprograms are grouped together

      elsif Category in Subprogram_Explorer_Category then
         return Cat_Procedure;

      elsif Category in Type_Category then
         return Cat_Type;

      end if;

      return Category;
   end Filter_Category;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Project_Explorer_Files;
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

   ----------------------
   -- On_Open_Explorer --
   ----------------------

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files    : Project_Explorer_Files;
      Child    : MDI_Child;
   begin
      --  Start with the files view, so that if both are needed, the project
      --  view ends up on top of the files view
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Files_Record'Tag);

      if Child = null then
         Gtk_New (Files, Kernel);
         Child := Put (Get_MDI (Kernel), Files);
         Set_Title
           (Child, -"Project Explorer - File View",  -"File View");
         Set_Dock_Side (Child, Left);
         Dock_Child (Child);
      else
         Raise_Child (Child);
         Set_Focus_Child (Get_MDI (Kernel), Child);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Explorer;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget
   is
      Files    : Project_Explorer_Files;
   begin
      if Node.Tag.all = "Project_Explorer_Files" then
         Gtk_New (Files, User);
         return Gtk_Widget (Files);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Project_Explorer_Files_Record'Class then
         N := new Node;
         N.Tag := new String'("Project_Explorer_Files");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Project : constant String := '/' & (-"Project");
      N       : Node_Ptr;
   begin
      Register_Module
        (Module                  => Explorer_Files_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => "Files_View",
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null,
         MDI_Child_Tag           => Project_Explorer_Files_Record'Tag);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      --  Add a files explorer to the default desktop
      N := new Node;
      N.Tag := new String'("Project_Explorer_Files");

      Add_Default_Desktop_Item
        (Kernel, N,
         10, 10,
         300, 600,
         "File View", "Project Explorer - File View",
         Docked, Left,
         False);

      Register_Menu
        (Kernel, Project, -"File View", "", On_Open_Explorer'Access);

   end Register_Module;

end Project_Explorers_Files;

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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;

with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Event;                use Gdk.Event;

with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Enums;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Box;                  use Gtk.Box;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with String_Utils;             use String_Utils;
with String_List_Utils;        use String_List_Utils;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Pixmaps_IDE;              use Pixmaps_IDE;
with Glide_Intl;               use Glide_Intl;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;

package body Glide_Result_View is

   Non_Leaf_Color_Name : constant String := "blue";
   --  <preference> color to use for category and file names

   use String_List;

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.

   Icon_Column          : constant := 0;
   Base_Name_Column     : constant := 1;
   Absolute_Name_Column : constant := 2;
   Message_Column       : constant := 3;
   Mark_Column          : constant := 4;
   Node_Type_Column     : constant := 5;
   Line_Column          : constant := 6;
   Column_Column        : constant := 7;
   Length_Column        : constant := 8;
   Weight_Column        : constant := 9;
   Color_Column         : constant := 10;
   Button_Column        : constant := 11;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (View : access Result_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Get_Category_File
     (View          : access Result_View_Record'Class;
      Category      : String;
      File          : String;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True);
   --  Return the iter corresponding to Category, create it if
   --  necessary and if Create is True.
   --  If File is "", then the category iter will be returned.
   --  If the category was created, New_Category is set to True.

   procedure Fill_Iter
     (View          : access Result_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Base_Name     : String;
      Absolute_Name : String;
      Message       : String;
      Mark          : String;
      Line          : String;
      Column        : String;
      Length        : String;
      Pixbuf        : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf);
   --  Fill information in Iter.

   procedure Add_Location
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String;
      Line     : Positive;
      Column   : Positive;
      Length   : Natural;
      Message  : String);
   --  Add a file locaton in Category.
   --  File is an absolute file name. If File is not currently open, do not
   --  create marks for File, but add it to the list of unresolved files
   --  instead.

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
      return Boolean;
   --  Callback for the "button_press" event.

   procedure Goto_Location (Object   : access Gtk_Widget_Record'Class);
   --  Goto the selected location in the Result_View.

   procedure Remove_Category (Object   : access Gtk_Widget_Record'Class);
   --  Remove the selected category in the Result_View.

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Default context factory.

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Result_View := Result_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path;
      Success : Boolean := True;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (View.Model, Iter);

      while Success and then Get_Depth (Path) /= 3 loop
         Success := Expand_Row (View.Tree, Path, True);
         Down (Path);
      end loop;

      Iter := Get_Iter (View.Model, Path);
      Path_Free (Path);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Mark : constant String := Get_String (Model, Iter, Mark_Column);
      begin
         if Mark /= "" then
            Interpret_Command (View.Kernel, "goto_mark " & Mark);
         end if;
      end;
   end Goto_Location;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Result_View := Result_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter /= Null_Iter then
         Remove (View.Model, Iter);
      end if;
   end Remove_Category;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (View          : access Result_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Base_Name     : String;
      Absolute_Name : String;
      Message       : String;
      Mark          : String;
      Line          : String;
      Column        : String;
      Length        : String;
      Pixbuf        : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf) is
   begin
      Set (View.Model, Iter, Base_Name_Column, Base_Name);
      Set (View.Model, Iter, Absolute_Name_Column, Absolute_Name);
      Set (View.Model, Iter, Message_Column, Message);
      Set (View.Model, Iter, Mark_Column, Mark);
      Set (View.Model, Iter, Line_Column, Line);
      Set (View.Model, Iter, Column_Column, Column);
      Set (View.Model, Iter, Length_Column, Length);
      Set (View.Model, Iter, Icon_Column, C_Proxy (Pixbuf));
      Set (View.Model, Iter, Button_Column, C_Proxy (Pixbuf));

      if Line = "" then
         Set (View.Model, Iter, Weight_Column, 400);

         --  We can safely take the address of the colors, since they have the
         --  same lifespan as View and View.Model.
         Set (View.Model, Iter, Color_Column,
              Convert (View.Non_Leaf_Color'Address));
      else
         Set (View.Model, Iter, Weight_Column, 600);
         Set (View.Model, Iter, Color_Column,
              Convert (View.Leaf_Color'Address));
      end if;
   end Fill_Iter;

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (View      : access Result_View_Record'Class;
      Backwards : Boolean := False)
   is
      Iter          : Gtk_Tree_Iter;
      Path          : Gtk_Tree_Path;
      File_Path     : Gtk_Tree_Path;
      Category_Path : Gtk_Tree_Path;
      Model         : Gtk_Tree_Model;
      Success       : Boolean := True;

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Path := Get_Path (View.Model, Iter);

      --  Expand to the next path corresponding to a location node.

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (View.Tree, Path, True);
         Down (Path);
         Select_Path (Get_Selection (View.Tree), Path);
      end loop;

      if Get_Depth (Path) /= 3 then
         Path_Free (Path);

         return;
      end if;

      File_Path := Copy (Path);
      Success := Up (File_Path);

      Category_Path := Copy (File_Path);
      Success := Up (Category_Path);

      if Backwards then
         Success := Prev (Path);
      else
         Next (Path);
      end if;

      if not Success or else Get_Iter (View.Model, Path) = Null_Iter then
         if Backwards then
            Success := Prev (File_Path);
         else
            Next (File_Path);
         end if;

         if not Success
           or else Get_Iter (View.Model, File_Path) = Null_Iter
         then
            File_Path := Copy (Category_Path);
            Down (File_Path);

            if Backwards then
               while Get_Iter (View.Model, File_Path) /= Null_Iter loop
                  Next (File_Path);
               end loop;

               Success := Prev (File_Path);
            end if;
         end if;

         Success := Expand_Row (View.Tree, File_Path, True);
         Path := Copy (File_Path);
         Down (Path);

         if Backwards then
            while Get_Iter (View.Model, Path) /= Null_Iter loop
               Next (Path);
            end loop;

            Success := Prev (Path);
         end if;
      end if;

      Select_Path (Get_Selection (View.Tree), Path);
      Scroll_To_Cell (View.Tree, Path, null, True, 0.1, 0.1);
      Goto_Location (View);

      Path_Free (File_Path);
      Path_Free (Path);
      Path_Free (Category_Path);
   end Next_Item;

   -----------------------
   -- Get_Category_File --
   -----------------------

   procedure Get_Category_File
     (View          : access Result_View_Record'Class;
      Category      : String;
      File          : String;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True) is
   begin
      Category_Iter := Get_Iter_First (View.Model);
      New_Category := False;

      while Category_Iter /= Null_Iter loop
         if Get_String
           (View.Model, Category_Iter, Base_Name_Column) = Category
         then
            exit;
         end if;

         Next (View.Model, Category_Iter);
      end loop;

      if Category_Iter = Null_Iter then
         if Create then
            Append (View.Model, Category_Iter, Null_Iter);
            Fill_Iter (View, Category_Iter, Category, "", "", "", "", "", "",
                       View.Category_Pixbuf);
            New_Category := True;
         else
            return;
         end if;
      end if;

      if File = "" then
         return;
      end if;

      File_Iter := Children (View.Model, Category_Iter);

      while File_Iter /= Null_Iter loop
         if Get_String
           (View.Model, File_Iter, Absolute_Name_Column) = File
         then
            return;
         end if;

         Next (View.Model, File_Iter);
      end loop;

      --  When we reach this point, we need to create a new sub-category.

      if Create then
         Append (View.Model, File_Iter, Category_Iter);
         Fill_Iter
           (View, File_Iter, Base_Name (File), File, "", "", "", "", "",
            View.File_Pixbuf);
      end if;

      return;
   end Get_Category_File;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String;
      Line     : Positive;
      Column   : Positive;
      Length   : Natural;
      Message  : String)
   is
      Category_Iter    : Gtk_Tree_Iter;
      File_Iter        : Gtk_Tree_Iter;
      Iter             : Gtk_Tree_Iter;
      Category_Created : Boolean;
      Dummy            : Boolean;
      Path             : Gtk_Tree_Path;
   begin
      Get_Category_File
        (View, Category, File, Category_Iter, File_Iter, Category_Created);

      Append (View.Model, Iter, File_Iter);

      declare
         Output : constant String := Create_Mark
           (View.Kernel, File, Line, Column, Length);
      begin
         Fill_Iter
           (View, Iter, Image (Line) & ":" & Image (Column),
            File, Message, Output,
            Image (Line), Image (Column), Image (Length));
      end;

      if Category_Created then
         Path := Get_Path (View.Model, Category_Iter);
         Dummy := Expand_Row (View.Tree, Path, False);
         Path_Free (Path);

         declare
            MDI   : constant MDI_Window := Get_MDI (View.Kernel);
            Child : constant MDI_Child :=
              Find_MDI_Child_By_Tag (MDI, Result_View_Record'Tag);
         begin
            if Child /= null then
               Raise_Child (Child);
            end if;
         end;

         Path := Get_Path (View.Model, File_Iter);
         Dummy := Expand_Row (View.Tree, Path, True);
         Path_Free (Path);

         Path := Get_Path (View.Model, Iter);
         Select_Path (Get_Selection (View.Tree), Path);
         Scroll_To_Cell (View.Tree, Path, null, True, 0.1, 0.1);
         Goto_Location (View);
         Path_Free (Path);
      end if;
   end Add_Location;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (View : access Result_View_Record'Class) is
      Tree          : constant Gtk_Tree_View := View.Tree;
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;

      Dummy         : Gint;

   begin
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (Col, Pixbuf_Rend, False);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Button_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Base_Name_Column);
      Add_Attribute (Col, Text_Rend, "weight", Weight_Column);
      Add_Attribute (Col, Text_Rend, "foreground_gdk", Color_Column);
      Dummy := Append_Column (Tree, Col);

      Gtk_New (Col);
      Gtk_New (Text_Rend);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Message_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column               => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column      => GType_String,
         Message_Column            => GType_String,
         Base_Name_Column          => GType_String,
         Mark_Column               => GType_String,
         Line_Column               => GType_String,
         Column_Column             => GType_String,
         Length_Column             => GType_String,
         Node_Type_Column          => GType_Int,
         Weight_Column             => GType_Int,
         Color_Column              => Gdk_Color_Type,
         Button_Column             => Gdk.Pixbuf.Get_Type);
   end Columns_Types;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V : constant Result_View := Result_View (View);
   begin
      Unref (V.Category_Pixbuf);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Result_View;
      Kernel : Kernel_Handle;
      Module : Module_ID)
   is
   begin
      View := new Result_View_Record;
      Initialize (View, Kernel, Module);
   end Gtk_New;

   ------------------
   -- Context_Func --
   ------------------

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Event);
      Mitem    : Gtk_Menu_Item;

      Explorer : constant Result_View := Result_View (Object);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
      Model    : Gtk_Tree_Model;

      Result   : File_Location_Context_Access := null;

   begin
      Get_Selected (Get_Selection (Explorer.Tree), Model, Iter);
      Path := Get_Path (Model, Iter);

      if Path /= null then
         if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
            Unselect_All (Get_Selection (Explorer.Tree));
            Select_Path (Get_Selection (Explorer.Tree), Path);
         end if;

         Iter := Get_Iter (Explorer.Model, Path);

         if Get_Depth (Path) = 1 then
            Gtk_New (Mitem, -"Remove category");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Mitem, "activate",
               Gtkada.Handlers.Widget_Callback.To_Marshaller
                 (Remove_Category'Access),
               Explorer,
               After => False);
            Append (Menu, Mitem);

         elsif Get_Depth (Path) = 2 then
            Gtk_New (Mitem, -"Remove File");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Mitem, "activate",
               Gtkada.Handlers.Widget_Callback.To_Marshaller
                 (Remove_Category'Access),
               Explorer,
               After => False);
            Append (Menu, Mitem);

         elsif Get_Depth (Path) = 3 then
            Gtk_New (Mitem, -"Jump to location");
            Gtkada.Handlers.Widget_Callback.Object_Connect
              (Mitem, "activate",
               Gtkada.Handlers.Widget_Callback.To_Marshaller
                 (Goto_Location'Access),
               Explorer,
               After => False);

            Append (Menu, Mitem);

            declare
               Line   : constant Positive := Positive'Value
                 (Get_String (Model, Iter, Line_Column));
               Column : constant Positive := Positive'Value
                 (Get_String (Model, Iter, Column_Column));
               Par    : constant Gtk_Tree_Iter := Parent (Model, Iter);
               Granpa : constant Gtk_Tree_Iter := Parent (Model, Par);
               File   : constant String :=
                 Get_String (Model, Par, Absolute_Name_Column);
               Category : constant String :=
                 Get_String (Model, Granpa, Base_Name_Column);
               Message : constant String :=
                 Get_String (Model, Iter, Base_Name_Column) &
                 Get_String (Model, Iter, Message_Column);
            begin
               Result := new File_Location_Context;

               Set_File_Information
                 (Result,
                  Dir_Name (File),
                  Base_Name (File));

               Set_Location_Information
                 (Result,
                  Category,
                  Message,
                  Line,
                  Column);
            end;
         end if;

         Path_Free (Path);
      end if;

      return Selection_Context_Access (Result);
   end Context_Func;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Result_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Module_ID)
   is
      Scrolled : Gtk_Scrolled_Window;
      Success  : Boolean;
   begin
      Initialize_Hbox (View);
      View.Kernel := Kernel;

      View.Non_Leaf_Color := Parse (Non_Leaf_Color_Name);
      Alloc_Color
        (Get_Default_Colormap, View.Non_Leaf_Color, False, True, Success);

      View.Category_Pixbuf := Gdk_New_From_Xpm_Data (var_xpm);
      View.File_Pixbuf     := Gdk_New_From_Xpm_Data (mini_page_xpm);

      View.Leaf_Color := Black (Get_Default_Colormap);

      --  Initialize the tree.

      Gtk_New (View.Model, Columns_Types);
      Gtk_New (View.Tree, View.Model);
      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, False);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled,
         Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Always);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      Widget_Callback.Connect
        (View, "destroy", Widget_Callback.To_Marshaller (On_Destroy'Access));

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         View,
         After => False);

      Register_Contextual_Menu
        (View.Kernel,
         View.Tree,
         View,
         Module,
         Context_Func'Access);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Source_File   : String;
      Source_Line   : Positive;
      Source_Column : Positive;
      Message       : String;
      Length        : Natural) is
   begin
      --  Transform Source_File in an absolute file name if needed.

      if GNAT.OS_Lib.Is_Absolute_Path (Source_File) then
         Add_Location
           (View, Identifier, Source_File,
            Source_Line, Source_Column, Length, Message);

      else
         declare
            F : constant String := Find_Source_File
              (View.Kernel, Source_File, True);
         begin
            if GNAT.OS_Lib.Is_Absolute_Path (F) then
               Add_Location
                 (View, Identifier, F,
                  Source_Line, Source_Column, Length, Message);
            end if;
         end;
      end if;
   end Insert;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (View          : access Result_View_Record'Class;
      Identifier    : String)
   is
      Iter       : Gtk_Tree_Iter;
      Dummy_Iter : Gtk_Tree_Iter;
      Dummy      : Boolean;
   begin
      Get_Category_File (View, Identifier, "", Iter, Dummy_Iter, Dummy);

      if Iter /= Null_Iter then
         Remove (View.Model, Iter);
      end if;
   end Remove_Category;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
     return Boolean
   is
      Explorer : constant Result_View := Result_View (View);
      Path     : Gtk_Tree_Path;

      function Get_Path_At_Event return Gtk_Tree_Path;
      --  Return the path at which Event has occured.
      --  User must free memory associated to the returned path.

      function Get_Path_At_Event return Gtk_Tree_Path is
         X         : constant Gdouble := Get_X (Event);
         Y         : constant Gdouble := Get_Y (Event);
         Buffer_X  : Gint;
         Buffer_Y  : Gint;
         Row_Found : Boolean;
         Path      : Gtk_Tree_Path;
         Column    : Gtk_Tree_View_Column := null;

      begin
         Path := Gtk_New;
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         return Path;
      end Get_Path_At_Event;

      Success : Boolean;
   begin
      if Get_Button (Event) = 1 then
         Path := Get_Path_At_Event;

         if Path /= null then
            if Get_Depth (Path) in 1 .. 2 then
               if Row_Expanded (Explorer.Tree, Path) then
                  Success := Collapse_Row (Explorer.Tree, Path);
               else
                  Success := Expand_Row (Explorer.Tree, Path, True);
               end if;

            elsif Get_Depth (Path) = 3 then
               Select_Path (Get_Selection (Explorer.Tree), Path);
               Goto_Location (View);
            end if;

            Path_Free (Path);
         end if;

         return True;
      else

         --  If there is no selection, select the item under the cursor.
         Path := Get_Path_At_Event;

         if Path /= null then
            if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
               Unselect_All (Get_Selection (Explorer.Tree));
               Select_Path (Get_Selection (Explorer.Tree), Path);
            end if;

            Path_Free (Path);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Put_Line ("Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   ---------------------
   -- Add_Action_Item --
   ---------------------

   procedure Add_Action_Item
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Category      : String;
      File          : String;
      Line          : Natural;
      Column        : Natural;
      Message       : String;
      Action        : Action_Item)
   is
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Created       : Boolean;
      Line_Iter     : Gtk_Tree_Iter;

      pragma Unreferenced (Identifier);
   begin
      Get_Category_File (View, Category,
                         File, Category_Iter, File_Iter, Created, False);

      if Category_Iter /= Null_Iter
        and then File_Iter /= Null_Iter
      then
         Line_Iter := Children (View.Model, File_Iter);

         while File_Iter /= Null_Iter loop
            if Get_String
              (View.Model, File_Iter, Message_Column) = Message
              and then Get_String
                (View.Model, File_Iter, Line_Column) = Image (Line)
              and then Get_String
                (View.Model, File_Iter, Column_Column) = Image (Column)
            then
               Set (View.Model, File_Iter,
                    Button_Column, C_Proxy (Action.Image));
            end if;

            Next (View.Model, File_Iter);
         end loop;
      end if;
   end Add_Action_Item;

end Glide_Result_View;

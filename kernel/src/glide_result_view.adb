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
with Glib.Values;              use Glib.Values;

with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Event;                use Gdk.Event;

with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Box;                  use Gtk.Box;

with Gtkada.Handlers;          use Gtkada.Handlers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with String_List_Utils;        use String_List_Utils;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Project;     use Glide_Kernel.Project;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;

package body Glide_Result_View is

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

   --  Number_Of_Columns    : constant := 2;
   --  Number of columns in the ctree.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view.

   function Get_Category_File
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String)
     return Gtk_Tree_Iter;
   --  Return the iter corresponding to Category, create it if
   --  necessary.

   procedure Fill_Iter
     (View          : access Result_View_Record'Class;
      Iter          : Gtk_Tree_Iter;
      Base_Name     : String;
      Absolute_Name : String;
      Message       : String;
      Mark          : String;
      Line          : String;
      Column        : String);
   --  Fill information in Iter.

   procedure Add_Location
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String;
      Line     : Integer;
      Column   : Integer;
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

   procedure File_Opened
     (View : Result_View;
      File : String);
   --  Check if File is in the list of unopened files, if so
   --  remove File from the list of unopened files and create the appropriate
   --  marks.

   procedure File_Closed_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_closed" kernel signal.

   procedure File_Opened_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for the "file_opened" kernel signal.

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
      View  : constant Result_View := Result_View (Widget);
      File  : constant String := Get_String (Nth (Args, 1));

      Category  : Gtk_Tree_Iter;
      File_Iter : Gtk_Tree_Iter;
      Location  : Gtk_Tree_Iter;

      Added : Boolean := False;
   begin
      --  Browse through the tree, invalidate all iters contained in File,
      --  and if such an iter is found, add File to the list of unopened files.

      Category := Get_Iter_First (View.Model);

      while Category /= Null_Iter loop
         File_Iter := Children (View.Model, Category);

         while File_Iter /= Null_Iter loop
            Location := Children (View.Model, File_Iter);

            while Location /= Null_Iter loop
               if File =
                 Get_String (View.Model, Location, Absolute_Name_Column)
               then
                  Set (View.Model, Location, Mark_Column, "");

                  if not Added then
                     Append (View.Unopened_Files, File);
                     Added := True;
                  end if;
               end if;

               Next (View.Model, Location);
            end loop;

            Next (View.Model, File_Iter);
         end loop;

         Next (View.Model, Category);
      end loop;

   exception
      when E : others =>
         Put_Line ("Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   --------------------
   -- File_Opened_Cb --
   --------------------

   procedure File_Opened_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      View  : constant Result_View := Result_View (Widget);
      File  : constant String := Get_String (Nth (Args, 1));
      pragma Unreferenced (Kernel);
   begin
      File_Opened (View, File);
   exception
      when E : others =>
         Put_Line ("Unexpected exception: " & Exception_Information (E));
   end File_Opened_Cb;

   -----------------
   -- File_Opened --
   -----------------

   procedure File_Opened
     (View : Result_View;
      File : String)
   is
      Category  : Gtk_Tree_Iter;
      File_Iter : Gtk_Tree_Iter;
      Location  : Gtk_Tree_Iter;
   begin
      if Is_In_List (View.Unopened_Files, File) then
         Remove_From_List (View.Unopened_Files, File);

         --  Browse the list of potential marks to be created.

         Category := Get_Iter_First (View.Model);

         while Category /= Null_Iter loop
            File_Iter := Children (View.Model, Category);

            while File_Iter /= Null_Iter loop
               Location := Children (View.Model, File_Iter);

               while Location /= Null_Iter loop
                  if File =
                    Get_String (View.Model, Location, Absolute_Name_Column)
                  then
                     Set
                       (View.Model, Location, Mark_Column,
                        Interpret_Command
                          (View.Kernel,
                           "create_mark -l"
                             & Get_String (View.Model, Location, Line_Column)
                           & " -c"
                             & Get_String
                               (View.Model, Location, Column_Column)
                           & " " & File));
                  end if;

                  Next (View.Model, Location);
               end loop;

               Next (View.Model, File_Iter);
            end loop;

            Next (View.Model, Category);
         end loop;
      end if;
   end File_Opened;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Result_View := Result_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      --  If the file is not open, open it and fill all the marks.

      declare
         File : constant String :=
           Get_String (Model, Iter, Absolute_Name_Column);
      begin
         if File /= "" then
            Open_File_Editor (View.Kernel, File, 0, 0);
            File_Opened (View, File);
         end if;
      end;

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
      Column        : String) is
   begin
      Set (View.Model, Iter, Base_Name_Column, Base_Name);
      Set (View.Model, Iter, Absolute_Name_Column, Absolute_Name);
      Set (View.Model, Iter, Message_Column, Message);
      Set (View.Model, Iter, Mark_Column, Mark);
      Set (View.Model, Iter, Line_Column, Line);
      Set (View.Model, Iter, Column_Column, Column);
   end Fill_Iter;

   -----------------------
   -- Get_Category_File --
   -----------------------

   function Get_Category_File
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String)
     return Gtk_Tree_Iter
   is
      Iter  : Gtk_Tree_Iter := Get_Iter_First (View.Model);
      Child : Gtk_Tree_Iter;
   begin
      while Iter /= Null_Iter loop
         if Get_String
           (View.Model, Iter, Base_Name_Column) = Category
         then
            exit;
         end if;

         Next (View.Model, Iter);
      end loop;

      if Iter = Null_Iter then
         Append (View.Model, Iter, Null_Iter);
         Fill_Iter (View, Iter, Category, "", "", "", "", "");
      end if;

      Child := Children (View.Model, Iter);

      while Child /= Null_Iter loop
         if Get_String
           (View.Model, Child, Absolute_Name_Column) = File
         then
            return Child;
         end if;

         Next (View.Model, Child);
      end loop;

      --  When we reach this point, we need to create a new sub-category.

      Append (View.Model, Child, Iter);
      Fill_Iter (View, Child, Base_Name (File), File, "", "", "", "");

      return Child;
   end Get_Category_File;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (View     : access Result_View_Record'Class;
      Category : String;
      File     : String;
      Line     : Integer;
      Column   : Integer;
      Message  : String)
   is
      Category_Iter : Gtk_Tree_Iter;
      Iter          : Gtk_Tree_Iter;
   begin
      Category_Iter := Get_Category_File (View, Category, File);

      Append (View.Model, Iter, Category_Iter);

      --  If File is open, create a mark, otherwise add File to the list
      --  of unopened files.

      if Is_Open (View.Kernel, File) then
         declare
            Output : constant String := Create_Mark
              (View.Kernel, File, Line, Column);
         begin
            Fill_Iter
              (View, Iter, Line'Img & ":" & Column'Img, File, Message, Output,
               Line'Img, Column'Img);
         end;
      else
         if not Is_In_List (View.Unopened_Files, File) then
            Append (View.Unopened_Files, File);
         end if;

         Fill_Iter
           (View, Iter, Line'Img & ":" & Column'Img, File, Message, "",
            Line'Img, Column'Img);
      end if;
   end Add_Location;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;
      Dummy         : Gint;

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

      Gtk_New (Col);
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
         Node_Type_Column          => GType_Int);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Result_View;
      Kernel : Kernel_Handle := null)
   is
   begin
      View := new Result_View_Record;
      Initialize (View, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Result_View_Record'Class;
      Kernel : Kernel_Handle := null)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Hbox (View);
      View.Kernel := Kernel;

      --  Initialize the tree.

      Gtk_New (View.Model, Columns_Types);
      Gtk_New (View.Tree, View.Model);
      Set_Column_Types (View.Tree);
      Set_Headers_Visible (View.Tree, False);

      Gtk_New (Scrolled);
      Add (Scrolled, View.Tree);

      Add (View, Scrolled);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press'Access),
         View,
         After => False);

      Kernel_Callback.Object_Connect
        (View.Kernel, File_Closed_Signal,
         File_Closed_Cb'Access,
         View,
         View.Kernel);

      Kernel_Callback.Object_Connect
        (View.Kernel, File_Edited_Signal,
         File_Opened_Cb'Access,
         View,
         View.Kernel);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Source_File   : String;
      Source_Line   : Natural;
      Source_Column : Natural;
      Message       : String) is
   begin
      --  Transform Source_File in an absolute file name if needed.

      if GNAT.OS_Lib.Is_Absolute_Path (Source_File) then
         Add_Location
           (View, Identifier, Source_File,
            Source_Line, Source_Column, Message);

      else
         declare
            F : constant String := Find_Source_File
              (View.Kernel, Source_File, True);
         begin
            if GNAT.OS_Lib.Is_Absolute_Path (F) then
               Add_Location
                 (View, Identifier, F,
                  Source_Line, Source_Column, Message);
            end if;
         end;
      end if;
   end Insert;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event)
     return Boolean
   is
      Menu     : Gtk_Menu;
      Mitem    : Gtk_Menu_Item;

      Explorer : constant Result_View := Result_View (View);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

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
                  Success := Expand_Row (Explorer.Tree, Path, False);
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

            Iter := Get_Iter (Explorer.Model, Path);

            if Get_Depth (Path) in 1 .. 3 then
               Gtk_New (Menu);
            end if;

            if Get_Depth (Path) = 1 then
               Gtk_New (Mitem, "Remove category");
               Gtkada.Handlers.Widget_Callback.Object_Connect
                 (Mitem, "activate",
                  Gtkada.Handlers.Widget_Callback.To_Marshaller
                    (Remove_Category'Access),
                  Explorer,
                  After => False);
               Append (Menu, Mitem);

            elsif Get_Depth (Path) = 2 then
               Gtk_New (Mitem, "Remove File");
               Gtkada.Handlers.Widget_Callback.Object_Connect
                 (Mitem, "activate",
                  Gtkada.Handlers.Widget_Callback.To_Marshaller
                    (Remove_Category'Access),
                  Explorer,
                  After => False);
               Append (Menu, Mitem);

            elsif Get_Depth (Path) = 3 then
               Gtk_New (Mitem, "Jump to location");
               Gtkada.Handlers.Widget_Callback.Object_Connect
                 (Mitem, "activate",
                  Gtkada.Handlers.Widget_Callback.To_Marshaller
                    (Goto_Location'Access),
                  Explorer,
                  After => False);

               Append (Menu, Mitem);
            end if;

            if Get_Depth (Path) in 1 .. 3 then
               Grab_Focus (Explorer);
               Show_All (Menu);
               Popup (Menu);
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

end Glide_Result_View;

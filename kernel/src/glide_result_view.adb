-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Glib.Convert;
with Glib.Values;              use Glib.Values;
with Glib.Object;              use Glib.Object;

with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Event;                use Gdk.Event;

with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
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

with GNAT.OS_Lib;

with String_Utils;             use String_Utils;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Pixmaps_IDE;              use Pixmaps_IDE;
with Glide_Intl;               use Glide_Intl;
with VFS;                      use VFS;

with Traces;                   use Traces;
with Commands;                 use Commands;
with Basic_Types;              use Basic_Types;
with System;

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Conversion;

package body Glide_Result_View is

   Me : constant Debug_Handle := Create ("Glide_Result_View");

   Non_Leaf_Color_Name : constant String := "blue";
   --  <preference> color to use for category and file names

   Auto_Jump_To_First : Param_Spec_Boolean;
   --  Preferences local to this module

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
   Action_Column        : constant := 12;
   Highlight_Column     : constant := 13;
   Highlight_Category_Column : constant := 14;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types (View : access Result_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view.

   procedure Get_Category_File
     (View          : access Result_View_Record'Class;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True);
   --  Return the iter corresponding to Category, create it if
   --  necessary and if Create is True.
   --  If File is "", then the category iter will be returned.
   --  If the category was created, New_Category is set to True.

   procedure Fill_Iter
     (View               : access Result_View_Record'Class;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : VFS.Virtual_File;
      Message            : String;
      Mark               : String;
      Line               : String;
      Column             : String;
      Length             : String;
      Highlighting       : Boolean;
      Highlight_Category : String;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf);
   --  Fill information in Iter.
   --  Base_Name can be left to the empty string, it will then be computed
   --  automatically from Absolute_Name.

   procedure Add_Location
     (View               : access Result_View_Record'Class;
      Category           : String;
      File               : VFS.Virtual_File;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural;
      Highlight          : Boolean;
      Message            : String;
      Highlight_Category : String);
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

   procedure Remove_Category_Or_File_Iter
     (View : Result_View;
      Iter : in out Gtk_Tree_Iter);
   --  Clear all the marks and highlightings in file or category.

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

   function Create_Mark
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String;
   --  Create a mark for Filename, at position given by Line, Column, with
   --  length Length.
   --  Return the identifier corresponding to the mark that has been created.

   procedure Highlight_Line
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename           : VFS.Virtual_File;
      Line               : Natural := 1;
      Highlight_Category : String;
      Highlight          : Boolean := True);
   --  Highlight the line with the corresponding category.
   --  If Highlight is set to False, remove the highlighting.
   --  If Line = 0, highlight / unhighlight all lines in file.

   procedure On_Row_Expanded
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "row_expanded" signal.

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : VFS.Virtual_File;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String
   is
      Args : GNAT.OS_Lib.Argument_List :=
        (1 => new String'(Full_Name (Filename).all),
         2 => new String'(Image (Line)),
         3 => new String'(Image (Column)),
         4 => new String'(Image (Length)));
      Result : constant String :=
        Execute_GPS_Shell_Command (Kernel, "create_mark", Args);
   begin
      Basic_Types.Free (Args);
      return Result;
   end Create_Mark;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename           : VFS.Virtual_File;
      Line               : Natural := 1;
      Highlight_Category : String;
      Highlight          : Boolean := True)
   is
      Args    : GNAT.OS_Lib.Argument_List (1 .. 3) :=
        (1 => new String'(Full_Name (Filename).all),
         2 => new String'(Highlight_Category),
         3 => new String'(Image (Line)));
      Command : GNAT.OS_Lib.String_Access;
   begin
      if Highlight then
         Command := new String'("highlight");
      else
         Command := new String'("unhighlight");
      end if;

      if Line = 0 then
         Execute_GPS_Shell_Command (Kernel, Command.all, Args (1 .. 2));
      else
         Execute_GPS_Shell_Command (Kernel, Command.all, Args);
      end if;

      Basic_Types.Free (Args);
      GNAT.OS_Lib.Free (Command);
   end Highlight_Line;

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

      Path := Get_Path (View.Tree.Model, Iter);

      while Success and then Get_Depth (Path) /= 3 loop
         Success := Expand_Row (View.Tree, Path, False);
         Down (Path);
      end loop;

      Iter := Get_Iter (View.Tree.Model, Path);
      Path_Free (Path);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Mark : constant String := Get_String (Model, Iter, Mark_Column);
         Args : GNAT.OS_Lib.Argument_List := (1 => new String'(Mark));
      begin
         if Mark /= "" then
            Execute_GPS_Shell_Command (View.Kernel, "goto_mark", Args);
         end if;
         Free (Args);
      end;
   end Goto_Location;

   ----------------------------------
   -- Remove_Category_Or_File_Iter --
   ----------------------------------

   procedure Remove_Category_Or_File_Iter
     (View : Result_View;
      Iter : in out Gtk_Tree_Iter)
   is
      File_Iter : Gtk_Tree_Iter;
      File_Path : Gtk_Tree_Path;
      Loc_Iter  : Gtk_Tree_Iter;
      Category  : GNAT.OS_Lib.String_Access;

      Removing_Category : Boolean := False;
      --  Indicates whether we are removing a whole category or just a file.
   begin
      --  Unhighight all the lines and remove all marks in children of the
      --  category / file.

      if Iter = Null_Iter then
         return;
      end if;

      Iter_Copy (Iter, File_Iter);

      File_Path := Get_Path (View.Tree.Model, File_Iter);

      if Get_Depth (File_Path) = 1 then
         Category  := new String'
           (Get_String (View.Tree.Model, File_Iter, Base_Name_Column));
         File_Iter := Children (View.Tree.Model, File_Iter);
         Removing_Category := True;
      elsif Get_Depth (File_Path) = 2 then
         Category := new String'
           (Get_String
              (View.Tree.Model,
               Parent (View.Tree.Model, File_Iter),
               Base_Name_Column));
      else
         Path_Free (File_Path);
         return;
      end if;

      Path_Free (File_Path);

      while File_Iter /= Null_Iter loop
         --  Delete the marks corresponding to all locations in this file.
         Loc_Iter := Children (View.Tree.Model, File_Iter);

         while Loc_Iter /= Null_Iter loop
            declare
               Mark : aliased String :=
                 Get_String (View.Tree.Model, Loc_Iter, Mark_Column);
               Args : constant GNAT.OS_Lib.Argument_List :=
                 (1 => Mark'Unchecked_Access);

            begin
               if Mark /= "" then
                  Execute_GPS_Shell_Command (View.Kernel, "delete_mark", Args);
               end if;

               Highlight_Line
                 (View.Kernel,
                  Create
                    (Full_Filename => Get_String
                       (View.Tree.Model, File_Iter, Absolute_Name_Column)),
                  0,
                  Get_String
                    (View.Tree.Model, Loc_Iter, Highlight_Category_Column),
                  False);
            end;

            Next (View.Tree.Model, Loc_Iter);
         end loop;

         exit when not Removing_Category;

         Next (View.Tree.Model, File_Iter);
      end loop;

      GNAT.OS_Lib.Free (Category);
      Remove (View.Tree.Model, Iter);
   end Remove_Category_Or_File_Iter;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category (Object   : access Gtk_Widget_Record'Class) is
      View  : constant Result_View := Result_View (Object);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

   begin
      Get_Selected (Get_Selection (View.Tree), Model, Iter);
      Remove_Category_Or_File_Iter (View, Iter);
   end Remove_Category;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (View               : access Result_View_Record'Class;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : VFS.Virtual_File;
      Message            : String;
      Mark               : String;
      Line               : String;
      Column             : String;
      Length             : String;
      Highlighting       : Boolean;
      Highlight_Category : String;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf)
   is
      function To_Proxy is new
        Ada.Unchecked_Conversion (System.Address, C_Proxy);

      Model : constant Gtk_Tree_Store := View.Tree.Model;
   begin
      if Base_Name = "" then
         Set
           (Model, Iter, Base_Name_Column, VFS.Base_Name (Absolute_Name));
      else
         Set (Model, Iter, Base_Name_Column, Base_Name);
      end if;

      Set (Model, Iter, Absolute_Name_Column, Full_Name (Absolute_Name).all);
      Set (Model, Iter, Message_Column,
           Glib.Convert.Locale_To_UTF8 (Message));
      Set (Model, Iter, Mark_Column, Mark);
      Set (Model, Iter, Line_Column, Line);
      Set (Model, Iter, Column_Column, Column);
      Set (Model, Iter, Length_Column, Length);
      Set (Model, Iter, Icon_Column, C_Proxy (Pixbuf));
      Set (Model, Iter, Highlight_Column, Highlighting);
      Set (Model, Iter, Highlight_Category_Column, Highlight_Category);

      if Line = "" then
         Set (Model, Iter, Weight_Column, 400);

         --  We can safely take the address of the colors, since they have the
         --  same lifespan as View and View.Model.
         Set (Model, Iter, Color_Column,
              To_Proxy (View.Non_Leaf_Color'Address));
      else
         Set (Model, Iter, Weight_Column, 600);
         Set (Model, Iter, Color_Column,
              To_Proxy (View.Leaf_Color'Address));
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

      Path := Get_Path (View.Tree.Model, Iter);

      --  Expand to the next path corresponding to a location node.

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (View.Tree, Path, False);
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

      if not Success or else Get_Iter (View.Tree.Model, Path) = Null_Iter then
         if Backwards then
            Success := Prev (File_Path);
         else
            Next (File_Path);
         end if;

         if not Success
           or else Get_Iter (View.Tree.Model, File_Path) = Null_Iter
         then
            File_Path := Copy (Category_Path);
            Down (File_Path);

            if Backwards then
               while Get_Iter (View.Tree.Model, File_Path) /= Null_Iter loop
                  Next (File_Path);
               end loop;

               Success := Prev (File_Path);
            end if;
         end if;

         Success := Expand_Row (View.Tree, File_Path, False);
         Path := Copy (File_Path);
         Down (Path);

         if Backwards then
            while Get_Iter (View.Tree.Model, Path) /= Null_Iter loop
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
      H_Category    : String;
      File          : VFS.Virtual_File;
      Category_Iter : out Gtk_Tree_Iter;
      File_Iter     : out Gtk_Tree_Iter;
      New_Category  : out Boolean;
      Create        : Boolean := True)
   is
      Category_UTF8 : constant String :=
        Glib.Convert.Locale_To_UTF8 (Category);

   begin
      Category_Iter := Get_Iter_First (View.Tree.Model);
      New_Category := False;

      while Category_Iter /= Null_Iter loop
         if Get_String
           (View.Tree.Model, Category_Iter, Base_Name_Column) = Category_UTF8
         then
            exit;
         end if;

         Next (View.Tree.Model, Category_Iter);
      end loop;

      if Category_Iter = Null_Iter then
         if Create then
            Append (View.Tree.Model, Category_Iter, Null_Iter);
            Fill_Iter (View, Category_Iter, Category_UTF8, VFS.No_File,
                       "", "", "", "", "", False,
                       H_Category, View.Category_Pixbuf);
            New_Category := True;
         else
            return;
         end if;
      end if;

      if File = VFS.No_File then
         return;
      end if;

      File_Iter := Children (View.Tree.Model, Category_Iter);

      while File_Iter /= Null_Iter loop
         if Get_String (View.Tree.Model, File_Iter, Absolute_Name_Column) =
           Full_Name (File).all
         then
            return;
         end if;

         Next (View.Tree.Model, File_Iter);
      end loop;

      --  When we reach this point, we need to create a new sub-category.

      if Create then
         Append (View.Tree.Model, File_Iter, Category_Iter);
         Fill_Iter
           (View, File_Iter, "", File, "", "", "", "", "",
            False, H_Category, View.File_Pixbuf);
      end if;

      return;
   end Get_Category_File;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (View               : access Result_View_Record'Class;
      Category           : String;
      File               : VFS.Virtual_File;
      Line               : Positive;
      Column             : Positive;
      Length             : Natural;
      Highlight          : Boolean;
      Message            : String;
      Highlight_Category : String)
   is
      Category_Iter    : Gtk_Tree_Iter;
      File_Iter        : Gtk_Tree_Iter;
      Iter             : Gtk_Tree_Iter;
      Category_Created : Boolean;
      Dummy            : Boolean;
      pragma Unreferenced (Dummy);

      Path               : Gtk_Tree_Path;
   begin
      Get_Category_File
        (View, Category, Highlight_Category,
         File, Category_Iter, File_Iter, Category_Created);

      --  Check whether the same item already exists.

      if Category_Iter /= Null_Iter
        and then File_Iter /= Null_Iter
      then
         Iter := Children (View.Tree.Model, File_Iter);

         while Iter /= Null_Iter loop
            if Get_String (View.Tree.Model, Iter, Line_Column) = Image (Line)
              and then Get_String
                (View.Tree.Model, Iter, Column_Column) = Image (Column)
              and then Get_String
                (View.Tree.Model, Iter, Message_Column) = Message
            then
               return;
            end if;

            Next (View.Tree.Model, Iter);
         end loop;
      end if;

      Append (View.Tree.Model, Iter, File_Iter);

      if Highlight then
         Highlight_Line (View.Kernel, File, Line, Highlight_Category);
      end if;

      declare
         Output : constant String := Create_Mark
           (View.Kernel, File, Line, Column, Length);
      begin
         Fill_Iter
           (View, Iter,
            Image (Line) & ":" & Image (Column), File,
            Message, Output,
            Image (Line), Image (Column), Image (Length), Highlight,
            Highlight_Category);
      end;

      if Category_Created then
         Path := Get_Path (View.Tree.Model, Category_Iter);
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

         Path := Get_Path (View.Tree.Model, File_Iter);
         Dummy := Expand_Row (View.Tree, Path, False);
         Path_Free (Path);

         Path := Get_Path (View.Tree.Model, Iter);
         Select_Path (Get_Selection (View.Tree), Path);
         Scroll_To_Cell (View.Tree, Path, null, True, 0.1, 0.1);

         if Get_Pref (View.Kernel, Auto_Jump_To_First) then
            Goto_Location (View);
         end if;

         Path_Free (Path);
      end if;
   end Add_Location;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (View : access Result_View_Record'Class) is
      Tree          : constant Tree_View := View.Tree;
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend   : Gtk_Cell_Renderer_Pixbuf;

      Dummy         : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (View.Action_Column);
      Gtk_New (Pixbuf_Rend);
      Pack_Start (View.Action_Column, Pixbuf_Rend, False);
      Add_Attribute (View.Action_Column, Pixbuf_Rend, "pixbuf", Button_Column);
      Dummy := Append_Column (Tree, View.Action_Column);

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
      Set_Expander_Column (Tree, Col);

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
         Button_Column             => Gdk.Pixbuf.Get_Type,
         Action_Column             => GType_Pointer,
         Highlight_Column          => GType_Boolean,
         Highlight_Category_Column => GType_String);
   end Columns_Types;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V    : constant Result_View := Result_View (View);
      Iter : Gtk_Tree_Iter;
   begin
      --  Remove all categories.

      Iter := Get_Iter_First (V.Tree.Model);

      while Iter /= Null_Iter loop
         Remove_Category_Or_File_Iter (V, Iter);
         Iter := Get_Iter_First (V.Tree.Model);
      end loop;

      Unref (V.Category_Pixbuf);
      Unref (V.File_Pixbuf);
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

      Result   : Message_Context_Access := null;

   begin
      Get_Selected (Get_Selection (Explorer.Tree), Model, Iter);

      if Model = null then
         return null;
      end if;

      Path := Get_Path (Model, Iter);

      if Path = null then
         return null;
      end if;

      if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
         Unselect_All (Get_Selection (Explorer.Tree));
         Select_Path (Get_Selection (Explorer.Tree), Path);
      end if;

      Iter := Get_Iter (Explorer.Tree.Model, Path);

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
            File   : constant Virtual_File := Create
              (Full_Filename => Get_String (Model, Par, Absolute_Name_Column));
         begin
            Result := new Message_Context;
            Set_File_Information
              (Result,
               File,
               Line => Line,
               Column => Column);
            Set_Message_Information
              (Result,
               Category => Get_String (Model, Granpa, Base_Name_Column),
               Message  => Get_String (Model, Iter, Message_Column));
         end;
      end if;

      Path_Free (Path);
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

      Gtk_New (View.Tree, Columns_Types);
      Set_Column_Types (View);
      Set_Headers_Visible (View.Tree, False);

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Always);
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

      Widget_Callback.Connect
        (View.Tree, "row_expanded", On_Row_Expanded'Access);

      Register_Contextual_Menu
        (View.Kernel,
         View.Tree,
         View,
         Module,
         Context_Func'Access);
   end Initialize;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Tree : constant Gtk_Tree_View := Gtk_Tree_View (Widget);
      Iter : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 1), Iter);
      Scroll_To_Cell
        (Tree, Get_Path (Get_Model (Tree), Iter), null, True, 0.1, 0.1);
   end On_Row_Expanded;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View               : access Result_View_Record'Class;
      Identifier         : String;
      Source_File        : VFS.Virtual_File;
      Source_Line        : Positive;
      Source_Column      : Positive;
      Message            : String;
      Length             : Natural;
      Highlight          : Boolean := False;
      Highlight_Category : String := "") is
   begin
      --  Transform Source_File in an absolute file name if needed.

      if Is_Absolute_Path (Source_File) then
         Add_Location
           (View, Identifier, Source_File,
            Source_Line, Source_Column, Length, Highlight, Message,
            Highlight_Category);
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
      Get_Category_File
        (View, Identifier, "", VFS.No_File, Iter, Dummy_Iter, Dummy);
      Remove_Category_Or_File_Iter (Result_View (View), Iter);
   end Remove_Category;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View     : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      Explorer : constant Result_View := Result_View (View);
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Success   : Command_Return_Type;
      pragma Unreferenced (Success);

   begin
      if Get_Button (Event) = 1 then
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

         if Path /= null then
            if Get_Depth (Path) /= 3 then
               Path_Free (Path);
               return False;
            else
               if Column = Explorer.Action_Column then
                  declare
                     Value   : GValue;
                     Iter    : Gtk_Tree_Iter;
                     Action  : Action_Item;

                  begin
                     Iter := Get_Iter (Explorer.Tree.Model, Path);
                     Get_Value
                       (Explorer.Tree.Model, Iter, Action_Column, Value);
                     Action := To_Action_Item (Get_Address (Value));

                     if Action /= null
                       and then Action.Associated_Command /= null
                     then
                        Success := Execute (Action.Associated_Command);
                     end if;

                     Unset (Value);
                  end;
               end if;

               Select_Path (Get_Selection (Explorer.Tree), Path);
               Goto_Location (View);
            end if;

            Path_Free (Path);
         end if;

         return True;

      else
         Grab_Focus (Explorer.Tree);

         --  If there is no selection, select the item under the cursor.
         Get_Path_At_Pos
           (Explorer.Tree,
            Gint (X),
            Gint (Y),
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Row_Found);

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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

   ---------------------
   -- Add_Action_Item --
   ---------------------

   procedure Add_Action_Item
     (View          : access Result_View_Record'Class;
      Identifier    : String;
      Category      : String;
      H_Category    : String;
      File          : VFS.Virtual_File;
      Line          : Natural;
      Column        : Natural;
      Message       : String;
      Action        : Action_Item)
   is
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Created       : Boolean;
      Line_Iter     : Gtk_Tree_Iter;

      Value         : GValue;
      Old_Action    : Action_Item;

      pragma Unreferenced (Identifier);
   begin
      Trace (Me, "Add_Action_Item: "
             & Full_Name (File).all
             & ' ' & Category & Line'Img & Column'Img
             & ' ' & Message);

      Get_Category_File
        (View, Category, H_Category,
         File, Category_Iter, File_Iter, Created, False);

      if Category_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: Category " & H_Category & " not found");
      end if;

      if File_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: File " & Full_Name (File).all
                & " not found");
      end if;

      if Category_Iter /= Null_Iter
        and then File_Iter /= Null_Iter
      then
         Line_Iter := Children (View.Tree.Model, File_Iter);

         while Line_Iter /= Null_Iter loop
            if Get_String
              (View.Tree.Model, Line_Iter, Message_Column) = Message
              and then Get_String
                (View.Tree.Model, Line_Iter, Line_Column) = Image (Line)
              and then Get_String
                (View.Tree.Model, Line_Iter, Column_Column) = Image (Column)
            then
               if Action = null then
                  Set (View.Tree.Model, Line_Iter,
                       Button_Column, C_Proxy (Null_Pixbuf));

                  Get_Value (View.Tree.Model, Line_Iter, Action_Column, Value);
                  Old_Action := To_Action_Item (Get_Address (Value));

                  if Old_Action /= null then
                     Free (Old_Action);
                  end if;

                  Set_Address (Value, System.Null_Address);
                  Set_Value (View.Tree.Model, Line_Iter,
                             Action_Column, Value);
                  Unset (Value);

               else
                  Set (View.Tree.Model, Line_Iter,
                       Button_Column, C_Proxy (Action.Image));
                  Init (Value, GType_Pointer);
                  Set_Address (Value, To_Address (Action));
                  Set_Value (View.Tree.Model, Line_Iter,
                             Action_Column, Value);
                  Unset (Value);
               end if;

               return;
            end if;

            Next (View.Tree.Model, Line_Iter);
         end loop;
      end if;

      Trace (Me, "Add_Action_Item: entry not found");
   end Add_Action_Item;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Auto_Jump_To_First := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Auto-Jump-To-First",
           Default => True,
           Blurb   =>
             -("Whether GPS should automatically jump to the first location"
               & " when entries are added to the Location window (error"
               & " messages, find results, ...)"),
           Nick    => -"Jump to first location"));
      Register_Property
        (Kernel, Param_Spec (Auto_Jump_To_First), -"General");
   end Register_Module;

end Glide_Result_View;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Glib.Object;
with Glib.Values;
with Glib_Values_Utils;      use Glib_Values_Utils;

with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_Sortable;
with Gtk.Tree_Store;         use Gtk.Tree_Store;

with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;    use GNATCOLL.VFS.GtkAda;
with GNATCOLL.Xref;

with Commands;
with GPS.Editors.GtkAda;
with GPS.Editors.Line_Information;
pragma Unreferenced (GPS.Editors.Line_Information);
--  Required to access to members of Line_Information_Record.
with Default_Preferences;    use Default_Preferences;
with String_Utils;           use String_Utils;

package body GPS.Location_View.Listener is

   use type Commands.Command_Access;
   use type Glib.Gint;
   use type Glib.Main.G_Source_Id;
   use type GNATCOLL.Xref.Visible_Column;

   Column_Types : Glib.GType_Array
     (0 .. Listener_Columns'Pos (Listener_Columns'Last));

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

   function To_Address is
     new Ada.Unchecked_Conversion
       (GPS.Kernel.Messages.Action_Item, System.Address);

   package Classic_Tree_Model_Sources is
     new Glib.Main.Generic_Sources (Classic_Tree_Model);

   procedure Update_Background_Color
     (Self    : not null access Locations_Listener'Class;
      Message : not null access Abstract_Message'Class);
   --  Modify the background color of the file and category related to message

   procedure Refresh_Background_Color (Self : Classic_Tree_Model);
   --  Refresh the background color after the deletions are finished

   procedure Copy_Background
     (Model : Classic_Tree_Model;
      From  : Gtk.Tree_Model.Gtk_Tree_Iter;
      To    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Do a copy of the background color of From in To

   function Heaviest_Non_Deleted_Iter
     (Model      : Classic_Tree_Model;
      Start_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Get the heaviest iter not in the list of removed messages

   procedure Find_Category
     (Self     : not null access Locations_Listener'Class;
      Category : String;
      Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified category and set Iter to it's iterator in tree.

   procedure Find_File
     (Self          : not null access Locations_Listener'Class;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified file in specified category.

   procedure Find_Message
     (Self          : not null access Locations_Listener'Class;
      Message       : not null access Abstract_Message'Class;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified message.

   function Get
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return GNATCOLL.VFS.Virtual_File;
   --  Gets value of specifid cells

   type Compare_Functions is
     array (Positive range <>) of Gtk_Tree_Iter_Compare_Func;

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter;
      Funcs : Compare_Functions) return Glib.Gint;
   --  General compare function, it compares A and B using Funcs till
   --  nonequvalence is reported or end of Funcs is reached.

   function Compare_In_Base_Name_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint;
   --  Compare files of A and B in base name order.

   function Compare_In_Line_Column_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint;
   --  Compare message of A and B in line:column order.

   function Compare_In_Weight_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint;
   --  Compare nodes A and B in weight order.

   function Compare_In_Path_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint;
   --  Compare A and B in path order.

   function Compare_Nodes
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint;
   --  Compares rows

   procedure Disable_Sorting
     (Self : not null access Classic_Tree_Model_Record'Class);
   --  Disables sorting by GtkTreeStore and set up idle handler to reenable it

   function On_Idle (Self : Classic_Tree_Model) return Boolean;
   --  Restore sorting of items in the model

   Location_Padding : constant := 10;
   --  Size of field for line:column location information in view

   Non_Leaf_Color_Name : constant String := "blue";
   --  Name of the color to be used for category and file names

   procedure Insert_With_Values
     (Tree_Store : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record;
      Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Glib.Gint;
      Columns    : Glib.Gint_Array;
      Values     : Glib.Values.GValue_Array);
   --  ??? Must be moved to GtkAda

   --------------------
   -- Category_Added --
   --------------------

   overriding procedure Category_Added
     (Self                     : not null access Locations_Listener;
      Category                 : Ada.Strings.Unbounded.Unbounded_String;
      Allow_Auto_Jump_To_First : Boolean)
   is
      pragma Unreferenced (Allow_Auto_Jump_To_First);

      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Mark : Glib.Values.GValue;
   begin
      Self.Model.Disable_Sorting;

      Self.Model.Append (Iter, Gtk.Tree_Model.Null_Iter);

      Glib.Values.Init (Mark, GPS.Editors.GtkAda.Get_Editor_Mark_Type);
      GPS.Editors.GtkAda.Set_Mark (Mark, GPS.Editors.Nil_Editor_Mark);

      Set_And_Clear
        (Gtk_Tree_Store (Self.Model), Iter,
         (-Category_Column,
          -Weight_Column,
          -File_Column,
          -Line_Column,
          -Column_Column,
          -Text_Column,
          -Node_Icon_Name_Column,
          -Node_Markup_Column,
          -Node_Tooltip_Column,
          -Node_Mark_Column,
          -Action_Command_Column,
          -Action_Tooltip_Column,
          -Number_Of_Children_Column,
          -Sort_Order_Hint_Column,
          -Message_Column),
         (1  => As_String  (To_String (Category)),
          2  => As_Int     (0),
          3  => As_File    (No_File),
          4  => As_Int     (-1),
          5  => As_Int     (-1),
          6  => As_String  (To_String (Category)),
          7  => As_String  ("gps-emblem-category"),
          8  => As_String  (To_String (Category)),
          9  => As_String  (To_String (Category)),
          10 => Mark,
          11 => As_Pointer (System.Null_Address),
          12 => As_String  (To_String (Category)),
          13 => As_Int     (0),
          14 => As_Int
            (Sort_Order_Hint'Pos
               (Self.Kernel.Get_Messages_Container.Get_Sort_Order_Hint
                    (To_String (Category)))),
          15 => As_Pointer (System.Null_Address)));
   end Category_Added;

   ----------------------
   -- Category_Removed --
   ----------------------

   overriding procedure Category_Removed
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String)
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Self.Model.Disable_Sorting;

      Self.Find_Category (To_String (Category), Iter);
      Self.Model.Remove (Iter);
   end Category_Removed;

   -------------
   -- Compare --
   -------------

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter;
      Funcs : Compare_Functions) return Glib.Gint
   is
      Result : Glib.Gint := 0;

   begin
      for J in Funcs'Range loop
         Result := Funcs (J) (Model, A, B);

         exit when Result /= 0;
      end loop;

      return Result;
   end Compare;

   -------------------
   -- Compare_Nodes --
   -------------------

   function Compare_Nodes
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint
   is
      Self  : constant Classic_Tree_Model :=
        Classic_Tree_Model (Gtk_Tree_Store'(-Model));
      Depth : constant Glib.Gint := Self.Iter_Depth (A);
      Hint  : Sort_Order_Hint;

   begin
      if Depth = 1 then
         --  File level node

         case Self.File_Order is
            when Category_Default_Sort =>
               Hint := Sort_Order_Hint'Val
                 (Get_Int (Model, A, -Sort_Order_Hint_Column));

               case Self.Messages_Order is
                  when By_Weight =>
                     case Hint is
                        when Chronological =>
                           return Compare
                             (Model, A, B,
                              (Compare_In_Weight_Order'Access,
                               Compare_In_Path_Order'Access));

                        when Sort_Order_Hint'(Alphabetical) =>
                           return Compare
                             (Model, A, B,
                              (Compare_In_Weight_Order'Access,
                               Compare_In_Base_Name_Order'Access,
                               Compare_In_Path_Order'Access));
                     end case;

                  when others =>
                     case Hint is
                        when Chronological =>
                           return Compare_In_Path_Order (Model, A, B);

                        when Sort_Order_Hint'(Alphabetical) =>
                           return Compare
                             (Model, A, B,
                              (Compare_In_Base_Name_Order'Access,
                               Compare_In_Path_Order'Access));
                     end case;
               end case;

            when Alphabetical =>
               return Compare
                 (Model, A, B,
                  (Compare_In_Base_Name_Order'Access,
                   Compare_In_Path_Order'Access));
         end case;

      elsif Depth = 2 then
         --  Message level node

         case Self.Messages_Order is
            when By_Location =>
               return Compare
                 (Model, A, B,
                  (Compare_In_Line_Column_Order'Access,
                   Compare_In_Path_Order'Access));

            when By_Weight =>
               return Compare
                 (Model, A, B,
                  (Compare_In_Weight_Order'Access,
                   Compare_In_Line_Column_Order'Access,
                   Compare_In_Path_Order'Access));
         end case;

      else
         --  GtkTreeSortModel breaks underlying order of equal rows, so return
         --  result of compare of last indices to save underlying order.

         return Compare_In_Path_Order (Model, A, B);
      end if;
   end Compare_Nodes;

   --------------------------------
   -- Compare_In_Base_Name_Order --
   --------------------------------

   function Compare_In_Base_Name_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint
   is
      A_Name : constant Filesystem_String :=
        Get_File (Model, A, -File_Column).Base_Name;
      B_Name : constant Filesystem_String :=
        Get_File (Model, B, -File_Column).Base_Name;

   begin
      if A_Name < B_Name then
         return -1;

      elsif A_Name = B_Name then
         return 0;

      else
         return 1;
      end if;
   end Compare_In_Base_Name_Order;

   ----------------------------------
   -- Compare_In_Line_Column_Order --
   ----------------------------------

   function Compare_In_Line_Column_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint
   is
      A_Line   : constant Glib.Gint := Get_Int (Model, A, -Line_Column);
      A_Column : constant Glib.Gint := Get_Int (Model, A, -Column_Column);
      B_Line   : constant Glib.Gint := Get_Int (Model, B, -Line_Column);
      B_Column : constant Glib.Gint := Get_Int (Model, B, -Column_Column);

   begin
      if A_Line < B_Line then
         return -1;

      elsif A_Line = B_Line then
         if A_Column < B_Column then
            return -1;

         elsif A_Column = B_Column then
            return 0;
         end if;
      end if;

      return 1;
   end Compare_In_Line_Column_Order;

   ---------------------------
   -- Compare_In_Path_Order --
   ---------------------------

   function Compare_In_Path_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint
   is
      A_Path    : constant Gtk_Tree_Path := Get_Path (Model, A);
      B_Path    : constant Gtk_Tree_Path := Get_Path (Model, B);
      A_Indices : constant Glib.Gint_Array := Get_Indices (A_Path);
      B_Indices : constant Glib.Gint_Array := Get_Indices (B_Path);

   begin
      Path_Free (A_Path);
      Path_Free (B_Path);

      if A_Indices (A_Indices'Last) < B_Indices (B_Indices'Last) then
         return -1;

      elsif A_Indices (A_Indices'Last) = B_Indices (B_Indices'Last) then
         return 0;

      else
         return 1;
      end if;
   end Compare_In_Path_Order;

   -----------------------------
   -- Compare_In_Weight_Order --
   -----------------------------

   function Compare_In_Weight_Order
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Glib.Gint
   is
      A_Weight : constant Glib.Gint := Get_Int (Model, A, -Weight_Column);
      B_Weight : constant Glib.Gint := Get_Int (Model, B, -Weight_Column);

   begin
      if A_Weight > B_Weight then
         return -1;

      elsif A_Weight = B_Weight then
         return 0;

      else
         return 1;
      end if;
   end Compare_In_Weight_Order;

   ---------------------
   -- Disable_Sorting --
   ---------------------

   procedure Disable_Sorting
     (Self : not null access Classic_Tree_Model_Record'Class) is
   begin
      --  Disable sorting till complete construction of the model

      if Self.Idle_Handler = Glib.Main.No_Source_Id then
         Self.Idle_Handler :=
          Classic_Tree_Model_Sources.Idle_Add (On_Idle'Access, Self);
         Self.Sort_Column := Self.Freeze_Sort;
      end if;
   end Disable_Sorting;

   ----------------
   -- File_Added --
   ----------------

   overriding procedure File_Added
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Mark          : Glib.Values.GValue;
   begin
      Self.Model.Disable_Sorting;

      --  Lookup for iter for parent category row.

      Self.Find_Category (To_String (Category), Category_Iter);

      --  Append row for file.

      Self.Model.Append (Iter, Category_Iter);

      Glib.Values.Init (Mark, GPS.Editors.GtkAda.Get_Editor_Mark_Type);
      GPS.Editors.GtkAda.Set_Mark (Mark, GPS.Editors.Nil_Editor_Mark);

      Set_And_Clear
        (Gtk_Tree_Store (Self.Model), Iter,
         (-Category_Column,
          -Weight_Column,
          -File_Column,
          -Line_Column,
          -Column_Column,
          -Text_Column,
          -Node_Icon_Name_Column,
          -Node_Markup_Column,
          -Node_Tooltip_Column,
          -Node_Mark_Column,
          -Action_Command_Column,
          -Action_Tooltip_Column,
          -Number_Of_Children_Column,
          -Sort_Order_Hint_Column,
          -Message_Column),
         (1 => As_String (To_String (Category)),
          2 => As_Int  (0),
          3 => As_File (File),
          4 => As_Int  (-1),
          5 => As_Int  (-1),
          6 => As_String
            ((if File /= No_File
             then String (File.Base_Name)
             else "<unknown>")),
          7 => As_String ("gps-emblem-file-unmodified"),
          8 => As_String
            ((if File /= No_File
             then String (File.Base_Name)
             else "&lt;unknown&gt;")),
          9  => As_String  (String (File.Base_Name)),
          10 => Mark,
          11 => As_Pointer (System.Null_Address),
          12 => As_String  (String (File.Base_Name)),
          13 => As_Int     (0),
          14 => As_Int (Self.Model.Get_Int
            (Category_Iter, -Sort_Order_Hint_Column)),
          15 => As_Pointer (System.Null_Address)));
   end File_Added;

   ------------------
   -- File_Removed --
   ------------------

   overriding procedure File_Removed
     (Self     : not null access Locations_Listener;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Path     : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      Self.Model.Disable_Sorting;
      Self.Find_File (To_String (Category), File, Category_Iter, File_Iter);
      File_Path := Self.Model.Get_Path (File_Iter);

      for Index in reverse
        Self.Model.Removed_Rows.First_Index
          .. Self.Model.Removed_Rows.Last_Index
      loop
         declare
            Path : Gtk.Tree_Model.Gtk_Tree_Path;
         begin
            Path := Self.Model.Removed_Rows (Index).Get_Path;
            if Is_Ancestor (File_Path, Path) then
               Self.Model.Removed_Rows.Delete (Index);
            end if;
            Path_Free (Path);
         end;
      end loop;

      Path_Free (File_Path);
      Self.Model.Remove (File_Iter);
      Self.Model.Need_Refresh := True;
   end File_Removed;

   -----------------------------
   -- Update_Background_Color --
   -----------------------------

   procedure Update_Background_Color
     (Self    : not null access Locations_Listener'Class;
      Message : not null access Abstract_Message'Class)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Message_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sorted_Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Heaviest_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Dummy         : Boolean;
   begin
      Find_Message (Self          => Self,
                    Message       => Message,
                    Category_Iter => Category_Iter,
                    File_Iter     => File_Iter,
                    Iter          => Message_Iter);

      Sorted_Iter := new Gtk.Tree_Model.Gtk_Tree_Iter;
      if not Self.Model.Sorted_Model.Convert_Child_Iter_To_Iter
        (Sorted_Iter, File_Iter)
      then
         return;
      end if;

      --  The sorted model is sorted by weight so the parent should have
      --  the weight of its heavier child which is not being deleted
      Heaviest_Iter :=
        Heaviest_Non_Deleted_Iter
          (Self.Model, Self.Model.Sorted_Model.Children (Sorted_Iter.all));

      if Heaviest_Iter = Null_Iter then
         return;
      end if;

      Copy_Background (Self.Model, Heaviest_Iter, File_Iter);

      if not Self.Model.Sorted_Model.Convert_Child_Iter_To_Iter
        (Sorted_Iter, Category_Iter)
      then
         return;
      end if;

      Self.Model.Sorted_Model.Convert_Iter_To_Child_Iter
        (Heaviest_Iter, Self.Model.Sorted_Model.Children (Sorted_Iter.all));

      Copy_Background (Self.Model, Heaviest_Iter, Category_Iter);
   end Update_Background_Color;

   ------------------------------
   -- Refresh_Background_Color --
   ------------------------------

   procedure Refresh_Background_Color (Self : Classic_Tree_Model)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Self.Get_Iter_First;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Heaviest_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sorted_Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Sorted_Iter := new Gtk.Tree_Model.Gtk_Tree_Iter;
      while Category_Iter /= Null_Iter loop
         File_Iter := Self.Children (Category_Iter);

         while File_Iter /= Null_Iter loop
            if not Self.Sorted_Model.Convert_Child_Iter_To_Iter
              (Sorted_Iter, File_Iter)
            then
               return;
            end if;
            Heaviest_Iter :=
              Heaviest_Non_Deleted_Iter
                (Self, Self.Sorted_Model.Children (Sorted_Iter.all));
            Copy_Background (Self, Heaviest_Iter, File_Iter);

            Self.Next (File_Iter);
         end loop;

         --  Set the category background color after updating all its files
         if not Self.Sorted_Model.Convert_Child_Iter_To_Iter
              (Sorted_Iter, Category_Iter)
         then
            return;
         end if;
         Heaviest_Iter :=
           Heaviest_Non_Deleted_Iter
             (Self, Self.Sorted_Model.Children (Sorted_Iter.all));
         Copy_Background (Self, Heaviest_Iter, Category_Iter);
         Self.Next (Category_Iter);
      end loop;
   end Refresh_Background_Color;

   ---------------------
   -- Copy_Background --
   ---------------------

   procedure Copy_Background
     (Model : Classic_Tree_Model;
      From  : Gtk.Tree_Model.Gtk_Tree_Iter;
      To    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Bg : Glib.Values.GValue;
   begin
      if From = Null_Iter or else To = Null_Iter then
         return;
      end if;
      Get_Value
        (Gtk_Tree_Store (Model), From, -Background_Color_Column, Bg);
      Set_Value
        (Gtk_Tree_Store (Model), To, -Background_Color_Column, Bg);
      Glib.Values.Unset (Bg);
   end Copy_Background;

   -------------------------------
   -- Heaviest_Non_Deleted_Iter --
   -------------------------------

   function Heaviest_Non_Deleted_Iter
     (Model      : Classic_Tree_Model;
      Start_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function Is_Deleted
        (Model : Classic_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
      --  Verify if Path correspond to a removed messages

      ----------------
      -- Is_Deleted --
      ----------------

      function Is_Deleted
        (Model : Classic_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
      is
         Image : constant String := To_String (Path);

      begin
         for Reference of Model.Removed_Rows loop
            declare
               Path       : constant Gtk_Tree_Path := Reference.Get_Path;
               Path_Image : constant String := To_String (Path);

            begin
               Path_Free (Path);

               if Reference.Valid and then Path_Image = Image then
                  return True;
               end if;
            end;
         end loop;

         return False;
      end Is_Deleted;

      Sorted_Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Start_Iter;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      while Sorted_Iter /= Null_Iter loop
         Model.Sorted_Model.Convert_Iter_To_Child_Iter
           (Iter, Sorted_Iter);
         if not Is_Deleted (Model, Model.Get_Path (Iter)) then
            return Iter;
         end if;

         Model.Sorted_Model.Next (Sorted_Iter);
      end loop;

      return Null_Iter;
   end Heaviest_Non_Deleted_Iter;

   -------------------
   -- Find_Category --
   -------------------

   procedure Find_Category
     (Self     : not null access Locations_Listener'Class;
      Category : String;
      Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Iter := Self.Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         exit when Self.Model.Get_String (Iter, -Category_Column) = Category;

         Self.Model.Next (Iter);
      end loop;
   end Find_Category;

   ---------------
   -- Find_File --
   ---------------

   procedure Find_File
     (Self          : not null access Locations_Listener'Class;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Self.Find_Category (Category, Category_Iter);

      File_Iter := Self.Model.Children (Category_Iter);

      while File_Iter /= Null_Iter loop
         exit when Self.Model.Get (File_Iter, -File_Column) = File;

         Self.Model.Next (File_Iter);
      end loop;
   end Find_File;

   ------------------
   -- Find_Message --
   ------------------

   procedure Find_Message
     (Self          : not null access Locations_Listener'Class;
      Message       : not null access Abstract_Message'Class;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type System.Address;

      Parent_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Message.Level = Primary then
         Self.Find_File
           (To_String (Message.Get_Category),
            Message.Get_File,
            Category_Iter,
            File_Iter);
         Parent_Iter := File_Iter;

      else
         Self.Find_Message
           (Message.Get_Parent, Category_Iter, File_Iter, Parent_Iter);
      end if;

      Iter := Self.Model.Children (Parent_Iter);

      while Iter /= Null_Iter loop
         exit when Self.Model.Get_Address (Iter, -Message_Column) =
           Message_Conversions.To_Address
             (Message_Conversions.Object_Pointer (Message));

         Self.Model.Next (Iter);
      end loop;
   end Find_Message;

   ---------
   -- Get --
   ---------

   function Get
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return GNATCOLL.VFS.Virtual_File is
   begin
      return GNATCOLL.VFS.GtkAda.Get_File (Self, Iter, Column);
   end Get;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
      Column : Glib.Gint) return Message_Access is
   begin
      return
        Message_Access
          (Message_Conversions.To_Pointer
             (Get_Address (Model, Iter, Column)));
   end Get_Message;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (L : Locations_Listener_Access) return Gtk.Tree_Model.Gtk_Tree_Model is
   begin
      return To_Interface (L.Model);
   end Get_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Object : out Classic_Tree_Model) is
   begin
      Object := new Classic_Tree_Model_Record;
      Initialize (Object);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Classic_Tree_Model_Record'Class) is
   begin
      Gtk.Tree_Store.Initialize (Self, Column_Types);
      Gtk.Tree_Model_Sort.Gtk_New_With_Model (Self.Sorted_Model, +Self);
      Self.Sorted_Model.Set_Sort_Column_Id (-Weight_Column, Sort_Descending);
      Self.Set_Default_Sort_Func (Compare_Nodes'Access);
      Self.Set_Sort_Column_Id
        (Gtk.Tree_Sortable.Default_Sort_Column_Id, Sort_Ascending);
   end Initialize;

   ------------------------
   -- Insert_With_Values --
   ------------------------

   procedure Insert_With_Values
      (Tree_Store : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Glib.Gint;
       Columns    : Glib.Gint_Array;
       Values     : Glib.Values.GValue_Array)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address;
          Position   : Glib.Gint;
          Columns    : not null access Glib.Gint;
          Values     : not null access Glib.Values.GValue;
          N_Values   : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_store_insert_with_valuesv");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal
        (Glib.Object.Get_Object (Tree_Store),
         Tmp_Iter,
         Iter_Or_Null (Parent'Address),
         Position,
         Columns (Columns'First)'Unrestricted_Access,
         Values (Values'First)'Unrestricted_Access,
         Values'Length);
      Iter := Tmp_Iter;
   end Insert_With_Values;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

      Values  : Glib.Values.GValue_Array (1 .. 17);
      Columns : constant Columns_Array (Values'Range) :=
        (-Category_Column,
         -Weight_Column,
         -File_Column,
         -Line_Column,
         -Column_Column,
         -Text_Column,
         -Node_Icon_Name_Column,
         -Node_Markup_Column,
         -Node_Tooltip_Column,
         -Node_Mark_Column,
         -Icon_Name_Column,
         -Action_Command_Column,
         -Action_Tooltip_Column,
         -Number_Of_Children_Column,
         -Sort_Order_Hint_Column,
         -Message_Column,
         -Background_Color_Column);

      File_Values  : Glib.Values.GValue_Array (1 .. 2);
      File_Columns : Columns_Array (File_Values'Range);
      File_Last    : Glib.Gint := 0;
      Message_Last : Glib.Gint := 16;
      Color        : Gdk.RGBA.Gdk_RGBA;
   begin
      Self.Model.Disable_Sorting;

      if Message.Get_Parent /= null then
         Self.Find_Message
           (Message.Get_Parent, Category_Iter, File_Iter, Parent_Iter);

      else
         Self.Find_File
           (To_String (Message.Get_Category),
            Message.Get_File,
            Category_Iter,
            File_Iter);
         Parent_Iter := File_Iter;
      end if;

      Glib.Values.Init_Set_String
        (Values (1), To_String (Message.Get_Category));

      case Message.Level is
         when Primary =>
            Glib.Values.Init_Set_Int
              (Values (2),
               Glib.Gint
                 (Message_Importance_Type'Pos (Message.Get_Importance)));

            File_Last := 1;
            File_Columns (1) := -Weight_Column;
            Glib.Values.Init_Set_Int
              (File_Values (1),
               Glib.Gint'Max
                 (Self.Model.Get_Int (File_Iter, -Weight_Column),
                  Glib.Gint
                    (Message_Importance_Type'Pos (Message.Get_Importance))));

         when Secondary =>
            Glib.Values.Init_Set_Int (Values (2), 0);
      end case;

      Values (3 .. 7) :=
        (3 => As_File   (Message.Get_File),
         4 => As_Int    (Glib.Gint (Message.Get_Line)),
         5 => As_Int    (Glib.Gint (Message.Get_Column)),
         6 => As_String (To_String (Message.Get_Text)),
         7 => As_String (""));

      if Message.Level = Primary
        and (Message.Get_Line /= 0 or Message.Get_Column /= 0)
      then
         --  For primary messages, output line:column information and text of
         --  the message when line:column information is available.

         declare
            Location : constant String :=
              Image (Message.Get_Line)
                & ':'
                & Image (Natural (Message.Get_Column));
            Length   : constant Natural :=
              Integer'Max (0, Location_Padding - Location'Length);

         begin
            Glib.Values.Init_Set_String
              (Values (8),
               "<b>" & Location & "</b>" & (Length * ' ')
               & To_String (Message.Get_Markup));
         end;

      else
         --  Otherwise output message text only.

         Glib.Values.Init_Set_String
           (Values (8),
            (Location_Padding * ' ') & To_String (Message.Get_Markup));
      end if;

      declare
         Markup : Unbounded_String;
         M      : Message_Access := Message_Access (Message);

      begin
         loop
            case M.Level is
               when Primary =>
                  Markup := ASCII.LF & M.Get_Markup & Markup;

                  exit;

               when Secondary =>
                  Markup := ASCII.LF & "  " & M.Get_Markup & Markup;
            end case;

            M := M.Get_Parent;
         end loop;

         Markup :=
           M.Get_Category
           & ASCII.LF
           & String (M.Get_File.Base_Name)
           & ":" & Image (M.Get_Line)
           & ':' & Image (Integer (M.Get_Column))
           & To_String (Markup);

         Glib.Values.Init_Set_String (Values (9), To_String (Markup));
      end;

      Glib.Values.Init (Values (10), GPS.Editors.GtkAda.Get_Editor_Mark_Type);
      GPS.Editors.GtkAda.Set_Mark (Values (10), Message.Get_Editor_Mark);

      Values (11 .. 16) :=
        (11 => As_String
           (if Message.Get_Action /= null
            and then Message.Get_Action.Associated_Command /= null
            and then Message.Get_Action.Image /= Null_Unbounded_String
            then To_String (Message.Get_Action.Image)
            else ""),
         12 => As_Pointer (To_Address (Message.Get_Action)),
         13 => As_String
           (if Message.Get_Action /= null
            and then Message.Get_Action.Tooltip_Text /= Null_Unbounded_String
            then To_String (Message.Get_Action.Tooltip_Text)
            else ""),
         14 => As_Int (0),
         15 => As_Int
           (Sort_Order_Hint'Pos
                (Self.Kernel.Get_Messages_Container.Get_Sort_Order_Hint
                     (To_String (Message.Get_Category)))),
         --  XXX Can it be changed dynamically?
         16 => As_Pointer
           (Message_Conversions.To_Address
                (Message_Conversions.Object_Pointer (Message))));

      Color := Message.Get_Background_Color;
      if not Gdk.RGBA.Equal (Color, Gdk.RGBA.Null_RGBA) then
         Message_Last := 17;
         Gdk.RGBA.Set_Value (Values (Message_Last), Color);
      end if;

      --  Create row for the message using prepared data

      Insert_With_Values
        (Gtk.Tree_Store.Gtk_Tree_Store_Record (Self.Model.all)'Access,
         Iter,
         Parent_Iter,
         -1, Glib.Gint_Array (Columns (1 .. Message_Last)),
         Values (1 .. Message_Last));

      Unset (Values (1 .. Message_Last));

      --  Update message counts for category and file row when message is
      --  primary.

      if Message.Level = Primary then
         Self.Model.Set
           (Category_Iter,
            -Number_Of_Children_Column,
            Self.Model.Get_Int
              (Category_Iter, -Number_Of_Children_Column) + 1);

         File_Last := File_Last + 1;
         File_Columns (File_Last) := -Number_Of_Children_Column;
         Glib.Values.Init_Set_Int
           (File_Values (File_Last),
            Self.Model.Get_Int (File_Iter, -Number_Of_Children_Column) + 1);
      end if;

      if File_Last > 0 then
         Set_And_Clear
           (Gtk_Tree_Store (Self.Model), File_Iter,
            File_Columns (1 .. File_Last), File_Values (1 .. File_Last));
      end if;

      Update_Background_Color (Self, Message);
   end Message_Added;

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self     : not null access Locations_Listener;
      Message  : not null access Abstract_Message'Class;
      Property : String)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Bg            : Glib.Values.GValue;

   begin
      if Property = "action" then
         Self.Find_Message (Message, Category_Iter, File_Iter, Iter);

         Set_And_Clear
           (Gtk_Tree_Store (Self.Model), Iter,
            (-Icon_Name_Column,
             -Action_Command_Column,
             -Action_Tooltip_Column),
            (1 => As_String
                 (if Message.Get_Action /= null
                  and then Message.Get_Action.Associated_Command /= null
                  and then Message.Get_Action.Image /= Null_Unbounded_String
                  then To_String (Message.Get_Action.Image)
                  else ""),
             2 => As_Pointer (To_Address (Message.Get_Action)),
             3 => As_String
               (if Message.Get_Action /= null
                and then
                Message.Get_Action.Tooltip_Text /= Null_Unbounded_String
                then To_String (Message.Get_Action.Tooltip_Text)
                else "")));

      elsif Property = "highlighting" then
         Self.Find_Message (Message, Category_Iter, File_Iter, Iter);
         Gdk.RGBA.Set_Value (Bg, Message.Get_Background_Color);
         Set_Value
           (Gtk_Tree_Store
              (Self.Model), Iter, -Background_Color_Column, Bg);
         Glib.Values.Unset (Bg);
         Update_Background_Color (Self, Message);
      end if;
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Reference     : Gtk.Tree_Row_Reference.Gtk_Tree_Row_Reference;

   begin
      Self.Model.Disable_Sorting;

      Self.Find_Message (Message, Category_Iter, File_Iter, Iter);

      --  Postpone remove of the message

      Path := Self.Model.Get_Path (Iter);
      Reference :=
        Gtk.Tree_Row_Reference.Gtk_Tree_Row_Reference_New
          (Self.Model.To_Interface, Path);
      Self.Model.Removed_Rows.Append (Reference);
      Path_Free (Path);

      if Message.Level = Primary then
         --  Update message counters

         Self.Model.Set
           (Category_Iter,
            -Number_Of_Children_Column,
            Self.Model.Get_Int
              (Category_Iter, -Number_Of_Children_Column) - 1);
         Self.Model.Set
           (File_Iter,
            -Number_Of_Children_Column,
            Self.Model.Get_Int (File_Iter, -Number_Of_Children_Column) - 1);
      end if;
   end Message_Removed;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Self : Classic_Tree_Model) return Boolean is
      Iter         : Gtk_Tree_Iter;
      Need_Refresh : constant Boolean := not Self.Removed_Rows.Is_Empty;
   begin
      Self.Idle_Handler := Glib.Main.No_Source_Id;

      --  Remove idividual messages when it was not removed by removing of
      --  files/categories.

      for Reference of Self.Removed_Rows loop
         if Reference.Valid then
            Iter := Self.Get_Iter (Reference.Get_Path);
            Self.Remove (Iter);
         end if;

         Gtk.Tree_Row_Reference.Free (Reference);
      end loop;

      Self.Removed_Rows.Clear;

      if Self.Need_Refresh or else Need_Refresh then
         Refresh_Background_Color (Self);
         Self.Need_Refresh := False;
      end if;

      --  Enable sorting

      Self.Thaw_Sort (Self.Sort_Column);

      return False;
   end On_Idle;

   ---------
   -- Pos --
   ---------

   function Pos (Column : Listener_Columns) return Glib.Gint is
   begin
      return Listener_Columns'Pos (Column);
   end Pos;
   --------------
   -- Register --
   --------------

   function Register
     (Kernel : Kernel_Handle) return Locations_Listener_Access
   is
      Container  : constant not null GPS.Kernel.Messages_Container_Access :=
                     Kernel.Get_Messages_Container;
      Success    : Boolean;
      Self       : Locations_Listener_Access;
      File_Added : Boolean;

   begin
      Column_Types :=
        (Listener_Columns'Pos (Category_Column)           =>
             Glib.GType_String,
         Listener_Columns'Pos (Weight_Column)             =>
             Glib.GType_Int,
         Listener_Columns'Pos (File_Column)               =>
             Get_Virtual_File_Type,
         Listener_Columns'Pos (Line_Column)               =>
             Glib.GType_Int,
         Listener_Columns'Pos (Column_Column)             =>
             Glib.GType_Int,
         Listener_Columns'Pos (Text_Column)               =>
             Glib.GType_String,
         Listener_Columns'Pos (Node_Icon_Name_Column)     =>
             Glib.GType_String,
         Listener_Columns'Pos (Node_Markup_Column)        =>
             Glib.GType_String,
         Listener_Columns'Pos (Node_Tooltip_Column)       =>
             Glib.GType_String,
         Listener_Columns'Pos (Node_Mark_Column)          =>
             GPS.Editors.GtkAda.Get_Editor_Mark_Type,
         Listener_Columns'Pos (Icon_Name_Column)          =>
             Glib.GType_String,
         Listener_Columns'Pos (Action_Command_Column)     =>
             Glib.GType_Pointer,
         Listener_Columns'Pos (Action_Tooltip_Column)     =>
             Glib.GType_String,
         Listener_Columns'Pos (Number_Of_Children_Column) =>
             Glib.GType_Int,
         Listener_Columns'Pos (Sort_Order_Hint_Column)    =>
             Glib.GType_Int,
         Listener_Columns'Pos (Message_Column)            =>
             Glib.GType_Pointer,
         Listener_Columns'Pos (Background_Color_Column)   =>
             Gdk.RGBA.Get_Type);

      Self := new Locations_Listener;
      Self.Kernel := Kernel;

      --  Allocate foreground color for category and file nodes

      Gdk.RGBA.Parse (Self.Non_Leaf_Color, Non_Leaf_Color_Name, Success);

      --  Create GtkTreeModel

      Gtk_New (Self.Model);

      --  Register listener

      Container.Register_Listener (Listener_Access (Self), Locations_Only);

      --  Construct tree for currently visible messages

      for Category of Container.Get_Categories loop
         if Container.Get_Flags (To_String (Category)) (Locations) then
            Self.Category_Added (Category, False);

            for File of Container.Get_Files (Category) loop
               File_Added := False;

               for Message of Container.Get_Messages (Category, File) loop
                  if Message.Get_Flags (GPS.Kernel.Messages.Locations) then
                     if not File_Added then
                        File_Added := True;
                        Self.File_Added (Category, File);
                     end if;

                     Self.Message_Added (Message);
                  end if;
               end loop;
            end loop;
         end if;
      end loop;

      return Self;
   end Register;

   ---------------
   -- Set_Order --
   ---------------

   procedure Set_Order
     (Self       : not null access Classic_Tree_Model_Record'Class;
      File_Order : File_Sort_Order;
      Msg_Order  : Messages_Sort_Order) is
   begin
      Self.Messages_Order := Msg_Order;
      Self.File_Order := File_Order;

      --  Force a re-sort

      Self.Set_Default_Sort_Func (Compare_Nodes'Access);
   end Set_Order;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : Kernel_Handle;
      Self   : in out Locations_Listener_Access)
   is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation
          (Locations_Listener'Class, Locations_Listener_Access);

   begin
      --  Unregister the listener

      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Self));

      --  Destroy the model

      Self.Model.Clear;
      Unref (Self.Model);

      Unchecked_Free (Self);
   end Unregister;

end GPS.Location_View.Listener;

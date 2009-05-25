-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with System;

with Glib;                 use Glib;
with Glib.Convert;
with Glib.Object;          use Glib.Object;
with Glib.Values;          use Glib.Values;
with Gtk.Enums;
with Gtk.Object;           use Gtk.Object;
with Gtk.Widget;

with GNATCOLL.VFS;         use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;  use GNATCOLL.VFS.GtkAda;
with GPS.Editors.GtkAda;   use GPS.Editors.GtkAda;
with GPS.Kernel.Locations; use GPS.Kernel.Locations;
with Traces;               use Traces;

package body GPS.Location_Model is

   Me : constant Debug_Handle := Create ("GPS_Location_Model");

   function To_Style is new Ada.Unchecked_Conversion
     (System.Address, GPS.Kernel.Styles.Style_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (Style_Access, System.Address);

   function Columns_Types return Glib.GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   procedure Remove_Line
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Model      : not null access Gtk_Tree_Model_Record'Class;
      Loc_Iter   : Gtk_Tree_Iter);
   --  Clear the marks and highlightings of one specific line

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address);
   pragma Convention (C, On_Destroy);

   Messages_Padding : constant Integer := 10;

   Non_Leaf_Color_Name : constant String := "blue";
   --  <preference> color to use for category and file names

   ---------------------
   -- Add_Action_Item --
   ---------------------

   procedure Add_Action_Item
     (Self       : not null access Location_Model_Record'Class;
      Identifier : String;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Natural;
      Message    : String;
      Action     : Action_Item)
   is
      Escaped_Message : constant String := Glib.Convert.Escape_Text (Message);
      Category_Iter   : Gtk_Tree_Iter;
      File_Iter       : Gtk_Tree_Iter;
      Created         : Boolean;
      Line_Iter       : Gtk_Tree_Iter;
      Children_Iter   : Gtk_Tree_Iter;
      Next_Iter       : Gtk_Tree_Iter;
      Main_Line_Iter  : Gtk_Tree_Iter;
      Value           : GValue;
      Old_Action      : Action_Item;

      function Escaped_Compare (S1, S2 : String) return Boolean;
      --  Compare S1 and S2, abstracting any pango markup or escape sequence

      ---------------------
      -- Escaped_Compare --
      ---------------------

      function Escaped_Compare (S1, S2 : String) return Boolean is
         I1, I2 : Natural;

         procedure Advance (J : in out Natural; S : String);
         --  Auxiliary function

         -------------
         -- Advance --
         -------------

         procedure Advance (J : in out Natural; S : String) is
         begin
            J := J + 1;

            if J > S'Last then
               return;
            end if;

            if S (J) = '<' then
               loop
                  J := J + 1;
                  exit when J > S'Last
                    or else (S (J - 1) = '>' and then S (J) /= '<');
               end loop;

            elsif S (J) = '&' then
               loop
                  J := J + 1;
                  exit when J > S'Last or else S (J - 1) = ';';
               end loop;
            end if;
         end Advance;

      begin
         I1 := S1'First - 1;
         I2 := S2'First - 1;

         loop
            Advance (I1, S1);
            Advance (I2, S2);

            if I1 > S1'Last and then I2 > S2'Last then
               return True;
            end if;

            if I1 > S1'Last or else I2 > S2'Last
              or else S1 (I1) /= S2 (I2)
            then
               return False;
            end if;
         end loop;
      end Escaped_Compare;

      pragma Unreferenced (Identifier);
   begin
      Trace (Me, "Add_Action_Item: "
             & File.Display_Full_Name
             & ' ' & Category & Line'Img & Column'Img
             & ' ' & Message);

      Get_Category_File
        (Self,
         Glib.Convert.Escape_Text (Category),
         File, Category_Iter, File_Iter, Created, False);

      if Category_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: Category " & Category & " not found");
      end if;

      if File_Iter = Null_Iter then
         Trace (Me, "Add_Action_Item: File " & File.Display_Full_Name
                & " not found");
      end if;

      if Category_Iter /= Null_Iter
        and then File_Iter /= Null_Iter
      then
         Line_Iter := Self.Children (File_Iter);
         Main_Line_Iter := Line_Iter;
         Next_Iter := File_Iter;
         Self.Next (Next_Iter);

         while Line_Iter /= Null_Iter loop
            if Self.Get_Int (Main_Line_Iter, Line_Column) = Gint (Line)
              and then Self.Get_Int (Main_Line_Iter, Column_Column)
                = Gint (Column)
              and then Escaped_Compare
                (Get_Message (Self, Line_Iter), Escaped_Message)
            then
               if Action = null then
                  Self.Set (Line_Iter, Button_Column, GObject (Null_Pixbuf));

                  Self.Get_Value (Line_Iter, Action_Column, Value);
                  Old_Action := To_Action_Item (Get_Address (Value));

                  if Old_Action /= null then
                     Free (Old_Action);
                  end if;

                  Set_Address (Value, System.Null_Address);
                  Self.Set_Value (Line_Iter, Action_Column, Value);
                  Unset (Value);

               else
                  Self.Set (Line_Iter, Button_Column, GObject (Action.Image));
                  Init (Value, GType_Pointer);
                  Set_Address (Value, To_Address (Action));

                  Self.Set_Value (Line_Iter, Action_Column, Value);
                  Unset (Value);
               end if;

               return;
            end if;

            Children_Iter := Self.Children (Line_Iter);

            if Children_Iter /= Null_Iter then
               Line_Iter := Children_Iter;

            else
               if Main_Line_Iter = Line_Iter then
                  Self.Next (Main_Line_Iter);
               end if;

               Children_Iter := Line_Iter;
               Self.Next (Line_Iter);

               if Line_Iter = Null_Iter then
                  Line_Iter := Self.Parent (Children_Iter);
                  Self.Next (Line_Iter);
                  Main_Line_Iter := Line_Iter;
               end if;
            end if;

            exit when Line_Iter = Next_Iter;
         end loop;
      end if;

      Trace (Me, "Add_Action_Item: entry not found");
   end Add_Action_Item;

   --------------------
   -- Category_Count --
   --------------------

   function Category_Count
     (Model    : not null access Gtk_Tree_Model_Record'Class;
      Category : String) return Natural
   is
      Cat   : Gtk_Tree_Iter;
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;

   begin
      Get_Category_File
        (Location_Model (Model),
         Glib.Convert.Escape_Text (Category),
         GNATCOLL.VFS.No_File, Cat, Iter, Dummy, False);

      if Cat = Null_Iter then
         return 0;
      end if;

      return Natural (Model.Get_Int (Cat, Number_Of_Items_Column));
   end Category_Count;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return Glib.GType_Array is
   begin
      return Glib.GType_Array'
        (Icon_Column               => Gdk.Pixbuf.Get_Type,
         Absolute_Name_Column      => Get_Virtual_File_Type,
         Base_Name_Column          => Glib.GType_String,
         Mark_Column               => Get_Editor_Mark_Type,
         Line_Column               => Glib.GType_Int,
         Column_Column             => Glib.GType_Int,
         Length_Column             => Glib.GType_Int,
         Color_Column              => Gdk.Color.Gdk_Color_Type,
         Button_Column             => Gdk.Pixbuf.Get_Type,
         Action_Column             => Glib.GType_Pointer,
         Highlight_Column          => Glib.GType_Boolean,
         Highlight_Category_Column => Glib.GType_Pointer,
         Number_Of_Items_Column    => Glib.GType_Int,
         Category_Line_Column      => Glib.GType_String);
   end Columns_Types;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Model              : not null access Location_Model_Record'Class;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : GNATCOLL.VFS.Virtual_File;
      Message            : Glib.UTF8_String;
      Mark               : Editor_Mark'Class := Nil_Editor_Mark;
      Line               : Integer;
      Column             : Visible_Column_Type;
      Length             : Integer;
      Highlighting       : Boolean;
      Highlight_Category : Style_Access;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf)
   is
      function To_Proxy is
        new Ada.Unchecked_Conversion (System.Address, Gdk.C_Proxy);

      Value : GValue;

   begin
      if Base_Name = "" then
         Set (Model, Iter, Base_Name_Column,
              GNATCOLL.VFS.Display_Base_Name (Absolute_Name));

      else
         if Message = "" then
            Set (Model, Iter, Base_Name_Column, Base_Name);
         else
            declare
               Padding : constant String (1 .. Messages_Padding) :=
                           (others => ' ');
            begin
               Set (Model, Iter, Base_Name_Column,
                    "<b>" & Base_Name & "</b>"
                    & Padding (1 .. Messages_Padding - Base_Name'Length)
                    & Message);
            end;
         end if;
      end if;

      Init (Value, Get_Virtual_File_Type);
      Set_File (Value, Absolute_Name);
      Set_Value (Model, Iter, Absolute_Name_Column, Value);
      Unset (Value);

      Init (Value, Get_Editor_Mark_Type);
      Set_Mark (Value, Mark);
      Set_Value (Model, Iter, Mark_Column, Value);
      Unset (Value);

      Set (Model, Iter, Line_Column, Glib.Gint (Line));
      Set (Model, Iter, Column_Column, Glib.Gint (Column));
      Set (Model, Iter, Length_Column, Glib.Gint (Length));
      Set (Model, Iter, Icon_Column, Glib.Object.GObject (Pixbuf));
      Set (Model, Iter, Highlight_Column, Highlighting);

      Init (Value, Glib.GType_Pointer);
      Set_Address (Value, To_Address (Highlight_Category));

      Set_Value (Model, Iter, Highlight_Category_Column, Value);
      Unset (Value);

      Set (Model, Iter, Number_Of_Items_Column, 0);

      --  ??? Lexicographic order will be used for line numbers > 1_000_000

      declare
         Img : constant String := Integer'Image (Line + 1_000_000);
      begin
         Set
           (Model,
            Iter,
            Category_Line_Column,
            Get_Name (Highlight_Category) & Img (Img'Last - 5 .. Img'Last));
      end;

      if Line = 0 then
         Model.Set
           (Iter, Color_Column, To_Proxy (Model.Non_Leaf_Color'Address));

      else
         Model.Set (Iter, Color_Column, Gdk.C_Proxy'(null));
      end if;
   end Fill_Iter;

   -----------------------
   -- Get_Category_File --
   -----------------------

   procedure Get_Category_File
     (Model           : not null access Location_Model_Record'Class;
      Category        : Glib.UTF8_String;
      File            : GNATCOLL.VFS.Virtual_File;
      Category_Iter   : out Gtk_Tree_Iter;
      File_Iter       : out Gtk_Tree_Iter;
      New_Category    : out Boolean;
      Create          : Boolean) is
   begin
      File_Iter := Null_Iter;
      Category_Iter := Get_Iter_First (Model);
      New_Category := False;

      while Category_Iter /= Null_Iter
        and then Get_Category_Name (Model, Category_Iter) /= Category
      loop
         Next (Model, Category_Iter);
      end loop;

      if Category_Iter = Null_Iter then
         if Create then
            Append (Model, Category_Iter, Null_Iter);
            Fill_Iter
              (Model, Category_Iter, Category, GNATCOLL.VFS.No_File,
               "", Nil_Editor_Mark, 0, 0, 0, False, null,
               Model.Category_Pixbuf);
            New_Category := True;
         else
            return;
         end if;
      end if;

      if File = GNATCOLL.VFS.No_File then
         return;
      end if;

      File_Iter := Children (Model, Category_Iter);

      while File_Iter /= Null_Iter loop
         if Get_File (Model, File_Iter) = File then
            return;
         end if;

         Next (Model, File_Iter);
      end loop;

      --  When we reach this point, we need to create a new sub-category

      if Create then
         Append (Model, File_Iter, Category_Iter);
         Fill_Iter
           (Model, File_Iter, "", File, "", Nil_Editor_Mark, 0, 0, 0,
            False, null, Model.File_Pixbuf);
      end if;

      return;
   end Get_Category_File;

   -----------------------
   -- Get_Category_Name --
   -----------------------

   function Get_Category_Name
     (Model    : access Gtk_Tree_Model_Record'Class;
      Category : Gtk_Tree_Iter) return String
   is
      Cat : Gtk_Tree_Iter := Category;
   begin
      while Parent (Model, Cat) /= Null_Iter loop
         Cat := Parent (Model, Cat);
      end loop;

      declare
         Message : constant String :=
           Get_String (Model, Cat, Base_Name_Column);
         Matches : Match_Array (0 .. 1);
         Cut     : Integer;
      begin
         Match (Items_Count_Matcher, Message, Matches);

         if Matches (0) /= No_Match then
            Cut := Matches (1).First - 1;
         else
            Cut := Message'Last;
         end if;

         return Message (1 .. Cut);
      end;
   end Get_Category_Name;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File
   is
      Result : GNATCOLL.VFS.Virtual_File;
      Value  : GValue;

   begin
      Model.Get_Value (Iter, Absolute_Name_Column, Value);
      Result := Get_File (Value);
      Unset (Value);

      return Result;
   end Get_File;

   ----------------------------
   -- Get_Highlighting_Style --
   ----------------------------

   function Get_Highlighting_Style
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      return GPS.Kernel.Styles.Style_Access
   is
      Result : GPS.Kernel.Styles.Style_Access;
      Value  : GValue;

   begin
      Model.Get_Value (Iter, Highlight_Category_Column, Value);
      Result := To_Style (Get_Address (Value));
      Unset (Value);

      return Result;
   end Get_Highlighting_Style;

   --------------------------
   -- Get_Line_Column_Iter --
   --------------------------

   procedure Get_Line_Column_Iter
     (Model     : not null access Gtk_Tree_Model_Record'Class;
      File_Iter : Gtk_Tree_Iter;
      Line      : Natural;
      Column    : Natural := 0;
      Loc_Iter  : out Gtk_Tree_Iter)
   is
   begin
      Loc_Iter := Model.Children (File_Iter);

      while Loc_Iter /= Null_Iter loop
         if Model.Get_Int (Loc_Iter, Line_Column) = Gint (Line)
           and then
             (Column = 0
              or else Model.Get_Int (Loc_Iter, Column_Column) = Gint (Column))
         then
            return;
         end if;

         Model.Next (Loc_Iter);
      end loop;

      Loc_Iter := Null_Iter;
   end Get_Line_Column_Iter;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message
     (Model : not null access Gtk_Tree_Model_Record'Class;
      Iter  : Gtk_Tree_Iter) return String
   is
      M : constant String := Get_String (Model, Iter, Base_Name_Column);

   begin
      if M'Length > Messages_Padding then
         return M (M'First + Messages_Padding + 7 .. M'Last);
      end if;

      return "";
   end Get_Message;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model  : out Location_Model;
      Kernel : Kernel_Handle) is
   begin
      Model := new Location_Model_Record;
      Initialize (Model, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Location_Model_Record'Class;
      Kernel : Kernel_Handle)
   is
      Success : Boolean;

   begin
      Gtk.Tree_Store.Initialize (Self, Columns_Types);
      Self.Kernel := Kernel;
      Self.Category_Pixbuf :=
        Get_Main_Window (Kernel).Render_Icon
          ("gps-box", Gtk.Enums.Icon_Size_Menu);
      Self.File_Pixbuf :=
        Get_Main_Window (Kernel).Render_Icon
          ("gps-file", Gtk.Enums.Icon_Size_Menu);
      Self.Non_Leaf_Color := Gdk.Color.Parse (Non_Leaf_Color_Name);
      Gdk.Color.Alloc_Color
        (Gtk.Widget.Get_Default_Colormap,
         Self.Non_Leaf_Color, False, True, Success);

      Weak_Ref (Self, On_Destroy'Access);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Data   : System.Address;
      Object : System.Address)
   is
      pragma Unreferenced (Data);

      Stub : Location_Model_Record;
      pragma Warnings (Off, Stub);
      Self : constant Location_Model :=
               Location_Model (Get_User_Data (Object, Stub));
      Iter : Gtk_Tree_Iter;

   begin
      --  Remove all categories

      Iter := Self.Get_Iter_First;

      while Iter /= Null_Iter loop
         Remove_Category_Or_File_Iter (Self.Kernel, Self, Iter);
         Iter := Self.Get_Iter_First;
      end loop;

      --  Free pixbufs

      Unref (Self.Category_Pixbuf);
      Unref (Self.File_Pixbuf);
   end On_Destroy;

   ----------------------
   -- Recount_Category --
   ----------------------

   procedure Recount_Category
     (Model    : not null access Location_Model_Record'Class;
      Category : String)
   is
      Cat   : Gtk_Tree_Iter;
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
      Total : Gint := 0;
      Sub   : Gint := 0;

   begin
      Get_Category_File
        (Model,
         Glib.Convert.Escape_Text (Category),
         GNATCOLL.VFS.No_File, Cat, Iter, Dummy, False);

      if Cat = Null_Iter then
         return;
      end if;

      Iter := Model.Children (Cat);

      while Iter /= Null_Iter loop
         Sub := Model.N_Children (Iter);
         Model.Set (Iter, Number_Of_Items_Column, Sub);
         Total := Total + Sub;
         Model.Next (Iter);
      end loop;

      Model.Set (Cat, Number_Of_Items_Column, Total);
   end Recount_Category;

   ---------------------
   -- Remove_Category --
   ---------------------

   procedure Remove_Category
     (Kernel     : not null access Kernel_Handle_Record'Class;
      Model      : not null access Location_Model_Record'Class;
      Identifier : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural := 0)
   is
      Iter      : Gtk_Tree_Iter;
      File_Iter : Gtk_Tree_Iter;
      Dummy     : Boolean;
   begin
      Get_Category_File
        (Model, Identifier, File, Iter, File_Iter, Dummy, False);

      if File_Iter = Null_Iter then
         Remove_Category_Or_File_Iter (Kernel, Model, Iter);

      else
         --  Remove the indicated file
         Remove_Category_Or_File_Iter (Kernel, Model, File_Iter, Line);

         if Model.Children (Iter) = Null_Iter then
            --  If Category has no more children remove it
            Remove_Category_Or_File_Iter (Kernel, Model, Iter);
         end if;
      end if;
   end Remove_Category;

   ----------------------------------
   -- Remove_Category_Or_File_Iter --
   ----------------------------------

   procedure Remove_Category_Or_File_Iter
     (Kernel : not null access Kernel_Handle_Record'Class;
      Model  : not null access Location_Model_Record'Class;
      Iter   : in out Gtk_Tree_Iter;
      Line   : Natural := 0)
   is
      File_Iter : Gtk_Tree_Iter;
      Parent    : Gtk_Tree_Iter;
      File_Path : Gtk_Tree_Path;
      Loc_Iter  : Gtk_Tree_Iter;

      Removing_Category : Boolean := False;
      --  Indicates whether we are removing a whole category or just a file

   begin
      --  Unhighlight all the lines and remove all marks in children of the
      --  category / file.

      if Iter = Null_Iter then
         return;
      end if;

      Iter_Copy (Iter, File_Iter);

      File_Path := Model.Get_Path (File_Iter);

      if Get_Depth (File_Path) = 1 then
         File_Iter := Model.Children (File_Iter);
         Removing_Category := True;

      elsif Get_Depth (File_Path) /= 2 then
         Path_Free (File_Path);
         return;
      end if;

      Path_Free (File_Path);

      while File_Iter /= Null_Iter loop
         --  Delete the marks corresponding to all locations in this file
         Loc_Iter := Model.Children (File_Iter);

         if Line /= 0 then
            --  Delete one specific line location in one specific file
            while Loc_Iter /= Null_Iter loop
               if Model.Get_Int (Loc_Iter, Line_Column)
                 = Gint (Line) then
                  Remove_Line (Kernel, Model, Loc_Iter);
                  Model.Remove (Loc_Iter);

               else
                  Model.Next (Loc_Iter);
               end if;
            end loop;

         else
            while Loc_Iter /= Null_Iter loop
               Remove_Line (Kernel, Model, Loc_Iter);
               Model.Next (Loc_Iter);
            end loop;
         end if;

         if Line /= 0 then
            if Model.Children (File_Iter) = Null_Iter then
               Model.Remove (File_Iter);
            end if;

            return;
         end if;

         exit when not Removing_Category;

         Model.Next (File_Iter);
      end loop;

      if not Removing_Category then
         Parent := Model.Parent (Iter);

         Model.Set
           (Parent,
            Number_Of_Items_Column,
            Model.Get_Int (Parent, Number_Of_Items_Column)
              - Model.Get_Int (Iter, Number_Of_Items_Column));
      end if;

      Model.Remove (Iter);
   end Remove_Category_Or_File_Iter;

   -----------------
   -- Remove_Line --
   -----------------

   procedure Remove_Line
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Model      : not null access Gtk_Tree_Model_Record'Class;
      Loc_Iter   : Gtk_Tree_Iter)
   is
      File_Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                    Model.Parent (Loc_Iter);
      Mark      : constant Editor_Mark'Class :=
                    Get_Mark (Model, Loc_Iter, Mark_Column);

   begin
      Highlight_Line
        (Kernel,
         Get_File (Model, File_Iter),
         Mark.Line,
         Visible_Column_Type (Mark.Column),
         Integer (Get_Int (Model, Loc_Iter, Length_Column)),
         Get_Highlighting_Style (Model, Loc_Iter),
         False);

      if Mark /= Nil_Editor_Mark then
         Mark.Delete;
      end if;
   end Remove_Line;

end GPS.Location_Model;

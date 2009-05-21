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

with Glib.Object;
with Glib.Values;         use Glib.Values;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda; use GNATCOLL.VFS.GtkAda;
with GPS.Editors.GtkAda;  use GPS.Editors.GtkAda;

package body GPS.Location_Model is

   function To_Style is new Ada.Unchecked_Conversion
     (System.Address, GPS.Kernel.Styles.Style_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (Style_Access, System.Address);

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
     (Model              : Gtk_Tree_Store;
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
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf;
      Color              : access Gdk.Color.Gdk_Color := null)
   is
      type Gdk_Color_Access is access all Gdk.Color.Gdk_Color;

      function To_Proxy is
        new Ada.Unchecked_Conversion (Gdk_Color_Access, Gdk.C_Proxy);

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
         Model.Set (Iter, Color_Column, To_Proxy (Gdk_Color_Access (Color)));

      else
         Model.Set (Iter, Color_Column, Gdk.C_Proxy'(null));
      end if;
   end Fill_Iter;

   -----------------------
   -- Get_Category_File --
   -----------------------

   procedure Get_Category_File
     (Model           : Gtk_Tree_Store;
      Category        : Glib.UTF8_String;
      File            : GNATCOLL.VFS.Virtual_File;
      Category_Iter   : out Gtk_Tree_Iter;
      File_Iter       : out Gtk_Tree_Iter;
      New_Category    : out Boolean;
      Create          : Boolean;
      Category_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf;
      File_Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf;
      Color           : access Gdk.Color.Gdk_Color := null) is
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
               Category_Pixbuf, Color);
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
            False, null, File_Pixbuf, Color);
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

end GPS.Location_Model;

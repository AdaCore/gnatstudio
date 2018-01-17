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

with Ada.Strings.Unbounded;
with System.Address_To_Access_Conversions;
with Gtk.Tree_Model;                        use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;

package body GNATStack.CI_Models is

   use type Glib.Gint;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;
   use GNATStack.Data_Model;

   procedure Check_Stamp
     (Self : not null access CI_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Checks stamp of the iter.

   package Subprogram_Information_Conversion is
     new System.Address_To_Access_Conversions
       (GNATStack.Data_Model.Subprogram_Information);

   function Create_Iter
     (Self : not null access CI_Model_Record'Class;
      Item : GNATStack.Data_Model.Subprogram_Information_Access)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Creates Gtk+ iter.

   -----------------
   -- Check_Stamp --
   -----------------

   procedure Check_Stamp
     (Self : not null access CI_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      if Iter /= Gtk.Tree_Model.Null_Iter
        and then Self.Stamp /= Gtk.Tree_Model.Utils.Get_Stamp (Iter)
      then
         raise Program_Error;
      end if;
   end Check_Stamp;

   -----------------
   -- Create_Iter --
   -----------------

   function Create_Iter
     (Self : not null access CI_Model_Record'Class;
      Item : GNATStack.Data_Model.Subprogram_Information_Access)
      return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return
        Gtk.Tree_Model.Utils.Init_Tree_Iter
          (Self.Stamp,
           Subprogram_Information_Conversion.To_Address
             (Subprogram_Information_Conversion.Object_Pointer (Item)));
   end Create_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access CI_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return 2;
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access CI_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Name_Column =>
            return Glib.GType_String;

         when Bytes_Column =>
            return Glib.GType_Int;

         when others =>
            raise Program_Error;
            --  Should never be happen.
      end case;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access CI_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indicies : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);

   begin
      if Indicies'Length = 1
        and then Indicies (0) in 0 .. Glib.Gint (Self.Map.Length) - 1
      then
         return
           Create_Iter (Self, Self.Map.Element (Integer (Indicies (0)) + 1));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access CI_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Subprogram : constant Subprogram_Information_Access :=
                     Subprogram_At (Self, Iter);
      Path       : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      if Subprogram /= null then
         Gtk.Tree_Model.Gtk_New (Path);
         Gtk.Tree_Model.Append_Index
           (Path, Glib.Gint (Self.Map.Find_Index (Subprogram) - 1));
         return Path;
      end if;

      return Null_Gtk_Tree_Path;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access CI_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Subprogram : constant Subprogram_Information_Access :=
                     Self.Subprogram_At (Iter);

   begin
      case Column is
         when Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String
              (Value,
               Ada.Strings.Unbounded.To_String
                 (Subprogram.Identifier.Prefix_Name));

         when Bytes_Column =>
            Glib.Values.Init (Value, Glib.GType_Int);
            Glib.Values.Set_Int
              (Value, Glib.Gint (Subprogram.Local_Usage.Size));

         when others =>
            raise Program_Error;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item : out CI_Model;
      Set  : GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set) is
   begin
      Item := new CI_Model_Record;
      Initialize (Item, Set);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access CI_Model_Record'Class;
      Set  : GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set)
   is

      procedure Append
        (Position : Data_Model.Subprogram_Information_Ordered_Sets.Cursor);

      ------------
      -- Append --
      ------------

      procedure Append
        (Position : Data_Model.Subprogram_Information_Ordered_Sets.Cursor) is
      begin
         Self.Map.Append
           (Data_Model.Subprogram_Information_Ordered_Sets.Element (Position));
      end Append;

   begin
      Gtkada.Abstract_List_Model.Initialize (Self);
      Set.Iterate (Append'Access);
   end Initialize;

   --------------
   -- Inserted --
   --------------

   procedure Inserted
     (Self       : not null access CI_Model_Record'Class;
      Subprogram : GNATStack.Data_Model.Subprogram_Information_Access)
   is
      use GNATStack.Data_Model.Subprogram_Information_Vectors;

      Position : Data_Model.Subprogram_Information_Vectors.Cursor :=
                   Self.Map.First;
      Index    : Natural := 0;
      Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path     : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      Self.Stamp := Self.Stamp + 1;

      while Has_Element (Position) loop
         if Element_Is_Less (Element (Position), Subprogram)
           or else Equivalent_Elements (Element (Position), Subprogram)
         then
            Self.Map.Insert (Position, Subprogram);
            Index := To_Index (Position);

            exit;
         end if;

         Next (Position);
      end loop;

      if Index = 0 then
         Self.Map.Append (Subprogram);
         Index := Self.Map.Last_Index;
      end if;

      Iter := Create_Iter (Self, Subprogram);
      Path := Self.Get_Path (Iter);
      Row_Inserted (To_Interface (Self), Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Inserted;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access CI_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use Data_Model.Subprogram_Information_Vectors;

      Subprogram : constant Data_Model.Subprogram_Information_Access :=
                     Self.Subprogram_At (Iter);
      Position   : Data_Model.Subprogram_Information_Vectors.Cursor;

   begin
      if Subprogram /= null then
         Position := Self.Map.Find (Subprogram);
         Next (Position);

         if Has_Element (Position) then
            Iter := Create_Iter (Self, Element (Position));

         else
            Iter := Gtk.Tree_Model.Null_Iter;
         end if;

      else
         Iter := Gtk.Tree_Model.Null_Iter;
      end if;
   end Next;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access CI_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Glib.Gint (Self.Map.Length);

      else
         return 0;
      end if;
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access CI_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      Check_Stamp (Self, Parent);

      if Parent = Gtk.Tree_Model.Null_Iter
        and then N in 0 .. Glib.Gint (Self.Map.Length) - 1
      then
         return Create_Iter (Self, Self.Map.Element (Integer (N) + 1));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   -------------
   -- Removed --
   -------------

   procedure Removed
     (Self       : not null access CI_Model_Record'Class;
      Subprogram : GNATStack.Data_Model.Subprogram_Information_Access)
   is
      Index : constant Natural := Self.Map.Find_Index (Subprogram);
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      Gtk_New (Path);
      Self.Stamp := Self.Stamp + 1;
      Self.Map.Delete (Index);
      Gtk.Tree_Model.Append_Index (Path, Glib.Gint (Index - 1));
      Row_Deleted (To_Interface (Self), Path);
      Gtk.Tree_Model.Path_Free (Path);
   end Removed;

   -------------------
   -- Subprogram_At --
   -------------------

   function Subprogram_At
     (Self : not null access CI_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Data_Model.Subprogram_Information_Access is
   begin
      Check_Stamp (Self, Iter);

      if Iter /= Gtk.Tree_Model.Null_Iter then
         return
           Data_Model.Subprogram_Information_Access
             (Subprogram_Information_Conversion.To_Pointer
                  (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));

      else
         return null;
      end if;
   end Subprogram_At;

end GNATStack.CI_Models;

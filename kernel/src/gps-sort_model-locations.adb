-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with GNATCOLL.VFS.GtkAda;
with GPS.Kernel.Messages;

package body GPS.Sort_Model.Locations is

   use Glib;
   use Gtk.Tree_Model;
   use GNATCOLL.VFS;
   use GNATCOLL.VFS.GtkAda;
   use GPS.Kernel.Messages;

   type Compare_Functions is array (Positive range <>) of Compare_Function;

   function Compare
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter;
      Funcs : Compare_Functions) return Gint;
   --  General compare function, it compares A and B using Funcs till
   --  nonequvalence is reported or end of Funcs is reached.

   function Compare_In_Base_Name_Order
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint;
   --  Compare files of A and B in base name order.

   function Compare_In_Line_Column_Order
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint;
   --  Compare message of A and B in line:column order.

   function Compare_In_Weight_Order
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint;
   --  Compare nodes A and B in weight order.

   function Compare_In_Path_Order
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint;
   --  Compare A and B in path order.

   function Compare_By_Location
     (Self : not null access Gtk_Tree_Model_Record'Class;
      A    : Gtk_Tree_Iter;
      B    : Gtk_Tree_Iter) return Gint;
   --  Compares rows in locations order

   function Compare_By_Weight
     (Self : not null access Gtk_Tree_Model_Record'Class;
      A    : Gtk_Tree_Iter;
      B    : Gtk_Tree_Iter) return Gint;
   --  Compares rows in weight order first

   -------------
   -- Compare --
   -------------

   function Compare
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter;
      Funcs : Compare_Functions) return Gint
   is
      Result : Gint := 0;

   begin
      for J in Funcs'Range loop
         Result := Funcs (J) (Model, A, B);

         exit when Result /= 0;
      end loop;

      return Result;
   end Compare;

   -------------------------
   -- Compare_By_Location --
   -------------------------

   function Compare_By_Location
     (Self : not null access Gtk_Tree_Model_Record'Class;
      A    : Gtk_Tree_Iter;
      B    : Gtk_Tree_Iter) return Gint
   is
      Path  : constant Gtk_Tree_Path := Self.Get_Path (A);
      Depth : Natural;

   begin
      --  GtkTreeSortModel breaks underling order of equal rows, so return
      --  result of compare of last indices to save underling order.

      Depth := Natural (Get_Depth (Path));
      Path_Free (Path);

      if Depth = 2 then
         --  File level node

         case Sort_Order_Hint'Val (Self.Get_Int (A, Sort_Order_Hint_Column)) is
            when Chronological =>
               return Compare_In_Path_Order (Self, A, B);

            when Alphabetical =>
               return
                 Compare
                   (Self,
                    A,
                    B,
                    (Compare_In_Base_Name_Order'Access,
                     Compare_In_Path_Order'Access));
         end case;

      elsif Depth = 3 then
         --  Message level node

         return
           Compare
             (Self,
              A,
              B,
              (Compare_In_Line_Column_Order'Access,
               Compare_In_Path_Order'Access));

      else
         return Compare_In_Path_Order (Self, A, B);
      end if;
   end Compare_By_Location;

   -----------------------
   -- Compare_By_Weight --
   -----------------------

   function Compare_By_Weight
     (Self : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A    : Gtk.Tree_Model.Gtk_Tree_Iter;
      B    : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      Path     : constant Gtk_Tree_Path := Self.Get_Path (A);
      Depth    : Natural;

   begin
      --  GtkTreeSortModel breaks underling order of equal rows, so return
      --  result of compare of last indices to save underling order.

      Depth := Natural (Get_Depth (Path));
      Path_Free (Path);

      if Depth = 2 then
         --  File level node

         case Sort_Order_Hint'Val (Self.Get_Int (A, Sort_Order_Hint_Column)) is
            when Chronological =>
               return
                 Compare
                   (Self,
                    A,
                    B,
                    (Compare_In_Weight_Order'Access,
                     Compare_In_Path_Order'Access));

            when Alphabetical =>
               return
                 Compare
                   (Self,
                    A,
                    B,
                    (Compare_In_Weight_Order'Access,
                     Compare_In_Base_Name_Order'Access,
                     Compare_In_Path_Order'Access));
         end case;

      elsif Depth = 3 then
         --  Message level node

         return
           Compare
             (Self,
              A,
              B,
              (Compare_In_Weight_Order'Access,
               Compare_In_Line_Column_Order'Access,
               Compare_In_Path_Order'Access));

      else
         return Compare_In_Path_Order (Self, A, B);
      end if;
   end Compare_By_Weight;

   --------------------------------
   -- Compare_In_Base_Name_Order --
   --------------------------------

   function Compare_In_Base_Name_Order
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint
   is
      A_Name : constant Filesystem_String :=
                 Get_File (Model, A, File_Column).Base_Name;
      B_Name : constant Filesystem_String :=
                 Get_File (Model, B, File_Column).Base_Name;

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
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint
   is
      A_Line   : constant Gint := Model.Get_Int (A, Line_Column);
      A_Column : constant Gint := Model.Get_Int (A, Column_Column);
      B_Line   : constant Gint := Model.Get_Int (B, Line_Column);
      B_Column : constant Gint := Model.Get_Int (B, Column_Column);

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
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint
   is
      A_Path    : constant Gtk_Tree_Path := Model.Get_Path (A);
      B_Path    : constant Gtk_Tree_Path := Model.Get_Path (B);
      A_Indices : constant Gint_Array := Get_Indices (A_Path);
      B_Indices : constant Gint_Array := Get_Indices (B_Path);

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
     (Model : not null access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint
   is
      A_Weight : constant Gint := Model.Get_Int (A, Weight_Column);
      B_Weight : constant Gint := Model.Get_Int (B, Weight_Column);

   begin
      if A_Weight > B_Weight then
         return -1;

      elsif A_Weight = B_Weight then
         return 0;

      else
         return 1;
      end if;
   end Compare_In_Weight_Order;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model  : in out Locations_Proxy_Model;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model) is
   begin
      Model := new Locations_Proxy_Model_Record;
      GPS.Sort_Model.Locations.Initialize (Model, Source);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Locations_Proxy_Model_Record'Class;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model) is
   begin
      Self.Compare := Compare_By_Location'Access;
      GPS.Sort_Model.Initialize (Self, Source);
   end Initialize;

   ---------------
   -- Less_Than --
   ---------------

   overriding function Less_Than
     (Self  : not null access Locations_Proxy_Model_Record;
      Left  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Right : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean is
   begin
      return Self.Compare (Self.Source, Left, Right) < 0;
   end Less_Than;

   -------------------------
   -- Set_Locations_Order --
   -------------------------

   procedure Set_Locations_Order
     (Self : not null access Locations_Proxy_Model_Record'Class) is
   begin
      if Self.Compare /= Compare_By_Location'Access then
         Self.Compare := Compare_By_Location'Access;
         Self.Invalidate;
      end if;
   end Set_Locations_Order;

   ----------------------
   -- Set_Weight_Order --
   ----------------------

   procedure Set_Weight_Order
     (Self : not null access Locations_Proxy_Model_Record'Class) is
   begin
      if Self.Compare /= Compare_By_Weight'Access then
         Self.Compare := Compare_By_Weight'Access;
         Self.Invalidate;
      end if;
   end Set_Weight_Order;

end GPS.Sort_Model.Locations;

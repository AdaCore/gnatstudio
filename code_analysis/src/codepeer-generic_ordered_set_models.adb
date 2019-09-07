------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;

package body CodePeer.Generic_Ordered_Set_Models is

   package Category_Conversions is
     new System.Address_To_Access_Conversions (Item);

   ---------------
   -- All_Items --
   ---------------

   function All_Items
     (Self : access Ordered_Set_Model_Record'Class) return Item_Sets.Set is
   begin
      return Self.Items;
   end All_Items;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Ordered_Set_Model_Record) is
   begin
      Self.Items.Clear;
   end Clear;

   ----------------------
   -- Create_Tree_Iter --
   ----------------------

   function Create_Tree_Iter
     (Self : access Ordered_Set_Model_Record'Class;
      Item : Item_Access) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);

   begin
      if Item /= null then
         return
           Gtk.Tree_Model.Utils.Init_Tree_Iter
             (1,
              Category_Conversions.To_Address
                (Category_Conversions.Object_Pointer (Item)));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Create_Tree_Iter;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Ordered_Set_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
      Index   : Natural;
      Current : Item_Sets.Cursor := Self.Items.First;

   begin
      if Indices'Length = 1 then
         Index := Natural (Indices (Indices'First));

         while Index /= 0 loop
            Current := Item_Sets.Next (Current);
            Index := Index - 1;
         end loop;

         if Item_Sets.Has_Element (Current) then
            return Self.Create_Tree_Iter (Item_Sets.Element (Current));
         end if;
      end if;

      return Gtk.Tree_Model.Null_Iter;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Ordered_Set_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
   begin
      return Self.Get_Path (Self.Item_At (Iter));
   end Get_Path;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Self : access Ordered_Set_Model_Record;
      Item : Item_Access)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Result  : Gtk.Tree_Model.Gtk_Tree_Path;
      Index   : Natural := 0;
      Current : Item_Sets.Cursor := Self.Items.Find (Item);
   begin
      Gtk.Tree_Model.Gtk_New (Result);
      Current := Item_Sets.Previous (Current);

      while Item_Sets.Has_Element (Current) loop
         Index := Index + 1;
         Current := Item_Sets.Previous (Current);
      end loop;

      Gtk.Tree_Model.Append_Index (Result, Glib.Gint (Index));

      return Result;
   end Get_Path;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : access Ordered_Set_Model_Record'Class;
      Items : Item_Sets.Set) is
   begin
      Gtkada.Abstract_List_Model.Initialize (Self);
      Self.Items := Items;
   end Initialize;

   -------------
   -- Item_At --
   -------------

   function Item_At
     (Self : access Ordered_Set_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return
        Item_Access
          (Category_Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));
   end Item_At;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Ordered_Set_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      if Iter = Null_Iter then
         return Glib.Gint (Self.Items.Length);

      else
         return 0;
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Ordered_Set_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Current : Item_Sets.Cursor;

   begin
      Current := Self.Items.Find (Self.Item_At (Iter));
      Current := Item_Sets.Next (Current);

      if Item_Sets.Has_Element (Current) then
         Iter := Self.Create_Tree_Iter (Item_Sets.Element (Current));

      else
         Iter := Gtk.Tree_Model.Null_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Ordered_Set_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Parent);

      Index   : Natural := Natural (N);
      Current : Item_Sets.Cursor := Self.Items.First;

   begin
      while Index /= 0 loop
         Index := Index - 1;
         Current := Item_Sets.Next (Current);
      end loop;

      if Item_Sets.Has_Element (Current) then
         return Self.Create_Tree_Iter (Item_Sets.Element (Current));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
     (Self : access Ordered_Set_Model_Record'Class;
      Item : Item_Access)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Self.Create_Tree_Iter (Item);
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path := Self.Get_Path (Iter);

   begin
      Row_Changed (To_Interface (Self), Path, Iter);
      Gtk.Tree_Model.Path_Free (Path);
   end Row_Changed;

   ------------
   -- Update --
   ------------

   procedure Update (Self : access Ordered_Set_Model_Record'Class) is

      procedure Process (Position : Item_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Item_Sets.Cursor) is
      begin
         Self.Row_Changed (Item_Sets.Element (Position));
      end Process;

   begin
      Self.Items.Iterate (Process'Access);
   end Update;

end CodePeer.Generic_Ordered_Set_Models;

-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with System.Address_To_Access_Conversions;

with Gtk.Tree_Model.Utils;

with Code_Peer.Utilities;

package body Code_Peer.Entity_Messages_Models is

   package Category_Conversions is
     new System.Address_To_Access_Conversions (Code_Peer.Message_Category);

   -----------------
   -- Category_At --
   -----------------

   function Category_At
     (Self : access Entity_Messages_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Peer.Message_Category_Access
   is
      pragma Unreferenced (Self);

   begin
      return
        Code_Peer.Message_Category_Access
          (Category_Conversions.To_Pointer
               (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));
   end Category_At;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Entity_Messages_Model_Record'Class) is
   begin
      Self.Categories.Clear;
      Self.Tree_Node       := null;
      Self.Project_Node    := null;
      Self.File_Node       := null;
      Self.Subprogram_Node := null;
   end Clear;

   ----------------------
   -- Create_Tree_Iter --
   ----------------------

   function Create_Tree_Iter
     (Self     : access Entity_Messages_Model_Record'Class;
      Category : Code_Peer.Message_Category_Access)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);

   begin
      if Category /= null then
         return
           Gtk.Tree_Model.Utils.Init_Tree_Iter
             (1,
              Category_Conversions.To_Address
                (Category_Conversions.Object_Pointer (Category)));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Create_Tree_Iter;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Entity_Messages_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Category_Name_Column
            | Informational_Count_Column
            | Low_Count_Column
            | Medium_Count_Column
            | High_Count_Column
            | Suppressed_Count_Column
              =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Entity_Messages_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
      Index   : Natural;
      Current : Message_Category_Ordered_Sets.Cursor := Self.Categories.First;

   begin
      if Indices'Length = 1 then
         Index := Natural (Indices (Indices'First));

         while Index /= 0 loop
            Current := Message_Category_Ordered_Sets.Next (Current);
            Index := Index - 1;
         end loop;

         if Message_Category_Ordered_Sets.Has_Element (Current) then
            return
              Self.Create_Tree_Iter
                (Message_Category_Ordered_Sets.Element (Current));
         end if;
      end if;

      return Gtk.Tree_Model.Null_Iter;
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Entity_Messages_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Number_Of_Columns;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Entity_Messages_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Result  : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                  Gtk.Tree_Model.Gtk_New;
      Index   : Natural := 0;
      Current : Message_Category_Ordered_Sets.Cursor :=
                  Self.Categories.Find (Self.Category_At (Iter));

   begin
      Current := Message_Category_Ordered_Sets.Previous (Current);

      while Message_Category_Ordered_Sets.Has_Element (Current) loop
         Index := Index + 1;
         Current := Message_Category_Ordered_Sets.Previous (Current);
      end loop;

      Gtk.Tree_Model.Append_Index (Result, Glib.Gint (Index));

      return Result;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Entity_Messages_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use type Code_Analysis.Code_Analysis_Tree;
      use type Code_Analysis.Project_Access;
      use type Code_Analysis.File_Access;
      use type Code_Analysis.Subprogram_Access;

      procedure Set_Integer_Image (Item : Natural);

      procedure Set_Count_Image (Level : Code_Peer.Message_Probability_Level);

      ---------------------
      -- Set_Count_Image --
      ---------------------

      procedure Set_Count_Image
        (Level : Code_Peer.Message_Probability_Level) is
      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Self.Subprogram_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Subprogram_Node, Level, Self.Category_At (Iter)));

         elsif Self.File_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Self.File_Node, Level, Self.Category_At (Iter)));

         elsif Self.Project_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Project_Node, Level, Self.Category_At (Iter)));

         elsif Self.Tree_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Tree_Node, Level, Self.Category_At (Iter)));

         else
            Glib.Values.Set_String (Value, "");
         end if;
      end Set_Count_Image;

      -----------------------
      -- Set_Integer_Image --
      -----------------------

      procedure Set_Integer_Image (Item : Natural) is
         Image : constant String := Natural'Image (Item);

      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Item = 0 then
            Glib.Values.Set_String (Value, "");

         else
            Glib.Values.Set_String
              (Value, Image (Image'First + 1 .. Image'Last));
         end if;
      end Set_Integer_Image;

   begin
      case Column is
         when Category_Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String (Value, Self.Category_At (Iter).Name.all);

         when Informational_Count_Column =>
            Set_Count_Image (Code_Peer.Informational);

         when Low_Count_Column =>
            Set_Count_Image (Code_Peer.Low);

         when Medium_Count_Column =>
            Set_Count_Image (Code_Peer.Medium);

         when High_Count_Column =>
            Set_Count_Image (Code_Peer.High);

         when Suppressed_Count_Column =>
            Set_Count_Image (Code_Peer.Suppressed);

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model      : out Entity_Messages_Model;
      Categories : Code_Peer.Message_Category_Sets.Set) is
   begin
      Model := new Entity_Messages_Model_Record;
      Initialize (Model, Categories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : access Entity_Messages_Model_Record'Class;
      Categories : Code_Peer.Message_Category_Sets.Set)
   is

      procedure Process (Position : Code_Peer.Message_Category_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Peer.Message_Category_Sets.Cursor) is
         Category : constant Code_Peer.Message_Category_Access :=
                      Code_Peer.Message_Category_Sets.Element (Position);

      begin
         Self.Categories.Insert (Category);
      end Process;

   begin
      Gtkada.Abstract_List_Model.Initialize (Self);
      Categories.Iterate (Process'Access);
   end Initialize;

   ----------
   -- Less --
   ----------

   function Less
     (Left, Right : Code_Peer.Message_Category_Access) return Boolean is
   begin
      return Left.Name.all < Right.Name.all;
   end Less;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Entity_Messages_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint is
   begin
      if Gtk.Tree_Model.Utils.Is_Null (Iter) then
         return Glib.Gint (Self.Categories.Length);

      else
         return 0;
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Entity_Messages_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Current : Message_Category_Ordered_Sets.Cursor;

   begin
      Current := Self.Categories.Find (Self.Category_At (Iter));
      Current := Message_Category_Ordered_Sets.Next (Current);

      if Message_Category_Ordered_Sets.Has_Element (Current) then
         Iter :=
           Self.Create_Tree_Iter
             (Message_Category_Ordered_Sets.Element (Current));

      else
         Iter := Gtk.Tree_Model.Null_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Entity_Messages_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Parent);

      Index   : Natural := Natural (N);
      Current : Message_Category_Ordered_Sets.Cursor := Self.Categories.First;

   begin
      while Index /= 0 loop
         Index := Index - 1;
         Current := Message_Category_Ordered_Sets.Next (Current);
      end loop;

      if Message_Category_Ordered_Sets.Has_Element (Current) then
         return
           Self.Create_Tree_Iter
             (Message_Category_Ordered_Sets.Element (Current));

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Project_Access)
   is

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor) is
         Category : constant Code_Peer.Message_Category_Access :=
                      Message_Category_Ordered_Sets.Element (Position);
         Iter     : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Self.Create_Tree_Iter (Category);

      begin
         Self.Row_Changed (Self.Get_Path (Iter), Iter);
      end Process;

   begin
      Self.Tree_Node       := null;
      Self.Project_Node    := Entity;
      Self.File_Node       := null;
      Self.Subprogram_Node := null;
      Self.Categories.Iterate (Process'Access);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Code_Analysis_Tree)
   is

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor) is
         Category : constant Code_Peer.Message_Category_Access :=
                      Message_Category_Ordered_Sets.Element (Position);
         Iter     : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Self.Create_Tree_Iter (Category);

      begin
         Self.Row_Changed (Self.Get_Path (Iter), Iter);
      end Process;

   begin
      Self.Tree_Node       := Entity;
      Self.Project_Node    := null;
      Self.File_Node       := null;
      Self.Subprogram_Node := null;
      Self.Categories.Iterate (Process'Access);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.File_Access)
   is

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor) is
         Category : constant Code_Peer.Message_Category_Access :=
                      Message_Category_Ordered_Sets.Element (Position);
         Iter     : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Self.Create_Tree_Iter (Category);
      begin
         Self.Row_Changed (Self.Get_Path (Iter), Iter);
      end Process;

   begin
      Self.Tree_Node       := null;
      Self.Project_Node    := null;
      Self.File_Node       := Entity;
      Self.Subprogram_Node := null;
      Self.Categories.Iterate (Process'Access);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Subprogram_Access)
   is

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Category_Ordered_Sets.Cursor) is
         Category : constant Code_Peer.Message_Category_Access :=
                      Message_Category_Ordered_Sets.Element (Position);
         Iter     : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Self.Create_Tree_Iter (Category);

      begin
         Self.Row_Changed (Self.Get_Path (Iter), Iter);
      end Process;

   begin
      Self.Tree_Node       := null;
      Self.Project_Node    := null;
      Self.File_Node       := null;
      Self.Subprogram_Node := Entity;
      Self.Categories.Iterate (Process'Access);
   end Set;

end Code_Peer.Entity_Messages_Models;

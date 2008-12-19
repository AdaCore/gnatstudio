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

with Code_Peer.Utilities;

package body Code_Peer.Entity_Messages_Models is

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : access Entity_Messages_Model_Record) is
   begin
      Code_Peer.Message_Categories_Models.Message_Categories_Model_Record
        (Self.all).Clear;

      Self.Categories.Clear;
      Self.Tree_Node       := null;
      Self.Project_Node    := null;
      Self.File_Node       := null;
      Self.Subprogram_Node := null;
   end Clear;

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
      Code_Peer.Message_Categories_Models.Initialize (Self, Categories);
      Categories.Iterate (Process'Access);
   end Initialize;

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
      begin
         Self.Row_Changed (Message_Category_Ordered_Sets.Element (Position));
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
      begin
         Self.Row_Changed (Message_Category_Ordered_Sets.Element (Position));
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
      begin
         Self.Row_Changed (Message_Category_Ordered_Sets.Element (Position));
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
      begin
         Self.Row_Changed (Message_Category_Ordered_Sets.Element (Position));
      end Process;

   begin
      Self.Tree_Node       := null;
      Self.Project_Node    := null;
      Self.File_Node       := null;
      Self.Subprogram_Node := Entity;
      Self.Categories.Iterate (Process'Access);
   end Set;

end Code_Peer.Entity_Messages_Models;

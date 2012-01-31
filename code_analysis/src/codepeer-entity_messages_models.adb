------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with CodePeer.Utilities;

package body CodePeer.Entity_Messages_Models is

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : access Entity_Messages_Model_Record) is
   begin
      CodePeer.Message_Categories_Models.Message_Categories_Model_Record
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
      --  use type Code_Analysis.File_Access;
      --  ??? Uncomment this line after I120-013 will be fixed
      use type Code_Analysis.Subprogram_Access;

      procedure Set_Integer_Image (Item : Natural);

      procedure Set_Count_Image (Level : CodePeer.Message_Ranking_Level);

      ---------------------
      -- Set_Count_Image --
      ---------------------

      procedure Set_Count_Image (Level : CodePeer.Message_Ranking_Level) is
      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Self.Subprogram_Node /= null then
            Set_Integer_Image
              (CodePeer.Utilities.Compute_Messages_Count
                 (Self.Subprogram_Node, Level, Self.Category_At (Iter)));

         elsif Self.File_Node /= null then
            Set_Integer_Image
              (CodePeer.Utilities.Compute_Messages_Count
                 (Self.File_Node, Level, Self.Category_At (Iter)));

         elsif Self.Project_Node /= null then
            Set_Integer_Image
              (CodePeer.Utilities.Compute_Messages_Count
                 (Self.Project_Node, Level, Self.Category_At (Iter)));

         elsif Self.Tree_Node /= null then
            Set_Integer_Image
              (CodePeer.Utilities.Compute_Messages_Count
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
            Set_Count_Image (CodePeer.Informational);

         when Low_Count_Column =>
            Set_Count_Image (CodePeer.Low);

         when Medium_Count_Column =>
            Set_Count_Image (CodePeer.Medium);

         when High_Count_Column =>
            Set_Count_Image (CodePeer.High);

         when Suppressed_Count_Column =>
            Set_Count_Image (CodePeer.Suppressed);

         when Message_Category_Column =>
            Glib.Values.Init (Value, Glib.GType_Pointer);
            Glib.Values.Set_Address
              (Value, Self.Category_At (Iter).all'Address);

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model      : out Entity_Messages_Model;
      Categories : CodePeer.Message_Category_Sets.Set) is
   begin
      Model := new Entity_Messages_Model_Record;
      Initialize (Model, Categories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : access Entity_Messages_Model_Record'Class;
      Categories : CodePeer.Message_Category_Sets.Set)
   is

      procedure Process (Position : CodePeer.Message_Category_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : CodePeer.Message_Category_Sets.Cursor) is
         Category : constant CodePeer.Message_Category_Access :=
                      CodePeer.Message_Category_Sets.Element (Position);

      begin
         Self.Categories.Insert (Category);
      end Process;

   begin
      CodePeer.Message_Categories_Models.Initialize (Self, Categories);
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

end CodePeer.Entity_Messages_Models;

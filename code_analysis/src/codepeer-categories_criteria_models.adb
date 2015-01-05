------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with Histories;

package body CodePeer.Categories_Criteria_Models is

   function History_Key
     (Self     : not null access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access)
      return Histories.History_Key;
   --  Constructs history key for specified category

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : access Categories_Criteria_Model_Record) is
   begin
      CodePeer.Message_Categories_Models.Message_Categories_Model_Record
        (Self.all).Clear;

      Self.Selected_Categories.Clear;
   end Clear;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Categories_Criteria_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Active_Column =>
            return Glib.GType_Boolean;

         when Name_Column =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Categories_Criteria_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Column_Count;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Categories_Criteria_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
   begin
      case Column is
         when Active_Column =>
            Glib.Values.Init (Value, Glib.GType_Boolean);
            Glib.Values.Set_Boolean
              (Value, Self.Selected_Categories.Contains
                 (Self.Category_At (Iter)));

         when Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String (Value, Self.Category_At (Iter).Name.all);

         when others =>
            Glib.Values.Init (Value, Glib.GType_Invalid);
      end case;
   end Get_Value;

   ----------------------------
   -- Get_Visible_Categories --
   ----------------------------

   function Get_Visible_Categories
     (Self : access Categories_Criteria_Model_Record'Class)
      return CodePeer.Message_Category_Sets.Set is
   begin
      return Self.Selected_Categories;
   end Get_Visible_Categories;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model          : in out Categories_Criteria_Model;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set) is
   begin
      Model := new Categories_Criteria_Model_Record;

      Initialize (Model, Kernel, History_Prefix, Categories);
   end Gtk_New;

   ----------
   -- Hide --
   ----------

   procedure Hide
     (Self     : access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access)
   is
   begin
      Self.Selected_Categories.Exclude (Category);
      Histories.Set_History
        (Self.Kernel.Get_History.all, Self.History_Key (Category), False);

      Self.Row_Changed (Category);
   end Hide;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All (Self : access Categories_Criteria_Model_Record'Class) is

      procedure Process
        (Position : CodePeer.Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process
        (Position : CodePeer.Message_Category_Ordered_Sets.Cursor)
      is
         Category : constant CodePeer.Message_Category_Access :=
           CodePeer.Message_Category_Ordered_Sets.Element (Position);

      begin
         if Self.Selected_Categories.Contains (Category) then
            Self.Hide (Category);
         end if;
      end Process;

   begin
      Self.All_Categories.Iterate (Process'Access);
   end Hide_All;

   -----------------
   -- History_Key --
   -----------------

   function History_Key
     (Self     : not null access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access)
      return Histories.History_Key is
   begin
      return
        Histories.History_Key
          (Ada.Strings.Unbounded.To_String (Self.History_Prefix)
             & '-'
             & Category.Name.all);
   end History_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : access Categories_Criteria_Model_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Categories     : CodePeer.Message_Category_Sets.Set)
   is
      procedure Restore (Position : CodePeer.Message_Category_Sets.Cursor);
      --  Restores previous state from history

      -------------
      -- Restore --
      -------------

      procedure Restore (Position : CodePeer.Message_Category_Sets.Cursor) is
         Category : constant CodePeer.Message_Category_Access
           := CodePeer.Message_Category_Sets.Element (Position);

      begin
         Histories.Create_New_Boolean_Key_If_Necessary
           (Self.Kernel.Get_History.all, Self.History_Key (Category), True);

         if Histories.Get_History
             (Self.Kernel.Get_History.all, Self.History_Key (Category))
         then
            Self.Selected_Categories.Insert (Category);
         end if;
      end Restore;

   begin
      CodePeer.Message_Categories_Models.Initialize (Self, Categories);
      Self.Kernel := Kernel;
      Self.History_Prefix :=
        Ada.Strings.Unbounded.To_Unbounded_String (History_Prefix);
      Categories.Iterate (Restore'Access);
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Self : access Categories_Criteria_Model_Record'Class) return Boolean is
   begin
      return Self.Selected_Categories.Is_Empty;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full
     (Self : access Categories_Criteria_Model_Record'Class) return Boolean
   is
      use type Ada.Containers.Count_Type;

   begin
      return Self.All_Categories.Length = Self.Selected_Categories.Length;
   end Is_Full;

   ----------
   -- Show --
   ----------

   procedure Show
     (Self     : access Categories_Criteria_Model_Record'Class;
      Category : CodePeer.Message_Category_Access)
   is
   begin
      Self.Selected_Categories.Include (Category);
      Histories.Set_History
        (Self.Kernel.Get_History.all, Self.History_Key (Category), True);

      Self.Row_Changed (Category);
   end Show;

   --------------
   -- Show_All --
   --------------

   procedure Show_All (Self : access Categories_Criteria_Model_Record'Class) is

      procedure Process
        (Position : CodePeer.Message_Category_Ordered_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process
        (Position : CodePeer.Message_Category_Ordered_Sets.Cursor)
      is
         Category : constant CodePeer.Message_Category_Access :=
           CodePeer.Message_Category_Ordered_Sets.Element (Position);

      begin
         if not Self.Selected_Categories.Contains (Category) then
            Self.Show (Category);
         end if;
      end Process;

   begin
      Self.All_Categories.Iterate (Process'Access);
   end Show_All;

end CodePeer.Categories_Criteria_Models;

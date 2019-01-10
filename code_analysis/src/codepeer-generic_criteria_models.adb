------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Histories;

package body CodePeer.Generic_Criteria_Models is

   function History_Key
     (Self : not null access Criteria_Model_Record'Class;
      Item : Item_Access) return Histories.History_Key;
   --  Constructs history key for specified category

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : access Criteria_Model_Record) is
   begin
      Ordered_Set_Models.Ordered_Set_Model_Record (Self.all).Clear;
      Self.Selected_Items.Clear;
   end Clear;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Criteria_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Active_Column =>
            return Glib.GType_Boolean;

         when Name_Column =>
            return Glib.GType_String;

         when Tooltip_Column =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Criteria_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Column_Count;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Criteria_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
   begin
      case Column is
         when Active_Column =>
            Glib.Values.Init (Value, Glib.GType_Boolean);
            Glib.Values.Set_Boolean
              (Value, Self.Selected_Items.Contains (Self.Item_At (Iter)));

         when Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String (Value, Get_Name (Self.Item_At (Iter).all));

         when Tooltip_Column =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String
              (Value, Get_Tooltip (Self.Item_At (Iter).all));

         when others =>
            Glib.Values.Init (Value, Glib.GType_Invalid);
      end case;
   end Get_Value;

   -----------------------
   -- Get_Visible_Items --
   -----------------------

   function Get_Visible_Items
     (Self : access Criteria_Model_Record'Class) return Item_Sets.Set is
   begin
      return Self.Selected_Items;
   end Get_Visible_Items;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model          : out Criteria_Model;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean) is
   begin
      Model := new Criteria_Model_Record;
      Initialize (Model, Kernel, History_Prefix, Items, Default);
   end Gtk_New;

   ----------
   -- Hide --
   ----------

   procedure Hide
     (Self : access Criteria_Model_Record'Class;
      Item : Item_Access) is
   begin
      Self.Selected_Items.Exclude (Item);
      Histories.Set_History
        (Self.Kernel.Get_History.all, Self.History_Key (Item), False);

      Self.Row_Changed (Item);
   end Hide;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All (Self : access Criteria_Model_Record'Class) is

      procedure Process (Position : Item_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Item_Sets.Cursor) is
         Item : constant Item_Access := Item_Sets.Element (Position);

      begin
         if Self.Selected_Items.Contains (Item) then
            Self.Hide (Item);
         end if;
      end Process;

   begin
      Self.All_Items.Iterate (Process'Access);
   end Hide_All;

   -----------------
   -- History_Key --
   -----------------

   function History_Key
     (Self : not null access Criteria_Model_Record'Class;
      Item : Item_Access) return Histories.History_Key is
   begin
      return
        Histories.History_Key
          (Ada.Strings.Unbounded.To_String (Self.History_Prefix)
             & '-'
             & Get_Name (Item.all));
   end History_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : access Criteria_Model_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean)
   is
      procedure Restore (Position : Item_Sets.Cursor);
      --  Restores previous state from history

      -------------
      -- Restore --
      -------------

      procedure Restore (Position : Item_Sets.Cursor) is
         Item : constant Item_Access := Item_Sets.Element (Position);

      begin
         Histories.Create_New_Boolean_Key_If_Necessary
           (Self.Kernel.Get_History.all, Self.History_Key (Item), Default);

         if Histories.Get_History
             (Self.Kernel.Get_History.all, Self.History_Key (Item))
         then
            Self.Selected_Items.Insert (Item);
         end if;
      end Restore;

   begin
      Ordered_Set_Models.Initialize (Self, Items);
      Self.Kernel := Kernel;
      Self.History_Prefix :=
        Ada.Strings.Unbounded.To_Unbounded_String (History_Prefix);
      Items.Iterate (Restore'Access);
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Self : access Criteria_Model_Record'Class) return Boolean is
   begin
      return Self.Selected_Items.Is_Empty;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full
     (Self : access Criteria_Model_Record'Class) return Boolean
   is
      use type Ada.Containers.Count_Type;

   begin
      return Self.All_Items.Length = Self.Selected_Items.Length;
   end Is_Full;

   ----------
   -- Show --
   ----------

   procedure Show
     (Self : access Criteria_Model_Record'Class;
      Item : Item_Access) is
   begin
      Self.Selected_Items.Include (Item);
      Histories.Set_History
        (Self.Kernel.Get_History.all, Self.History_Key (Item), True);

      Self.Row_Changed (Item);
   end Show;

   --------------
   -- Show_All --
   --------------

   procedure Show_All (Self : access Criteria_Model_Record'Class) is

      procedure Process (Position : Item_Sets.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Item_Sets.Cursor) is
         Item : constant Item_Access := Item_Sets.Element (Position);

      begin
         if not Self.Selected_Items.Contains (Item) then
            Self.Show (Item);
         end if;
      end Process;

   begin
      Self.All_Items.Iterate (Process'Access);
   end Show_All;

end CodePeer.Generic_Criteria_Models;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

private with Glib.Values;
private with Gtk.Tree_Model;

with CodePeer.Generic_Ordered_Set_Models;

generic
   type Item is limited private;
   type Item_Access is access all Item;
   --  Criteria of filter

   with function Get_Name (Self : Item) return String;
   --  Should return name of given criteria

   with function Get_Tooltip (Self : Item) return String;
   --  Should return tooltip to be displayed for given criteria

   with function "<" (Left : Item_Access; Right : Item_Access) return Boolean;
   with package Item_Sets is new Ada.Containers.Ordered_Sets (Item_Access);
   with package Ordered_Set_Models is
     new CodePeer.Generic_Ordered_Set_Models
       (Item, Item_Access, "<", Item_Sets);
   --  Containers of criterias

package CodePeer.Generic_Criteria_Models is

   Active_Column  : constant := 0;
   Name_Column    : constant := 1;
   Tooltip_Column : constant := 2;
   Column_Count   : constant := 3;

   type Criteria_Model_Record is
     new Ordered_Set_Models.Ordered_Set_Model_Record with private;

   type Criteria_Model is access all Criteria_Model_Record'Class;

   procedure Gtk_New
     (Model          : out Criteria_Model;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);
   --  Creates new instance. History_Prefix is a prefix to manage persistent
   --  state of selected categories.

   procedure Initialize
     (Self           : access Criteria_Model_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);

   procedure Show
     (Self : access Criteria_Model_Record'Class;
      Item : Item_Access);

   procedure Hide
     (Self : access Criteria_Model_Record'Class;
      Item : Item_Access);

   procedure Show_All (Self : access Criteria_Model_Record'Class);

   procedure Hide_All (Self : access Criteria_Model_Record'Class);

   function Get_Visible_Items
     (Self : access Criteria_Model_Record'Class) return Item_Sets.Set;

   function Is_Empty
     (Self : access Criteria_Model_Record'Class) return Boolean;
   --  Returns True if there are no selected items in the model

   function Is_Full (Self : access Criteria_Model_Record'Class) return Boolean;
   --  Returns True is all items is selected

   overriding procedure Clear (Self : access Criteria_Model_Record);

private

   type Criteria_Model_Record is
     new Ordered_Set_Models.Ordered_Set_Model_Record
   with record
      Kernel         : GPS.Kernel.Kernel_Handle;
      History_Prefix : Ada.Strings.Unbounded.Unbounded_String;
      Selected_Items : Item_Sets.Set;
   end record;

   overriding function Get_N_Columns
     (Self : access Criteria_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Criteria_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding procedure Get_Value
     (Self   : access Criteria_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

end CodePeer.Generic_Criteria_Models;

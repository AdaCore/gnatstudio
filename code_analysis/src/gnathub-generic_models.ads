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

with Ada.Containers.Ordered_Sets;

with Glib;                                use Glib;
with Glib.Values;
with Gtk.Widget;

with GPS.Kernel;
with CodePeer.Generic_Ordered_Set_Models;

private with Ada.Strings.Unbounded;
private with Gtk.Tree_Model;

generic
   type Item is limited private;
   type Item_Access is access all Item;

   Columns : Glib.GType_Array;
   --  Types of columns

   with procedure Get_Value
     (Self   : Item_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Should set column's value in to Value parameter

   with function Get_History_Name
     (Self  : Item;
      View  : Gtk.Widget.Gtk_Widget)
      return String;
   --  Should return name for history of given item

   with function "<" (Left : Item_Access; Right : Item_Access) return Boolean;
   with package Item_Sets is new Ada.Containers.Ordered_Sets (Item_Access);
   --  Containers of items

package GNAThub.Generic_Models is

   Active_Column : constant Glib.Gint := Glib.Gint (Columns'Last + 1);

   package Ordered_Set_Models is
     new CodePeer.Generic_Ordered_Set_Models
       (Item, Item_Access, "<", Item_Sets);

   type Criteria_Model_Record is
     new Ordered_Set_Models.Ordered_Set_Model_Record with private;

   type Criteria_Model is access all Criteria_Model_Record'Class;

   procedure Gtk_New
     (Model          : in out Criteria_Model;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean);
   --  Creates new instance. History_Prefix is a prefix to manage persistent
   --  state of selected categories.
   --  Default is default value for history of items which don't have history.

   procedure Initialize
     (Self           : access Criteria_Model_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
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
      View           : Gtk.Widget.Gtk_Widget;
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

end GNAThub.Generic_Models;

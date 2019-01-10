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

--  This file contains an abstract implementation of the Gtk+ tree model for
--  the message categories. Derived model must override Get_N_Columns,
--  Get_Column_Type and Get_Value subprograms.

with Glib;
with Gtk.Tree_Model;
with Gtkada.Abstract_List_Model;

generic
   type Item is limited private;
   type Item_Access is access all Item;
   with function "<" (Left : Item_Access; Right : Item_Access) return Boolean;
   with package Item_Sets is new Ada.Containers.Ordered_Sets (Item_Access);

package CodePeer.Generic_Ordered_Set_Models is

   type Ordered_Set_Model_Record is abstract
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with
       private;

   procedure Initialize
     (Self  : access Ordered_Set_Model_Record'Class;
      Items : Item_Sets.Set);

   function Item_At
     (Self : access Ordered_Set_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Item_Access;

   function All_Items
     (Self : access Ordered_Set_Model_Record'Class) return Item_Sets.Set;

   procedure Clear (Self : access Ordered_Set_Model_Record);

   procedure Update (Self : access Ordered_Set_Model_Record'Class);

   procedure Row_Changed
     (Self : access Ordered_Set_Model_Record'Class;
      Item : Item_Access);
   --  Emit "row_changed" signal

   function Get_Path
     (Self : access Ordered_Set_Model_Record;
      Item : Item_Access)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Return item's path. Path sould be released by caller.

   --  GtkTreeModel operations

   overriding function Get_Iter
     (Self : access Ordered_Set_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Ordered_Set_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Next
     (Self : access Ordered_Set_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function N_Children
     (Self : access Ordered_Set_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Ordered_Set_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

private

   type Ordered_Set_Model_Record is abstract
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record
   with record
      Items : Item_Sets.Set;
   end record;

   function Create_Tree_Iter
     (Self : access Ordered_Set_Model_Record'Class;
      Item : Item_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;

end CodePeer.Generic_Ordered_Set_Models;

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

--  This file contains an abstract implementation of the Gtk+ tree model for
--  the message categories. Derived model must override Get_N_Columns,
--  Get_Column_Type and Get_Value subprograms.

with Glib;
with Gtk.Tree_Model;
with Gtkada.Abstract_List_Model;

package CodePeer.Message_Categories_Models is

   type Message_Categories_Model_Record is abstract
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with
       private;

   procedure Initialize
     (Self       : access Message_Categories_Model_Record'Class;
      Categories : CodePeer.Message_Category_Sets.Set);

   function Category_At
     (Self : access Message_Categories_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return CodePeer.Message_Category_Access;

   function All_Categories
     (Self : access Message_Categories_Model_Record'Class)
      return CodePeer.Message_Category_Ordered_Sets.Set;

   procedure Clear (Self : access Message_Categories_Model_Record);

   procedure Update (Self : access Message_Categories_Model_Record'Class);

   procedure Row_Changed
     (Self     : access Message_Categories_Model_Record'Class;
      Category : CodePeer.Message_Category_Access);
   --  Emit "row_changed" signal

   --  GtkTreeModel operations

   overriding function Get_Iter
     (Self : access Message_Categories_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Message_Categories_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Next
     (Self : access Message_Categories_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function N_Children
     (Self : access Message_Categories_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Message_Categories_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

private

   type Message_Categories_Model_Record is abstract
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record
   with record
      Categories : Message_Category_Ordered_Sets.Set;
   end record;

   function Create_Tree_Iter
     (Self     : access Message_Categories_Model_Record'Class;
      Category : CodePeer.Message_Category_Access)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

end CodePeer.Message_Categories_Models;

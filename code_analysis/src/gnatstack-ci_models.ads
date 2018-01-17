------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Glib.Values;
with Gtk.Tree_Model;
with Gtkada.Abstract_List_Model;

with GNATStack.Data_Model;

package GNATStack.CI_Models is

   type CI_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record
       with private;

   type CI_Model is access all CI_Model_Record'Class;

   procedure Gtk_New
     (Item : out CI_Model;
      Set  : GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set);

   procedure Initialize
     (Self : not null access CI_Model_Record'Class;
      Set  : GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set);

   function Subprogram_At
     (Self : not null access CI_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return GNATStack.Data_Model.Subprogram_Information_Access;
   --  Converts Iter to subprogram.

   procedure Removed
     (Self       : not null access CI_Model_Record'Class;
      Subprogram : GNATStack.Data_Model.Subprogram_Information_Access);
   --  Notify model that subprogram is removed.

   procedure Inserted
     (Self       : not null access CI_Model_Record'Class;
      Subprogram : GNATStack.Data_Model.Subprogram_Information_Access);
   --  Notify model that subprogram is inserted.

   Name_Column  : constant Glib.Gint;
   Bytes_Column : constant Glib.Gint;

private

   type CI_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with
   record
      Stamp : Glib.Gint := 1;
      Map   : GNATStack.Data_Model.Subprogram_Information_Vectors.Vector;
   end record;

   overriding function Get_N_Columns
     (Self : access CI_Model_Record) return Glib.Gint;
   --  Override this to return the number of columns supported by Tree_Model.

   overriding function Get_Column_Type
     (Self  : access CI_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  Override this to return the type of the Index-th column in the model.

   overriding function Get_Iter
     (Self : access CI_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   overriding function Get_Path
     (Self : access CI_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Override this to return a newly created Gtk_Tree_Path referenced by
   --  Iter. This path will be freed with Path_Free by the caller.

   overriding procedure Get_Value
     (Self   : access CI_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Override this get a value from the model, at column Column and line
   --  Iter. Value must be freed by the caller.

   overriding procedure Next
     (Self : access CI_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Override this to set Iter to point to the node following it at the
   --  current level. If there is none, Iter is set to Null_Iter.

   overriding function N_Children
     (Self : access CI_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;
   --  Override this to return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   overriding function Nth_Child
     (Self   : access CI_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter. If Parent is Null_Iter, then the nth root node is set.

   Name_Column  : constant Glib.Gint := 0;
   Bytes_Column : constant Glib.Gint := 1;

end GNATStack.CI_Models;

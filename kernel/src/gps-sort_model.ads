------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
private with Ada.Containers.Vectors;

private with Glib.Values;
with Gtk.Tree_Model;
private with Gtkada.Abstract_Tree_Model;

package GPS.Sort_Model is

   type GPS_Sort_Model_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with private;

   type GPS_Sort_Model is access all GPS_Sort_Model_Record'Class;

   procedure Initialize
     (Self   : not null access GPS_Sort_Model_Record'Class;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Invalidate (Self : not null access GPS_Sort_Model_Record'Class);

   function Map_To_Source
     (Self       : not null access GPS_Sort_Model_Record'Class;
      Proxy_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Map_To_Proxy
     (Self        : not null access GPS_Sort_Model_Record'Class;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Less_Than
     (Self  : not null access GPS_Sort_Model_Record;
      Left  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Right : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

private

   type Node_Record;
   type Node_Access is access all Node_Record;

   subtype Valid_Index_Subtype is Glib.Gint range 0 .. Glib.Gint'Last;

   package Node_Vectors is
     new Ada.Containers.Vectors (Valid_Index_Subtype, Node_Access);

   type Node_Record is record
      Parent      : Node_Access;
      From_Proxy  : Node_Vectors.Vector;
      From_Source : Node_Vectors.Vector;
   end record;

   type GPS_Sort_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with record
      Stamp  : Glib.Gint;
      Source : Gtk.Tree_Model.Gtk_Tree_Model;
      Root   : Node_Access;
   end record;

   overriding function Get_N_Columns
     (Self : access GPS_Sort_Model_Record)
      return Glib.Gint;
   --  Override this to return the number of columns supported by Tree_Model.

   overriding function Get_Column_Type
     (Self  : access GPS_Sort_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  Override this to return the type of the Index-th column in the model.

   overriding function Get_Iter
     (Self : access GPS_Sort_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   overriding function Get_Path
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Override this to return a newly created Gtk_Tree_Path referenced by
   --  Iter. This path will be freed with Path_Free by the caller.

   overriding procedure Get_Value
     (Self   : access GPS_Sort_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Override this get a value from the model, at column Column and line
   --  Iter. Value must be freed by the caller.

   overriding procedure Next
     (Self : access GPS_Sort_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Override this to set Iter to point to the node following it at the
   --  current level. If there is none, Iter is set to Null_Iter.

   overriding function Children
     (Self   : access GPS_Sort_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the first child of Parent. If Parent has no
   --  children, return Null_Iter. Parent will remain a valid node after this
   --  function has been called.

   overriding function Has_Child
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Override this to return True if Iter has children, False otherwise.

   overriding function N_Children
     (Self : access GPS_Sort_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;
   --  Override this to return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   overriding function Nth_Child
     (Self   : access GPS_Sort_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter. If Parent is Null_Iter, then the nth root node is set.

   overriding function Parent
     (Self  : access GPS_Sort_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the parent of Child. If Child is at the
   --  toplevel, and doesn't have a parent, then Null_Iter is returned.

end GPS.Sort_Model;

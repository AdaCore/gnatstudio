-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides classic Category/File/Message Gtk+ model to
--  be used in user interface.

private with Glib.Values;
private with Gtk.Tree_Model;

private package GPS.Kernel.Messages.Classic_Models is

   type Classic_Tree_Model_Record is
     new Abstract_Messages_Tree_Model_Record with private;

   type Classic_Tree_Model is access all Classic_Tree_Model_Record'Class;

   procedure Initialize
     (Self : access Classic_Tree_Model_Record'Class);
   --  Initialize object and connect it with messages container of the
   --  specified kernel.

   procedure Gtk_New
     (Object    : out Classic_Tree_Model;
      Container : not null access Messages_Container'Class);
   --  Allocates new object.

private

   type Classic_Tree_Model_Record is
     new Abstract_Messages_Tree_Model_Record with record
      Stamp : Glib.Gint := 0;
   end record;

   procedure Node_Added
     (Self : not null access Classic_Tree_Model_Record;
      Node : not null Node_Access);
   --  Common implementation of node adding handling, it handles items of all
   --  levels. Category_Added and File_Added just a renaming of Node_Added;
   --  Message_Added can't be renamed due to difference in subprogram's
   --  profile.

   overriding procedure Category_Added
     (Self     : not null access Classic_Tree_Model_Record;
      Category : not null Node_Access) renames Node_Added;

   overriding procedure Category_Removed
     (Self  : not null access Classic_Tree_Model_Record;
      Index : Positive);

   overriding procedure File_Added
     (Self : not null access Classic_Tree_Model_Record;
      File : not null Node_Access) renames Node_Added;

   overriding procedure File_Removed
     (Self     : not null access Classic_Tree_Model_Record;
      Category : not null Node_Access;
      Index    : Positive);

   overriding procedure Message_Added
     (Self    : not null access Classic_Tree_Model_Record;
      Message : not null Message_Access);

   overriding procedure Message_Removed
     (Self  : not null access Classic_Tree_Model_Record;
      File  : not null Node_Access;
      Index : Positive);

   overriding function Get_N_Columns
     (Self : access Classic_Tree_Model_Record) return Glib.Gint;
   --  Override this to return the number of columns supported by Tree_Model.

   overriding function Get_Column_Type
     (Self  : access Classic_Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  Override this to return the type of the Index-th column in the model.

   overriding function Get_Iter
     (Self : access Classic_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   overriding function Get_Path
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Override this to return a newly created Gtk_Tree_Path referenced by
   --  Iter. This path will be freed with Path_Free by the caller.

   overriding procedure Get_Value
     (Self   : access Classic_Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Override this get a value from the model, at column Column and line
   --  Iter. Value must be freed by the caller.

   overriding procedure Next
     (Self : access Classic_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Override this to set Iter to point to the node following it at the
   --  current level. If there is none, Iter is set to Null_Iter.

   overriding function Children
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the first child of Parent. If Parent has no
   --  children, return Null_Iter. Parent will remain a valid node after this
   --  function has been called.

   overriding function Has_Child
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Override this to return True if Iter has children, False otherwise.

   overriding function N_Children
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;
   --  Override this to return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   overriding function Nth_Child
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter. If Parent is Null_Iter, then the nth root node is set.

   overriding function Parent
     (Self  : access Classic_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Override this to return the parent of Child. If Child is at the
   --  toplevel, and doesn't have a parent, then Null_Iter is returned.

end GPS.Kernel.Messages.Classic_Models;

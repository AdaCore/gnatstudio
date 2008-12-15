-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  This file contains an abstract implementation of the Gtk+ tree model for
--  the code analysis module. Derived model must override Get_N_Columns,
--  Get_Column_Type and Get_Value subprograms.

with Glib;
with Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model;

package Code_Analysis.Tree_Models is

   type Simple_Tree_Model_Record is abstract
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with
       private;

   procedure Initialize
     (Self : access Simple_Tree_Model_Record'Class;
      Tree : Code_Analysis.Code_Analysis_Tree);

   function Project_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Project_Access;
   --  Returns Project at the specified position.

   function File_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.File_Access;
   --  Returns File at the specified position.

   function Subprogram_At
     (Self : access Simple_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Subprogram_Access;
   --  Returns Subprogram at the specified position.

   function Create_Tree_Iter
     (Self            : access Simple_Tree_Model_Record'Class;
      Project_Node    : Code_Analysis.Project_Access    := null;
      File_Node       : Code_Analysis.File_Access       := null;
      Subprogram_Node : Code_Analysis.Subprogram_Access := null)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Creates model specific GtkTreeIter object.

   --  GtkTreeModel subprograms

   overriding function Get_Iter
     (Self : access Simple_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Next
     (Self : access Simple_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function Children
     (Self   : access Simple_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Has_Child
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   overriding function N_Children
     (Self : access Simple_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Simple_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Parent
     (Self  : access Simple_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

private

   type Simple_Tree_Model_Record is abstract
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with record
      Tree : Code_Analysis.Code_Analysis_Tree;
   end record;

end Code_Analysis.Tree_Models;

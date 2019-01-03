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
--  the code analysis module. Derived model must override Get_N_Columns,
--  Get_Column_Type and Get_Value subprograms.

private with Ada.Containers.Vectors;

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

   --  Filterable tree model

   type Project_Item (Node : Code_Analysis.Project_Access) is tagged private;
   type Project_Item_Access is access all Project_Item'Class;

   type File_Item (Node : Code_Analysis.File_Access) is tagged private;
   type File_Item_Access is access all File_Item'Class;

   type Subprogram_Item (Node : Code_Analysis.Subprogram_Access) is
     tagged private;
   type Subprogram_Item_Access is access all Subprogram_Item'Class;

   type Filterable_Tree_Model_Record is abstract
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with
       private;

   procedure Initialize
     (Self : access Filterable_Tree_Model_Record'Class;
      Tree : Code_Analysis.Code_Analysis_Tree);

   function Is_Visible
     (Self    : access Filterable_Tree_Model_Record;
      Project : Project_Item_Access) return Boolean is abstract;
   --  Returns True if specified project must be visible in the tree

   function Is_Visible
     (Self    : access Filterable_Tree_Model_Record;
      Project : Project_Item_Access;
      File    : File_Item_Access) return Boolean is abstract;
   --  Returns True if specified file must be visible in the tree

   function Is_Visible
     (Self       : access Filterable_Tree_Model_Record;
      Project    : Project_Item_Access;
      File       : File_Item_Access;
      Subprogram : Subprogram_Item_Access) return Boolean is abstract;
   --  Returns True if specified subprogram must be visible in the tree

   function Is_Changed
     (Self    : access Filterable_Tree_Model_Record;
      Project : Project_Item_Access) return Boolean is abstract;
   --  Returns True if specified project data has been changed

   function Is_Changed
     (Self    : access Filterable_Tree_Model_Record;
      Project : Project_Item_Access;
      File    : File_Item_Access) return Boolean is abstract;
   --  Returns True if specified file data has been changed

   function Is_Changed
     (Self       : access Filterable_Tree_Model_Record;
      Project    : Project_Item_Access;
      File       : File_Item_Access;
      Subprogram : Subprogram_Item_Access) return Boolean is abstract;
   --  Returns True if specified subprogram data has been changed

   procedure Reconstruct (Self : access Filterable_Tree_Model_Record'Class);
   --  Reconstructs internal data structures, emit signals if needed.

   function Create
     (Self    : access Filterable_Tree_Model_Record;
      Project : Code_Analysis.Project_Access) return Project_Item_Access;

   function Create
     (Self : access Filterable_Tree_Model_Record;
      File : Code_Analysis.File_Access) return File_Item_Access;

   function Create
     (Self       : access Filterable_Tree_Model_Record;
      Subprogram : Code_Analysis.Subprogram_Access)
      return Subprogram_Item_Access;

   function Project_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Project_Access;
   --  Returns Project at the specified position.

   function File_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.File_Access;
   --  Returns File at the specified position.

   function Subprogram_At
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Analysis.Subprogram_Access;
   --  Returns Subprogram at the specified position.

   function Create_Tree_Iter
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access    := null;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Creates model specific GtkTreeIter object.

   --  GtkTreeModel subprograms

   overriding function Get_Iter
     (Self : access Filterable_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Next
     (Self : access Filterable_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function Children
     (Self   : access Filterable_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Has_Child
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   overriding function N_Children
     (Self : access Filterable_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Filterable_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Parent
     (Self  : access Filterable_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Project
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Project_Item_Access;
   --  ???

   function File
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return File_Item_Access;
   --  ???

   function Subprogram
     (Self : access Filterable_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Subprogram_Item_Access;
   --  ???

private

   type Simple_Tree_Model_Record is abstract
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with record
      Tree : Code_Analysis.Code_Analysis_Tree;
   end record;

   package Project_Vectors is new Ada.Containers.Vectors
     (Positive, Project_Item_Access);

   package File_Vectors is new Ada.Containers.Vectors
     (Positive, File_Item_Access);

   package Subprogram_Vectors is new Ada.Containers.Vectors
     (Positive, Subprogram_Item_Access);

   type Project_Item (Node : Code_Analysis.Project_Access) is tagged record
      Files : File_Vectors.Vector;
   end record;

   type File_Item (Node : Code_Analysis.File_Access) is tagged record
      Subprograms : Subprogram_Vectors.Vector;
   end record;

   type Subprogram_Item (Node : Code_Analysis.Subprogram_Access) is
     tagged null record;

   type Filterable_Tree_Model_Record is abstract
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record with record
      Tree     : Code_Analysis.Code_Analysis_Tree;
      Projects : Project_Vectors.Vector;
   end record;

   procedure Row_Inserted
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null);
   --  Emit "row_inserted" signal

   procedure Row_Changed
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null);
   --  Emit "row_changed" signal

   procedure Row_Deleted
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null);
   --  Emit "row_deleted" signal

   procedure Row_Has_Child_Toggled
     (Self       : access Filterable_Tree_Model_Record'Class;
      Project    : Project_Item_Access;
      File       : File_Item_Access       := null;
      Subprogram : Subprogram_Item_Access := null);
   --  Emit "row_has_child_toggled" signal

end Code_Analysis.Tree_Models;

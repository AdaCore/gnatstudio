------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

with Glib.Values;
with Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model;

package Gtkada.Abstract_Filter_Model is

   type Gtk_Abstract_Filter_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
       with private;

   type Gtk_Abstract_Filter_Model is
     access all Gtk_Abstract_Filter_Model_Record'Class;

   function Is_Visible
     (Self        : not null access Gtk_Abstract_Filter_Model_Record;
      Source_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Source_Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Called to determine whether row is visible. It need to be overridden by
   --  derived filter model to be able to filter out rows. Note: its
   --  implementation can access to any rows of source model, but must not
   --  access to filter model's rows.

   procedure Invalidate
     (Self : not null access Gtk_Abstract_Filter_Model_Record);
   --  Invalidate model.

   procedure Set_Source_Model
     (Self  : not null access Gtk_Abstract_Filter_Model_Record;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Sets source model.

   function Get_Source_Model
     (Self  : not null access Gtk_Abstract_Filter_Model_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns source model.

   function Map_To_Source
     (Self : not null access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns iter of source model which corresponds to specified iter of
   --  proxy model.

   procedure Initialize
     (Self : not null access Gtk_Abstract_Filter_Model_Record'Class);
   --  Initialize specified object.

   --  Overrided operations of Abstract_Tree_Model, they need to be declared
   --  in visible part of the package.

   overriding function Children
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns iter of first children of Parent.

   overriding function Get_Column_Type
     (Self  : access Gtk_Abstract_Filter_Model_Record;
      Index : Glib.Gint) return Glib.GType;
   --  Returns column's type of source model.

   overriding function Get_Iter
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns iter for the specified path.

   overriding function Get_N_Columns
     (Self : access Gtk_Abstract_Filter_Model_Record)
      return Glib.Gint;
   --  Returns number of columns of source model.

   overriding function Get_Path
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returns path of the specified iter.

   overriding procedure Get_Value
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);
   --  Returns value at the specified position.

   overriding function Has_Child
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Returns True when iter has children.

   overriding function N_Children
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gint;
   --  Returns number of children.

   overriding procedure Next
     (Self : access Gtk_Abstract_Filter_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Returns iter of next item.

   overriding function Nth_Child
     (Self   : access Gtk_Abstract_Filter_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns iter of Nth child of the specified iter.

   overriding function Parent
     (Self  : access Gtk_Abstract_Filter_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Returns iter of Parent of the specified Child.

private

   type Visibility_Kinds is (Visible, Invisible, Unknown);

   type Node_Record;
   type Node_Access is access all Node_Record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Natural, Node_Access);

   type Node_Record is limited record
      Parent     : Node_Access;
      Children   : Node_Vectors.Vector;
      Visibility : Visibility_Kinds;
   end record;

   type Gtk_Abstract_Filter_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with record
      Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Stamp : Glib.Gint;
      Root  : Node_Access;
   end record;

end Gtkada.Abstract_Filter_Model;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2016-2019, AdaCore                  --
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

--  This package defines the tree model for test view.

with Glib;
with Glib.Values;
with Gtk.Tree_Model;
with Gtkada.Abstract_Tree_Model;

package GNATTest_Module.Tree_Models is

   type Tree_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
       with private;

   type Tree_Model is access all Tree_Model_Record'Class;

   First_Icon_Column    : constant := 0;
   Second_Icon_Column   : constant := 1;
   File_Level_Column    : constant := 2;
   Name_Column          : constant := 3;

   procedure Gtk_New
     (Model : out Tree_Model;
      Data  : Source_Entity_Maps.Map);

   procedure Initialize
     (Self : access Tree_Model_Record'Class;
      Data : Source_Entity_Maps.Map);

   overriding function Get_N_Columns
     (Self : access Tree_Model_Record)
      return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding function Get_Iter
     (Self : access Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Get_Value
     (Self   : access Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   overriding procedure Next
     (Self : access Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function Children
     (Self   : access Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Has_Child
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   overriding function N_Children
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Parent
     (Self  : access Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   not overriding function Get_Source_Entity
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Source_Entity;
   --  Return Source_Entity corresponding to given Iter

private

   function Less (Left, Right : Row_Index) return Boolean;

   package Index_Ordered_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Row_Index,
      Element_Type => Source_Entity_Maps.Cursor,
      "<"          => Less,
      "="          => Source_Entity_Maps."=");

   type Tree_Model_Record is
     new Gtkada.Abstract_Tree_Model.Gtk_Abstract_Tree_Model_Record
   with record
      Index : Index_Ordered_Maps.Map;
   end record;

end GNATTest_Module.Tree_Models;

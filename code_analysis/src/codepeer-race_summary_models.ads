------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Glib;
private with Glib.Values;
with Gtk.Tree_Model;
with Gtkada.Abstract_List_Model;

package CodePeer.Race_Summary_Models is

   type Race_Summary_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record
       with private;

   type Race_Summary_Model is access all Race_Summary_Model_Record'Class;

   procedure Gtk_New
     (Model  : out Race_Summary_Model;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   procedure Initialize
     (Self   : not null access Race_Summary_Model_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   function Get_Entry_Points
     (Self : not null access Race_Summary_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return CodePeer.Entry_Point_Object_Access_Vectors.Vector;

   Object_Name_Column : constant Glib.Gint := 0;
   Message_Column     : constant Glib.Gint := 1;
   Total_Columns      : constant Glib.Gint := 2;

private

   type Race_Summary_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with record
      Data : Object_Race_Vectors.Vector;
   end record;

   overriding function Get_Column_Type
     (Self  : access Race_Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding function Get_Iter
     (Self : access Race_Summary_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_N_Columns
     (Self : access Race_Summary_Model_Record) return Glib.Gint;

   overriding function Get_Path
     (Self : access Race_Summary_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Get_Value
     (Self   : access Race_Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   overriding function N_Children
     (Self : access Race_Summary_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gint;

   overriding procedure Next
     (Self : access Race_Summary_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function Nth_Child
     (Self   : access Race_Summary_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

end CodePeer.Race_Summary_Models;

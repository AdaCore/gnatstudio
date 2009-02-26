-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2009, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This widget provides a tree view mapping a tree_store model, with
--  the additional capacity to remember which nodes are open.
--  </description>

with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_Store;        use Gtk.Tree_Store;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Glib;

package Gtkada.Tree_View is

   type Tree_View_Record is new Gtk_Tree_View_Record with record
      Model : Gtk_Tree_Store;
      --  The data model.

      Filter : Gtk_Tree_Model_Filter;
      --  Optional view filter

      Expanded_State_Column : Glib.Gint;
      --  The column memorizing the collapsed/expanded states of rows.

      Lock : Boolean := False;
      --  Whether the expand callbacks should do anything.
      --  It's useful to set this lock to True when the user wants to
      --  control expansion himself.
   end record;
   type Tree_View is access all Tree_View_Record'Class;

   procedure Gtk_New
     (Widget       : out Tree_View;
      Column_Types : Glib.GType_Array;
      Filtered     : Boolean := False);
   --  Create a new Tree_View with column types given by Column_Types.

   procedure Initialize
     (Widget       : access Tree_View_Record'Class;
      Column_Types : Glib.GType_Array;
      Filtered     : Boolean);
   --  Internal initialization function.

   function Get_Expanded
     (Widget : access Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean;
   --  Return whether the node corresponding to Iter is expanded.

   procedure Convert_To_Store_Iter
     (Self        : access Tree_View_Record'Class;
      Store_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Converts model filter iter into model store iter

   procedure Convert_To_Filter_Iter
     (Self        : access Tree_View_Record'Class;
      Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Store_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Converts model store iter into filter store iter

   function Get_Store_Iter_For_Filter_Path
     (Self        : access Tree_View_Record'Class;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts model filter path into model store iter

   function Get_Filter_Path_For_Store_Iter
     (Self       : access Tree_View_Record'Class;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Converts model store iter into model filter path

end Gtkada.Tree_View;

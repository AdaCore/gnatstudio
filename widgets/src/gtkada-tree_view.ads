-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Glib;           use Glib;

package Gtkada.Tree_View is

   type Tree_View_Record is new Gtk_Tree_View_Record with record
      Model                 : Gtk_Tree_Store;
      --  The data model.

      Expanded_State_Column : Gint;
      --  The column memorizing the collapsed/expanded states of rows.
   end record;
   type Tree_View is access all Tree_View_Record'Class;

   procedure Gtk_New
     (Widget       : out Tree_View;
      Column_Types : GType_Array);
   --  Create a new Smart_Tree with column types given by Column_Types.

   procedure Initialize
     (Widget       : access Tree_View_Record'Class;
      Column_Types : GType_Array);
   --  Internal initialization function.

end Gtkada.Tree_View;

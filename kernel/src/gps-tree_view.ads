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
--  This package provides Gtk_Tree_View which can remember expand/collapse
--  state of the nodes between collapse/expand operation on the parent node;
--  filtering out/in and resorting.
--
--  The some limitation of the current implementation is support for
--  limited number of classes of intermediate models. Right now, only
--  Gtk_Tree_Model_Filter and Gtk_Tree_Model_Sort can be used.

private with Ada.Containers.Vectors;

private with Glib;
private with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.Tree_View;

package GPS.Tree_View is

   type GPS_Tree_View_Record is
     new Gtk.Tree_View.Gtk_Tree_View_Record with private;

   type GPS_Tree_View is access all GPS_Tree_View_Record'Class;

   procedure Gtk_New (Object : out GPS_Tree_View);
   procedure Gtk_New
     (Object : out GPS_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

   procedure Initialize (Self : not null access GPS_Tree_View_Record'Class);
   procedure Initialize
     (Self  : not null access GPS_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

   overriding procedure Set_Model
     (Self  : access GPS_Tree_View_Record;
      Model : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Set new source model. The stack of models must never be changed.

private

   subtype Node_Index is Glib.Gint range 0 .. Glib.Gint'Last;

   type Node_Record;
   type Node_Access is access all Node_Record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Node_Index, Node_Access);

   type Node_Record is record
      Parent   : Node_Access;
      Expanded : Boolean;
      Children : Node_Vectors.Vector;
   end record;

   type GPS_Tree_View_Record is
     new Gtk.Tree_View.Gtk_Tree_View_Record with record
      Root                          : Node_Access;
      --  Root node. The tree reflects underling model, not the model directly
      --  connected to the view.

      Row_Inserted_Handler          : Gtk.Handlers.Handler_Id;
      Row_Deleted_Handler           : Gtk.Handlers.Handler_Id;
      Row_Has_Child_Toggled_Handler : Gtk.Handlers.Handler_Id;
      Rows_Reordered_Handler        : Gtk.Handlers.Handler_Id;
      --  Handle for callacks connected to model
   end record;

   procedure On_Lowerst_Model_Row_Inserted
     (Self : not null access GPS_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access) is null;
   --  Called when new row is inserted into the lowerst model. Node is
   --  initialized and inserted internal node.

   procedure On_Row_Expanded
     (Self : not null access GPS_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access) is null;
   --  Called when row is expanded as result of user or application request

end GPS.Tree_View;

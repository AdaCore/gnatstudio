------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package provides a Gtk_Tree_View which can remember expanded/collapsed
--  state of the nodes between collapsed/expanded operation on the parent node;
--  filtering out/in and resorting.
--
--  Application must define derived type to provide implementation of two
--  abstract subprograms to convert iterator from view's source model to
--  Lowest model and backward.
--
--  To properly initialize view, call to Initialize must be done first and
--  reference to Lowest model passed to it; after when stack of intermediate
--  models can be created and view's source model must be set by calling
--  Set_Source_Model. Stack of source models can't be changed during view's
--  life. Rationale: callbacks to Lowest model must be set before it is
--  connected to intermediate model; but standard intermediate models allows
--  to set its source model only at creation time.

private with Ada.Containers.Vectors;

with Glib;
with Glib.Main; use Glib.Main;

with Ada.Containers.Doubly_Linked_Lists;

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View;

package GPS.Tree_View is

   type GPS_Tree_View_Record is
     abstract new Gtk.Tree_View.Gtk_Tree_View_Record with private;

   type GPS_Tree_View is access all GPS_Tree_View_Record'Class;

   procedure Initialize
     (Self         : not null access GPS_Tree_View_Record'Class;
      Lowest_Model : Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Set_Source_Model
     (Self         : access GPS_Tree_View_Record;
      Source_Model : not null
         access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class);
   --  Set source model. The stack of models must never be changed.

   function To_Lowest_Model_Iter
     (Self : not null access GPS_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is abstract;
   --  Converts iterator from the view's source model to Lowest model.

private

   subtype Node_Index is Glib.Gint range 0 .. Glib.Gint'Last;

   type Node_Record;
   type Node_Access is access all Node_Record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Node_Index, Node_Access);

   package Path_List is
      new Ada.Containers.Doubly_Linked_Lists (Gtk.Tree_Model.Gtk_Tree_Path);

   package Tree_View_Sources is new Generic_Sources (GPS_Tree_View);

   type Node_Record is record
      Parent   : Node_Access;
      Expanded : Boolean;
      Children : Node_Vectors.Vector;
   end record;

   type GPS_Tree_View_Record is
     abstract new Gtk.Tree_View.Gtk_Tree_View_Record with record
      Lowest_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      --  Lowest model.

      Root          : Node_Access;
      --  Root node. The tree reflects underling model, not the model directly
      --  connected to the view.

      Paths_To_Be_Expanded : Path_List.List;
      --  A list of nodes that need to be expanded

      On_Idle : G_Source_Id := 0;
      --  The currently registered idle callback.
   end record;

   procedure On_Lowest_Model_Row_Inserted
     (Self : not null access GPS_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access) is null;
   --  Called when new row is inserted into the Lowest model. Node is
   --  initialized and inserted internal node.

   procedure On_Row_Expanded
     (Self : not null access GPS_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access) is null;
   --  Called when row is expanded as result of user or application request

end GPS.Tree_View;

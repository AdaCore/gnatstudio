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

private with Ada.Containers.Ordered_Sets;

private with Glib.Values;
private with Gtk.Tree_Model;
with Gtkada.Abstract_List_Model;

with Code_Analysis;

package Code_Peer.Entity_Messages_Models is

   Category_Name_Column       : constant := 0;
   Informational_Count_Column : constant := 1;
   Low_Count_Column           : constant := 2;
   Medium_Count_Column        : constant := 3;
   High_Count_Column          : constant := 4;
   Suppressed_Count_Column    : constant := 5;
   Number_Of_Columns          : constant := 6;

   type Entity_Messages_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with
       private;

   type Entity_Messages_Model is access all Entity_Messages_Model_Record'Class;

   procedure Gtk_New
     (Model      : out Entity_Messages_Model;
      Categories : Code_Peer.Message_Category_Sets.Set);

   procedure Initialize
     (Self       : access Entity_Messages_Model_Record'Class;
      Categories : Code_Peer.Message_Category_Sets.Set);

   procedure Clear (Self : access Entity_Messages_Model_Record'Class);
   --  Clean internal data. Used for avoid access to the already deallocated
   --  memory in object destruction process.

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Code_Analysis_Tree);

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Project_Access);

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.File_Access);

   procedure Set
     (Self   : access Entity_Messages_Model_Record'Class;
      Entity : Code_Analysis.Subprogram_Access);

private

   function Less
     (Left, Right : Code_Peer.Message_Category_Access) return Boolean;

   package Message_Category_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Code_Peer.Message_Category_Access, Less, Code_Peer."=");

   type Entity_Messages_Model_Record is
     new Gtkada.Abstract_List_Model.Gtk_Abstract_List_Model_Record with record
      Tree_Node       : Code_Analysis.Code_Analysis_Tree;
      Project_Node    : Code_Analysis.Project_Access;
      File_Node       : Code_Analysis.File_Access;
      Subprogram_Node : Code_Analysis.Subprogram_Access;
      Categories      : Message_Category_Ordered_Sets.Set;
   end record;

   function Create_Tree_Iter
     (Self     : access Entity_Messages_Model_Record'Class;
      Category : Code_Peer.Message_Category_Access)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Category_At
     (Self : access Entity_Messages_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Code_Peer.Message_Category_Access;

   overriding function Get_N_Columns
     (Self : access Entity_Messages_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Entity_Messages_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding function Get_Iter
     (Self : access Entity_Messages_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Get_Path
     (Self : access Entity_Messages_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Get_Value
     (Self   : access Entity_Messages_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   overriding procedure Next
     (Self : access Entity_Messages_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function N_Children
     (Self : access Entity_Messages_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Entity_Messages_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

end Code_Peer.Entity_Messages_Models;

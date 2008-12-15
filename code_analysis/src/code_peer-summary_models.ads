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

with Glib.Values;
with Gtk.Tree_Model;

with Code_Analysis.Tree_Models;

package Code_Peer.Summary_Models is

   Entity_Name_Column         : constant := 0;
   Informational_Count_Column : constant := 1;
   Low_Count_Column           : constant := 2;
   Medium_Count_Column        : constant := 3;
   High_Count_Column          : constant := 4;
   Suppressed_Count_Column    : constant := 5;
   Number_Of_Columns          : constant := 6;

   type Summary_Model_Record is
     new Code_Analysis.Tree_Models.Simple_Tree_Model_Record with private;

   type Summary_Model is access all Summary_Model_Record'Class;

   procedure Gtk_New
     (Model : out Summary_Model;
      Tree  : Code_Analysis.Code_Analysis_Tree);

   procedure Initialize
     (Model : access Summary_Model_Record'Class;
      Tree  : Code_Analysis.Code_Analysis_Tree);

private

   type Summary_Model_Record is
     new Code_Analysis.Tree_Models.Simple_Tree_Model_Record with record
      Tree : Code_Analysis.Code_Analysis_Tree;
   end record;

   --  Override standard GtkTreeModel subprograms.

   overriding function Get_N_Columns
     (Self : access Summary_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self : access Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding procedure Get_Value
     (Self   : access Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

end Code_Peer.Summary_Models;

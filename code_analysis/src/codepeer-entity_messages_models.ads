------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

private with Glib.Values;
private with Gtk.Tree_Model;

with Code_Analysis;
with CodePeer.Message_Categories_Models;

package CodePeer.Entity_Messages_Models is

   Category_Name_Column       : constant := 0;
   Informational_Count_Column : constant := 1;
   Low_Count_Column           : constant := 2;
   Medium_Count_Column        : constant := 3;
   High_Count_Column          : constant := 4;
   Suppressed_Count_Column    : constant := 5;
   Message_Category_Column    : constant := 6;
   Number_Of_Columns          : constant := 7;

   type Entity_Messages_Model_Record is
     new CodePeer.Message_Categories_Models.Message_Categories_Model_Record
       with private;

   type Entity_Messages_Model is access all Entity_Messages_Model_Record'Class;

   procedure Gtk_New
     (Model      : out Entity_Messages_Model;
      Categories : CodePeer.Message_Category_Sets.Set);

   procedure Initialize
     (Self       : access Entity_Messages_Model_Record'Class;
      Categories : CodePeer.Message_Category_Sets.Set);

   overriding procedure Clear (Self : access Entity_Messages_Model_Record);
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

   type Entity_Messages_Model_Record is
     new CodePeer.Message_Categories_Models.Message_Categories_Model_Record
       with record
      Tree_Node       : Code_Analysis.Code_Analysis_Tree;
      Project_Node    : Code_Analysis.Project_Access;
      File_Node       : Code_Analysis.File_Access;
      Subprogram_Node : Code_Analysis.Subprogram_Access;
      Categories      : CodePeer.Message_Category_Ordered_Sets.Set;
   end record;

   overriding function Get_N_Columns
     (Self : access Entity_Messages_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Entity_Messages_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding procedure Get_Value
     (Self   : access Entity_Messages_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

end CodePeer.Entity_Messages_Models;

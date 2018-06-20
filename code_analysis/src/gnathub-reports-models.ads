------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Glib.Values;
with Gtk.Tree_Model;

with Code_Analysis.Tree_Models;
--  with CodePeer.Utilities;

package GNAThub.Reports.Models is

   Entity_Icon_Name_Column    : constant := 0;
   Entity_Name_Column         : constant := 1;

   type Messages_Model_Record (Columns_Count : Natural) is
     new Code_Analysis.Tree_Models.Filterable_Tree_Model_Record with private;

   type Messages_Model is access all Messages_Model_Record'Class;

   procedure Gtk_New
     (Model      : out Messages_Model;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Initialize
     (Model      : access Messages_Model_Record'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Calculate_Total (Model : access Messages_Model_Record'Class);
   --  Calculate total line for report

private

   type Total_Node is record
      Severity : Severity_Access;
      Value    : Natural;
   end record;

   type Total_Nodes is array (Positive range <>) of Total_Node;

   type Messages_Model_Record (Columns_Count : Natural) is
     new Code_Analysis.Tree_Models.Filterable_Tree_Model_Record with record
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree;
      Total  : Total_Nodes (1 .. Columns_Count);
   end record;

   --  Override FilterableTreeModel subprograms

   overriding function Is_Visible
     (Self    : access Messages_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean;
   --  Returns True if specified project must be visible in the tree

   overriding function Is_Visible
     (Self    : access Messages_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean;
   --  Returns True if specified file must be visible in the tree

   overriding function Is_Visible
     (Self       : access Messages_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean;
   --  Returns True if specified subprogram must be visible in the tree

   overriding function Is_Changed
     (Self          : access Messages_Model_Record;
      Dummy_Project : Code_Analysis.Tree_Models.Project_Item_Access)
      return Boolean
     is (True);
   --  Returns True if specified project data has been changed

   overriding function Is_Changed
     (Self          : access Messages_Model_Record;
      Dummy_Project : Code_Analysis.Tree_Models.Project_Item_Access;
      Dummy_File    : Code_Analysis.Tree_Models.File_Item_Access)
      return Boolean
     is (True);
   --  Returns True if specified file data has been changed

   overriding function Is_Changed
     (Self             : access Messages_Model_Record;
      Dummy_Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      Dummy_File       : Code_Analysis.Tree_Models.File_Item_Access;
      Dummy_Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean is (True);
   --  Returns True if specified subprogram data has been changed

   --  Override standard GtkTreeModel subprograms

   overriding function Get_N_Columns
     (Self : access Messages_Model_Record) return Glib.Gint;

   overriding function Get_Column_Type
     (Self : access Messages_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding procedure Get_Value
     (Self   : access Messages_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

end GNAThub.Reports.Models;

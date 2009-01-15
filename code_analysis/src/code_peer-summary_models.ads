-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
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
with Gdk.Pixbuf;
with Gtk.Tree_Model;

with Code_Analysis.Tree_Models;
with Code_Peer.Utilities;

package Code_Peer.Summary_Models is

   Entity_Icon_Column                 : constant :=  0;
   Entity_Name_Column                 : constant :=  1;
   Entity_Lifeage_Column              : constant :=  2;
   Informational_Base_Count_Column    : constant :=  3;
   Informational_Deltas_Count_Column  : constant :=  4;
   Informational_Current_Count_Column : constant :=  5;
   Low_Base_Count_Column              : constant :=  6;
   Low_Deltas_Count_Column            : constant :=  7;
   Low_Current_Count_Column           : constant :=  8;
   Low_Current_Color_Column           : constant :=  9;
   Medium_Base_Count_Column           : constant := 10;
   Medium_Deltas_Count_Column         : constant := 11;
   Medium_Current_Count_Column        : constant := 12;
   Medium_Current_Color_Column        : constant := 13;
   High_Base_Count_Column             : constant := 14;
   High_Deltas_Count_Column           : constant := 15;
   High_Current_Count_Column          : constant := 16;
   High_Current_Color_Column          : constant := 17;
   Suppressed_Base_Count_Column       : constant := 18;
   Suppressed_Deltas_Count_Column     : constant := 19;
   Suppressed_Current_Count_Column    : constant := 20;
   Number_Of_Columns                  : constant := 21;

   type Summary_Model_Record is
     new Code_Analysis.Tree_Models.Filterable_Tree_Model_Record with private;

   type Summary_Model is access all Summary_Model_Record'Class;

   procedure Gtk_New
     (Model           : out Summary_Model;
      Tree            : Code_Analysis.Code_Analysis_Tree;
      Categories      : Code_Peer.Message_Category_Sets.Set;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Initialize
     (Model           : access Summary_Model_Record'Class;
      Tree            : Code_Analysis.Code_Analysis_Tree;
      Categories      : Code_Peer.Message_Category_Sets.Set;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Set_Show_All_Subprograms
     (Self : access Summary_Model_Record'Class;
      Show : Boolean);
   --  Toggle filering of the subprograms which don't have messages. Filtering
   --  is enabled by default.

   procedure Set_Visible_Message_Categories
     (Self : access Summary_Model_Record'Class;
      To   : Code_Peer.Message_Category_Sets.Set);

   procedure Clear (Self : access Summary_Model_Record);

private

   type Summary_Model_Record is
     new Code_Analysis.Tree_Models.Filterable_Tree_Model_Record with record
      Tree                 : Code_Analysis.Code_Analysis_Tree;
      Show_All_Subprograms : Boolean := False;
      Show_All_Files       : Boolean := True;
      Show_All_Projects    : Boolean := True;
      Message_Categories   : Code_Peer.Message_Category_Sets.Set;
      --  Set of the message categories, which is showed in the report
      Project_Icon         : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon            : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon      : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

   type Subprogram_Item is
     new Code_Analysis.Tree_Models.Subprogram_Item with record
      Computed        : Boolean := False;
      Messages_Counts : Code_Peer.Utilities.Messages_Counts;
   end record;

   type Subprogram_Item_Access is access all Subprogram_Item'Class;

   type File_Item is new Code_Analysis.Tree_Models.File_Item with record
      Computed        : Boolean := False;
      Messages_Counts : Code_Peer.Utilities.Messages_Counts;
   end record;

   type File_Item_Access is access all File_Item'Class;

   type Project_Item is new Code_Analysis.Tree_Models.Project_Item with record
      Computed        : Boolean := False;
      Messages_Counts : Code_Peer.Utilities.Messages_Counts;
   end record;

   type Project_Item_Access is access all Project_Item'Class;

   --  Override FilterableTreeModel subprograms

   overriding function Create
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Project_Access)
      return Code_Analysis.Tree_Models.Project_Item_Access;

   overriding function Create
     (Self : access Summary_Model_Record;
      File : Code_Analysis.File_Access)
      return Code_Analysis.Tree_Models.File_Item_Access;

   overriding function Create
     (Self       : access Summary_Model_Record;
      Subprogram : Code_Analysis.Subprogram_Access)
      return Code_Analysis.Tree_Models.Subprogram_Item_Access;

   overriding function Is_Visible
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean;
   --  Returns True if specified project must be visible in the tree

   overriding function Is_Visible
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean;
   --  Returns True if specified file must be visible in the tree

   overriding function Is_Visible
     (Self       : access Summary_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean;
   --  Returns True if specified subprogram must be visible in the tree

   overriding function Is_Changed
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean;
   --  Returns True if specified project data has been changed

   overriding function Is_Changed
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean;
   --  Returns True if specified file data has been changed

   overriding function Is_Changed
     (Self       : access Summary_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean;
   --  Returns True if specified subprogram data has been changed

   --  Override standard GtkTreeModel subprograms

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

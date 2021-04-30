------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2021, AdaCore                     --
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

--  Report of GNAThub's messages

with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Glib;                    use Glib;

with Gtkada.Tree_View;        use Gtkada.Tree_View;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Gesture_Multi_Press; use Gtk.Gesture_Multi_Press;

with GPS.Kernel;              use GPS.Kernel;
with GNAThub.Metrics;         use GNAThub.Metrics;
with GUI_Utils;               use GUI_Utils;

package GNAThub.Reports.Messages is

   type GNAThub_Report_Messages_Record is new Gtk_Paned_Record with private;
   type GNAThub_Report_Messages is
     access all GNAThub_Report_Messages_Record'Class;

   procedure Gtk_New
     (Self       : out GNAThub_Report_Messages;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Severities : GNAThub.Severities_Ordered_Sets.Set;
      Metrics    : Rule_Sets.Set);
   --  Create a new messages report, allowing one column per severity.

   procedure Expand_Or_Collapse_Selected_Rows
     (Self    : not null GNAThub_Report_Messages;
      Command : Expansion_Command_Type);
   --  Expand or collapse the rows that are currently selected in the given
   --  tree view.

private

   type Messages_And_Metrics_Listener (View : GNAThub_Report_Messages) is
     new GPS.Kernel.Messages.Abstract_Listener
     and Metrics_Listener_Interface with null record;
   type Messages_And_Metrics_Listener_Access is
     access all Messages_And_Metrics_Listener'Class;
   --  listeners used to update the messages report according to
   --  the messages/metrics being added/removed.

   overriding procedure Message_Added
     (Self    : not null access Messages_And_Metrics_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class);

   overriding procedure Message_Removed
     (Self    : not null access Messages_And_Metrics_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class);

   overriding procedure Metric_Added
     (Self   : not null access Messages_And_Metrics_Listener;
      Metric : not null access Metric_Record'Class);

   overriding procedure Metrics_Visibility_Changed
     (Self    : not null access Messages_And_Metrics_Listener;
      Metrics : Rule_Sets.Set);

   procedure Free is new Ada.Unchecked_Deallocation
     (Messages_And_Metrics_Listener'Class,
      Messages_And_Metrics_Listener_Access);

   type GNAThub_Report_Tree_View_Record is new Tree_View_Record
   with record
      Kernel     : Kernel_Handle;

      Multipress : Gtk_Gesture_Multi_Press;
      --  Used to handle double-clicks.

      Locked     : Boolean := False;
      --  Used to avoid recursion when synchronizing selection between
      --  the tree that lists entities and the report tree.
   end record;
   type GNAThub_Report_Tree_View is
     access all GNAThub_Report_Tree_View_Record'Class;

   overriding function Is_Visible
     (Self       : not null access GNAThub_Report_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean;
   --  Return True if the row contains messages, False othwerwise.

   type Severity_Columns_Info_Type is record
      Total_Col : Gint;
      --  The severity's total number of messages column ID in the model

      Color_Col : Gint;
      --  The severity's color column ID in the model

      Tree_Col  : Gint;
      --  The severity's column ID in the tree
   end record;
   package Severity_To_Column_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Severity_Columns_Info_Type,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --  Used to store information about the indexes of the severities columns.

   type Metric_Columns_Info_Type is record
      Has_Metrics : Boolean := False;
      --  True if the column actually contains metrics to be shown.

      Model_Col   : Gint;
      --  The metric's column ID in the model

      Tree_Col    : Gint;
      --  The metric's column ID in the tree view
   end record;
   package Metric_Rules_To_Column_ID_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Metric_Columns_Info_Type,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   type GNAThub_Report_Messages_Record is new Gtk_Paned_Record with record
      Kernel                  : access Kernel_Handle_Record'Class;
      Entities_Tree           : GNAThub_Report_Tree_View;
      Report_Tree             : GNAThub_Report_Tree_View;
      Severities              : GNAThub.Severities_Ordered_Sets.Set;
      Metrics                 : Rule_Sets.Set;
      Listener                : Messages_And_Metrics_Listener_Access;
      Severities_Columns_Info : Severity_To_Column_Maps.Map;
      Metric_Rules_Column_IDs : Metric_Rules_To_Column_ID_Maps.Map;
      Total_Messages_Col      : Gint;
      Message_Columns_Shown   : Boolean := False;
   end record;

end GNAThub.Reports.Messages;

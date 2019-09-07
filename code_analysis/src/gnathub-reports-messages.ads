------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Gesture_Multi_Press; use Gtk.Gesture_Multi_Press;

with GPS.Kernel;              use GPS.Kernel;
with GNAThub.Metrics;         use GNAThub.Metrics;

package GNAThub.Reports.Messages is

   type GNAThub_Report_Messages_Record is new Tree_View_Record with private;
   type GNAThub_Report_Messages is
     access all GNAThub_Report_Messages_Record'Class;

   procedure Gtk_New
     (Self       : out GNAThub_Report_Messages;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Severities : GNAThub.Severities_Ordered_Sets.Set);
   --  Create a new messages report, allowing one column per severity.

   procedure Clear
     (Self : not null access GNAThub_Report_Messages_Record'Class);
   --  Clear the messages report.

   overriding function Is_Visible
     (Self       : not null access GNAThub_Report_Messages_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean;
   --  Return True if the row contains messages, False othwerwise.

   procedure Show_Messages
     (Self : not null access GNAThub_Report_Messages_Record;
      ID   : String);
   --  Show the first message in the Locations view related to the given row

   function Get_ID
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Row  : Gtk_Tree_Iter) return String;
   --  Return a unique ID for the given row.

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

   procedure Free is new Ada.Unchecked_Deallocation
     (Messages_And_Metrics_Listener'Class,
      Messages_And_Metrics_Listener_Access);

   type Severity_Columns_Info_Type is record
      Total_Col : Gint;
      Color_Col : Gint;
   end record;
   package Severity_To_Column_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Severity_Columns_Info_Type,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --  Used to store information about the indexes of the severities columns.

   type GNAThub_Report_Messages_Record is new Tree_View_Record with record
      Severities              : GNAThub.Severities_Ordered_Sets.Set;
      Kernel                  : access Kernel_Handle_Record'Class;
      Listener                : Messages_And_Metrics_Listener_Access;
      Severities_Columns_Info : Severity_To_Column_Maps.Map;
      Multipress              : Gtk_Gesture_Multi_Press;
   end record;

end GNAThub.Reports.Messages;

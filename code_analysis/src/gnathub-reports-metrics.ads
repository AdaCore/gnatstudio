------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  Report of GNAThub's metrics

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Gtkada.Tree_View;
with Gtk.Tree_Model;    use Gtk.Tree_Model;

with GNAThub.Metrics;   use GNAThub.Metrics;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package GNAThub.Reports.Metrics is

   type GNAThub_Report_Metrics_Record is new Gtkada.Tree_View.Tree_View_Record
     and Metrics_Listener_Interface with private;
   type GNAThub_Report_Metrics is
     access all GNAThub_Report_Metrics_Record'Class;

   procedure Gtk_New (Widget : out GNAThub_Report_Metrics);
   --  Create a new metrics' report.

   procedure Initialize
     (Self : not null access GNAThub_Report_Metrics_Record'Class);
   --  Initialize the metrics' report.

   procedure Show_Metrics
     (Self        : not null access GNAThub_Report_Metrics_Record'Class;
      Location_ID : String);
   --  Show all the metrics associated to the given location's ID.

   procedure Clear
     (Self : not null access GNAThub_Report_Metrics_Record'Class);
   --  Clear the metrics' report

   overriding procedure Metric_Added
     (Self   : not null access GNAThub_Report_Metrics_Record;
      Metric : not null access Metric_Record'Class);

private

   package Entities_To_Metrics_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Metrics_Ordered_Sets.Set,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => Metrics_Ordered_Sets."=");

   type GNAThub_Report_Metrics_Record is new Gtkada.Tree_View.Tree_View_Record
     and Metrics_Listener_Interface with record
      Metrics : Entities_To_Metrics_Maps.Map;
   end record;

end GNAThub.Reports.Metrics;

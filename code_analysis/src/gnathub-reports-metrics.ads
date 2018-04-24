------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Gtk.Box;
with Gtkada.Tree_View;

package GNAThub.Reports.Metrics is

   type GNAThub_Report_Metrics is new Gtk.Box.Gtk_Vbox_Record with private;

   type Metrics_Report is access all GNAThub_Report_Metrics'Class;

   procedure Gtk_New (Widget : out Metrics_Report);

   procedure Initialize (Self : not null access GNAThub_Report_Metrics'Class);

   procedure Display_Metrics_Report
     (Self    : not null access GNAThub_Report_Metrics'Class;
      Metrics : Metric_Tool_Maps.Map);

private

   type GNAThub_Report_Metrics is
     new Gtk.Box.Gtk_Vbox_Record with record
      Metrics_View : Gtkada.Tree_View.Tree_View;
   end record;

end GNAThub.Reports.Metrics;

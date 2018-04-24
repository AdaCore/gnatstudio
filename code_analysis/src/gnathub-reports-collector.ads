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

--  Collector for all GNAThub's reports which organizes all reports
--  as a tabs in one notebook

with Gtk.Box;
with GPS.Kernel;
with GNAThub.Reports.Messages;
with GNAThub.Reports.Metrics;

package GNAThub.Reports.Collector is

   type GNAThub_Report_Collector is new Gtk.Box.Gtk_Vbox_Record with private;

   type Report is access all GNAThub_Report_Collector'Class;

   procedure Gtk_New
     (Widget     : out Report;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Initialize
     (Self       : not null access GNAThub_Report_Collector'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Update (Self : not null access GNAThub_Report_Collector'Class);

private

   ----------------------
   -- Message_Listener --
   ----------------------

   type Message_Listener (View : Gtk.Box.Gtk_Vbox) is
     new GPS.Kernel.Messages.Abstract_Listener with null record;

   type Message_Listener_Access is access all Message_Listener'Class;

   overriding procedure Message_Added
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class);

   ------------------------------
   -- GNAThub_Report_Collector --
   ------------------------------

   type GNAThub_Report_Collector is new Gtk.Box.Gtk_Vbox_Record with record
      Kernel          : GPS.Kernel.Kernel_Handle;
      Messages_Report : GNAThub.Reports.Messages.Messages_Report;
      Metric_Report   : GNAThub.Reports.Metrics.Metrics_Report;
      Listener        : Message_Listener_Access;
   end record;

end GNAThub.Reports.Collector;

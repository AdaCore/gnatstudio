-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2011, AdaCore                   --
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

with Gtk.Box;

with Code_Peer.Messages_Reports;
private with Code_Peer.Race_Condition_Reports;
with GPS.Kernel.Modules;

package Code_Peer.Reports is

   type Report_Record is new Gtk.Box.Gtk_Vbox_Record with private;

   type Report is access all Report_Record'Class;

   procedure Gtk_New
     (Widget : out Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   procedure Initialize
     (Self   : not null access Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   function Messages_Report
     (Self : not null access Report_Record'Class)
      return Code_Peer.Messages_Reports.Messages_Report;

private

   type Report_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Messages_Report : Code_Peer.Messages_Reports.Messages_Report;
      Race_Report     : Code_Peer.Race_Condition_Reports.Race_Condition_Report;
   end record;

end Code_Peer.Reports;

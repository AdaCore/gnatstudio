-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2011, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  Object race condition report for CodePeer.

with Gtk.Box;
private with Gtk.Tree_View;

private with Code_Peer.Race_Details_Models;
private with Code_Peer.Race_Summary_Models;

package Code_Peer.Race_Condition_Reports is

   type Race_Condition_Report_Record is
     new Gtk.Box.Gtk_Vbox_Record with private;

   type Race_Condition_Report is access all Race_Condition_Report_Record'Class;

   procedure Gtk_New
     (Report : out Race_Condition_Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   procedure Initialize
     (Self   : not null access Race_Condition_Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree);

private

   type Race_Condition_Report_Record is
     new Gtk.Box.Gtk_Vbox_Record with record
      Kernel        : GPS.Kernel.Kernel_Handle;
      Summary_Model : Code_Peer.Race_Summary_Models.Race_Summary_Model;
      Summary_View  : Gtk.Tree_View.Gtk_Tree_View;
      Details_Model : Code_Peer.Race_Details_Models.Race_Details_Model;
   end record;

end Code_Peer.Race_Condition_Reports;

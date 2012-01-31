------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Gtk.Box;

with CodePeer.Messages_Reports;
private with CodePeer.Race_Condition_Reports;
with GPS.Kernel.Modules;

package CodePeer.Reports is

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
      return CodePeer.Messages_Reports.Messages_Report;

private

   type Report_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Messages_Report : CodePeer.Messages_Reports.Messages_Report;
      Race_Report     : CodePeer.Race_Condition_Reports.Race_Condition_Report;
   end record;

end CodePeer.Reports;

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

--  Object race condition report for CodePeer.

with Gtk.Box;
private with Gtk.Tree_View;

private with CodePeer.Race_Details_Models;
private with CodePeer.Race_Summary_Models;

package CodePeer.Race_Condition_Reports is

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
      Summary_Model : CodePeer.Race_Summary_Models.Race_Summary_Model;
      Summary_View  : Gtk.Tree_View.Gtk_Tree_View;
      Details_Model : CodePeer.Race_Details_Models.Race_Details_Model;
      Details_View  : Gtk.Tree_View.Gtk_Tree_View;
   end record;

end CodePeer.Race_Condition_Reports;

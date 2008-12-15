-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
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

with Glib;
with Gtk.Box;
with Gtk.Tree_View;

with GPS.Kernel.Modules;

with Code_Analysis;
with Code_Peer.Entity_Messages_Models;
with Code_Peer.Summary_Models;

package Code_Peer.Summary_Reports is

   type Summary_Report_Record is new Gtk.Box.Gtk_Vbox_Record with private;

   type Summary_Report is access all Summary_Report_Record'Class;

   procedure Gtk_New
     (Report : out Summary_Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   procedure Initialize
     (Self   : access Summary_Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree);

   function Get_Selected_Project
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Project_Access;

   function Get_Selected_File
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.File_Access;

   function Get_Selected_Subprogram
     (Self : access Summary_Report_Record'Class)
      return Code_Analysis.Subprogram_Access;

   Signal_Activated : constant Glib.Signal_Name;

private

   type Summary_Report_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Kernel         : GPS.Kernel.Kernel_Handle;
      Tree           : Code_Analysis.Code_Analysis_Tree;
      Analysis_Model : Code_Peer.Summary_Models.Summary_Model;
      Analysis_View  : Gtk.Tree_View.Gtk_Tree_View;
      Messages_Model : Code_Peer.Entity_Messages_Models.Entity_Messages_Model;
      Messages_View  : Gtk.Tree_View.Gtk_Tree_View;
   end record;

   Signal_Activated : constant Glib.Signal_Name := "activated";

end Code_Peer.Summary_Reports;

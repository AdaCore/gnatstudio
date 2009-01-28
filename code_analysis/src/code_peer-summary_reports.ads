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
with Gtk.Paned;
private with Gtk.Tree_View;

with GPS.Kernel.Modules;

with Code_Analysis;
private with Code_Peer.Entity_Messages_Models;
private with Code_Peer.Summary_Models;
private with Code_Peer.Messages_Filter_Models;

package Code_Peer.Summary_Reports is

   type Summary_Report_Record is new Gtk.Paned.Gtk_Hpaned_Record with private;

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

   procedure Update_Criteria
     (Self     : access Summary_Report_Record'Class;
      Criteria : in out Code_Peer.Message_Filter_Criteria);

   procedure Update (Self : access Summary_Report_Record'Class);

   Signal_Activated        : constant Glib.Signal_Name;
   Signal_Criteria_Changed : constant Glib.Signal_Name;

private

   type Summary_Report_Record is new Gtk.Paned.Gtk_Hpaned_Record with record
      Kernel         : GPS.Kernel.Kernel_Handle;
      Tree           : Code_Analysis.Code_Analysis_Tree;
      Analysis_Model : Code_Peer.Summary_Models.Summary_Model;
      Analysis_View  : Gtk.Tree_View.Gtk_Tree_View;
      Messages_Model : Code_Peer.Entity_Messages_Models.Entity_Messages_Model;
      Messages_View  : Gtk.Tree_View.Gtk_Tree_View;
      Hide_Model     : Code_Peer.Messages_Filter_Models.Messages_Filter_Model;
      Hide_View      : Gtk.Tree_View.Gtk_Tree_View;

      Show_Lifeage   : Lifeage_Kinds_Flags :=
                         (Added => True, Unchanged => True, Removed => False);
      Show_Probabilities : Message_Probability_Level_Flags :=
                             (Suppressed => False, others => True);
   end record;

   Signal_Activated        : constant Glib.Signal_Name := "activated";
   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria_changed";

end Code_Peer.Summary_Reports;

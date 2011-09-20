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

with Ada.Strings.Fixed;

with Gtk.Label;
with Gtk.Notebook;

with GPS.Kernel.Project;

package body Code_Peer.Reports is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Report;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree) is
   begin
      Widget := new Report_Record;
      Initialize (Widget, Kernel, Module, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Report_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID;
      Tree   : Code_Analysis.Code_Analysis_Tree)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Inspections_Box     : Gtk.Box.Gtk_Hbox;
      Baseline_Inspection : Gtk.Label.Gtk_Label;
      Current_Inspection  : Gtk.Label.Gtk_Label;
      Notebook            : Gtk.Notebook.Gtk_Notebook;
      Project_Data        : Code_Peer.Project_Data'Class renames
        Code_Peer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
               (Tree,
                GPS.Kernel.Project.Get_Project
                  (Kernel)).Analysis_Data.Code_Peer_Data.all);

   begin
      Gtk.Box.Initialize_Vbox (Self);

      --  Baseline and current inspections' ids

      Gtk.Box.Gtk_New_Hbox (Inspections_Box, True);
      Self.Pack_Start (Inspections_Box, False);

      Gtk.Label.Gtk_New (Baseline_Inspection, "baseline");
      Baseline_Inspection.Set_Alignment (0.1, 0.0);
      Baseline_Inspection.Set_Label
        ("Baseline inspection #"
         & Trim (Natural'Image (Project_Data.Baseline_Inspection), Both));
      Inspections_Box.Pack_Start (Baseline_Inspection);
      Gtk.Label.Gtk_New (Current_Inspection, "current");
      Current_Inspection.Set_Alignment (0.9, 0.0);
      Current_Inspection.Set_Label
        ("Current inspection #"
         & Trim (Natural'Image (Project_Data.Current_Inspection), Both));
      Inspections_Box.Pack_End (Current_Inspection);

      --  Notebook

      Gtk.Notebook.Gtk_New (Notebook);
      Self.Pack_Start (Notebook);

      --  Messages report tab

      Code_Peer.Messages_Reports.Gtk_New
        (Self.Messages_Report,
         Kernel,
         Module,
         Tree);
      Notebook.Append_Page (Self.Messages_Report);
      Notebook.Set_Tab_Label_Text (Self.Messages_Report, "Messages");

      --  Race condition report tab

      Code_Peer.Race_Condition_Reports.Gtk_New
        (Self.Race_Report, Kernel, Tree);
      Notebook.Append_Page (Self.Race_Report);
      Notebook.Set_Tab_Label_Text (Self.Race_Report, "Race conditions");
   end Initialize;

   ---------------------
   -- Messages_Report --
   ---------------------

   function Messages_Report
     (Self : not null access Report_Record'Class)
      return Code_Peer.Messages_Reports.Messages_Report is
   begin
      return Self.Messages_Report;
   end Messages_Report;

end Code_Peer.Reports;

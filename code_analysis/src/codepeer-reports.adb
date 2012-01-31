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

with Ada.Strings.Fixed;

with Gtk.Label;
with Gtk.Notebook;

with GPS.Kernel.Project;

package body CodePeer.Reports is

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
      Project_Data        : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
               (Tree,
                GPS.Kernel.Project.Get_Project
                  (Kernel)).Analysis_Data.CodePeer_Data.all);

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

      CodePeer.Messages_Reports.Gtk_New
        (Self.Messages_Report,
         Kernel,
         Module,
         Tree);
      Notebook.Append_Page (Self.Messages_Report);
      Notebook.Set_Tab_Label_Text (Self.Messages_Report, "Messages");

      --  Race condition report tab

      CodePeer.Race_Condition_Reports.Gtk_New
        (Self.Race_Report, Kernel, Tree);
      Notebook.Append_Page (Self.Race_Report);
      Notebook.Set_Tab_Label_Text (Self.Race_Report, "Race conditions");
   end Initialize;

   ---------------------
   -- Messages_Report --
   ---------------------

   function Messages_Report
     (Self : not null access Report_Record'Class)
      return CodePeer.Messages_Reports.Messages_Report is
   begin
      return Self.Messages_Report;
   end Messages_Report;

end CodePeer.Reports;

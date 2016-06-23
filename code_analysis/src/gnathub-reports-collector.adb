------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
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

with Gtk.Notebook;

package body GNAThub.Reports.Collector is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget     : out Report;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set) is
   begin
      Widget := new GNAThub_Report_Collector;
      Initialize (Widget, Kernel, Tree, Severities);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access GNAThub_Report_Collector'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set)
   is
      Notebook : Gtk.Notebook.Gtk_Notebook;
   begin
      Gtk.Box.Initialize_Vbox (Self);

      --  Notebook

      Gtk.Notebook.Gtk_New (Notebook);
      Self.Pack_Start (Notebook);

      --  Messages report tab

      GNAThub.Reports.Messages.Gtk_New
        (Self.Messages_Report, Kernel, Tree, Severities);

      Notebook.Append_Page (Self.Messages_Report);
      Notebook.Set_Tab_Label_Text (Self.Messages_Report, "Messages");
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (Self : not null access GNAThub_Report_Collector'Class) is
   begin
      Self.Messages_Report.Update;
   end Update;

end GNAThub.Reports.Collector;

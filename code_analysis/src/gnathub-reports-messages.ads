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

--  Report of GNAThub's messages

with Gtk.Box;
with Gtk.Tree_Model_Sort;
with Gtk.Tree_View;
with GNAThub.Reports.Models;

private with Glib;
private with Gtk.Gesture_Multi_Press;

package GNAThub.Reports.Messages is

   type GNAThub_Report_Messages is new Gtk.Box.Gtk_Vbox_Record with private;

   type Messages_Report is access all GNAThub_Report_Messages'Class;

   procedure Gtk_New
     (Widget     : out Messages_Report;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Initialize
     (Self       : not null access GNAThub_Report_Messages'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set);

   procedure Update (Self : not null access GNAThub_Report_Messages'Class);

   function Get_Tree
     (Self : not null access GNAThub_Report_Messages'Class)
      return Gtk.Tree_View.Gtk_Tree_View;

   function Get_Analysis_Model
     (Self : not null access GNAThub_Report_Messages'Class)
      return GNAThub.Reports.Models.Messages_Model;

   function Get_Sort_Model
     (Self : not null access GNAThub_Report_Messages'Class)
     return Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort;
private

   type GNAThub_Report_Messages is
     new Gtk.Box.Gtk_Vbox_Record with record
      Kernel              : GPS.Kernel.Kernel_Handle;
      Analysis_Model      : GNAThub.Reports.Models.Messages_Model;
      Analysis_Sort_Model : Gtk.Tree_Model_Sort.Gtk_Tree_Model_Sort;
      Analysis_View       : Gtk.Tree_View.Gtk_Tree_View;
      Total_Column        : Glib.Gint;
      Multipress          : Gtk.Gesture_Multi_Press.Gtk_Gesture_Multi_Press;
   end record;

end GNAThub.Reports.Messages;

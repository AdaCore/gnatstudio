------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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
with GNATStack.CI_Models;
with GNATStack.Data_Model;

package GNATStack.CI_Editors is

   type CI_Editor_Record is new Gtk.Box.Gtk_Vbox_Record with private;

   type CI_Editor is access all CI_Editor_Record'Class;

   procedure Gtk_New
     (Item : out CI_Editor;
      Data : not null access GNATStack.Data_Model.Analysis_Information);

   procedure Initialize
     (Self : not null access CI_Editor_Record'Class;
      Data : not null access GNATStack.Data_Model.Analysis_Information);

private

   type CI_Editor_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Data             : access GNATStack.Data_Model.Analysis_Information;
      CI_Model         : GNATStack.CI_Models.CI_Model;
      Unassigned_Model : GNATStack.CI_Models.CI_Model;
   end record;

end GNATStack.CI_Editors;

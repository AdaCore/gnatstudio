-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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

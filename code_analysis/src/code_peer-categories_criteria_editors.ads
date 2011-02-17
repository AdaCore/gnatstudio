-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009-2011, AdaCore              --
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

--  This package contains implementation of the CodePeer's message categories
--  filter criteria editor. It is used by Summary Report form.

with Glib;
private with Gtk.Check_Button;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_View;
with Gtk.Widget;

private with Code_Peer.Categories_Criteria_Models;

package Code_Peer.Categories_Criteria_Editors is

   type Categories_Criteria_Editor_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;

   type Categories_Criteria_Editor is
     access all Categories_Criteria_Editor_Record'Class;

   procedure Gtk_New
     (Editor     : in out Categories_Criteria_Editor;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Categories : Code_Peer.Message_Category_Sets.Set);

   procedure Initialize
     (Self       :
        not null access Categories_Criteria_Editor_Record'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Categories : Code_Peer.Message_Category_Sets.Set);

   function Get_Visible_Categories
     (Self : access Categories_Criteria_Editor_Record'Class)
      return Code_Peer.Message_Category_Sets.Set;
   --  Returns a set of selected message categories.

   Signal_Criteria_Changed : constant Glib.Signal_Name;
   --  This signal emitted by the editor in the case of the criteria change.

private

   type Categories_Criteria_Editor_Record is
     new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with
      record
         Model  : Categories_Criteria_Models.Categories_Criteria_Model;
         View   : Gtk.Tree_View.Gtk_Tree_View;
         Toggle : Gtk.Check_Button.Gtk_Check_Button;
      end record;

   Signal_Criteria_Changed : constant Glib.Signal_Name := "criteria-changed";

end Code_Peer.Categories_Criteria_Editors;

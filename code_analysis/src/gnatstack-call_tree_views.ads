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

with Glib;
with Gtk.Box;
private with Gtk.Tree_View;

private with GNATStack.Call_Tree_Models;
with GNATStack.Data_Model;

package GNATStack.Call_Tree_Views is

   type Call_Tree_View_Record is new Gtk.Box.Gtk_Hbox_Record with private;

   type Call_Tree_View is access all Call_Tree_View_Record'Class;

   procedure Gtk_New
     (Item       : out Call_Tree_View;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access);

   procedure Initialize
     (Self       : not null access Call_Tree_View_Record'Class;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access);

   function Get_Selected_Subprogram
     (Self : not null access Call_Tree_View_Record'Class)
      return GNATStack.Data_Model.Subprogram_Information_Access;
   --  Returns currently selected subprogram.

   Signal_Double_Clicked : constant Glib.Signal_Name;
   --  Emitted on double click in tree.

private

   type Call_Tree_View_Record is new Gtk.Box.Gtk_Hbox_Record with record
      View  : Gtk.Tree_View.Gtk_Tree_View;
      Model : GNATStack.Call_Tree_Models.Call_Tree_Model;
   end record;

   procedure Double_Clicked
     (Self : not null access Call_Tree_View_Record'Class);
   --  Emits "double_clicked" signal.

   Signal_Double_Clicked : constant Glib.Signal_Name := "double_clicked";

end GNATStack.Call_Tree_Views;

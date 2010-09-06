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
with Gtk.Cell_Renderer_Text;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with GPS.Intl;

with GNATStack.Call_Tree_Models;

package body GNATStack.Call_Tree_Views is

   use GPS.Intl;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item       : out Call_Tree_View;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access) is
   begin
      Item := new Call_Tree_View_Record;
      Initialize (Item, Subprogram);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access Call_Tree_View_Record'Class;
      Subprogram :
        not null GNATStack.Data_Model.Subprogram_Information_Access)
   is
      Model    : GNATStack.Call_Tree_Models.Call_Tree_Model;
      View     : Gtk.Tree_View.Gtk_Tree_View;
      Column   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy    : Glib.Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk.Box.Initialize_Hbox (Self);

      GNATStack.Call_Tree_Models.Gtk_New (Model, Subprogram);
      Gtk.Tree_View.Gtk_New (View, Model);
      Model.Unref;
      Self.Pack_Start (View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Subprogram");
      Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
      Column.Pack_Start (Renderer, True);
      Column.Add_Attribute (Renderer, "text", 0);
      Dummy := View.Append_Column (Column);
   end Initialize;

end GNATStack.Call_Tree_Views;

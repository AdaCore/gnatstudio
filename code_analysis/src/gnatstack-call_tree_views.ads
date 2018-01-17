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

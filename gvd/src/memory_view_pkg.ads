------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Viewport;        use Gtk.Viewport;

package Memory_View_Pkg is

   type Memory_View_Record is new Gtk_Vbox_Record with record
      Pgup           : Gtk_Button;
      Pgdn           : Gtk_Button;
      Scrolledwindow : Gtk_Scrolled_Window;
      Viewport       : Gtk_Viewport;
      View           : Gtk_Text_View;
      Reset          : Gtk_Button;
      Submit         : Gtk_Button;
      Close          : Gtk_Button;
      Address_View   : Gtk_Button;
      Format         : Gtk_Combo_Box_Text;
      Show_Ascii     : Gtk_Check_Button;
      Size           : Gtk_Combo_Box_Text;
      Address_Entry  : Gtk_Entry;
      Lines_Spin     : Gtk_Spin_Button;
   end record;
   type Memory_View_Access is access all Memory_View_Record'Class;

   procedure Gtk_New (Memory_View : out Memory_View_Access);
   procedure Initialize (Memory_View : access Memory_View_Record'Class);

end Memory_View_Pkg;

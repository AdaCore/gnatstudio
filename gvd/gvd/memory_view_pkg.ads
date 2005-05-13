-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Viewport;        use Gtk.Viewport;
with Gtk.Window;          use Gtk.Window;

package Memory_View_Pkg is

   type Memory_View_Record is new Gtk_Window_Record with record
      Frame          : Gtk_Frame;
      Pgup           : Gtk_Button;
      Pgdn           : Gtk_Button;
      Scrolledwindow : Gtk_Scrolled_Window;
      Viewport       : Gtk_Viewport;
      View           : Gtk_Text_View;
      Reset          : Gtk_Button;
      Submit         : Gtk_Button;
      Close          : Gtk_Button;
      Address_View   : Gtk_Button;
      Format         : Gtk_Combo;
      Search_Button  : Gtk_Button;
      Show_Ascii     : Gtk_Check_Button;
      Size           : Gtk_Combo;
      Size_Entry     : Gtk_Entry;
      Address_Entry  : Gtk_Entry;
      Search_Entry   : Gtk_Entry;
      Data_Entry     : Gtk_Entry;
      Lines_Spin     : Gtk_Spin_Button;
   end record;
   type Memory_View_Access is access all Memory_View_Record'Class;

   procedure Gtk_New (Memory_View : out Memory_View_Access);
   procedure Initialize (Memory_View : access Memory_View_Record'Class);

end Memory_View_Pkg;

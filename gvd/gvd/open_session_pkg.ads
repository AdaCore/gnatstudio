-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Label; use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Open_Program_Pkg; use Open_Program_Pkg;
with Odd.Types;

package Open_Session_Pkg is

   type Open_Session_Record is new Gtk_Window_Record with record
      Vbox17 : Gtk_Vbox;
      Frame13 : Gtk_Frame;
      Table8 : Gtk_Table;
      Combo12 : Gtk_Combo;
      Program_Entry : Gtk_Entry;
      Open_Button : Gtk_Button;
      Launch_Menu : Gtk_Option_Menu;
      Label69 : Gtk_Label;
      Label71 : Gtk_Label;
      Hbuttonbox9 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
      Valid : Boolean;
   end record;

   type Open_Session_Access is access all Open_Session_Record'Class;

   procedure Open_Session
     (Open   : in out Open_Session_Access;
      File   : out Odd.Types.String_Access;
      Launch : out Launch_Method);
   --  Open a program window and launch a main loop until the ok or cancel
   --  button has been pressed.
   --  Open if null is set to the created window, that is hidden on return.
   --  If non null, Open_Session will show it instead of creating a new one.
   --  Return the session file name. If Launch is None,
   --  this means a cancellation from the user.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

private

   procedure Gtk_New (Open_Session : out Open_Session_Access);
   procedure Initialize (Open_Session : access Open_Session_Record'Class);

end Open_Session_Pkg;

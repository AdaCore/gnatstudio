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

with Gtk.Window;          use Gtk.Window;
with Gtk.Box;             use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport;        use Gtk.Viewport;
with Gtk.List;            use Gtk.List;
with Gtk.Label;           use Gtk.Label;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Separator;       use Gtk.Separator;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Odd.Types;           use Odd.Types;

package Open_Session_Pkg is

   type Button_Node is private;
   type Button_Link is access Button_Node;

   type Open_Session_Record is new Gtk_Window_Record with record

      Sessions_Dir : String_Access;
      First_Button : Button_Link := null;
      Lock_Buttons : Boolean := False;

      ------------

      Vbox17 : Gtk_Vbox;
      Hbox7 : Gtk_Hbox;
      Vbox18 : Gtk_Vbox;
      Scrolledwindow10 : Gtk_Scrolled_Window;
      Viewport1        : Gtk_Viewport;
      List             : Gtk_List;
      Hbox6            : Gtk_Hbox;
      Label73          : Gtk_Label;
      Entry1           : Gtk_Entry;
      Hbuttonbox9      : Gtk_Hbutton_Box;
      Ok_Button        : Gtk_Button;
      Cancel_Button    : Gtk_Button;
      Help_Button      : Gtk_Button;
      Vseparator4 : Gtk_Vseparator;
      Vbox19 : Gtk_Vbox;
      Scrolledwindow11 : Gtk_Scrolled_Window;
      Viewport2 : Gtk_Viewport;
      File_Buttons : Gtk_Vbox;
      Hbuttonbox10 : Gtk_Hbutton_Box;
      Select_All : Gtk_Button;
      Unselect_All : Gtk_Button;
      Hseparator1 : Gtk_Hseparator;
   end record;
   type Open_Session_Access is access all Open_Session_Record'Class;

   procedure Gtk_New (Open_Session : out Open_Session_Access);
   procedure Initialize (Open_Session : access Open_Session_Record'Class);

   procedure Save_Session (Window : access Gtk_Widget_Record'Class;
                           Open   : in out Open_Session_Access;
                           Dir    : in String);
   --  Save a session. Window is the main debug window.

   procedure Open_Session (Window : access Gtk_Widget_Record'Class;
                           Open   : in out Open_Session_Access;
                           Dir    : in String);
   --  Load a session into Gvd. Window is the main debug window.


   procedure Create_Buttons (Open      : access Open_Session_Record'Class;
                             File_Name : in String);
   --  Add all the check_buttons corresponding to the session information.

   procedure Remove_All_Buttons (Open : access Open_Session_Record'Class);
   --  Clear all the check_buttons.

private

   type Button_Node is record
      Next       : Button_Link;
      Button     : Gtk_Check_Button;
      Label      : String_Access;
   end record;

end Open_Session_Pkg;

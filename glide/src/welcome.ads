-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Radio_Button;
with Gtk.Combo;
with Gtk.GEntry;

with Glide_Kernel;
with Logo_Boxes;

package Welcome is

   type Welcome_Screen_Record is new Logo_Boxes.Logo_Box_Record with private;
   type Welcome_Screen is access all Welcome_Screen_Record'Class;

   procedure Gtk_New
     (Screen              : out Welcome_Screen;
      Kernel              : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project_Name        : String := "";
      Default_Is_Tutorial : Boolean);
   --  Create a new welcome dialog. Project_Name is the project that should be
   --  suggested by default (empty string for the default project).
   --  If Default_Is_Tutorial is true, then the choice initially activated is
   --  to display the tutorial

   type Welcome_Result is (Project_Loaded,
                           Quit_GPS,
                           Show_Tutorial);
   --  The various possible choices the user made in the welcome screen.
   --  - Show_Tutorial is returned if the user wishes to see the tutorial.
   --    The default project was loaded.
   --  - Quit_GPS is returned if the user has decided to quit GPS
   --  - Project_Loaded if either a specific project or the default project was
   --    loaded

   function Run_Welcome (Screen : access Welcome_Screen_Record)
      return Welcome_Result;
   --  Display the screen dialog on the screen, and let the user choose the
   --  initial project. If the preference Display_Welcome is false, the dialog
   --  is not actually displayed on the screen, but the project specified in
   --  Gtk_New is selected.

private
   type Welcome_Screen_Record is new Logo_Boxes.Logo_Box_Record with record
      Default_Project     : Gtk.Radio_Button.Gtk_Radio_Button;
      Default_Dir         : Gtk.GEntry.Gtk_Entry;
      Default_Browse      : Gtk.Button.Gtk_Button;
      Create_Project      : Gtk.Radio_Button.Gtk_Radio_Button;
      Open_Project        : Gtk.Combo.Gtk_Combo;
      Open_Project_Button : Gtk.Radio_Button.Gtk_Radio_Button;
      Open_Tutorial_Button : Gtk.Radio_Button.Gtk_Radio_Button;
      Open_Browse         : Gtk.Button.Gtk_Button;
      Always_Show         : Gtk.Check_Button.Gtk_Check_Button;
      Kernel              : Glide_Kernel.Kernel_Handle;
   end record;

end Welcome;

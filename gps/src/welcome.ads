------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Radio_Button;
with Gtk.Combo_Box;
with Gtk.GEntry;

with GPS.Kernel;
with Logo_Boxes;
with GNATCOLL.VFS;

package Welcome is

   type Welcome_Screen_Record is new Logo_Boxes.Logo_Box_Record with private;
   type Welcome_Screen is access all Welcome_Screen_Record'Class;

   procedure Gtk_New
     (Screen       : out Welcome_Screen;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project_Name : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File);
   --  Create a new welcome dialog. Project_Name is the project that should be
   --  suggested by default (empty string for the default project).
   --  If Default_Is_Tutorial is true, then the choice initially activated is
   --  to display the tutorial

   type Welcome_Result is (Project_Loaded, Quit_GPS);
   --  The various possible choices the user made in the welcome screen.
   --  - Show_Tutorial is returned if the user wishes to see the tutorial.
   --    The default project was loaded.
   --  - Quit_GPS is returned if the user has decided to quit GPS
   --  - Project_Loaded if either a specific project or the default project was
   --    loaded

   function Run_Welcome
     (Screen : access Welcome_Screen_Record) return Welcome_Result;
   --  Display the screen dialog on the screen, and let the user choose the
   --  initial project. If the preference Display_Welcome is false, the dialog
   --  is not actually displayed on the screen, but the project specified in
   --  Gtk_New is selected.

private
   type Welcome_Screen_Record is new Logo_Boxes.Logo_Box_Record with record
      Project_Templates   : Gtk.Radio_Button.Gtk_Radio_Button;
      Default_Project     : Gtk.Radio_Button.Gtk_Radio_Button;
      Default_Dir         : Gtk.GEntry.Gtk_Entry;
      Default_Browse      : Gtk.Button.Gtk_Button;
      Create_Project      : Gtk.Radio_Button.Gtk_Radio_Button;
      Open_Project        : Gtk.Combo_Box.Gtk_Combo_Box;
      Open_Project_Button : Gtk.Radio_Button.Gtk_Radio_Button;
      Open_Browse         : Gtk.Button.Gtk_Button;
      Always_Show         : Gtk.Check_Button.Gtk_Check_Button;
      Kernel              : GPS.Kernel.Kernel_Handle;
   end record;

end Welcome;

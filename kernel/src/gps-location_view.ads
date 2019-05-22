------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package handles source file locations and displays them
--  in a graphical tree, per category.

with GNATCOLL.VFS;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Widget;                     use Gtk.Widget;

with GPS.Kernel;                     use GPS.Kernel;

package GPS.Location_View is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands for this module. This must be a separate
   --  subprogram, since the console is loaded before all other modules,
   --  including the scripting languages.

   subtype Location_View_Access is Gtk.Widget.Gtk_Widget;

   function Get_Or_Create_Location_View
     (Kernel : access Kernel_Handle_Record'Class) return Location_View_Access;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.
   --  This does not give the focus or raise the locations view: use
   --  Raise_Location_Window for this.

   procedure Next_Item
     (Self      : Location_View_Access;
      Backwards : Boolean := False);
   --  If an item is selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Expand_Category
     (Self       : Location_View_Access;
      Category   : String;
      Goto_First : Boolean);
   --  Requests to expand specified category and goto first visible location

   procedure Expand_File
     (Self       : Location_View_Access;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean);
   --  Requests to expand specified category and file and goto first visible
   --  location.

   procedure Expand_File
     (Self     : Location_View_Access;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File);
   --  Same as above but jump to first visible location is controlled
   --  by locations-auto-jump-to-first preference

   procedure Set_Activity_Progress_Bar_Visibility
     (Self    : not null Location_View_Access;
      Visible : Boolean);
   --  Show or hide the Locations view activity progress bar.
   --  This progress bar can be used to tell the user that messages are
   --  currently being processed and will be shown in the Locations view.

   procedure Raise_Locations_Window
     (Self             : not null access Kernel_Handle_Record'Class;
      Give_Focus       : Boolean := True;
      Create_If_Needed : Boolean := False);
   --  Raise the locations window, if it exists or create it before raising
   --  it if Create_If_Needed is True.

   procedure Set_Locations_Filter
     (Self  : not null access Kernel_Handle_Record'Class;
      Value : String);
   --  Set the Locations view filter to Value

end GPS.Location_View;

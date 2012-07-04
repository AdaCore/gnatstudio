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

--  This package handles source file locations and displays them
--  in a graphical tree, per category.

private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Gtk.Box;                        use Gtk.Box;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Glib;
with Glib.Main;

with GNATCOLL.VFS;
with GPS.Kernel.Messages;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Location_View_Filter_Panel; use GPS.Location_View_Filter_Panel;
with GPS.Tree_View;                  use GPS.Tree_View;
with GPS.Tree_View.Locations;        use GPS.Tree_View.Locations;

package GPS.Location_View is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module in GPS

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class);
   --  Register the shell commands for this module. This must be a separate
   --  subprogram, since the console is loaded before all other modules,
   --  including the scripting languages.

   type Location_View_Record is new Gtk_Vbox_Record with private;
   type Location_View is access all Location_View_Record'Class;

   procedure Gtk_New
     (View   : out Location_View;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID);
   --  Create a new Location_View

   procedure Initialize
     (Self   : access Location_View_Record'Class;
      Kernel : Kernel_Handle;
      Module : Abstract_Module_ID);
   --  Internal initialization procedure

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True) return Location_View;
   --  Return the results view widget. Create it if it doesn't exist and
   --  Allow_Creation is true.

   procedure Next_Item
     (Self      : access Location_View_Record'Class;
      Backwards : Boolean := False);
   --  If an item is selected, jump to the location pointed to by the iter
   --  immediately following it in the same category. If there is none, jump
   --  to the first item in the category.

   procedure Expand_Category
     (Self       : not null access Location_View_Record'Class;
      Category   : Ada.Strings.Unbounded.Unbounded_String;
      Goto_First : Boolean);
   --  Requests to expand specified category and goto first visible location

   procedure Expand_File
     (Self       : not null access Location_View_Record'Class;
      Category   : Ada.Strings.Unbounded.Unbounded_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean);
   --  Requests to expand specified category and file and goto first visible
   --  location.

private

   type Expansion_Request is record
      Category   : Ada.Strings.Unbounded.Unbounded_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean;
   end record;

   package Expansion_Request_Vectors is
     new Ada.Containers.Vectors (Positive, Expansion_Request);

   type Location_View_Record is new Gtk_Hbox_Record with record
      Kernel              : Kernel_Handle;
      Filter_Panel        : Locations_Filter_Panel;
      Sort_By_Category    : Boolean := False;
      --  Whether the view should be sorted by category

      View                : GPS_Locations_Tree_View;

      --  Idle handlers

      Idle_Expand_Handler : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Requests            : Expansion_Request_Vectors.Vector;
      --  Expansion requests.

      --  Message listener
      Listener            : GPS.Kernel.Messages.Listener_Access;

      Do_Not_Delete_Messages_On_Exit : Boolean := False;
      --  Protection against reentrancy
   end record;

   --  These callbacks are used by actions of locations view, which are
   --  declared in child package.

   procedure On_Clear_Locations (Self : access Location_View_Record'Class);
   --  Remove all locations from the view

   procedure On_Remove_Message (Self : access Location_View_Record'Class);
   --  Removes selected message

end GPS.Location_View;

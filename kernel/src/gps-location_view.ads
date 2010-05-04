-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2010, AdaCore                  --
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

--  This package handles source file locations and displays them
--  in a graphical tree, per category.

private with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Expect;
with GNAT.Strings;

with Gtk.Box;                        use Gtk.Box;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;          use Gtk.Tree_Model_Filter;
with Glib;
with Glib.Main;

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
     (Self         : not null access Location_View_Record'Class;
      Category     : Ada.Strings.Unbounded.Unbounded_String;
      Goto_First   : Boolean);
   --  Requests to expand specified category and goto first visible location

private

   type Expansion_Request is record
      Category     : Ada.Strings.Unbounded.Unbounded_String;
      Goto_First   : Boolean;
   end record;

   package Expansion_Request_Vectors is
     new Ada.Containers.Vectors (Positive, Expansion_Request);

   type Location_View_Record is new Gtk_Hbox_Record with record
      Kernel : Kernel_Handle;

      Filter_Panel : Locations_Filter_Panel;

      Regexp       : GNAT.Expect.Pattern_Matcher_Access;
      Text         : GNAT.Strings.String_Access;
      Is_Hide      : Boolean := False;

      Sort_By_Category : Boolean := False;
      --  Whether the view should be sorted by category

      View   : GPS_Locations_Tree_View;
      Filter : Gtk_Tree_Model_Filter;
      --  Tree filter model

      --  Idle handlers

      Idle_Expand_Handler : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Requests            : Expansion_Request_Vectors.Vector;
      --  Expansion requests.

      --  Message listener
      Listener : GPS.Kernel.Messages.Listener_Access;
   end record;

end GPS.Location_View;

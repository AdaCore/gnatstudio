-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This widget simply acts as a Gtk_Paned with a handle that is hidden
--  when there only one child.

with Glib; use Glib;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Box;
with Gtk.Paned;

package Dock_Paned is

   type Dock_Paned_Record is new Gtk.Box.Gtk_Box_Record with private;

   subtype Dock_Vpaned_Record is Dock_Paned_Record;
   subtype Dock_Hpaned_Record is Dock_Paned_Record;

   type Dock_Paned is access all Dock_Paned_Record'Class;
   subtype Dock_Vpaned is Dock_Paned;
   subtype Dock_Hpaned is Dock_Paned;

   procedure Gtk_New_Vpaned (Widget : out Dock_Paned);
   --  Create a new vertical container.
   --  The children will be displayed one on top of the other.

   procedure Gtk_New_Hpaned (Widget : out Dock_Paned);
   --  Create a new horizontal container.
   --  The children will be displayed one besides the other.

   procedure Initialize_Vpaned (Widget : access Dock_Paned_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_Hpaned (Widget : access Dock_Paned_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Add1 (Paned : access Dock_Paned_Record;
                   Child : access Gtk_Widget_Record'Class);
   --  Add the first child of the container.
   --  The child will be displayed either in the top or in the left pane,
   --  depending on the orientation of the container.

   procedure Add2 (Paned : access Dock_Paned_Record;
                   Child : access Gtk_Widget_Record'Class);
   --  Add the second child of the container.
   --  It will be displayed in the bottom or right pane, depending on the
   --  container's orientation.

   procedure Add (Paned : access Dock_Paned_Record;
                  Child : access Gtk_Widget_Record'Class);
   --  If there is a child 1 and no child 2, then acts as Add2.
   --  If there is a child 2 and no child 1, then acts as Add1.
   --  Otherwise, does nothing.

   procedure Dock_Remove (Paned : access Dock_Paned_Record'Class;
                          Child : access Gtk_Widget_Record'Class);
   --  Remove the child from the paned;

   function Get_Paned (Paned : access Dock_Paned_Record)
                      return Gtk.Paned.Gtk_Paned;
   --  Return the paned within the Dock_Paned.

   procedure Set_Position (Paned    : access Dock_Paned_Record;
                           Position : Gint);
   --  Change the position of the separator.
   --  If position is negative, the remembered position is forgotten,
   --  and the division is recomputed from the requisitions of the
   --  children.
   --  Position is in fact the size (either vertically or horizontally,
   --  depending on the container) set for the first child.

   procedure Set_Handle_Size (Paned : access Dock_Paned_Record;
                              Size  : Guint16);
   --  Set the handle size to Size x Size pixels.
   --  This is the small handle that the user has to drag to resize the
   --  panes.

   function Get_Handle_Size (Paned : access Dock_Paned_Record) return Guint16;
   --  Return the current size in pixels of the handle.

   procedure Set_Gutter_Size (Paned : access Dock_Paned_Record;
                              Size  : in Guint16);
   --  Set the width in pixels of the gutter.
   --  This is the area between the two panes.

   function Get_Gutter_Size (Paned : access Dock_Paned_Record) return Guint16;
   --  Return the width in pixels of the gutter.

private
   type Dock_Paned_Record is new Gtk.Box.Gtk_Box_Record with record
      Paned     : Gtk.Paned.Gtk_Paned;
      Is_Child1 : Boolean := False;
      Is_Child2 : Boolean := False;
   end record;
end Dock_Paned;

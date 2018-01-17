------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

--  This package provides a search entry.
--  This is an entry with special styles and icons. In particular, it
--  provides an icon to clear the entry.

with Gdk.Event;   use Gdk.Event;
with Gtk.GEntry;  use Gtk.GEntry;

package Gtkada.Search_Entry is

   type Gtkada_Search_Entry_Record is new Gtk.GEntry.Gtk_Entry_Record
      with null record;
   type Gtkada_Search_Entry is access all Gtkada_Search_Entry_Record'Class;

   procedure Gtk_New
      (Self        : out Gtkada_Search_Entry;
       Placeholder : String := "");
   --  Create a new search entry

   function Get_Icon_Position
     (Self   : access Gtkada_Search_Entry_Record'Class;
      Event  : Gdk_Event_Button) return Gtk_Entry_Icon_Position;
   --  Returns the icon which was clicked on.
   --  For some reason, gtk+ always seems to return the primary icon otherwise.

end Gtkada.Search_Entry;

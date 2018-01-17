------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This package provides a scrolled window for an text buffer,
--  which displays tooltips while scrolling.

with Gtk.Label;             use Gtk.Label;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Window;            use Gtk.Window;

package Src_Editor_Box.Scrolled_Window is

   type Tooltip_Scrolled_Window_Record is
      new Gtk_Scrolled_Window_Record with private;
   type Tooltip_Scrolled_Window
      is access all Tooltip_Scrolled_Window_Record'Class;

   procedure Gtk_New (Self : out Tooltip_Scrolled_Window);
   --  Create a new scrolled window.
   --  This must contain a Source_View.

private
   type Tooltip_Scrolled_Window_Record is new Gtk_Scrolled_Window_Record with
      record
         Has_Button_Press : Boolean := False;
         Label            : Gtk_Label;
         Window           : Gtk_Window;
      end record;

end Src_Editor_Box.Scrolled_Window;

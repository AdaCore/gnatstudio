------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

--  This package provides utility subprograms used to display ephemeral
--  informational popups that use icons on a given window.
--
--  These popups can be used to pass some explicit information in a more visual
--  way (e.g: display a 'loopback' icon when to tell the user that the search
--  in the current editor has restarted from the beginning of the file).

with Gdk.RGBA;   use Gdk.RGBA;
with Gtk.Window; use Gtk.Window;

package Informational_Popups is

   procedure Display_Informational_Popup
     (Parent                : not null access Gtk_Window_Record'Class;
      Icon_Name             : String;
      No_Transparency_Color : Gdk_RGBA := Black_RGBA;
      Text                  : String := "");
   --  Display for a brief time an infomational popup in the center of Parent,
   --  using Icon_Name to retrieve the icon that should be displayed.
   --
   --  No_Transparency_Color is used for the informational popup's background
   --  when transparency is not supported.

end Informational_Popups;

------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package gives access to run-time specific information about Gtk+

with Gdk.Display;  use Gdk.Display;

package Gtk_Utils is

   function Gtk_Major_Version return Integer;
   --  Return the Gtk+ run-time major version

   function Gtk_Minor_Version return Integer;
   --  Return the Gtk+ run-time minor version

   function Gtk_Micro_Version return Integer;
   --  Return the Gtk+ run-time micro version

   function Have_Render (Display : Gdk_Display) return Boolean;
   --  Return True if the display associated with Window has the RENDER
   --  extension, or if the RENDER extension is not relevant (e.g. under
   --  Windows).

private
   pragma Inline (Gtk_Major_Version);
   pragma Inline (Gtk_Minor_Version);
   pragma Inline (Gtk_Micro_Version);
end Gtk_Utils;

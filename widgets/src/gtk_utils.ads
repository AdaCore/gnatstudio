-----------------------------------------------------------------------
--                                 G P S                             --
--                                                                   --
--                        Copyright (C) 2005                         --
--                                AdaCore                            --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package gives access to run-time specific information about Gtk+

package Gtk_Utils is

   function Gtk_Major_Version return Integer;
   --  Return the Gtk+ run-time major version

   function Gtk_Minor_Version return Integer;
   --  Return the Gtk+ run-time minor version

   function Gtk_Micro_Version return Integer;
   --  Return the Gtk+ run-time micro version

private
   pragma Inline (Gtk_Major_Version);
   pragma Inline (Gtk_Minor_Version);
   pragma Inline (Gtk_Micro_Version);
end Gtk_Utils;

-----------------------------------------------------------------------
--                                 G P S                             --
--                                                                   --
--                     Copyright (C) 2005-2007                       --
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

--  Windows specific implementation
--  ??? This version should be the only one, assuming DLL convention is
--  mapped to C under non Windows systems, which is no longer the case
--  in recent GNAT versions.

package body Gtk_Utils is

   -----------------------
   -- Gtk_Major_Version --
   -----------------------

   function Gtk_Major_Version return Integer is
      Version : Integer;
      pragma Import (Dll, Version, "gtk_major_version");
   begin
      return Version;
   end Gtk_Major_Version;

   -----------------------
   -- Gtk_Minor_Version --
   -----------------------

   function Gtk_Minor_Version return Integer is
      Version : Integer;
      pragma Import (Dll, Version, "gtk_minor_version");
   begin
      return Version;
   end Gtk_Minor_Version;

   -----------------------
   -- Gtk_Micro_Version --
   -----------------------

   function Gtk_Micro_Version return Integer is
      Version : Integer;
      pragma Import (Dll, Version, "gtk_micro_version");
   begin
      return Version;
   end Gtk_Micro_Version;

   -----------------
   -- Have_Render --
   -----------------

   function Have_Render (Window : Gdk.Gdk_Window) return Boolean;
   begin
      --  Pretend we have RENDER extension under Windows, since this is
      --  not relevant to this platform
      return True;
   end Have_Render;

end Gtk_Utils;

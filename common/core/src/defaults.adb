-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2015-2019, AdaCore              --
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

with Config; use Config;

package body Defaults is

   ------------------
   -- Default_Font --
   ------------------

   function Default_Font return String is
   begin
      if Host = Windows then
         --  Windows

         return
           "Segoe WP 10,"               --  Windows 8.1 Pro
           & "Segoe UI 10,"             --  Windows 8.1 Home and 7
           & "Tahoma 10,"               --  Windows XP
           & "Arial 10,"                --  NT/Classic
           & "Sans 10";                 --  for safety

      elsif Darwin_Target then
         --  Mac OS

         return
           "Helvetica Neue Medium 12,"  --  Yosemite
           & "Lucida Grande Medium 12";        --  all other versions

      else
         --  Unix/Linux
         --
         --  The "DejaVu Sans"/"DejaVu Sans Mono" combo exists on
         --  Ubuntu 12/Debian 7, Suse 10: try it first

         return "DejaVu Sans 9,"   --  a lot of systems, see above
           & "DejaVu LGC Sans 9,"  --  default on RedHat 5
           & "Sans 9";             --  reliable fallback for a variable width
      end if;
   end Default_Font;

   ------------------------
   -- Default_Fixed_Font --
   ------------------------

   function Default_Fixed_Font return String is
   begin
      if Host = Windows then
         --  Windows

         return
           "Consolas 9,"               --  Windows 8, Windows 7
           & "Lucida Console 9";       --  all versions

      elsif Darwin_Target then
         --  Mac OS

         return
           "Menlo 11,"     --  Xcode 6 default, available since Mavericks
           & "Monaco 11";  --  all previous versions

      else
         --  Unix/Linux

         --  Note: On RedHat 5, "Monospace" is *not* a valid alias for a
         --  monospaced font!

         return "DejaVu Sans Mono 8,"   --  a lot of systems, see above
           & "DejaVu LGC Sans Mono 8,"  --  default on RedHat 5
           & "MiscFixed 10,"            --  fallback on some versions
           & "Courier 10";              --  last resort
      end if;
   end Default_Fixed_Font;

end Defaults;

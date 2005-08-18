-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2005                         --
--                              AdaCore                              --
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

--  This package handles the charsets supported by GPS

with Glib;

package GPS.Kernel.Charsets is

   -----------------
   -- Preferences --
   -----------------

   type Param_Spec_Charset is new Glib.Param_Spec;

   function Gnew_Charset
     (Name, Nick, Blurb   : String;
      Default             : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec_Charset;
   --  Create a new preference representing a charset

   function Get_Pref (Pref : Param_Spec_Charset) return String;
   --  Return the currently selected charset

end GPS.Kernel.Charsets;

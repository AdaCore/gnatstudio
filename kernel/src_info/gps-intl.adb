------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  ??? Should use a callback to avoid direct dependency on GtkAda.Intl
--  with Gtkada.Intl; use Gtkada.Intl;

package body GPS.Intl is

   function "-" (Msg : String) return String is
   begin
      --  The default domain name is already "gps", no need to look it up again
      --  ??? dummy implementation for now, we don't really support i18n anyway
      --  return Gettext (Msg);
      return Msg;
   end "-";

end GPS.Intl;

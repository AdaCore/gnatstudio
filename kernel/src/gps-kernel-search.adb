------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with GPS.Search;  use GPS.Search;

package body GPS.Kernel.Search is

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self : Kernel_Provider_Registry;
      Name : String) return Search_Provider_Access
   is
      P : constant Search_Provider_Access :=
        Search_Provider_Registry (Self).Get (Name);
   begin
      if P /= null
        and then P.all in Kernel_Search_Provider'Class
      then
         Kernel_Search_Provider (P.all).Kernel := Self.Kernel;
      end if;

      return P;
   end Get;

end GPS.Kernel.Search;

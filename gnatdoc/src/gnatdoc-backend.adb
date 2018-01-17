------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with GNATdoc.Backend.Dummy;  use GNATdoc.Backend.Dummy;
with GNATdoc.Backend.HTML;   use GNATdoc.Backend.HTML;
with GNATdoc.Backend.Simple; use GNATdoc.Backend.Simple;

package body GNATdoc.Backend is

   -----------------
   -- New_Backend --
   -----------------

   function New_Backend (Name : String) return GNATdoc_Backend'Class is
   begin
      if Name = "html" then
         return Result : HTML_Backend;

      elsif Name = "test" then
         return Result : Simple_Backend;

      elsif Name = "cm" then
         --  This is special kind of backend to generate cm/pr files only

         return Result : Dummy_Backend;

      else
         raise Unknown_Backend;
      end if;
   end New_Backend;

end GNATdoc.Backend;

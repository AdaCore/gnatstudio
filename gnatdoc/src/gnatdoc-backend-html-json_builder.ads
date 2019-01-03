------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2019, AdaCore                   --
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
--  This package contains code to convert stream of markup events into JSON
--  array which is used by JavaScript to render documentation page.

with GNATdoc.Markup_Streams;

private package GNATdoc.Backend.HTML.JSON_Builder is

   function To_JSON_Representation
     (Stream : GNATdoc.Markup_Streams.Event_Vectors.Vector;
      Kernel : not null access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return GNATCOLL.JSON.JSON_Array;
   --  Converts stream of markup events to JSON_Array to be used by JavaScript
   --  to render documentation page.

end GNATdoc.Backend.HTML.JSON_Builder;

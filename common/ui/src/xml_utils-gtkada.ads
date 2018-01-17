------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This is the part of XML_Utils that is dependent on GtkAda.

with Ada.Unchecked_Conversion;
with Glib.Xml_Int; use Glib.Xml_Int;

package XML_Utils.GtkAda is

   function Convert is new Ada.Unchecked_Conversion
     (Glib.Xml_Int.Node_Ptr, XML_Utils.Node_Ptr);
   function Convert is new Ada.Unchecked_Conversion
     (XML_Utils.Node_Ptr, Glib.Xml_Int.Node_Ptr);

end XML_Utils.GtkAda;

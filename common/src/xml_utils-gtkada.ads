-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This is the part of XML_Utils that is dependent on GtkAda.

with Ada.Unchecked_Conversion;
with Glib.Xml_Int; use Glib.Xml_Int;

package XML_Utils.GtkAda is

   function Convert is new Ada.Unchecked_Conversion
     (Glib.Xml_Int.Node_Ptr, XML_Utils.Node_Ptr);
   function Convert is new Ada.Unchecked_Conversion
     (XML_Utils.Node_Ptr, Glib.Xml_Int.Node_Ptr);

end XML_Utils.GtkAda;

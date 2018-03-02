------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

private package CodePeer.Bridge.Inspection_Readers.Utilities is

   function Get_Value
     (Attrs : Sax.Attributes.Attributes'Class;
      Name  : String) return Ada.Strings.Unbounded.Unbounded_String;

   function Get_Lifeage
     (Attrs : Sax.Attributes.Attributes'Class) return Lifeage_Kinds;
   --  Returns value of "lifeage" attribure.

end CodePeer.Bridge.Inspection_Readers.Utilities;

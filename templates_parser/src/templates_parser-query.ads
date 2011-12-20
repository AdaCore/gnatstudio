------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

package Templates_Parser.Query is

   function Kind
     (Association : Templates_Parser.Association) return Association_Kind;
   --  Returns the kind for this association

   function Variable
     (Association : Templates_Parser.Association) return String;
   --  Returns the variable name for Association

   function Composite
     (Association : Templates_Parser.Association) return Tag;
   --  Returns the vector tag for this association, raises Constraint_Error
   --  if it is not a vector.

   function Nested_Level (T : Tag) return Positive;
   --  Returns the nested level for tag T, 1 means that this is a vector tag,
   --  2 that it is a matrix.

end Templates_Parser.Query;

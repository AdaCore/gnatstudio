------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2015-2019, AdaCore                   --
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

private package GNATdoc.Frontend.Comment_Parser is

   function May_Have_Tags
     (Text : Unbounded_String_Vectors.Vector) return Boolean;
   --  Return true if Text may contain some tag

   procedure Build_Structured_Comments
     (Context : access constant Docgen_Context;
      Root    : Entity_Id);
   --  Traverse the Tree of entities and replace blocks of comments by
   --  structured comments.

end GNATdoc.Frontend.Comment_Parser;

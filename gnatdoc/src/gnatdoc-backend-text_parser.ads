------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2014, AdaCore                   --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package GNATdoc.Backend.Text_Parser is

   type Event_Kinds is (Start_Tag, Text, End_Tag);

   type Event_Type (Kind : Event_Kinds := Text) is record
      case Kind is
         when Start_Tag | End_Tag =>
            Name : Ada.Strings.Unbounded.Unbounded_String;

            case Kind is
               when Start_Tag =>
                  Parameter : Ada.Strings.Unbounded.Unbounded_String;

               when others =>
                  null;
            end case;

         when Text =>
            Text : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   package Event_Vectors is
     new Ada.Containers.Vectors (Positive, Event_Type);

   function Parse_Text (Comment_Text : String) return Event_Vectors.Vector;

end GNATdoc.Backend.Text_Parser;

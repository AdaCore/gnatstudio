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

package body GNATdoc.Backend.HTML.JSON_Builder is

   use GNATCOLL.JSON;

   ----------------------------
   -- To_JSON_Representation --
   ----------------------------

   function To_JSON_Representation
     (Stream : GNATdoc.Backend.Text_Parser.Event_Vectors.Vector)
      return GNATCOLL.JSON.JSON_Array
   is
      type State_Type is record
         Object   : GNATCOLL.JSON.JSON_Value;
         Children : GNATCOLL.JSON.JSON_Array;
      end record;

      package State_Vectors is
        new Ada.Containers.Vectors (Positive, State_Type);

      State_Stack : State_Vectors.Vector;
      State       : State_Type;
      Object      : GNATCOLL.JSON.JSON_Value;
      Aux         : GNATCOLL.JSON.JSON_Array;
      Number      : Positive;

   begin
      for Event of Stream loop
         case Event.Kind is
            when GNATdoc.Backend.Text_Parser.Start_Tag =>
               State_Stack.Append (State);
               State := (Create_Object, Empty_Array);

               if Event.Name = "p" then
                  State.Object.Set_Field ("kind", "paragraph");

               elsif Event.Name = "pre" then
                  State.Object.Set_Field ("kind", "code");
                  Number := 1;

               else
                  State.Object.Set_Field ("kind", To_String (Event.Name));
               end if;

            when GNATdoc.Backend.Text_Parser.End_Tag =>
               State.Object.Set_Field ("children", State.Children);
               Object := State.Object;
               State := State_Stack.Last_Element;
               State_Stack.Delete_Last;
               Append (State.Children, Object);

            when GNATdoc.Backend.Text_Parser.Text =>
               if String'(State.Object.Get ("kind")) = "code" then
                  Object := Create_Object;
                  Object.Set_Field ("kind", "span");
                  Object.Set_Field ("text", To_String (Event.Text));

                  Clear (Aux);
                  Append (Aux, Object);

                  Object := Create_Object;
                  Object.Set_Field ("number", Number);
                  Object.Set_Field ("children", Aux);
                  Number := Number + 1;

               else
                  Object := Create_Object;
                  Object.Set_Field ("kind", "span");
                  Object.Set_Field ("text", To_String (Event.Text) & ASCII.LF);
               end if;

               Append (State.Children, Object);
         end case;
      end loop;

      return State.Children;
   end To_JSON_Representation;

end GNATdoc.Backend.HTML.JSON_Builder;

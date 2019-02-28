------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  This package provide a simple-minded implementation of a virtual list
--  component, using a generic list.

with GPS_Indefinite_Vectors;

generic
   with procedure Free (Data : in out Data_Type) is <>;
   --  Free the given data

   with procedure Copy_On_Get (Data : in out Data_Type) is null;
   --  If a copy needs to be done before returning the object on the Get
   --  subprogram. Does nothing by default.
package Virtual_Lists.Extensive is

   ------------------------------
   -- Extensive_List_Component --
   ------------------------------

   package Extensive_List_Pckg is new GPS_Indefinite_Vectors (Data_Type);
   --  May be used to create list for Data_Type.

   type Extensive_List_Component is new Virtual_List_Component with private;

   type Extensive_List_Iterator is new Virtual_List_Component_Iterator
   with private;

   function To_Extensive_List (L : Extensive_List_Pckg.Vector)
      return Extensive_List_Component;
   --  Return an extensive list corresponding to the list given in parameter.
   --  Note that no hard copy is done, so the two lists will share the same
   --  nodes.

   overriding function First (List : Extensive_List_Component)
      return Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   overriding function At_End (It : Extensive_List_Iterator) return Boolean;
   --  See inherited documentation

   overriding procedure Next (It : in out Extensive_List_Iterator);
   --  See inherited documentation

   overriding function Get
     (It : in out Extensive_List_Iterator) return Data_Type;
   --  See inherited documentation

   overriding procedure Free (List : in out Extensive_List_Component);
   --  See inherited documentation

private
   type Extensive_List_Component is new Virtual_List_Component with record
      Content : Extensive_List_Pckg.Vector;
   end record;

   type Extensive_List_Iterator is new Virtual_List_Component_Iterator with
      record
         It : Extensive_List_Pckg.Std_Vectors.Cursor;
      end record;

end Virtual_Lists.Extensive;

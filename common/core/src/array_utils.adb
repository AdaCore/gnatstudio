------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body Array_Utils is

   -------------
   -- Map_Gen --
   -------------

   function Map_Gen (In_Array : Array_Type) return Out_Array_Type
   is
   begin
      return Ret_Array : Out_Array_Type (In_Array'Range) do
         for J in In_Array'Range loop
            Ret_Array (J) := Transform (In_Array (J));
         end loop;
      end return;
   end Map_Gen;

   ---------
   -- Map --
   ---------

   function Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Out_Type) return Out_Array_Type
   is
      function Map_Gen_Internal
      is new Map_Gen (Out_Type, Out_Array_Type, Transform.all);
   begin
      return Map_Gen_Internal (In_Array);
   end Map;

   ------------
   -- Filter --
   ------------

   function Filter
     (In_Array : Array_Type;
      Pred : access function
        (E : Element_Type) return Boolean) return Array_Type
   is
      function Filter_Internal is new Filter_Gen (Pred.all);
   begin
      return Filter_Internal (In_Array);
   end Filter;

   --------------
   -- Contains --
   --------------

   function Contains
     (In_Array : Array_Type; El : Element_Type) return Boolean
   is
   begin
      for E of In_Array loop
         if E = El then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ------------
   -- Unique --
   ------------

   function Unique
     (In_Array : Array_Type) return Array_Type
   is
      Keep : Bool_Array (In_Array'Range) := (others => True);
      Count : Natural := 0;
   begin
      for I in In_Array'Range loop
         for J in In_Array'First .. I - 1 loop
            if In_Array (I) = In_Array (J) then
               Keep (J) := False;
               Count := Count - 1;
            end if;

            exit when not Keep (J);
         end loop;

         Count := Count + 1;
      end loop;

      return Ret_Array : Array_Type (In_Array'First
                                     .. In_Array'First + (Count - 1))
      do
         Count := In_Array'First;

         for J in In_Array'Range loop
            if Keep (J) then
               Ret_Array (Count) := In_Array (J);
               Count := Count + 1;
            end if;
         end loop;
      end return;
   end Unique;

   ------------
   -- Filter --
   ------------

   function Filter_Gen (In_Array : Array_Type) return Array_Type
   is
      Pred_Array : Bool_Array (In_Array'Range);
      Keep_Count : Natural := 0;
   begin
      for J in In_Array'Range loop
         Pred_Array (J) := Predicate (In_Array (J));
         if Pred_Array (J) then
            Keep_Count := Keep_Count + 1;
         end if;
      end loop;

      if Keep_Count = 0 then
         return Empty_Array;
      end if;

      return Ret_Array : Array_Type (In_Array'First
                                     .. In_Array'First + (Keep_Count - 1))
      do
         Keep_Count := In_Array'First;

         for J in In_Array'Range loop
            if Pred_Array (J) then
               Ret_Array (Keep_Count) := In_Array (J);
               Keep_Count := Keep_Count + 1;
            end if;
         end loop;
      end return;
   end Filter_Gen;

   ---------------------
   -- Id_Flat_Map_Gen --
   ---------------------

   function Id_Flat_Map_Gen (In_Array : Array_Type) return Array_Type
   is
      function Flat_Map_Internal
      is new Flat_Map_Gen (Element_Type, Array_Type, Transform);
   begin
      return Flat_Map_Internal (In_Array);
   end Id_Flat_Map_Gen;

   -----------------
   -- Id_Flat_Map --
   -----------------

   function Id_Flat_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Array_Type)
      return Array_Type
   is
      function Id_Flat_Map_Internal is new Id_Flat_Map_Gen (Transform.all);
   begin
      return Id_Flat_Map_Internal (In_Array);
   end Id_Flat_Map;

   ------------------
   -- Flat_Map_Gen --
   ------------------

   function Flat_Map_Gen (In_Array : Array_Type) return Fun_Ret_Array_Type
   is
      subtype Empty_Ret_Type is Fun_Ret_Array_Type (1 .. 0);
   begin
      if In_Array'Length = 0 then
         return Empty_Ret_Type'(others => <>);
      else
         return Transform (In_Array (In_Array'First))
           & Flat_Map_Gen (In_Array (In_Array'First + 1 .. In_Array'Last));
      end if;
   end Flat_Map_Gen;

   --------------
   -- Flat_Map --
   --------------

   function Flat_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Fun_Ret_Array_Type)
      return Fun_Ret_Array_Type
   is
      function Flat_Map_Internal
      is new Flat_Map_Gen (F_Type, Fun_Ret_Array_Type, Transform.all);
   begin
      return Flat_Map_Internal (In_Array);
   end Flat_Map;

   ----------------
   -- Id_Map_Gen --
   ----------------

   function Id_Map_Gen (In_Array : Array_Type) return Array_Type
   is
      function Id_Map_Internal
      is new Map_Gen (Element_Type, Array_Type, Transform);
   begin
      return Id_Map_Internal (In_Array);
   end Id_Map_Gen;

   ------------
   -- Id_Map --
   ------------

   function Id_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Element_Type) return Array_Type
   is
      function Id_Map_Internal is new Id_Map_Gen (Transform.all);
   begin
      return Id_Map_Internal (In_Array);
   end Id_Map;

end Array_Utils;

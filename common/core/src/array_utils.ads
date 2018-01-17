------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

generic
   type Element_Type is private;
   with function "=" (L, R : Element_Type) return Boolean is <>;
package Array_Utils is

   subtype Index_Type is Positive;

   type Array_Type is array (Index_Type range <>) of Element_Type;
   Empty_Array : constant Array_Type (1 .. 0) := (others => <>);

   type Array_Type_Access is access all Array_Type;

   type Option_Type (Has_Element : Boolean) is record
      case Has_Element is
      when True =>
         Element : Element_Type;
      when False => null;
      end case;
   end record;

   function Create (El : Element_Type) return Option_Type;

   None : Option_Type := (Has_Element => False);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Array_Type, Name => Array_Type_Access);

   ---------
   -- Map --
   ---------

   generic
      type Out_Type is private;
      type Out_Array_Type is array (Index_Type range <>) of Out_Type;
      with function Transform (In_Element : Element_Type) return Out_Type;
   function Map_Gen (In_Array : Array_Type) return Out_Array_Type;

   generic
      type Out_Type is private;
      type Out_Array_Type is array (Index_Type range <>) of Out_Type;
   function Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Out_Type) return Out_Array_Type;

   generic
      with function Transform (In_Element : Element_Type) return Element_Type;
   function Id_Map_Gen (In_Array : Array_Type) return Array_Type;

   function Id_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Element_Type) return Array_Type;

   ------------
   -- Filter --
   ------------

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Filter_Gen (In_Array : Array_Type) return Array_Type;

   function Filter
     (In_Array : Array_Type;
      Pred : access function
        (E : Element_Type) return Boolean) return Array_Type;

   generic
      with function "=" (L, R : Element_Type) return Boolean;
   function Unique_Gen
     (In_Array : Array_Type) return Array_Type;

   function Unique
     (In_Array : Array_Type) return Array_Type;

   function Contains
     (In_Array : Array_Type; El : Element_Type) return Boolean;

   function Contains
     (In_Array : Array_Type;
      Pred : access function (El : Element_Type) return Boolean)
      return Boolean;

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Find_Gen (In_Array : Array_Type;
                      Rev : Boolean := False) return Option_Type;

   function Find
     (In_Array : Array_Type;
      Predicate :
      access function (El : Element_Type) return Boolean;
      Rev : Boolean := False) return Option_Type;

   function Find
     (In_Array : Array_Type;
      Predicate :
      access function (El : Element_Type) return Boolean;
      Rev : Boolean := False) return Natural;

   function Find (In_Array : Array_Type;
                  Predicate :
                  access function (El : Element_Type) return Boolean;
                  Rev : Boolean := False;
                  Ret : out Element_Type) return Boolean;

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Find_Gen_Or (In_Array : Array_Type;
                         Val_If_Not_Found : Element_Type;
                         Rev : Boolean := False) return Element_Type;

   function Find
     (In_Array : Array_Type;
      Predicate :
      access function (El : Element_Type) return Boolean;
      Val_If_Not_Found : Element_Type;
      Rev : Boolean := False) return Element_Type;

   --------------
   -- Flat_Map --
   --------------

   generic
      type F_Type is private;
      type Index_Type is range <>;
      type Fun_Ret_Array_Type is array (Index_Type range <>) of F_Type;
   function Flat_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Fun_Ret_Array_Type)
      return Fun_Ret_Array_Type;

   function Id_Flat_Map
     (In_Array : Array_Type;
      Transform : access function
        (In_Element : Element_Type) return Array_Type)
      return Array_Type;

   generic
      type F_Type is private;
      type Index_Type is range <>;
      type Fun_Ret_Array_Type is array (Index_Type range <>) of F_Type;
      with function Transform
        (In_Element : Element_Type) return Fun_Ret_Array_Type;
   function Flat_Map_Gen (In_Array : Array_Type) return Fun_Ret_Array_Type;

   generic
      with function Transform
        (In_Element : Element_Type) return Array_Type;
   function Id_Flat_Map_Gen (In_Array : Array_Type) return Array_Type;

   ----------
   -- Sort --
   ----------
   generic
      with function "<" (Left, Right : Element_Type) return Boolean;
   function Sort_Gen (In_Array : Array_Type) return Array_Type;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean;
   procedure In_Place_Sort_Gen (In_Out_Array : in out Array_Type);

private
   type Bool_Array is array (Index_Type range <>) of Boolean;
end Array_Utils;

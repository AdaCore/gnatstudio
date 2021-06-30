------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;

with GVD.Breakpoints_List;

package DAP.breakpoint_map is

   function Breakpoint_Data_Equal
     (L, R : GVD.Breakpoints_List.Breakpoint_Data) return Boolean;

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => GVD.Breakpoints_List.Breakpoint_Data,
      "="          => Breakpoint_Data_Equal);

   function Breakpoint_Vector_Equal
     (L, R : Breakpoint_Vectors.Vector) return Boolean;

   --!pp off
   package Breakpoint_Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Breakpoint_Vectors.Vector,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
     "="              => Breakpoint_Vectors."=");
   --!pp on

   use Breakpoint_Hash_Maps;

   type Breakpoint_Map is new Breakpoint_Hash_Maps.Map with null record;

   procedure Add
     (Self : in out Breakpoint_Map; B : GVD.Breakpoints_List.Breakpoint_Data);

   procedure Dump (Self : in out Breakpoint_Map);
end DAP.breakpoint_map;

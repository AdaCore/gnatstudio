------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

--  Lists/vectors to hold breakpoints

with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with GPS.Markers;            use GPS.Markers;
with DAP.Types;              use DAP.Types;

package DAP.Breakpoint_Maps is

   type Breakpoint_Disposition is (Delete, Disable, Keep);
   --  What to do with a breakpoint when it is reached.

   package Breakpoint_Nums is new Ada.Containers.Vectors
     (Positive, Breakpoint_Identifier);

   type Breakpoint_Data is record
      Num         : Breakpoint_Identifier := No_Breakpoint;

      Location    : Location_Marker   := No_Marker;
      --  The location of the breakpoint

      Subprogram  : Ada.Strings.Unbounded.Unbounded_String;

      Disposition : Breakpoint_Disposition := Keep;
      --  What is done when the breakpoint is reached

      Condition   : Unbounded_String;
      --  Condition on which this breakpoint is activated

      Ignore      : Natural := 0;
      --  Number of hits that will be ignored before actually stopping

      Enabled     : Boolean := True;
   end record;

   Empty_Breakpoint_Data : constant Breakpoint_Data :=
     Breakpoint_Data'(others => <>);

   function Breakpoint_Data_Equal
     (L, R : Breakpoint_Data) return Boolean is (L.Num = R.Num);

   package Breakpoint_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Breakpoint_Data,
      "="          => Breakpoint_Data_Equal);

   function Breakpoint_Vector_Equal
     (L, R : Breakpoint_Vectors.Vector) return Boolean;

   package Breakpoint_Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Breakpoint_Vectors.Vector,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Breakpoint_Vectors."=");

   use Breakpoint_Hash_Maps;

   type Breakpoint_Map is new Breakpoint_Hash_Maps.Map with null record;

   procedure Add
     (Self : in out Breakpoint_Map;
      Data : Breakpoint_Data);

   function Get_For_File
     (Self : Breakpoint_Map;
      File : Virtual_File)
      return Breakpoint_Vectors.Vector;

   procedure Set_For_File
     (Self   : in out Breakpoint_Map;
      File   : Virtual_File;
      Vector : Breakpoint_Vectors.Vector);

   procedure Remove_File
     (Self : in out Breakpoint_Map;
      File : Virtual_File);

   type All_Breakpoints is record
      Sources     : Breakpoint_Map;
      Subprograms : Breakpoint_Vectors.Vector;
   end record;

end DAP.Breakpoint_Maps;
